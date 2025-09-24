!-----------------------------------------------------------------------
!  Programmer: Tim Hunter,  22 Sep 2016
!
!  This module provides a structured storage container for a single time-series
!  of daily data.  One kind of data for a single point/region/whatever.
!
!  It relies on the object-oriented functionality added in Fortran 2003, so you
!  MUST use a compiler that supports that version of the Fortran standard.
!
!  The members of the type (or class) are:
!    DataIsValid  = a logical(boolean) variable that indicates if we think the
!                   data in the 2-d data array is valid.
!    SDate        = sequence number for the first day's data
!    EDate        = sequence number for the last day's data
!    NumDays      = merely a convenience for users of the type; NumDays = EDate-SDate+1
!    DataType     = one of the GDT_* entries from module GlerlDataTypesAndUnits
!    DataUnits    = one of the GDU_* entries from module GlerlDataTypesAndUnits
!    DataVals     = 1-d array of REAL values that contains a continuous time-series of
!                   the data values. Missing data will be represented by MissingData_REAL.
!
!  Behavioral notes, for reference (discovered/verified by testing). These serve partly
!  as a reminder to myself, but the main purpose is to be an aid to future programmers
!  who may work with this code.
!
!  1) If you happen to be a Java programmer... Fortran ain't Java. Java has
!     automatic garbage-collection, but Fortran does not. You need to explicitly 
!     clean up allocations when they are no longer needed/valid.
!
!  2) What happens when you say A=B with these kinds of objects?
!  
!     Assume the following declaration in a program:
!         TYPE (TDlyData) :: D1, D2
!
!     And then a build and fill of D1:
!         D1 = TDlyData()
!         Flag=D1%SetValues(DataType=GDT_AirtempMax, DataUnits=GDU_Celsius,          &
!                           Start=SSeq, Finish=ESeq, DataArray=SomeData)
!
!     If I then say:
!         D2 = D1
!     A *fully independent COPY* is made.  D2 immediately has all of the same 
!     data values as D1 (both for the metadata AND for the data array).  I can
!     assign new values into D2, and D1 remains unchanged.  I can clear D1, and
!     D2 retains the D2 values.  So this is not a "shallow" copy (pointer assignment),
!     it is a "deep" copy (all data in the structure).
!
!     THIS IS CRITICALLY IMPORTANT, because that means D2 has automatically allocated 
!     RAM from the heap, and in order to avoid memory leakage, I will need to then use 
!     D2%Clear() at some point, just like I need to use D1%Clear().
!
!  3) Related to 2), What if I did something really wacky like....
! 
!        DO I = 1, 100000
!           D2 = D1
!        END DO
!
!     Does each assignment essentially overwrite the previous one?  Or does each
!     assignment allocate a new chunk of data storage, resulting in a massive memory leak?
!     Gotta know.  Well, I did some searching, and found a discussion that pointed me toward 
!     the relevant portion of the Fortran 2003 standard. Specifically, in section 7.4.1.3
!     of the final version (May 2004, J3/04-007) it answers the question. It defines
!     assignment as being of the form 
!         variable = expr
!     then says (in part):
!         If variable is or becomes an unallocated allocatable variable, then it is 
!         allocated with each deferred type parameter equal to the corresponding type 
!         parameters of expr , with the shape of expr , and with each lower bound equal
!         to the corresponding element of LBOUND(expr).
!
!     So that allocatable array in D2 gets deallocated, then reallocated with the correct
!     size.  Exactly what I would hope. We are safe.
!
!  4) The research to answer questions 2 and 3 revealed a behavior of Fortran 
!     that I previously did not know.
!
!     Assume the declaration
!        INTEGER, DIMENSION(:), ALLOCATABLE :: A, B
!     Then assume A gets allocated and stuff assigned to it, but B is never referenced 
!     until we do this:
!        B = A
!     I would have expected an error, but that language in the standard says it is ok.
!     B will be automatically allocated to the same size as A and then the contents copied.
!     That is something to keep in mind as this coding progresses.
!
!-----------------------------------------------------------------------


!=======================================================================================
!  TDlyData is a basic structured type for storing a single timeseries of daily data. 
!  It will be used by other classes to store their data.
!
!  I am leaving the members PUBLIC, and avoiding the need for setter/getter methods. 
!  I hope this doesn't come back to bite me. I think it will keep the code slightly
!  easier to follow for programmers down the line who may be less familiar with OOP 
!  concepts. 
!
!  Note that static variables within the class are given initial values, which will
!  assist in determining if the object has ever been used.
!=======================================================================================
MODULE DailyDataTimeSeries
      USE GlerlDataTypesAndUnits
      USE ErrorProcess
      USE GLSHFS_Util
      IMPLICIT NONE

      PRIVATE     ! hide things by default. PUBLIC will be set as needed.
      TYPE, PUBLIC :: TDlyData
            INTEGER :: DataType    = GDT_UNDEFINED
            INTEGER :: DataUnits   = GDU_UNDEFINED
            INTEGER :: SDate       = MissingData_Date
            INTEGER :: EDate       = MissingData_Date 
            INTEGER :: NumDays     = 0
            LOGICAL :: DataIsValid = .FALSE.
            REAL, DIMENSION(:), ALLOCATABLE :: DataVals
         CONTAINS
            PROCEDURE, PRIVATE, PASS :: Initialize
            PROCEDURE, PUBLIC        :: Clear
            PROCEDURE, PUBLIC        :: SetValues
            PROCEDURE, PUBLIC        :: CopyFrom, CopyTo
            PROCEDURE, PUBLIC        :: PrintHeaderInfo
            PROCEDURE, PUBLIC        :: PrintData
      END TYPE TDlyData
      
      !
      !  Define a type-bound "constructor" method.
      !  This will be accessed by something like: X = TDlyData()
      !
      INTERFACE TDlyData
         PROCEDURE Create
      END INTERFACE TDlyData
      
CONTAINS
      !--------------------------------------------------------
      FUNCTION Create() RESULT(DD)
         IMPLICIT NONE
         TYPE (TDlyData) :: DD
         CALL DD%Initialize()
      END FUNCTION Create
      
      !--------------------------------------------------------
      SUBROUTINE Initialize(this)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER :: IOS
         this%DataType    = GDT_Undefined
         this%DataUnits   = GDU_Undefined
         this%SDate       = MissingData_Date
         this%EDate       = MissingData_Date
         this%NumDays     = 0
         this%DataIsValid = .FALSE.
         IF (ALLOCATED(this%DataVals)) DEALLOCATE(this%DataVals, STAT=IOS)
      END SUBROUTINE Initialize
         
      !--------------------------------------------------------
      SUBROUTINE Clear(this)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         CALL this%Initialize()
      END SUBROUTINE Clear

      
      !--------------------------------------------------------
      !  Assign data to anything/everything
      !--------------------------------------------------------
      FUNCTION SetValues(this, DataType, DataUnits, Start, Finish, DataArray)  Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         LOGICAL :: Flag
         INTEGER, OPTIONAL, INTENT(IN) :: DataType, DataUnits, Start, Finish
         REAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: DataArray
         INTEGER :: IOS, DSize

         !
         !  Directly use the dummy arguments
         !         
         IF (PRESENT(DataType))  this%DataType  = DataType
         IF (PRESENT(DataUnits)) this%DataUnits = DataUnits
         IF (PRESENT(Start))     this%SDate     = Start
         IF (PRESENT(Finish))    this%EDate     = Finish
         
         !
         !  If we have both start and end dates, we can calculate the
         !  time-series length, and allocate the necessary RAM.
         !  If there was existing data in the DataVals array, it will be lost.
         !  I make no attempt to preserve things in place.
         !  If the allocation fails, the object is just reinitialized (freeing
         !  any allocated RAM) and we return to calling procedure with the
         !  ErrorLevel value set to 1.
         !
         IF ((this%SDate .NE. MissingData_Date) .AND. (this%EDate .NE. MissingData_Date)) THEN
            this%NumDays = this%EDate - this%SDate + 1
            IF (ALLOCATED(this%DataVals)) DEALLOCATE(this%DataVals, STAT=IOS)
            ALLOCATE(this%DataVals(this%NumDays), STAT=IOS)
            IF (IOS .NE. 0) THEN
               WRITE(ErrorMessage, 1002);  CALL PassMsg
               CALL this%Initialize()
               GOTO 899
            END IF
            this%DataVals(:) = MissingData_Real
         END IF

         !
         !  If the user passed in a data array:
         !   1) Make sure it matches (length-wise) the defined start/end dates 
         !   2) Clear any old data and then use this new data to build a full
         !      time-series of data.
         !
         IF (PRESENT(DataArray)) THEN
            DSize = UBOUND(DataArray,1)
            IF (DSize .NE. this%NumDays) THEN
               WRITE(ErrorMessage, 1001);  CALL PassMsg
               CALL this%Initialize()
               GOTO 899
            END IF
         
            IF (ALLOCATED(this%DataVals)) DEALLOCATE(this%DataVals, STAT=IOS)
            ALLOCATE(this%DataVals(DSize), STAT=IOS)
            IF (IOS .NE. 0) THEN
               WRITE(ErrorMessage, 1002);  CALL PassMsg
               CALL this%Initialize()
               GOTO 899
            END IF
            this%DataVals(:) = DataArray(:)
            this%DataIsValid = .TRUE.
         END IF
         
         Flag = .TRUE.
         RETURN
         
         899  Flag = .FALSE.
              ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyData%SetValues()...';  CALL PassMsg
              RETURN
              
         1001 FORMAT('Mismatch in size between the array data and the date extents')
         1002 FORMAT('Error allocating memory for a dataset')
      END FUNCTION SetValues
      
      
      !--------------------------------------------------------
      !  Copy data from another object to this one.  Note that
      !  we make a full new copy of the data values.
      !--------------------------------------------------------
      FUNCTION CopyFrom(this, Src) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this, Src
         LOGICAL :: Flag
         INTEGER :: IOS
         this%DataIsValid = Src%DataIsValid
         this%SDate       = Src%SDate
         this%EDate       = Src%EDate
         this%NumDays     = Src%NumDays
         this%DataType    = Src%DataType
         this%DataUnits   = Src%DataUnits
         IF (ALLOCATED(this%DataVals)) DEALLOCATE(this%DataVals, STAT=IOS)
         ALLOCATE(this%DataVals(this%NumDays), STAT=IOS)
         IF (IOS .NE. 0) THEN
            WRITE(ErrorMessage, 1001);  CALL PassMsg
            CALL this%Initialize()
            GOTO 899
         END IF
         this%DataVals(:) = Src%DataVals(:)
         Flag = .TRUE.
         RETURN
         
         899  Flag = .FALSE.
              ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyData%CopyFrom()...';  CALL PassMsg
              RETURN
              
         1001 FORMAT('Error allocating memory for a dataset')
      END FUNCTION CopyFrom
         
      !--------------------------------------------------------
      !  Copy from this object to another object.  Note that the 
      !  other object must already exist.  Rather than make whole
      !  new routine, I simply transfer control to the other 
      !--------------------------------------------------------
      FUNCTION CopyTo(this, Dest) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this, Dest
         LOGICAL :: Flag
         Flag = CopyFrom(Dest, this)
         IF (.NOT. Flag) GOTO 899
         RETURN         
        
         899  Flag = .FALSE.
              ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyData%CopyTo()...';  CALL PassMsg
      END FUNCTION CopyTo
         
      !--------------------------------------------------------
      SUBROUTINE PrintHeaderInfo(this)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         PRINT*, 'SDate=',     this%SDate
         PRINT*, 'EDate=',     this%EDate
         PRINT*, 'NumDays=',   this%NumDays
         PRINT*, 'DataType=',  GlerlDataTypeString(this%DataType)
         PRINT*, 'DataUnits=', GlerlDataUnitString(this%DataUnits)
      END SUBROUTINE PrintHeaderInfo

      !--------------------------------------------------------
      SUBROUTINE PrintData(this)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER :: I
         DO I = 1, this%NumDays
            WRITE(*, 1001) I, this%DataVals(I)
         END DO
         
         1001 FORMAT('DataVals(',I0,') = ', F9.3)
      END SUBROUTINE PrintData

END MODULE DailyDataTimeSeries

