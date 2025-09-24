!-----------------------------------------------------------------------
!  Programmer: Tim Hunter,  22 Sep 2016
!
!  This module provides structured storage container for sets of daily 
!  data such as would be associated with a single station, a single 
!  lake/basin/subbasin, a collection of stations, etc.
!
!
!
!-------------------------------------------------------------------------
!  Discussion of programming-specific topics:
!
!  This code relies on the object-oriented functionality added in Fortran 2003, so you
!  MUST use a compiler that supports that version of the Fortran standard.
!
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
!         Flag=D1%SetValues(DataType=GDT_AirtempMax, DataUnit=GDU_Celsius,          &
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
!-------------------------------------------------------------------------
MODULE DailyDataStructures
      USE GlerlDataTypesAndUnits
      USE ErrorProcess
      USE GLSHFS_Util
      IMPLICIT NONE

      PRIVATE     ! hide everything by default. PUBLIC will be set as needed.
      
      !=======================================================================================
      !  TDlyData is a basic structured type for storing a single timeseries of daily data. 
      !  It will be used by other classes to store their data.
      !
      !  I was going to leave the data members public, thinking it would keep the code a
      !  little easier for inexperienced OOP programmers, but it causes issues with internal
      !  consistency, so I will have to use setter/getter methods. Probably better this way, 
      !  anyway, since it is more "OOP-like". It also helps protect against doing bad things.
      !
      !  Note that static variables within the class are given initial values, which will
      !  assist in determining if the object has ever been used.
      !=======================================================================================
      TYPE, PUBLIC :: TDlyData
            PRIVATE
            INTEGER :: DataType    = GDT_UNDEFINED
            INTEGER :: DataUnit    = GDU_UNDEFINED
            INTEGER :: SDate       = MissingData_Date
            INTEGER :: EDate       = MissingData_Date 
            INTEGER :: NumDays     = 0
            LOGICAL :: DataIsValid = .FALSE.
            REAL, DIMENSION(:), ALLOCATABLE :: DataVals
         CONTAINS
            FINAL :: Delete_TDD
         
            PROCEDURE, PRIVATE, PASS :: Initialize    => Initialize_TDD
            PROCEDURE, PUBLIC        :: Clear         => Clear_TDD
            PROCEDURE, PUBLIC        :: CopyFrom      => CopyFrom_TDD
            PROCEDURE, PUBLIC        :: CopyTo        => CopyTo_TDD
            PROCEDURE, PUBLIC        :: AdjustPeriod  => AdjustPeriod_TDD
            PROCEDURE, PUBLIC        :: TrimToValid   => TrimToValid_TDD
            
            PROCEDURE, PUBLIC        :: GetDataType   => GetDataType_TDD
            PROCEDURE, PUBLIC        :: GetDataUnit   => GetDataUnit_TDD
            PROCEDURE, PUBLIC        :: GetStartDate  => GetStartDate_TDD
            PROCEDURE, PUBLIC        :: GetEndDate    => GetEndDate_TDD
            PROCEDURE, PUBLIC        :: GetNumDays    => GetNumDays_TDD
            PROCEDURE, PUBLIC        :: GetValidity   => GetValidity_TDD
            PROCEDURE, PUBLIC        :: GetDataVals   => GetDataVals_TDD
            PROCEDURE, PUBLIC        :: GetDataVal    => GetDataVal_TDD
            
            PROCEDURE, PUBLIC        :: SetDataType   => SetDataType_TDD
            PROCEDURE, PUBLIC        :: SetDataUnit   => SetDataUnit_TDD
            PROCEDURE, PUBLIC        :: SetDataValue  => SetDataValue_TDD
            PROCEDURE, PUBLIC        :: AssignData    => AssignData_TDD   
            PROCEDURE, PUBLIC        :: ClearData     => ClearData_TDD  
            PROCEDURE, PUBLIC        :: MergeData     => MergeData_TDD            
            PROCEDURE, PUBLIC        :: PrintSummaryInfo  => PrintSummaryInfo_TDD
      END TYPE TDlyData
      
      !  Define a type-bound "constructor" method.
      !  This will be accessed with syntax of: X = TDlyData()
      INTERFACE TDlyData
         PROCEDURE Create_TDD
      END INTERFACE TDlyData

      !
      !  Node structure for building a linked list of TDlyData objects
      !
      TYPE, PRIVATE :: DDnode
         TYPE(TDlyData) :: DD
         TYPE(DDnode), POINTER :: Next
      END TYPE DDnode


      !=================================================================================
      !  All of the following types are similar. The only difference is the metadata
      !  items. Thus, I am using a base class for most of the functionality and then 
      !  extending it as needed.
      !
      !  These objects all store the data (a collection of TDlyDataset objects) for a 
      !  single "location". Location being a single station, or subbasin, or lake basin.
      !  Each TDlyDataset object must have a unique datatype.  e.g. there can only be
      !  ONE dataset with precipitation, ONE with windspeed, etc.
      !
      !  One more requirement that will make things much simpler when using these data 
      !  objects:  All of the component datasets must have the same period of record.
      !  This may require padding of the records (with missing data values) on either
      !  end.
      !=================================================================================
      TYPE, PUBLIC :: TDlyDataOneLocation
            CHARACTER(LEN=100) :: Description
            INTEGER :: SDateSeq    = MissingData_Date     ! Start date of the time-series data (all datasets match)
            INTEGER :: EDateSeq    = MissingData_Date     ! End date of the time-series data (all datasets match)
            INTEGER :: NewDataSSeq = MissingData_Date     ! Earliest new data that has been added; Only used for some subtypes
            INTEGER :: NumDays     = 0                    ! Number of days in the date extent. All individual datasets must conform.
            INTEGER :: NumDatasets = 0
            TYPE (DDnode), POINTER, PRIVATE :: DatasetList
         CONTAINS
            FINAL  :: Delete_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: SetDescription
            PROCEDURE, PASS(this), PUBLIC   :: GetStartDate
            PROCEDURE, PASS(this), PUBLIC   :: GetEndDate
            PROCEDURE, PASS(this), PUBLIC   :: GetNewDataStartDate
            PROCEDURE, PASS(this), PUBLIC   :: GetNumDays
            PROCEDURE, PASS(this), PUBLIC   :: GetNumDataSets
            PROCEDURE, PASS(this), PRIVATE  :: Initialize_TDOL
            PROCEDURE, PASS(this), PRIVATE  :: ClearTheDatasetList_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: Clear         => Clear_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: CopyMetaFrom  => CopyMetaFrom_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: CopyFrom      => CopyFrom_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: CopyTo        => CopyTo_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: AddDataset    => AddDataset_TDOL
            GENERIC,               PUBLIC   :: MergeData     => MergeTDD_TDOL, MergeTDOL_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: AlignPeriods  => AlignPeriods_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: GetDatasetPtr => GetDatasetPtr_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: GetPointerToDataOfType  => GetPointerToDataOfType_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: GetPointerToDataByIndex => GetPointerToDataByIndex_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: GetIndexForDataOfType   => GetIndexForDataOfType_TDOL
            PROCEDURE, PASS(this), PUBLIC   :: PrintSummaryInfo => PrintSummaryInfo_TDOL
            
            PROCEDURE, PASS(this), PRIVATE  :: MergeTDD_TDOL
            PROCEDURE, PASS(this), PRIVATE  :: MergeTDOL_TDOL
      END TYPE TDlyDataOneLocation
      
      INTERFACE TDlyDataOneLocation
         PROCEDURE Create_TDOL
      END INTERFACE TDlyDataOneLocation
     
      
      !---------------------------------------------------------------------------------
      !  The met station structure contains information about the station
      !  (ID, location, etc), plus all of the data timeseries
      !
      TYPE, EXTENDS (TDlyDataOneLocation), PUBLIC :: TDlyDataMetStn
            CHARACTER(LEN=25) :: StnID
            CHARACTER(LEN=80) :: StnName
            REAL    :: Latitude  = MissingData_Real   ! decimal degrees North of equator (neg = southern hemisphere)
            REAL    :: Longitude = MissingData_Real   ! decimal degrees West of Prime Meridian (GL values > 0)
            REAL    :: Elevation = MissingData_Real   ! meters
         CONTAINS
            PROCEDURE, PASS(this), PRIVATE  :: Initialize_TDMS
            PROCEDURE, PASS(this), PUBLIC   :: Clear => Clear_TDMS
      END TYPE TDlyDataMetStn
      
      INTERFACE TDlyDataMetStn
         PROCEDURE Create_TDMS
      END INTERFACE TDlyDataMetStn
     
      !---------------------------------------------------------------------------------
      !  The ForSubbasin structure contains information from a single subbasin
      !
      TYPE, EXTENDS (TDlyDataOneLocation), PUBLIC :: TDlyDataForSubbasin
            CHARACTER(LEN=3) :: Bsn         = 'xxx'              ! which lake?
            INTEGER          :: SubNum      = MissingData_Int    ! the subbasin number
            REAL             :: SubArea     = MissingData_Real   ! area in sq meters
         CONTAINS
            PROCEDURE, PASS(this), PRIVATE  :: Initialize_TDFS
            PROCEDURE, PASS(this), PUBLIC   :: Clear => Clear_TDFS
            END TYPE TDlyDataForSubbasin
      
      INTERFACE TDlyDataForSubbasin
         PROCEDURE Create_TDFS
      END INTERFACE TDlyDataForSubbasin
     
     
      !---------------------------------------------------------------------------------
      !  The ForLake structure contains information from a single lake (or lake basin)
      !
      TYPE, EXTENDS (TDlyDataOneLocation), PUBLIC :: TDlyDataForLake
            CHARACTER(LEN=3)  :: Bsn  = 'xxx'       ! which lake (3-char code)
         CONTAINS
            PROCEDURE, PASS(this), PRIVATE  :: Initialize_TDFL
            PROCEDURE, PASS(this), PUBLIC   :: Clear => Clear_TDFL
      END TYPE TDlyDataForLake
      
      INTERFACE TDlyDataForLake
         PROCEDURE Create_TDFL
      END INTERFACE TDlyDataForLake
     
     
CONTAINS
      !========================================================================================
      !========================================================================================
      !  Implementation procedures for the class: TDlyData
      !========================================================================================
      !========================================================================================
      !--------------------------------------------------------
      FUNCTION Create_TDD() RESULT(this)
         IMPLICIT NONE
         TYPE (TDlyData) :: this
         this%NumDays = 0
         CALL this%Initialize()
      END FUNCTION Create_TDD
      
      !--------------------------------------------------------
      SUBROUTINE Delete_TDD(this)
         IMPLICIT NONE
         TYPE (TDlyData), INTENT(INOUT) :: this
         INTEGER :: IOS
         IF (ALLOCATED(this%DataVals)) DEALLOCATE(this%DataVals, STAT=IOS)
      END SUBROUTINE Delete_TDD
      
      !--------------------------------------------------------
      SUBROUTINE Initialize_TDD(this)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         CALL this%Clear()
      END SUBROUTINE Initialize_TDD
         
      !--------------------------------------------------------
      SUBROUTINE Clear_TDD(this)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER :: IOS
         this%DataType    = GDT_Undefined
         this%DataUnit    = GDU_Undefined
         this%SDate       = MissingData_Date
         this%EDate       = MissingData_Date
         this%NumDays     = 0
         this%DataIsValid = .FALSE.
         IF (ALLOCATED(this%DataVals)) DEALLOCATE(this%DataVals, STAT=IOS)
      END SUBROUTINE Clear_TDD

      !--------------------------------------------------------
      !  "Get" methods
      !--------------------------------------------------------
      FUNCTION GetDataType_TDD(this)  Result(DType)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER :: DType
         DType = this%DataType
      END FUNCTION GetDataType_TDD
         
      FUNCTION GetDataUnit_TDD(this)  Result(DUnit)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER :: DUnit
         DUnit = this%DataUnit
      END FUNCTION GetDataUnit_TDD
         
      FUNCTION GetStartDate_TDD(this)  Result(Seq)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER :: Seq
         Seq = this%SDate
      END FUNCTION GetStartDate_TDD
         
      FUNCTION GetEndDate_TDD(this)  Result(Seq)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER :: Seq
         Seq = this%EDate
      END FUNCTION GetEndDate_TDD
         
      FUNCTION GetNumDays_TDD(this)  Result(NDays)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER :: NDays
         NDays = this%NumDays
      END FUNCTION GetNumDays_TDD
         
      FUNCTION GetValidity_TDD(this)  Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         LOGICAL :: Flag
         Flag = this%DataisValid
      END FUNCTION GetValidity_TDD
         
      FUNCTION GetDataVals_TDD(this)  Result(A)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         REAL, DIMENSION(:), ALLOCATABLE :: A
         INTEGER :: IOS, Sz
         Sz = UBOUND(this%DataVals, 1)
         ALLOCATE(A(Sz), STAT=IOS)
         IF (IOS .NE. 0) THEN
            ErrorMessage = 'Error allocating memory for data array.'; CALL PassMsg
            ErrorMessage = '[traceback] GetDataVals_TDD()...'; CALL PassMsg
            ErrorLevel = 1
            RETURN
         END IF
         A(:) = this%DataVals(:)
      END FUNCTION GetDataVals_TDD
      
      
      FUNCTION GetDataVal_TDD(this, I)  Result(V)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER, INTENT(IN) :: I
         REAL :: V
         IF ((I .GE. 1) .AND. (I .LE. this%NumDays)) THEN
            V = this%DataVals(I)
         ELSE
            V = MissingData_Real
         END IF
      END FUNCTION GetDataVal_TDD
         

      !--------------------------------------------------------
      !  Basic Setters
      !--------------------------------------------------------
      FUNCTION SetDataType_TDD(this, DataType)  Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER, INTENT(IN) :: DataType
         LOGICAL :: Flag
         this%DataType = DataType
         Flag = .TRUE.
      END FUNCTION SetDataType_TDD

      FUNCTION SetDataUnit_TDD(this, DataUnit)  Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER, INTENT(IN) :: DataUnit
         LOGICAL :: Flag
         this%DataUnit = DataUnit
         Flag = .TRUE.
      END FUNCTION SetDataUnit_TDD
      
      !--------------------------------------------------------
      !  The setter method for assigning a single value in the data array. 
      !  Kinda messy and certainly slower than directly accessing the 
      !  array, but hopefully still acceptable in terms of performance.
      !--------------------------------------------------------
      SUBROUTINE SetDataValue_TDD(this, I, NewVal)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER, INTENT(IN) :: I
         REAL,    INTENT(IN) :: NewVal
         IF (.NOT. this%DataIsValid) GOTO 880
         IF (this%NumDays .EQ. 0)    GOTO 880
         IF ((I .LT. 1) .OR. (I .GT. UBOUND(this%DataVals,1))) GOTO 885
         this%DataVals(I) = NewVal
         RETURN

         880 WRITE(ErrorMessage, 1001);    CALL PassMsg;  GOTO 898
             CALL PassMsg;  GOTO 898
         885 WRITE(ErrorMessage, 1002) I;  CALL PassMsg;  GOTO 898
             
         898 ErrorLevel = 1
             ErrorMessage = '[traceback] TDlyData%SetDataValue()...'; CALL PassMsg
             RETURN
             
        1001 FORMAT('Error assigning data to array. The array is not valid.')
        1002 FORMAT('Error. Invalid array index (', I0, ') passed.')
         
      END SUBROUTINE SetDataValue_TDD
         
         
      !--------------------------------------------------------
      !  The setter method for the data array. A bit more complicated, due to all
      !  of the error checking/handling.
      !--------------------------------------------------------
      FUNCTION AssignData_TDD(this, Start, Finish, NewVals)  Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER,            INTENT(IN) :: Start, Finish
         REAL, DIMENSION(:), INTENT(IN) :: NewVals
         LOGICAL :: Flag
         INTEGER :: IOS, DSize

         !
         !  First, clear out old contents (data and dates, not station metadata)
         !
         Flag = this%ClearData()

         !
         !  If we have both start and end dates, we can calculate the
         !  time-series length, and allocate the necessary RAM.
         !  If there was existing data in the DataVals array, it will be lost.
         !  I make no attempt to preserve things in place.
         !  If the allocation fails, the object is just reinitialized (freeing
         !  any allocated RAM) and we return to calling procedure with the
         !  ErrorLevel value set to 1.
         !
         IF ((Start .NE. MissingData_Date) .AND. (Finish .NE. MissingData_Date)) THEN
            this%SDate = Start
            this%EDate = Finish
            this%NumDays = this%EDate - this%SDate + 1

            DSize = UBOUND(NewVals,1)
            IF (DSize .NE. this%NumDays) THEN
               WRITE(ErrorMessage, 1001);  CALL PassMsg
               WRITE(ErrorMessage, 1011) DSize, this%NumDays;  CALL PassMsg
               this%SDate = MissingData_Date
               this%EDate = MissingData_Date
               this%NumDays = 0
               GOTO 899
            END IF

            ALLOCATE(this%DataVals(this%NumDays), STAT=IOS)
            IF (IOS .NE. 0) THEN
               WRITE(ErrorMessage, 1002);  CALL PassMsg
               this%SDate = MissingData_Date
               this%EDate = MissingData_Date
               this%NumDays = 0
               GOTO 899
            END IF
            this%DataVals(:) = NewVals(:)
         END IF

         this%DataIsValid = .TRUE.
         Flag = .TRUE.
         RETURN
         
         899  Flag = .FALSE.
              ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyData%AssignData()...';  CALL PassMsg
              RETURN
              
         1001 FORMAT('Mismatch in size between the array size and the date extents')
         1002 FORMAT('Error allocating memory for a dataset')
         1011 FORMAT('  Array size = ', I0, ';  Calculated # of days = ', I0)
      END FUNCTION AssignData_TDD

      !--------------------------------------------------------
      !  The setter method for clearing the data array.
      !--------------------------------------------------------
      FUNCTION ClearData_TDD(this)  Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         LOGICAL :: Flag
         INTEGER :: IOS
         IF (ALLOCATED(this%DataVals)) DEALLOCATE(this%DataVals, STAT=IOS)
         this%SDate   = MissingData_Date
         this%EDate   = MissingData_Date
         this%NumDays = 0
         this%DataIsValid = .FALSE.
         Flag = .TRUE.
         RETURN
      END FUNCTION ClearData_TDD

      
      !--------------------------------------------------------
      !  Copy data from another object to this one.  Note that
      !  we make a full new copy of the data values.
      !  If the data array is invalid, then set the metadata
      !  appropriately.
      !--------------------------------------------------------
      FUNCTION CopyFrom_TDD(this, Src) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this, Src
         LOGICAL :: Flag
         INTEGER :: IOS
  
         this%DataType  = Src%DataType
         this%DataUnit  = Src%DataUnit
         Flag = this%ClearData()
         
         IF (ALLOCATED(Src%DataVals)) THEN
            this%SDate    = Src%GetStartDate()
            this%EDate    = Src%GetEndDate()
            this%NumDays  = Src%NumDays
            IF (Src%NumDays .GT. 0) THEN
               ALLOCATE(this%DataVals(Src%NumDays), STAT=IOS)
               IF (IOS .NE. 0) THEN
                  WRITE(ErrorMessage, 1001);  CALL PassMsg
                  CALL this%Clear()
                  GOTO 899
               END IF
               this%DataVals = Src%DataVals
               this%DataIsValid = .TRUE.
            END IF
         ELSE
            this%SDate    = MissingData_Date
            this%EDate    = MissingData_Date
            this%NumDays  = 0
            this%DataIsValid = .FALSE.
         END IF
         
         Flag = .TRUE.
         RETURN
         
         899  Flag = .FALSE.
              ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyData%CopyFrom()...';  CALL PassMsg
              RETURN
              
         1001 FORMAT('Error allocating memory for a dataset')
      END FUNCTION CopyFrom_TDD
         
      !--------------------------------------------------------
      !  Copy from this object to another object.  Note that the 
      !  other object must already exist.  Rather than make whole
      !  new routine, I simply transfer control to the other 
      !--------------------------------------------------------
      FUNCTION CopyTo_TDD(this, Dest) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this, Dest
         LOGICAL :: Flag
         Flag = CopyFrom_TDD(Dest, this)
         IF (.NOT. Flag) GOTO 899
         RETURN         
        
         899  Flag = .FALSE.
              ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyData%CopyTo()...';  CALL PassMsg
      END FUNCTION CopyTo_TDD


      !--------------------------------------------------------
      !  Adjust the date extents, padding or removing data as needed.
      !--------------------------------------------------------
      SUBROUTINE AdjustPeriod_TDD(this, NewSSeq, NewESeq)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         INTEGER         :: NewSSeq, NewESeq
         INTEGER :: I, J, IOS, NewDays
         INTEGER :: Seq, CopySSeq, CopyESeq
         REAL, DIMENSION(:), ALLOCATABLE :: Temp
         
         !
         !  Make sure the requested dates are valid
         !
         IF (NewSSeq .EQ. MissingData_Date) RETURN
         IF (NewESeq .EQ. MissingData_Date) RETURN

         !
         !  Make sure there is existing data
         !
         IF (this%NumDays .EQ. 0) RETURN
         IF (this%SDate .EQ. MissingData_Date) RETURN
         IF (this%EDate .EQ. MissingData_Date) RETURN
         
         !
         !
         NewDays = NewESeq - NewSSeq + 1
         ALLOCATE(Temp(NewDays), STAT=IOS)
         IF (IOS .NE. 0) THEN
            WRITE(ErrorMessage, 1001);  CALL PassMsg
            CALL this%Clear()
            GOTO 899
         END IF
         Temp(:) = MissingData_Real

         !
         !  Find the dates from which we can copy old data into
         !  the new array.
         !
         CopySSeq = MAX(this%SDate, NewSSeq)
         CopyESeq = MIN(this%EDate, NewESeq)
         
         !
         !  Copy data into the new array
         !
         DO Seq = CopySSeq, CopyESeq
            I = Seq - this%SDate + 1
            J = Seq - NewSSeq    + 1
            Temp(J) = this%DataVals(I)
         END DO
         
         !
         !  Replace the old DataVals array with the Temp array
         !
         DEALLOCATE(this%DataVals, STAT=IOS)
         ALLOCATE(this%DataVals(NewDays), STAT=IOS)
         IF (IOS .NE. 0) THEN
            WRITE(ErrorMessage, 1002);  CALL PassMsg
            CALL this%Clear()
            GOTO 899
         END IF
         this%DataVals(:) = Temp(:)
         DEALLOCATE(Temp, STAT=IOS)
 
         !
         !  Update the date fields
         !
         this%SDate   = NewSSeq
         this%EDate   = NewESeq
         this%NumDays = NewDays
         
         RETURN
         
         899  ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyData%AdjustPeriod()...';  CALL PassMsg
              DEALLOCATE(Temp, STAT=IOS)
              RETURN
              
         1001 FORMAT('Error allocating memory for a temporary dataset')
         1002 FORMAT('Error allocating memory for the new permanent dataset')
      END SUBROUTINE AdjustPeriod_TDD
         
         
      !--------------------------------------------------------
      !  Adjust the date extents and data array to reflect only 
      !  the period with valid data.
      !--------------------------------------------------------
      SUBROUTINE TrimToValid_TDD(this)         
         CLASS(TDlyData) :: this
         INTEGER :: NewSSeq, NewESeq
         INTEGER :: I, IOS, Seq
         LOGICAL :: Found

         !
         !  Find the new valid period.
         !
         NewSSeq = this%EDate
         NewESeq = this%SDate
         
         Found = .FALSE.
         Seq = this%SDate
         DO WHILE ((.NOT. Found) .AND. (Seq .LT. this%EDate))
            I = Seq - this%SDate + 1
            IF (.NOT. IsMissing(this%DataVals(I))) THEN
               NewSSeq = Seq
               Found = .TRUE.
            END IF
            Seq = Seq + 1
         END DO

         Found = .FALSE.
         Seq = this%EDate
         DO WHILE ((.NOT. Found) .AND. (Seq .GT. this%SDate))
            I = Seq - this%SDate + 1
            IF (.NOT. IsMissing(this%DataVals(I))) THEN
               NewESeq = Seq
               Found = .TRUE.
            END IF
            Seq = Seq - 1
         END DO

         !
         !
         IF (NewSSeq .GT. NewESeq) THEN
            this%SDate = MissingData_Date
            this%EDate = MissingData_Date
            this%NumDays = 0
            DEALLOCATE(this%DataVals, STAT=IOS)
            this%DataIsValid = .FALSE.
            RETURN
         END IF

         !
         !  
         CALL this%AdjustPeriod(NewSSeq, NewESeq); IF (ErrorLevel .NE. 0) GOTO 899
         
         !
         RETURN
         
         899  ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyData%TrimIt()...'; CALL PassMsg
              RETURN

      END SUBROUTINE TrimToValid_TDD
      
      
      !--------------------------------------------------------
      !  Merge the supplied data set with "this". The data in the
      !  new data set will be preferentially used over the data values
      !  in the existing one.
      !  The NewD object is left unchanged.
      !--------------------------------------------------------
      FUNCTION MergeData_TDD(this, NewD) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyData) :: this, NewD
         LOGICAL :: Flag
         INTEGER :: I, J, IOS, SSeq, ESeq, Seq, NDays
         REAL    :: CFact
         REAL, DIMENSION(:), ALLOCATABLE :: Temp
         
         !
         IF (this%DataType .NE. NewD%DataType) THEN
            WRITE(ErrorMessage, 1001) TRIM(GlerlDataTypeString(NewD%DataType)),               &
                                      TRIM(GlerlDataTypeString(this%DataType)); CALL PassMsg
            GOTO 899
         END IF

         !
         !  If the new dataset has no data, no need to mess with it.
         !  Return value is TRUE to indicate successful completion.
         !
         IF ((NewD%SDate .EQ. MissingData_Date) .OR. (NewD%EDate .EQ. MissingData_Date)) THEN
            Flag = .TRUE.
            RETURN
         END IF
         
         !
         !  What will be the combined period of record?
         !
         SSeq = MIN(this%SDate, NewD%SDate)
         ESeq = MAX(this%EDate, NewD%EDate)
         
         !
         !  Find the conversion factor for the new data
         !  (i.e. if it is in a different units)
         !
         CFact = UnitConvertedDataValue(1.0, this%DataUnit, NewD%DataUnit)
         IF (CFact .EQ. MissingData_Real) THEN
            WRITE(ErrorMessage, 1002) TRIM(GlerlDataUnitString(NewD%DataUnit)),               &
                                      TRIM(GlerlDataUnitString(this%DataUnit)); CALL PassMsg
            GOTO 899
         END IF

         !
         !  Allocate temporary simple array for the merged data
         !
         NDays = ESeq - SSeq + 1         
         ALLOCATE(Temp(NDays), STAT=IOS)
         IF (IOS .NE. 0) THEN
            WRITE(ErrorMessage, 1003) NDays; CALL PassMsg
            GOTO 899
         END IF
         Temp(:) = MissingData_Real

         !
         !  Merge the data sets
         !  1) Copy the old data values into the temporary array
         !  2) Copy the new data values, overwriting old ones if required
         !
         DO Seq = this%SDate, this%EDate
            I = Seq - SSeq + 1
            J = Seq - this%SDate + 1
            Temp(I) = this%DataVals(J)
         END DO
         
         DO Seq = NewD%SDate, NewD%EDate
            I = Seq - SSeq + 1
            J = Seq - NewD%SDate + 1
            Temp(I) = NewD%DataVals(J) * CFact
         END DO

         !
         !  Update the object members to reflect the merged dataset
         !
         DEALLOCATE(this%DataVals, STAT=IOS)
         ALLOCATE(this%DataVals(NDays), STAT=IOS)
         IF (IOS .NE. 0) THEN
            WRITE(ErrorMessage, 1003) NDays; CALL PassMsg
            GOTO 899
         END IF
         
         this%SDate       = SSeq
         this%EDate       = ESeq
         this%NumDays     = NDays
         this%DataVals(:) = Temp(:) 
         this%DataIsValid = .TRUE.

         !
         !  Clean up
         !         
         DEALLOCATE(Temp, STAT=IOS)
         Flag = .TRUE.
         RETURN
         
         899  Flag = .FALSE.
              ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyData%MergeData()...';  CALL PassMsg
              RETURN
              
         1001 FORMAT('Invalid attempt to merge data of type ', A, ' with existing data of type ', A)
         1002 FORMAT('Invalid attempt to merge data in ', A, ' with existing data in ', A)
         1003 FORMAT('Error allocating memory for temporary array of ', I0, ' days.')
      END FUNCTION MergeData_TDD
         

      !-----------------------------------------------------------------
      !  Print summary of contents. Mainly for debugging.
      !-----------------------------------------------------------------
      SUBROUTINE PrintSummaryInfo_TDD(this)
         IMPLICIT NONE
         CLASS(TDlyData) :: this
         CHARACTER(LEN=25) :: DTS
         DTS = GlerlDataTypeString(this%GetDataType())
         WRITE(ErrorMessage, 1005) TRIM(DTS), this%GetStartDate(), this%GetEndDate();  CALL PassMsg
         RETURN
         
         1005 FORMAT(4X, 'DType, SDate, EDate: ', A, 2I9)
      END SUBROUTINE PrintSummaryInfo_TDD


        


      !========================================================================================
      !========================================================================================
      !  Implementation procedures for the base class: TDlyDataOneLocation
      !========================================================================================
      !========================================================================================
      !--------------------------------------------------------
      !  Constructor for the TDlyDataOneLocation base class
      !--------------------------------------------------------
      FUNCTION Create_TDOL() RESULT(this)
         IMPLICIT NONE
         TYPE (TDlyDataOneLocation) :: this
         NULLIFY(this%DatasetList)
         this%NumDatasets = 0
         CALL Initialize_TDOL(this)
      END FUNCTION Create_TDOL
      
      !--------------------------------------------------------
      SUBROUTINE Delete_TDOL(this)
         IMPLICIT NONE
         TYPE (TDlyDataOneLocation) :: this
         IF (ASSOCIATED(this%DatasetList)) CALL ClearTheDatasetList_TDOL(this)         
      END SUBROUTINE Delete_TDOL
      
      !--------------------------------------------------------
      !  Initialize() for the base class
      !--------------------------------------------------------
      SUBROUTINE Initialize_TDOL(this)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         this%Description = ''
         this%SDateSeq    = MissingData_Date       ! Start date of the time-series data
         this%EDateSeq    = MissingData_Date       ! End date of the time-series data
         this%NewDataSSeq = MissingData_Date       ! Start date of the NEW data (not processed)
         this%NumDays     = 0                      ! Number of days in the date extent. The individual datasets may be different.
         RETURN
      END SUBROUTINE Initialize_TDOL
      
      !--------------------------------------------------------
      !  Clear() for the base
      !--------------------------------------------------------
      SUBROUTINE Clear_TDOL(this)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         CALL ClearTheDatasetList_TDOL(this)
         this%Description = '-'
         this%SDateSeq    = MissingData_Date
         this%EDateSeq    = MissingData_Date
         this%NewDataSSeq = MissingData_Date
         this%NumDays     = MissingData_Int
         RETURN
      END SUBROUTINE Clear_TDOL
               
       !--------------------------------------------------------
      SUBROUTINE ClearTheDatasetList_TDOL(this)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         TYPE(DDNode), POINTER :: ThisNode, NextNode, NodeToWipe
         
         IF (ASSOCIATED(this%DatasetList)) THEN
            ThisNode => this%DatasetList          ! set to root node (#1)
            NextNode => ThisNode%Next             ! the next node (#2)
            CALL ThisNode%DD%Clear()
            DO WHILE (ASSOCIATED(NextNode))
               NodeToWipe => NextNode
               CALL NextNode%DD%Clear()
               NextNode=>NextNode%Next
               DEALLOCATE(NodeToWipe)
               this%NumDatasets = this%NumDatasets - 1
            END DO
            DEALLOCATE(this%DatasetList)
            NULLIFY(this%DatasetList)
         END IF
         this%NumDatasets = 0
         this%SDateSeq    = MissingData_Date
         this%EDateSeq    = MissingData_Date
         this%NewDataSSeq = MissingData_Date
         this%NumDays     = 0
      END SUBROUTINE ClearTheDatasetList_TDOL

      !--------------------------------------------------------
      !  Assign data to the Description entry.  
      !--------------------------------------------------------
      FUNCTION SetDescription(this, Description)  Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         LOGICAL :: Flag
         CHARACTER(LEN=*),  INTENT(IN) :: Description
         this%Description = TRIM(Description)
         Flag = .TRUE.
         RETURN
      END FUNCTION SetDescription


      !--------------------------------------------------------
      !  Simple "Get" functions
      !--------------------------------------------------------
      FUNCTION GetStartDate(this)  Result(Seq)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         INTEGER :: Seq
         Seq = this%SDateSeq
      END FUNCTION

      FUNCTION GetEndDate(this)  Result(Seq)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         INTEGER :: Seq
         Seq = this%EDateSeq
      END FUNCTION

      FUNCTION GetNewDataStartDate(this)  Result(Seq)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         INTEGER :: Seq
         Seq = this%NewDataSSeq
      END FUNCTION

      FUNCTION GetNumDays(this)  Result(N)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         INTEGER :: N
         N = this%NumDays
      END FUNCTION

      FUNCTION GetNumDataSets(this)  Result(N)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         INTEGER :: N
         N = this%NumDataSets
      END FUNCTION


      !---------------------------------------------------------------------
      !  Get a pointer to the head node of the list
      !--------------------------------------------------------
      FUNCTION GetDatasetPtr_TDOL(this) Result(DSPtr)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         TYPE (DDNode), POINTER :: DSPtr
         IF (ASSOCIATED(this%DatasetList)) THEN
            DSPtr => this%DatasetList                ! head node of the dataset list
         ELSE
            NULLIFY(DSPtr)
         END IF
      END FUNCTION GetDatasetPtr_TDOL
         
     !--------------------------------------------------------
      !  Assign data to the user-settable metadata (header) entries.
      !
      !  The assumption is that this routine will be called simply to 
      !  set the basic header stuff prior to actually adding some 
      !  datasets to the object.
      !--------------------------------------------------------
      FUNCTION CopyMetaFrom_TDOL(this, Src)  Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this, Src
         LOGICAL :: Flag
         
         this%Description = TRIM(Src%Description)

         !
         !  Type-specific stuff.
         !  Notice how I have to use 2-layer SELECT statements, one 
         !  for "this" and one for "Src".
         !  This is necessary, albeit a little ugly. The compiler chokes
         !  without it. Apparently, within the "TYPE IS (x)" block, the
         !  variable of interest gets treated as being of type x. But without
         !  being inside that block, it gets treated as being of the type 
         !  specified for the dummy argument (TDlyDataOneLocation in this case).
         !  This behavior wasn't totally clear from what I was reading, but
         !  it became obvious through trial & error as I tried to get this to
         !  compile and run correctly.
         !
         !  By the way, I can't just define a CopyMeta_X routine for each of the
         !  child types. The compiler always interprets the Src argument as being
         !  of type TDlyDataOneLocation, and won't compile due to the "mismatched"
         !  argument types.
         !
         !  (Tim Hunter - 13 Oct 2016)
         !
         SELECT TYPE (this)
            TYPE IS (TDlyDataMetStn)
               SELECT TYPE (Src)
                  TYPE IS (TDlyDataMetStn)
                     this%StnID      = Src%StnID
                     this%StnName    = Src%StnName
                     this%Latitude   = Src%Latitude
                     this%Longitude  = Src%Longitude
                     this%Elevation  = Src%Elevation
               END SELECT
            TYPE IS (TDlyDataForSubbasin)
               SELECT TYPE (Src)
                  TYPE IS (TDlyDataForSubbasin)
                     this%Bsn     = Src%Bsn
                     this%SubNum  = Src%SubNum
                     this%SubArea = Src%SubArea
               END SELECT
            TYPE IS (TDlyDataForLake)
               SELECT TYPE (Src)
                  TYPE IS (TDlyDataForLake)
                     this%Bsn = Src%Bsn
               END SELECT
         END SELECT

         Flag = .TRUE.
         RETURN
      END FUNCTION CopyMetaFrom_TDOL
      
      !--------------------------------------------------------
      !  Copy data from another object to this one.  Note that
      !  we make a full new copy of the data values.
      !--------------------------------------------------------
      FUNCTION CopyFrom_TDOL(this, Src) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this, Src
         LOGICAL :: Flag
         INTEGER :: IOS
         LOGICAL :: F
         TYPE(DDNode), POINTER :: ThisNode, NewNode, SrcNode
         
         !
         !  Clear any existing met data in "this", releasing the memory, etc.
         !
         CALL ClearTheDatasetList_TDOL(this)
         
         !
         !  Copy the metadata (header info) and date extent info
         !
         F = this%CopyMetaFrom(Src); IF (.NOT. F) GOTO 899
         this%SDateSeq    = Src%SDateSeq
         this%EDateSeq    = Src%EDateSeq
         this%NewDataSSeq = Src%NewDataSSeq
         this%NumDays     = Src%NumDays

         !
         !  If Src has no datasets, then we don't need to copy anything else.
         !  Just to be sure it is still set right, I will NULLIFY the 
         !  head node of "this".
         !
         SrcNode => Src%GetDatasetPtr()      ! head node of Src list
         IF (.NOT. ASSOCIATED(SrcNode)) THEN
            this%NumDatasets = 0
            NULLIFY(this%DatasetList)
            Flag = .TRUE.
            RETURN
         END IF
         
         !
         !  Create a new node that will be the head node for "this"
         !
         ALLOCATE(NewNode, STAT=IOS)
         IF (IOS .NE. 0) THEN
            ErrorMessage = 'Error allocating new head node.'; CALL PassMsg
            GOTO 899
         END IF
         NULLIFY(NewNode%Next)
         NewNode%DD = TDlyData()
         this%DatasetList => NewNode
         ThisNode => NewNode
         
         !
         !  Copy the data for that first dataset
         !
         SrcNode => Src%GetDatasetPtr()      ! head node of Src list
         Flag = ThisNode%DD%CopyFrom(SrcNode%DD); IF (.NOT. Flag) GOTO 899
         this%NumDatasets = 1
         
         !
         !  Now copy any other datasets in Src
         !
         SrcNode => SrcNode%Next
         DO WHILE (ASSOCIATED(SrcNode))
            ALLOCATE(NewNode, STAT=IOS)
            IF (IOS .NE. 0) THEN
               ErrorMessage = 'Error allocating new node.'; CALL PassMsg
               GOTO 899
            END IF
            NULLIFY(NewNode%Next)
            NewNode%DD = TDlyData()
            Flag = NewNode%DD%CopyFrom(SrcNode%DD); IF (.NOT. Flag) GOTO 899
            
            ThisNode%Next => NewNode
            ThisNode => ThisNode%Next
            this%NumDatasets = this%NumDatasets + 1
            SrcNode => SrcNode%Next
         END DO
         
         Flag = .TRUE.
         RETURN
         
         899  Flag = .FALSE.
              ErrorLevel = 1
              ErrorMessage = '[traceback] CopyFrom_TDOL()...';  CALL PassMsg
      END FUNCTION CopyFrom_TDOL
      
      !--------------------------------------------------------
      !  Copy from this object to another object.  Note that the 
      !  other object must already exist.  Rather than make whole
      !  new routine, I simply transfer control to the other 
      !--------------------------------------------------------
      FUNCTION CopyTo_TDOL(this, Dest) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this, Dest
         LOGICAL :: Flag
         Flag = CopyFrom_TDOL(Dest, this)
         IF (.NOT. Flag) GOTO 899
         RETURN         
        
         899  Flag = .FALSE.
              ErrorLevel = 1
              ErrorMessage = '[traceback] CopyTo()...';  CALL PassMsg
      END FUNCTION CopyTo_TDOL
         
      !--------------------------------------------------------
      !  This function that will take the data stored in variable DD
      !  and add it as a new node at the end of the Datasets list.
      !--------------------------------------------------------
      FUNCTION AddDataset_TDOL(this, DD) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         CLASS(TDlyData) :: DD
         LOGICAL :: Flag
         INTEGER :: IOS, SSeq, ESeq, SD, ED
         TYPE(DDNode), POINTER :: ThisNode, NewNode

         !
         !  Create the new node that will contain the data
         !
         ALLOCATE(NewNode, STAT=IOS)
         IF (IOS .NE. 0) THEN
            ErrorMessage = 'Error allocating new node.'; CALL PassMsg
            GOTO 899
         END IF
         NULLIFY(NewNode%Next)
         NewNode%DD = TDlyData()
         Flag = NewNode%DD%CopyFrom(DD); IF (.NOT. Flag) GOTO 899
         
         !
         !  Now add the new node to the list
         !
         IF (.NOT. ASSOCIATED(this%DatasetList)) THEN
            this%DatasetList => NewNode
            this%NumDatasets = 1
         ELSE
            ThisNode => this%DatasetList
            DO WHILE (ASSOCIATED(thisNode%Next))
               ThisNode => ThisNode%Next
            END DO
            ThisNode%Next => NewNode               ! adds it at the end of the list
            this%NumDatasets = this%NumDatasets + 1
         END IF
         
         !
         !  Determine the new overall period of record
         !
         SSeq = DateSeq_MaxValue    ! initialize to far future
         ESeq = DateSeq_MinValue    ! initialize to far past
         ThisNode => this%DatasetList
         DO WHILE (ASSOCIATED(ThisNode))
            SD = ThisNode%DD%GetStartDate()
            ED = ThisNode%DD%GetEndDate()
            IF ((SD .NE. MissingData_Date) .AND. (ED .NE. MissingData_Date)) THEN
               IF (SD .LT. SSeq) SSeq = SD
               IF (ED .GT. ESeq) ESeq = ED
            END IF
            ThisNode => ThisNode%Next
         END DO
         IF (ESeq .GE. SSeq) THEN
            this%SDateSeq = SSeq
            this%EDateSeq = ESeq
            this%NumDays  = ESeq - SSeq + 1
         END IF

         !
         !  For each dataset... if the period of record is not identical to
         !  the overall period, then we need to adjust this dataset by
         !  padding start/end out to match the overall period.
         !
         IF (this%SDateSeq .NE. MissingData_Date) THEN
            ThisNode => this%DatasetList
            DO WHILE (ASSOCIATED(ThisNode))
               SD = ThisNode%DD%GetStartDate()
               ED = ThisNode%DD%GetEndDate()
               IF ((SD .GT. SSeq) .OR. (ED .LT. ESeq)) THEN
                  CALL ThisNode%DD%AdjustPeriod(SSeq, ESeq); IF (ErrorLevel .NE. 0) GOTO 899
               END IF
               ThisNode => ThisNode%Next
            END DO
         END IF
         
         !         
         Flag = .TRUE.
         RETURN
         
         899  ErrorLevel = 1
              Flag = .FALSE.
              DEALLOCATE(NewNode, STAT=IOS)
              ErrorMessage = '[traceback] AddDataset_TDOL()...'; CALL PassMsg
              RETURN
              
      END FUNCTION AddDataset_TDOL
      
      !--------------------------------------------------------
      !  This function will take the data stored in variable NewDD
      !  and merge it with the existing data.
      !  If a dataset of the same datatype already exists, the new 
      !  data will be merged with that TDlyDataset object, overwriting
      !  the old values.
      !  If no dataset of this datatype exists, then this new dataset
      !  will simply be added to the TDOL object.      
      !--------------------------------------------------------
      FUNCTION MergeTDD_TDOL(this, NewDD) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         CLASS(TDlyData)            :: NewDD
         LOGICAL                    :: Flag
         
         INTEGER :: NewDType, ThisDType
         INTEGER :: Old_SSeq, Old_ESeq, New_SSeq, New_ESeq
         INTEGER :: Old_NSeq, New_NSeq
         INTEGER :: Result_SSeq, Result_ESeq, Result_NSeq
         LOGICAL :: F, Merged
         TYPE(DDNode),   POINTER :: ThisNode
         
         !
         !  Save the date info prior to any manipulations.
         !  Calculate the resulting period of record.
         !
         Old_SSeq = this%SDateSeq
         Old_ESeq = this%EDateSeq
         Old_NSeq = this%NewDataSSeq
         New_SSeq = NewDD%GetStartDate()
         New_ESeq = NewDD%GetEndDate()
         New_NSeq = NewDD%GetStartDate()
         Result_SSeq = MIN(Old_SSeq, New_SSeq)
         Result_ESeq = MAX(Old_ESeq, New_ESeq)
         Result_NSeq = MIN(Old_NSeq, New_NSeq)
         IF (Old_SSeq .EQ. MissingData_Date) Result_SSeq = New_SSeq
         IF (Old_ESeq .EQ. MissingData_Date) Result_ESeq = New_ESeq
         IF (Old_NSeq .EQ. MissingData_Date) Result_NSeq = New_NSeq
         
         !
         !  Step through the list of datasets in "this" to see if we will
         !  be merging with an existing dataset. 
         !
         !  Note that as we do this, the period of record for individual 
         !  TDlyDataset objects may not align with the others. That will be
         !  resolved at the end.
         !
         Merged = .FALSE.
         NewDType = NewDD%GetDataType()
         ThisNode => this%DatasetList
         DO WHILE (ASSOCIATED(ThisNode)) 
            ThisDType = ThisNode%DD%GetDataType()       ! data type of an object in the existing dataset
            IF (ThisDType .EQ. NewDType) THEN
               Merged = ThisNode%DD%MergeData(NewDD)    ! NewDD is unchanged
            END IF
            ThisNode => ThisNode%Next
         END DO
         IF (.NOT. Merged) THEN
            F = this%AddDataset(NewDD); IF (.NOT. F) GOTO 899
         END IF
         
         !
         !  Now revise (if necessary) the periods of each TDlyDataset object to
         !  match the unified period of record.
         !
         F = this%AlignPeriods(Result_SSeq, Result_ESeq); IF (.NOT. F) GOTO 899
         
         !         
         Flag = .TRUE.
         RETURN
         
         899  ErrorLevel = 1
              Flag = .FALSE.
              ErrorMessage = '[traceback] MergeTDD_TDOL()...'; CALL PassMsg
              RETURN
              
      END FUNCTION MergeTDD_TDOL
        
        
      !--------------------------------------------------------
      !  This function will take the data stored in variable NewDD
      !  and merge it with the existing data.
      !--------------------------------------------------------
      FUNCTION MergeTDOL_TDOL(this, NewDD) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         CLASS(TDlyDataOneLocation) :: NewDD
         LOGICAL                    :: Flag
         INTEGER :: NewDType, ThisDType
         INTEGER :: Old_SSeq, Old_ESeq, New_SSeq, New_ESeq
         INTEGER :: Result_SSeq, Result_ESeq
         LOGICAL :: F, Merged
         TYPE(DDNode),   POINTER :: ThisNode, NewNode
         
         !
         !  Save the date info prior to any manipulations
         !
         Old_SSeq = this%SDateSeq
         Old_ESeq = this%EDateSeq
         New_SSeq = NewDD%SDateSeq
         New_ESeq = NewDD%EDateSeq
         Result_SSeq = MIN(Old_SSeq, New_SSeq)
         Result_ESeq = MAX(Old_ESeq, New_ESeq)
         
         !
         !  Step through the list of datasets in NewDD. For each one,
         !  look for a matching one (same data type) in this object's
         !  dataset list.
         !  Whenever a match is found, merge those datasets.
         !  if no match is found, add the new dataset to this.
         !
         !  Note that as we do this, the period of record for individual 
         !  TDlyDataset objects may not align with the others. That will be
         !  resolved at the end.
         !
         NewNode => NewDD%GetDatasetPtr()           ! head node of NewStn's dataset list
         DO WHILE (ASSOCIATED(NewNode)) 
            NewDType = NewNode%DD%GetDataType()     ! data type of the subject dataset from the new station
            Merged = .FALSE.
            ThisNode => this%DatasetList            ! head node of this station object's dataset list   
            DO WHILE (ASSOCIATED(ThisNode))
               ThisDType = ThisNode%DD%GetDataType()   
               IF (ThisDType .EQ. NewDType) THEN
                  Merged = ThisNode%DD%MergeData(NewNode%DD)
               END IF
               ThisNode => ThisNode%Next
            END DO
            IF (.NOT. Merged) THEN
               F = this%AddDataset(NewNode%DD); IF (.NOT. F) GOTO 899
            END IF
            NewNode => NewNode%Next
         END DO
         
         !
         !  Now we need to resolve the master start/end date entries.
         !
         F = this%AlignPeriods(Result_SSeq, Result_ESeq); IF (.NOT. F) GOTO 899
         
         !         
         Flag = .TRUE.
         RETURN
         
         899  ErrorLevel = 1
              Flag = .FALSE.
              ErrorMessage = '[traceback] MergeTDOL_TDOL()...'; CALL PassMsg
              RETURN
              
      END FUNCTION MergeTDOL_TDOL
        
      !--------------------------------------------------------
      !  Adjust the data period of each TDlyData object as well as the overall
      !  start/end to match the specified period.
      !--------------------------------------------------------
      FUNCTION AlignPeriods_TDOL(this, SSeq, ESeq) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         INTEGER                    :: SSeq, ESeq
         INTEGER :: SSeqT, ESeqT
         LOGICAL :: Flag
         TYPE(DDNode),   POINTER :: ThisNode
         TYPE(TDlyData), POINTER :: TDDP
         
         !
         !  If the requested period matches the existing period, no need
         !  to do anything.
         !
         IF ((SSeq .EQ. this%SDateSeq) .AND. (ESeq .EQ. this%EDateSeq)) THEN
            Flag = .TRUE.
            RETURN
         END IF
         
         !
         !  First, adjust the overall period of record dates
         !
         this%SDateSeq = SSeq
         this%EDateSeq = ESeq
         
         !
         !  Step through the list of datasets. For each one, if the
         !  date extent does not match the specified one, fix it by
         !  padding and adjusting the meta data.
         !
         ThisNode => this%GetDatasetPtr()          ! head node of dataset list
         DO WHILE (ASSOCIATED(ThisNode)) 
            TDDP => ThisNode%DD
            SSeqT = TDDP%GetStartDate()
            ESeqT = TDDP%GetEndDate()
            IF ((SSeqT .NE. SSeq) .OR. (ESeqT .NE. ESeq)) THEN
               CALL TDDP%AdjustPeriod(SSeq, ESeq); IF (ErrorLevel .NE. 0) GOTO 899
            END IF
            ThisNode => ThisNode%Next
         END DO
         
         !         
         Flag = .TRUE.
         RETURN
         
         899  ErrorLevel = 1
              Flag = .FALSE.
              ErrorMessage = '[traceback] AlignPeriods_TDOL()...'; CALL PassMsg
              RETURN
              
      END FUNCTION AlignPeriods_TDOL
        
         
      !--------------------------------------------------------
      FUNCTION GetPointerToDataOfType_TDOL(this, DType) Result(TDD)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         INTEGER, INTENT(IN)        :: DType
         TYPE(TDlyData), POINTER    :: TDD
         TYPE(DDNode),   POINTER    :: ThisNode

         ThisNode => this%DatasetList
         DO WHILE (ASSOCIATED(ThisNode))
            IF (ThisNode%DD%GetDataType() .EQ. DType) THEN
               TDD => ThisNode%DD
               RETURN
            END IF
            ThisNode => ThisNode%Next
         END DO
         NULLIFY(TDD)           ! never found a match
      END FUNCTION GetPointerToDataOfType_TDOL
         
      !--------------------------------------------------------
      FUNCTION GetPointerToDataByIndex_TDOL(this, Ndx) Result(TDD)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         INTEGER, INTENT(IN)        :: Ndx
         TYPE(TDlyData), POINTER    :: TDD
         TYPE(DDNode),   POINTER    :: ThisNode
         INTEGER :: I

         IF (Ndx .LT. 1) GOTO 899
         IF (Ndx .GT. this%NumDatasets) GOTO 899
         I = 0
         ThisNode => this%DatasetList
         DO WHILE (ASSOCIATED(ThisNode))
            I = I + 1
            IF (I .EQ. Ndx) THEN
               TDD => ThisNode%DD
               RETURN
            END IF
            ThisNode => ThisNode%Next
         END DO
         
         899  NULLIFY(TDD)          ! error or no match
      END FUNCTION GetPointerToDataByIndex_TDOL

      !--------------------------------------------------------
      FUNCTION GetIndexForDataOfType_TDOL(this, DType) Result(Ndx)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         INTEGER, INTENT(IN)        :: DType
         INTEGER                    :: Ndx
         
         INTEGER :: I
         TYPE(DDNode),   POINTER    :: ThisNode
         
         I = 0
         ThisNode => this%DatasetList
         DO WHILE (ASSOCIATED(ThisNode))
            I = I + 1
            IF (ThisNode%DD%GetDataType() .EQ. DType) THEN
               Ndx = I
               RETURN
            END IF
            ThisNode => ThisNode%Next
         END DO
         Ndx = 0           ! never found a match
      END FUNCTION GetIndexForDataOfType_TDOL
         


      
      !-----------------------------------------------------------------
      !  Print summary of contents. Mainly for debugging.
      !-----------------------------------------------------------------
      SUBROUTINE PrintSummaryInfo_TDOL(this)
         IMPLICIT NONE
         CLASS(TDlyDataOneLocation) :: this
         TYPE(TDlyData), POINTER    :: TDD
         TYPE(DDNode),   POINTER    :: ThisNode
         CHARACTER(LEN=25) :: DTS
      
         WRITE(ErrorMessage, 1003) this%SDateSeq, this%EDateSeq; CALL PassMsg
         ThisNode => this%DatasetList
         DO WHILE (ASSOCIATED(ThisNode))
            TDD => ThisNode%DD
            DTS = GlerlDataTypeString(TDD%GetDataType())
            WRITE(ErrorMessage, 1005) TRIM(DTS), TDD%GetStartDate(), TDD%GetEndDate(); CALL PassMsg
            ThisNode => ThisNode%Next
         END DO
         RETURN
         
         1003 FORMAT(4X, 'SDateSeq, EDateSeq: ', 2I9)
         1005 FORMAT(4X, 'DType, SDate, EDate: ', A, 2I9)
      END SUBROUTINE PrintSummaryInfo_TDOL


         
      !========================================================================================
      !========================================================================================
      !========================================================================================
      !========================================================================================
      !  Implementation procedures for the child class: TDlyDataMetStn
      !========================================================================================
      !========================================================================================
      !--------------------------------------------------------
      FUNCTION Create_TDMS() RESULT(this)
         IMPLICIT NONE
         TYPE (TDlyDataMetStn) :: this
         NULLIFY(this%DatasetList)
         this%NumDatasets = 0
         CALL Initialize_TDMS(this)
      END FUNCTION Create_TDMS
      
      !--------------------------------------------------------
      SUBROUTINE Initialize_TDMS(this)
         IMPLICIT NONE
         CLASS(TDlyDataMetStn) :: this
         this%StnID       = ''
         this%StnName     = ''
         this%Latitude    = MissingData_REAL
         this%Longitude   = MissingData_REAL
         this%Elevation   = MissingData_REAL
         CALL Initialize_TDOL(this)
      END SUBROUTINE Initialize_TDMS
      
      !--------------------------------------------------------
      SUBROUTINE Clear_TDMS(this)
         IMPLICIT NONE
         CLASS(TDlyDataMetStn) :: this
         this%StnID       = ''
         this%StnName     = ''
         this%Latitude    = MissingData_REAL
         this%Longitude   = MissingData_REAL
         this%Elevation   = MissingData_REAL
         CALL Clear_TDOL(this)
         RETURN
      END SUBROUTINE Clear_TDMS
         
         
      !========================================================================================
      !========================================================================================
      !  Implementation procedures for the child class: TDlyDataForSubbasin
      !========================================================================================
      !========================================================================================
      !--------------------------------------------------------
      FUNCTION Create_TDFS() RESULT(this)
         IMPLICIT NONE
         TYPE (TDlyDataForSubbasin) :: this
         NULLIFY(this%DatasetList)
         this%NumDatasets = 0
         CALL Initialize_TDFS(this)
      END FUNCTION Create_TDFS
      
      !--------------------------------------------------------
      SUBROUTINE Initialize_TDFS(this)
         IMPLICIT NONE
         CLASS(TDlyDataForSubbasin) :: this
         this%Bsn     = 'xxx'
         this%SubNum  = MissingData_Int
         this%SubArea = MissingData_REAL
         CALL Initialize_TDOL(this)
      END SUBROUTINE Initialize_TDFS
      
      !--------------------------------------------------------
      SUBROUTINE Clear_TDFS(this)
         IMPLICIT NONE
         CLASS(TDlyDataForSubbasin) :: this
         this%Bsn     = 'xxx'
         this%SubNum  = MissingData_Int
         this%SubArea = MissingData_REAL
         CALL Clear_TDOL(this)
         RETURN
      END SUBROUTINE Clear_TDFS
         
      !========================================================================================
      !========================================================================================
      !  Implementation procedures for the child class: TDlyDataForSubbasin
      !========================================================================================
      !========================================================================================
      !--------------------------------------------------------
      FUNCTION Create_TDFL() RESULT(this)
         IMPLICIT NONE
         TYPE (TDlyDataForLake) :: this
         NULLIFY(this%DatasetList)
         this%NumDatasets = 0
         CALL Initialize_TDFL(this)
      END FUNCTION Create_TDFL
      
      !--------------------------------------------------------
      SUBROUTINE Initialize_TDFL(this)
         IMPLICIT NONE
         CLASS(TDlyDataForLake) :: this
         this%Bsn     = 'xxx'
         CALL Initialize_TDOL(this)
      END SUBROUTINE Initialize_TDFL
      
      !--------------------------------------------------------
      SUBROUTINE Clear_TDFL(this)
         IMPLICIT NONE
         CLASS(TDlyDataForLake) :: this
         this%Bsn     = 'xxx'
         CALL Clear_TDOL(this)
         RETURN
      END SUBROUTINE Clear_TDFL
         
END MODULE DailyDataStructures
