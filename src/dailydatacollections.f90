
MODULE DailyDataCollections

      USE GlerlDataTypesAndUnits
      USE ErrorProcess
      USE GLSHFS_Util
      USE DailyDataStructures
      IMPLICIT NONE

      PRIVATE     ! hide everything by default. PUBLIC will be set as needed.

      !
      !  TYPE stuff for the list of daily met stations
      !
      TYPE :: DMSnode
         TYPE(TDlyDataMetStn) :: DDMS
         TYPE(DMSnode), POINTER :: Next
      END TYPE DMSnode
      
      !=======================================================================================
      !  Now some structures for collections of things.
      !---------------------------------------------------------------------------------
      !  A collection of TDlyDataMetStn objects
      !
      TYPE, PUBLIC :: TDlyDataForMultipleStations
            CHARACTER(LEN=100) :: Description  = ''
            INTEGER            :: NumStations  = 0
            INTEGER            :: EarliestData = MissingData_Date       ! Earliest start date 
            INTEGER            :: LatestData   = MissingData_Date       ! Latest end date 
            INTEGER            :: NewDataStart = MissingData_Date       ! Earliest NEW station data      
            TYPE (DMSnode), POINTER :: StnList
         CONTAINS
            FINAL :: Delete_TDDFMS
            PROCEDURE, PASS       :: Initialize           => Initialize_TDDFMS
            PROCEDURE, PUBLIC     :: Clear                => Clear_TDDFMS
            PROCEDURE, PUBLIC     :: AddStation           => AddStation_TDDFMS
            PROCEDURE, PUBLIC     :: GetStationPtrByIndex => GetStationPtrByIndex_TDDFMS
            PROCEDURE, PUBLIC     :: GetStationPtrByStnID => GetStationPtrByStnID_TDDFMS
            PROCEDURE, PUBLIC     :: CopyFrom             => CopyFrom_TDDFMS
            PROCEDURE, PUBLIC     :: CopyTo               => CopyTo_TDDFMS
            PROCEDURE, PUBLIC     :: PrintSummaryInfo
            PROCEDURE, PRIVATE    :: AddNewStation
      END TYPE TDlyDataForMultipleStations
      
      INTERFACE TDlyDataForMultipleStations
         PROCEDURE Create_TDDFMS
      END INTERFACE TDlyDataForMultipleStations

CONTAINS

      !========================================================================================
      !========================================================================================
      !  Implementation procedures for the base class: TDlyDataForMultipleStations
      !========================================================================================
      !========================================================================================
      !--------------------------------------------------------
      !  Constructor for the class
      !--------------------------------------------------------
      FUNCTION Create_TDDFMS() RESULT(this)
         IMPLICIT NONE
         TYPE (TDlyDataForMultipleStations) :: this
         this%NumStations  = 0   
         this%EarliestData = MissingData_Date       ! Earliest start date 
         this%LatestData   = MissingData_Date       ! Latest end date 
         this%NewDataStart = MissingData_Date       ! Start of NEW data
         NULLIFY(this%StnList)
         CALL this%Initialize()          ! uses Initialize_TDDFMS
      END FUNCTION Create_TDDFMS
      
      !--------------------------------------------------------
      !  Clear() for the base
      !--------------------------------------------------------
      SUBROUTINE Delete_TDDFMS(this)
         IMPLICIT NONE
         TYPE(TDlyDataForMultipleStations) :: this
         TYPE(DMSnode), POINTER :: ThisNode, NextNode
         this%Description = ''
         IF (ASSOCIATED(this%StnList)) THEN
            ThisNode => this%StnList            ! set to root node (#1)
            DO WHILE (ASSOCIATED(ThisNode))
               CALL ThisNode%DDMS%Clear()
               NextNode=>ThisNode%Next
               DEALLOCATE(ThisNode)
               this%NumStations = this%NumStations - 1
               ThisNode => NextNode
            END DO
            NULLIFY(this%StnList)
         END IF
         this%NumStations  = 0
         this%EarliestData = MissingData_Date       ! Earliest start date 
         this%LatestData   = MissingData_Date       ! Latest end date 
         this%NewDataStart = MissingData_Date       ! Start of NEW data
      END SUBROUTINE Delete_TDDFMS
               
      !--------------------------------------------------------
      !  Initialize() for the class
      !--------------------------------------------------------
      SUBROUTINE Initialize_TDDFMS(this)
         IMPLICIT NONE
         CLASS(TDlyDataForMultipleStations) :: this
         CALL this%Clear()
      END SUBROUTINE Initialize_TDDFMS
      
      !--------------------------------------------------------
      !  Clear() for the base
      !--------------------------------------------------------
      SUBROUTINE Clear_TDDFMS(this)
         IMPLICIT NONE
         CLASS(TDlyDataForMultipleStations) :: this
         TYPE(DMSnode), POINTER :: ThisNode, NextNode, NodeToClear
         this%Description = ''
         IF (ASSOCIATED(this%StnList)) THEN
            ThisNode => this%StnList            ! set to root node (#1)
            NextNode => ThisNode%Next           ! the next node (#2)
            CALL ThisNode%DDMS%Clear()
            DO WHILE (ASSOCIATED(NextNode))
               NodeToClear => NextNode
               CALL NextNode%DDMS%Clear()
               ThisNode=>NextNode
               NextNode=>NextNode%Next
               DEALLOCATE(NodeToClear)
               this%NumStations = this%NumStations - 1
            END DO
            DEALLOCATE(this%StnList)
            NULLIFY(this%StnList)
         END IF
         this%NumStations  = 0
         this%EarliestData = MissingData_Date       ! Earliest start date 
         this%LatestData   = MissingData_Date       ! Latest end date 
         this%NewDataStart = MissingData_Date       ! Start of NEW data
      END SUBROUTINE Clear_TDDFMS

      !--------------------------------------------------------
      !  This function accepts a single TDlyDataMetStn object, DDMS.
      !  It adds that dataset to "this".  If the station already exists, 
      !  then the data is merged with the old.
      !
      !  The EarliestData and LatestData fields are updated to reflect 
      !  the dates from the new station.
      !--------------------------------------------------------
      FUNCTION AddStation_TDDFMS(this, DDMS) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataForMultipleStations) :: this
         CLASS(TDlyDataMetStn) :: DDMS
         LOGICAL :: Flag, OK
         INTEGER :: SSeq, ESeq, NSeq
         TYPE (TDlyDataMetStn), POINTER :: TDDMSP

         !
         !  If the station list is empty, just add this new station.
         !  Date information is updated by the AddNewStation routine.
         !
         IF (.NOT. ASSOCIATED(this%StnList)) THEN
            OK = this%AddNewStation(DDMS); IF (.NOT. OK) GOTO 898
            Flag = .TRUE.
            RETURN
         END IF
         
         !
         !  Add this station to the existing this%StnList.
         !
         TDDMSP => this%GetStationPtrByStnID(DDMS%StnID)
         IF (ASSOCIATED(TDDMSP)) THEN
            OK = TDDMSP%MergeData(DDMS); IF (.NOT. OK) GOTO 898

            IF (this%EarliestData .EQ. MissingData_Date) this%EarliestData = DDMS%SDateSeq
            IF (this%LatestData   .EQ. MissingData_Date) this%LatestData   = DDMS%EDateSeq
            IF (this%NewDataStart .EQ. MissingData_Date) this%NewDataStart = DDMS%SDateSeq

            SSeq = this%EarliestData
            ESeq = this%LatestData
            NSeq = this%NewDataStart
            this%EarliestData = MIN(SSeq, DDMS%SDateSeq)
            this%LatestData   = MAX(ESeq, DDMS%EDateSeq)
            this%NewDataStart = MIN(NSeq, DDMS%SDateSeq)
            this%NumStations  = this%NumStations + 1
         ELSE
            OK = this%AddNewStation(DDMS); IF (.NOT. OK) GOTO 898
         END IF
         Flag = .TRUE.
         RETURN

         
         898  ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyDataForMultipleStations%AddStation_TDDFMS...'; CALL PassMsg
              Flag = .FALSE.
              RETURN
      END FUNCTION AddStation_TDDFMS

      
      !--------------------------------------------------------
      !  This function accepts a single TDlyDataMetStn object, DDMS.
      !  It creates a new node in the Datasets list, containing a 
      !  new TDlyDataMetStn object, and then copies the data from 
      !  DDMS into that new object.  No check is made for the station
      !  already existing. We just always add a new node.
      !
      !  The EarliestData and LatestData fields are updated to reflect 
      !  the dates from the new dataset.
      !--------------------------------------------------------
      FUNCTION AddNewStation(this, DDMS) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataForMultipleStations) :: this
         CLASS(TDlyDataMetStn) :: DDMS
         LOGICAL :: Flag
         INTEGER :: IOS, SSeq, ESeq, NSeq
         TYPE(DMSnode), POINTER :: ThisNode, NewNode

         !
         !  Create a new empty node that can be added to the list
         !
         ALLOCATE(NewNode, STAT=IOS)
         IF (IOS .NE. 0) THEN
            ErrorMessage = 'Error allocating new node in station list.'; CALL PassMsg
            GOTO 898
         END IF
         NULLIFY(NewNode%Next)
         NewNode%DDMS = TDlyDataMetStn()

         !
         !  Copy the info from DD into the TDlyDataMetStn object that is part 
         !  of this new node. This effectively creates the new entry we need 
         !  to add to the this%StnList object.
         !
         Flag = NewNode%DDMS%CopyFrom(DDMS); IF (.NOT. Flag) GOTO 899

         !
         !  Add it to the end of the list
         !
         IF (ASSOCIATED(this%StnList)) THEN
            ThisNode => this%StnList
            DO WHILE (ASSOCIATED(thisNode%Next))
               ThisNode => ThisNode%Next
            END DO
            ThisNode%Next => NewNode
         ELSE
            this%StnList => NewNode           ! list was empty; this is the only entry (head node)
         END IF

         !
         !  Update the EarliestData/LatestData fields
         !
         IF (this%EarliestData .EQ. MissingData_Date) this%EarliestData = DDMS%SDateSeq
         IF (this%LatestData   .EQ. MissingData_Date) this%LatestData   = DDMS%EDateSeq
         IF (this%NewDataStart .EQ. MissingData_Date) this%NewDataStart = DDMS%SDateSeq

         SSeq = this%EarliestData
         ESeq = this%LatestData
         NSeq = this%NewDataStart
         this%EarliestData = MIN(SSeq, DDMS%SDateSeq)
         this%LatestData   = MAX(ESeq, DDMS%EDateSeq)
         this%NewDataStart = MIN(NSeq, DDMS%SDateSeq)
         this%NumStations  = this%NumStations + 1
         Flag = .TRUE.
         RETURN
         
         898  ErrorLevel = 1
         899  ErrorMessage = '[traceback] TDlyDataForMultipleStations%AddNewStation...'; CALL PassMsg
              Flag = .FALSE.
              RETURN
              
      END FUNCTION AddNewStation

      !--------------------------------------------------------
      !  This function will return a pointer to the Nth entry
      !  in the this%StnList list.
      !--------------------------------------------------------
      FUNCTION GetStationPtrByIndex_TDDFMS(this, Indx) Result(Stn)
         IMPLICIT NONE
         CLASS(TDlyDataForMultipleStations) :: this
         INTEGER, INTENT(IN) :: Indx
         TYPE (TDlyDataMetStn), POINTER :: Stn
         LOGICAL :: Flag
         INTEGER :: I
         TYPE(DMSnode), POINTER :: ThisNode

         NULLIFY(Stn)   !  default return value

         !
         !  Error situations
         !
         IF (Indx .LT. 1) THEN
            WRITE(ErrorMessage, 1001) Indx; CALL PassMsg
            GOTO 898
         END IF
         IF (Indx .GT. this%NumStations) THEN
            WRITE(ErrorMessage, 1002) Indx, this%NumStations; CALL PassMsg
            GOTO 898
         END IF
         IF (.NOT. ASSOCIATED(this%StnList)) THEN
            WRITE(ErrorMessage, 1003); CALL PassMsg
            GOTO 898
         END IF

         !
         !  Search
         !
         ThisNode => this%StnList
         I = 1
         DO WHILE ((I .LT. Indx) .AND. (ASSOCIATED(ThisNode)))
            ThisNode => ThisNode%Next
            I = I + 1
         END DO
         IF (.NOT. ASSOCIATED(ThisNode)) GOTO 898
         Stn => ThisNode%DDMS
         RETURN
         
         898  ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyDataForMultipleStations%GetStationPtrByIndex...'; CALL PassMsg
              Flag = .FALSE.
              NULLIFY(Stn)
              RETURN
               
         1001 FORMAT('Invalid index (', I0, ') passed to GetStation. Must be positive.')
         1002 FORMAT('Invalid index (', I0, ') passed to GetStation. There are only ', I0, ' stations.')
         1003 FORMAT('The TDlyDataForMultipleStations object contains 0 stations.')
      END FUNCTION GetStationPtrByIndex_TDDFMS
      
      !--------------------------------------------------------
      !  This function will return a pointer to the correct entry
      !  in the this%StnList list (the one with a station ID matching the
      !  requested ID - case insensitive).
      !
      !  If there are no stations, or the requested station is not found,
      !  then this routine just returns a NULL pointer.
      !--------------------------------------------------------
      FUNCTION GetStationPtrByStnID_TDDFMS(this, ReqID)   Result(Stn)
         IMPLICIT NONE
         CLASS(TDlyDataForMultipleStations) :: this
         CHARACTER(LEN=*), INTENT(IN) :: ReqID
         TYPE (TDlyDataMetStn), POINTER :: Stn
         CHARACTER(LEN=100) :: ReqLwr, S
         TYPE(DMSnode), POINTER :: ThisNode

         NULLIFY(Stn)
         IF (.NOT. ASSOCIATED(this%StnList)) RETURN

         ReqLwr = GetLowerCase(ReqID)
         ThisNode => this%StnList
         DO WHILE (ASSOCIATED(ThisNode))
            S = ThisNode%DDMS%StnID
            CALL Lowercase(S)
            IF (TRIM(S) .EQ. TRIM(ReqLwr)) THEN
               Stn => ThisNode%DDMS
               RETURN
            END IF
            ThisNode => ThisNode%Next
         END DO
         RETURN
         
      END FUNCTION GetStationPtrByStnID_TDDFMS
      
      !--------------------------------------------------------
      !  Copy data from another object to this one.  Note that
      !  we make a full new copy of the data values.
      !--------------------------------------------------------
      FUNCTION CopyFrom_TDDFMS(this, Src) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataForMultipleStations) :: this, Src
         LOGICAL :: Flag
         INTEGER :: IOS
         TYPE(DMSnode), POINTER :: ThisNow, ThisNext, SrcNow, SrcNext, NewNode

         !
         !  Clear this object, if needed
         !
         IF (this%NumStations .GT. 0) CALL this%Clear()
         
         !
         !  Copy the description member
         !
         this%Description = Src%Description

         !
         !  If the source list is empty, no need to do anything else
         !
         IF (Src%NumStations .EQ. 0) THEN
            Flag = .TRUE.
            RETURN
         END IF
         
         !
         !  Create a new node, this will be used as the new head node.
         !
         ALLOCATE(NewNode, STAT=IOS)
         IF (IOS .NE. 0) THEN
            ErrorMessage = 'Error allocating new node for station list.'; CALL PassMsg
            GOTO 898
         END IF
         NULLIFY(NewNode%Next)
         Flag = NewNode%DDMS%CopyFrom(Src%StnList%DDMS);  IF (.NOT. Flag) GOTO 899
         this%StnList => NewNode
         
         !
         !  Now step through the list, copying each item
         !
         ThisNow  => this%StnList
         ThisNext => ThisNow%Next
         SrcNow   => Src%StnList
         SrcNext  => SrcNow%Next
         DO WHILE (ASSOCIATED(SrcNext))
            ALLOCATE(NewNode, STAT=IOS)
            IF (IOS .NE. 0) THEN
               ErrorMessage = 'Error allocating new node for station list.'; CALL PassMsg
               GOTO 898
            END IF
            NULLIFY(NewNode%Next)
            
            Flag = NewNode%DDMS%CopyFrom(SrcNext%DDMS);  IF (.NOT. Flag) GOTO 899
            
            ThisNext => NewNode
            ThisNow  => ThisNext
            SrcNow   => SrcNext
         END DO
         
         this%NumStations  = Src%NumStations
         this%EarliestData = Src%EarliestData
         this%LatestData   = Src%LatestData
         Flag = .TRUE.
         RETURN
         
         898  ErrorLevel = 1
         899  Flag = .FALSE.
              ErrorMessage = '[traceback] TDlyDataForMultipleStations%CopyFrom()...';  CALL PassMsg
      END FUNCTION CopyFrom_TDDFMS
         
      !--------------------------------------------------------
      !  Copy from this object to another object.  Note that the 
      !  other object must already exist.  Rather than make whole
      !  new routine, I simply transfer control to the other 
      !--------------------------------------------------------
      FUNCTION CopyTo_TDDFMS(this, Dest) Result(Flag)
         IMPLICIT NONE
         CLASS(TDlyDataForMultipleStations) :: this, Dest
         LOGICAL :: Flag
         Flag = CopyFrom_TDDFMS(Dest, this)
         IF (.NOT. Flag) GOTO 899
         RETURN         
        
         899  Flag = .FALSE.
              ErrorLevel = 1
              ErrorMessage = '[traceback] TDlyDataForMultipleStations%CopyTo()...';  CALL PassMsg
      END FUNCTION CopyTo_TDDFMS


      !--------------------------------------------------------
      !  Print out summary info. Intended for use in debugging.
      !--------------------------------------------------------
      SUBROUTINE PrintSummaryInfo(this)
         IMPLICIT NONE
         CLASS(TDlyDataForMultipleStations) :: this
         INTEGER :: I, J
         CHARACTER(LEN=15) :: DTS_Out
         CHARACTER(LEN=20) :: DTS
         TYPE (TDlyDataMetStn), POINTER :: Stn
         TYPE (TDlyData),       POINTER :: DD
         TYPE(DMSnode), POINTER :: ThisNode
         REAL, DIMENSION(:), ALLOCATABLE :: DV
         CHARACTER(LEN=15), PARAMETER :: Blank15 = '               '

         print*, '---------------- Data in TDlyDataForMultipleStations object --------------------'
         WRITE(ErrorMessage, 1001) this%NumStations, this%EarliestData, this%LatestData; CALL PassMsg
         ThisNode => this%StnList
         DO WHILE (ASSOCIATED(ThisNode))
            Stn => ThisNode%DDMS
            WRITE(ErrorMessage, 1005) TRIM(Stn%StnID), Stn%SDateSeq, Stn%EDateSeq, Stn%NumDatasets; CALL PassMsg
            DO I = 1, Stn%NumDatasets
               DD => Stn%GetPointerToDataByIndex(I)
               DTS = GlerlDataTypeString(DD%GetDataType())
               J = MIN(LEN_TRIM(DTS), 15)
               DTS_Out = DTS(1:J) // Blank15(1:15-J)
               DV = DD%GetDataVals()
               IF (ALLOCATED(DV)) THEN
                  WRITE(ErrorMessage, 1010) DTS_Out, DD%GetStartDate(), DD%GetEndDate(); CALL PassMsg
               ELSE
                  WRITE(ErrorMessage, 1011) DTS_Out, DD%GetStartDate(), DD%GetEndDate(); CALL PassMsg
               END IF
               
            END DO
            ThisNode => ThisNode%Next
         END DO
         print*, '--------------------------------------------------------------------------------'
         RETURN         
         
         1001 FORMAT('NumStations=', I0, ': EarliestData, LatestData=', 2I9)
         1005 FORMAT('  Data for ', A, ': Start,End=', 2I9, ' NumDatasets=', I0)
         1010 FORMAT('    ', A15, ': Start, End = ', 2I9)
         1011 FORMAT('    ', A15, ': Start, End = ', 2I9, '  *NOT allocated*')
         
      END SUBROUTINE PrintSummaryInfo
      
      
END MODULE DailyDataCollections      
