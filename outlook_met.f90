!-----------------------------------------------------------------
!  Build sets of subbasin average meteorology for use as outlook meteorology
!  as input to the LBRM and LLTM.
!
!  Remember that for GLSHFS, all we care about are these data types:
!     GDT_AirtempMax       (for LBRM)
!     GDT_AirtempMin       (for LBRM)
!     GDT_Precipitation    (for LBRM)
!
!     GDT_AirtempMean      (for LLTM)
!     GDT_DewpointMean     (for LLTM)
!     GDT_Windspeed        (for LLTM)
!     GDT_CloudCover       (for LLTM)
!     GDT_NetLongWaveRad   (for LLTM)
!     GDT_IncidentRad      (for LLTM)
!
!---------------------------------------------------------------------
!  Given a start date (month & day) and forecast length (in days), create
!  sequences of meteorology for the specified lake.
!  The source of met data will be the existing subbasin meteorology files
!  in that lake's GLSHFS folder named subdata_<bsn><sub>.csv
!  (e.g. subdata_sup01.csv)
!
!  One outlook file will be built for every possible starting year in 
!  the existing file.  The outlook data files will be named
!  otlkdata_<bsn><sub>_<year>.csv   (e.g. otlkdata_sub01_1948.csv), where
!  <year> is the year where we started pulling data.
!
!  For example, if the existing file contains data for Jan 1, 1948 through
!  April 17, 2017 and the calling procedure specifies a start date of
!  April 1 with forecast length of 61 days, then outlook files will be 
!  built that contain data from:
!    1948-04-01 to 1948-05-31
!    1949-04-01 to 1949-05-31
!    1950-04-01 to 1950-05-31
!    1951-04-01 to 1951-05-31
!      ...
!    2015-04-01 to 2015-05-31
!    2016-04-01 to 2016-05-31
!
!  If the start date was January 1, then the outlook files would have:
!    1948-01-01 to 1948-03-01     (leap year)
!    1949-01-01 to 1949-03-02
!    1950-01-01 to 1950-03-02
!    1951-01-01 to 1951-03-02
!    1952-01-01 to 1952-03-02     (leap year)
!    1953-01-01 to 1953-03-02
!    1954-01-01 to 1954-03-02
!      ...
!    2015-01-01 to 2015-03-02
!    2016-01-01 to 2016-03-01     (leap year)
!    2017-01-01 to 2017-03-02
!
!  In the first case, 2017 was not available, because we could not
!  get a full 61 days from 2017.
!
!  The data timeseries will ignore calendar year boundaries in order to
!  get the sufficient days of data.
!-----------------------------------------------------------------

MODULE Outlook_Met
      USE ErrorProcess
      USE StatusMessages
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GL_Constants
      USE GlerlDataTypesAndUnits
      USE DailyDataStructures
      USE GLSHFS_StructuredData
      USE GLSHFS_ConfigFile
      USE GLSHFS_Files

      !
      !  By default, hide everything. Things that need to be public outside
      !  the module will be explicitly declared PUBLIC.
      !
      PRIVATE

      PUBLIC :: Build_Outlook_Meteorology
      PUBLIC :: BuildHistEnsembleFiles_List
      
      !
      !
      !
      INTEGER, PARAMETER :: NumDataTypes = 9
      INTEGER, DIMENSION(NumDataTypes), PARAMETER :: MyDataTypes = (/         &
         GDT_AirtempMax,   GDT_AirtempMin,     GDT_Precipitation,             &
         GDT_AirtempMean,  GDT_DewpointMean,   GDT_WindSpeed,                 &
         GDT_CloudCover,   GDT_NetLongWaveRad, GDT_IncidentRad  /)



CONTAINS

!---------------------------------------------------------------------------      
      !---------------------------------------------------------------------
      !  Build outlook meteorology files
      !  These are extracted from the historical data files.
      !---------------------------------------------------------------------
      SUBROUTINE Build_Outlook_Meteorology()
      IMPLICIT NONE
      
      INTEGER :: J, Lk, U1, IOS, Dy, Mn, Yr
      INTEGER :: AtEndSeq, DataEndSeq, NumDays, ScenCount
      INTEGER :: FcDay, FcMon, FcYear, SSeq, ESeq, SeqS
      INTEGER :: FSSeq, FESeq, FSYear, FEYear
      INTEGER :: ActualForecastStartDate
      INTEGER :: ActualForecastEndDate
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=200) :: OtlkDir, LakeDir, FName
      TYPE (THeaderInfoType) :: HdrInfo
      TYPE (TDlyDataForSubbasin) :: SubData
      
      !
      !  Initialization
      !
      U1 = -1
      SubData = TDlyDataForSubbasin()
      
      StatusMsg = 'Now creating outlook meteorology files';   CALL WriteMsg()

      !
      !  The outlook met files will be written to the forecast directory.  This
      !  directory is named to match ForecastName from the config file.
      !  
      WRITE(OtlkDir, 1001) TRIM(GLSHFS_Config%BaseDir), TRIM(GLSHFS_Config%ForecastName), FilePathSeparator
      
      !
      !  First step is to determine the start date. This can either be 
      !  a user-specified date or the day after the end of the observed
      !  data.  That option is set in the main configuration file.
      !
      !  If ForecastStartDate = 8888-08-08 then it is a flag value indicating
      !  that the forecast should start at the end of the available historical
      !  data. In that case, we will check the subbasin meteorology files for
      !  each lake (only looking at the 00, or overlake, "subbasin") to find
      !  the EARLIEST end date. The forecast will start one day after that.
      !
      SSeq = GLSHFS_Config%ForecastStartSeq
      CALL DateSequence(8, 8, 8888, AtEndSeq)
      IF (SSeq .EQ. AtEndSeq) THEN
         DataEndSeq = MissingData_Date
         DO Lk = 1, 7
            Bsn = TRIM(LakeName3(Lk))
            WRITE(LakeDir, 1001) TRIM(GLSHFS_Config%BaseDir), Bsn, FilePathSeparator
            WRITE(FName, 1002) TRIM(LakeDir), Bsn, 0
            U1 = GetFreeUnitNumber()
            OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', ERR=811)
            CALL FileWasOpened(U1)
            CALL ReadHeaderInfo(FName, U1, HFT_SingleSubbasin, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
            CLOSE(U1)
            CALL FileWasClosed(U1)
            IF (HdrInfo%EDate .EQ. MissingData_Date) GOTO 701
            IF (DataEndSeq    .EQ. MissingData_Date) DataEndSeq = HdrInfo%EDate
            IF (HdrInfo%EDate .LT.       DataEndSeq) DataEndSeq = HdrInfo%EDate
         END DO
         IF (DataEndSeq .EQ. MissingData_Date) GOTO 702
         SSeq = DataEndSeq + 1
      END IF
      IF (SSeq .EQ. MissingData_Date) GOTO 703
      ActualForecastStartDate = SSeq

      !
      !  Now that we have a valid start date, we can determine the exact forecast period.
      !
      CALL SequenceDate(FcDay, FcMon, FcYear, ActualForecastStartDate)
      IF (ErrorLevel .NE. 0) THEN
         ErrorMessage = 'Error with forecast start date when building outlook met files.'; CALL PassMsg
         GOTO 899
      END IF
      Yr = FcYear
      Mn = FcMon + GLSHFS_Config%ForecastLen - 1
      IF (FcDay .GT. 1) Mn = Mn + 1
      DO WHILE (Mn .GT. 12)
         Mn = Mn - 12; Yr = Yr + 1
      END DO
      CALL DateSequence(1, Mn, Yr, ESeq); IF (ErrorLevel .NE. 0) GOTO 899
      CALL AdjustToEndOfMonth(ESeq);          IF (ErrorLevel .NE. 0) GOTO 899
      ActualForecastEndDate = ESeq
      NumDays = ActualForecastEndDate - ActualForecastStartDate + 1
 
      !
      !  If source is 'extractfromhist' and scenarionames='all', then we need
      !  to build the list of valid years to use and then replace the old
      !  value in scenarionames ('all') with that list.
      !
      ScenCount = UBOUND(GLSHFS_Config%ScenarioNames, 1)
      IF (GLSHFS_Config%ForecastMetSource .EQ. 1) THEN
         IF (GLSHFS_Config%ScenarioNames(1) .EQ. 'all') THEN
            !
            !  Find the common period of record for all lakes.
            !  FSSeq = Start of common period for the Files
            !  FESeq = End of common period for the Files
            !
            FSSeq = MissingData_Date
            FESeq = MissingData_Date
            DO Lk = 1, 7
               CALL SubData%Clear()
               Bsn = TRIM(LakeName3(Lk))                                                    ! use Lake Superior, assuming other lakes match
               WRITE(LakeDir, 1001) TRIM(GLSHFS_Config%BaseDir), Bsn, FilePathSeparator
               WRITE(FName, 1002) TRIM(LakeDir), Bsn, 0                                    ! get dates from subbasin 0
               CALL ReadFile_OneSubbasin(FName, SubData); IF (ErrorLevel .NE. 0) GOTO 899
               SSeq = SubData%GetStartDate()
               ESeq = SubData%GetEndDate()
               IF (FSSeq .EQ. MissingData_Date) FSSeq = SSeq
               IF (FESeq .EQ. MissingData_Date) FESeq = ESeq
               IF (SSeq .GT. FSSeq) FSSeq = SSeq                    ! "squeeze" the period to the smallest common
               IF (ESeq .LT. FESeq) FESeq = ESeq                    ! period-of-record among all lakes
            END DO
           
            !
            !  Count up how many valid forecast periods that we can extract from that
            !  common period-of-record
            !
            CALL SequenceDate(Dy, Mn, FSYear, FSSeq); IF (ErrorLevel .NE. 0) GOTO 899   ! first year of file to try
            CALL SequenceDate(Dy, Mn, FEYear, FESeq); IF (ErrorLevel .NE. 0) GOTO 899   ! last year of file to try
            J = 0
            DO Yr = FSYear, FEYear
               CALL DateSequence(FcDay, FcMon, Yr, SeqS); IF (ErrorLevel .NE. 0) GOTO 899
               ESeq = SeqS + NumDays - 1
               IF ((SeqS .GE. FSSeq) .AND. (ESeq .LE. FESeq)) J = J + 1
            END DO
            ScenCount = J

            !
            !  Update the ScenarioNames array
            !
            DEALLOCATE(GLSHFS_Config%ScenarioNames, STAT=IOS)
            ALLOCATE(GLSHFS_Config%ScenarioNames(ScenCount), STAT=IOS)
            IF (IOS .NE. 0) THEN
               ErrorMessage = 'Error allocating memory for forecast scenario names'; CALL PassMsg
               GOTO 898
            END IF
            GLSHFS_Config%ScenarioNames(:) = '------'
            J = 0
            DO Yr = FSYear, FEYear
               CALL DateSequence(FcDay, FcMon, Yr, SeqS); IF (ErrorLevel .NE. 0) GOTO 899
               ESeq = SeqS + NumDays - 1
               IF ((SeqS .GE. FSSeq) .AND. (ESeq .LE. FESeq)) THEN
                  J = J + 1
                  WRITE(GLSHFS_Config%ScenarioNames(J), 1055, IOSTAT=IOS) Yr
                  IF (IOS .NE. 0) THEN
                     ErrorMessage = 'Error creating list of scenario names (years).';   CALL PassMsg
                     GOTO 898
                  END IF
               END IF
            END DO
         END IF
      END IF
      
      !
      !  If forecast met source is 'extractfromhist' then build the outlook met files from 
      !  the historical data files;  1 per entry in ScenarioNames.
      !
      IF (GLSHFS_Config%ForecastMetSource .EQ. 1) THEN
         CALL SequenceDate(FcDay, FcMon, FcYear, ActualForecastStartDate); IF (ErrorLevel .NE. 0) GOTO 899
         DO Lk = 1, 7
            Bsn = TRIM(LakeName3(Lk))
            CALL BuildHistEnsembleFiles(Bsn, FcYear, FcMon, FcDay, NumDays);          IF (ErrorLevel .NE. 0) GOTO 899
         END DO
      END IF
      
      !
      !  If forecast met source is 'usersupplied' then build the outlook met files from 
      !  the user-supplied met data files;  1 per entry in ScenarioNames.
      !
      IF (GLSHFS_Config%ForecastMetSource .EQ. 2) THEN
         DO Lk = 1, 7
            Bsn = TRIM(LakeName3(Lk))
            CALL BuildUserEnsembleFiles(Bsn, ActualForecastStartDate, ActualForecastEndDate);  IF (ErrorLevel .NE. 0) GOTO 899
         END DO
      END IF
      
      
         
      RETURN
      
      !
      !  Error handling
      !
  701 ErrorMessage = 'Unable to get date extent from file '   // TRIM(FName);  CALL PassMsg;  GOTO 898
  702 ErrorMessage = 'Unable to determine historical data period of record.';  CALL PassMsg;  GOTO 898
  703 ErrorMessage = 'Invalid forecast start date. Unable to run forecasts.';  CALL PassMsg;  GOTO 898

  811 ErrorMessage = 'Error opening file ' // TRIM(FName);   CALL PassMsg;  GOTO 898


  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] Build_Outlook_Meteorology...';  CALL PassMsg
      IF (U1 .GT. 0) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF
      
 1001 FORMAT(A, A, A1)
 1002 FORMAT(A, 'subdata_', A3, I2.2, '.csv')
 1055 FORMAT(I4.4, '  ')
 
      END SUBROUTINE Build_Outlook_Meteorology


      
      !---------------------------------------------------------------------
      !  Fill missing data values by using the long-term daily mean value
      !  (from that subbasin) for the missing day of the year.
      !---------------------------------------------------------------------
      SUBROUTINE FillMissingData(MetData, DT, SSeq, ESeq)
      IMPLICIT NONE
      REAL, DIMENSION(:,:), INTENT(INOUT) :: MetData
      INTEGER,              INTENT(IN)    :: DT, SSeq, ESeq

      INTEGER :: I, J, Seq
      INTEGER, DIMENSION(365) :: Count_LT
      REAL,    DIMENSION(365) :: Total_LT, Mean_LT
      
      !
      !  Compute long-term mean values for each of the first 365 days of the year
      !
      Count_LT(:) = 0
      Total_LT(:) = 0.0
      DO Seq = SSeq, ESeq
         I = Seq - SSeq + 1
         IF (MetData(DT,I) .GT. MissingData_Real_Test) THEN
            J = DayOfYear(Seq)
            IF (J .EQ. 366) J = 365
            Total_LT(J) = Total_LT(J) + MetData(DT,I)
            Count_LT(J) = Count_LT(J) + 1
         END IF
      END DO
      Mean_LT(:) = MissingData_Real
      DO J = 1, 365
         IF (Count_LT(J) .GT. 0) THEN
            Mean_LT(J) = Total_LT(J) / Count_LT(J)
         END IF
      END DO
      
      !
      !  Fill any missing data values in the original MetData array
      !
      DO Seq = SSeq, ESeq
         I = Seq - SSeq + 1
         IF (MetData(DT,I) .LE. MissingData_Real_Test) THEN
            J = DayOfYear(Seq)
            IF (J .EQ. 366) J = 365
            MetData(DT,I) = Mean_LT(J)
         END IF
      END DO

      END SUBROUTINE FillMissingData

!--------------------------------------------------------------------------------------
      !--------------------------------------------------------------------------------
      !  Builds an ensemble of outlook meteorology files from the user-supplied data files
      !  using information in the config data structure.  We will build outlook data 
      !  sets from each user-supplied file named userdata_xxxnn_scode.csv, where
      !    xxx   = lake code (sup, mic, hur, etc)
      !    nn    = subbasin number (00..99)
      !    scode = scenario name (up to 6-characters in length)
      !  The resulting files will be written to the forecast directory, and will be named
      !  otlkdata_xxxnn_scode.csv
      !  They will all start on the forecast start date and contain data for the extent of 
      !  the forecast period.
      !
      !  There will be no filling in of missing data. User-supplied files MUST have data
      !  for every element on every day.
      !--------------------------------------------------------------------------------
      SUBROUTINE BuildUserEnsembleFiles(Bsn, FcStart, FcEnd)
      IMPLICIT NONE
      CHARACTER(LEN=3),  INTENT(IN) :: Bsn
      INTEGER,           INTENT(IN) :: FcStart, FcEnd

      INTEGER :: I, K1, K2, DT, LkNum, IOS
      INTEGER :: Sub, NumDays
      INTEGER :: FSSeq, FESeq
      INTEGER :: NumOtlks, NumDone, NumScenarios
      LOGICAL :: OK
      CHARACTER(LEN=6)   :: Scen
      CHARACTER(LEN=200) :: FName, OtlkDir, UserDir
      REAL, DIMENSION(:), ALLOCATABLE :: MetIn, MetOut
      
      TYPE (TDlyData)                      :: TDD 
      TYPE (TDlyData),           POINTER   :: TDDP
      TYPE (TDlyDataForSubbasin)           :: DataIn, OtlkData
      TYPE (THeaderInfoType)               :: HdrInfo

      !
      !  Do the initial creation of the data objects so that if an error causes
      !  short-circuit execution and exit from the routine, the resulting finalization
      !  steps do not cause an error (e.g. trying to Clear() a non-existent object).
      !
      TDD      = TDlyData()
      DataIn   = TDlyDataForSubbasin()
      OtlkData = TDlyDataForSubbasin()
      HdrInfo  = THeaderInfoType()
      
      NumScenarios = UBOUND(GLSHFS_Config%ScenarioNames, 1)
      
      !
      !  Convert Bsn into a lake number for use when accessing arrays
      !
      LkNum = LakeNumberFromName3(Bsn)
      IF (LkNum .LT. 1) THEN
         ErrorMessage = 'Error: Invalid basin specified ['//TRIM(Bsn)//']'; CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Set UserDir to the correct value for the userdata_*.csv files
      !  Set OtlkDir to the correct value for the otlkdata_*.csv files
      !
      UserDir = NormalizedFilePath(TRIM(GLSHFS_Config%UserMetLocation))
      OtlkDir = TRIM(GLSHFS_Config%BaseDir) // TRIM(GLSHFS_Config%ForecastName) // FilePathSeparator
      
      !
      !  Allocate array that will be repeatedly used to temporarily hold data values
      !
      NumDays = FcEnd - FcStart + 1
      ALLOCATE(MetOut(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for outlook met data';  CALL PassMsg
         GOTO 898
      END IF
      MetOut(:) = MissingData_Real
      
      !
      !  How many outlook files will we be creating for this lake?
      !
      NumOtlks = NumScenarios * (NumSubbasins(LkNum)+1)
      NumDone  = 0
      
      !
      !  Main loop is the subbasins
      !
      DO Sub = 0, NumSubbasins(LkNum)
         DO I = 1, NumScenarios
            NumDone = NumDone + 1
            Scen = TRIM(GLSHFS_Config%ScenarioNames(I))
            WRITE(StatusMsg, 5001) Bsn, Sub, TRIM(Scen), NumDone, NumOtlks, Bsn;   CALL WriteMsg(.TRUE., .TRUE.)
            CALL DataIn%Clear()
            WRITE(FName, 2000) TRIM(UserDir), Bsn, Sub, TRIM(Scen)
            CALL ReadFile_OneSubbasin(FName, DataIn); IF (ErrorLevel .NE. 0) GOTO 899
            FSSeq = DataIn%GetStartDate()
            FESeq = DataIn%GetEndDate()
            
            CALL OtlkData%Clear()
            OtlkData%Bsn      = Bsn
            OtlkData%SubNum   = Sub
            OtlkData%SubArea  = DataIn%SubArea
            OtlkData%SDateSeq = FcStart
            OtlkData%EDateSeq = FcEnd
            OtlkData%NumDays  = FcEnd - FcStart + 1
            OtlkData%NumDataSets = 0

            K1 = FcStart - FSSeq + 1      ! offset into the data read in order to start with FcStart date
            K2 = FcEnd   - FSSeq + 1      ! offset into the data read in order to stop  with FcEnd date

            DO DT = 1, 9
               CALL TDD%Clear()
               OK = TDD%SetDataType(MyDataTypes(DT));                          IF (.NOT. OK) GOTO 899
               TDDP => DataIn%GetPointerToDataOfType(MyDataTypes(DT))
               IF (ASSOCIATED(TDDP)) THEN
                  OK = TDD%SetDataUnit(TDDP%GetDataUnit());                    IF (.NOT. OK) GOTO 899
                  IF (ALLOCATED(MetIn)) DEALLOCATE(MetIn, STAT=IOS)
                  MetIn = TDDP%GetDataVals();                                  IF (.NOT. OK) GOTO 899
                  MetOut(:) = MetIn(K1:K2)
               ELSE
                  OK = TDD%SetDataUnit(DefaultUnitForType(MyDataTypes(DT)));   IF (.NOT. OK) GOTO 899
                  MetOut(:) = MissingData_Real
               END IF
               OK = TDD%AssignData(FcStart, FcEnd, MetOut);                    IF (.NOT. OK) GOTO 899
               OK = OtlkData%AddDataset(TDD);                                  IF (.NOT. OK) GOTO 899
            END DO
            WRITE(FName, 2000) TRIM(OtlkDir), Bsn, Sub, TRIM(Scen)
            CALL WriteFile_OneSubbasin(FName, OtlkData);              IF (ErrorLevel .NE. 0) GOTO 899
         END DO
      END DO
     
      !
      !  We have been writing status message with no advance to the next line, and
      !  the "pointer" is still sitting at the end of the last line written.
      !  In order to drop to the next line, we just need to write a blank status 
      !  message at the end of the current line.
      !
      StatusMsg = '';  CALL WriteMsg()

      GOTO 999

      !
      !  Error handling
      !

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] BuildUserEnsembleFiles...'; CALL PassMsg

  999 CALL DataIn%Clear()
      CALL OtlkData%Clear()
      CALL TDD%Clear()
      IF (ALLOCATED(MetIn))  DEALLOCATE(MetIn,  STAT=IOS)
      IF (ALLOCATED(MetOut)) DEALLOCATE(MetOut, STAT=IOS)
      RETURN

 2000 FORMAT(A, 'otlkdata_', A3, I2.2, '_', A, '.csv')
 5001 FORMAT('Building otlkdata_', A3, I2.2, '_', A, '.csv;  ', I0, '/', I0, ' for ', A3)

      END SUBROUTINE BuildUserEnsembleFiles

!--------------------------------------------------------------------------------------
      !--------------------------------------------------------------------------------
      !  Builds an ensemble of outlook meteorology files from the historical data files
      !  using the information in the config data structure.  It is assumed that the 
      !  ScenarioNames array has a list of years (as text strings) that we will use as 
      !  the starting year for each ensemble member.  For example, if ScenarioNames has 
      !  entries "1950", "1951", "1952" then we will build outlook data sets that start 
      !  in those years (starting on forecast start date) and extend for the extent 
      !  of the forecast.
      !--------------------------------------------------------------------------------
      SUBROUTINE BuildHistEnsembleFiles(Bsn, FcYear, FcMonth, FcDay, NumDays)
      IMPLICIT NONE
      CHARACTER(LEN=3),  INTENT(IN) :: Bsn
      INTEGER,           INTENT(IN) :: FcYear, FcMonth, FcDay, NumDays

      INTEGER :: I, K1, K2, DT, DType, LkNum, IOS
      INTEGER :: Yr, SSeq, ESeq, Sub, MetDays
      INTEGER :: FcStart, FcEnd, FSSeq, FESeq
      INTEGER :: NumOtlks, NumDone, NumScenarios
      LOGICAL :: OK
      CHARACTER(LEN=6)   :: Scen
      CHARACTER(LEN=200) :: FName, LakeDir, OtlkDir

      TYPE (TDlyData)                      :: TDD 
      TYPE (TDlyData),           POINTER   :: TDDP
      TYPE (TDlyDataForSubbasin)           :: SubData, OtlkData
      TYPE (THeaderInfoType)               :: HdrInfo
      REAL,    DIMENSION(:),   ALLOCATABLE :: OutVals
      REAL,    DIMENSION(:,:), ALLOCATABLE :: MetData

      !
      !  Do the initial creation of the data objects so that if an error causes
      !  short-circuit execution and exit from the routine, the resulting finalization
      !  steps do not cause an error (e.g. trying to Clear() a non-existent object).
      !
      TDD      = TDlyData()
      SubData  = TDlyDataForSubbasin()
      OtlkData = TDlyDataForSubbasin()
      HdrInfo  = THeaderInfoType()
      
      NumScenarios = UBOUND(GLSHFS_Config%ScenarioNames, 1)
      
      !
      !  Convert Bsn into a lake number for use when accessing arrays
      !
      LkNum = LakeNumberFromName3(Bsn)
      IF (LkNum .LT. 1) THEN
         ErrorMessage = 'Error: Invalid basin specified ['//TRIM(Bsn)//']'; CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Set LakeDir to the correct value for this lake
      !  Set OtlkDir to the correct value for the otlkdata_*.csv files
      !
      LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator
      OtlkDir = TRIM(GLSHFS_Config%BaseDir) // TRIM(GLSHFS_Config%ForecastName) // FilePathSeparator

      !
      !  Set the outlook data dates. These are the actual forecast dates.
      !  The output files otlkdata_xxxnn_yyyy.csv will contain data for this period.
      !
      CALL DateSequence(FcDay, FcMonth, FcYear, FcStart); IF (ErrorLevel .NE. 0) GOTO 899
      FcEnd = FcStart + NumDays - 1
      
      !
      !  Allocate array that will be repeatedly used to store a single time-series
      !  of the outlook data.
      !
      ALLOCATE(OutVals(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for outlook met series';  CALL PassMsg
         GOTO 898
      END IF
      OutVals(:) = MissingData_Real

      !
      !  How many outlook files will we be creating for this lake?
      !
      NumOtlks = NumScenarios * (NumSubbasins(LkNum)+1)
      NumDone  = 0
      
      !
      !  Allocate the met data storage array, assuming all subbasins have identical
      !  periods of record.
      !
      CALL SubData%Clear()
      WRITE(FName, 1003) TRIM(LakeDir), Bsn, 0
      CALL ReadJustHeaderInfo(FName, HFT_SingleSubbasin, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
      FSSeq = HdrInfo%SDate
      FESeq = HdrInfo%EDate
      MetDays = FESeq - FSSeq + 1
      ALLOCATE(MetData(9,MetDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for outlook meteorology storage';  CALL PassMsg
         GOTO 898
      END IF
      MetData(:,:) = MissingData_Real
      
      !
      !  Main loop is the subbasins
      !
      DO Sub = 0, NumSubbasins(LkNum)
         CALL SubData%Clear()
         WRITE(FName, 1003) TRIM(LakeDir), Bsn, Sub
         CALL ReadFile_OneSubbasin(FName, SubData); IF (ErrorLevel .NE. 0) GOTO 899
         FSSeq = SubData%GetStartDate()
         FESeq = SubData%GetEndDate()

         !
         !  The SubData object is useful for organizing the data, but somewhat 
         !  inefficient and clumsy for repeatedly accessing the data values, and
         !  there is no good way to access a subsection of the data, so let's
         !  transfer the actual met data values into a simple 2-D array.
         !  MetData(DType, DayNum)
         !
         MetData(:,:) = MissingData_Real
         DO DT = 1, 9
            DType = MyDataTypes(DT)
            TDDP => SubData%GetPointerToDataOfType(DType)
            IF (ASSOCIATED(TDDP)) MetData(DT,:) = TDDP%GetDataVals()
         END DO

         !
         !  The time-series of data just read may include missing values. That is
         !  not acceptable for use in driving the models, which is our goal here.
         !  So as a prophylactic step, fill in any missing data.
         !
         !  Note that when this step finishes, there still could be missing data in the
         !  MetData array if the procedure was unable to fill it in.  I will not
         !  treat that as an error condition, because the missing value may be for
         !  a data type and/or date that we don't need.
         !  We will need to test for missing data at the time we build the actual
         !  time-series of data.
         !
         !  Also note that I am only going to fill in the meteorological variables. 
         !  The radiation data is so sporadic that I do not want to fill it in.
         !  Any long-term values for that would be based on a very small set of 
         !  values and would likely be spurious.  Standard forcing for the LLTM 
         !  in GLSHFS is going to be cloud cover data derived from station observations.
         !  If we end up with radiation values to drive LLTM they will usually/always (?) 
         !  be from model output, and there should be no missing data.
         !
         DO DT = 1, 7
            CALL FillMissingData(MetData, DT, FSSeq, FESeq); IF (ErrorLevel .NE. 0) GOTO 899
         END DO

         !
         !  For each year in the list, try to create an outlook met file starting on
         !  FcMonth/FcDay of the specified year.
         !
         DO I = 1, NumScenarios
            Scen = TRIM(GLSHFS_Config%ScenarioNames(I))
            NumDone = NumDone + 1
            WRITE(StatusMsg, 5001) Bsn, Sub, TRIM(Scen), NumDone, NumOtlks, Bsn;   CALL WriteMsg(.TRUE., .TRUE.)
            IF (GLSHFS_Config%ForecastMetSource .EQ. 1) THEN       ! 1 = extract from historical data sets
               READ(Scen, *, IOSTAT=IOS) Yr
               IF (IOS .EQ. 0) THEN
                  IF ((Yr .GT. 1000) .AND. (Yr .LT. 3000)) THEN
                     CALL DateSequence(FcDay, FcMonth, Yr, SSeq); IF (ErrorLevel .NE. 0) GOTO 899
                     ESeq = SSeq + NumDays - 1
                     IF ((SSeq .GE. FSSeq) .AND. (ESeq .LE. FESeq)) THEN
                        CALL OtlkData%Clear()
                        OtlkData%Bsn      = Bsn
                        OtlkData%SubNum   = Sub
                        OtlkData%SubArea  = SubData%SubArea
                        OtlkData%SDateSeq = FcStart
                        OtlkData%EDateSeq = FcEnd
                        OtlkData%NumDays  = FcEnd - FcStart + 1
                        OtlkData%NumDataSets = 0
                        K1 = SSeq - FSSeq + 1
                        K2 = ESeq - FSSeq + 1
                        DO DT = 1, 9
                           TDDP => SubData%GetPointerToDataOfType(MyDataTypes(DT))
                           OK = TDD%SetDataType(MyDataTypes(DT));              IF (.NOT. OK) GOTO 899
                           IF (ASSOCIATED(TDDP)) THEN
                              OK = TDD%SetDataUnit(TDDP%GetDataUnit());        IF (.NOT. OK) GOTO 899
                              OutVals(:) = MetData(DT, K1:K2)
                           ELSE
                              OK = TDD%SetDataUnit(DefaultUnitForType(MyDataTypes(DT))); IF (.NOT. OK) GOTO 899
                              OutVals(:) = MissingData_Real
                           END IF
                           OK = TDD%AssignData(FcStart, FcEnd, OutVals);       IF (.NOT. OK) GOTO 899
                           OK = OtlkData%AddDataset(TDD);                      IF (.NOT. OK) GOTO 899
                        END DO
                        WRITE(FName, 2000) TRIM(OtlkDir), Bsn, Sub, TRIM(Scen)
                        CALL WriteFile_OneSubbasin(FName, OtlkData); IF (ErrorLevel .NE. 0) GOTO 899
                     END IF
                  END IF
               END IF
            END IF
         END DO
      END DO
     
      !
      !  We have been writing status message with no advance to the next line, and
      !  the "pointer" is still sitting at the end of the last line written.
      !  In order to drop to the next line, we just need to write a blank status 
      !  message at the end of the current line.
      !
      StatusMsg = '';  CALL WriteMsg()

      GOTO 999

      !
      !  Error handling
      !

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] BuildHistEnsembleFiles...'; CALL PassMsg

  999 CALL SubData%Clear()
      CALL OtlkData%Clear()
      CALL TDD%Clear()
      IF (ALLOCATED(OutVals)) DEALLOCATE(OutVals, STAT=IOS)
      IF (ALLOCATED(MetData)) DEALLOCATE(MetData, STAT=IOS)
      RETURN

 1003 FORMAT(A, 'subdata_', A3, I2.2, '.csv')
 2000 FORMAT(A, 'otlkdata_', A3, I2.2, '_', A, '.csv')
 5001 FORMAT('Building otlkdata_', A3, I2.2, '_', A, '.csv;  ', I0, '/', I0, ' for ', A3)

      END SUBROUTINE BuildHistEnsembleFiles

      
END MODULE Outlook_Met

      