!--------------------------------------------------------
!  Read various input and output files from the GLSHFS package, then 
!  produce summary file(s) of the information.
!--------------------------------------------------------

MODULE SummaryFiles
      USE GL_Constants
      USE ErrorProcess
      USE StatusMessages
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GLSHFS_Files
      USE GlerlDataTypesAndUnits
      USE DailyDataStructures
      USE BasinInfo

      INTEGER, DIMENSION(13), PARAMETER, PRIVATE :: SubDataTypes =                     &     
        (/ GDT_AirtempMin,          GDT_AirtempMax,          GDT_Precipitation,        &
           GDT_AirtempMean,         GDT_DewpointMean,        GDT_WindSpeed,            &
           GDT_CloudCover,          GDT_Runoff,              GDT_UpperSoilMoisture,    &
           GDT_LowerSoilMoisture,   GDT_GroundWaterMoisture, GDT_SurfaceZoneMoisture,  &
           GDT_SnowWater  /)
           
      INTEGER, DIMENSION(13), PARAMETER, PRIVATE :: SubDataUnits =                     &
        (/ GDU_Celsius,             GDU_Celsius,             GDU_Millimeters,          &
           GDU_Celsius,             GDU_Celsius,             GDU_MetersPerSecond,      &
           GDU_Percent,             GDU_Millimeters,         GDU_Millimeters,          &
           GDU_Millimeters,         GDU_Millimeters,         GDU_Millimeters,          &
           GDU_Millimeters  /)
           
           
      INTEGER, DIMENSION(34), PARAMETER, PRIVATE :: LakeDataTypes =                    &
        (/ GDT_OverlakeRunoff,      GDT_OverlakePrecip,      GDT_Evaporation,          &
           GDT_NetBasinSupply,      GDT_OverlandRunoff,      GDT_OverlandPrecip,       &
           GDT_OverlakeAirtempMin,  GDT_OverlakeAirtempMax,  GDT_OverlakeAirtempMean,  & 
           GDT_OverlakeDewpoint,    GDT_OverlakeWindspeed,   GDT_OverlakeCloudCover,   &
           GDT_OverlandAirtempMin,  GDT_OverlandAirtempMax,  GDT_OverlandAirtempMean,  & 
           GDT_OverlandDewpoint,    GDT_OverlandWindspeed,   GDT_OverlandCloudCover,   &
           GDT_UpperSoilMoisture,   GDT_LowerSoilMoisture,   GDT_GroundWaterMoisture,  &
           GDT_SurfaceZoneMoisture, GDT_SnowWater,           GDT_WaterTemp,            &
           GDT_IceArea,             GDT_IceDepth,            GDT_IceTemp,              &
           GDT_ReflectedRad,        GDT_LatentRad,           GDT_SensibleRad,          &
           GDT_Advection,           GDT_IncidentRad,         GDT_NetLongWaveRad,       &
           GDT_TotalHeat   /)
           
      INTEGER, DIMENSION(34), PARAMETER, PRIVATE :: LakeDataUnits =                    &
        (/ GDU_Millimeters,         GDU_Millimeters,         GDU_Millimeters,          &
           GDU_Millimeters,         GDU_Millimeters,         GDU_Millimeters,          &
           GDU_Celsius,             GDU_Celsius,             GDU_Celsius,              &
           GDU_Celsius,             GDU_MetersPerSecond,     GDU_Percent,              &
           GDU_Celsius,             GDU_Celsius,             GDU_Celsius,              &
           GDU_Celsius,             GDU_MetersPerSecond,     GDU_Percent,              &
           GDU_Millimeters,         GDU_Millimeters,         GDU_Millimeters,          &
           GDU_Millimeters,         GDU_Millimeters,         GDU_Celsius,              &
           GDU_Percent,             GDU_Millimeters,         GDU_Celsius,              &
           GDU_WattsPerM2,          GDU_WattsPerM2,          GDU_WattsPerM2,           &
           GDU_WattsPerM2,          GDU_WattsPerM2,          GDU_WattsPerM2,           &
           GDU_Calories   /)
           
      INTEGER, DIMENSION(20), PARAMETER, PRIVATE :: LLTMDataTypes =                                    &
        (/ GDT_Evaporation,         GDT_WaterTemp,        GDT_IceTemp,           GDT_IceArea,          &
           GDT_IceDepth,            GDT_ReflectedRad,     GDT_LatentRad,         GDT_SensibleRad,      &
           GDT_Advection,           GDT_IncidentRad,      GDT_NetLongWaveRad,    GDT_TotalHeat,        &
           GDT_AirtempMean,         GDT_DewPointMean,     GDT_WindSpeed,         GDT_CloudCover,       &
           GDT_OverlakeAirTempMean, GDT_OverlakeDewpoint, GDT_OverlakeWindSpeed, GDT_OverlakeCloudCover /)
      
      INTEGER, DIMENSION(20), PARAMETER, PRIVATE :: LLTMDataUnits =                                    &
        (/ GDU_Millimeters,         GDU_Celsius,          GDU_Celsius,           GDU_Percent,          &
           GDU_Millimeters,         GDU_WattsPerM2,       GDU_WattsPerM2,        GDU_WattsPerM2,       &
           GDU_WattsPerM2,          GDU_WattsPerM2,       GDU_WattsPerM2,        GDU_Calories,         &
           GDU_Celsius,             GDU_Celsius,          GDU_MetersPerSecond,   GDU_Percent,          &
           GDU_Celsius,             GDU_Celsius,          GDU_MetersPerSecond,   GDU_Percent      /)
      
   CONTAINS
   
!------------------------------------------------------------------------------
      !--------------------------------------------------------------
      SUBROUTINE MakeSummaryFiles_Hist()
      IMPLICIT NONE
      INTEGER :: Lk
      CHARACTER(LEN=3) :: Bsn
      
      DO Lk = 1, 7
         Bsn = LakeName3(Lk)
         CALL Summarize_Hist(Bsn);  IF (ErrorLevel .NE. 0) GOTO 899
      END DO
      RETURN
      
  899 ErrorMessage = '[traceback] MakeSummaryFiles_Hist...';   CALL PassMsg
      
      END SUBROUTINE MakeSummaryFiles_Hist
   
   
!------------------------------------------------------------------------------
      !--------------------------------------------------------------
      SUBROUTINE MakeSummaryFiles_Otlk()
      IMPLICIT NONE
      INTEGER :: Lk, S, NumScen, U1, IOS
      CHARACTER(LEN=3) :: Bsn
      CHARACTER(LEN=6) :: SCode
      CHARACTER(LEN=200) :: FName
      
      NumScen = UBOUND(GLSHFS_Config%ScenarioNames, 1)
      DO Lk = 1, 7
         Bsn = LakeName3(Lk)
         DO S = 1, NumScen
            SCode = GLSHFS_Config%ScenarioNames(S)
            CALL Summarize_Otlk(Bsn, TRIM(SCode))
         END DO
      END DO
      
      DO S = 1, NumScen
         SCode = GLSHFS_Config%ScenarioNames(S)
         CALL Combine_MHG_For_CGLRRM(TRIM(SCode))
      END DO
      
      !
      !  Write an output file listing the scenarios. This file will make things
      !  a little easier for Lauren Fry.
      !  One file per run, named "hindcast_yyyymm_scenarios.txt", where 
      !  yyyymm = year and month of forecast start.  This depends on the 
      !  forecast name being set to "hindcast_yyyymm" in the config file.
      !
      WRITE(FName, 1000) TRIM(GLSHFS_Config%BaseDir), TRIM(GLSHFS_Config%ForecastName)
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)
      DO S = 1, NumScen
         SCode = GLSHFS_Config%ScenarioNames(S)
         WRITE(U1, 1001, IOSTAT=IOS) TRIM(SCode)
         IF (IOS .NE. 0) THEN
            CLOSE(U1);  CALL FileWasClosed(U1)
            GOTO 813
         END IF
      END DO
      CLOSE(U1);  CALL FileWasClosed(U1)
      
      RETURN
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening file' // TRIM(FName); CALL PassMsg;  GOTO 898
  813 ErrorMessage = 'Error writing file' // TRIM(FName); CALL PassMsg;  GOTO 898
      
  898 ErrorLevel = 1
      ErrorMessage = '[traceback] MakeSummaryFiles_Otlk...';   CALL PassMsg
 
 1000 FORMAT(A, A, '_scenarios.txt')
 1001 FORMAT(A)
      
      END SUBROUTINE MakeSummaryFiles_Otlk
   
   
!------------------------------------------------------------------------------
      !--------------------------------------------------------------
      !  Summarize the historical period.
      !
      !  Two sets/types of files are created.  
      !  The first set of output summary files is a set of subbasin summary files. 
      !  These are only for the land subbasins, 1 to n.  These might be considered
      !  redundant, since they are just a consolidation of the meteorology and
      !  LBRM output.  But that does seem somewhat useful to me.
      !
      !  The second is more useful. It summarizes lakewide and/or landwide values.
      !  Meteorology, output from LBRM and LLTM, and component NBS are all in this
      !  one.
      !
      !  Data stored in each array slice: 
      !   Index  LandSubSummary           LakeBasinSummary
      !    1     AirTempMin (C)           Runoff - overlake (mm)
      !    2     AirTempMax (C)           Precip - overlake (mm)
      !    3     Precipitation (mm)       Evaporation - overlake (mm)
      !    4     AirTempMean (C)          Net Basin Supply - overlake (mm)
      !    5     Dewpoint (C)             Runoff as depth over land (mm)
      !    6     Wind Speed (m/s)         Precip - overland (mm)
      !    7     Cloud (%)                AirTempMin - overlake (C)
      !    8     Runoff (mm)              AirTempMax - overlake (C)
      !    9     USZM (mm)                AirTempMean - overlake (C)   as adjusted by LLTM
      !   10     LSZM (mm)                Dewpoint - overlake (C)      as adjusted by LLTM
      !   11     GroundZM (mm)            Windspeed - overlake (m/s)   as adjusted by LLTM
      !   12     SurfaceZM (mm)           CloudCover - overlake (%)
      !   13     SnowWaterEq (mm)         AirTempMin - overland (C)
      !   14                              AirTempMax - overland (C)
      !   15                              AirTempMean - overland (C)
      !   16                              Dewpoint - overland (C)
      !   17                              Windspeed - overland (m/s)
      !   18                              CloudCover - overland (%)
      !   19                              USZM (mm)
      !   20                              LSZM (mm)
      !   21                              GZM (mm)
      !   22                              Surface ZM (mm)
      !   23                              SWE (mm)
      !   24                              Water Surface Temp (C)
      !   25                              Ice Cover (%)
      !   26                              Ice Depth (mm)
      !   27                              Ice Temp (C)
      !   28                              ReflectedRad (w/m2)
      !   29                              LatentRad (w/m2)
      !   30                              SensibleRad (w/m2)
      !   31                              AdvectionRad (w/m2)
      !   32                              IncidentRad (w/m2)
      !   33                              NetLongWaveRad (w/m2)
      !   34                              Stored Heat (calories)
      !--------------------------------------------------------------
      SUBROUTINE Summarize_Hist(Bsn)
      IMPLICIT NONE
      CHARACTER(LEN=3),  INTENT(IN) :: Bsn
      
      INTEGER :: I, J, K, DT, IOS, Sub, LkNum
      INTEGER :: Seq, SSeq, ESeq, NumDays
      INTEGER :: OldUnits, NewUnits
      LOGICAL :: OK
      REAL    :: Area, LandArea, LandToLakeRatio
      REAL    :: Run, Prc, Evp
      CHARACTER(LEN=200) :: LakeDir, FName

      REAL, DIMENSION(:),   ALLOCATABLE :: DD
      REAL, DIMENSION(:,:), ALLOCATABLE :: LandSubSummary, LakeBasinSummary
      REAL, DIMENSION(:,:), ALLOCATABLE :: AccTot, AccArea
      
      TYPE (BasinInfoData)       :: BData
      TYPE (THeaderInfoType)     :: HdrInfo
      TYPE (TDlyDataForSubbasin) :: SubData, SubOut
      TYPE (TDlyDataForLake)     :: LakeData, LakeOut
      TYPE (TDlyData)            :: TDD
      TYPE (TDlyData), POINTER   :: TDDP

      !
      LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator
      LkNum = LakeNumberFromName3(Bsn)

      !
      !  Initialize the class objects to protect against Finalize errors
      !
      HdrInfo  = THeaderInfoType()
      SubData  = TDlyDataForSubbasin()
      SubOut   = TDlyDataForSubbasin()
      LakeData = TDlyDataForLake()
      LakeOut  = TDlyDataForLake()
      TDD     = TDlyData()
      
      !
      !  Read the basininfo.txt file to get the subbasin areas
      !
      WRITE(FName, 1001) TRIM(LakeDir)
      CALL ReadBasinInfoFile(TRIM(FName), BData);  IF (ErrorLevel .NE. 0) GOTO 899
      LandArea = 0.0
      DO Sub = 1, NumSubbasins(LkNum)
         LandArea = LandArea + BData%SubbasinArea(Sub)
      END DO
      LandToLakeRatio = LandArea / BData%LakeArea      ! factor to convert overland depth into overlake depth (e.g. runoff)
      
      !
      !  Read the subbasin 1 meteorology file header to get the dates.  I am going to
      !  assume that the rest of the subbasins have the same period of record. If that
      !  is incorrect, we may get an error later on.
      !
      WRITE(FName, 1002) TRIM(LakeDir), Bsn, 1
      CALL ReadJustHeaderInfo(FName, HFT_SingleSubbasin, HdrInfo);  IF (ErrorLevel .NE. 0) GOTO 899
      SSeq = HdrInfo%SDate
      ESeq = HdrInfo%EDate
      NumDays = ESeq - SSeq + 1
      
      !
      !  Allocate memory to hold the relevant data, including large area accumulators.
      !
      ALLOCATE(LandSubSummary(13, NumDays), LakeBasinSummary(34, NumDays),        &
               AccTot(13, NumDays), AccArea(13, NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory.';  CALL PassMsg
         GOTO 898
      END IF
      LakeBasinSummary(:,:) = MissingData_Real 
      AccTot(:,:) = 0.0
      AccArea(:,:)  = 0.0

      !
      !  Read the historical daily data from each land subbasin
      !
      DO Sub = 1, NumSubbasins(LkNum)
         Area = BData%SubbasinArea(Sub)
         LandSubSummary(:,:)  = MissingData_Real
         
         WRITE(StatusMsg, 2001) Bsn, Sub;   CALL WriteMsg(.TRUE., .TRUE.)
         
         !
         !  subbasin meteorology
         !
         WRITE(FName, 1002) TRIM(LakeDir), Bsn, Sub
         CALL ReadFile_OneSubbasin(FName, SubData);  IF (ErrorLevel .NE. 0) GOTO 899
         DO I = 1, 7
            DT = SubDataTypes(I)
            TDDP => SubData%GetPointerToDataOfType(DT)
            DD = TDDP%GetDataVals()
            OldUnits = TDDP%GetDataUnit()
            NewUnits = SubDataUnits(I)
            DO J = 1, NumDays
               LandSubSummary(I,J) = UnitConvertedDataValue(DD(J), OldUnits, NewUnits)
            END DO
         END DO
         
         !
         !  subbasin LBRM results
         !
         WRITE(FName, 1003) TRIM(LakeDir), Bsn, Sub
         CALL ReadFile_LBRM(TRIM(FName), SubData);  IF (ErrorLevel .NE. 0) GOTO 899
         DO I = 8, 13
            DT = SubDataTypes(I)
            TDDP => SubData%GetPointerToDataOfType(DT)
            DD = TDDP%GetDataVals()
            OldUnits = TDDP%GetDataUnit()
            NewUnits = SubDataUnits(I)
            DO J = 1, NumDays
               LandSubSummary(I,J) = UnitConvertedDataValue(DD(J), OldUnits, NewUnits)
            END DO
         END DO

         !
         !  Consolidate the subbasin's data into SubOut for output
         !
         CALL SubOut%Clear()
         SubOut%Bsn     = Bsn
         SubOut%SubNum  = Sub
         SubOut%SubArea = SubData%SubArea
         CALL TDD%Clear()
         DO I = 1, 13
            OK = TDD%SetDataType(SubDataTypes(I));                 IF (.NOT. OK) GOTO 898
            OK = TDD%SetDataUnit(SubDataUnits(I));                 IF (.NOT. OK) GOTO 898
            OK = TDD%AssignData(SSeq, ESeq, LandSubSummary(I,:));  IF (.NOT. OK) GOTO 898
            OK = SubOut%AddDataset(TDD);                           IF (.NOT. OK) GOTO 898
         END DO
         WRITE(FName, 1101) TRIM(LakeDir), Bsn, Sub
         CALL WriteFile_SubbasinSummary(TRIM(FName), SubOut);  IF (ErrorLevel .NE. 0) GOTO 899
         
         !
         !  Add the data from this subbasin to the relevant accumulators.
         !  We don't accumulate everything.
         !
         CALL AddToAccum(AccTot( 1,:), AccArea( 1,:), LandSubSummary( 8,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! runoff as depth over land
         CALL AddToAccum(AccTot( 2,:), AccArea( 2,:), LandSubSummary( 1,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! TMin
         CALL AddToAccum(AccTot( 3,:), AccArea( 3,:), LandSubSummary( 2,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! TMax
         CALL AddToAccum(AccTot( 4,:), AccArea( 4,:), LandSubSummary( 4,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! TMean
         CALL AddToAccum(AccTot( 5,:), AccArea( 5,:), LandSubSummary( 5,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! DewP
         CALL AddToAccum(AccTot( 6,:), AccArea( 6,:), LandSubSummary( 6,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! Wind
         CALL AddToAccum(AccTot( 7,:), AccArea( 7,:), LandSubSummary( 7,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! Cloud
         CALL AddToAccum(AccTot( 8,:), AccArea( 8,:), LandSubSummary( 9,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! USZM
         CALL AddToAccum(AccTot( 9,:), AccArea( 9,:), LandSubSummary(10,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! LSZM
         CALL AddToAccum(AccTot(10,:), AccArea(10,:), LandSubSummary(11,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! GZM
         CALL AddToAccum(AccTot(11,:), AccArea(11,:), LandSubSummary(12,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! Surf
         CALL AddToAccum(AccTot(12,:), AccArea(12,:), LandSubSummary(13,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! SWE
         CALL AddToAccum(AccTot(13,:), AccArea(13,:), LandSubSummary( 3,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! precipitation depth over land
      END DO

      WRITE(StatusMsg, 2002) Bsn;   CALL WriteMsg(.TRUE., .TRUE.)
      
      !
      !  Now compute the lumped values for the accumulated quantities
      !  These are all expressed as a lumped overLAND value.
      !  I = index into the accumulator arrays
      !  J = index into the final summary output array for lake/land values
      !
      DO I = 1, 13
         IF (I .EQ.  1) J =  5          ! runoff
         IF (I .EQ.  2) J = 13          ! TMin
         IF (I .EQ.  3) J = 14          ! TMax
         IF (I .EQ.  4) J = 15          ! TMean
         IF (I .EQ.  5) J = 16          ! Dewpt
         IF (I .EQ.  6) J = 17          ! Wind
         IF (I .EQ.  7) J = 18          ! Cloud
         IF (I .EQ.  8) J = 19          ! USZM
         IF (I .EQ.  9) J = 20          ! LSZM
         IF (I .EQ. 10) J = 21          ! GZM
         IF (I .EQ. 11) J = 22          ! Surf
         IF (I .EQ. 12) J = 23          ! SWE
         IF (I .EQ. 13) J =  6          ! Precip
         
         DO K = 1, NumDays
            IF (AccArea(I,K) .GT. 0) THEN
               LakeBasinSummary(J,K) = AccTot(I,K) / AccArea(I,K)
            END IF
         END DO
      END DO
      
      !
      !  Convert runoff as a depth over land (index 5) into a depth over the lake
      !  and assign it into the big array at index 1.
      !
      DO K = 1, NumDays
         IF (LakeBasinSummary(5,K) .GT. MissingData_Real_Test) THEN
            LakeBasinSummary(1,K) = LakeBasinSummary(5,K) * LandToLakeRatio
         END IF
      END DO
      
      !
      !  Read the historical daily data from each lake surface
      !
      Area = BData%SubbasinArea(0)
      LandSubSummary(:,:) = MissingData_Real
         
      !
      !  Subbasin meteorology for the lake surface.
      !  This is just like any of the land subbasins, but it gets 
      !  assigned into the LakeBasinSummary array.
      !
      WRITE(FName, 1002) TRIM(LakeDir), Bsn, 0
      CALL ReadFile_OneSubbasin(FName, SubData);  IF (ErrorLevel .NE. 0) GOTO 899
      DO I = 1, 7
         IF (I .EQ. 1) J =  7    ! TMin
         IF (I .EQ. 2) J =  8    ! TMax
         IF (I .EQ. 3) J =  2    ! overlake precip
         IF (I .EQ. 4) J = -1    ! Raw values of TMean, Dewpt, Wind, Cloud are not
         IF (I .EQ. 5) J = -1    !   used in the summary file. Instead, the adjusted
         IF (I .EQ. 6) J = -1    !   values output by LLTM are used in the
         IF (I .EQ. 7) J = -1    !   summary file
         IF (J .GT. 0) THEN
            DT = SubDataTypes(I)
            TDDP => SubData%GetPointerToDataOfType(DT)
            DD = TDDP%GetDataVals()
            OldUnits = TDDP%GetDataUnit()
            NewUnits = SubDataUnits(I)
            DO K = 1, NumDays
               LakeBasinSummary(J,K) = UnitConvertedDataValue(DD(K), OldUnits, NewUnits)
            END DO
         END IF
      END DO
         
      !
      !  LLTM results for the lake surface
      !
      WRITE(FName, 1004) TRIM(LakeDir), Bsn
      CALL ReadFile_LLTM(FName, SubData);  IF (ErrorLevel .NE. 0) GOTO 899
      DO I = 1, 20
         IF (I .EQ.  1) J =  3    ! evap
         IF (I .EQ.  2) J = 24    ! Water Temp
         IF (I .EQ.  3) J = 27    ! ice temp
         IF (I .EQ.  4) J = 25    ! ice cover %
         IF (I .EQ.  5) J = 26    ! ice depth 
         IF (I .EQ.  6) J = 28    ! reflected
         IF (I .EQ.  7) J = 29    ! latent
         IF (I .EQ.  8) J = 30    ! sensible
         IF (I .EQ.  9) J = 31    ! advection
         IF (I .EQ. 10) J = 32    ! incident
         IF (I .EQ. 11) J = 33    ! net lw
         IF (I .EQ. 12) J = 34    ! total heat
         IF (I .EQ. 13) J = -1    ! Raw met values are
         IF (I .EQ. 14) J = -1    ! not used in
         IF (I .EQ. 15) J = -1    ! the summary file.
         IF (I .EQ. 16) J = -1    ! 
         IF (I .EQ. 17) J =  9    ! overlake TMean as adjusted by LLTM
         IF (I .EQ. 18) J = 10    ! overlake Dewpt as adjusted by LLTM
         IF (I .EQ. 19) J = 11    ! overlake Windspeed as adjusted by LLTM
         IF (I .EQ. 20) J = 12    ! overlake Cloud cover "as adjusted", which is no adjustment
         IF (J .GT. 0) THEN
            DT = LLTMDataTypes(I)
            TDDP => SubData%GetPointerToDataOfType(DT)
            DD = TDDP%GetDataVals()
            OldUnits = TDDP%GetDataUnit()
            NewUnits = LLTMDataUnits(I)
            DO K = 1, NumDays
               LakeBasinSummary(J,K) = UnitConvertedDataValue(DD(K), OldUnits, NewUnits)
            END DO
         END IF
      END DO

      !
      !  Net Basin Supply values
      !
      LakeBasinSummary(4,:) = MissingData_Real
      DO Seq = SSeq, ESeq
         I = Seq - SSeq + 1
         Run = LakeBasinSummary(1,I) 
         Prc = LakeBasinSummary(2,I) 
         Evp = LakeBasinSummary(3,I) 
         OK = .TRUE.
         IF (IsMissing(Run)) OK = .FALSE.
         IF (IsMissing(Prc)) OK = .FALSE.
         IF (IsMissing(Evp)) OK = .FALSE.
         IF (OK) LakeBasinSummary(4,I) = Run + Prc - Evp
      END DO

      !
      !  Build the LakeOut object to contain all of the lake/basin data and then output it to a file.
      !
      CALL LakeOut%Clear()
      LakeOut%Bsn = GetLowercase(Bsn)
      CALL TDD%Clear()
      DO I = 1, 34
         OK = TDD%SetDataType(LakeDataTypes(I));                  IF (.NOT. OK) GOTO 898
         OK = TDD%SetDataUnit(LakeDataUnits(I));                  IF (.NOT. OK) GOTO 898
         OK = TDD%AssignData(SSeq, ESeq, LakeBasinSummary(I,:));  IF (.NOT. OK) GOTO 898
         OK = LakeOut%AddDataset(TDD);                            IF (.NOT. OK) GOTO 898
      END DO
      WRITE(FName, 1102) TRIM(LakeDir), Bsn
      CALL WriteFile_LakeSummary(TRIM(FName), LakeOut);  IF (ErrorLevel .NE. 0) GOTO 899
      
      StatusMsg = '';   CALL WriteMsg()
      GOTO 999
      
      !
      !  Error handling
      !
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] Summarize_Hist...';   CALL PassMsg
      GOTO 999
  
      !
      !  Final cleanup
      !
  999 DEALLOCATE(LandSubSummary, LakeBasinSummary, AccTot, AccArea, STAT=IOS)
  
      !
      !  FORMATs
      !      
 1001 FORMAT(A, 'basininfo.txt')
 1002 FORMAT(A, 'subdata_', A3, I2.2, '.csv') 
 1003 FORMAT(A, 'lbrm_output_', A3, I2.2, '.csv') 
 1004 FORMAT(A, 'lltm_output_', A3, '.csv')
      
 1101 FORMAT(A, 'summary_', A3, I2.2, '.csv') 
 1102 FORMAT(A, 'summary_', A3, '.csv') 

 2001 FORMAT('Building summary file for ', A3, I2.2)
 2002 FORMAT('Building summary file for ', A3, ' lake&land    ')
 
      END SUBROUTINE Summarize_Hist

      
!------------------------------------------------------------------------------
      !--------------------------------------------------------------
      !  Summarize the outlook period.
      !
      !  Two sets/types of files are created.  
      !  The first set of output summary files is a set of subbasin summary files. 
      !  These are only for the land subbasins, 1 to n.  These might be considered
      !  redundant, since they are just a consolidation of the meteorology and
      !  LBRM output.  But that does seem somewhat useful to me.
      !  The user can turn off the output of these files with an appropriate setting
      !  in the configuration file.  If they have the line
      !     MakeAllSummaryFiles = FALSE
      !  then only the basin-wide summary files will be output. The subbasin files will not.
      !
      !  The second is more useful. It summarizes lakewide and/or landwide values.
      !  Meteorology, output from LBRM and LLTM, and component NBS are all in this
      !  one.
      !
      !  Data stored in each array slice: 
      !   Index  LandSubSummary           LakeBasinSummary
      !    1     AirTempMin (C)           Runoff - overlake (mm)
      !    2     AirTempMax (C)           Precip - overlake (mm)
      !    3     Precipitation (mm)       Evaporation - overlake (mm)
      !    4     AirTempMean (C)          Net Basin Supply - overlake (mm)
      !    5     Dewpoint (C)             Runoff as depth over land (mm)
      !    6     Wind Speed (m/s)         Precip - overland (mm)
      !    7     Cloud (%)                AirTempMin - overlake (C)
      !    8     Runoff (mm)              AirTempMax - overlake (C)
      !    9     USZM (mm)                AirTempMean - overlake (C)   as adjusted by LLTM
      !   10     LSZM (mm)                Dewpoint - overlake (C)      as adjusted by LLTM
      !   11     GroundZM (mm)            Windspeed - overlake (m/s)   as adjusted by LLTM
      !   12     SurfaceZM (mm)           CloudCover - overlake (%)
      !   13     SnowWaterEq (mm)         AirTempMin - overland (C)
      !   14                              AirTempMax - overland (C)
      !   15                              AirTempMean - overland (C)
      !   16                              Dewpoint - overland (C)
      !   17                              Windspeed - overland (m/s)
      !   18                              CloudCover - overland (%)
      !   19                              USZM (mm)
      !   20                              LSZM (mm)
      !   21                              GZM (mm)
      !   22                              Surface ZM (mm)
      !   23                              SWE (mm)
      !   24                              Water Surface Temp (C)
      !   25                              Ice Cover (%)
      !   26                              Ice Depth (mm)
      !   27                              Ice Temp (C)
      !   28                              ReflectedRad (w/m2)
      !   29                              LatentRad (w/m2)
      !   30                              SensibleRad (w/m2)
      !   31                              AdvectionRad (w/m2)
      !   32                              IncidentRad (w/m2)
      !   33                              NetLongWaveRad (w/m2)
      !   34                              Stored Heat (calories)
      !--------------------------------------------------------------
      SUBROUTINE Summarize_Otlk(Bsn, SCode)
      IMPLICIT NONE
      CHARACTER(LEN=3), INTENT(IN) :: Bsn
      CHARACTER(LEN=*), INTENT(IN) :: SCode
      
      INTEGER :: I, J, K, DT, IOS, Sub, LkNum
      INTEGER :: Seq, SSeq, ESeq, NumDays
      INTEGER :: OldUnits, NewUnits
      LOGICAL :: OK
      REAL    :: Area, LandArea, LandToLakeRatio
      REAL    :: Run, Prc, Evp, NBS_mm
      CHARACTER(LEN=70)  :: Description
      CHARACTER(LEN=200) :: LakeDir, OtlkDir, FName

      REAL, DIMENSION(:),   ALLOCATABLE :: DD, NBS_CMS
      REAL, DIMENSION(:,:), ALLOCATABLE :: LandSubSummary, LakeBasinSummary
      REAL, DIMENSION(:,:), ALLOCATABLE :: AccTot, AccArea
      
      TYPE (BasinInfoData)       :: BData
      TYPE (THeaderInfoType)     :: HdrInfo
      TYPE (TDlyDataForSubbasin) :: SubData, SubOut
      TYPE (TDlyDataForLake)     :: LakeData, LakeOut
      TYPE (TDlyData)            :: TDD
      TYPE (TDlyData), POINTER   :: TDDP

      !
      LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator
      OtlkDir = TRIM(GLSHFS_Config%BaseDir) // TRIM(GLSHFS_Config%ForecastName) // FilePathSeparator
      LkNum = LakeNumberFromName3(Bsn)

      !
      !  Initialize the class objects to protect against Finalize errors
      !
      HdrInfo  = THeaderInfoType()
      SubData  = TDlyDataForSubbasin()
      SubOut   = TDlyDataForSubbasin()
      LakeData = TDlyDataForLake()
      LakeOut  = TDlyDataForLake()
      TDD     = TDlyData()

      !
      !  Read the basininfo.txt file to get the subbasin areas
      !
      WRITE(FName, 1001) TRIM(LakeDir)
      CALL ReadBasinInfoFile(TRIM(FName), BData);  IF (ErrorLevel .NE. 0) GOTO 899
      LandArea = 0.0
      DO Sub = 1, NumSubbasins(LkNum)
         LandArea = LandArea + BData%SubbasinArea(Sub)
      END DO
      LandToLakeRatio = LandArea / BData%LakeArea      ! factor to convert overland depth into overlake depth (e.g. runoff)
      
      !
      !  Read the subbasin 1 meteorology file header to get the dates.  I am going to
      !  assume that the rest of the subbasins have the same period of record. If that
      !  is incorrect, we may get an error later on.
      !
      WRITE(FName, 1002) TRIM(OtlkDir), Bsn, 1, TRIM(SCode)
      CALL ReadJustHeaderInfo(FName, HFT_SingleSubbasin, HdrInfo);  IF (ErrorLevel .NE. 0) GOTO 899
      SSeq = HdrInfo%SDate
      ESeq = HdrInfo%EDate
      NumDays = ESeq - SSeq + 1
      
      !
      !  Allocate memory to hold the relevant data, including large area accumulators.
      !
      ALLOCATE(LandSubSummary(13, NumDays), LakeBasinSummary(34, NumDays),        &
               AccTot(13, NumDays), AccArea(13, NumDays), NBS_CMS(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory.';  CALL PassMsg
         GOTO 898
      END IF
      LakeBasinSummary(:,:) = MissingData_Real 
      AccTot(:,:)   = 0.0
      AccArea(:,:)  = 0.0
      NBS_CMS(:)    = MissingData_Real     

      !
      !  Read the daily data from each land subbasin
      !
      DO Sub = 1, NumSubbasins(LkNum)
         Area = BData%SubbasinArea(Sub)
         LandSubSummary(:,:)  = MissingData_Real
         
         WRITE(StatusMsg, 2001) Bsn, Sub, TRIM(SCode);   CALL WriteMsg(.TRUE., .TRUE.)
         
         !
         !  subbasin meteorology
         !
         WRITE(FName, 1002) TRIM(OtlkDir), Bsn, Sub, TRIM(SCode)
         CALL ReadFile_OneSubbasin(FName, SubData);  IF (ErrorLevel .NE. 0) GOTO 899
         DO I = 1, 7
            DT = SubDataTypes(I)
            TDDP => SubData%GetPointerToDataOfType(DT)
            DD = TDDP%GetDataVals()
            OldUnits = TDDP%GetDataUnit()
            NewUnits = SubDataUnits(I)
            DO J = 1, NumDays
               LandSubSummary(I,J) = UnitConvertedDataValue(DD(J), OldUnits, NewUnits)
            END DO
         END DO
         
         !
         !  subbasin LBRM results
         !
         WRITE(FName, 1003) TRIM(OtlkDir), Bsn, Sub, TRIM(SCode)
         CALL ReadFile_LBRM(TRIM(FName), SubData);  IF (ErrorLevel .NE. 0) GOTO 899
         DO I = 8, 13
            DT = SubDataTypes(I)
            TDDP => SubData%GetPointerToDataOfType(DT)
            DD = TDDP%GetDataVals()
            OldUnits = TDDP%GetDataUnit()
            NewUnits = SubDataUnits(I)
            DO J = 1, NumDays
               LandSubSummary(I,J) = UnitConvertedDataValue(DD(J), OldUnits, NewUnits)
            END DO
         END DO

         !
         !  Output the subbasin summary files, if appropriate
         !
         IF (GLSHFS_Config%MakeSubbasinSummaries) THEN
            CALL SubOut%Clear()
            SubOut%Bsn     = Bsn
            SubOut%SubNum  = Sub
            SubOut%SubArea = SubData%SubArea
            CALL TDD%Clear()
            DO I = 1, 13
               OK = TDD%SetDataType(SubDataTypes(I));                 IF (.NOT. OK) GOTO 898
               OK = TDD%SetDataUnit(SubDataUnits(I));                 IF (.NOT. OK) GOTO 898
               OK = TDD%AssignData(SSeq, ESeq, LandSubSummary(I,:));  IF (.NOT. OK) GOTO 898
               OK = SubOut%AddDataset(TDD);                           IF (.NOT. OK) GOTO 898
            END DO
            WRITE(FName, 1101) TRIM(OtlkDir), Bsn, Sub, TRIM(SCode)
            CALL WriteFile_SubbasinSummary(TRIM(FName), SubOut);  IF (ErrorLevel .NE. 0) GOTO 899
         END IF
         
         !
         !  Add the data from this subbasin to the relevant accumulators.
         !  We don't accumulate everything.
         !
         CALL AddToAccum(AccTot( 1,:), AccArea( 1,:), LandSubSummary( 8,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! runoff as depth over land
         CALL AddToAccum(AccTot( 2,:), AccArea( 2,:), LandSubSummary( 1,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! TMin
         CALL AddToAccum(AccTot( 3,:), AccArea( 3,:), LandSubSummary( 2,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! TMax
         CALL AddToAccum(AccTot( 4,:), AccArea( 4,:), LandSubSummary( 4,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! TMean
         CALL AddToAccum(AccTot( 5,:), AccArea( 5,:), LandSubSummary( 5,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! DewP
         CALL AddToAccum(AccTot( 6,:), AccArea( 6,:), LandSubSummary( 6,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! Wind
         CALL AddToAccum(AccTot( 7,:), AccArea( 7,:), LandSubSummary( 7,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! Cloud
         CALL AddToAccum(AccTot( 8,:), AccArea( 8,:), LandSubSummary( 9,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! USZM
         CALL AddToAccum(AccTot( 9,:), AccArea( 9,:), LandSubSummary(10,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! LSZM
         CALL AddToAccum(AccTot(10,:), AccArea(10,:), LandSubSummary(11,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! GZM
         CALL AddToAccum(AccTot(11,:), AccArea(11,:), LandSubSummary(12,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! Surf
         CALL AddToAccum(AccTot(12,:), AccArea(12,:), LandSubSummary(13,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! SWE
         CALL AddToAccum(AccTot(13,:), AccArea(13,:), LandSubSummary( 3,:), Area, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899      ! precipitation depth over land
      END DO

      WRITE(StatusMsg, 2002) Bsn, TRIM(SCode);   CALL WriteMsg(.TRUE., .TRUE.)
      
      !
      !  Now compute the lumped values for the accumulated quantities
      !  These are all expressed as a lumped overLAND value.
      !  I = index into the accumulator arrays
      !  J = index into the final summary output array for lake/land values
      !
      DO I = 1, 13
         IF (I .EQ.  1) J =  5          ! runoff
         IF (I .EQ.  2) J = 13          ! TMin
         IF (I .EQ.  3) J = 14          ! TMax
         IF (I .EQ.  4) J = 15          ! TMean
         IF (I .EQ.  5) J = 16          ! Dewpt
         IF (I .EQ.  6) J = 17          ! Wind
         IF (I .EQ.  7) J = 18          ! Cloud
         IF (I .EQ.  8) J = 19          ! USZM
         IF (I .EQ.  9) J = 20          ! LSZM
         IF (I .EQ. 10) J = 21          ! GZM
         IF (I .EQ. 11) J = 22          ! Surf
         IF (I .EQ. 12) J = 23          ! SWE
         IF (I .EQ. 13) J =  6          ! Precip
         
         DO K = 1, NumDays
            IF (AccArea(I,K) .GT. 0) THEN
               LakeBasinSummary(J,K) = AccTot(I,K) / AccArea(I,K)
            END IF
         END DO
      END DO
      
      !
      !  Convert runoff as a depth over land (index 5) into a depth over the lake
      !  and assign it into the big array at index 1.
      !
      DO K = 1, NumDays
         IF (LakeBasinSummary(5,K) .GT. MissingData_Real_Test) THEN
            LakeBasinSummary(1,K) = LakeBasinSummary(5,K) * LandToLakeRatio
         END IF
      END DO
      
      !
      !  Read the historical daily data from each lake surface
      !
      Area = BData%SubbasinArea(0)
      LandSubSummary(:,:) = MissingData_Real
         
      !
      !  Subbasin meteorology for the lake surface.
      !  This is just like any of the land subbasins, but it gets 
      !  assigned into the LakeBasinSummary array.
      !
      WRITE(FName, 1002) TRIM(OtlkDir), Bsn, 0, TRIM(SCode)
      CALL ReadFile_OneSubbasin(FName, SubData);  IF (ErrorLevel .NE. 0) GOTO 899
      DO I = 1, 7
         IF (I .EQ. 1) J =  7    ! TMin
         IF (I .EQ. 2) J =  8    ! TMax
         IF (I .EQ. 3) J =  2    ! overlake precip
         IF (I .EQ. 4) J = -1    ! raw values of TMean, Dewpt, Wind, Cloud are not
         IF (I .EQ. 5) J = -1    ! used in the summary file. Instead, the adjusted
         IF (I .EQ. 6) J = -1    ! values output by LLTM are used in the
         IF (I .EQ. 7) J = -1    ! summary file
         IF (J .GT. 0) THEN
            DT = SubDataTypes(I)
            TDDP => SubData%GetPointerToDataOfType(DT)
            DD = TDDP%GetDataVals()
            OldUnits = TDDP%GetDataUnit()
            NewUnits = SubDataUnits(I)
            DO K = 1, NumDays
               LakeBasinSummary(J,K) = UnitConvertedDataValue(DD(K), OldUnits, NewUnits)
            END DO
         END IF
      END DO
         
      !
      !  LLTM results for the lake surface
      !
      WRITE(FName, 1004) TRIM(OtlkDir), Bsn, TRIM(SCode)
      CALL ReadFile_LLTM(FName, SubData);  IF (ErrorLevel .NE. 0) GOTO 899
      DO I = 1, 20
         IF (I .EQ.  1) J =  3    ! evap
         IF (I .EQ.  2) J = 24    ! Water Temp
         IF (I .EQ.  3) J = 27    ! ice temp
         IF (I .EQ.  4) J = 25    ! ice cover %
         IF (I .EQ.  5) J = 26    ! ice depth 
         IF (I .EQ.  6) J = 28    ! reflected
         IF (I .EQ.  7) J = 29    ! latent
         IF (I .EQ.  8) J = 30    ! sensible
         IF (I .EQ.  9) J = 31    ! advection
         IF (I .EQ. 10) J = 32    ! incident
         IF (I .EQ. 11) J = 33    ! net lw
         IF (I .EQ. 12) J = 34    ! total heat
         IF (I .EQ. 13) J = -1    ! Raw met values are
         IF (I .EQ. 14) J = -1    ! not used in
         IF (I .EQ. 15) J = -1    ! the summary file.
         IF (I .EQ. 16) J = -1    ! 
         IF (I .EQ. 17) J =  9    ! overlake TMean as adjusted by LLTM
         IF (I .EQ. 18) J = 10    ! overlake Dewpt as adjusted by LLTM
         IF (I .EQ. 19) J = 11    ! overlake Windspeed as adjusted by LLTM
         IF (I .EQ. 20) J = 12    ! overlake Cloud cover "as adjusted", which is no adjustment
         IF (J .GT. 0) THEN
            DT = LLTMDataTypes(I)
            TDDP => SubData%GetPointerToDataOfType(DT)
            DD = TDDP%GetDataVals()
            OldUnits = TDDP%GetDataUnit()
            NewUnits = LLTMDataUnits(I)
            DO K = 1, NumDays
               LakeBasinSummary(J,K) = UnitConvertedDataValue(DD(K), OldUnits, NewUnits)
            END DO
         END IF
      END DO

      !
      !  Net Basin Supply values
      !
      LakeBasinSummary(4,:) = MissingData_Real
      DO Seq = SSeq, ESeq
         I = Seq - SSeq + 1
         Run = LakeBasinSummary(1,I) 
         Prc = LakeBasinSummary(2,I) 
         Evp = LakeBasinSummary(3,I) 
         OK = .TRUE.
         IF (IsMissing(Run)) OK = .FALSE.
         IF (IsMissing(Prc)) OK = .FALSE.
         IF (IsMissing(Evp)) OK = .FALSE.
         IF (OK) THEN
            NBS_mm = Run + Prc - Evp
            LakeBasinSummary(4,I) = NBS_mm
            NBS_CMS(I) = (NBS_mm / 1000.0) * BData%LakeArea / 86400.0    ! mm -> cms
         END IF
      END DO

      !
      !  Build the LakeOut object to contain all of the lake/basin data and then output it to a file.
      !
      CALL LakeOut%Clear()
      LakeOut%Bsn = GetLowercase(Bsn)
      CALL TDD%Clear()
      DO I = 1, 34
         OK = TDD%SetDataType(LakeDataTypes(I));                  IF (.NOT. OK) GOTO 898
         OK = TDD%SetDataUnit(LakeDataUnits(I));                  IF (.NOT. OK) GOTO 898
         OK = TDD%AssignData(SSeq, ESeq, LakeBasinSummary(I,:));  IF (.NOT. OK) GOTO 898
         OK = LakeOut%AddDataset(TDD);                            IF (.NOT. OK) GOTO 898
      END DO
      WRITE(FName, 1102) TRIM(OtlkDir), Bsn, TRIM(SCode)
      CALL WriteFile_LakeSummary(TRIM(FName), LakeOut);  IF (ErrorLevel .NE. 0) GOTO 899
      
      StatusMsg = '';   CALL WriteMsg()
      
      !
      !  Write an output NBS file that is in the compatible with the historical CGLRRM
      !
      Description = 'Outlook NBS values based on meteorology from the ' // TRIM(SCode) //' scenario'
      WRITE(FName, 1105) TRIM(OtlkDir), LakeName2(LkNum), TRIM(SCode)
      CALL WriteFile_CGLRRM(TRIM(FName), Bsn, NBS_CMS, SSeq, ESeq, Description);  IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      ! go to final cleanup
      !
      GOTO 999
      
      !
      !  Error handling
      !
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] Summarize_Otlk...';   CALL PassMsg
      GOTO 999
  
      !
      !  Final cleanup
      !
  999 DEALLOCATE(LandSubSummary, LakeBasinSummary, AccTot, AccArea, NBS_CMS, STAT=IOS)
  
      !
      !  FORMATs
      !      
 1001 FORMAT(A, 'basininfo.txt')
 1002 FORMAT(A, 'otlkdata_', A3, I2.2, '_', A, '.csv') 
 1003 FORMAT(A, 'lbrm_output_', A3, I2.2, '_', A, '.csv') 
 1004 FORMAT(A, 'lltm_out_', A3, '_', A, '.csv')
      
 1101 FORMAT(A, 'summary_', A3, I2.2, '_', A, '.csv') 
 1102 FORMAT(A, 'summary_', A3, '_', A, '.csv') 
 1105 FORMAT(A, A2, 'dnbs_', A, '.txt')

 2001 FORMAT('Building summary file for ', A3, I2.2, ' (', A, ')          ')
 2002 FORMAT('Building summary file for ', A3, ' (', A, ') lake&land      ')
 
      END SUBROUTINE Summarize_Otlk

!------------------------------------------------------------------------------
      !--------------------------------------------------------------
      !  Combine the daily nbs files (CGLRRM format) from Michigan, Huron, and 
      !  Georgian Bay into a single file for the combined lake.
      !  Values are supplied as a rate (tens of CMS) so combining them is
      !  simple addition.
      !--------------------------------------------------------------
      SUBROUTINE Combine_MHG_For_CGLRRM(SCode)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: SCode
      
      INTEGER :: I, IOS, IM, IH, IG
      INTEGER :: Seq, SSeq, ESeq, NumDays
      INTEGER :: SSeqMic, ESeqMic, SSeqHur, ESeqHur, SSeqGeo, ESeqGeo
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=70)  :: Description
      CHARACTER(LEN=200) :: OtlkDir, FName

      REAL, DIMENSION(:),   ALLOCATABLE :: NbsMic, NbsHur, NbsGeo, NbsMHG
      

      !
      OtlkDir = TRIM(GLSHFS_Config%BaseDir) // TRIM(GLSHFS_Config%ForecastName) // FilePathSeparator
      
      !
      !  The allocatable arrays must be allocated before being passed to the subroutine.
      !  I don't know how big they need to be at this point in execution, so I will
      !  simply allocate them to size 1, then deallocate/allocate them to the correct size
      !  in the subroutine.
      !
      ALLOCATE(NbsMic(1), NbsHur(1), NbsGeo(1), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for the NBS values from the 3 lakes';  CALL PassMsg
         GOTO 898
      END IF

      !
      !  Read each of the files
      !
      WRITE(FName, 1105) TRIM(OtlkDir), 'mi', TRIM(SCode)
      CALL ReadFile_CGLRRM(FName, Bsn, NbsMic, SSeqMic, ESeqMic, Description)
      IF (ErrorLevel .NE. 0) GOTO 899
      
      WRITE(FName, 1105) TRIM(OtlkDir), 'hu', TRIM(SCode)
      CALL ReadFile_CGLRRM(FName, Bsn, NbsHur, SSeqHur, ESeqHur, Description)
      IF (ErrorLevel .NE. 0) GOTO 899
      
      WRITE(FName, 1105) TRIM(OtlkDir), 'gb', TRIM(SCode)
      CALL ReadFile_CGLRRM(FName, Bsn, NbsGeo, SSeqGeo, ESeqGeo, Description)
      IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Combine the data values
      !
      SSeq = MAX(SSeqMic, SSeqHur, SSeqGeo)
      ESeq = MIN(ESeqMic, ESeqHur, ESeqGeo)
      NumDays = ESeq - SSeq + 1
      ALLOCATE(NbsMHG(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for combined NBS values';  CALL PassMsg
         GOTO 898
      END IF

      DO Seq = SSeq, ESeq
         IM = Seq - SSeqMic + 1
         IH = Seq - SSeqHur + 1
         IG = Seq - SSeqGeo + 1
         I  = Seq - SSeq    + 1
         NbsMhg(I) = NbsMic(IM) + NbsHur(IH) + NbsGeo(IG)
         IF (NbsMic(IM) .LT. -99998) NbsMHG(I) = MissingData_Real
         IF (NbsHur(IH) .LT. -99998) NbsMHG(I) = MissingData_Real
         IF (NbsGeo(IG) .LT. -99998) NbsMHG(I) = MissingData_Real
      END DO      
      
      !
      !  Write an output NBS file that is in the format compatible with the historical CGLRRM
      !
      Description = 'Outlook NBS values based on meteorology from the ' // TRIM(SCode) //' scenario'
      WRITE(FName, 1105) TRIM(OtlkDir), 'mh', TRIM(SCode)
      Bsn = 'mhg'
      CALL WriteFile_CGLRRM(TRIM(FName), Bsn, NbsMHG, SSeq, ESeq, Description);  IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      ! go to final cleanup
      !
      GOTO 999
      
      !
      !  Error handling
      !
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] Combine_MHG_For_CGLRRM...';   CALL PassMsg
      GOTO 999
  
      !
      !  Final cleanup
      !
  999 DEALLOCATE(NbsMic, STAT=IOS)
      DEALLOCATE(NbsHur, STAT=IOS)
      DEALLOCATE(NbsGeo, STAT=IOS)
      DEALLOCATE(NbsMHG, STAT=IOS)
  
  
      !
      !  FORMATs
      !      
 1105 FORMAT(A, A2, 'dnbs_', A, '.txt')

      END SUBROUTINE Combine_MHG_For_CGLRRM

!-----------------------------------------------------------------------------------
      !-----------------------------------------------------------------------------
      !  This routine is a helper routine that accomplishes the computation of summing
      !  subbasin values (adjusted by an area ratio) into an areal-averaged value.
      !-----------------------------------------------------------------------------
      SUBROUTINE AddToAccum(TotVals, TotAreas, ThisVals, ThisArea, SSeq, ESeq)
      IMPLICIT NONE
      REAL, DIMENSION(:), INTENT(INOUT) :: TotVals, TotAreas
      REAL, DIMENSION(:), INTENT(IN)    :: ThisVals
      REAL,               INTENT(IN)    :: ThisArea
      INTEGER,            INTENT(IN)    :: SSeq, ESeq
      INTEGER :: I, Seq
      DO Seq = SSeq, ESeq
         I = Seq - SSeq + 1
         IF (ThisVals(I) .GT. MissingData_Real_Test) THEN
            TotVals(I)  = TotVals(I)  + (ThisVals(I)*ThisArea)
            TotAreas(I) = TotAreas(I) + ThisArea
         END IF
      END DO
      END SUBROUTINE AddToAccum

!-----------------------------------------------------------------------------------
      !------------------------------------------------------------------
      !  Delete the temporary forecast files
      !------------------------------------------------------------------
      SUBROUTINE ClearTempForecastFiles
      IMPLICIT NONE
      CHARACTER(LEN=200) :: OtlkDir, FName
      
      !
      OtlkDir = TRIM(GLSHFS_Config%BaseDir) // TRIM(GLSHFS_Config%ForecastName) // FilePathSeparator
      
      FName = TRIM(OtlkDir) // 'lbrm_bounds_*.csv'
      CALL DeleteFile(TRIM(FName))

      FName = TRIM(OtlkDir) // 'lbrm_output_*.csv'
      CALL DeleteFile(TRIM(FName))
      
      FName = TRIM(OtlkDir) // 'lbrm_parm_*.txt'
      CALL DeleteFile(TRIM(FName))
      
      FName = TRIM(OtlkDir) // 'lbrmdata_*.csv'
      CALL DeleteFile(TRIM(FName))
      
      FName = TRIM(OtlkDir) // 'lltm_met_*.csv'
      CALL DeleteFile(TRIM(FName))
      
      FName = TRIM(OtlkDir) // 'lltm_out_*.csv'
      CALL DeleteFile(TRIM(FName))
      
      FName = TRIM(OtlkDir) // 'otlkdata_*.csv'
      CALL DeleteFile(TRIM(FName))
      
      
      END SUBROUTINE ClearTempForecastFiles
            
      
      
      
END MODULE SummaryFiles