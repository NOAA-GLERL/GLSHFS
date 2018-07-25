!----------------------------------------------------------------

   MODULE Setup_FC
   
      CHARACTER(LEN=15), DIMENSION(19), PARAMETER :: ModelName15 =    &
       (/  'bcc-csm1-1     ', 'CanESM2        ', 'CCSM4          ',   &
           'CNRM-CM5       ', 'CSIRO-Mk3-6-0  ', 'GFDL-CM3       ',   &
           'GFDL-ESM2G     ', 'GFDL-ESM2M     ', 'GISS-E2-R      ',   &
           'HadGEM2-CC     ', 'HadGEM2-ES     ', 'inmcm4         ',   &
           'IPSL-CM5A-LR   ', 'IPSL-CM5A-MR   ', 'MIROC5         ',   &
           'MIROC-ESM      ', 'MPI-ESM-MR     ', 'MRI-CGCM3      ',   &
           'NorESM1-M      '  /)

      !
      !  This set of 6-character codes was used when I generated the 
      !  source data sets in late July, 2017.  However, upon further 
      !  reflection, I realized that I don't like the fact that these
      !  don't sort (alphabetically) to the same order as the 15-character
      !  names. I made the 4-character ones sort correctly, but I want
      !  the 6-character ones to work that way, too. So I have defined 
      !  set B to do that. However... In order to run these hindcasts
      !  right away, I am going to use the old 6-character names (all 
      !  the source files were already named with them). For future
      !  stuff, I want to switch to set B.
      !  
      CHARACTER(LEN=6), DIMENSION(19), PARAMETER :: ModelName6  =  &
       (/  'bcccsm', 'canesm', 'ccsm4 ', 'cnrmc5', 'csiro6',       &
           'gfdlc3', 'esm2g ', 'esm2m ', 'gisse2', 'gem2cc',       &
           'gem2es', 'inmcm4', 'cm5alr', 'cm5amr', 'miroc5',       &
           'miresm', 'mpiesm', 'cgcm3 ', 'noresm'    /)

!      CHARACTER(LEN=6), DIMENSION(19), PARAMETER :: ModelName6b =  &
!       (/  'bcccsm', 'canesm', 'ccsm4x', 'cnrmc5', 'csiro6',       &
!           'gfdlc3', 'gesm2g', 'gesm2m', 'gisse2', 'hgm2cc',       &
!           'hgm2es', 'inmcm4', 'ic5alr', 'ic5amr', 'miroc5',       &
!           'miroce', 'mpiesm', 'mricg3', 'noresm'    /)

      CHARACTER(LEN=4), DIMENSION(19), PARAMETER :: ModelName4  =  &
       (/  'bccc', 'caes', 'ccs4', 'cnr5', 'csir',                 &
           'gcm3', 'ge2g', 'ge2m', 'gie2', 'hgcc',                 &
           'hges', 'inm4', 'iplr', 'ipmr', 'mir5',                 &
           'mire', 'mpim', 'mric', 'ne1m'    /)

   END MODULE Setup_FC
   
!----------------------------------------------------------------------------   
!----------------------------------------------------------------------------   
!----------------------------------------------------------------------------   
   PROGRAM SetupForecastCmip
      USE Setup_FC
      USE GL_Constants
      USE GLSHFS_Global
      USE GLSHFS_Util
      USE MyKinds
      USE ErrorProcess
      USE GlerlDataTypesAndUnits
      
      USE DailyDataStructures
      USE GLSHFS_ConfigFile
      USE GLSHFS_Files
      
      IMPLICIT NONE

      INTEGER :: I, ArgCount, CS, Lk, Sub
      INTEGER :: SDate, EDate, AtEndSeq, DataEndSeq
      INTEGER :: Dy, Mn, Yr
      LOGICAL :: OK
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=6)   :: SCode
      CHARACTER(LEN=200) :: CmdLine, ArgStrings(5)
      CHARACTER(LEN=200) :: S, FName
      CHARACTER(LEN=200) :: LakeDir, SrcDir, DstDir, SrcFile, DstFile
   
      TYPE (THeaderInfoType)     :: HdrInfo
      TYPE (TDlyData), POINTER   :: TDDP
      TYPE (TDlyDataForSubbasin) :: TDin, TDout

      
      TDin  = TDlyDataForSubbasin()      
      TDout = TDlyDataForSubbasin()      

      !
      !  Process the command line.
      !  User should have specified the controlling configuration file.
      !  If not, then we cannot proceed.
      !  Remember that CmdLine includes the program name, so the config file
      !  name should be entry 2.
      !
      CALL GET_COMMAND(CmdLine)
      CALL ParseCmdLine(CmdLine, ArgStrings, ArgCount); IF (ErrorLevel .NE. 0) GOTO 899
      DO I = 2, ArgCount
         S = GetLowercase(ArgStrings(I))
         IF (TRIM(S) .EQ. '--help') GOTO 850
         IF (TRIM(S) .EQ. '-help')  GOTO 850
         IF (TRIM(S) .EQ. '/?')     GOTO 850
         IF (TRIM(S) .EQ. '?')      GOTO 850
      END DO
      IF (ArgCount .EQ. 1) THEN
         ErrorMessage = 'No configuration file specified.'; CALL PassMsg
         GOTO 850
      END IF
      IF (ArgCount .GT. 2) THEN
         ErrorMessage = 'Too many entries on command line.'; CALL PassMsg
         GOTO 850
      END IF
      
      FName = ArgStrings(2)
      INQUIRE(FILE=TRIM(FName), EXIST=OK)
      IF (.NOT. OK) THEN
         ErrorMessage = 'Unable to find the specified configuration file ['//TRIM(FName)//']'; CALL PassMsg
         GOTO 850
      END IF
      
      !
      !  Read the configuration file
      !
      CALL Read_ConfigFile(TRIM(FName), GLSHFS_Config);  IF (ErrorLevel .NE. 0) GOTO 899

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
      SDate = GLSHFS_Config%ForecastStartSeq
      CALL DateSequence(8, 8, 8888, AtEndSeq)
      IF (SDate .EQ. AtEndSeq) THEN
         DataEndSeq = MissingData_Date
         DO Lk = 1, 7
            Bsn = TRIM(LakeName3(Lk))
            WRITE(LakeDir, 1001) TRIM(GLSHFS_Config%BaseDir), Bsn, FilePathSeparator
            WRITE(FName, 1002) TRIM(LakeDir), Bsn, 0
            CALL ReadJustHeaderInfo(FName, HFT_SingleSubbasin, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
            IF (HdrInfo%EDate .EQ. MissingData_Date) GOTO 701
            IF (DataEndSeq    .EQ. MissingData_Date) DataEndSeq = HdrInfo%EDate
            IF (HdrInfo%EDate .LT.       DataEndSeq) DataEndSeq = HdrInfo%EDate
         END DO
         IF (DataEndSeq .EQ. MissingData_Date) GOTO 702
         SDate = DataEndSeq + 1
      END IF
      IF (SDate .EQ. MissingData_Date) GOTO 703
      
      !
      !  Now we have the actual forecast start date.
      !  Compute the end date.
      !
      CALL SequenceDate(Dy, Mn, Yr, SDate); IF (ErrorLevel .NE. 0) GOTO 899
      Mn = Mn + GLSHFS_Config%ForecastLen
      IF (Dy .EQ. 1) Mn = Mn - 1
      DO WHILE (Mn .GT. 12)
         Mn = Mn - 12
         Yr = Yr + 1
      END DO
      Dy = DaysInMonth(Mn, Yr)
      CALL DateSequence(Dy, Mn, Yr, EDate); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Assume:
      !    1) BaseDir is the working directory with subdirectories for each lake
      !    2) Source CMIP data files are in a subdirectory of BaseDir named "cmip_input"
      !
      !  Generate outlook scenario met files for the forecast period from the
      !  data files found there.
      !
      DO CS = 1, 19
         SCode = ModelName6(CS)
         SrcDir = TRIM(GLSHFS_Config%BaseDir) // 'cmip_input' // FilePathSeparator // TRIM(ModelName15(CS)) // FilePathSeparator
         DstDir = TRIM(GLSHFS_Config%BaseDir) // 'cmip_fcst' // FilePathSeparator
      
         DO Lk = 1, 7
            Bsn = LakeName3(Lk)
            DO Sub = 0, NumSubbasins(Lk)
               CALL TDin%Clear()
               CALL TDOut%Clear()
               WRITE(SrcFile, 1002) TRIM(SrcDir), Bsn, Sub, TRIM(ModelName6(CS))
               WRITE(DstFile, 1002) TRIM(DstDir), Bsn, Sub, TRIM(ModelName4(CS))
               CALL ReadFile_OneSubbasin(SrcFile, TDin);         IF (ErrorLevel .NE. 0) GOTO 899
               TDout%Bsn     = TDin%Bsn
               TDout%SubNum  = TDin%SubNum
               TDout%SubArea = TDin%SubArea
               TDDP => TDin%GetPointerToDataOfType(GDT_AirTempMin)
               CALL TDDP%AdjustPeriod(SDate, EDate);             IF (ErrorLevel .NE. 0) GOTO 899
               OK = TDout%AddDataset(TDDP);                      IF (.NOT. OK)          GOTO 899
               TDDP => TDin%GetPointerToDataOfType(GDT_AirTempMax)
               CALL TDDP%AdjustPeriod(SDate, EDate);             IF (ErrorLevel .NE. 0) GOTO 899
               OK = TDout%AddDataset(TDDP);                      IF (.NOT. OK)          GOTO 899
               TDDP => TDin%GetPointerToDataOfType(GDT_Precipitation)
               CALL TDDP%AdjustPeriod(SDate, EDate);             IF (ErrorLevel .NE. 0) GOTO 899
               OK = TDout%AddDataset(TDDP);                      IF (.NOT. OK)          GOTO 899
               TDDP => TDin%GetPointerToDataOfType(GDT_AirTempMean)
               CALL TDDP%AdjustPeriod(SDate, EDate);             IF (ErrorLevel .NE. 0) GOTO 899
               OK = TDout%AddDataset(TDDP);                      IF (.NOT. OK)          GOTO 899
               TDDP => TDin%GetPointerToDataOfType(GDT_DewpointMean)
               CALL TDDP%AdjustPeriod(SDate, EDate);             IF (ErrorLevel .NE. 0) GOTO 899
               OK = TDout%AddDataset(TDDP);                      IF (.NOT. OK)          GOTO 899
               TDDP => TDin%GetPointerToDataOfType(GDT_WindSpeed)
               CALL TDDP%AdjustPeriod(SDate, EDate);             IF (ErrorLevel .NE. 0) GOTO 899
               OK = TDout%AddDataset(TDDP);                      IF (.NOT. OK)          GOTO 899
               TDDP => TDin%GetPointerToDataOfType(GDT_CloudCover)
               CALL TDDP%AdjustPeriod(SDate, EDate);             IF (ErrorLevel .NE. 0) GOTO 899
               OK = TDout%AddDataset(TDDP);                      IF (.NOT. OK)          GOTO 899
               TDDP => TDin%GetPointerToDataOfType(GDT_NetLongWaveRad)
               CALL TDDP%AdjustPeriod(SDate, EDate);             IF (ErrorLevel .NE. 0) GOTO 899
               OK = TDout%AddDataset(TDDP);                      IF (.NOT. OK)          GOTO 899
               TDDP => TDin%GetPointerToDataOfType(GDT_IncidentRad)
               CALL TDDP%AdjustPeriod(SDate, EDate);             IF (ErrorLevel .NE. 0) GOTO 899
               OK = TDout%AddDataset(TDDP);                      IF (.NOT. OK)          GOTO 899
               CALL WriteFile_OneSubbasin(DstFile, TDout);       IF (ErrorLevel .NE. 0) GOTO 899
            END DO
         END DO
      END DO
      
      CALL EXIT(0)   
      
      !
      !  Error handling
      !
 701  ErrorMessage = 'Unable to get date extent from file '  // TRIM(FName);              CALL PassMsg;  GOTO 898
 702  ErrorMessage = 'Unable to determine historical data period of record.';             CALL PassMsg;  GOTO 898
 703  ErrorMessage = 'Invalid forecast start date. Unable to build forecast scenarios.';  CALL PassMsg;  GOTO 898
 
 850  ErrorMessage = 'USAGE: setup_forecast_cmip config_file';  CALL PassMsg
      
 898  ErrorLevel = 1
 899  ErrorMessage = 'Program ended in failure.';               CALL PassMsg
      ErrorMessage = '[traceback] SetupHindcastCmip main...';   CALL PassMsg
      CALL EXIT(1)
   
 1001 FORMAT(A, A, A1)
 1002 FORMAT(A, 'otlkdata_', A3, I2.2, '_', A, '.csv')      
   
   END PROGRAM SetupForecastCmip
   