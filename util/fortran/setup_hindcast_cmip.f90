!----------------------------------------------------------------

   MODULE SetupHCC_Subs
      USE GL_Constants
      USE MyKinds
      USE ErrorProcess
      USE GLSHFS_Util
      USE GlerlDataTypesAndUnits
      USE GLSHFS_Files
      USE DailyDataStructures

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

   CONTAINS
   
!----------------------------------------------------------------------------   
      SUBROUTINE BuildConfigFile(SYr, SMn, BaseDir, LBRM)
      IMPLICIT NONE
      INTEGER,          INTENT(IN) :: SYr, SMn
      CHARACTER(LEN=*), INTENT(IN) :: BaseDir
      INTEGER,          INTENT(IN) :: LBRM       ! 1982 or 2016
   
      INTEGER :: U1, I, FD, D
      CHARACTER(LEN=200) :: FName, BDir, PDir, SDir
      CHARACTER(LEN=400) :: S
      
   
      !
      !  Lauren Fry needs the forecasts to start on the first Friday of the month.
      !  Calculate the correct calendar day for that.
      !
      D = DayOfTheWeek(1, SMn, SYr)    ! day of the week when day=1  (Sunday=1; Saturday=7)
      IF (D .LE. 6) THEN
         FD = 7 - D 
      ELSE
         FD = 14 - D
      END IF
      
      !
      !
      BDir = TRIM(BaseDir)
      PDir = '/home/hunter/timg/glshfs/sourcecode/main'
      SDir = TRIM(BaseDir) // 'stn'
      
      !
      WRITE(FName, 2001) SYr, SMn
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)
      
      WRITE(U1, 3001, ERR=813) SYr, SMn
      WRITE(U1, 3002, ERR=813)
      WRITE(U1, 3003, ERR=813)
      WRITE(U1, 3004, ERR=813)
      WRITE(U1, 3005, ERR=813)
      WRITE(U1, 3006, ERR=813) TRIM(BDir)
      WRITE(U1, 3007, ERR=813) TRIM(PDir)
      WRITE(U1, 3008, ERR=813) TRIM(SDir)
      WRITE(U1, 3009, ERR=813)
      WRITE(U1, 3010, ERR=813) LBRM
      WRITE(U1, 3011, ERR=813)
      WRITE(U1, 3012, ERR=813)
      WRITE(U1, 3013, ERR=813)
      WRITE(U1, 3014, ERR=813)
      
      WRITE(U1, 3020, ERR=813) SYr, SMn
      WRITE(U1, 3021, ERR=813) SYr, SMn, FD
      WRITE(U1, 3022, ERR=813)
      WRITE(U1, 3023, ERR=813)
      
      WRITE(S, 4001) (ModelName6(I), I=1,19)
      WRITE(U1, 3024, ERR=813) (TRIM(ModelName4(I)), I=1,19)
      WRITE(U1, 3030, ERR=813)
      
      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN
   
      !
      !
  811 ErrorMessage = 'Error opening output file '//TRIM(FName); CALL PassMsg; GOTO 898
  813 ErrorMessage = 'Error writing output file '//TRIM(FName); CALL PassMsg; GOTO 898

  898 ErrorLevel = 1  
  899 ErrorMessage = '[traceback] BuildConfigFile...';  CALL PassMsg
  
      IF (U1 .GT. 0) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
      RETURN
  
      !
      !  FORMATs
      !
 2001 FORMAT('hindcast_cfg_', I4, I2.2, '.txt')
   
 3001 FORMAT('cfgname = hindcast_', I4.4, I2.2)
 3002 FORMAT('AddStationData   = No')
 3003 FORMAT('BuildSubbasinMet = No')
 3004 FORMAT('UpdateHistorical = No')
 3005 FORMAT('RunForecasts     = Yes')

 3006 FORMAT('basedir = ', A)
 3007 FORMAT('prgdir  = ', A)
 3008 FORMAT('stndir  = ', A)
 3009 FORMAT('BasinBuffer = 50')

 3010 FORMAT('LbrmMethodET = ', I0)
 3011 FORMAT('LltmMethodRadiationHist = 1')
 3012 FORMAT('LltmMethodRadiationFcst = 2')

 3013 FORMAT('LltmApplyDataCorrectionsHist = TRUE')
 3014 FORMAT('LltmApplyDataCorrectionsFcst = FALSE')

 3020 FORMAT('ForecastName = hindc_', I4.4, I2.2)
 3021 FORMAT('ForecastStart = ', I4.4, '-', I2.2, '-', I2.2)
 3022 FORMAT('ForecastLength  = 60')
 3023 FORMAT('ForecastMetSource = usersupplied')
 3024 FORMAT('ScenarioNames = ', 18(A,','), A)

 3030 FORMAT('MissingValueString = NA')
 
 4001 FORMAT(18(A6,','), A6)

   
      END SUBROUTINE BuildConfigFile
   
!----------------------------------------------------------------------------   
      SUBROUTINE BuildDataFiles(SYr, SMn, BaseDir)
      IMPLICIT NONE
      INTEGER,          INTENT(IN) :: SYr, SMn
      CHARACTER(LEN=*), INTENT(IN) :: BaseDir
   
      INTEGER :: D, FD, EMn, EYr, SDate, EDate, Lk, Sub, CS
      LOGICAL :: OK
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=6)   :: SCode
      CHARACTER(LEN=200) :: SrcDir, DstDir, WDir, SrcFile, DstFile
      
!      TYPE (TDlyData)            :: TDD
      TYPE (TDlyData), POINTER   :: TDDP
      TYPE (TDlyDataForSubbasin) :: TDin, TDout
      
      !
!      TDD   = TDlyData()
      TDin  = TDlyDataForSubbasin()      
      TDout = TDlyDataForSubbasin()      

      !
      !  Lauren Fry needs the forecasts to start on the first Friday of the month.
      !  Calculate the correct calendar day for that.
      !
      D = DayOfTheWeek(1, SMn, SYr)    ! day of the week when day=1  (Sunday=1; Saturday=7)
      IF (D .LE. 6) THEN
         FD = 7 - D 
      ELSE
         FD = 14 - D
      END IF
      
      !
      !  Calculate forecast dates
      !
      CALL DateSequence(FD, SMn, SYr, SDate);    IF (ErrorLevel .NE. 0) GOTO 899
      IF (FD .EQ. 1) THEN
         EMn = SMn + 60 - 1
      ELSE
         EMn = SMn + 60
      END IF
      EYr = SYr
      DO WHILE (EMn .GT. 12)
         EYr = EYr + 1
         EMn = EMn - 12
      END DO
      D = DaysInMonth(EMn, EYr)
      CALL DateSequence(D, EMn, EYr, EDate);    IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Assume:
      !    1) BaseDir is the working directory with subdirectories for each lake
      !    2) Source CMIP data files are in a subdirectory named ../cmip_data
      !
      !  Generate outlook scenario met files for the forecast period from the
      !  data files found there.
      !
      DO CS = 1, 19
         SCode = ModelName6(CS)
         SrcDir = '../cmip_data' // FilePathSeparator // TRIM(ModelName15(CS)) // FilePathSeparator
         WRITE(WDir, 1001) SYr, SMn
         DstDir = TRIM(BaseDir) // FilePathSeparator // TRIM(WDir) // FilePathSeparator
      
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

!      CALL TDD%Clear()
      CALL TDin%Clear()
      CALL TDout%Clear()
      
      RETURN
   
      !
      !

  898 ErrorLevel = 1  
  899 ErrorMessage = '[traceback] BuildDataFiles...';  CALL PassMsg

!      CALL TDD%Clear()
      CALL TDin%Clear()
      CALL TDout%Clear()
  
      RETURN
  
      !
      !  FORMATs
      !
 1001 FORMAT('hindc_', I4.4, I2.2)
 1002 FORMAT(A, 'otlkdata_', A3, I2.2, '_', A, '.csv')      

   
      END SUBROUTINE BuildDataFiles
   
   
   END MODULE SetupHCC_Subs
   
!----------------------------------------------------------------------------   
!----------------------------------------------------------------------------   
!----------------------------------------------------------------------------   
   PROGRAM SetupHindcastCmip
      USE SetupHCC_Subs
      IMPLICIT NONE

      INTEGER :: SYr, SMn, ArgC, LBRM
      CHARACTER(LEN=200) :: Args(10)
      CHARACTER(LEN=200) :: CLine, S
      CHARACTER(LEN=200) :: BaseDir
   
      !
      !  Process the command line to get the relevant info
      !
      CALL GET_COMMAND(CLine)
      IF (LEN_TRIM(CLine) .GT. 0) THEN
         CALL ParseCmdLine(CLine, Args, ArgC)
      END IF
      IF (ArgC .LT. 3) GOTO 855
      S = TRIM(Args(2))
      READ(S, 5001) SYr, SMn
      IF (SYr .LT. 1950) GOTO 850
      IF (SYr .GT. 2030) GOTO 850
      IF (SMn .LT.  1) GOTO 850
      IF (SMn .GT. 12) GOTO 850

      !
      !  BaseDir needs to have the trailing slash
      !
      BaseDir = NormalizedFilePath(TRIM(Args(3)))
      
      !
      !  Build the files for this start date
      !
      LBRM = 2016
      CALL BuildConfigFile(SYr, SMn, TRIM(BaseDir), LBRM);  IF (ErrorLevel .NE. 0) GOTO 899
      CALL BuildDataFiles(SYr, SMn, TRIM(BaseDir));  IF (ErrorLevel .NE. 0) GOTO 899
   
      CALL EXIT(0)   
      
      !
      !  Error handling
      !
 850  PRINT*, 'Invalid start date specified.'
      PRINT*, 'Year  = ', SYr
      PRINT*, 'Month = ', SMn
      
 855  PRINT*, 'USAGE: setup_hindcast_cmip  YYYYMM'
      PRINT*, '  YYYYMM = forecast start date'
      
 899  PRINT*, 'Program ended in failure.'
      PRINT*, '[traceback] SetupHindcastCmip main...'

      CALL EXIT(1)
   
 5001 FORMAT(I4, I2)
   
   
   
   END PROGRAM SetupHindcastCmip
   