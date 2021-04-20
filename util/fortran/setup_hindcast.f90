!----------------------------------------------------------------

   MODULE SetupHC_Subs
      USE GL_Constants
      USE MyKinds
      USE ErrorProcess
      USE GLSHFS_Util

      
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
      !  BaseDir includes the trailing file path separator
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
      
      WRITE(S, 4001) (I, I=1950,SYr-5)
      I = INDEX(S, ',', .TRUE.)     !  find trailing comma
      S = S(1:I-1)                  !  chop it off
      
      WRITE(U1, 3024, ERR=813) TRIM(S)
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
 3012 FORMAT('LltmMethodRadiationFcst = 1')

 3013 FORMAT('LltmApplyDataCorrectionsHist = TRUE')
 3014 FORMAT('LltmApplyDataCorrectionsFcst = TRUE')

 3020 FORMAT('ForecastName = hindc_', I4.4, I2.2)
 3021 FORMAT('ForecastStart = ', I4.4, '-', I2.2, '-', I2.2)
 3022 FORMAT('ForecastLength  = 60')
 3023 FORMAT('ForecastMetSource = extractfromhist')
 3024 FORMAT('ScenarioNames = ', A)

 3030 FORMAT('MissingValueString = NA')
 
 4001 FORMAT(100(I4,','))

   
      END SUBROUTINE BuildConfigFile
   
   
   END MODULE SetupHC_Subs
   
!----------------------------------------------------------------------------   
!----------------------------------------------------------------------------   
!----------------------------------------------------------------------------   
   PROGRAM SetupHindcast
      USE SetupHC_Subs
      IMPLICIT NONE

      INTEGER :: SYr, SMn, ArgC, LBRM
      CHARACTER(LEN=99)  :: Args(10)
      CHARACTER(LEN=300) :: CLine, S
      CHARACTER(LEN=200) :: BaseDir
   
      !
      !  Process the command line to get the relevant info
      !
      CALL GET_COMMAND(CLine)
      IF (LEN_TRIM(CLine) .GT. 0) THEN
         CALL ParseCmdLine(CLine, Args, ArgC)
      END IF
      IF (ArgC .LT. 4) GOTO 855
      S = TRIM(Args(2))
      READ(S, 5001) SYr, SMn
      IF (SYr .LT. 1950) GOTO 850
      IF (SYr .GT. 2020) GOTO 850
      IF (SMn .LT.  1) GOTO 850
      IF (SMn .GT. 12) GOTO 850

      S = TRIM(Args(3))
      IF (GetLowercase(S) .EQ. 'old') THEN
         LBRM = 1982
      ELSE IF (GetLowercase(S) .EQ. 'new') THEN
         LBRM = 2016
      ELSE
         ErrorMessage = 'Bad scenario specified.  Must be old or new'; CALL PassMsg
      END IF

      BaseDir = NormalizedFilePath(TRIM(Args(4)))
      
      !
      !  Build the config file for this start date
      !
      CALL BuildConfigFile(SYr, SMn, TRIM(BaseDir), LBRM);  IF (ErrorLevel .NE. 0) GOTO 899
   
      CALL EXIT(0)   
      
      !
      !  Error handling
      !
 850  PRINT*, 'Invalid start date specified.'
      PRINT*, 'Year  = ', SYr
      PRINT*, 'Month = ', SMn
      
 855  PRINT*, 'USAGE: setuphindcast  YYYYMM  scenario  basedir'
      PRINT*, '  YYYYMM = forecast start date'
      PRINT*, '  scenario = "old"  or   "new"'
      PRINT*, '  basedir = full path name where sup, mic, hur, etc are located'
      
 899  PRINT*, 'Program ended in failure.'
      PRINT*, '[traceback] SetupHindcast main...'

      CALL EXIT(1)
   
 5001 FORMAT(I4, I2)
   
   
   
   END PROGRAM SetupHindcast
   