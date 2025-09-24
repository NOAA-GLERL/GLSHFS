      PROGRAM GLSHFS
      USE ErrorProcess
      USE StatusMessages
      USE GLSHFS_Util
      USE GLSHFS_Global

      USE GlerlDataTypesAndUnits
      USE DailyDataStructures
      USE DailyDataCollections
      
      USE GLSHFS_ConfigFile
      USE GLSHFS_Files
      USE GlerlMaps
      USE BasinInfo
      USE Subbasin_Met
      USE StnData_RangeCheck
      USE StationData_Aggregation
      USE BuildSubbasinMet
      USE Outlook_Met
      USE SummaryFiles
      
      USE GLSHFS_LBRM
      USE GLSHFS_LLTM
      USE LBRM_Main
      
      IMPLICIT NONE
      
      INTEGER :: I, IOS, ArgCount, TimeInSecs
      LOGICAL :: OK, FExist
      CHARACTER(LEN=25)  :: HMD
      CHARACTER(LEN=150) :: S, ArgStrings(5)
      CHARACTER(LEN=200) :: CmdLine, CurDir, FName, OtlkDir, FileSpec, TDir
      INTEGER, DIMENSION(8) :: RunStart, RunFinish, RunTime1, RunTime2

      ErrorLevel = 0

      CALL DATE_AND_TIME(values=RunStart)
      
      !
      !  Get the current directory and make sure that the string contains 
      !  the trailing path separation character. Subsequent usage always 
      !  assumes that it does.
      !
      CALL GetTheCurrentDirectory(CurDir)         ! in the utils module
      I = LEN_TRIM(CurDir)
      IF (CurDir(I:I) .NE. FilePathSeparator) CurDir = TRIM(CurDir) // FilePathSeparator
      
      !
      !  Create the name of the temporary directory that we will use.
      !  See comments in glshfs_global.f90 for more explanation.
      !
      !  If we are running in the linux environment we will always(?) have a defined
      !  environment variable called HOME. If HOME is a defined environment variable,
      !  then we will use that as the parent directory.
      !
      !  If we are running in the Windows environment we will always(?) have a defined
      !  environment variable called TEMP. If TEMP is a defined environment variable,
      !  then we will use that as the parent directory.
      !
      !  If neither of those options worked, just use the current directory as the parent.
      !  Not the ideal solution, but I don't have a better one right now.
      !
      CALL GetEnvironmentVariableValue('HOME', TDir)
      IF (LEN_TRIM(TDir) .LT. 1) CALL GetEnvironmentVariableValue('TEMP', TDir)
      IF (LEN_TRIM(TDir) .LT. 1) TDir = TRIM(CurDir)
      GlshfsTempDir = TRIM(TDir) // FilePathSeparator // 'glshfs_tmp' // FilePathSeparator
      
      !
      !  Create the temporary directory
      !
      FileSpec = TRIM(GlshfsTempDir) // '.'
      INQUIRE(FILE=TRIM(FileSpec), EXIST=FExist)
      IF (.NOT. FExist) CALL CreateDirectory(GlshfsTempDir)
      
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
      !  Process station data to produce subbasin (and overlake) meteorology
      !
      CALL DATE_AND_TIME(values=RunTime1)
      IF (GLSHFS_Config%AddStationData .OR. GLSHFS_Config%BuildSubbasinMet) THEN
         CALL BuildSubbasinMeteorology();  IF (ErrorLevel .NE. 0) GOTO 899
      END IF

      !
      !  Status update on screen
      !
      CALL DATE_AND_TIME(values=RunTime2)
      CALL GetElapsedSeconds(RunTime1, RunTime2, TimeInSecs);   IF (ErrorLevel .NE. 0) GOTO 899
      HMD = GetStringHMS(TimeInSecs);                           IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(StatusMsg, 3001) TRIM(HMD);   CALL WriteMsg()
      
      !
      !  Update the LBRM and LLTM output files to incorporate the new 
      !  subbasin meteorology data
      !
      IF (GLSHFS_Config%UpdateHistorical) THEN
         CALL DATE_AND_TIME(values=RunTime1)
         CALL UpdateLBRM_Simulation();  IF (ErrorLevel .NE. 0) GOTO 899
         CALL UpdateLLTM_Simulation();  IF (ErrorLevel .NE. 0) GOTO 899
         CALL MakeSummaryFiles_Hist();  IF (ErrorLevel .NE. 0) GOTO 899
         !
         !  Status update on screen
         !
         CALL DATE_AND_TIME(values=RunTime2)
         CALL GetElapsedSeconds(RunTime1, RunTime2, TimeInSecs);   IF (ErrorLevel .NE. 0) GOTO 899
         HMD = GetStringHMS(TimeInSecs);                           IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(StatusMsg, 3002) TRIM(HMD);   CALL WriteMsg()
      END IF
      StatusMsg = 'Historical update completed';  CALL WriteMsg()

      
      !
      !  Do forecast stuff, if appropriate
      !      
      IF (GLSHFS_Config%DoForecasts) THEN
         CALL DATE_AND_TIME(values=RunTime1)
         !
         !  Create the directory for this forecast (if required), then 
         !  clear everything out from it.
         !
         WRITE(OtlkDir, 1001) TRIM(GLSHFS_Config%BaseDir), TRIM(GLSHFS_Config%ForecastName)
         ErrorMessage = 'Creating and/or clearing folder for forecast files';  CALL PassMsg
         ErrorMessage = 'Folder name is '//TRIM(OtlkDir);                      CALL PassMsg
         FileSpec = TRIM(OtlkDir) // '.'
         INQUIRE(FILE=TRIM(FileSpec), EXIST=FExist)
         IF (.NOT. FExist) CALL CreateDirectory(OtlkDir)
         FileSpec = TRIM(OtlkDir) // FilePathSeparator // '*.*'
         CALL DeleteFile_SYSTEM(FileSpec)
      
         !
         !  Build the outlook meteorology files
         !
         CALL Build_Outlook_Meteorology();   IF (ErrorLevel .NE. 0) GOTO 899
         
         !
         !  Run the models for the forecast period
         !
         CALL RunLbrm_Forecasts();     IF (ErrorLevel .NE. 0) GOTO 899
         CALL RunLltm_Forecasts();     IF (ErrorLevel .NE. 0) GOTO 899

         !
         !  Make summary files.  lakewide/basinwide files will always be created.
         !  Subbasin summary files will depend on the user config setting.
         !
         CALL MakeSummaryFiles_Otlk();  IF (ErrorLevel .NE. 0) GOTO 899
         
         !
         !  Clean up temporary/intermediate files, if user-specified to do so
         !
         IF (GLSHFS_Config%ClearTemporaryForecastFiles) THEN
            CALL ClearTempForecastFiles();  IF (ErrorLevel .NE. 0) GOTO 899
         END IF

         !
         !  Status update on screen
         !
         CALL DATE_AND_TIME(values=RunTime2)
         CALL GetElapsedSeconds(RunTime1, RunTime2, TimeInSecs);   IF (ErrorLevel .NE. 0) GOTO 899
         HMD = GetStringHMS(TimeInSecs);                           IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(StatusMsg, 3003) TRIM(HMD);   CALL WriteMsg()
      END IF

      !
      !  Finished;  Successfully!
      !
      CALL DATE_AND_TIME(values=RunFinish)
      CALL GetElapsedSeconds(RunStart, RunFinish, TimeInSecs);   IF (ErrorLevel .NE. 0) GOTO 899
      HMD = GetStringHMS(TimeInSecs);                            IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(StatusMsg, 3004) TRIM(HMD);   CALL WriteMsg()
      
      GOTO 999
      
      !
      !  Error handling
      !
      
  850 ErrorMessage = 'USAGE: glshfs configfilename'; CALL PassMsg;  GOTO 898
      
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] GLSHFS Main...'; CALL PassMsg

      !
      !  Final cleanup and status message
      !
  999 DEALLOCATE(GLSHFS_Config%ScenarioNames, STAT=IOS)
      ErrorMessage = 'GLSHFS finished'; CALL PassMsg
      CALL EXIT(ErrorLevel)

 1001 FORMAT(A, A)
 3001 FORMAT('Station data processing elapsed time (H:M:S) = ', A)
 3002 FORMAT('Historical update processing elapsed time (H:M:S) = ', A)
 3003 FORMAT('Outlook processing elapsed time (H:M:S) = ', A)
 3004 FORMAT('Total elapsed runtime (H:M:S) = ', A)
      END PROGRAM GLSHFS
      
      
      
