! ******************************************************************************
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
! ******************************************************************************
MODULE StnData_RangeCheck
      USE ErrorProcess
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GlerlDataTypesAndUnits
      USE DailyDataStructures
      USE GLSHFS_Files

      !
      !  By default, hide everything. Things that need to be public outside
      !  the module will be explicitly declared PUBLIC.
      !
      PRIVATE
      PUBLIC :: DoStnDataRangeChecking

      !
      !  Allowable range of provisional met data.
      !  I am setting these values to make sense for our current operational forecast
      !  usage (daily forecasts and hindcasts for the NYPA-OPG project).
      !
      !  These may need to be changed if the program is used for other 
      !  purposes (e.g. processing historical data from the 1800s)
      !      
      INTEGER, PARAMETER :: YearRange_Min = 1940
      INTEGER, PARAMETER :: YearRange_Max = 2030

      !
      !  Allowable region for station data.
      !  This is set to only allow GL region data. If using this program for some other
      !  area, these will need to be changed.
      !
      !  Note that longitude values are expressed as degrees EAST of the Prime Meridian.
      !  Thus, the G.L. region of US and Canada is negative values.
      !
      REAL, PARAMETER :: LatitudeRange_Min  = 35.0
      REAL, PARAMETER :: LatitudeRange_Max  = 60.0
      REAL, PARAMETER :: LongitudeRange_Min = -95.0
      REAL, PARAMETER :: LongitudeRange_Max = -65.0

      !
      !  Variables to track errors and warnings for the current station only.
      !
      INTEGER :: Station_Fatal, Station_Warnings, Station_Removed 
      
      !
      !  Variables for the log file
      !
      INTEGER :: ULog
      CHARACTER(LEN=200) :: LogFilename
      
      
CONTAINS

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
      SUBROUTINE DoStnDataRangeChecking(WorkingDir)
      IMPLICIT  NONE
      CHARACTER(LEN=*), INTENT(IN) :: WorkingDir     ! includes trailing filepath separator

      INTEGER :: I, U1, U2, U3, IOS
      INTEGER :: StationsToProcess, GoodStations, BadStations, StNum
      INTEGER :: Total_Fatal, Total_Warnings, Total_Removed
      LOGICAL :: IsOK
      CHARACTER (LEN=10)  :: DateStr, Time, Zone
      CHARACTER (LEN=200) :: File1, File2, File3, SFName, TempFile
      CHARACTER (LEN=200) :: EMsg, FName, FSpec, FN, FP
      INTEGER, DIMENSION(8) :: DTV
      CHARACTER(LEN=200), DIMENSION(MaxNumberOfStations) :: StationFileNames
      TYPE (TDlyDataMetStn) :: StnDataIn, StnDataOut

      !
      !  Initialize some variables to avoid compiler tool "choking"
      !
      U1=0; U2=0; U3=0
      StnDataIn  = TDlyDataMetStn()     ! instantiate the object that will be used for each station
      StnDataOut = TDlyDataMetStn()
      
      !
      !  Build the filename for a log file that will list the errors 
      !  and warnings for problems found in the station data.
      !  Filename will be based on the current timestamp.
      !  StationErrorLog-yyyymmdd-hhmm.txt
      !
      !  Unit number variable (ULog) is a module global variable in the 
      !  subroutines module so that the subroutines can write to the log file.
      !
      CALL DATE_AND_TIME(DateStr, Time, Zone, DTV); IF (ErrorLevel .NE. 0) GOTO 899
      IsOK = .TRUE.
      IF (DTV(1) .LE. 0) IsOK = .FALSE.      ! year
      IF (DTV(2) .LE. 0) IsOK = .FALSE.      ! month
      IF (DTV(3) .LE. 0) IsOK = .FALSE.      ! day
      IF (DTV(5) .LE. 0) IsOK = .FALSE.      ! hour
      IF (DTV(6) .LE. 0) IsOK = .FALSE.      ! minute
      IF (IsOK) THEN
         WRITE(LogFilename, 9001) (DTV(I), I=1,3), (DTV(I), I=5,6)
      ELSE
         LogFilename = 'stn_err_log.txt'
      END IF
      LogFilename = TRIM(WorkingDir) // TRIM(LogFilename)
      
      ULog = GetFreeUnitNumber()
      OPEN(UNIT=ULog, FILE=TRIM(LogFilename), STATUS='REPLACE', ERR=801)
      CALL FileWasOpened(ULog); IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(ULog, 9002, ERR=803) DTV(1), MonCode3(DTV(2)), DTV(3), (DTV(I), I=5,6)

      !
      !  Initialize variables used for tracking total number of warnings/errors.
      !
      Total_Fatal    = 0           ! Fatal errors
      Total_Warnings = 0           ! Warnings
      Total_Removed  = 0           ! Values removed (changed to "missing")
  
      !
      !  Build a list of the station files to process
      !  Use mode 1 (bare filename listing)
      !
      FName = TRIM(WorkingDir) // 'station_file_list.txt'
      FSpec = TRIM(WorkingDir) // 'met_*.csv'
      
      CALL BuildDirFileList(TRIM(FName), TRIM(FSpec), 1); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Read the station file names.
      !
      StationsToProcess = 0
      StationFileNames(:) = ''
      File1 = TRIM(WorkingDir) // 'station_file_list.txt'
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(File1), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      READ(U1, 1101, IOSTAT=IOS) SFName
      DO WHILE (IOS .EQ. 0)
        StationsToProcess = StationsToProcess + 1
        IF (StationsToProcess .GT. MaxNumberOfStations) THEN
           WRITE(ErrorMessage, 1301) MaxNumberOfStations; CALL PassMsg
           GOTO 899
        END IF
        StationFileNames(StationsToProcess) = TRIM(SFName)
        READ(U1, 1101, IOSTAT=IOS) SFName
      END DO
      CLOSE(U1, STATUS='DELETE')
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  For user-friendliness, sort the list into alphabetical order
      !
      CALL SortStationFileList(StationFileNames); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Open a new list file. This one will contain the filenames of only
      !  those stations that had zero fatal errors.
      !
      File2 = TRIM(WorkingDir) // 'station_file_list_good.txt'
      U2 = GetFreeUnitNumber()
      OPEN(UNIT=U2, FILE=TRIM(File2), STATUS='REPLACE', ERR=821)
      CALL FileWasOpened(U2); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Open another list file. This one will contain the filenames of only
      !  those stations that DID have fatal errors.  If all stations pass
      !  with no fatal errors, this file will be empty, and it will be
      !  deleted as the last step in this procedure.
      !
      File3 = TRIM(WorkingDir) // 'station_file_list_bad.txt'
      U3 = GetFreeUnitNumber()
      OPEN(UNIT=U3, FILE=TRIM(File3), STATUS='REPLACE', ERR=831)
      CALL FileWasOpened(U3); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Main loop
      !
      GoodStations = 0;  BadStations = 0
      DO StNum = 1, StationsToProcess
         !
         !  Build a version of the file name that includes the file path
         !
         SFName = TRIM(WorkingDir) // TRIM(StationFileNames(StNum))
         FP = FilePathOnly(SFName);  IF (ErrorLevel .NE. 0) GOTO 899    ! probably empty
         FN = FileNameOnly(SFName);  IF (ErrorLevel .NE. 0) GOTO 899
         FName = TRIM(WorkingDir) // TRIM(FN)
   
         !
         !  Count of errors/warnings for just this station
         !  Variables are MODULE global, but PRIVATE to module.
         !
         Station_Fatal    = 0
         Station_Warnings = 0
         Station_Removed  = 0
         
         !
         !  Do all of the checks, creating the temporary file as it goes.
         !  If ReadFile_DailyStation() encounters problems, then that means
         !  there was some problem with the format of the file. In that case,
         !  we will just skip that file.
         !
         !  Note that we do NOT assume fixed width format
         !
         CALL StnDataIn%Clear()
         CALL ReadFile_DailyStation(FName, StnDataIn)
         
         !
         !  File was read correctly. Check the data.
         !
         IF (ErrorLevel .EQ. 0) THEN
            CALL StnDataOut%Clear()
            CALL CheckDataForOneStation(StnDataIn, StnDataOut);  IF (ErrorLevel .NE. 0) GOTO 899
            WRITE(EMsg, 1150) TRIM(StnDataIn%StnID), Station_Fatal, Station_Warnings, Station_Removed
            CALL WriteErrorMessage(TRIM(EMsg), ULog)

            !
            !  Is there valid data to be written?  This is tested by looking at
            !  the specified start date.
            !  If valid, write a new file with the (possibly) revised data.
            !  If not, we cannot use this station at all, but I don't want to flag it
            !  as "fatal", and stop execution. But I can't just leave the file there, or it
            !  will get used in the next steps. So I will rename it to invalid_<filename>.
            !
            IF (StnDataOut%SDateSeq .EQ. MissingData_Date) THEN
               TempFile = TRIM(WorkingDir) // 'invalid_' // TRIM(FN)
               CALL RenameFile(TRIM(SFName), TRIM(TempFile), IsOK);    IF (ErrorLevel .NE. 0) GOTO 899
            ELSE            
               IF (Station_Fatal .EQ. 0) THEN
                  TempFile = TRIM(WorkingDir) // 'temporary_station_file.txt'
                  CALL WriteFile_DailyStation(TRIM(TempFile), StnDataOut); IF (ErrorLevel .NE. 0) GOTO 899
                  CALL RenameFile(TRIM(TempFile), TRIM(SFName), IsOK);    IF (ErrorLevel .NE. 0) GOTO 899
                  IF (.NOT. IsOK) THEN
                     EMsg = 'Error renaming temporary station file.'
                     CALL WriteErrorMessage(TRIM(EMsg), ULog)
                     GOTO 898
                  END IF
                  CALL DeleteFile(TRIM(TempFile));     IF (ErrorLevel .NE. 0) GOTO 899
                  GoodStations = GoodStations + 1
                  WRITE(U2, 1101, ERR=823) TRIM(FName)
               ELSE
                  BadStations = BadStations + 1
                  WRITE(U3, 1101, ERR=833) TRIM(FName)
               END IF
            END IF
            
            Total_Fatal    = Total_Fatal    + Station_Fatal
            Total_Warnings = Total_Warnings + Station_Warnings
            Total_Removed  = Total_Removed  + Station_Removed
            CALL StnDataOut%Clear()
         ELSE
            WRITE(ErrorMessage, *) 'Unrecoverable formatting errors found in station file: ', TRIM(FName), ' [skipping]'
            CALL PassMsg
            Total_Fatal = Total_Fatal + 1
!            ErrorLevel = 0              ! Now that we have handled the error condition, reset ErrorLevel for next station
            GOTO 999
         END IF
         CALL StnDataIn%Clear()
         CALL StnDataOut%Clear()     
      END DO

      CLOSE(U2)
      CALL FileWasClosed(U2)

      !
      !  Summary information
      !
      WRITE(EMsg, 1201) Total_Fatal
      CALL WriteErrorMessage(TRIM(EMsg), ULog)
      WRITE(EMsg, 1202) Total_Warnings
      CALL WriteErrorMessage(TRIM(EMsg), ULog)
      WRITE(EMsg, 1203) Total_Removed
      CALL WriteErrorMessage(TRIM(EMsg), ULog)

      IF (Total_Fatal .GT. 0) THEN
         IF (Total_Fatal .EQ. 1) THEN
            WRITE(EMsg, 1221) BadStations
         ELSE
            WRITE(EMsg, 1222) BadStations
         END IF
         CALL WriteErrorMessage(TRIM(EMsg), ULog)
         WRITE(EMsg, 1223)
         CALL WriteErrorMessage(TRIM(EMsg), ULog)
         CLOSE(U3)
         CALL FileWasClosed(U3)
      ELSE
         WRITE(EMsg, 1222) 0
         CALL WriteErrorMessage(TRIM(EMsg), ULog)
         CLOSE(U3, STATUS='DELETE')
         CALL FileWasClosed(U3)
      END IF
      
      EMsg = 'Stations ready to be added to the database are listed in station_file_list_good.txt'
      CALL WriteErrorMessage(TRIM(EMsg), ULog)

      CALL DATE_AND_TIME(DateStr, Time, Zone, DTV); IF (ErrorLevel .NE. 0) GOTO 899
      IsOK = .TRUE.
      IF (DTV(1) .LE. 0) IsOK = .FALSE.      ! year
      IF (DTV(2) .LE. 0) IsOK = .FALSE.      ! month
      IF (DTV(3) .LE. 0) IsOK = .FALSE.      ! day
      IF (DTV(5) .LE. 0) IsOK = .FALSE.      ! hour
      IF (DTV(6) .LE. 0) IsOK = .FALSE.      ! minute
      WRITE(ULog, 9003, ERR=803) DTV(1), MonCode3(DTV(2)), DTV(3), (DTV(I), I=5,6)
      
      CLOSE(ULog)
      CALL FileWasClosed(ULog)
      CALL StnDataIn%Clear()
      
      GOTO 999

!
!  Handlers for I/O errors
!
  801 ErrorMessage = 'Error opening file '//TRIM(LogFilename); CALL PassMsg; GOTO 898
  803 ErrorMessage = 'Error writing file '//TRIM(LogFilename); CALL PassMsg; GOTO 898
  811 ErrorMessage = 'Error opening file '//TRIM(File1); CALL PassMsg; GOTO 898
  821 ErrorMessage = 'Error opening file '//TRIM(File2); CALL PassMsg; GOTO 898
  823 ErrorMessage = 'Error writing file '//TRIM(File2); CALL PassMsg; GOTO 898
  831 ErrorMessage = 'Error opening file '//TRIM(File3); CALL PassMsg; GOTO 898
  833 ErrorMessage = 'Error writing file '//TRIM(File3); CALL PassMsg; GOTO 898
  

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] DoStnDataRangeChecking()...'; CALL PassMsg

  999 IF (FileIsOpen(ULog)) THEN
         CLOSE(ULog); CALL FileWasClosed(ULog)
      END IF
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1);   CALL FileWasClosed(U1)
      END IF
      IF (FileIsOpen(U2)) THEN
         CLOSE(U2);   CALL FileWasClosed(U2)
      END IF
      CALL StnDataIn%Clear()
      CALL StnDataOut%Clear()
  
      RETURN
      
!
!  FORMAT statements
!
 1101 FORMAT(A)
 1150 FORMAT(' Summary for station ',A, ':  Fatal=',I0, ', Warnings=',I0, ', Removed=',I0)
 1201 FORMAT('There were a total of ', I0, ' fatal errors encountered')
 1202 FORMAT('There were a total of ', I0, ' warnings encountered')
 1203 FORMAT('There were a total of ', I0, ' data values removed')
 1221 FORMAT(I0, ' station was flagged as having fatal errors.')
 1222 FORMAT(I0, ' stations were flagged as having fatal errors.')
 1223 FORMAT('Stations with fatal errors are listed in station_list_bad.txt.')
 1301 FORMAT('Error: Too many station files were supplied. Maximum limit is ', I0, '.')
 
 9001 FORMAT('stn_err_log-', I4.4,I2.2,I2.2, '-', I2.2,I2.2, '.txt')
 9002 FORMAT('Log of errors from StnDataRangeChk run on ', I4,A3,I2.2, ', at ', I2.2,':',I2.2)
 9003 FORMAT('Process StnDataRangeChk finished on ', I4,A3,I2.2, ', at ', I2.2,':',I2.2)
 
      END SUBROUTINE DoStnDataRangeChecking


!------------------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------------------
      SUBROUTINE CheckDataForOneStation(DataIn, DataOut)
      IMPLICIT NONE
      TYPE (TDlyDataMetStn) :: DataIn, DataOut
      INTEGER :: I, K1, K2, IOS, Dy, Mn, Yr, NDays
      INTEGER :: SeqMin, SeqMax, SSeq, ESeq, Seq, Seq1, Seq2
      LOGICAL :: F, OK, DoTruncation
      CHARACTER(LEN=50) :: StnID
      REAL, DIMENSION(:), ALLOCATABLE :: TempNew, TempOld
      TYPE (TDlyData), POINTER :: TDD, TDD1, TDD2

      !
      !  Default return state of DataOut object is to match meta info from
      !  DataIn, but with no actual data.
      !
      IF (.NOT. DataOut%CopyMetaFrom(DataIn)) GOTO 899
      
      !
      !  If either date value is missing, then the output file will be
      !  flagged as a fatal error and ignored.
      !
      IF ((DataIn%SDateSeq .EQ. MissingData_Date) .OR.            &
          (DataIn%EDateSeq   .EQ. MissingData_Date)) THEN
         WRITE(ErrorMessage, 1010) TRIM(DataIn%StnID)
         CALL WriteErrorMessage(TRIM(ErrorMessage), ULog)
         Station_Fatal = Station_Fatal + 1
         GOTO 999
      END IF
          
      !
      !  First thing to do is to clone DataIn to DataOut. This way,
      !  we can make modifications as we go, when appropriate.
      !
      IF (.NOT. DataOut%CopyFrom(DataIn)) GOTO 899

      !
      !  Check lat/long range
      !
      OK = .TRUE.
      IF (DataOut%Latitude .LT. LatitudeRange_Min) OK = .FALSE.
      IF (DataOut%Latitude .GT. LatitudeRange_Max) OK = .FALSE.
      IF (.NOT. OK) THEN 
         WRITE(ErrorMessage, 1001) TRIM(DataOut%StnID), DataOut%Latitude
         CALL WriteErrorMessage(TRIM(ErrorMessage), ULog)
         Station_Fatal = Station_Fatal + 1
      END IF
      
      OK = .TRUE.
      IF (DataOut%Longitude .LT. LongitudeRange_Min) OK = .FALSE.
      IF (DataOut%Longitude .GT. LongitudeRange_Max) OK = .FALSE.
      IF (.NOT. OK) THEN 
         WRITE(ErrorMessage, 1002) TRIM(DataOut%StnID), DataOut%Longitude
         CALL WriteErrorMessage(TRIM(ErrorMessage), ULog)
         Station_Fatal = Station_Fatal + 1
      END IF
      
      !
      !  Verify that the data types/units were all recognized
      !
      OK = .TRUE.
      DO I = 1, DataOut%NumDatasets
         TDD => DataOut%GetPointerToDataByIndex(I)
         IF (ASSOCIATED(TDD)) THEN
            IF (TDD%GetDataType() .EQ. GDT_Undefined) OK = .FALSE.
         END IF
      END DO
      IF (.NOT. OK) THEN 
         WRITE(ErrorMessage, 1003) TRIM(DataOut%StnID)
         CALL WriteErrorMessage(TRIM(ErrorMessage), ULog)
         Station_Fatal = Station_Fatal + 1
      END IF
      
      OK = .TRUE.
      DO I = 1, DataOut%NumDatasets
         TDD => DataOut%GetPointerToDataByIndex(I)
         IF (ASSOCIATED(TDD)) THEN
            IF (TDD%GetDataUnit() .EQ. GDU_Undefined) OK = .FALSE.
         END IF
      END DO
      IF (.NOT. OK) THEN 
         WRITE(ErrorMessage, 1004) TRIM(DataOut%StnID)
         CALL WriteErrorMessage(TRIM(ErrorMessage), ULog)
         Station_Fatal = Station_Fatal + 1
      END IF

      !
      !  Verify valid date range (based on header info)
      !  Note that in this case I will treat it as a WARNING, not an error.  
      !  I will then truncate the data records to eliminate the out-of-range data.
      !
      DoTruncation = .FALSE.
      CALL SequenceDate(Dy, Mn, Yr, DataOut%SDateSeq); IF (ErrorLevel .NE. 0) GOTO 899
      IF (Yr .LT. YearRange_Min) THEN
         WRITE(ErrorMessage, 1005) TRIM(DataOut%StnID), Yr, YearRange_Min
         CALL WriteErrorMessage(TRIM(ErrorMessage), ULog)
         Station_Warnings = Station_Warnings + 1
         DoTruncation = .TRUE.
      END IF
      
      CALL SequenceDate(Dy, Mn, Yr, DataOut%EDateSeq); IF (ErrorLevel .NE. 0) GOTO 899
      IF (Yr .GT. YearRange_Max) THEN
         WRITE(ErrorMessage, 1006) TRIM(DataOut%StnID), Yr, YearRange_Max
         CALL WriteErrorMessage(TRIM(ErrorMessage), ULog)
         Station_Warnings = Station_Warnings + 1
         DoTruncation = .TRUE.
      END IF

      !
      !  If we encountered any fatal errors in the header info, then we won't be using
      !  this station. No point in looking at the data values. Just return and let the
      !  main program eliminate this station from consideration.
      !
      !  To do this, I will just set NumDatasets to -1 in the DataOut object.
      !  The main program will recognize that as a special flag value and handle it
      !  accordingly.
      !
      IF (Station_Fatal .GT. 0) THEN
         DataOut%NumDatasets = -1
         RETURN
      END IF
      
      !
      !  If we need to truncate the date range, do that now.
      !
      IF (DoTruncation) THEN
         CALL DateSequence( 1,  1, YearRange_Min, SeqMin); IF (ErrorLevel .NE. 0) GOTO 899
         CALL DateSequence(31, 12, YearRange_Max, SeqMax); IF (ErrorLevel .NE. 0) GOTO 899
         SSeq = MAX(SeqMin, DataOut%SDateSeq)
         ESeq = MIN(SeqMax, DataOut%EDateSeq)
         DataOut%SDateSeq = SSeq
         DataOut%EDateSeq = ESeq
         
         !  Allocate array one time, big enough to hold any data set
         NDays = ESeq - SSeq + 1
         IF (NDays .GT. 0) THEN
            ALLOCATE(TempNew(NDays), STAT=IOS)
            IF (IOS .NE. 0) THEN
               WRITE(ErrorMessage, 1007) TRIM(DataOut%StnID)
               GOTO 898
            END IF
         
            !
            !  Copy/truncate data
            !
            DO I = 1, DataOut%NumDatasets
               TDD => DataOut%GetPointerToDataByIndex(I)
               IF (ASSOCIATED(TDD)) THEN
                  TempOld = TDD%GetDataVals()    ! automatically allocated by the assignment
                  Seq1 = TDD%GetStartDate()      ! old date extent for this data set
                  Seq2 = TDD%GetEndDate()
                  TempNew(:) = MissingData_Real                  ! new data will be in indexes 1..n
                  DO Seq = SSeq, ESeq
                     IF ((Seq .GE. Seq1) .AND. (Seq .LE. Seq2)) THEN
                        K1 = Seq - Seq1 + 1                         ! index into OLD data set
                        K2 = Seq - SSeq + 1                         ! index into NEW data set
                        TempNew(K2) = TempOld(K1)
                     END IF
                  END DO
                  F = TDD%ClearData();                     IF (ErrorLevel .NE. 0) GOTO 899
                  F = TDD%AssignData(SSeq, ESeq, TempNew); IF (ErrorLevel .NE. 0) GOTO 899
               END IF
            END DO
            DEALLOCATE(TempOld, STAT=IOS)
            DEALLOCATE(TempNew, STAT=IOS)
         ELSE
            DataOut%SDateSeq = MissingData_Date
            DataOut%EDateSeq = MissingData_Date
            DO I = 1, DataOut%NumDatasets
               TDD => DataOut%GetPointerToDataByIndex(I)
               F = TDD%ClearData();  IF (ErrorLevel .NE. 0) GOTO 899
            END DO            
         END IF
      END IF
      
      !
      !  If the data in the file was ALL outside the valid range, then we just cleared all of the
      !  data. Thus, we do not need to do any of the range checking stuff.
      !  We can just jump down to final cleanup and return.
      !
      IF ((DataOut%SDateSeq .EQ. MissingData_Date) .OR.            &
          (DataOut%EDateSeq .EQ. MissingData_Date)) THEN
         ErrorMessage = ' Station will be renamed invalid_<filename> and not used.'; CALL PassMsg
         GOTO 999
      END IF
      
      !
      !  Now check the daily data values in DataOut, removing them if they fail the tests.
      !  I will only be checking the data types that are pertinent to GLSHFS.
      !
      StnID = TRIM(DataOut%StnID)
      TDD1 => DataOut%GetPointerToDataOfType(GDT_AirTempMax)
      IF (ASSOCIATED(TDD1)) CALL CheckAirtempRange(StnID, TDD1)
      
      TDD2 => DataOut%GetPointerToDataOfType(GDT_AirTempMin)
      IF (ASSOCIATED(TDD2)) CALL CheckAirtempRange(StnID, TDD2)
      
      IF ((ASSOCIATED(TDD1)) .AND. (ASSOCIATED(TDD2))) THEN
         CALL CheckAirtempMaxMin(StnID, TDD1, TDD2)
      END IF

      
      TDD1 => DataOut%GetPointerToDataOfType(GDT_DewpointMean)
      IF (ASSOCIATED(TDD1)) CALL CheckAirtempRange(StnID, TDD1)
      
      TDD2 => DataOut%GetPointerToDataOfType(GDT_AirTempMean)
      IF (ASSOCIATED(TDD2)) CALL CheckAirtempRange(StnID, TDD2)
      
      IF ((ASSOCIATED(TDD1)) .AND. (ASSOCIATED(TDD2))) THEN
         CALL CheckDewpointVsAir(StnID, TDD1, TDD2)
      END IF

      
      TDD1 => DataOut%GetPointerToDataOfType(GDT_Precipitation)
      IF (ASSOCIATED(TDD1)) CALL CheckPrecipitationRange(StnID, TDD1)
      IF (ASSOCIATED(TDD1)) CALL CheckPrecipitationRange_Monthly(StnID, TDD1)
      
      TDD1 => DataOut%GetPointerToDataOfType(GDT_WindSpeed)
      IF (ASSOCIATED(TDD1)) CALL CheckWindSpeedRange(StnID, TDD1)
      
      TDD1 => DataOut%GetPointerToDataOfType(GDT_CloudCover)
      IF (ASSOCIATED(TDD1)) CALL CheckCloudCoverRange(StnID, TDD1)
      
      GOTO 999     ! for final cleanup

      !
      !  Error handling
      !
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] CheckDataForOneStation()... '; CALL PassMsg
  
  999 IF (ALLOCATED(TempNew)) DEALLOCATE(TempNew, STAT=IOS)
      IF (ALLOCATED(TempOld)) DEALLOCATE(TempOld, STAT=IOS)
      RETURN

      !
      !  FORMATs      
      !
 1001 FORMAT(A, ': Latitude (', F0.3, ') outside the allowable range. [fatal error]')
 1002 FORMAT(A, ': Longitude (', F0.3, ') outside the allowable range. [fatal error]')
 1003 FORMAT(A, ': One or more invalid data types. [fatal error]')
 1004 FORMAT(A, ': One or more invalid data units. [fatal error]')
 1005 FORMAT(A, ': Earliest data (',I4.4,') is before allowable earliest data (',I4.4,') [warning]')
 1006 FORMAT(A, ': Latest data (',I4.4,') is after allowable latest data (',I4.4,') [warning]')
 1007 FORMAT(A, ': Error allocating memory for truncation of date range.')
 1010 FORMAT(A, ': Invalid start and/or end date. [fatal error]')

      END SUBROUTINE CheckDataForOneStation

!------------------------------------------------------------------------------
!  This routine checks that the daily air temp values are within reasonable limits.
!  Note that I will use this same routine for all of the drybulb air temp checks.
!  I won't make special routines for daily max, daily min, and daily mean.
!  I simply assume that the ranges are good enough to handle all 3 data types.
!
!  Note that I do adjust the valid temperature ranges based on the date.
!  That's because 35 C is possible in July, but not in January.
!  And -30 C is possible in January, but not July.
!
!  To set the range, I used:
!  International Falls, MN was used as the standard for the minimums. 
!  Fort Wayne, IN was used as the standard for the maximums.
!
!   Month    AirTemps (Max / Min)
!   Jan      25 / -50
!   Feb      25 / -50
!   Mar      30 / -40
!   Apr      35 / -30
!   May      40 / -20
!   Jun      45 / -10
!   Jul      50 /  -5
!   Aug      45 /  -5
!   Sep      40 / -15
!   Oct      35 / -25
!   Nov      30 / -40
!   Dec      25 / -50
!
!   For each day:
!      1) Convert to Celsius
!      2) Check to see if it is within that min/max range for the month. If
!         not, I simply replace it with MissingData_Real.
!
!--------------------------------------------------------------------------
      SUBROUTINE CheckAirtempRange(StnID, TDD)      
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: StnID
      TYPE (TDlyData), POINTER :: TDD

      INTEGER :: I, Seq, Dy, Mn, Yr
      REAL :: CVal
      CHARACTER(LEN=30) :: DTS
      CHARACTER(LEN=99) :: EMsg
      
      INTEGER, DIMENSION(12), PARAMETER :: MaxTemps =      &
              (/ 25,  25,  30,  35,  40,  45, 50, 45,  40,  35,  30,  25/)
      INTEGER, DIMENSION(12), PARAMETER :: MinTemps =      &
              (/-50, -50, -40, -30, -20, -10, -5, -5, -15, -25, -40, -50/)
      
      IF (TDD%GetStartDate() .EQ. MissingData_Date) RETURN
      IF (TDD%GetEndDate()   .EQ. MissingData_Date) RETURN
      
      DTS = GlerlDataTypeString(TDD%GetDataType())
      DO Seq = TDD%GetStartDate(), TDD%GetEndDate()
         CALL SequenceDate(Dy, Mn, Yr, Seq)
         I = Seq - TDD%GetStartDate() + 1
         CVal = UnitConvertedDataValue(TDD%GetDataVal(I), TDD%GetDataUnit(), GDU_Celsius)
         IF (.NOT. IsMissing(CVal)) THEN
            IF (CVal .LT. MinTemps(Mn)) THEN
               WRITE(EMsg, 1001) TRIM(StnID), Yr, Mn, Dy, TRIM(DTS), CVal, MinTemps(Mn)
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDD%SetDataValue(I, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
            IF (CVal .GT. MaxTemps(Mn)) THEN
               WRITE(EMsg, 1002) TRIM(StnID), Yr, Mn, Dy, TRIM(DTS), CVal, MaxTemps(Mn)
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDD%SetDataValue(I, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
         END IF
      END DO
      RETURN
 
 1001 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : ', A, ' (', F0.2, 'C) is < lower limit (', I0, 'C). Removed.')
 1002 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : ', A, ' (', F0.2, 'C) is > upper limit (', I0, 'C). Removed.')

      END SUBROUTINE CheckAirtempRange
      
      
!------------------------------------------------------------------------------
!  This routine just checks each day that the Max air temp >= Min air temp.
!  If either value is missing, no test is performed.
!  If the test fails, BOTH values are removed.
!  Only the common period for the data sets is checked. 
!--------------------------------------------------------------------------
      SUBROUTINE CheckAirtempMaxMin(StnID, TDDMax, TDDMin)      
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: StnID
      TYPE (TDlyData), POINTER :: TDDMax, TDDMin

      INTEGER :: I, J, Seq, Dy, Mn, Yr, SSeq, ESeq
      REAL :: CValMax, CValMin
      CHARACTER(LEN=99) :: EMsg

      REAL, PARAMETER :: MaxDiff = 40.0
      
      IF (TDDMax%GetStartDate() .EQ. MissingData_Date) RETURN
      IF (TDDMax%GetEndDate()   .EQ. MissingData_Date) RETURN
      IF (TDDMin%GetStartDate() .EQ. MissingData_Date) RETURN
      IF (TDDMin%GetEndDate()   .EQ. MissingData_Date) RETURN
      
      SSeq = MAX(TDDMax%GetStartDate(), TDDMin%GetStartDate())
      ESeq = MIN(TDDMax%GetEndDate(), TDDMin%GetEndDate())
      
      DO Seq = SSeq, ESeq
         I = Seq - TDDMax%GetStartDate() + 1
         CValMax = UnitConvertedDataValue(TDDMax%GetDataVal(I), TDDMax%GetDataUnit(), GDU_Celsius)
         J = Seq - TDDMin%GetStartDate() + 1
         CValMin = UnitConvertedDataValue(TDDMin%GetDataVal(J), TDDMin%GetDataUnit(), GDU_Celsius)

         IF ((.NOT. IsMissing(CValMax)) .AND. (.NOT. IsMissing(CValMin))) THEN
            IF (CValMax .LT. CValMin) THEN
               CALL SequenceDate(Dy, Mn, Yr, Seq)
               WRITE(EMsg, 1001) TRIM(StnID), Yr, Mn, Dy, CValMax, CValMin
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDDMax%SetDataValue(I, MissingData_REAL)
               CALL TDDMin%SetDataValue(J, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
         END IF
         IF ((.NOT. IsMissing(CValMax)) .AND. (.NOT. IsMissing(CValMin))) THEN
            IF ((CValMax-CValMin) .GT. MaxDiff) THEN
               CALL SequenceDate(Dy, Mn, Yr, Seq)
               WRITE(EMsg, 1002) TRIM(StnID), Yr, Mn, Dy, CValMax-CValMin
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDDMax%SetDataValue(I, MissingData_REAL)
               CALL TDDMin%SetDataValue(J, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
         END IF
      END DO
      RETURN

 1001 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : MaxAirTemp (', F0.2, 'C) < MinAirTemp (', F0.2, 'C). Both removed.')
 1002 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : AirTemp spread (Max-Min) too large (', F0.2, 'C). Both removed.')

       END SUBROUTINE CheckAirtempMaxMin
      
!------------------------------------------------------------------------------
!  This routine just checks each day that the dewpoint temp <= mean air temp.
!  If either value is missing, no test is performed.
!  If the test fails, ONLY Dewpoint value is removed.  Maybe this should change?
!  Only the common period for the data sets is checked. 
!--------------------------------------------------------------------------
      SUBROUTINE CheckDewpointVsAir(StnID, TDDDew, TDDAir)      
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: StnID
      TYPE (TDlyData), POINTER :: TDDDew, TDDAir

      INTEGER :: I, J, Seq, Dy, Mn, Yr, SSeq, ESeq
      REAL :: DewP, AirT
      CHARACTER(LEN=99) :: EMsg
      
      IF (TDDDew%GetStartDate() .EQ. MissingData_Date) RETURN
      IF (TDDDew%GetEndDate()   .EQ. MissingData_Date) RETURN

      SSeq = MAX(TDDDew%GetStartDate(), TDDAir%GetStartDate())
      ESeq = MIN(TDDDew%GetEndDate(), TDDAir%GetEndDate())
      
      DO Seq = SSeq, ESeq
         I = Seq - TDDDew%GetStartDate() + 1
         DewP = UnitConvertedDataValue(TDDDew%GetDataVal(I), TDDDew%GetDataUnit(), GDU_Celsius)
         J = Seq - TDDAir%GetStartDate() + 1
         AirT = UnitConvertedDataValue(TDDAir%GetDataVal(J), TDDAir%GetDataUnit(), GDU_Celsius)

         IF ((.NOT. IsMissing(DewP)) .AND. (.NOT. IsMissing(AirT))) THEN
            IF (DewP .GT. AirT) THEN
               CALL SequenceDate(Dy, Mn, Yr, Seq)
               WRITE(EMsg, 1001) TRIM(StnID), Yr, Mn, Dy, DewP, AirT
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDDDew%SetDataValue(I, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
         END IF
      END DO
      RETURN

 1001 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : DewpointTemp (', F0.2, 'C) > AirTemp (', F0.2, 'C). Dewpoint removed.')

       END SUBROUTINE CheckDewpointVsAir

!------------------------------------------------------------------------------
!  This routine checks that the daily precipitation values are within reasonable limits.
!  Allowable range is set at 0 to 254 millimeters (10 inches).
!------------------------------------------------------------------------------
      SUBROUTINE CheckPrecipitationRange(StnID, TDD)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: StnID
      TYPE (TDlyData),  POINTER    :: TDD

      INTEGER :: I, Seq, Dy, Mn, Yr
      REAL :: CVal
      CHARACTER(LEN=99) :: EMsg
      
      REAL, PARAMETER :: MinPrec =   0.0
      REAL, PARAMETER :: MaxPrec = 254.0
      
      IF (TDD%GetStartDate() .EQ. MissingData_Date) RETURN
      IF (TDD%GetEndDate()   .EQ. MissingData_Date) RETURN

      DO Seq = TDD%GetStartDate(), TDD%GetEndDate()
         CALL SequenceDate(Dy, Mn, Yr, Seq)
         I = Seq - TDD%GetStartDate() + 1
         
         !
         !  Some formats of my station files use a value of -9.9e9
         !  to indicate missing data values. This caused me to have 
         !  over 1 million "out of range" messages in a test run I did.
         !  Values like this are obviously NOT really an "out of range"
         !  issue, and I would like to suppress those messages. All I 
         !  actually need to do is change the value to missing.
         !  So... do that.  In the case of precip, it's kind of easy to
         !  check the range, since it should never be <0. So for this
         !  particular check, I will use -99.99 (or less) as the nominal 
         !  indicator that the file is actually intending it to be a
         !  missing data value. I will assume that the units are something
         !  reasonable.
         !
         IF (TDD%GetDataVal(I) .LT. -99.98) CALL TDD%SetDataValue(I, MissingData_REAL)
         
         !
         !  Now do the normal check
         !
         CVal = UnitConvertedDataValue(TDD%GetDataVal(I), TDD%GetDataUnit(), GDU_Millimeters)
         IF (.NOT. IsMissing(CVal)) THEN
            IF (CVal .LT. MinPrec) THEN
               WRITE(EMsg, 1001) TRIM(StnID), Yr, Mn, Dy, CVal, MinPrec
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDD%SetDataValue(I, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
            IF (CVal .GT. MaxPrec) THEN
               WRITE(EMsg, 1002) TRIM(StnID), Yr, Mn, Dy, CVal, MaxPrec
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDD%SetDataValue(I, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
         END IF
      END DO
      RETURN
 
 1001 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : Precipitation (', F0.2, 'mm) is < lower limit (', F0.2, 'mm). Removed.')
 1002 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : Precipitation (', F0.2, 'mm) is > upper limit (', F0.2, 'mm). Removed.')

      END SUBROUTINE CheckPrecipitationRange
      
!------------------------------------------------------------------------------
!  This routine checks that the monthly precipitation values are within reasonable limits.
!  Allowable range is set at 0 to 457 millimeters (18 inches).
!
!  Note that this routine MUST FOLLOW the daily range check routine. It presumes that
!  various checks and adjustments have already been performed, which happen
!  during the daily range checking.  If this is run prior to thwe daily check, it may
!  give invalid results.
!------------------------------------------------------------------------------
      SUBROUTINE CheckPrecipitationRange_Monthly(StnID, TDD)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: StnID
      TYPE (TDlyData),  POINTER    :: TDD

      INTEGER :: I, J, K, Dy, Mn, Yr, Seq, SSeq, ESeq
      INTEGER :: PM, PY, SD, SM, SY, ED, EM, EY, DMax, Seq1, Seq2
      LOGICAL :: ClearValues
      REAL    :: CVal, MTotal
      CHARACTER(LEN=99) :: EMsg
      REAL, PARAMETER :: MaxPrec = 457.0
      
      SSeq = TDD%GetStartDate()
      ESeq = TDD%GetEndDate()
      IF (SSeq .EQ. MissingData_Date) RETURN
      IF (ESeq .EQ. MissingData_Date) RETURN
      
      CALL SequenceDate(SD, SM, SY, SSeq); IF (ErrorLevel .NE. 0) GOTO 899
      CALL SequenceDate(ED, EM, EY, SSeq); IF (ErrorLevel .NE. 0) GOTO 899
      
      PM = SM
      PY = SY
      MTotal = 0.0
      DO Seq = SSeq, ESeq+1
         I = Seq - SSeq + 1
         CALL SequenceDate(Dy, Mn, Yr, Seq); IF (ErrorLevel .NE. 0) GOTO 899
         
         !
         !  I am using Seq == ESeq+1 as the flag that my previous pass was the last one.
         !  Force a test of the monthly total for that final month.
         !
         IF (Seq .EQ. ESeq+1) Mn = -1
         
         !
         !  If this is a new month, test the previous month's total
         !  If it is outside the valid range, set all daily values for
         !  that month to "missing".
         !
         IF ((Mn .NE. PM) .OR. (Yr .NE. PY)) THEN
            ClearValues = .FALSE.
            IF (MTotal .LT. 0.0) THEN
               WRITE(EMsg, 1001) TRIM(StnID), PY, PM, MTotal, 0.0
               CALL WriteErrorMessage(EMsg, ULog)
               ClearValues = .TRUE.
            END IF
            IF (MTotal .GT. MaxPrec) THEN
               WRITE(EMsg, 1002) TRIM(StnID), PY, PM, MTotal, MaxPrec
               CALL WriteErrorMessage(EMsg, ULog)
               ClearValues = .TRUE.
            END IF
            IF (ClearValues) THEN
               CALL DateSequence(1, PM, PY, Seq1)
               DMax = DaysInMonth(PM, PY)
               CALL DateSequence(DMax, PM, PY, Seq2)
               Seq1 = MAX(Seq1, SSeq)
               Seq2 = MIN(Seq2, ESeq)
               DO J = Seq1, Seq2
                  K = J - SSeq + 1
                  CALL TDD%SetDataValue(K, MissingData_REAL)
               END DO
               Station_Removed = Station_Removed + (Seq2-Seq1+1)
            END IF
            PM = Mn
            PY = Yr
            MTotal = 0.0
         END IF
         IF (Mn .GT. 0) THEN
            CVal = UnitConvertedDataValue(TDD%GetDataVal(I), TDD%GetDataUnit(), GDU_Millimeters)
            IF (.NOT. IsMissing(CVal)) MTotal = MTotal + CVal
         END IF
      END DO
  
      RETURN
 
      !
      !  Error handling
      !
  899 ErrorMessage = '[traceback] CheckPrecipitationRange_Monthly... '; CALL PassMsg
  
 1001 FORMAT(A, ': ', I4.4,'-',I2.2, 3X, ' : Monthly Precipitation (', F0.1, 'mm) is < lower limit (', F0.1, 'mm). Removed.')
 1002 FORMAT(A, ': ', I4.4,'-',I2.2, 3X, ' : Monthly Precipitation (', F0.1, 'mm) is > upper limit (', F0.1, 'mm). Removed.')

      END SUBROUTINE CheckPrecipitationRange_Monthly
      
!------------------------------------------------------------------------------
!  This routine checks that the windspeed values are within reasonable limits.
!  Allowable range is set at 0 to 25 meters/sec (56 miles per hour).
!------------------------------------------------------------------------------
      SUBROUTINE CheckWindspeedRange(StnID, TDD)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: StnID
      TYPE (TDlyData),  POINTER    :: TDD

      INTEGER :: I, Seq, Dy, Mn, Yr
      REAL :: CVal
      CHARACTER(LEN=99) :: EMsg
      
      REAL, PARAMETER :: MinSpeed =  0.0
      REAL, PARAMETER :: MaxSpeed = 25.0
      
      IF (TDD%GetStartDate() .EQ. MissingData_Date) RETURN
      IF (TDD%GetEndDate()   .EQ. MissingData_Date) RETURN

      DO Seq = TDD%GetStartDate(), TDD%GetEndDate()
         CALL SequenceDate(Dy, Mn, Yr, Seq)
         I = Seq - TDD%GetStartDate() + 1
         CVal = UnitConvertedDataValue(TDD%GetDataVal(I), TDD%GetDataUnit(), GDU_MetersPerSecond)
         IF (.NOT. IsMissing(CVal)) THEN
            IF (CVal .LT. MinSpeed) THEN
               WRITE(EMsg, 1001) TRIM(StnID), Yr, Mn, Dy, CVal, MinSpeed
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDD%SetDataValue(I, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
            IF (CVal .GT. MaxSpeed) THEN
               WRITE(EMsg, 1002) TRIM(StnID), Yr, Mn, Dy, CVal, MaxSpeed
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDD%SetDataValue(I, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
         END IF
      END DO
      RETURN
 
 1001 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : Windspeed (', F0.2, 'm/s) is < lower limit (', F0.2, 'm/s). Removed.')
 1002 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : Windspeed (', F0.2, 'm/s) is > upper limit (', F0.2, 'm/s). Removed.')

      END SUBROUTINE CheckWindspeedRange
      
       
!------------------------------------------------------------------------------
!  This routine checks that the Cloud Cover values are within reasonable limits.
!  Allowable range is set at 0 to 100%.
!------------------------------------------------------------------------------
      SUBROUTINE CheckCloudCoverRange(StnID, TDD)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: StnID
      TYPE (TDlyData),  POINTER    :: TDD

      INTEGER :: I, Seq, Dy, Mn, Yr
      REAL :: CVal
      CHARACTER(LEN=99) :: EMsg
      
      REAL, PARAMETER :: MinCloud =   0.0
      REAL, PARAMETER :: MaxCloud = 100.0
      
      IF (TDD%GetStartDate() .EQ. MissingData_Date) RETURN
      IF (TDD%GetEndDate()   .EQ. MissingData_Date) RETURN

      DO Seq = TDD%GetStartDate(), TDD%GetEndDate()
         CALL SequenceDate(Dy, Mn, Yr, Seq)
         I = Seq - TDD%GetStartDate() + 1
         CVal = UnitConvertedDataValue(TDD%GetDataVal(I), TDD%GetDataUnit(), GDU_Percent)
         IF (.NOT. IsMissing(CVal)) THEN
            IF (CVal .LT. MinCloud) THEN
               WRITE(EMsg, 1001) TRIM(StnID), Yr, Mn, Dy, CVal, MinCloud
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDD%SetDataValue(I, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
            IF (CVal .GT. MaxCloud) THEN
               WRITE(EMsg, 1002) TRIM(StnID), Yr, Mn, Dy, CVal, MaxCloud
               CALL WriteErrorMessage(EMsg, ULog)
               CALL TDD%SetDataValue(I, MissingData_REAL)
               Station_Removed = Station_Removed + 1
            END IF
         END IF
      END DO
      RETURN
 
 1001 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : CloudCover (', F0.2, '%) is < lower limit (', F0.2, '%). Removed.')
 1002 FORMAT(A, ': ', I4.4,'-',I2.2,'-',I2.2, ' : CloudCover (', F0.2, '%) is > upper limit (', F0.2, '%). Removed.')

      END SUBROUTINE CheckCloudCoverRange
      
!------------------------------------------------------------------------------
      SUBROUTINE SortStationFileList(FileNames)
      IMPLICIT NONE
      CHARACTER(LEN=*), DIMENSION(:), INTENT(INOUT) :: FileNames
      INTEGER :: I, J, Num
      CHARACTER(LEN=200) :: Temp

      Num = 0
      DO I = 1, UBOUND(FileNames,1)
         IF (LEN_TRIM(FileNames(I)) .GT. 0) Num = Num + 1
      END DO

      DO I = 1, Num-1
         DO J = I+1, Num
            IF (TRIM(FileNames(J)) .LT. TRIM(FileNames(I))) THEN
               Temp = FileNames(I)
               FileNames(I) = TRIM(FileNames(J))
               FileNames(J) = TRIM(Temp)
            END IF
         END DO
      END DO
      RETURN      
      
      END SUBROUTINE SortStationFileList

      
END MODULE StnData_RangeCheck




