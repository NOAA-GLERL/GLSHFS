!------------------------------------------------------------------
!  This module reads the files that control how AHPS is run. They
!  are used when running AHPS as a command-line app.
!------------------------------------------------------------------
MODULE GLSHFS_ConfigFile
      USE ErrorProcess
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GlerlDataTypesAndUnits

      PRIVATE
      PUBLIC :: Read_ConfigFile, Write_ConfigFile
      
      CHARACTER(LEN=4), DIMENSION(7), PARAMETER :: MetDetailStrings =    &
                (/'tmax', 'tmin', 'prec', 'airt', 'dewp', 'wspd', 'cldc'/)
      

CONTAINS
!-----------------------------------------------------------------------------------
!  The GLSHFS configuration file contains the specifications of stuff like the
!  directory for each lake, LBRM version, etc.
!-----------------------------------------------------------------------------------
      SUBROUTINE Read_ConfigFile(Filename, CfgData)
      IMPLICIT NONE
      CHARACTER(LEN=*),         INTENT(IN)    :: Filename
      TYPE (GLSHFS_ConfigData), INTENT(INOUT) :: CfgData
      
      INTEGER :: I, J, K, IOS, U1, Seq, NumStr
      LOGICAL :: DoSomething
      CHARACTER(LEN=50)   :: Item, CsvStrings(MaxScenarioCount+3)
      CHARACTER(LEN=1500) :: Line, S, StrVal

      !
      !  Initialize the entries
      !
      CALL InitializeConfig(CfgData)
      
      !
      U1 = -1
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)
      
      READ(U1, 1101, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         Line = TRIM(ADJUSTL(Line))
         CALL ParseCfgLine(Line, Item, StrVal); IF (ErrorLevel .NE. 0) Item = 'ignore_me'
         CALL LowerCase(Item)
        
         !
         IF (TRIM(Item) .EQ. 'cfgname') CfgData%ConfigName = TRIM(StrVal)
         
         !
         !  Directory specs
         !  These will all end with the file path separator character (/ or \)
         !
         IF (TRIM(Item) .EQ. 'basedir') CfgData%BaseDir = TRIM(NormalizedFilePath(TRIM(ADJUSTL(StrVal))))
         IF (TRIM(Item) .EQ. 'prgdir')  CfgData%PrgDir  = TRIM(NormalizedFilePath(TRIM(ADJUSTL(StrVal))))
         IF (TRIM(Item) .EQ. 'stndir')  CfgData%StnDir  = TRIM(NormalizedFilePath(TRIM(ADJUSTL(StrVal))))
         
         !
         !  Execution options
         !
         IF (TRIM(Item) .EQ. 'addstationdata')      CfgData%AddStationData        = TextToLogical(StrVal)
         IF (TRIM(Item) .EQ. 'buildsubbasinmet')    CfgData%BuildSubbasinMet      = TextToLogical(StrVal)
         IF (TRIM(Item) .EQ. 'updatehistorical')    CfgData%UpdateHistorical      = TextToLogical(StrVal)
         IF (TRIM(Item) .EQ. 'runforecasts')        CfgData%DoForecasts           = TextToLogical(StrVal)
         IF (TRIM(Item) .EQ. 'makeallsummaryfiles') CfgData%MakeSubbasinSummaries = TextToLogical(StrVal)
         IF (TRIM(Item) .EQ. 'cleartempfiles')      CfgData%ClearTemporaryForecastFiles = TextToLogical(StrVal)
         
         !
         !  Specification of detailed meteorology output.
         !  When building subbasin meteorology from station data, you can specify that the program
         !  output detailed met files. These files contain the station data for that day, the weighting 
         !  factors used, and also have a "map" of the polygons.
         !
         !  This output is primarily intended for debugging and/or verification of results, but I 
         !  also see it as potentially useful for production of presentation materials (plots, etc)
         !  or other similar purposes.
         !
         !  Each individual file isn't terribly large (a couple of MB), but it adds up. It also 
         !  slows down the process immensely (as we know -- I/O is slow). I initially set this up
         !  as a simple yes/no to create these files for everything. But after seeing the impact
         !  on processing time, as well as disk storage, I have made it more granular.
         !  To output nothing, the config file should have either:
         !     OutputMetDetails = []
         !   or have no entry for OutputMetDetails.
         !  To output the files for a particular data type, specify it by using the 4-character
         !  string for that data type.
         !  For example, to output the detailed files for precipitation and cloud cover, say:
         !     OutputMetDetails = [prec,cldc]
         !  The 4-character codes (case-insensitive) are:
         !     airtempman    = TMAX
         !     airtempmin    = TMIN
         !     precipitation = PREC
         !     airtempmean   = AIRT
         !     dewpoint      = DEWP
         !     windspeed     = WSPD
         !     cloudcover    = CLDC
         !
         IF (TRIM(Item) .EQ. 'outputmetdetails') THEN
            S = TRIM(StrVal)
            I = INDEX(S, '[')
            J = INDEX(S, ']')
            IF ((I .GT. 0) .AND. (J .GT. I)) THEN
               S = TRIM(S(I+1:J-1))
               CALL LowerCase(S)
               DO K = 1, 7
                  IF (INDEX(S, MetDetailStrings(K)) .GT. 0) THEN
                      CfgData%OutputMetDetails(K) = 'Y'
                  END IF
               END DO
            END IF
         END IF

         !
         !  Buffer distance around the basin (kilometers). Stations within this buffer 
         !  will be used to compute subbasin lumped meteorology. 
         !
         IF (TRIM(Item) .EQ. 'basinbuffer')  CfgData%BasinBoundaryBuffer = TextToReal(StrVal)
         
         !
         !  LBRM options
         !  Method to use in the LBRM for computation of the evapotranspiration heat index.
         !    '1982' = use the method developed by Tom Croley circa 1982
         !    '2016' = use method proposed by Brent Lofgren and implemented in 2016.
         !
         IF (TRIM(Item) .EQ. 'lbrmmethodet') CfgData%LbrmMethodET = TRIM(ADJUSTL(StrVal))
         
         !
         !  LLTM options
         !    LltmMethodRadiation____ specifies how to compute the Incident and Net 
         !    Longwave radiation values.
         !    1 = Read cloudcover and compute both Incident and NetLW.
         !        This is the historical method that allows us to use the data
         !        most readily available at station locations.
         !    2 = Read Incident and Longwave radiation directly. The Longwave radiation
         !        is interpreted as INCOMING longwave. Net longwave will be computed
         !        by LLTM.
         !        This is the option that we need to choose when using the
         !        output from CMIP models. The NetLW radiation that comes out of
         !        those models apparently does not properly account for the lake, 
         !        and the net value that is calculated and output is significantly
         !        wrong.  Seasonality is reversed, etc.
         !    3 = Read Incident and Longwave directly. Treat the longwave radiation
         !        as a correct computation of the net value and use both radiation 
         !        values as direct inputs.
         !        This will probably be used only when we are using output from
         !        some sort of coupled atmosphere-water model that correctly 
         !        computes the net LW value.
         !
         IF (TRIM(Item) .EQ. 'lltmmethodradiationhist') CfgData%LltmMethodRadiationhist = TextToInteger(StrVal)
         IF (TRIM(Item) .EQ. 'lltmmethodradiationfcst') CfgData%LltmMethodRadiationfcst = TextToInteger(StrVal)
         
         IF (TRIM(Item) .EQ. 'lltmapplydatacorrectionshist') CfgData%LltmApplyDataCorrectionsHist = TextToLogical(StrVal)
         IF (TRIM(Item) .EQ. 'lltmapplydatacorrectionsfcst') CfgData%LltmApplyDataCorrectionsFcst = TextToLogical(StrVal)        
         
         !
         !  Forecast options
         !
         IF (TRIM(Item) .EQ. 'forecaststart')  THEN
            Seq = MissingData_Date
            S = TRIM(ADJUSTL(StrVal))
            CALL Lowercase(S)
            IF (TRIM(S) .EQ. 'endofhistdata') THEN
               CALL DateSequence(8, 8, 8888, Seq)
            ELSE
               Seq = DateStringYMDToSeq(S)
               IF ((ErrorLevel .NE. 0) .OR. (Seq .EQ. MissingData_Date)) THEN
                  ErrorMessage = 'Error parsing the forecast start date.'; CALL PassMsg
                  ErrorMessage = 'Unable to interpret the string [' // TRIM(S) // '].'; CALL PassMsg
                  GOTO 898
               END IF
            END IF
            IF (Seq .EQ. MissingData_Date) THEN
               ErrorMessage = 'Invalid entry for ForecastStartDate. Value=['//TRIM(S)//']';     CALL PassMsg
               ErrorMessage = 'Must be either "EndOfHistData" or a valid date in Y-M-D format'; CALL PassMsg
               GOTO 898
            END IF
            CfgData%ForecastStartSeq = Seq
         END IF
         
         IF (TRIM(Item) .EQ. 'forecastlength')  CfgData%ForecastLen = TextToInteger(StrVal)
         IF (TRIM(Item) .EQ. 'forecastname')    CfgData%ForecastName = TRIM(ADJUSTL(StrVal))
         IF (TRIM(Item) .EQ. 'usermetlocation') CfgData%UserMetLocation = TRIM(ADJUSTL(StrVal))
         
         IF (TRIM(Item) .EQ. 'forecastmetsource') THEN
            S = GetLowercase(TRIM(ADJUSTL(StrVal)))
            IF (TRIM(S) .EQ. 'extractfromhist') CfgData%ForecastMetSource = 1
            IF (TRIM(S) .EQ. 'usersupplied')    CfgData%ForecastMetSource = 2
         END IF
         
         IF (TRIM(Item) .EQ. 'scenarionames') THEN
            S = TRIM(ADJUSTL(StrVal))
            CALL ParseCommaSepLine(S, CsvStrings, NumStr)
            ALLOCATE(CfgData%ScenarioNames(NumStr), STAT=IOS)
            IF (IOS .NE. 0) THEN
               ErrorMessage = 'Error allocating memory for forecast scenario names'; CALL PassMsg
               GOTO 898
            END IF
            DO I = 1, NumStr
               S  = TRIM(ADJUSTL(CsvStrings(I)))
               J = MIN(6, LEN_TRIM(S))
               CfgData%ScenarioNames(I) = S(1:J)
            END DO
         END IF
         
         !
         !  General options
         !   User-specified missing value string that will be used for all "transient"
         !   output files.  i.e. When a file is written that GLSHFS does not need to read
         !   in subsequent runs, this value will be used to denote a missing value.
         !
         IF (TRIM(Item) .EQ. 'missingvaluestring')  CfgData%UserMissingValueString = TRIM(StrVal)

         READ(U1, 1101, IOSTAT=IOS) Line
      END DO
      
      CLOSE(U1)
      CALL FileWasClosed(U1)

      !
      !  Do some validation checks
      !
      DoSomething = .FALSE.
      IF (CfgData%AddStationData)   DoSomething = .TRUE.
      IF (CfgData%BuildSubbasinMet) DoSomething = .TRUE.
      IF (CfgData%UpdateHistorical) DoSomething = .TRUE.
      IF (CfgData%DoForecasts)      DoSomething = .TRUE.
      IF (.NOT. DoSomething) GOTO 701

      IF (LEN_TRIM(CfgData%ConfigName)   .EQ. 0) GOTO 702
      IF (LEN_TRIM(CfgData%BaseDir)      .EQ. 0) GOTO 703
      IF (LEN_TRIM(CfgData%PrgDir)       .EQ. 0) GOTO 704

      IF (CfgData%AddStationData) THEN
         IF (LEN_TRIM(CfgData%StnDir)    .EQ. 0) GOTO 711
         IF (CfgData%BasinBoundaryBuffer .LT. 0) GOTO 712
      END IF

      IF (CfgData%DoForecasts) THEN
         IF (LEN_TRIM(CfgData%ForecastName) .EQ. 0)           GOTO 731
         IF (CfgData%ForecastStartSeq  .EQ. MissingData_Date) GOTO 732
         IF (CfgData%ForecastLen       .LE. 0)                GOTO 733
         IF (CfgData%ForecastMetSource .LT. 1)                GOTO 734
         IF (CfgData%ForecastMetSource .GT. 2)                GOTO 734
         IF (CfgData%ForecastMetSource .EQ. 2) THEN
            IF (.NOT. ALLOCATED(CfgData%ScenarioNames))       GOTO 735
            IF (LEN_TRIM(CfgData%UserMetLocation) .EQ. 0)     GOTO 705
         END IF
         
         !
         !  Replace any imbedded spaces with underscores
         !
         DO I = 1, LEN_TRIM(CfgData%ForecastName)
            IF (CfgData%ForecastName(I:I) .EQ. ' ') CfgData%ForecastName(I:I) = '_'
         END DO
      END IF      

      RETURN

      !
      !  Missing/invalid item error messages
      !
  701 ErrorMessage = 'No actions were specified in '  // TRIM(FileName);  CALL PassMsg;  GOTO 898

  702 ErrorMessage = 'Missing CfgName entry in '         // TRIM(FileName);  CALL PassMsg;  GOTO 898
  703 ErrorMessage = 'Missing BaseDir entry in '         // TRIM(FileName);  CALL PassMsg;  GOTO 898
  704 ErrorMessage = 'Missing PrgDir entry in '          // TRIM(FileName);  CALL PassMsg;  GOTO 898
  705 ErrorMessage = 'Missing UserMetLocation entry in ' // TRIM(FileName);  CALL PassMsg;  GOTO 898
  
  711 ErrorMessage = 'Missing StnDir entry in '                 // TRIM(FileName);  CALL PassMsg;  GOTO 898
  712 ErrorMessage = 'Missing or invalid BasinBuffer entry in ' // TRIM(FileName);  CALL PassMsg;  GOTO 898

  731 ErrorMessage = 'Missing ForecastName entry in '                 // TRIM(FileName);  CALL PassMsg;  GOTO 898
  732 ErrorMessage = 'Missing or invalid ForecastStart entry in '     // TRIM(FileName);  CALL PassMsg;  GOTO 898
  733 ErrorMessage = 'Missing or invalid ForecastLen entry in '       // TRIM(FileName);  CALL PassMsg;  GOTO 898
  734 ErrorMessage = 'Missing or invalid ForecastMetSource entry in ' // TRIM(FileName);  CALL PassMsg;  GOTO 898
  735 ErrorMessage = 'Missing or invalid ScenarioNames entry in '     // TRIM(FileName);  CALL PassMsg;  GOTO 898
      
      !
      !  Error Handling
      !
  811 ErrorMessage = 'Error opening file ' // TRIM(Filename);  CALL PassMsg
      GOTO 898
 

  898 ErrorLevel = 1 
      IF (U1 .GT. 0) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF
      ErrorMessage = '[traceback] Read_ConfigFile()...'; CALL PassMsg
      RETURN
      
 1101 FORMAT(A)

      END SUBROUTINE Read_ConfigFile
      
!----------------------------------------------------------------------------------
!     Write a valid command-line configuration file.
!----------------------------------------------------------------------------------
      SUBROUTINE Write_ConfigFile(Filename, CfgData)
      IMPLICIT NONE
      CHARACTER(LEN=*),         INTENT(IN)    :: Filename
      TYPE (GLSHFS_ConfigData), INTENT(INOUT) :: CfgData

      INTEGER :: U1, IOS, Dy, Mn, Yr, I
      CHARACTER(LEN=15) :: S
      CHARACTER(LEN=80) :: MDS
      
      U1 = -1
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)
      
      WRITE(U1, 1101, ERR=813) 'CfgName', TRIM(CfgData%ConfigName)
      WRITE(U1, 1101, ERR=813) 'BaseDir', TRIM(CfgData%BaseDir)
      WRITE(U1, 1101, ERR=813) 'PrgDir',  TRIM(CfgData%PrgDir)
      WRITE(U1, 1101, ERR=813) 'StnDir',  TRIM(CfgData%StnDir)

      WRITE(U1, 1101, ERR=813) 'AddStationData',      LogicalToText(CfgData%AddStationData)
      WRITE(U1, 1101, ERR=813) 'BuildSubbasinMet',    LogicalToText(CfgData%BuildSubbasinMet)
      WRITE(U1, 1101, ERR=813) 'UpdateHistorical',    LogicalToText(CfgData%UpdateHistorical)
      WRITE(U1, 1101, ERR=813) 'RunForecasts',        LogicalToText(CfgData%DoForecasts)
      WRITE(U1, 1101, ERR=813) 'MakeAllSummaryFiles', LogicalToText(CfgData%MakeSubbasinSummaries)
      WRITE(U1, 1101, ERR=813) 'ClearTempFiles',      LogicalToText(CfgData%ClearTemporaryForecastFiles)

      !
      MDS = '['
      DO I = 1, 7
         IF (CfgData%OutputMetDetails(I) .EQ. 'Y') THEN
            IF (LEN_TRIM(MDS) .EQ. 1) THEN
               MDS = TRIM(MDS) // MetDetailStrings(I)
            ELSE
               MDS = TRIM(MDS) // ',' // MetDetailStrings(I)
            END IF
         END IF
      END DO
      MDS = TRIM(MDS) // ']'
      WRITE(U1, 1101, ERR=813) 'OutputMetDetails', TRIM(MDS)
      
      IF (CfgData%ForecastStartSeq .EQ. MissingData_Date) THEN
         S = '9999-99-99'
      ELSE
         CALL SequenceDate(Dy, Mn, Yr, CfgData%ForecastStartSeq); IF (ErrorLevel .NE. 0) GOTO 898
         WRITE(S, 1110) Yr, Mn, Dy
      END IF
      WRITE(U1, 1101, ERR=813) 'ForecastStart(Y-M-D)',    TRIM(S)
      WRITE(U1, 1105, ERR=813) 'ForecastLength(months)',  CfgData%ForecastLen
      WRITE(U1, 1106, ERR=813) 'BasinBuffer(km)',         CfgData%BasinBoundaryBuffer
      WRITE(U1, 1101, ERR=813) 'MissingValueString',      TRIM(CfgData%UserMissingValueString)
      WRITE(U1, 1101, ERR=813) 'LbrmMethodET',            TRIM(CfgData%LbrmMethodET)
      
      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN

      !
      !  Error Handling
      !
  811 ErrorMessage = 'Error opening file ' // TRIM(Filename);  CALL PassMsg
      GOTO 898
  813 ErrorMessage = 'Error writing file ' // TRIM(Filename);  CALL PassMsg
      GOTO 898
 

  898 ErrorLevel = 1 
      IF (U1 .GT. 0) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF
      ErrorMessage = '[traceback] Write_ConfigFile()...'; CALL PassMsg
      RETURN
      
 1101 FORMAT(A, ' = ', A)
 1105 FORMAT(A, ' = ', I0)
 1106 FORMAT(A, ' = ', F0.3)
 1110 FORMAT(I4.4,'-',I2.2,'-',I2.2)
 
      END SUBROUTINE Write_ConfigFile


!----------------------------------------------------------------------------------
!     Initialize the configuration info.
!     Variables will be initialized to either missing or a default value that
!     the user can override in the config file.
!----------------------------------------------------------------------------------
      SUBROUTINE InitializeConfig(CfgData)
      IMPLICIT NONE
      TYPE (GLSHFS_ConfigData), INTENT(INOUT) :: CfgData

      INTEGER :: IOS
      
      !
      !  String variables
      !
      CfgData%ConfigName               = ''
      CfgData%BaseDir                  = ''
      CfgData%PrgDir                   = ''
      CfgData%StnDir                   = ''
      CfgData%UserMetLocation          = ''
      CfgData%LbrmMethodET             = ''
      CfgData%UserMissingValueString   = 'NA'
      CfgData%ForecastName             = ''
      IF (ALLOCATED(CfgData%ScenarioNames)) DEALLOCATE(CfgData%ScenarioNames, STAT=IOS)
      
      !
      !  Numeric variables
      !
      CfgData%ForecastStartSeq    = MissingData_Date
      CfgData%ForecastLen         = MissingData_Int
      CfgData%BasinBoundaryBuffer = MissingData_Real
      CfgData%ForecastMetSource   = MissingData_Int
      CfgData%LltmMethodRadiationHist  = -1
      CfgData%LltmMethodRadiationFcst  = -1
      
      !
      !  Boolean variables
      !
      CfgData%AddStationData               = .FALSE.
      CfgData%BuildSubbasinMet             = .FALSE.
      CfgData%UpdateHistorical             = .FALSE.
      CfgData%DoForecasts                  = .FALSE.
      CfgData%MakeSubbasinSummaries        = .TRUE.
      CfgData%ClearTemporaryForecastFiles  = .TRUE.
      CfgData%LltmApplyDataCorrectionsHist = .FALSE.
      CfgData%LltmApplyDataCorrectionsFcst = .FALSE.
      
      !
      !  Specification for detailed met output
      !  By default, detailed output is OFF for all data types
      !
      CfgData%OutputMetDetails(:) = 'N'
      
      END SUBROUTINE InitializeConfig
      
END MODULE GLSHFS_ConfigFile
