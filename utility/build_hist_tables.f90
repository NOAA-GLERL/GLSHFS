      PROGRAM BuildHistTables
      USE GLSHFS_Util
      
      IMPLICIT  NONE

      INTEGER :: Err, Hndl, I, IOS, TL, R, ReqVals
      CHARACTER (LEN=3)  :: Lake3
      CHARACTER (LEN=10) :: TimeInt, LevMix, QuietStr
      CHARACTER (LEN=20) :: File11
      CHARACTER (LEN=80) :: CmdLine, S, S2
      CHARACTER (LEN=20), DIMENSION(5) :: ParmStrings

      !
      !  Process command-line arguments
      !
      !
      !  Process the command line.
      !  User should have specified the controlling configuration file.
      !  If not, then we cannot proceed.
      !  Remember that CmdLine includes the program name, so the config file
      !  name should be entry 2.
      !
      CALL GET_COMMAND(CmdLine)
      CALL ParseCmdLine(CmdLine, ArgStrings, ArgCount); IF (ErrorLevel .NE. 0) GOTO 899
      IF (ArgCount .LT. 3) GOTO 850

      CALL SetTimeInterval('x')
      CALL SetBasin('xxx')
      CALL SetUnits('xxx')
      CALL SetQuiet('xxx')
      
      DO I = 2, ArgCount
         S = GetLowercase(ArgStrings(I))
         IF (TRIM(S) .EQ. '--help') GOTO 850
         IF (TRIM(S) .EQ. '-help')  GOTO 850
         IF (TRIM(S) .EQ. '/?')     GOTO 850
         IF (TRIM(S) .EQ. '?')      GOTO 850

         ArgProcessed = .FALSE.
         IF (TRIM(S) .EQ. 'annual')     CALL SetTimeInterval('Y')
         IF (TRIM(S) .EQ. 'year')       CALL SetTimeInterval('Y')
         IF (TRIM(S) .EQ. 'yearly')     CALL SetTimeInterval('Y')
         IF (TRIM(S) .EQ. 'month')      CALL SetTimeInterval('M')
         IF (TRIM(S) .EQ. 'monthly')    CALL SetTimeInterval('M')
         IF (TRIM(S) .EQ. 'quarter')    CALL SetTimeInterval('Q')
         IF (TRIM(S) .EQ. 'qtrmonth')   CALL SetTimeInterval('Q')
         IF (TRIM(S) .EQ. 'qtrmonthly') CALL SetTimeInterval('Q')
         IF (TRIM(S) .EQ. 'week')       CALL SetTimeInterval('W')
         IF (TRIM(S) .EQ. 'weekly')     CALL SetTimeInterval('W')
         IF (TRIM(S) .EQ. 'day')        CALL SetTimeInterval('D')
         IF (TRIM(S) .EQ. 'daily')      CALL SetTimeInterval('D')
         
         IF (TRIM(S) .EQ. 'sup')        CALL SetBasin('SUP')
         IF (TRIM(S) .EQ. 'mic')        CALL SetBasin('MIC')
         IF (TRIM(S) .EQ. 'hur')        CALL SetBasin('HUR')
         IF (TRIM(S) .EQ. 'geo')        CALL SetBasin('GEO')
         IF (TRIM(S) .EQ. 'stc')        CALL SetBasin('STC')
         IF (TRIM(S) .EQ. 'eri')        CALL SetBasin('ERI')
         IF (TRIM(S) .EQ. 'ont')        CALL SetBasin('ONT')
         IF (TRIM(S) .EQ. 'superior')   CALL SetBasin('SUP')
         IF (TRIM(S) .EQ. 'michigan')   CALL SetBasin('MIC')
         IF (TRIM(S) .EQ. 'huron')      CALL SetBasin('HUR')
         IF (TRIM(S) .EQ. 'georgian')   CALL SetBasin('GEO')
         IF (TRIM(S) .EQ. 'stclair')    CALL SetBasin('STC')
         IF (TRIM(S) .EQ. 'erie')       CALL SetBasin('ERI')
         IF (TRIM(S) .EQ. 'ontario')    CALL SetBasin('ONT')
         
         IF (TRIM(S) .EQ. 'cms')        CALL SetUnits('cms')
         
         IF (TRIM(S) .EQ. 'quiet')      CALL SetQuiet('yes')
         
         IF (.NOT. ArgProcessed) THEN
            ErrorMessage = 'Command line argument [' // TRIM(S) // '] not recognized'
            CALL PassMsg
            GOTO 850
         END IF
      END DO
      
!
!     Default number of values to request
!
      ReqVals = 0    ! program will adjust to default

!
!     Did the user request a specific number of values?
!
      TL = LEN(S)
      I = 1
      DO WHILE (I .LT. TL)
         S2 = S(I:TL)
         IF ((S2(2:2) .GE. '0') .AND. (S2(2:2) .LE. '9')) THEN
            IF (S2(1:1) .EQ. ' ') THEN    ! if start of number
               READ(S2, *, IOSTAT=IOS) R
               IF (IOS .EQ. 0) THEN
                  ReqVals = R
                  I = TL     ! force exit
               END IF
            END IF
         END IF
         I = I + 1
      END DO


      CALL DO_PROVHIST(Err, Hndl, ReqVals, ParmStrings)
      GOTO 999

!
  811 ErrorMessage = 'POUTLOOK: Error opening file ' // TRIM(File11)
      CALL PassMsg; GOTO 898
  812 ErrorMessage = 'POUTLOOK: Error reading file ' // TRIM(File11)
      CALL PassMsg; GOTO 898
      
  850 ErrorMessage = 'USAGE build_hist_tables <lake> <interval> <cms> <quiet>';    CALL PassMsg
      ErrorMessage = '  lake     = 3-character lake name';                         CALL PassMsg
      ErrorMessage = '             sup, mic, hur, geo, stc, eri, or ont';          CALL PassMsg
      ErrorMessage = '  interval = aggregation interval - one of the following:';  CALL PassMsg
      ErrorMessage = '             annual     or yearly   or  year or';            CALL PassMsg
      ErrorMessage = '             monthly    or month    or ';                    CALL PassMsg
      ErrorMessage = '             qtrmonthly or qtrmonth or ';                    CALL PassMsg
      ErrorMessage = '             weekly     or week     or ';                    CALL PassMsg
      ErrorMessage = '             daily      or day         ';                    CALL PassMsg
      ErrorMessage = '  cms      = 3-character string that flags the NBS output';  CALL PassMsg
      ErrorMessage = '             to be expressed in cubic meters/second rather'; CALL PassMsg
      ErrorMessage = '             than millimeters over the lake surface';        CALL PassMsg
      ErrorMessage = '  quiet    = flag that suppresses output messages';          CALL PassMsg
      

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : BuildHistTables main...'
      CALL PassMsg
      GOTO 999

!  -----------------------
  999 CALL CloseAllFiles

 1101 FORMAT(A3)

      END PROGRAM BuildHistTables


      
