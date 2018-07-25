!=====================================================================================================
!=====================================================================================================
!=====================================================================================================
MODULE MyKinds
      INTEGER, PARAMETER :: INTEGER1 = SELECTED_INT_KIND(R=1)           ! 1-byte
      INTEGER, PARAMETER :: INTEGER2 = SELECTED_INT_KIND(R=4)           ! 2-byte
      INTEGER, PARAMETER :: INTEGER4 = SELECTED_INT_KIND(R=8)           ! 4-byte
      INTEGER, PARAMETER :: INTEGER8 = SELECTED_INT_KIND(R=12)          ! 8-byte
      INTEGER, PARAMETER :: REAL4    = SELECTED_REAL_KIND(P=6,R=37)     ! 4-byte
      INTEGER, PARAMETER :: REAL8    = SELECTED_REAL_KIND(P=15,R=100)   ! 8-byte
END MODULE MyKinds

!=====================================================================================================
!=====================================================================================================
!=====================================================================================================
MODULE ErrorProcess
      INTEGER  :: ErrorLevel
      CHARACTER (LEN=255) :: ErrorMessage
CONTAINS
!-----------------------------------------------------------------------------
      SUBROUTINE PassMsg
      PRINT *, TRIM(ErrorMessage) 
      END SUBROUTINE PassMsg

!-----------------------------------------------------------------------------
!  Write the message to standard output.
!  If LogFileUnit is valid, also write to the file.
!  
      SUBROUTINE WriteErrorMessage(Message, LogFileUnit)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: Message
      INTEGER,          INTENT(IN) :: LogFileUnit

      INTEGER :: IOS
      LOGICAL :: IsOpen
      
      ErrorMessage = TRIM(Message);  CALL PassMsg
      
      INQUIRE(UNIT=LogFileUnit, OPENED=IsOpen)
      IF (IsOpen) WRITE(LogFileUnit, *, IOSTAT=IOS) TRIM(Message)

      END SUBROUTINE WriteErrorMessage
      
END MODULE ErrorProcess

!=====================================================================================================
!=====================================================================================================
!=====================================================================================================
!  Status will be used only where we intend to write a bunch of lines that should overwrite the
!  previous output. For example, if we want to tell which station we are processing, with a
!  regular print it might like like:
!    Now processing station 1
!    Now processing station 2
!    Now processing station 3
!    Now processing station 4
!    Now processing station 5
!    Now processing station 6
!    Now processing station 7
!
!  But by using this status message stuff, we can make lines 2-7 write on the same line as line 1.
!  
!  Note that there is no facility (in any programming language) to make console output
!  do what we actually want. Ideally, we would be able to say something like "move back up 
!  one line, then output this string starting in column 1."  But there is no way to do the
!  "move back up one line" part.  Instead, we have to anticipate that we might want the
!  next line of output to overwrite THIS one, and prevent the console output from advancing 
!  to the next line.
!  That means if some other write/print statement gets used while in the middle of doing a 
!  block of these, that message will end up appending to the end of the line just written 
!  this way. That includes error messages from my code or the O/S. It will end up being a 
!  bit messy-looking. But there is no operating-system independent way to move UP one line 
!  and write over the top of the line previously written. If we could do that, it would 
!  make this SOOO much easier.
!=====================================================================================================
!
MODULE StatusMessages
      CHARACTER (LEN=255) :: StatusMsg
CONTAINS
!-----------------------------------------------------------------------------
!  The optional parameter StayOnThisLine is set to true when we want to stay
!  on the same line in anticipation of the next console output overwriting
!  or appending to the current line. If the parameter is not provided, 
!  or is set to .FALSE., then we treat the line normally.
!
!  If GoToStart is present and TRUE, then this output is intended to overwrite the 
!  current line.  We start with a carriage return that forces a shift back to column 1. 
!  If it is not present, or is FALSE, then we simply append this text to what was 
!  previously written.
!
      SUBROUTINE WriteMsg(StayOnThisLine, GoToStart)
      IMPLICIT NONE
      LOGICAL, OPTIONAL, INTENT(IN) :: StayOnThisLine, GoToStart
      LOGICAL :: DoAdvance, AddCR
      CHARACTER (LEN=1), PARAMETER :: CR = CHAR(13)
      CHARACTER(LEN=256) :: ThisMsg

      !
      !  Default behavior is to act like a normal write.
      !  We will advance to the next line after writing this message and
      !  we do not need to prepend a carriage return forcing us to column 1.
      !
      DoAdvance = .TRUE.
      AddCR     = .FALSE.
      
      !
      ! If user said to stay on this line, then we change the behavior
      !
      IF (PRESENT(StayOnThisLine)) THEN
         IF (StayOnThisLine) DoAdvance = .FALSE.
      ENDIF
         
      !
      ! If user said to start at column 1, change that behavior setting
      !
      !
      IF (PRESENT(GoToStart)) THEN
         IF (GoToStart) AddCR = .TRUE.
      ENDIF

      !
      !  Write the message with the assigned settings
      !
      IF (AddCR) THEN
         ThisMsg = CR // TRIM(StatusMsg)
      ELSE
         ThisMsg = TRIM(StatusMsg)
      END IF

      IF (DoAdvance) THEN
         WRITE(*, 1000, ADVANCE='YES') TRIM(ThisMsg)
      ELSE
         WRITE(*, 1000, ADVANCE='NO')  TRIM(ThisMsg)
      END IF
      
  1000 FORMAT(A)
      END SUBROUTINE WriteMsg

!-----------------------------------------------------------------------------
!  Write the message to standard output. Using this routine will
!  force normal operation, where advancing is done for each line.
!  If LogFileUnit is valid, also write to the file.
!  
      SUBROUTINE WriteStatusMessage(Message, LogFileUnit)
      IMPLICIT NONE
      CHARACTER(LEN=*),  INTENT(IN) :: Message
      INTEGER, OPTIONAL, INTENT(IN) :: LogFileUnit
      INTEGER :: IOS
      LOGICAL :: IsOpen
      
      IF (LEN_TRIM(Message) .LT. 1) RETURN
     
      StatusMsg = TRIM(Message);   CALL WriteMsg()
      
      IF (PRESENT(LogFileUnit)) THEN
         IF (LogFileUnit .GT. 0) THEN
            INQUIRE(UNIT=LogFileUnit, OPENED=IsOpen)
            IF (IsOpen) WRITE(LogFileUnit, *, IOSTAT=IOS) TRIM(StatusMsg)
         END IF
      END IF

      END SUBROUTINE WriteStatusMessage
      
END MODULE StatusMessages

!=====================================================================================================
!=====================================================================================================
!=====================================================================================================
MODULE GLSHFS_Util
      USE MyKinds
      USE ErrorProcess
      USE StatusMessages
      USE, intrinsic :: iso_c_binding, only : C_CHAR, C_INT
     
      !
      !  Define the OS-specific file path separator character
      !  Make sure to compile this file so these directives are handled correctly.
      !
#if defined(__windows__)
      CHARACTER(LEN=1), PARAMETER :: FilePathSeparator = '\'
#else
      CHARACTER(LEN=1), PARAMETER :: FilePathSeparator = '/'
#endif

      !
      !  Define some useful constant values
      !
      INTEGER, PARAMETER :: Int32_MaxValue        =  2147483647    ! 4-byte integer
      INTEGER, PARAMETER :: Int32_MinValue        = -2147483647
      INTEGER, PARAMETER :: Int16_MaxValue        =  65535         ! 2-byte integer
      INTEGER, PARAMETER :: Int16_MinValue        = -65535
      INTEGER, PARAMETER :: DateSeq_MaxValue      = 3652365        ! 12/31/9999 with standard DateSequence routine
      INTEGER, PARAMETER :: DateSeq_MinValue      = 307            !  1/ 1/   1 with standard DateSequence routine
      
      INTEGER, PARAMETER :: MissingData_Int       = -2147483647    ! Int32_MinValue + 1
      REAL,    PARAMETER :: MissingData_Real      = -9.9e29
      REAL,    PARAMETER :: MissingData_Real_Test = -9.5e29        ! in IF tests, test for values > this one to avoid roundoff issues
      INTEGER, PARAMETER :: MissingData_Date      = 3652365        ! 12/31/9999 with standard DateSequence routine

      !
      ! This is a list of the open files (unit numbers) at any given time in
      ! the program execution.  An entry of -1 indicates the end of the list.
      ! It's used to ensure that all files are closed whenever we exit this
      ! program for any reason.  It is especially useful if a CANCEL condition
      ! is encountered.  The CANCEL condition is initiated by the Windows GUI
      ! and results in the existence of a file called CANCEL.INT.  The program
      ! can periodically test for the existence of that file and exit gracefully
      ! if it is there.
      !
      INTEGER              :: OpenedFiles = 0
      INTEGER, PARAMETER   :: MaxOpenedFiles = 60
      INTEGER, DIMENSION(MaxOpenedFiles)  :: OpenedFileUnits

      !
      ! This is the definition of a quarter-month based on the
      ! definitions received from USACE - Buffalo.
      ! The values in each array are the end day of the qtr-months when
      ! the month has the specified number of days.  i.e. when the month
      ! has 30 days then the quarter-months end on days 8, 15, 23, 30.
      !
      INTEGER, PARAMETER, DIMENSION(4) :: Q28 = (/7, 14, 21, 28/)
      INTEGER, PARAMETER, DIMENSION(4) :: Q29 = (/7, 14, 21, 29/)
      INTEGER, PARAMETER, DIMENSION(4) :: Q30 = (/8, 15, 23, 30/)
      INTEGER, PARAMETER, DIMENSION(4) :: Q31 = (/8, 15, 23, 31/)


      CHARACTER(LEN=1), PARAMETER, DIMENSION(12) :: MonCode1 =               &
                     (/'J','F','M','A','M','J','J','A','S','O','N','D'/)

      CHARACTER(LEN=3), PARAMETER, DIMENSION(12) :: MonCode3 =               &
              (/'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',                   &
                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/)


             
!-------------------------------------------------------------------------
 
      PUBLIC :: DayOfYear
      PRIVATE :: DayOfYearFromSeq, DayOfYearFromDMY
      INTERFACE DayOfYear
         MODULE PROCEDURE DayOfYearFromSeq, DayOfYearFromDMY
      END INTERFACE DayOfYear

      PUBLIC :: FirstDayOfQtrMon
      PRIVATE :: StartOfQtr1, StartOfQtr2, StartOfQtr3
      INTERFACE FirstDayOfQtrMon
         MODULE PROCEDURE StartOfQtr1, StartOfQtr2, StartOfQtr3
      END INTERFACE FirstDayOfQtrMon

      PUBLIC :: LastDayOfQtrMon
      PRIVATE :: EndOfQtr1, EndOfQtr2, EndOfQtr3
      INTERFACE LastDayOfQtrMon
         MODULE PROCEDURE EndOfQtr1, EndOfQtr2, EndOfQtr3
      END INTERFACE LastDayOfQtrMon

!--------------------------------------------------------------------------
      !
      !  Define interface to C++ routines that will perform functions not
      !  directly available in Fortran.
      !
      INTERFACE
         SUBROUTINE makefilelistC(m, fspec, fname) BIND(C, NAME="makefilelist")
            use iso_c_binding
            INTEGER (KIND=c_int)    :: m
            CHARACTER (KIND=c_char) :: fspec, fname
         END SUBROUTINE makefilelistC
      END INTERFACE

      INTERFACE
         SUBROUTINE file_info(filename, mode, fexist, time) BIND(C,name="file_info")
            USE iso_c_binding
            CHARACTER(kind=C_CHAR),INTENT(in)  :: filename(*)
            INTEGER(C_INT),        INTENT(out) :: mode, fexist, time
         END SUBROUTINE
      END INTERFACE
      
      INTERFACE
         SUBROUTINE file_is_directory(fname, isdir) BIND(C, NAME="file_is_directory")
            USE iso_c_binding
            CHARACTER (KIND=C_CHAR) :: fname
            INTEGER (KIND=C_INT)    :: isdir
         END SUBROUTINE file_is_directory
      END INTERFACE
      
      INTERFACE
         SUBROUTINE create_directory(dirname) BIND(C, NAME="create_directory")
            USE iso_c_binding
            CHARACTER (KIND=C_CHAR) :: dirname
         END SUBROUTINE create_directory
      END INTERFACE
      
CONTAINS

!-------------------------------------------------------------
      SUBROUTINE GetTheCurrentDirectory(CWD)
      IMPLICIT NONE
      CHARACTER(LEN=200), INTENT(INOUT) :: CWD

      !
      !  Define interface to a C++ routine that will get the current directory.
      !
      INTERFACE
         SUBROUTINE getWorkingDirectory(d) BIND(C, NAME="getWorkingDirectory")
            use iso_c_binding, only: c_char
            CHARACTER (KIND=c_char), INTENT(OUT) :: d(*)
         END SUBROUTINE getWorkingDirectory
      END INTERFACE
      CHARACTER(LEN=200) :: d
      
      d = ''
      CALL getWorkingDirectory(d)
      CWD = TRIM(d)

      END SUBROUTINE GetTheCurrentDirectory

!-------------------------------------------------------------
      SUBROUTINE GetEnvironmentVariableValue(EVName, EVValue)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: EVName
      CHARACTER(LEN=*), INTENT(INOUT) :: EVValue
      
      !
      !  Define interface to a C++ routine that will get the current directory.
      !
      INTERFACE
         SUBROUTINE getEnvironmentVariable(n,v) BIND(C, NAME="getEnvironmentVariable")
            use iso_c_binding, only: c_char
            CHARACTER (KIND=c_char), INTENT(OUT) :: n(*), v(*)
         END SUBROUTINE getEnvironmentVariable
      END INTERFACE
      INTEGER :: I
      CHARACTER(LEN=500) :: en, ev
      
      en = TRIM(EVName) // CHAR(0)
      ev = CHAR(0)
      CALL getEnvironmentVariable(en, ev)
      EVValue = ''
      I = INDEX(ev, CHAR(0))
      IF (I .GE. 1) EVValue = ev(1:I-1)

      END SUBROUTINE GetEnvironmentVariableValue

!-------------------------------------------------------------
      SUBROUTINE BuildDirFileList(ListFileName, FileSpec, LFormat)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: ListFileName, FileSpec
      INTEGER,           INTENT(IN) :: LFormat
      
      INTEGER (KIND=c_int) :: Mode
      CHARACTER(LEN=150) :: FNameC, FSpecC

      FNameC = TRIM(ListFileName) // CHAR(0)
      FSpecC = TRIM(FileSpec) // CHAR(0)
      Mode   = LFormat                   ! 1=bare, 2=with date&time, 3=with date/time and size
      
      CALL makefilelistC(Mode, FSpecC, FNameC)
      
      END SUBROUTINE BuildDirFileList

!-------------------------------------------------------------
      SUBROUTINE BuildListOfSubdirectories(SrcDir, ListFile)
      IMPLICIT   NONE
      CHARACTER(LEN=*), INTENT(IN) :: SrcDir, ListFile
      INTEGER :: U1, U2, IOS
      INTEGER (KIND=c_int) :: IsDir
      LOGICAL :: Skip
      CHARACTER(LEN=200) :: FName, TmpFile, FNameC, FSpecC, TheSrcDir
      
      !
      !  Use the directory name passed in to build a normalized one
      !
      TheSrcDir = NormalizedFilePath(SrcDir)
      
      !
      !  First make a temporary file that just has a list of everything
      !
      TmpFile = TRIM(TheSrcDir) // 'zzz_temporary_file_list.txt'
      FNameC = TRIM(TmpFile) // CHAR(0)
      FSpecC = TRIM(TheSrcDir) // '*' // CHAR(0)
      CALL makefilelistC(1, FSpecC, FNameC)

      !
      !  Now read that temporary file and test each entry for being a directory
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(TmpFile), STATUS='OLD')
      CALL FileWasOpened(U1)
      
      U2 = GetFreeUnitNumber()
      OPEN(UNIT=U2, FILE=TRIM(ListFile), STATUS='REPLACE')
      CALL FileWasOpened(U2)
      
      READ(U1, 1000, IOSTAT=IOS) FName
      DO WHILE (IOS .EQ. 0)
         Skip = .FALSE.
         IF (TRIM(FName) .EQ. '.')  Skip = .TRUE.
         IF (TRIM(FName) .EQ. '..') Skip = .TRUE.
         IF (.NOT. Skip) THEN
            FNameC = TRIM(TheSrcDir) // TRIM(FName) // CHAR(0)
            CALL file_is_directory(FNameC, IsDir)
            IF (IsDir .EQ. 1) WRITE(U2, 1001) TRIM(ADJUSTL(FName))
         END IF
         READ(U1, 1000, IOSTAT=IOS) FName
      END DO

      CLOSE(U2)
      CALL FileWasClosed(U2)
      CLOSE(U1, STATUS='DELETE')
      CALL FileWasClosed(U1)
      
 1000 FORMAT(A200)
 1001 FORMAT(A)
      
      END SUBROUTINE BuildListOfSubDirectories

!-------------------------------------------------------------
      SUBROUTINE CreateDirectory(Dir)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: Dir
      CHARACTER(LEN=250) :: dirnameC
      dirnameC = TRIM(Dir) // CHAR(0)
      CALL create_directory(dirnameC)
      END SUBROUTINE CreateDirectory

!--------------------------------------------------------------------------
!  Note about the allocatable string variables LS and RS....
!  You will notice that I never actually use ALLOCATE or DEALLOCATE.  This is by design.
!  Per online postings by the real Fortran gurus (e.g. Steve Lionel at Intel) the
!  variables will be automatically allocated when assigned to, and automatically
!  deallocated at the conclusion of the routine (because they are local, non_SAVE variables).
!  I am trusting that info to be valid.   
!  Sidenote is that the compiler spits out a message warning of possible uninitialized
!  value for each variable. But the code DOES work. Message is annoying and I wish I
!  could suppress it, but it is what it is.
!     -Tim Hunter
!  
      SUBROUTINE ParseCfgLine(Line, Item, String)
      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(INOUT) :: Line, Item, String

      INTEGER :: I
      CHARACTER(LEN=:), ALLOCATABLE :: LS, RS

      !
      !  Find the first = sign, if there is one
      !
      I = INDEX(Line, '=')
      IF (I .EQ. 0) THEN
         Item = ''
         String = ''
         RETURN
      END IF
      !
      !  Split the line into the left and right sides of that = sign
      !
      ALLOCATE(character(len=I) :: LS)
      ALLOCATE(character(len=LEN_TRIM(Line)-I) :: RS)
      LS = Line(1:I-1)
      RS = Line(I+1:LEN_TRIM(Line)) 
      !
      !  Trim leading and trailing blanks from each
      !
      LS = ADJUSTL(LS)
      LS = TRIM(LS)
      RS = ADJUSTL(RS)
      RS = TRIM(RS)
      !
      !  Assign to arguments passed back, ensuring that we don't 
      !  overrun the end of those strings.
      !
      I = LEN(LS)
      IF (I .GT. LEN(Item)) I = LEN(Item)
      Item = LS(1:I)
      I = LEN(RS)
      IF (I .GT. LEN(String)) I = LEN(String)
      String = RS(1:I)
      END SUBROUTINE ParseCfgLine

!--------------------------------------------------------------------------
      SUBROUTINE ParseCmdLine(Line, Args, ArgC)
      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(IN)  :: Line
      CHARACTER (LEN=*), INTENT(OUT) :: Args(:)
      INTEGER,           INTENT(OUT) :: ArgC

      INTEGER :: UB, TL, CP, LS, RS
      LOGICAL :: Found

      UB = UBOUND(Args,1)
      Args(:) = ''

      !
      !  Total length of the line
      !
      TL = LEN_TRIM(Line)     ! Trimmed Length
      ArgC = 0                ! # of arguments found
      CP = 1                  ! Current Position in Line
      DO WHILE (CP .LE. TL)
         Found = .FALSE.
         DO WHILE (.NOT. Found)
            IF ((Line(CP:CP) .NE. ' ') .OR. (CP .EQ. TL)) Found = .TRUE.
            IF (.NOT. Found) CP = CP +1
         ENDDO
         IF (CP .LE. TL) THEN
            LS = CP                           ! Left Side of argument
            Found = .FALSE.
            DO WHILE (.NOT. Found)
               IF (CP .GE. TL) THEN
                  RS = TL                     ! Right Side of argument
                  Found = .TRUE.
                  CP = TL + 1                 ! Force termination of main loop
               ELSE
                  CP = CP + 1
                  IF (Line(CP:CP) .EQ. ' ') THEN
                     RS = CP - 1              ! Right Side of argument
                     Found = .TRUE.
                  END IF
               END IF
            ENDDO
            ArgC = ArgC + 1
            IF (ArgC .LE. UB) Args(ArgC) = Line(LS:RS)
         ENDIF
      ENDDO
      END SUBROUTINE ParseCmdLine

!------------------------------------------------------------------
      SUBROUTINE ParseCmdLineOneArg(CLine, PNum, Param)
      IMPLICIT   NONE
      INTEGER, INTENT(IN)            :: PNum
      CHARACTER (LEN=*), INTENT(IN)  :: CLine
      CHARACTER (LEN=*), INTENT(OUT) :: Param

      INTEGER  ::  I, J, St, En, Lngth

      Param = ' '
      Lngth = LEN_TRIM(CLine)

      !
      !  Find the start of parameter number "PNum", if we can
      !
      I = 0
      DO J = 1, PNum
        I = I + 1
        DO WHILE ((I .LE. Lngth) .AND. (CLine(I:I) .EQ. ' '))
          I = I + 1
        END DO
        IF (I .GT. Lngth) RETURN
        IF (J .NE. PNum) THEN
          DO WHILE ((I .LT. Lngth) .AND. (CLine(I:I) .NE. ' '))
            I = I + 1
          END DO
        END IF
      END DO
      St = I

      !
      !  Find the end of that parameter
      !
      DO WHILE ((I .LT. Lngth) .AND. (CLine(I:I) .NE. ' '))
         I = I + 1
      END DO
      IF (CLine(I:I) .EQ. ' ') I = I-1      ! back up to the last valid char
      En = I

      !
      !  Assign value to Param
      !
      J = En - St + 1
      Param(1:J) = CLine(St:En) // ' '

      END SUBROUTINE ParseCmdLineOneArg

!--------------------------------------------------------------------------
      SUBROUTINE ParseTabDelimitedLine(Line, Args, ArgC)
      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(IN)  :: Line
      CHARACTER (LEN=*), INTENT(OUT) :: Args(:)
      INTEGER,           INTENT(OUT) :: ArgC

      INTEGER :: UB, TL, CP, LS, RS
      CHARACTER(LEN=1), PARAMETER :: Tab = CHAR(9)
      CHARACTER(LEN=1), PARAMETER :: Blank = CHAR(32)

      UB = UBOUND(Args,1)

      !
      !  Total length of the line
      !
      TL = LEN_TRIM(Line)     ! Total Length of line
      ArgC = 0                ! # of arguments found
      CP = 1                  ! Current position in Line
      DO WHILE (CP .LE. TL)
         !
         !  Skip any leading blanks
         !
         DO WHILE ((Line(CP:CP) .EQ. Blank) .AND. (CP .LE. TL))
            CP = CP +1
         ENDDO
         
         !
         !  Construct the string
         !
         IF (CP .LE. TL) THEN
            LS = CP        ! Left side of string
            RS = CP
            DO WHILE ((Line(CP:CP) .NE. Tab) .AND. (CP .LE. TL))
               RS = CP
               CP = CP + 1
            ENDDO
            ArgC = ArgC + 1
            IF (ArgC .LE. UB) Args(ArgC) = Line(LS:RS)
            CP = CP + 1
            DO WHILE ((Line(CP:CP) .EQ. Blank) .AND. (CP .LE. TL))
               CP = CP +1
            ENDDO
         ENDIF
      ENDDO
      RETURN
      END SUBROUTINE ParseTabDelimitedLine

!--------------------------------------------------------------------------
      SUBROUTINE ParseDelimitedLine(Line, Delimiter, Args, ArgC)
      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(IN)  :: Line
      CHARACTER (LEN=1), INTENT(IN)  :: Delimiter
      CHARACTER (LEN=*), INTENT(OUT) :: Args(:)
      INTEGER,           INTENT(OUT) :: ArgC

      INTEGER :: UB, TL, CP, LS, RS
      CHARACTER(LEN=1), PARAMETER :: Blank = CHAR(32)

      UB = UBOUND(Args,1)

      !
      !  Total length of the line
      !
      TL = LEN_TRIM(Line)     ! Total Length of line
      ArgC = 0                ! # of arguments found
      CP = 1                  ! Current position in Line
      DO WHILE (CP .LE. TL)
         !
         !  Skip any leading blanks
         !
         DO WHILE ((Line(CP:CP) .EQ. Blank) .AND. (CP .LE. TL))
            CP = CP +1
         ENDDO
         
         !
         !  Construct the string
         !
         IF (CP .LE. TL) THEN
            LS = CP        ! Left Side of argument
            RS = CP
            DO WHILE ((Line(CP:CP) .NE. Delimiter) .AND. (CP .LE. TL))
               RS = CP
               CP = CP + 1
            ENDDO
            ArgC = ArgC + 1
            IF (ArgC .LE. UB) Args(ArgC) = Line(LS:RS)
            CP = CP + 1
            DO WHILE ((Line(CP:CP) .EQ. Blank) .AND. (CP .LE. TL))
               CP = CP +1
            ENDDO
         ENDIF
      ENDDO
      RETURN
      END SUBROUTINE ParseDelimitedLine

!--------------------------------------------------------------------------
!     Parse a comma-separated line into tokens. Within each token, any imbedded 
!     spaces are removed.  e.g. 'This is a token' becomes 'Thisisatoken'.
!     Double-quotes around a string will be honored, and the double-quote
!     characters will be retained or stripped, based on the parameter value.
!--------------------------------------------------------------------------
      SUBROUTINE ParseCommaSepLineToTokens(Line, Strings, StripDoubleQuotes, ValidNum)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: Line
      CHARACTER(LEN=*), DIMENSION(:), INTENT(INOUT) :: Strings
      LOGICAL, OPTIONAL, INTENT(IN)  :: StripDoubleQuotes
      INTEGER, OPTIONAL, INTENT(OUT) :: ValidNum

      INTEGER :: MaxStrings, SNum, LineLen, StrLen
      INTEGER :: I, J, K, TokenNum
      LOGICAL :: InToken, KillQuotes
      CHARACTER(LEN=500) :: TStr
      CHARACTER(LEN=500), DIMENSION(:), ALLOCATABLE :: StringTokens
      CHARACTER(LEN=5000) :: TokenizedLine

      IF (PRESENT(ValidNum)) ValidNum = 0
      IF (PRESENT(StripDoubleQuotes)) THEN
         KillQuotes = StripDoubleQuotes
      ELSE
         KillQuotes = .TRUE.    ! default behavior is to remove double quotes
      END IF
      LineLen = LEN_TRIM(Line)
      MaxStrings = SIZE(Strings)
      Strings(:) = ''
      
      ALLOCATE(StringTokens(MaxStrings), STAT=I)
      IF (I .NE. 0) THEN
         ErrorLevel = 1
         RETURN
      END IF
      StringTokens(:) = ''

      !
      !  Fill the Strings() entries with all blanks. One character at a time.
      !  Seems like a messy way to do it, but is necessary, at least with the
      !  fortran compiler I am using.
      !
      StrLen = LEN(Strings(1))     ! max length of each string
      DO I = 1, MaxStrings
         DO J = 1, StrLen
            Strings(I)(J:J) = ' '
         END DO
      END DO
      
      !
      !  Extract string tokens
      !
      TokenizedLine = ''
      TokenNum = 0
      J = 0
      InToken = .FALSE.
      DO WHILE (J .LT. LineLen)
         J = J + 1
         IF (.NOT. InToken) THEN
            IF (Line(J:J) .EQ. CHAR(34)) THEN
               InToken = .TRUE.
               TokenNum = TokenNum + 1
               K = 1
               StringTokens(TokenNum)(K:K) = Line(J:J)           ! opening DblQuote
               TokenizedLine = TRIM(TokenizedLine) // '"'
            ELSE
               TokenizedLine = TRIM(TokenizedLine) // Line(J:J)
            END IF
         ELSE
            K = K + 1
            StringTokens(TokenNum)(K:K) = Line(J:J)
            IF (Line(J:J) .EQ. CHAR(34)) THEN
               InToken = .FALSE.
            END IF
         END IF
      END DO
      
      !
      !  Parse the tokenized line, separating by commas
      !
      SNum = 1
      J = 0
      I = 1
      LineLen = LEN(TokenizedLine)
      DO WHILE ((J .LT. LineLen) .AND. (SNum .LE. MaxStrings))
         J = J + 1
         IF (TokenizedLine(J:J) .EQ. ',') THEN
            IF (I .LT. J) THEN
               K = I + LEN(Strings(SNum)) - 1
               IF (K .GE. J-1) THEN
                  Strings(SNum) = TRIM(TokenizedLine(I:J-1))
               ELSE
                  Strings(SNum) = TRIM(TokenizedLine(I:K))    ! truncate to fit
               END IF
            ELSE
               Strings(SNum) = ''
            END IF
            SNum = SNum + 1
            I = J+1
         END IF
      END DO
      IF ((J .GE. LineLen) .AND. (SNum .LE. MaxStrings)) THEN
         IF (I .LT. J) THEN
            Strings(SNum) = TRIM(TokenizedLine(I:J))
         ELSE
            Strings(SNum) = ''
         END IF
      END IF   
      
      !
      !  Now replace the DblQuote tokens with their respective string.
      !  If the caller specified TRUE for StripDoubleQuotes, then the leading 
      !  and trailing double quote characters will be removed. If the caller
      !  specified false, then they remain as part of the string and are not removed.
      !
      TokenNum = 0
      DO I = 1, SNum
         IF (Strings(I)(1:1) .EQ. CHAR(34)) THEN
            TokenNum = TokenNum + 1
            IF (KillQuotes) THEN
               TStr = TRIM(StringTokens(TokenNum))
               J = LEN_TRIM(TStr)
               Strings(I) = TStr(2:J-1)
            ELSE
               Strings(I) = TRIM(StringTokens(TokenNum))
            END IF
         END IF
      END DO

      IF (PRESENT(ValidNum)) ValidNum = SNum
      DEALLOCATE(StringTokens, STAT=I)
      
      END SUBROUTINE ParseCommaSepLineToTokens
   
!--------------------------------------------------------------------------
!     Parse the line in a simplistic way, just splitting it wherever a comma
!     is found. The only special consideration is when there is a double-quoted
!     string. Commas imbedded in that string will not cause a split.
!
!     This routine is used a LOT when processing CSV files. And it seems to me 
!     that it is not super efficient. Stepping along one character at a time
!     just doesn't feel optimal. I tried an alternate method where I used
!     INDEX() to process things via substring operations. Turned out to take 
!     twice as long as this current method, so I am sticking with this one for 
!     now. But I still feel like I should re-visit this at some point, since it
!     could have a huge impact on processing time. 
!---------------------------------------------------------------------------
      SUBROUTINE ParseCommaSepLineMethod1(Line, Strings, ValidNum)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: Line
      CHARACTER(LEN=*), DIMENSION(:), INTENT(INOUT) :: Strings
      INTEGER, OPTIONAL, INTENT(OUT) :: ValidNum

      INTEGER :: MaxStrings, SNum, LineLen, SLen
      INTEGER :: I, J, K
      LOGICAL :: InQS
      CHARACTER(LEN=:), ALLOCATABLE :: Blanks

      IF (PRESENT(ValidNum)) ValidNum = 0
      LineLen = LEN_TRIM(Line)
      MaxStrings = SIZE(Strings)
      
      !
      !  Fill the Strings() entries with all blanks. 
      !
      SLen = LEN(Strings(1))     ! max length of each string
      ALLOCATE(CHARACTER(LEN=SLen) :: Blanks)
      DO I = 1, SLen
         Blanks(I:I) = ' '
      END DO
      SLen = LEN(Strings(1))     ! max length of each string
      DO I = 1, MaxStrings
         Strings(I) = Blanks
      END DO
      DEALLOCATE(Blanks)
      
      !
      !  Extract strings
      !
      SNum = 1                     ! currently-being-built string
      K = 1                        ! index into that string
      J = 0                        ! current character of the long line
      InQS = .FALSE.               ! are we within a quoted string?
      DO WHILE (J .LT. LineLen)
         J = J + 1
         IF (.NOT. InQS) THEN
            IF (Line(J:J) .EQ. CHAR(34)) THEN        ! an opening double-quote char
               InQS = .TRUE.
               IF (K .LE. SLen) Strings(SNum)(K:K) = Line(J:J)
            ELSE IF (Line(J:J) .EQ. ',') THEN
               SNum = SNum + 1
               IF (SNum .GT. MaxStrings) THEN
                  ErrorMessage = 'Too many comma-separated strings on the line.'; CALL PassMsg
                  WRITE(ErrorMessage, 5001) SNum, MaxStrings; CALL PassMsg
                  ErrorLevel = 1
                  RETURN
               END IF
               K = 1
            ELSE
               IF (K .LE. SLen) THEN
                  Strings(SNum)(K:K) = Line(J:J)
                  K = K + 1
               END IF
            END IF
         ELSE
            K = K + 1
            IF (K .LE. SLen) Strings(SNum)(K:K) = Line(J:J)
            IF (Line(J:J) .EQ. CHAR(34)) THEN         ! a closing double-quote char
               InQS = .FALSE.
            END IF
         END IF
      END DO
      
      !
      !  Left-justify and trim the strings
      !
      DO I = 1, SNum
         Strings(I) = TRIM(ADJUSTL(Strings(I)))
      END DO

      IF (PRESENT(ValidNum)) ValidNum = SNum
      
 5001 FORMAT('The line contains ', I0, ' strings, but only ', I0, ' are allowed')
      END SUBROUTINE ParseCommaSepLineMethod1
   
!--------------------------------------------------------------------------
!     Do the same thing as ParseCommaSepLineMethod1(), but using the Fortran SCAN()
!     intrinsic instead of parsing character by character. Mainly a test to 
!     see if I can improve performance.
!     After testing... It is about 10-15% faster. Not huge, but worthwhile.
!--------------------------------------------------------------------------
      SUBROUTINE ParseCommaSepLine(Line, Strings, ValidNum)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: Line
      CHARACTER(LEN=*), DIMENSION(:), INTENT(INOUT) :: Strings
      INTEGER, OPTIONAL, INTENT(OUT) :: ValidNum

      INTEGER :: MaxStrings, SNum, LineLen
      INTEGER :: I, J, K
      LOGICAL :: Done
      CHARACTER(LEN=2) :: QuoteComma
      CHARACTER(LEN=:), ALLOCATABLE :: S, Sq

      IF (PRESENT(ValidNum)) ValidNum = 0
      LineLen = LEN_TRIM(Line)
      MaxStrings = SIZE(Strings)
      
      !
      !  Some quick validity checks on the inputs
      !
      IF (LineLen    .EQ. 0) RETURN
      IF (MaxStrings .EQ. 0) RETURN
      
      !
      ALLOCATE(CHARACTER(LEN=LineLen) :: S)
      ALLOCATE(CHARACTER(LEN=LineLen) :: Sq)
      S = TRIM(Line)
      
      !
      !  Extract strings
      !
      QuoteComma = CHAR(34) // ','  ! double quote and comma characters
      SNum = 1                      ! current string
      Done = .FALSE.
      DO WHILE  (.NOT. Done)
         I = SCAN(S, QuoteComma)    ! find the next quote or comma character in S
         IF (I .LE. 0) THEN         ! reached end of S with no more delimiters, so remaining line is the string
            Strings(SNum) = TRIM(ADJUSTL(S))
            Done = .TRUE.
         ELSE
            IF (S(I:I) .EQ. CHAR(34)) THEN     ! found a double-quote character
               J = SCAN(S(I+1:), CHAR(34))     ! find the matching one (multi-level nesting not allowed)
               IF (J .EQ. 0) GOTO 850          ! error; did not find closing quote
               Strings(SNum) = S(I:I+J)        ! includes the quotes at each end
               K = SCAN(S(I+J+1:), ',')        ! find the comma that ends this item
               I = I + J + K + 1
            ELSE                               ! it was a comma
               Strings(SNum) = TRIM(S(1:I-1))
               I = I + 1
            END IF
            S = TRIM(ADJUSTL(S(I:)))
            IF (LEN_TRIM(S) .GT. 0) THEN
               SNum = SNum + 1
            ELSE
               Done = .TRUE.
            END IF
         END IF
      END DO 
      IF (PRESENT(ValidNum)) ValidNum = SNum
      GOTO 999
      
 850  ErrorMessage = 'Mismatched quote characters in CSV line'; CALL PassMsg
      ErrorMessage = 'Line=['//TRIM(Line)//']';  CALL PassMsg
      ErrorLevel = 1
      
 999  DEALLOCATE(S, Sq)
      
      END SUBROUTINE ParseCommaSepLine
      
!-------------------------------------------------------------
!  Returns the Path variable after replacing all path separator characters
!  to reflect the current operating system, and appending one to the
!  end of the string, if needed.
!-------------------------------------------------------------
      FUNCTION NormalizedFilePath(Path) RESULT(NFP)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: Path

      INTEGER :: SL
      INTEGER, PARAMETER            :: MaxLenStr = 1000
      CHARACTER(LEN=MaxLenStr)      :: Temp
      CHARACTER(LEN=:), ALLOCATABLE :: NFP            ! Fortran 2003 feature
      
      Temp = TRIM(Path)
      CALL ConvertFilePathDelimiters(Temp)
      SL = LEN_TRIM(Temp)
      IF (SL .EQ. 0) THEN
         NFP = ''
         RETURN
      END IF
      IF (Temp(SL:SL) .NE. FilePathSeparator) SL = SL + 1
      IF (SL .GT. MaxLenStr) THEN
         ErrorMessage = 'String too long in NormalizedFilePath()'; CALL PassMsg
         ErrorLevel = 1
         NFP = ''
         RETURN
      END IF
      IF (Temp(SL:SL) .NE. FilePathSeparator) Temp = TRIM(Temp) // FilePathSeparator
      NFP = TRIM(Temp)
      RETURN

      END FUNCTION NormalizedFilePath

!-------------------------------------------------------------
!  Strips out any path info that might be in the supplied file name
!  and returns just the path.
!  Trailing file path separator is retained.
!  If no path info was specified, this returns an empty string.
!-------------------------------------------------------------
      FUNCTION FilePathOnly(FName) RESULT(FP)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: FName

      INTEGER :: I, SL
      INTEGER, PARAMETER            :: MaxLenStr = 1000
      CHARACTER(LEN=MaxLenStr)      :: Temp
      CHARACTER(LEN=:), ALLOCATABLE :: FP            ! Fortran 2003 feature
      
      SL = LEN_TRIM(FName)
      IF (SL .GT. MaxLenStr) THEN
         ErrorMessage = 'String too long in FilePathOnly()'; CALL PassMsg
         ErrorLevel = 1
         FP = ''
         RETURN
      END IF
      Temp = TRIM(FName)
      CALL ConvertFilePathDelimiters(Temp);  IF (ErrorLevel .NE. 0) GOTO 899
      I = INDEX(Temp, FilePathSeparator, .TRUE.)
      IF (I .GT. 0) THEN
         FP = TRIM(Temp(:I))
         RETURN
      END IF
      
 899  FP = ''
      RETURN
      END FUNCTION FilePathOnly

!-------------------------------------------------------------
!  Strips off any path info that might be in the supplied file name
!  and returns just the file name portion.
!-------------------------------------------------------------
      FUNCTION FileNameOnly(FName) RESULT(FN)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: FName

      INTEGER :: I, SL
      INTEGER, PARAMETER            :: MaxLenStr = 1000
      CHARACTER(LEN=MaxLenStr)      :: Temp
      CHARACTER(LEN=:), ALLOCATABLE :: FN            ! Fortran 2003 feature
      
      SL = LEN_TRIM(FName)
      IF (SL .GT. MaxLenStr) THEN
         ErrorMessage = 'String too long in FileNameOnly()'; CALL PassMsg
         ErrorLevel = 1
         FN = ''
         RETURN
      END IF
      Temp = TRIM(FName)
      CALL ConvertFilePathDelimiters(Temp); IF (ErrorLevel .NE. 0) GOTO 899
      I = INDEX(Temp, FilePathSeparator, .TRUE.)
      FN = TRIM(Temp(I+1:))
      RETURN
      
    899 FN = ''
        RETURN

      END FUNCTION FileNameOnly

!-------------------------------------------------------------
!  Assumption is that FName does not include any path info, and the caller
!  is providing the valid info in Path.
!-------------------------------------------------------------
      FUNCTION FullyQualifiedFileName(Path, FName) RESULT(FQFN)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: Path, FName

      INTEGER :: I, SL
      INTEGER, PARAMETER            :: MaxLenStr = 1000
      CHARACTER(LEN=MaxLenStr)      :: FP, FN
      CHARACTER(LEN=:), ALLOCATABLE :: FQFN            ! Fortran 2003 feature

      SL = LEN_TRIM(Path)
      IF (SL .GT. MaxLenStr) THEN
         ErrorMessage = 'Path too long in FullyQualifiedFileName()'; CALL PassMsg
         ErrorLevel = 1
         FQFN = ''
         RETURN
      END IF
      FP = NormalizedFilePath(TRIM(Path)); IF (ErrorLevel .NE. 0) GOTO 899
      
      SL = LEN_TRIM(FName)
      IF (SL .GT. MaxLenStr) THEN
         ErrorMessage = 'FName too long in FullyQualifiedFileName()'; CALL PassMsg
         ErrorLevel = 1
         FQFN = ''
         RETURN
      END IF
      FN = TRIM(FName)
      I = INDEX(FN, FilePathSeparator)
      IF (I .NE. 0) THEN
         ErrorMessage = 'FName must not contain path information in FullyQualifiedFileName()'; CALL PassMsg
         ErrorLevel = 1
         FQFN = ''
         RETURN
      END IF

      FQFN = TRIM(FP) // TRIM(FN)
      RETURN
      
    899 FQFN = ''
        RETURN

      END FUNCTION FullyQualifiedFileName
   
!-------------------------------------------------------------
!  This routine will process the Path and FName variables, passing
!  back the correct values in FP (path) and FN (name).
!
!  1) If FName contains both a path and file name then they will be
!     split apart and assigned to FP and FN
!
!  2) If Path contains anything then it will be formatted into a
!     good path specification (adding a "\" at the end if needed) so
!     that it can be prepended to FN yielding a valid file name.
!     This will overwrite any value of FP that may have been assigned 
!     in step 1.
!
!  3) If the resultant value of FP is not a valid format
!        (C:\Dir1\...\Dir2\)
!     then it will be cleared before the routine returns.
!-------------------------------------------------------------
      SUBROUTINE ProcessPathAndFileName(Path, FName, FP, FN)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN)    :: Path, FName
      CHARACTER (LEN=*), INTENT(INOUT) :: FP, FN
      INTEGER :: I
      LOGICAL :: OK

      !
      !  Initial values to pass back
      !
      FN = TRIM(FName)
      FP = TRIM(Path)

      !
      !  If FName contains the path info, then separate the 2 parts
      !
      I = INDEX(FN, FilePathSeparator, .TRUE.)     ! find last '\' or '/' character
      IF (I .GT. 0) THEN
         FP = FN(1:I)
         FN = FN(I+1:LEN_TRIM(FName))
      END IF

      !
      !  Make sure the path info is good
      !
      IF (LEN_TRIM(Path) .GT. 0) THEN
         FP = TRIM(Path)
         I = LEN_TRIM(FP)
         IF (FP(I:I) .NE. FilePathSeparator) FP = TRIM(FP) // FilePathSeparator
      END IF

      IF (LEN_TRIM(FP) .GT. 0) THEN
         OK = .TRUE.
         IF (.NOT. CharIsLetter(FP(1:1)) ) OK = .FALSE.
         IF (.NOT.   (FP(2:2) .EQ. ':')  ) OK = .FALSE.
         IF (.NOT.   (FP(3:3) .EQ. FilePathSeparator)) OK = .FALSE.
         IF (.NOT. OK) FP = ''
      END IF

      RETURN

      END SUBROUTINE ProcessPathAndFileName
   
!-------------------------------------------------------------
        SUBROUTINE ConvertFilePathDelimiters(FName)
        IMPLICIT   NONE
        CHARACTER (LEN=*), INTENT(INOUT) :: FName
        INTEGER :: I
        CHARACTER (LEN=1) :: PB    ! the bad delimiter character

        IF (FilePathSeparator .EQ. '\') PB = '/'
        IF (FilePathSeparator .EQ. '/') PB = '\'
        DO I = 1, LEN_TRIM(FName)
           IF (FName(I:I) .EQ. PB) FName(I:I) = FilePathSeparator
        END DO
        END SUBROUTINE ConvertFilePathDelimiters
   
!-------------------------------------------------------------
      SUBROUTINE BuildDirFileList_SYSTEM(ListFileName, FileSpec)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: ListFileName, FileSpec
      CHARACTER(LEN=150) :: CL
   
#if defined(__windows__)
   CL = 'dir/b '//TRIM(FileSpec)//' > '//TRIM(ListFileName)
#else
   CL = 'ls -1 '//TRIM(FileSpec)//' > '//TRIM(ListFileName) // ' 2> /dev/null'
#endif
      CALL SYSTEM(TRIM(CL))
   
      END SUBROUTINE BuildDirFileList_SYSTEM
   
!-------------------------------------------------------------
      SUBROUTINE UpperCase(S)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(INOUT) :: S
      INTEGER   :: I, ChOff

      ChOff = ICHAR('a') - ICHAR('A')
      DO I=1, LEN_TRIM(S)
         IF ( (S(I:I) .GE. 'a') .AND. (S(I:I) .LE. 'z') ) THEN
            S(I:I) = CHAR(ICHAR(S(I:I)) - ChOff)
         ENDIF
      END DO
      END SUBROUTINE UpperCase
!-------------------------------------------------------------
      SUBROUTINE LowerCase(S)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(INOUT) :: S
      INTEGER   :: I, ChOff
      ChOff = ICHAR('a') - ICHAR('A')
      DO I=1, LEN_TRIM(S)
         IF ( (S(I:I) .GE. 'A') .AND. (S(I:I) .LE. 'Z') ) THEN
            S(I:I) = CHAR(ICHAR(S(I:I)) + ChOff)
         ENDIF
      END DO
      RETURN
      END SUBROUTINE LowerCase
!-------------------------------------------------------------
      FUNCTION GetUppercase(S) Result (U)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: S
      CHARACTER (LEN=:), ALLOCATABLE :: U
      U = TRIM(S)
      CALL Uppercase(U)
      END FUNCTION GetUppercase
!-------------------------------------------------------------
      FUNCTION GetLowercase(S) Result (L)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: S
      CHARACTER (LEN=:), ALLOCATABLE :: L
      L = TRIM(ADJUSTL(S))
      CALL Lowercase(L)
      END FUNCTION GetLowercase
!-------------------------------------------------------------
      FUNCTION StripAllBlanks(S) Result (T)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: S
      CHARACTER (LEN=:), ALLOCATABLE :: T
      CHARACTER (LEN=9999)  :: X
      INTEGER   :: I, J
      X = '';  J = 0
      DO I = 1, LEN_TRIM(S)
        IF (S(I:I) .NE. ' ') THEN
           J = J + 1
           X(J:J) = S(I:I)
        END IF
      END DO
      T = TRIM(X)
      RETURN
      END FUNCTION StripAllBlanks
!-------------------------------------------------------------
      FUNCTION StripDoubleQuotes(S) Result (T)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: S
      CHARACTER (LEN=:), ALLOCATABLE :: T
      CHARACTER (LEN=9999)  :: X
      INTEGER   :: I, J
      X = '';  J = 0
      DO I = 1, LEN_TRIM(S)
        IF (S(I:I) .NE. '"') THEN
           J = J + 1
           X(J:J) = S(I:I)
        END IF
      END DO
      T = TRIM(X)
      RETURN
      END FUNCTION StripDoubleQuotes
!-------------------------------------------------------------
      LOGICAL FUNCTION CharIsLetter(Ch)
      IMPLICIT  NONE
      CHARACTER (LEN=1), INTENT(IN) :: Ch
      INTEGER :: C
      LOGICAL :: L

      C = ICHAR(Ch)
      L = .FALSE.
      IF ((C .GE. IACHAR('a')) .AND. (C .LE. IACHAR('z'))) L = .TRUE.
      IF ((C .GE. IACHAR('A')) .AND. (C .LE. IACHAR('Z'))) L = .TRUE.
      CharIsLetter = L
      END FUNCTION CharIsLetter
!-------------------------------------------------------------
      LOGICAL FUNCTION CharIsNumber(Ch)
      IMPLICIT  NONE
      CHARACTER (LEN=1), INTENT(IN) :: Ch
      INTEGER :: C
      LOGICAL :: L

      C = IACHAR(Ch)
      L = .FALSE.
      IF ((C .GE. IACHAR('0')) .AND. (C .LE. IACHAR('9'))) L = .TRUE.
      CharIsNumber = L
      END FUNCTION CharIsNumber
!-------------------------------------------------------------
      LOGICAL FUNCTION CharIsSpecial(Ch)
      IMPLICIT  NONE
      CHARACTER (LEN=1), INTENT(IN) :: Ch
      INTEGER :: C
      LOGICAL :: L

      C = IACHAR(Ch)
      L = .FALSE.
      IF ((C .GE. IACHAR(' ')) .AND. (C .LE. IACHAR('~'))) THEN
         L = .TRUE.
         IF (CharIsLetter(Ch)) L = .FALSE.
         IF (CharIsNumber(Ch)) L = .FALSE.
      END IF
      CharIsSpecial = L
      END FUNCTION CharIsSpecial
!-------------------------------------------------------------
      FUNCTION ReplaceAllChars(String, OldCh, NewCh)  Result(NewStr)
      IMPLICIT  NONE
      CHARACTER (LEN=*), INTENT(IN) :: String
      CHARACTER (LEN=1), INTENT(IN) :: OldCh, NewCh
      CHARACTER (LEN=:), ALLOCATABLE :: NewStr
      INTEGER :: I

      NewStr = TRIM(String)
      DO I = 1, LEN_TRIM(NewStr)
         IF (NewStr(I:I) .EQ. OldCh) NewStr(I:I) = NewCh
      END DO
      END FUNCTION ReplaceAllChars
!-------------------------------------------------------------------------
      SUBROUTINE GetTodaysDateTime(FmtDate, FmtTime)
      IMPLICIT  NONE
      CHARACTER (LEN=*), INTENT(INOUT) :: FmtDate, FmtTime
      INTEGER, DIMENSION(8) :: DT_Values
      CHARACTER(LEN=10) :: DT_Date, DT_Time, DT_Zone, Tmp

      FmtDate = ''
      FmtTime = ''
      CALL Date_And_Time(DT_Date, DT_Time, DT_Zone, DT_Values)
      !
      ! Construct date string in form "MM/DD/YYYY"
      !
      IF (LEN(FmtDate) .GE. 10) THEN
         FmtDate = Tmp
         Tmp = DT_Date(5:6) // '/' // DT_Date(7:8) // '/' // DT_Date(1:4)
      END IF
      !
      ! Construct time string in form "HH:MM"
      !
      IF (LEN(FmtDate) .GE. 5) THEN
         FmtTime = Tmp
         Tmp = DT_Time(1:2) // ':' // DT_Time(3:4)
      END IF
      END SUBROUTINE GetTodaysDateTime
!-------------------------------------------------------------------------
      SUBROUTINE GetElapsedSeconds(StartTime, EndTime, Seconds)
      IMPLICIT NONE
      INTEGER, DIMENSION(8), INTENT(IN)  :: StartTime, EndTime     ! results from DATE_AND_TIME()
      INTEGER,               INTENT(OUT) :: Seconds
      INTEGER :: SSeq, ESeq, NDays, NHrs, NMins, NSecs
      CALL DateSequence(StartTime(3), StartTime(2), StartTime(1), SSeq); IF (ErrorLevel .NE. 0) GOTO 899
      CALL DateSequence(EndTime(3),   EndTime(2),   EndTime(1),   ESeq); IF (ErrorLevel .NE. 0) GOTO 899
      NDays = ESeq - SSeq
      NHrs  = EndTime(5) - StartTime(5)
      NMins = EndTime(6) - StartTime(6)
      NSecs = EndTime(7) - StartTime(7)
      IF (NSecs .LT. 0) THEN
         NSecs = NSecs + 60
         NMins = NMins - 1
      END IF
      IF (NMins .LT. 0) THEN
         NMins = NMins + 60
         NHrs  = NHrs - 1
      END IF
      IF (NHrs .LT. 0) THEN
         NHrs  = NHrs + 24
         NDays = NDays - 1
      END IF
      Seconds = (NDays * 24 * 3600) + (NHrs * 3600) + (NMins * 60) + NSecs
      RETURN
      
  899 Seconds = MissingData_Int
      ErrorMessage = '[traceback] GetElapsedSeconds...';  CALL PassMsg
      END SUBROUTINE GetElapsedSeconds
!-------------------------------------------------------------------------
      FUNCTION GetStringHMS(Seconds)     Result(StrHMS)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: Seconds
      CHARACTER (LEN=10)  :: StrHMS
      INTEGER :: H, M, S, R
      H  = Seconds / 3600       ! integer division (truncated)
      R  = Seconds - H*3600
      M  = R / 60              ! integer division (truncated)
      S  = R - M*60
      WRITE(StrHMS, 1001) H, M, S
 1001 FORMAT(I0, ':', I2.2, ':', I2.2)
      END FUNCTION GetStringHMS
     
!--------------------------------------------------------------------------
!  Accepts a sequence number and returns a string in format "MM/DD/YYYY"
!--------------------------------------------------------------------------
      CHARACTER (LEN=10) FUNCTION SeqToDateString10(SeqNum)
      IMPLICIT   NONE
      INTEGER :: SeqNum, D, M, Y
      CHARACTER (LEN=10) :: S

      SeqToDateString10 = ''
      CALL SequenceDate(D, M, Y, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      WRITE(S, 1000, ERR=811) M, D, Y
      DO D = 1, 10
         IF (S(D:D) .EQ. ' ') S(D:D) = '0'
      ENDDO
      SeqToDateString10 = S
      RETURN

    811 WRITE(ErrorMessage, 2001) SeqNum
        CALL PassMsg; GOTO 898
    898 ErrorLevel = 1
        ErrorMessage = '[traceback] : SeqToDateString10...'
        CALL PassMsg
   1000 FORMAT(I2.2,'/',I2.2,'/',I4.4)
   2001 FORMAT('Error building date string from ', I0)
      END FUNCTION SeqToDateString10

!--------------------------------------------------------------------------
!  Accepts a sequence number and returns a string in format "YYYY-MM-DD"
!--------------------------------------------------------------------------
      CHARACTER (LEN=10) FUNCTION SeqToDateStringYMD(SeqNum)
      IMPLICIT   NONE
      INTEGER :: SeqNum, D, M, Y
      CHARACTER (LEN=10) :: S

      SeqToDateStringYMD = '9999-99-99'
      IF (SeqNum .EQ. MissingData_Date) RETURN
      CALL SequenceDate(D, M, Y, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      WRITE(S, 1000, ERR=811) Y, M, D
      SeqToDateStringYMD = S
      RETURN

    811 WRITE(ErrorMessage, 2001) SeqNum;  CALL PassMsg
        ErrorLevel = 1
        ErrorMessage = '[traceback] : SeqToDateStringYMD...';  CALL PassMsg
   1000 FORMAT(I4.4, 2('-', I2.2))
   2001 FORMAT('Error building date string from ', I0)
      END FUNCTION SeqToDateStringYMD

!--------------------------------------------------------------------------
      INTEGER FUNCTION DateString10ToSeq(DS)
      IMPLICIT   NONE
      CHARACTER (LEN=10), INTENT(IN) :: DS
      INTEGER :: Seq

      Seq = DateStringMDYToSeq(DS)
      DateString10ToSeq = Seq
      RETURN

      END FUNCTION DateString10ToSeq

!--------------------------------------------------------------------------
!     Convert string like 07/25/2005 into sequence number
!     Will also work with 7/25/2005
!--------------------------------------------------------------------------
      INTEGER FUNCTION DateStringMDYToSeq(DS)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: DS
      INTEGER :: SeqNum, D, M, Y, Slash
      CHARACTER (LEN=50) :: S, SD, SM, SY

      Slash = INDEX(DS, '/')
      IF (Slash .EQ. 0) THEN
         DateStringMDYToSeq = -1
         RETURN
      END IF
      SM = DS(1:Slash-1)
      S = DS(Slash+1:)

      Slash = INDEX(S, '/')
      IF (Slash .EQ. 0) THEN
         DateStringMDYToSeq = -1
         RETURN
      END IF
      SD = S(1:Slash-1)
      SY = S(Slash+1:)

      READ(SM, *, ERR=888) M
      READ(SD, *, ERR=888) D
      READ(SY, *, ERR=888) Y
      IF ((M .LT. 1) .OR. (M .GT. 12)) GOTO 888
      IF ((Y .LT. 1000) .OR. (Y .GT. 2999)) GOTO 888
      IF ((D .LT. 1) .OR. (D .GT. DaysInMonth(M,Y))) GOTO 888

      CALL DateSequence(D, M, Y, SeqNum); IF (ErrorLevel .NE. 0) GOTO 888
      DateStringMDYToSeq = SeqNum
      RETURN

    888  DateStringMDYToSeq = MissingData_Date
         RETURN
      END FUNCTION DateStringMDYToSeq

!--------------------------------------------------------------------------
!     Convert string like 2005-07-25 into sequence number
!     Will also work with 2005-7-25
!--------------------------------------------------------------------------
      INTEGER FUNCTION DateStringYMDToSeq(DS)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: DS
      INTEGER :: SeqNum, D, M, Y, Dash
      CHARACTER (LEN=50) :: S, SD, SM, SY

      IF (TRIM(DS) .EQ. '9999-99-99') GOTO 888
      Dash = INDEX(DS, '-')
      IF (Dash .EQ. 0) THEN
         DateStringYMDToSeq = -1
         RETURN
      END IF
      SY = DS(1:Dash-1)
      S = DS(Dash+1:)

      Dash = INDEX(S, '-')
      IF (Dash .EQ. 0) THEN
         DateStringYMDToSeq = -1
         RETURN
      END IF
      SM = S(1:Dash-1)
      SD = S(Dash+1:)

      READ(SY, *, ERR=888) Y
      READ(SM, *, ERR=888) M
      READ(SD, *, ERR=888) D
      IF ((Y .LT. 1000) .OR. (Y .GT. 2999)) GOTO 888
      IF ((M .LT. 1) .OR. (M .GT. 12)) GOTO 888
      IF ((D .LT. 1) .OR. (D .GT. DaysInMonth(M,Y))) GOTO 888

      CALL DateSequence(D, M, Y, SeqNum); IF (ErrorLevel .NE. 0) GOTO 888
      DateStringYMDToSeq = SeqNum
      RETURN

    888  DateStringYMDToSeq = MissingData_Date
         RETURN
      END FUNCTION DateStringYMDToSeq

!-------------------------------------------------------------------------
!   This routine computes the number of the date, with days numbered
!   sequentially from 1 (corresponding to the base date).  The base
!   date must be 1 March on any LEAP YEAR or century boundary. Dates
!   must be between the base date and the base date plus 2,147,483,646.
!   This routine is set up to use only 4-byte integer variables and
!   to avoid intermediate use of reals in the evaluation of expressions.
!   It is a modification of the previous version which used 2-byte integer
!   variables. For reference on the 2-byte version of this subroutine
!   see Dr. Dobb's Journal 80:66-70.  Adapted by Tim Hunter.
!-------------------------------------------------------------------------
      SUBROUTINE DateSequence(Day, Month, Year, SequenceNumber)
      IMPLICIT NONE
      INTEGER, PARAMETER   :: BaseYear = 0
      INTEGER, INTENT(OUT) :: SequenceNumber
      INTEGER, INTENT(IN)  :: Day, Month, Year
      INTEGER              :: NumYears, TotC, LeapC, SkipC, NonLeapC

      IF (Year .LE. BaseYear) THEN
         SequenceNumber = MissingData_Date
         WRITE(ErrorMessage, 9001) Month, Day, Year
         CALL PassMsg
         ErrorLevel = 1
         RETURN
      END IF
      NumYears = Year - BaseYear - 1 + (Month + 9) / 12
      TotC = (BaseYear + NumYears)/100 - (BaseYear/100)
      SkipC = 3 - MOD(BaseYear/100, 4)
      LeapC = (TotC + 3 - SkipC) / 4
      NonLeapC = TotC - LeapC
      SequenceNumber = 365 * NumYears + (NumYears/4) - NonLeapC +        &
                       (153 * MOD(Month + 9, 12) + 2) / 5 + Day
      RETURN
      9001 FORMAT(' DateSequence: Year .LE. BaseYear   MM/DD/YYYY=', I2.2,'/',I2.2,'/',I4)
      END SUBROUTINE DateSequence
!-------------------------------------------------------------------------
!     This routine computes the date of the number, with days numbered
!     sequentially from 1 (corresponding to the base date).  The base date must
!     be 1 March of any year divisible by 4 (i.e. leap years and century years).
!     The base year must be greater than or equal to 0.  Dates must be between
!     the base date and the base date plus 2,147,483,636. This routine is set
!     up to use 4-byte integer variables and to avoid intermediate use of reals
!     in the evaluation of expressions. Adapted from Dr. Dobb's Journal 80:66-70
!     by Tim Hunter.
!-------------------------------------------------------------------------
      SUBROUTINE SequenceDate(Dy, Mn, Yr, SequenceNumber)
      IMPLICIT   NONE
      INTEGER, PARAMETER   :: BaseYear = 0         ! 0  AD
      INTEGER, INTENT(OUT) :: Dy, Mn, Yr
      INTEGER, INTENT(IN)  :: SequenceNumber
      INTEGER              :: SeqNum, CentDays, BY

      IF (SequenceNumber .EQ. MissingData_Date) THEN
         Dy = -99
         Mn = -99
         Yr = -9999
         RETURN
      END IF
      
      SeqNum = SequenceNumber
      BY = BaseYear
      IF (SeqNum .LT. 0) THEN
         WRITE(ErrorMessage, 9001) SeqNum
         CALL PassMsg
         ErrorLevel = 1
         RETURN
      END IF
      !
      !  Century Computations
      !
      CentDays = 36524                ! since 100 A.D. is not a leap year
      DO WHILE (SeqNum .GT. CentDays)
         BY = BY + 100
         SeqNum = SeqNum - CentDays
         CentDays = 36524
         IF (((BY+100)/400*400) .EQ. (BY+100)) CentDays = 36525
      END DO
      !
      !  At this point, SeqNum is less than 100 years.  The following code is
      !  identical to the original 2-byte version.
      !
      Yr = (SeqNum - 1 - SeqNum/1461) / 365
      Dy = SeqNum - 365*Yr - Yr/4
      Mn = MOD ((5*Dy - 3)/153 + 2, 12) + 1
      Yr = Yr + BY + 1 - (Mn+9)/12
      Dy = Dy - (153 * MOD(Mn+9, 12) + 2) / 5
      RETURN
      9001 FORMAT(' SequenceDate: SequenceNumber <0 not allowed.   SeqNum=', I0)
      END SUBROUTINE SequenceDate
!------------------------------------------------------------------------------
      LOGICAL FUNCTION IsLeapYear(Year)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: Year
      IsLeapYear = .FALSE.
      IF ((MOD(Year,400) .EQ. 0) .OR.             &
          ((MOD(Year,4) .EQ. 0) .AND. (MOD(Year,100) .NE. 0))) IsLeapYear = .TRUE.
      END FUNCTION IsLeapYear
!------------------------------------------------------------------------------
      INTEGER FUNCTION JulianDayToSeqNum(Year, JDay)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: Year, JDay
      INTEGER             :: Seq
      
      JulianDayToSeqNum = -1
      CALL DateSequence(1, 1, Year, Seq); IF (ErrorLevel .NE. 0) RETURN
      JulianDayToSeqNum = Seq + JDay - 1
      END FUNCTION JulianDayToSeqNum
!------------------------------------------------------------------------------
      SUBROUTINE SeqNumToJulianDay(Seq, JDay, Year)
      IMPLICIT   NONE
      INTEGER, INTENT(IN)  :: Seq
      INTEGER, INTENT(OUT) :: Year, JDay
      INTEGER              :: D, M, SeqJ1

      CALL SequenceDate(D, M, Year, Seq); IF (ErrorLevel .NE. 0) RETURN
      CALL DateSequence(1, 1, Year, SeqJ1); IF (ErrorLevel .NE. 0) RETURN
      JDay = Seq - SeqJ1 + 1
      END SUBROUTINE SeqNumToJulianDay
!-------------------------------------------------------------------------
      INTEGER FUNCTION DaysInYear(Year)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) ::  Year
      INTEGER ::  Seq1, Seq2

      DaysInYear = -1
      IF (Year .LT. 1)    GOTO 899
      IF (Year .GT. 2999) GOTO 899
      CALL DateSequence( 1,  1, Year, Seq1); IF (ErrorLevel .NE. 0) RETURN
      CALL DateSequence(31, 12, Year, Seq2); IF (ErrorLevel .NE. 0) RETURN
      DaysInYear = Seq2 - Seq1
      RETURN
   899   ErrorMessage = 'Bad parameter in routine DaysInYear'
         CALL PassMsg
         ErrorLevel = 1
         RETURN
      END FUNCTION DaysInYear
!-------------------------------------------------------------------------
      INTEGER FUNCTION DaysInMonth (Month, Year)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) ::  Month, Year
      INTEGER ::  M1, Y1, Seq1, M2, Y2, Seq2

      DaysInMonth = -1
      IF (Month .LT. 1)   GOTO 899
      IF (Month .GT. 12)  GOTO 899
      IF (Year .LT. 1)    GOTO 899
      IF (Year .GT. 2999) GOTO 899
      M1 = Month
      Y1 = Year
      M2 = Month + 1
      Y2 = Year
      IF (M2 .GT. 12) THEN
         M2 = M2 - 12
         Y2 = Y2 + 1
      ENDIF
      CALL DateSequence(1, M1, Y1, Seq1); IF (ErrorLevel .NE. 0) RETURN
      CALL DateSequence(1, M2, Y2, Seq2); IF (ErrorLevel .NE. 0) RETURN
      DaysInMonth = Seq2 - Seq1
      RETURN
   899   ErrorMessage = 'Bad parameter in routine DaysInMonth'
         CALL PassMsg
         ErrorLevel = 1
         RETURN
      END FUNCTION DaysInMonth
!------------------------------------------------------------------------
      LOGICAL FUNCTION IsLastDayOfMonth(Day, Month, Year)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: Day, Month, Year
      INTEGER :: I
      LOGICAL :: Flag

      IsLastDayOfMonth = .FALSE.   ! dummy assignment to eliminate compiler warning
      Flag = .FALSE.
      I = DaysInMonth(Month, Year); IF(ErrorLevel .NE. 0) RETURN
      IF (Day .EQ. I) Flag = .TRUE.
      IsLastDayOfMonth = Flag
      END FUNCTION IsLastDayOfMonth
!-------------------------------------------------------------------------
      INTEGER FUNCTION DaysInQtrMon(Q, M, Y)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: Q, M, Y
      INTEGER :: DM, D1, D2

      DaysInQtrMon = -1
      DM = DaysInMonth(M, Y);        IF(ErrorLevel .NE. 0) RETURN
      D2 = LastDayOfQtrMon(DM, Q);   IF(ErrorLevel .NE. 0) RETURN
      D1 = FirstDayOfQtrMon(DM, Q);  IF(ErrorLevel .NE. 0) RETURN
      DaysInQtrMon = D2 - D1 + 1
      END FUNCTION DaysInQtrMon
!------------------------------------------------------------------------
      INTEGER FUNCTION DayOfQtrMon(Day, Month, Year)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: Day, Month, Year
      DayOfQtrMon = Day - FirstDayOfQtrMon(Day, Month, Year) + 1
      END FUNCTION DayOfQtrMon
!------------------------------------------------------------------------
      INTEGER FUNCTION QtrMonNumber (D, M, Y)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: D, M, Y
      INTEGER :: I, Days, Q
      QtrMonNumber = -1
      Days = DaysInMonth(M, Y);         IF (ErrorLevel .NE. 0) RETURN
      DO I = 4, 1, -1
        Q = LastDayOfQtrMon(Days, I);   IF (ErrorLevel .NE. 0) RETURN
        IF (D .LE. Q) QtrMonNumber = I
      ENDDO
      END FUNCTION QtrMonNumber
!------------------------------------------------------------------------
!  Day = 1..8
      INTEGER FUNCTION SequenceFromYMQD(Year, Mon, Qtr, Day)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: Year, Mon, Qtr, Day
      INTEGER :: Q, DM, Seq

      SequenceFromYMQD = -1
      DM = 0
      DO Q = 1, Qtr-1
         DM = DM + DaysInQtrMon(Q, Mon, Year)
      END DO
      DM = DM + Day
      CALL DateSequence(DM, Mon, Year, Seq);  IF(ErrorLevel .NE. 0) RETURN
      SequenceFromYMQD = Seq
      END FUNCTION SequenceFromYMQD
!------------------------------------------------------------------------
      LOGICAL FUNCTION IsFirstDayOfQtrMon(Day, Month, Year)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: Day, Month, Year
      INTEGER :: I, J
      LOGICAL :: Flag

      IsFirstDayOfQtrMon = .FALSE.   ! dummy assignment to eliminate compiler warning
      I = DaysInMonth(Month, Year); IF(ErrorLevel .NE. 0) RETURN
      Flag = .FALSE.
      DO J = 1, 4
         IF (Day .EQ. FirstDayOfQtrMon(I,J)) Flag = .TRUE.
         IF (ErrorLevel .NE. 0) RETURN
      END DO
      IsFirstDayOfQtrMon = Flag
      END FUNCTION IsFirstDayOfQtrMon
!------------------------------------------------------------------------
      LOGICAL FUNCTION IsLastDayOfQtrMon(Day, Month, Year)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: Day, Month, Year
      INTEGER :: I, J
      LOGICAL :: Flag

      IsLastDayOfQtrMon = .FALSE.   ! dummy assignment to eliminate compiler warning
      I = DaysInMonth(Month, Year); IF(ErrorLevel .NE. 0) RETURN
      Flag = .FALSE.
      DO J = 1, 4
         IF (Day .EQ. LastDayOfQtrMon(I,J)) Flag = .TRUE.
         IF (ErrorLevel .NE. 0) RETURN
      END DO
      IsLastDayOfQtrMon = Flag
      END FUNCTION IsLastDayOfQtrMon
!------------------------------------------------------------------------------
!  Adjust the sequence number to the first day of the quarter-month.
!
      SUBROUTINE AdjustToStartOfQtrMonth(SeqNum)
      IMPLICIT   NONE
      INTEGER, INTENT(INOUT) :: SeqNum
      INTEGER :: Dy, Mn, Yr, I
      CALL SequenceDate(Dy, Mn, Yr, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      I = FirstDayOfQtrMon(SeqNum);          IF (ErrorLevel .NE. 0) RETURN
      CALL DateSequence(I, Mn, Yr, SeqNum);  IF (ErrorLevel .NE. 0) RETURN
      END SUBROUTINE AdjustToStartOfQtrMonth
!------------------------------------------------------------------------------
!  Adjust the sequence number to the last day of the quarter-month.
!
      SUBROUTINE AdjustToEndOfQtrMonth(SeqNum)
      IMPLICIT   NONE
      INTEGER, INTENT(INOUT) :: SeqNum
      INTEGER :: Dy, Mn, Yr, I
      CALL SequenceDate(Dy, Mn, Yr, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      I = LastDayOfQtrMon(SeqNum);           IF (ErrorLevel .NE. 0) RETURN
      CALL DateSequence(I, Mn, Yr, SeqNum);  IF (ErrorLevel .NE. 0) RETURN
      END SUBROUTINE AdjustToEndOfQtrMonth
!-----------------------------------------------------------------------------
      INTEGER FUNCTION GetNumYears(SSeq, ESeq)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: SSeq, ESeq
      INTEGER :: D1, M1, Y1, D2, M2, Y2

      GetNumYears = -1
      CALL SequenceDate(D1, M1, Y1, SSeq); IF (ErrorLevel .NE. 0) RETURN
      CALL SequenceDate(D2, M2, Y2, ESeq); IF (ErrorLevel .NE. 0) RETURN
      IF ((D1 .NE.  1) .OR. (M1 .NE.  1)) Y1 = Y1 + 1
      IF ((D2 .NE. 31) .OR. (M2 .NE. 12)) Y2 = Y2 - 1
      GetNumYears = Y2 - Y1 + 1
      END FUNCTION GetNumYears
!-----------------------------------------------------------------------------
      INTEGER FUNCTION GetNumMonths(SSeq, ESeq)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: SSeq, ESeq
      INTEGER :: D1, M1, Y1, D2, M2, Y2

      GetNumMonths = -1
      CALL SequenceDate(D1, M1, Y1, SSeq); IF (ErrorLevel .NE. 0) RETURN
      CALL SequenceDate(D2, M2, Y2, ESeq); IF (ErrorLevel .NE. 0) RETURN
      IF (D1 .NE. 1) M1 = M1 + 1
      IF (.NOT. IsLastDayOfMonth(D2, M2, Y2)) M2 = M2 - 1; IF (ErrorLevel .NE. 0) RETURN
      GetNumMonths = (Y2-Y1)*12 + M2 - M1 + 1
      END FUNCTION GetNumMonths
!-----------------------------------------------------------------------------
      INTEGER FUNCTION GetNumQtrMons(SSeq, ESeq)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: SSeq, ESeq
      INTEGER :: D1, M1, Y1, D2, M2, Y2, I, Seq, Q1, Q2

      GetNumQtrMons = -1
      CALL SequenceDate(D1, M1, Y1, SSeq);     IF (ErrorLevel .NE. 0) RETURN
      I = FirstDayOfQtrMon(D1, M1, Y1);        IF (ErrorLevel .NE. 0) RETURN
      IF (D1 .NE. I) THEN
         I = LastDayOfQtrMon(D1, M1, Y1);      IF (ErrorLevel .NE. 0) RETURN
         CALL DateSequence(D1, M1, Y1, Seq);   IF (ErrorLevel .NE. 0) RETURN
         CALL SequenceDate(D1, M1, Y1, Seq+1); IF (ErrorLevel .NE. 0) RETURN
      END IF 
      Q1 = QtrMonNumber(D1, M1, Y1);           IF (ErrorLevel .NE. 0) RETURN
      CALL SequenceDate(D2, M2, Y2, ESeq);     IF (ErrorLevel .NE. 0) RETURN
      I = LastDayOfQtrMon(D2, M2, Y2);         IF (ErrorLevel .NE. 0) RETURN
      IF (D2 .NE. I) THEN
         I = FirstDayOfQtrMon(D2, M2, Y2);     IF (ErrorLevel .NE. 0) RETURN
         CALL DateSequence(D2, M2, Y2, Seq);   IF (ErrorLevel .NE. 0) RETURN
         CALL SequenceDate(D2, M2, Y2, Seq-1); IF (ErrorLevel .NE. 0) RETURN
      END IF 
      Q2 = QtrMonNumber(D2, M2, Y2);           IF (ErrorLevel .NE. 0) RETURN
      GetNumQtrMons = (Y2-Y1)*48 + (M2-M1)*4 + Q2-Q1+1
      END FUNCTION GetNumQtrMons
!-----------------------------------------------------------------------------
      INTEGER FUNCTION GetNumWeeks(SSeq, ESeq)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) :: SSeq, ESeq
      INTEGER :: I, J
      I = SSeq + 6
      J = 0
      DO WHILE (I .LE. ESeq)
         J = J + 1
         I = I + 7
      END DO
      GetNumWeeks = J
      END FUNCTION GetNumWeeks
!------------------------------------------------------------------------------
!  Adjust the sequence number to the first day of the month.
!
      SUBROUTINE AdjustToStartOfMonth(SeqNum)
      IMPLICIT   NONE
      INTEGER, INTENT(INOUT) :: SeqNum
      INTEGER :: Dy, Mn, Yr
      CALL SequenceDate(Dy, Mn, Yr, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      CALL DateSequence( 1, Mn, Yr, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      END SUBROUTINE AdjustToStartOfMonth
!------------------------------------------------------------------------------
!  Adjust the sequence number to the last day of the month.
!
      SUBROUTINE AdjustToEndOfMonth(SeqNum)
      IMPLICIT   NONE
      INTEGER, INTENT(INOUT) :: SeqNum
      INTEGER :: Dy, Mn, Yr
      CALL SequenceDate(Dy, Mn, Yr, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      Mn = Mn + 1
      IF (Mn .GT. 12) THEN
         Mn = Mn - 12
         Yr = Yr + 1
      ENDIF
      CALL DateSequence( 1, Mn, Yr, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      SeqNum = SeqNum - 1
      END SUBROUTINE AdjustToEndOfMonth
!------------------------------------------------------------------------------
!  Adjust the sequence number to the first day of the year.
!
      SUBROUTINE AdjustToStartOfYear(SeqNum)
      IMPLICIT   NONE
      INTEGER, INTENT(INOUT) :: SeqNum
      INTEGER :: Dy, Mn, Yr
      CALL SequenceDate(Dy, Mn, Yr, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      CALL DateSequence( 1,  1, Yr, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      END SUBROUTINE AdjustToStartOfYear
!------------------------------------------------------------------------------
!  Adjust the sequence number to the last day of the month.
!
      SUBROUTINE AdjustToEndOfYear(SeqNum)
      IMPLICIT   NONE
      INTEGER, INTENT(INOUT) :: SeqNum
      INTEGER :: Dy, Mn, Yr
      CALL SequenceDate(Dy, Mn, Yr, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      CALL DateSequence(31, 12, Yr, SeqNum); IF (ErrorLevel .NE. 0) RETURN
      END SUBROUTINE AdjustToEndOfYear
!------------------------------------------------------------------------------
!  Return the day of the week.
!   Sunday=1, Monday=2, Tuesday=3, ..., Saturday=7
!  Note that this presumes a constant Gregorian calendar throughout time.
!  My anchor date is March 4, 0001, which is a Sunday in our fake calendar.
!  This routine gives historically incorrect answers for date prior to 1583,
!  but they are mathematically consistent throughout all years, which is what
!  I want for any application where I might use this.
!
      INTEGER FUNCTION DayOfTheWeek(Dy, Mn, Yr)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: Dy, Mn, Yr
      INTEGER :: SeqToday, SeqAnchor, NumDays
      CALL DateSequence( 4, 3, 1, SeqAnchor)
      CALL DateSequence(Dy,Mn,Yr, SeqToday)
      NumDays = SeqToday - SeqAnchor
      DayOfTheWeek = MOD(NumDays,7) + 1
      END FUNCTION DayOfTheWeek
!-----------------------------------------------------------------------------
      SUBROUTINE FileNameNoPath(LongName, ShortName)
      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(IN)  :: LongName
      CHARACTER (LEN=*), INTENT(OUT) :: ShortName
      INTEGER :: I

      !   
      !  Handle Windows paths or Linux paths
      !
      I = INDEX(LongName, FilePathSeparator, .TRUE.)   ! find last \ or / in filename
      IF (I .GT. 0) THEN
         ShortName = LongName(I+1:)
      ELSE
         ShortName = LongName
      END IF
      END SUBROUTINE FileNameNoPath
   
!-----------------------------------------------------------------------------
!  This routine is used to copy a file in a "Fortran-standard" manner.
!  It will work correctly for binary as well as ASCII files.
!-----------------------------------------------------------------------------
      SUBROUTINE CopyFile (FName1, FName2)
      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(IN) :: FName1, FName2

      INTEGER, PARAMETER      :: BufSize = 256
      INTEGER                 :: I, J, IOS, UNum1, UNum2
      INTEGER                 :: BR, Rin, Rout
      LOGICAL                 :: Flag, IsThere
      INTEGER (KIND=INTEGER1) :: AByte, FileBuffer(BufSize)

      !
      !  Does the file exist?  If not, then there is nothing to do.
      !
      INQUIRE(FILE=TRIM(FName1), EXIST=Flag)
      IF (.NOT. Flag) THEN
         ErrorMessage = 'Error copying file.  '//TRIM(FName1)//' does not exist.'
         GOTO 898
      END IF

      !
      !  Open the files
      !
      INQUIRE(FILE=TRIM(FName1), EXIST=IsThere)
      IF (.NOT. IsThere) GOTO 811
      UNum1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) RETURN
      OPEN(UNIT=UNum1, FILE=TRIM(FName1), STATUS='OLD',              &
     &     ACCESS='DIRECT', FORM='UNFORMATTED', RECL=1, ERR=811)
      CALL FileWasOpened(UNum1); IF (ErrorLevel .NE. 0) RETURN

      UNum2 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) RETURN
      OPEN(UNIT=UNum2, FILE=TRIM(FName2), STATUS='REPLACE',          &
     &     ACCESS='DIRECT', FORM='UNFORMATTED', RECL=1, ERR=821)
      CALL FileWasOpened(UNum2); IF (ErrorLevel .NE. 0) RETURN

      !
      !  Read and write using a buffer to reduce disk head movement (thrashing)
      !
      !   BR = bytes already read into the buffer
      !
      Rin  = 1
      Rout = 0
      BR = 0
      READ(UNum1, REC=Rin, IOSTAT=IOS) AByte
      DO WHILE (IOS .EQ. 0)
         BR = BR + 1
         FileBuffer(BR) = AByte
         IF (BR .EQ. BufSize) THEN
            DO I = 1, BR
               J = Rout + I
               WRITE(UNum2, REC=J, ERR=823) FileBuffer(I)
            END DO
            Rout = Rout + BR
            BR = 0
         END IF
         Rin = Rin + 1
         READ(UNum1, REC=Rin, IOSTAT=IOS) AByte
      END DO
      DO I = 1, BR
         J = Rout + I
         WRITE(UNum2, REC=J, ERR=823) FileBuffer(I)
      END DO
      
      CLOSE(UNum1)
      CALL FileWasClosed(UNum1);      IF (ErrorLevel .NE. 0) RETURN
      CLOSE(UNum2)
      CALL FileWasClosed(UNum2);      IF (ErrorLevel .NE. 0) RETURN
      GOTO 999

      !
      !  Error while writing
      !
 811  ErrorMessage =  'Error opening '//TRIM(FName1)//' for copy.'
      CALL PassMsg;  GOTO 898
 821  ErrorMessage =  'Error opening '//TRIM(FName2)//' for copy.'
      CALL PassMsg;  GOTO 898
 823  ErrorMessage =  'Error writing to '//TRIM(FName2)//' for copy.'
      CALL PassMsg;  GOTO 898

 898  CALL PassMsg
      ErrorLevel = 1
 999  RETURN

      END SUBROUTINE CopyFile
!-----------------------------------------------------------------------------
      SUBROUTINE RenameFile (File1, File2, Success)
      IMPLICIT NONE
      LOGICAL              :: Success
      CHARACTER (LEN=*)    :: File1, File2

      Success = .FALSE.

      !
      !  Attempt to copy File1 to File2
      !
      CALL CopyFile(File1, File2)

      IF (ErrorLevel .NE. 0) THEN
         CALL DeleteFile(File2)
         SUCCESS = .FALSE.
         RETURN
      END IF

      !
      !  If we get to here then the copy worked.  Delete the old file 
      !  because the new one is good.
      !
      CALL DeleteFile(File1)
      Success = .TRUE.
      RETURN

      END SUBROUTINE RenameFile

!-----------------------------------------------------------------------------
!     Copy a file using the built-in system command.
!     In Windows:
!        Based on my own testing, using a forward slash (/) as the
!        file path separator does NOT work for the rename command. Thus,
!        I have to replace any of those in the source name.
!        Additionally, since Windows allows imbedded spaces in the file names,
!        I need to surround both file names with double-quotes to make sure
!        it is treated as just 2 file names.
!-----------------------------------------------------------------------------
      SUBROUTINE Copy_SYSTEM (OldName, NewName, Success)
      IMPLICIT NONE
      CHARACTER (LEN=*)   :: OldName, NewName
      LOGICAL             :: Success
      CHARACTER (LEN=200) :: NameO, NameN
      CHARACTER (LEN=250) :: CL
#if defined(__windows__)
      CHARACTER (LEN=1), PARAMETER :: DQ = CHAR(34)
#endif

      Success = .FALSE.
#if defined(__windows__)
      NameO = TRIM(ReplaceAllChars(OldName, FilePathSeparator, '\'))
      NameN = TRIM(ReplaceAllChars(NewName, FilePathSeparator, '\'))
      NameO = DQ // TRIM(NameO) // DQ
      NameN = DQ // TRIM(NameN) // DQ     
      CL = 'copy /y '//TRIM(NameO)//' '//TRIM(NameN)//' > NUL'
      CALL SYSTEM(TRIM(CL))
#else
      NameO = TRIM(OldName)
      NameN = TRIM(NewName)
      CL = 'cp '//TRIM(NameO)//' '//TRIM(NameN)//' > /dev/null'
      CALL SYSTEM(TRIM(CL))
#endif
      Success = .TRUE.
      END SUBROUTINE Copy_SYSTEM
      
!-----------------------------------------------------------------------------
!     Rename a file.
!     In Windows:
!        The file path cannot be changed via the rename command, and in fact
!        it will fail if the destination filename contains any drive or path
!        information. Thus, we will:
!           1) Test for agreement between file paths
!           2) Strip any path info from the specified new name. 
!        Also, based on my own testing, using a forward slash (/) as the
!        file path separator does NOT work for the rename command. Thus,
!        I have to replace any of those in the source name.
!        Additionally, since Windows allows imbedded spaces in the file names,
!        I need to surround both file names with double-quotes to make sure
!        it is treated as just 2 file names.
!-----------------------------------------------------------------------------
      SUBROUTINE Rename_SYSTEM (OldName, NewName, Success)
      IMPLICIT NONE
      CHARACTER (LEN=*)   :: OldName, NewName
      LOGICAL             :: Success
      LOGICAL :: FExist, MustOverwrite
      CHARACTER (LEN=200) :: NameO, NameN, SrcPath
      CHARACTER (LEN=250) :: CL
#if defined(__windows__)
      CHARACTER (LEN=1), PARAMETER :: DQ = CHAR(34)
      CHARACTER (LEN=200) :: P1, P2
#endif

      Success = .FALSE.
      SrcPath = FilePathOnly(OldName)     ! includes trailing slash
      
#if defined(__windows__)
      P1 = TRIM(GetLowercase(SrcPath))
      P2 = TRIM(GetLowercase(FilePathOnly(NewName)))
      IF (LEN_TRIM(P2) .GT. 0) THEN
         IF (P1 .NE. P2) THEN
            ErrorMessage = 'Unable to rename file. Invalid destination path.'; CALL PassMsg
            ErrorLevel = 1
            RETURN
         END IF
      END IF
#endif

      NameO = TRIM(SrcPath) // TRIM(FileNameOnly(OldName))
      NameN = TRIM(SrcPath) // TRIM(FileNameOnly(NewName))
      INQUIRE(FILE=TRIM(NameO), EXIST=FExist)
      IF (.NOT. FExist) THEN
         ErrorMessage = 'Unable to execute the rename operation..'; CALL PassMsg
         ErrorMessage = 'Specified source file ('//TRIM(NameO)//') does not exist.';  CALL PassMsg
         ErrorLevel = 1
         RETURN
      END IF
      INQUIRE(FILE=TRIM(NameN), EXIST=MustOverwrite)
      
#if defined(__windows__)
      NameO = TRIM(ReplaceAllChars(NameO, FilePathSeparator, '\'))
      NameN = TRIM(FileNameOnly(NewName))
      NameN = TRIM(ReplaceAllChars(FileNameOnly(NameN), FilePathSeparator, '\'))
      NameO = DQ // TRIM(NameO) // DQ
      NameN = DQ // TRIM(NameN) // DQ
      IF (MustOverwrite) THEN
         CL = 'DEL '//TRIM(NameN)//' > NUL'
         CALL SYSTEM(TRIM(CL))
      END IF
      CL = 'REN '//TRIM(NameO)//' '//TRIM(NameN)//' > NUL'
      CALL SYSTEM(TRIM(CL))
#else
      NameN = TRIM(NewName)
      IF (MustOverwrite) THEN
         CL = 'rm '//TRIM(NameN)
         CALL SYSTEM(TRIM(CL))
      END IF
      CL = 'mv '//TRIM(NameO)//' '//TRIM(NameN)//' > /dev/null'
      CALL SYSTEM(TRIM(CL))
#endif
      Success = .TRUE.
      END SUBROUTINE Rename_SYSTEM
      
!-----------------------------------------------------------------------------
!  This routine is used to delete file(s) in a "Fortran-standard" manner.
!-----------------------------------------------------------------------------
      SUBROUTINE DeleteFile (FileSpec)
      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(IN) :: FileSpec
      INTEGER :: I, IOS, ListU, FileU
      LOGICAL :: Flag
      CHARACTER(LEN=200)  :: ListFile, FName, FPath, FullName

      !
      !  Extract the file path (if appropriate) from the FileSpec.
      !  The list file built by BuildDirFileList() will contain only
      !  the BARE file name, with no path info. We will need to tack
      !  the path info back onto the file names as we cycle through them.
      !
      FPath = ''
      I = INDEX(FileSpec, FilePathSeparator, .TRUE.)    ! search from end
      IF (I .GT. 0) FPath = TRIM(FileSpec(1:I))
      
      !
      !  Make the file list
      !
      ListFile = 'xxxxxxTemporaryListFile.tmp'
      CALL BuildDirFileList(ListFile, FileSpec, 1)
      
      !
      !  Loop through the results of that system command
      !      
      ListU = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) RETURN
      OPEN(UNIT=ListU, FILE=TRIM(ListFile), STATUS='OLD', IOSTAT=I)
      CALL FileWasOpened(ListU)
      READ(ListU, *, IOSTAT=IOS) FName
      DO WHILE (IOS .EQ. 0) 
         FullName = TRIM(FPath) // TRIM(FName)
         INQUIRE(FILE=TRIM(FullName), EXIST=Flag)
         IF (Flag) THEN
            FileU = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) RETURN
            OPEN(UNIT=FileU, FILE=TRIM(FullName), STATUS='OLD', IOSTAT=I)
            CLOSE(FileU, STATUS='DELETE', IOSTAT=I)
            IF (I .NE. 0) THEN
                ErrorMessage = 'Error deleting '//TRIM(FullName); CALL PassMsg
                CLOSE(ListU, STATUS='DELETE')
                CALL FileWasClosed(ListU)
                RETURN
            END IF
         END IF
         READ(ListU, *, IOSTAT=IOS) FName
      END DO
      CLOSE(ListU, STATUS='DELETE')
      CALL FileWasClosed(ListU)
      RETURN

      END SUBROUTINE DeleteFile

!-----------------------------------------------------------------------------
!  This routine is used to delete file(s) using system commands.
!  Wildcard specs DO work here.
!  ***** Note that this is not working correctly in linux *****
!  ***** Gonna have to do a bit more R&D on it. *****
!-----------------------------------------------------------------------------
      SUBROUTINE DeleteFile_SYSTEM (FName)
      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(IN) :: FName
      CHARACTER (LEN=500) :: CL

#if defined(__windows__)
      CL = 'IF EXIST '//TRIM(FName)//' DEL /Q '//TRIM(FName)//' > NUL'
#else
      CL = 'find -type f -path '//CHAR(39)//TRIM(FName)//CHAR(39)// ' -delete'
#endif
      CALL SYSTEM(TRIM(CL))
      END SUBROUTINE DeleteFile_SYSTEM
      
!-------------------------------------------------------------------------
!     This routines is used to check for the existence of a file called
!     CANCEL.INT.  If it exists, then the user has requested that the
!     program execution be terminated.  There are a few places during the
!     AHPS execution where an immediate STOP would have disastrous
!     consequences.  Thus, I've included this test so that any program
!     can check this routine whenever it is a SAFE time to immediately
!     terminate.  (Or it can *make it* safe to terminate if the user has
!     requested that.)
!-------------------------------------------------------------------------
     LOGICAL FUNCTION UserWantsToCancel()
     IMPLICIT    NONE
     LOGICAL  :: IsThere
      
     INQUIRE(FILE='CANCEL.INT', EXIST=IsThere)
     IF (IsThere) THEN
        ErrorMessage = 'User has requested that the procedure be cancelled.'
        CALL PassMsg
        ErrorLevel = -99
        UserWantsToCancel = .TRUE.
        RETURN
     END IF

     UserWantsToCancel = .FALSE.
     RETURN
     END FUNCTION UserWantsToCancel
     
!-------------------------------------------------------------------------
      SUBROUTINE ClearFilesOpen()
      IMPLICIT   NONE
      INTEGER  :: I

      OpenedFiles = 0
      DO I = 1, MaxOpenedFiles
         OpenedFileUnits(I) = -1
      END DO
      END SUBROUTINE ClearFilesOpen
      
!-------------------------------------------------------------------------
      SUBROUTINE FileWasOpened(UNum)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) ::  UNum
      INTEGER             ::  I, J
      CHARACTER (LEN=6)   ::  SNum

      J = -1
      DO I = 1, OpenedFiles
         IF (OpenedFileUnits(I) .EQ. UNum) J = I
      END DO
      IF (J .GE. 0) THEN
        WRITE(SNum, '(I0)') UNum
        ErrorMessage = 'Unit '//TRIM(SNum)//' was already OPEN!'
        GOTO 898
      END IF
      IF (OpenedFiles .GE. MaxOpenedFiles) THEN
        ErrorMessage = 'Programmed limit on number of open files was exceeded!'
        GOTO 898
      END IF
      OpenedFiles = OpenedFiles + 1
      OpenedFileUnits(OpenedFiles) = UNum
      RETURN
 898  ErrorLevel = 1
      CALL PassMsg
      END SUBROUTINE FileWasOpened
      
!-------------------------------------------------------------------------
      SUBROUTINE FileWasClosed(UNum)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) ::  UNum
      INTEGER             ::  I, J

      J = -1
      DO I = 1, OpenedFiles
         IF (OpenedFileUnits(I) .EQ. UNum) J = I
      END DO
      IF (J .EQ. -1) RETURN
      !
      !  Move last entry in array to the empty slot and decrement the counter
      !
      OpenedFileUnits(J) = OpenedFileUnits(OpenedFiles)
      OpenedFileUnits(OpenedFiles) = -1
      OpenedFiles = OpenedFiles - 1
      
      END SUBROUTINE FileWasClosed
      
!-------------------------------------------------------------------------
      LOGICAL FUNCTION FileIsOpen(UNum)
      IMPLICIT   NONE
      INTEGER, INTENT(IN) ::  UNum

      IF (UNum .LT. 1) THEN
         FileIsOpen = .FALSE.
         RETURN
      END IF
      IF (UnitIsAvailable(UNum)) THEN
         FileIsOpen = .FALSE.
      ELSE
         FileIsOpen = .TRUE.
      END IF
      END FUNCTION FileIsOpen
      
!-------------------------------------------------------------------------
      SUBROUTINE CloseAllFiles()
      IMPLICIT   NONE
      INTEGER  ::  I, J, K, IOS

      K = OpenedFiles
      DO I = K, 1, -1
         J = OpenedFileUnits(I)
         CLOSE(J, IOSTAT=IOS)       ! don't choke when closing file, even if not open
         OpenedFileUnits(I) = -1
         OpenedFiles = OpenedFiles - 1
      END DO
      END SUBROUTINE CloseAllFiles
      
!-------------------------------------------------------------------------
      INTEGER FUNCTION GetFreeUnitNumber()
      IMPLICIT  NONE
      INTEGER :: UNum, TNum     
      LOGICAL :: Found
      
      UNum = -1
      TNum = 200                ! start at 200 so first test will be on unit=201
      Found = .FALSE.           ! initial value to force loop traversal
      DO WHILE ((.NOT. Found) .AND. (TNum .LT. 399))
         TNum = TNum + 1
         IF (UnitIsAvailable(TNum)) THEN
            Found = .TRUE.
            UNum = TNum
         END IF
      END DO
      IF (UNum .EQ. -1) THEN
         ErrorMessage = 'Unable to get available unit number'
         CALL PassMsg
         GOTO 898
      END IF
      GetFreeUnitNumber = UNum
      RETURN
 898  ErrorLevel = 1
      GetFreeUnitNumber = UNum
      END FUNCTION GetFreeUnitNumber
!--------------------------------------------------------------------------
      LOGICAL FUNCTION UnitIsInUse(UNum)
      IMPLICIT  NONE
      INTEGER, INTENT(IN) :: UNum
      INTEGER :: IOS
      LOGICAL :: InUse
      IF (UNum .LT. 1) THEN
         UnitIsInUse = .FALSE.
         RETURN
      END IF
      INQUIRE(UNIT=UNum, OPENED=InUse, IOSTAT=IOS)
      IF (IOS .EQ. 0) THEN
         UnitIsInUse = InUse
         RETURN
      ELSE
         UnitIsInUse = .TRUE.
         RETURN
      END IF
      END FUNCTION UnitIsInUse
!--------------------------------------------------------------------------
      LOGICAL FUNCTION UnitIsAvailable(UNum)
      IMPLICIT  NONE
      INTEGER, INTENT(IN) :: UNum
      IF (UNum .LT. 1) THEN
         UnitIsAvailable = .FALSE.
         RETURN
      END IF
      UnitIsAvailable = (.NOT. UnitIsInUse(UNum))
      RETURN
      END FUNCTION UnitIsAvailable
!--------------------------------------------------------------------------
      LOGICAL FUNCTION UserRequestedTermination()
      IMPLICIT   NONE
      LOGICAL :: Flag      
      INQUIRE(FILE='CANCEL.INT', EXIST=Flag)
      IF (Flag) THEN
         ErrorMessage = 'User canceled the process.'
         CALL PassMsg
         ErrorLevel = 9999
      END IF
      UserRequestedTermination = Flag 
      END FUNCTION UserRequestedTermination
!-------------------------------------------------------------------------
      SUBROUTINE Round(Var, Inodgt)
      IMPLICIT NONE
      REAL, INTENT(INOUT) :: Var
      INTEGER, INTENT(IN) :: Inodgt
      REAL (KIND=REAL8) ::  Varval, Xrcc
      !
      !     Round the number to the specified number of significant digits.
      !
      !     DO NOT PASS A ZERO (Var = 0) TO THIS ROUTINE!
      !
      IF (Abs(Var) .LT. 1.0e-30) THEN
         ErrorLevel = 1
         ErrorMessage = 'Cannot ROUND zero argument'
         CALL PassMsg
         RETURN
      END IF
      Varval = Var
      Xrcc = 10.0D+0**(1-Inodgt+INT(DLOG10(Varval*.999999999999999D+0)))
      IF (Varval .LE. 1.0D+0) Xrcc = Xrcc / 10.0D+0
      Varval = Varval + Xrcc / 2.0D+0
      Xrcc = 10.0D+0**(1-Inodgt+INT(DLOG10(Varval*.999999999999999D+0)))
      IF (Varval .LE. 1.0D+0) Xrcc = Xrcc / 10.0D+0
      Varval = INT(Varval * 1.00000000000001D+0 / Xrcc) * Xrcc
      Var = REAL(Varval)
      RETURN
      END SUBROUTINE Round
!-------------------------------------------------------------------------
      LOGICAL FUNCTION REQUAL(A, B)
      IMPLICIT NONE
      REAL, INTENT(IN) :: A, B
      REAL :: Epsilon, SA, SB
      
      SA = SPACING(A)
      SB = SPACING(B)
      Epsilon = MAX(SA,SB)
      IF (ABS(A-B) .LT. Epsilon) THEN
         REQUAL = .TRUE.
      ELSE
         REQUAL = .FALSE.
      END IF
      RETURN
      END FUNCTION REQUAL

!-------------------------------------------------------------------------
      LOGICAL FUNCTION IsMissing(A)
      IMPLICIT NONE
      REAL, INTENT(IN) :: A
      LOGICAL :: F1, F2
      F1 = REQUAL(A, MissingData_Real)
      F2 = A .LE. MissingData_Real_Test
      IsMissing = F1 .OR. F2
      RETURN
      END FUNCTION IsMissing

!-------------------------------------------------------------------------
      LOGICAL FUNCTION TextToLogical(S)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: S
      CHARACTER(LEN=10)  :: T10
      CHARACTER(LEN=999) :: T999
      
      T999 = TRIM(ADJUSTL(S))
      T10  = TRIM(T999(1:10))
      CALL LowerCase(T10)
      TextToLogical = .FALSE.
      IF (T10(1:4) .EQ. 'true')      TextToLogical = .TRUE.
      IF (T10(1:3) .EQ. 'yes')       TextToLogical = .TRUE.
      IF (T10(1:8) .EQ. 'positive')  TextToLogical = .TRUE.
      RETURN
      END FUNCTION TextToLogical
      
!-------------------------------------------------------------------------
      CHARACTER(LEN=5) FUNCTION LogicalToText(TorF)
      IMPLICIT NONE
      LOGICAL, INTENT(IN) :: TorF
      
      IF (TorF) THEN
         LogicalToText = 'TRUE '
      ELSE
         LogicalToText = 'FALSE'
      END IF
      RETURN
      END FUNCTION LogicalToText
      
!-------------------------------------------------------------------------
      INTEGER FUNCTION TextToInteger(S)      
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: S
      INTEGER :: I, IOS
      CHARACTER(LEN=99) :: T99
      
      T99 = TRIM(ADJUSTL(S))
      READ(T99, *, IOSTAT=IOS) I
      IF (IOS .NE. 0) I = MissingData_Int
      TextToInteger = I
      RETURN
      END FUNCTION TextToInteger
      
!-------------------------------------------------------------------------
      REAL FUNCTION TextToReal(S)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: S
      INTEGER :: IOS
      REAL    :: R
      CHARACTER(LEN=99) :: T99
      
      T99 = TRIM(ADJUSTL(S))
      READ(T99, *, IOSTAT=IOS) R
      IF (IOS .NE. 0) R = MissingData_Real
      TextToReal = R
      RETURN
      END FUNCTION TextToReal
      
      !--------------------------------------------------------------------
      !  Downloaded from http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/median.f90
      !  on Feb 11, 2016.
      !  Modified from a program into a subroutine by Tim Hunter.
      !
      !  This is an internal REAL function for computing the
      !  median of a set of input.  The median of a set of N data values is
      !  defined as follows.  First, the data values must be sorted.  Then,
      !  the median is the middle value X(N/2+1) if N is odd; otherwise, the
      !  median is the average of the middle two values (i.e., (X(n)+X(N/2+1))/2).
      !  For example, the median of 4, 2, 3, 1 is 2.5 since the sorted data
      !  values are 1, 2, 3 and 4 and the average of the middle two data
      !  values is (2+3)/2.  The median of 5, 3, 4, 1, 2 is 3 since 3 is the
      !  middle value of the sorted data 1, 2, 3, 4, 5.
      !
      !  We shall use the sorting subroutine discussed earlier.
      !
      !  REAL FUNCTION  Median() :
      !    This function receives an array X of N entries, copies its value
      !    to a local array Temp(), sorts Temp() and computes the median.
      !    The returned value is of REAL type.
      ! --------------------------------------------------------------------
      REAL FUNCTION  Median(X, N)
      IMPLICIT  NONE
      REAL, DIMENSION(1:), INTENT(IN) :: X
      INTEGER, INTENT(IN)             :: N

      INTEGER :: I
      REAL, DIMENSION(1:N) :: Temp

      DO I = 1, N                       ! make a copy
         Temp(I) = X(I)
      END DO
      CALL Sort(Temp, N)               ! sort the copy
      IF (MOD(N,2) == 0) THEN          ! compute the median
         Median = (Temp(N/2) + Temp(N/2+1)) / 2.0
      ELSE
         Median = Temp(N/2+1)
      END IF
      END FUNCTION  Median
	
      ! --------------------------------------------------------------------
      ! INTEGER FUNCTION  FindMinimum():
      !    This function returns the location of the minimum in the section
      ! between Start and End.
      ! --------------------------------------------------------------------
      INTEGER FUNCTION FindMinimum(x, Start, End)
      IMPLICIT  NONE
      REAL, DIMENSION(1:), INTENT(IN) :: x
      INTEGER, INTENT(IN)             :: Start, End
      INTEGER :: I, Location
      REAL    :: Minimum

      Minimum  = x(Start)          ! assume the first is the min
      Location = Start             ! record its position
      DO I = Start+1, End          ! start with next elements
         IF (x(I) < Minimum) THEN  !   if x(i) less than the min?
            Minimum  = x(I)        !      Yes, a new minimum found
            Location = I           !      record its position
         END IF
      END DO
      FindMinimum = Location            ! return the position
      END FUNCTION  FindMinimum
      
      ! --------------------------------------------------------------------
      ! SUBROUTINE  Swap():
      !    This subroutine swaps the values of its two formal arguments.
      ! --------------------------------------------------------------------
      SUBROUTINE Swap(a, b)
      IMPLICIT  NONE
      REAL, INTENT(INOUT) :: a, b
      REAL                :: Temp

      Temp = a
      a    = b
      b    = Temp
      END SUBROUTINE  Swap

      ! --------------------------------------------------------------------
      ! SUBROUTINE  Sort():
      !    This subroutine receives an array x() and sorts it into ascending
      ! order.
      ! --------------------------------------------------------------------
      SUBROUTINE  Sort(x, Size)
      IMPLICIT  NONE
      REAL, DIMENSION(1:), INTENT(INOUT) :: x
      INTEGER, INTENT(IN)                :: Size
      INTEGER :: I, Location

      DO I = 1, Size-1                       ! except for the last
         Location = FindMinimum(x, I, Size)  ! find min from this to last
         CALL  Swap(x(i), x(Location))       ! swap this and the minimum
      END DO
      END SUBROUTINE  Sort
	
!-----------------------------------------------------
!  These are the routines referenced in INTERFACE DayOfYear
!
!  I = DayOfYear(integer_variable) will use DayOfYearFromSeq
!  I = DayOfYear(int, int, int) will use DayOfYearFromDMY
!
       INTEGER FUNCTION DayOfYearFromSeq(Seq)
       IMPLICIT  NONE
       INTEGER, INTENT(IN) :: Seq
       INTEGER :: SeqJan1, Dy, Mn, Yr
       DayOfYearFromSeq = -9999
       CALL SequenceDate(Dy, Mn, Yr, Seq)   ; IF (ErrorLevel .NE. 0) RETURN
       CALL DateSequence(1, 1, Yr, SeqJan1) ; IF (ErrorLevel .NE. 0) RETURN
       DayOfYearFromSeq = Seq - SeqJan1 + 1
       END FUNCTION DayOfYearFromSeq

       !-------------------------------------------------------------
       INTEGER FUNCTION DayOfYearFromDMY(Dy, Mn, Yr)
       IMPLICIT  NONE
       INTEGER, INTENT(IN) :: Dy, Mn, Yr
       INTEGER :: SeqJan1, SeqNow
       DayOfYearFromDMY = -9999
       IF ((Dy .LT. 1) .OR. (Dy .GT. 31)) THEN
          ErrorMessage = 'Invalid day specified in DayOfYear() function!'
          CALL PassMsg;  GOTO 899
       END IF
       IF ((Mn .LT. 1) .OR. (Mn .GT. 12)) THEN
          ErrorMessage = 'Invalid month specified in DayOfYear() function!'
          CALL PassMsg;  GOTO 899
       END IF
       IF ((Yr .LT. 1) .OR. (Yr .GT. 9999)) THEN
          ErrorMessage = 'Invalid year specified in DayOfYear() function!'
          CALL PassMsg;  GOTO 899
       END IF
       CALL DateSequence(Dy, Mn, Yr, SeqNow)  ; IF (ErrorLevel .NE. 0) RETURN
       CALL DateSequence( 1,  1, Yr, SeqJan1) ; IF (ErrorLevel .NE. 0) RETURN
       DayOfYearFromDMY = SeqNow - SeqJan1 + 1
       RETURN

  899  ErrorLevel = 1
       RETURN
       END FUNCTION DayOfYearFromDMY

!-----------------------------------------------------
!  These are the routines referenced in INTERFACE FirstDayOfQtrMon
!
!  I = FirstDayOfQtrMon(int)           will use StartOfQtr1
!  I = FirstDayOfQtrMon(int, int)      will use StartOfQtr2
!  I = FirstDayOfQtrMon(int, int, int) will use StartOfQtr3
!
       INTEGER FUNCTION StartOfQtr1(Seq)
       IMPLICIT   NONE
       INTEGER, INTENT(IN) :: Seq
       INTEGER :: Dy, Mn, Yr
       StartOfQtr1 = -1
       CALL SequenceDate(Dy, Mn, Yr, Seq)   ; IF (ErrorLevel .NE. 0) RETURN
       StartOfQtr1 = StartOfQtr3(Dy, Mn, Yr); IF (ErrorLevel .NE. 0) RETURN
       END FUNCTION StartOfQtr1
       
       !-------------------------------------------------------------
       INTEGER FUNCTION StartOfQtr2(Days, QtrNumber)
       IMPLICIT   NONE
       INTEGER, INTENT(IN) :: Days, QtrNumber
       INTEGER :: Q(4), QQ
       StartOfQtr2 = -1
       IF (QtrNumber .EQ. 0) THEN
          StartOfQtr2 = 0
          ErrorMessage = 'Invalid parameter in routine FirstDayOfQtrMon(D,Q)'
          CALL PassMsg;  GOTO 899
       END IF
       IF (Days .EQ. 28) Q(:) = Q28(:)
       IF (Days .EQ. 29) Q(:) = Q29(:)
       IF (Days .EQ. 30) Q(:) = Q30(:)
       IF (Days .EQ. 31) Q(:) = Q31(:)
       IF (QtrNumber .EQ. 1) THEN
          StartOfQtr2 = 1
          RETURN
       END IF
       QQ = QtrNumber - 1
       StartOfQtr2 = Q(QQ) + 1
       RETURN

899    ErrorLevel = 1
       RETURN
       END FUNCTION StartOfQtr2

       !-------------------------------------------------------------
       INTEGER FUNCTION StartOfQtr3(Day, Month, Year)
       IMPLICIT   NONE
       INTEGER, INTENT(IN) :: Day, Month, Year
       INTEGER :: DD, DM, Q
       StartOfQtr3 = -1
       IF (Year .LT. 1)    GOTO 801
       IF (Year .GT. 2999) GOTO 801
       IF (Month .LT. 1)   GOTO 801
       IF (Month .GT. 12)  GOTO 801
       DD = DaysInMonth(Month, Year); IF (ErrorLevel .NE. 0) RETURN
       IF (Day .LT. 1)     GOTO 801
       IF (Day .GT. DD)    GOTO 801
       DM = DaysInMonth(Month, Year)      ; IF (ErrorLevel .NE. 0) RETURN
       Q  = QtrMonNumber(Day, Month, Year); IF (ErrorLevel .NE. 0) RETURN
       StartOfQtr3 = StartOfQtr2(DM, Q)   ; IF (ErrorLevel .NE. 0) RETURN
       RETURN

  801  ErrorMessage = 'Invalid parameter in routine FirstDayOfQtrMon(D,M,Y)'
       CALL PassMsg
       ErrorLevel = 1
       StartOfQtr3 = 0
       RETURN
       END FUNCTION StartOfQtr3

!-----------------------------------------------------
!  These are the routines referenced in INTERFACE LastDayOfQtrMon
!
!  I = LastDayOfQtrMon(int)           will use EndOfQtr1
!  I = LastDayOfQtrMon(int, int)      will use EndOfQtr2
!  I = LastDayOfQtrMon(int, int, int) will use EndOfQtr3
!
       INTEGER FUNCTION EndOfQtr1(Seq)
       IMPLICIT   NONE
       INTEGER, INTENT(IN) :: Seq
       INTEGER :: Dy, Mn, Yr
       EndOfQtr1 = -1
       CALL SequenceDate(Dy, Mn, Yr, Seq) ; IF (ErrorLevel .NE. 0) RETURN
       EndOfQtr1 = EndOfQtr3(Dy, Mn, Yr)  ; IF (ErrorLevel .NE. 0) RETURN
       END FUNCTION EndOfQtr1

       !-------------------------------------------------------------
       INTEGER FUNCTION EndOfQtr2(DaysInMon, QtrNumber)
       IMPLICIT   NONE
       INTEGER, INTENT(IN) :: DaysInMon, QtrNumber
       INTEGER :: Q(4)
       EndOfQtr2 = -1
       IF (QtrNumber .LT. 1)   GOTO 801
       IF (QtrNumber .GT. 4)   GOTO 801
       IF (DaysInMon .LT. 28)  GOTO 801
       IF (DaysInMon .GT. 31)  GOTO 801
       IF (DaysInMon .EQ. 28) Q(:) = Q28(:)
       IF (DaysInMon .EQ. 29) Q(:) = Q29(:)
       IF (DaysInMon .EQ. 30) Q(:) = Q30(:)
       IF (DaysInMon .EQ. 31) Q(:) = Q31(:)
       EndOfQtr2 = Q(QtrNumber)
       RETURN
  801  ErrorMessage = 'Invalid parameter in routine EndOfQtr2(D,Q)'
       CALL PassMsg
       EndOfQtr2 = 0
       ErrorLevel = 1
       RETURN
       END FUNCTION EndOfQtr2

       !-------------------------------------------------------------
       INTEGER FUNCTION EndOfQtr3(Day, Month, Year)
       IMPLICIT   NONE
       INTEGER, INTENT(IN) :: Day, Month, Year
       INTEGER :: DD, DM, Q
       EndOfQtr3 = -1
       IF (Year .LT. 1)    GOTO 801
       IF (Year .GT. 2999) GOTO 801
       IF (Month .LT. 1)   GOTO 801
       IF (Month .GT. 12)  GOTO 801
       DD = DaysInMonth(Month, Year); IF (ErrorLevel .NE. 0) RETURN
       IF (Day .LT. 1)     GOTO 801
       IF (Day .GT. DD)    GOTO 801
       DM = DaysInMonth(Month, Year)      ; IF (ErrorLevel .NE. 0) RETURN
       Q  = QtrMonNumber(Day, Month, Year); IF (ErrorLevel .NE. 0) RETURN
       EndOfQtr3 = EndOfQtr2(DM, Q)       ; IF (ErrorLevel .NE. 0) RETURN
       RETURN

  801  ErrorMessage = 'Invalid parameter in routine EndOfQtr3(D,M,Y)'
       CALL PassMsg
       ErrorLevel = 1
       EndOfQtr3 = 0
       RETURN

       END FUNCTION EndOfQtr3


END MODULE GLSHFS_Util





!-------------------------------------------------------------------------

