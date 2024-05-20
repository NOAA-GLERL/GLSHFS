MODULE FcstUtil

   USE GLERLUTIL

   INTEGER, PARAMETER :: NoApplications = 13
   CHARACTER (LEN=80) :: CfgDirs(9), LevDir, ThisLakeDir
   LOGICAL            :: QuietMode

   INTEGER, PARAMETER :: ClmHdr = 2       ! header records in CLM/PRV files
   INTEGER, PARAMETER :: SumHdr = 2       ! header records in *.SUM files
   INTEGER, PARAMETER :: ZSHdr  = 1       ! header records in ZSEVAP-format files

   INTEGER, PARAMETER :: MaxNoYears = 160   ! Max # of outlook scenario years

!
!  The entries in ValidLake correspond in order to the entries in LakeName3.
!       (SUP,MIC,HUR,GEO,STC,ERI,ONT,MHU,CHA,URA,EMB,VOL,HGB,***)
!  Only the first 7 are valid (the individual lakes).
!
!  08 Feb 2013: Note that I have added HGB as entry 13. This is to support the newer
!  version of the LLTM, which includes a combined HurGeo computation.
!  However... AHPS will continue to do the split lakes.  [TSH]
!
   LOGICAL, PARAMETER :: ValidLake(NoApplications+1) =           &
     (/.TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,      &
       .TRUE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.,      &
      .FALSE., .FALSE./)

   CHARACTER (LEN=2), PARAMETER :: LevLkCode2(5) =               &
                      (/'sp', 'mh', 'sc', 'er', 'on'/)

   CHARACTER (LEN=3), PARAMETER :: LevLkCode3(5) =               &
                      (/'sup', 'mhu', 'stc', 'eri', 'ont'/)

   CHARACTER(LEN=10), PARAMETER :: LevLakeName10(5) =            &
         (/'Superior  ', 'Mich-Huron', 'St. Clair ',             &
           'Erie      ', 'Ontario   '/)

   CHARACTER (LEN=2), PARAMETER :: LakeName2(NoApplications) =   &
           (/'sp', 'mi', 'hu', 'gb', 'sc', 'er', 'on', 'mh',     &
             'ch', 'ur', 'em', 'vo', 'hg'/)

   CHARACTER (LEN=3), PARAMETER :: LakeName3(NoApplications+1) = &
     (/'sup', 'mic', 'hur', 'geo', 'stc', 'eri', 'ont', 'mhu',   &
       'cha', 'ura', 'emb', 'vol', 'hgb', '***'/)

   CHARACTER(LEN=10), PARAMETER :: LakeName10(NoApplications) =  &
         (/'Superior  ', 'Michigan  ', 'Huron     ',             &
           'Georgian  ', 'St. Clair ', 'Erie      ',             &
           'Ontario   ', 'Mich-Huron', 'Champlain ',             &
           'Ural      ', 'Emba      ', 'Volga     ',             &
           'HurAndGeoB'/)

   CHARACTER (LEN=19), PARAMETER :: FullLakeName(NoApplications) =  &
           (/'Lake Superior      ', 'Lake Michigan      ',          &
             'Lake Huron (no GB) ', 'Georgian Bay       ',          &
             'Lake St. Clair     ', 'Lake Erie          ',          &
             'Lake Ontario       ', 'Lake Michigan-Huron',          &
             'Lake Champlain     ', 'Ural River         ',          &
             'Emba River         ', 'Volga River        ',          &
             'Lake Huron (and GB)'/)

   INTEGER, PARAMETER :: NumSubbasins(NoApplications) =          &
         (/22, 27, 16, 13, 7, 21, 15, 56, 1, 1, 1, 1, 29/)

!
!  These coordinated values come from the 1977 publication "Coordinated
!  Great Lakes Physical Data".  To split the Lake Huron values into
!  Huron and Georgian as we've done here I looked at our digital values
!  for lake and land areas.  The ration for each portion was applied to 
!  the coordinated value for Lake Huron (w/Georgian Bay) and the result
!  is what is given here as the "coordinated" value.   (TSH  13sep2000)
!
!  The Michigan-Huron area is the sum of the component areas AFTER they
!  were rounded.  This allows internal consistency for our calculations.
!
   REAL, PARAMETER, DIMENSION(NoApplications) :: CoordLakeArea =          &
         (/  0.8210e+11,   0.5780e+11,   0.4064e+11,   0.1896e+11,        &
             0.1114e+10,   0.2570e+11,   0.1896e+11,   1.1740e+11,        &
            -9.9999e+09,  -9.9999e+09,  -9.9999e+09,  -9.9999e+09,        &
             0.5960e+11/)

   REAL, PARAMETER, DIMENSION(NoApplications) :: CoordLandArea =          &
         (/  0.1280e+12,   0.1180e+12,   0.5120e+11,   0.8280e+11,        &
             0.1570e+11,   0.6100e+11,   0.6400e+11,   0.2520e+12,        &
            -9.9999e+09,  -9.9999e+09,  -9.9999e+09,  -9.9999e+09,        &
             0.1340e+12/)

!
!  Digital areas are based on our digital maps.
!
   REAL, PARAMETER, DIMENSION(NoApplications) :: DigitLakeArea =          &
         (/ 8.19250e+10,  5.72910e+10,  4.06110e+10,  1.89490e+10,        &
            1.10900e+09,  2.54040e+10,  1.91210e+10,  1.16851e+11,        &
            -9.9999e+09,  -9.9999e+09,  -9.9999e+09,  -9.9999e+09,        &
             5.9580e+10/)

   REAL, PARAMETER, DIMENSION(NoApplications) :: DigitLandArea =          &
         (/ 1.28084e+11,  1.15804e+11,  5.04880e+10,  8.17200e+10,        &
            1.57370e+10,  6.06020e+10,  6.51180e+10,  2.48012e+11,        &
            -9.9999e+09,  -9.9999e+09,  -9.9999e+09,  -9.9999e+09,        &
            1.32208e+11/)

!
!  LakeVolume comes from Croley code. SUP, MIC, ERI, ONT and HGB match
!  the 1977 Coordinating Committee report. I'm not sure how he arrived
!  at the HUR/GEO values. I computed the MHU quite simply by summing MIC
!  and HGB.
!
   REAL, PARAMETER, DIMENSION(NoApplications) :: CoordLakeVolume =       &
         (/ 0.1210e+14,   0.4920e+13,   0.2761e+13,   0.0779e+13,        &
            0.3400e+10,   0.4840e+12,   0.1640e+13,   0.8460e+13,        &
           -9.9999e+09,  -9.9999e+09,  -9.9999e+09,  -9.9999e+09,        &
            0.3540e+13/)
            
            
!
!  Max depths were taken from the 1977 Coordinating Committee
!  document for SUP, MIC, HUR, STC, ERI, ONT. Georgian Bay came from
!  Encyclopaedia Brittanica (online). Mich-Hur and HGB are just max
!  depth of the deepest component.  Note that the STC max depth is
!  exclusive of the dredged navigation channel, which is slightly deeper.
!
   REAL, PARAMETER, DIMENSION(NoApplications) :: LakeDepthMaxMeters =     &
         (/ 405.0,  281.0,  229.0,  165.0,    6.0,   64.0,  244.0,        &
            281.0, -999.9, -999.9, -999.9, -999.9,  229.0/)
            

   CHARACTER (LEN=3), PARAMETER, DIMENSION(12) :: MonthName3 =            &
         (/'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',                      &
           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/)

   CHARACTER (LEN=10), PARAMETER, DIMENSION(12) :: MonthName10 =          &
         (/'January   ', 'February  ', 'March     ', 'April     ',        &
           'May       ', 'June      ', 'July      ', 'August    ',        &
           'September ', 'October   ', 'November  ', 'December  '/)

CONTAINS

!------------------------------------------------------------------
     !  The CfgDirs array gets filled as follows:
     !    CfgDirs(1) = Lake Superior
     !    CfgDirs(2) = Lake Michigan
     !    CfgDirs(3) = Lake Huron
     !    CfgDirs(4) = Georgian Bay
     !    CfgDirs(5) = Lake StClair
     !    CfgDirs(6) = Lake Erie
     !    CfgDirs(7) = Lake Ontario
     !    CfgDirs(8) = Lake Michigan-Huron
     !    CfgDirs(9) = Level (CGLRRM stuff)
     !
     !  All of directory specifications will end with a trailing "\" (or "/" in linux)
     !  so that they can be used along with a filename and the main
     !  programs won't have to worry about the directory separator characters.
     !
     SUBROUTINE ReadCfgFile(CfgFile)
     IMPLICIT  NONE
     CHARACTER (LEN=*) :: CfgFile
     INTEGER  :: UNum, IOS, J
     CHARACTER (LEN=1)  :: DC
     CHARACTER (LEN=80) :: Line, Varbl, InVal
     
     CfgDirs(:) = ''

!
!  Directory character
!
#if defined(LINUX)
     DC = '/'
#else
     DC = '\'
#endif

     UNum = GetFreeUnitNumber();  IF (ERRORLEVEL .NE. 0) GOTO 899
     OPEN(UNIT=UNum, FILE=CfgFile, STATUS='OLD', ERR=801)
     CALL FileWasOpened(UNum);    IF (ERRORLEVEL .NE. 0) GOTO 899

     READ(UNum, 1000, IOSTAT=IOS) Line
     DO WHILE (IOS .EQ. 0)
        CALL ParseCfgLine(Line, Varbl, InVal)
        CALL Caps(Varbl)
        J = LEN_TRIM(InVal)
        IF (J .GT. 0) THEN
           IF (InVal(J:J) .NE. DC) InVal = TRIM(InVal) // DC
           IF (Varbl(1:6) .EQ. 'SUPDIR') CfgDirs(1) = InVal
           IF (Varbl(1:6) .EQ. 'MICDIR') CfgDirs(2) = InVal
           IF (Varbl(1:6) .EQ. 'HURDIR') CfgDirs(3) = InVal
           IF (Varbl(1:6) .EQ. 'GEODIR') CfgDirs(4) = InVal
           IF (Varbl(1:6) .EQ. 'STCDIR') CfgDirs(5) = InVal
           IF (Varbl(1:6) .EQ. 'ERIDIR') CfgDirs(6) = InVal
           IF (Varbl(1:6) .EQ. 'ONTDIR') CfgDirs(7) = InVal
           IF (Varbl(1:6) .EQ. 'MHUDIR') CfgDirs(8) = InVal
           IF (Varbl(1:6) .EQ. 'LEVDIR') CfgDirs(9) = InVal
        ENDIF
        Line = ''
        READ(UNum, 1000, IOSTAT=IOS) Line
     ENDDO
     CLOSE(UNum)
     CALL FileWasClosed(UNum);    IF (ERRORLEVEL .NE. 0) GOTO 899
     LevDir = CfgDirs(9)
     RETURN

 801 ErrorMessage = 'Error opening forecast configuration file'
     CALL PassMsg;  GOTO 899
 899 ErrorLevel = 1
     RETURN

 1000 FORMAT(A)

     END SUBROUTINE ReadCfgFile

!------------------------------------------------------------------
      SUBROUTINE GetLakeNumAndDirFromName(Lake, LakeNum, LakeDir)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: Lake
      INTEGER,          INTENT(OUT) :: LakeNum
      CHARACTER(LEN=*), INTENT(OUT) :: LakeDir
      
      INTEGER :: I
      CHARACTER(LEN=3) :: S1, S2
      CHARACTER(LEN=99) :: S99
      
      LakeNum = -1
      LakeDir = ''
      S99 = TRIM(ADJUSTL(Lake)) 
      S1 = S99(1:3)
      CALL LowerCase(S1)
      LakeDir = ''
      DO I = 1, 8
         S2 = LakeName3(I)
         CALL LowerCase(S2)
         IF (S2 .EQ. S1) THEN
            LakeNum = I
            LakeDir = TRIM(CfgDirs(I))
            RETURN
         END IF
      END DO

      END SUBROUTINE GetLakeNumAndDirFromName
     
     
!------------------------------------------------------------------
     SUBROUTINE MakeErrorInt
     IMPLICIT  NONE
     INTEGER :: U
     U = 2222
     OPEN(UNIT=U, FILE='ERROR.INT', STATUS='OLD', ERR=10)
     CLOSE(U, STATUS='DELETE')
 10  OPEN(UNIT=U, FILE='ERROR.INT', STATUS='NEW')
     WRITE(U, *) 'This ERROR.INT file created by AHPS forecast package'
     CLOSE(U)
     END SUBROUTINE MakeErrorInt

!------------------------------------------------------------------
!  ReadLevel reads a single lake level from the specified file for
!  the specified date.
!
     SUBROUTINE ReadLevel(FName, SeqNum, Value)
     IMPLICIT  NONE
     INTEGER,           INTENT(IN)  :: SeqNum
     CHARACTER (LEN=*), INTENT(IN)  :: FName
     REAL,              INTENT(OUT) :: Value

     INTEGER  :: J, U, SSeq, ESeq
     REAL     :: LevVal

     U = GetFreeUnitNumber();   IF (ErrorLevel .NE. 0) GOTO 899
     OPEN(UNIT=U, FILE=TRIM(FName), STATUS='OLD', RECL=4,              &
          ACCESS='DIRECT', FORM='UNFORMATTED', ERR=801)
     CALL FileWasOpened(U);    IF (ERRORLEVEL .NE. 0) GOTO 899
     READ(U, REC=1, ERR=802) SSeq
     READ(U, REC=2, ERR=802) ESeq
     IF ((SeqNum .LT. SSeq) .OR. (SeqNum .GT. ESeq)) THEN
        ErrorMessage = 'Requested daily lake level is out of the ' //   &
                       'range of the file '//TRIM(FName)
        CALL PassMsg
        GOTO 899
     END IF
     J = SeqNum - SSeq + 3     ! record number in the file
     READ(U, REC=J, ERR=802) LevVal
     CALL FileWasClosed(U);    IF (ERRORLEVEL .NE. 0) GOTO 899
     Value = LevVal
     RETURN

 801 ErrorMessage = 'Error opening file '//TRIM(FName)
     CALL PassMsg;  GOTO 899
 802 ErrorMessage = 'Error reading file '//TRIM(FName)
     CALL PassMsg;  GOTO 899
 899 ErrorLevel = 1
     RETURN

     END SUBROUTINE ReadLevel

!------------------------------------------------------------------
!  ReadCglMon reads a single value from the specified file.  The file
!  is assumed to be in the monthly data format defined for the CGLRRM.
!  It is returned as a REAL value.
!
     SUBROUTINE ReadCglMon(FName, Month, Year, Value)
     IMPLICIT  NONE
     INTEGER,           INTENT(IN)  :: Month, Year
     CHARACTER (LEN=*), INTENT(IN)  :: FName
     REAL,              INTENT(OUT) :: Value

     INTEGER  :: U, IOS, IOS2, M, Y
     LOGICAL  :: Found
     REAL     :: Vals(12)
     CHARACTER (LEN=200) :: Line

     U = GetFreeUnitNumber();   IF (ErrorLevel .NE. 0) GOTO 899
     OPEN(UNIT=U, FILE=TRIM(FName), STATUS='OLD', ERR=801)
     CALL FileWasOpened(U);    IF (ERRORLEVEL .NE. 0) GOTO 899
     Found = .FALSE.
     READ(U, 1000, IOSTAT=IOS) Line
     DO WHILE ((IOS .EQ. 0) .AND. (.NOT. Found))
        READ(Line, 1111, IOSTAT=IOS2) Y, (Vals(M), M=1,12)
        IF ((IOS2 .EQ. 0) .AND. (Y .EQ. Year)) THEN
           Value = Vals(Month)
           Found = .TRUE.
        END IF
        READ(U, 1000, IOSTAT=IOS) Line
     END DO
     CLOSE(U)
     CALL FileWasClosed(U);    IF (ERRORLEVEL .NE. 0) GOTO 899
     IF (Found) RETURN
     ErrorMessage = 'Unable to find requested value in '//TRIM(FName)
     CALL PassMsg
     GOTO 899

 801 ErrorMessage = 'Error opening file '//TRIM(FName)
     CALL PassMsg;  GOTO 899
 899 ErrorLevel = 1
     RETURN

 1000 FORMAT(A)
 1111 FORMAT(I4, 12F8.0)

     END SUBROUTINE ReadCglMon

!------------------------------------------------------------------
!  ReadCglQtr reads a single value from the specified file.  The file
!  is assumed to be in the quarter-monthly data format defined for the CGLRRM.
!  It is returned as a REAL value.
!
     SUBROUTINE ReadCglQtr(FName, Qtr, Month, Year, Value)
     IMPLICIT  NONE
     INTEGER,           INTENT(IN)  :: Qtr, Month, Year
     CHARACTER (LEN=*), INTENT(IN)  :: FName
     REAL,              INTENT(OUT) :: Value

     INTEGER  :: U, IOS, IOS2, Q, M, Y
     LOGICAL  :: Found
     REAL     :: Vals(12)
     CHARACTER (LEN=200) :: Line

     U = GetFreeUnitNumber();   IF (ErrorLevel .NE. 0) GOTO 899
     OPEN(UNIT=U, FILE=TRIM(FName), STATUS='OLD', ERR=801)
     CALL FileWasOpened(U);    IF (ERRORLEVEL .NE. 0) GOTO 899
     Found = .FALSE.
     READ(U, 1000, IOSTAT=IOS) Line
     DO WHILE ((IOS .EQ. 0) .AND. (.NOT. Found))
        READ(Line, 1111, IOSTAT=IOS2) Y, Q, (Vals(M), M=1,12)
        IF ((IOS2 .EQ. 0) .AND. (Y .EQ. Year) .AND. (Q .EQ. Qtr)) THEN
           Value = Vals(Month)
           Found = .TRUE.
        END IF
        READ(U, 1000, IOSTAT=IOS) Line
     END DO
     CLOSE(U)
     CALL FileWasClosed(U);    IF (ERRORLEVEL .NE. 0) GOTO 899
     IF (Found) RETURN
     ErrorMessage = 'Unable to find requested value in '//TRIM(FName)
     CALL PassMsg
     GOTO 899

 801 ErrorMessage = 'Error opening file '//TRIM(FName)
     CALL PassMsg;  GOTO 899
 899 ErrorLevel = 1
     RETURN

 1000 FORMAT(A)
 1111 FORMAT(I4, 2I2, 12F8.0)

     END SUBROUTINE ReadCglQtr

!------------------------------------------------------------------
!  ReadCglDay reads a single value from the specified file.  The file
!  is assumed to be in the daily data format defined for the CGLRRM.
!  It is returned as a REAL value.
!
     SUBROUTINE ReadCglDay(FName, Day, Month, Year, Value)
     IMPLICIT  NONE
     INTEGER,           INTENT(IN)  :: Day, Month, Year
     CHARACTER (LEN=*), INTENT(IN)  :: FName
     REAL,              INTENT(OUT) :: Value

     INTEGER  :: U, IOS, IOS2, I, Q, M, Y, Qtr, DoQ
     LOGICAL  :: Found
     REAL     :: Vals(12)
     CHARACTER (LEN=200) :: Line

     Qtr = QtrMonNumber(Day, Month, Year)
     DoQ = DayOfQtrMon(Day, Month, Year)

     U = GetFreeUnitNumber();   IF (ErrorLevel .NE. 0) GOTO 899
     OPEN(UNIT=U, FILE=TRIM(FName), STATUS='OLD', ERR=801)
     CALL FileWasOpened(U);    IF (ERRORLEVEL .NE. 0) GOTO 899
     Found = .FALSE.
     READ(U, 1000, IOSTAT=IOS) Line
     DO WHILE ((IOS .EQ. 0) .AND. (.NOT. Found))
        READ(Line, 1111, IOSTAT=IOS2) Y, M, Q, (Vals(I), I=1,12)
        IF ((IOS2 .EQ. 0) .AND. (Y .EQ. Year) .AND.             &
               (M .EQ. Month) .AND. (Q .EQ. Qtr)) THEN
           Value = Vals(DoQ)
           Found = .TRUE.
        END IF
        READ(U, 1000, IOSTAT=IOS) Line
     END DO
     CLOSE(U)
     CALL FileWasClosed(U);    IF (ERRORLEVEL .NE. 0) GOTO 899
     IF (Found) RETURN
     ErrorMessage = 'Unable to find requested value in '//TRIM(FName)
     CALL PassMsg
     GOTO 899

 801 ErrorMessage = 'Error opening file '//TRIM(FName)
     CALL PassMsg;  GOTO 899
 899 ErrorLevel = 1
     RETURN

 1000 FORMAT(A)
 1111 FORMAT(I4, 2I2, 12F8.0)

     END SUBROUTINE ReadCglDay

!------------------------------------------------------------------
     INTEGER FUNCTION GetLakeNumber(Bsn)
     IMPLICIT  NONE
     CHARACTER (LEN=*), INTENT(IN) :: Bsn

     INTEGER :: I
     CHARACTER (LEN=3)  :: S3

     GetLakeNumber = -1       ! default bad value

     S3 = 'xxx'
     IF (LEN(Bsn) .GE. 3) S3 = ADJUSTL(Bsn(1:3))
     IF (S3 .EQ. 'xxx') RETURN

     CALL Small(S3)
     DO I = 1, NoApplications
        IF (S3 .EQ. LakeName3(I)) GetLakeNumber = I
     END DO
     RETURN
     
     END FUNCTION GetLakeNumber

!------------------------------------------------------------------
     INTEGER FUNCTION GetLevLakeNumber(Bsn)
     IMPLICIT  NONE
     CHARACTER (LEN=*), INTENT(IN) :: Bsn

     INTEGER :: I
     CHARACTER (LEN=3)  :: S3

     GetLevLakeNumber = -1       ! default bad value

     S3 = 'xxx'
     IF (LEN(Bsn) .GE. 3) S3 = ADJUSTL(Bsn(1:3))
     IF (S3 .EQ. 'xxx') RETURN

     CALL Small(S3)
     DO I = 1, NoApplications
        IF (S3 .EQ. LevLkCode3(I)) GetLevLakeNumber = I
     END DO
     RETURN
     
     END FUNCTION GetLevLakeNumber



!----------------------------------------------------------------------------
      SUBROUTINE ReadSIMULATEfile (BnS, FrcstStrt, HistDay, HistMonth,    &
     &                             CrrntDate, Length, NmbrYears, Years)
!
!     Reads SIMULATE file, checking validity of entries.
!
      IMPLICIT NONE

      INTEGER,           INTENT(OUT) :: FrcstStrt, HistDay, HistMonth
      INTEGER,           INTENT(OUT) :: CrrntDate, Length, NmbrYears
      INTEGER,           INTENT(OUT) :: Years(1:MaxNoYears)
      CHARACTER (LEN=3), INTENT(OUT) :: BnS

      INTEGER :: I, IOS, U1, U2, Day, Month, Year, NumBsn
      LOGICAL :: Found
      CHARACTER (LEN=1)  :: SimFormat
      CHARACTER (LEN=10) :: Lake, Lake3
      CHARACTER (LEN=99) :: File1, File2

!
!     Test for presence of SIMULATE file.
!
      File1 = TRIM(ThisLakeDir) // 'SIMULATE'
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(File1), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

!
!     Read first line.  If it is a number (I3 format), then this SIMULATE
!     file is in format "B" (used by Derivative Outlook Weights application).
!     In this case we will NOT use the basin name from the SIMULATE file, but
!     will get it from the BSNNM file.  As long as the SIMULATE file contains 
!     that same basin as one entry in its list we can continue.  (TSH, 26jun2001)
!
      READ(U1, 3000, ERR=812, END=812) Lake     ! assume it is a lake name (format A)
      READ(Lake, '(I3)', IOSTAT=IOS) NumBsn     ! if first 3 chars = numeric then it's format B
      SimFormat = 'A'
      IF (IOS .EQ. 0) SimFormat = 'B'
      IF (SimFormat .EQ. 'B') THEN
         File2 = TRIM(ThisLakeDir) // 'BSNNM'
         U2 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
         OPEN(UNIT=U2, FILE=TRIM(File2), STATUS='OLD', RECL=12,           &
     &        ACCESS='DIRECT', FORM='FORMATTED', ERR=821)
         CALL FileWasOpened(U2); IF (ErrorLevel .NE. 0) GOTO 899
         READ(U2, 3001, REC=1, ERR=822) Lake                        ! full lake name
         Lake = TRIM(ADJUSTL(Lake))
         CALL Small(Lake);  IF (ErrorLevel .NE. 0) GOTO 899
         CLOSE(U2)
         CALL FileWasClosed(U2); IF (ErrorLevel .NE. 0) GOTO 899
         Found = .FALSE.
         DO I = 1, NumBsn
            READ(U1, 3000, ERR=812) Lake3
            IF (Lake3 .EQ. Lake(1:3)) Found = .TRUE.
         END DO
         IF (.NOT. Found) THEN
            ErrorMessage = 'SIMULATE file is not valid for this lake ('//TRIM(Lake)//')'
            CALL PassMsg
            GOTO 898
         END IF
      ELSE
         Lake = TRIM(ADJUSTL(Lake))
         CALL Small(Lake);  IF (ErrorLevel .NE. 0) GOTO 899
         IF (LEN_TRIM(Lake) .LT. 3) THEN 
            WRITE(ErrorMessage, 9001)
            CALL PassMsg
            GOTO 898
         END IF
      END IF

!
!     Get and check lake basin name.
!
      BnS = Lake(1:3)
      I = 1
      DO WHILE (I .LE. NoApplications .AND. BnS .NE. LakeName3(I))
         I = I + 1
      END DO
      IF (I .GT. NoApplications .OR. .NOT. ValidLake(I)) THEN
         WRITE(ErrorMessage, 9001)
         CALL PassMsg
         GOTO 898
      END IF

!
!     Get dates & forecast length.
!
      READ(U1, 3002, ERR=812, END=812) Day, Month, Year
      CALL DateSequence(Day, Month, Year, FrcstStrt); IF (ErrorLevel .NE. 0) GOTO 899
      READ(U1, 3002, ERR=812, END=812) HistDay, HistMonth
      READ(U1, 3002, ERR=812, END=812) Day, Month, Year
      CALL DateSequence(Day, Month, Year, CrrntDate); IF (ErrorLevel .NE. 0) GOTO 899
      READ(U1, 3003, ERR=812, END=812) Length
      IF (SimFormat .EQ. 'A') THEN
         READ(U1, 3004, ERR=812, END=812) NmbrYears
      ELSE
         READ(U1, 3002, ERR=812, END=812) NmbrYears
      END IF
      DO I = 1, NmbrYears
         READ(U1, 3004, ERR=812, END=812) Years(I)
      END DO
      CLOSE (U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      RETURN

!
!  Handlers for I/O errors
!
 811  ErrorMessage = 'FRCST: Error opening file '      // TRIM(File1)
      CALL PassMsg; GOTO 898
 812  ErrorMessage = 'FRCST: Error reading from file ' // TRIM(File1)
      CALL PassMsg; GOTO 898
 821  ErrorMessage = 'FRCST: Error opening file '      // TRIM(File2)
      CALL PassMsg; GOTO 898
 822  ErrorMessage = 'FRCST: Error reading from file ' // TRIM(File2)
      CALL PassMsg; GOTO 898

 898  ErrorLevel = 1
 899  ErrorMessage = '[traceback] : ReadSimulateFile...'
      CALL PassMsg
      RETURN

!
! FORMATs
!
 3000 FORMAT(A)
 3001 FORMAT(A10)
 3002 FORMAT(2I3, I5)
 3003 FORMAT(I5)
 3004 FORMAT(I4)

 9001 FORMAT('FRCST: Invalid lake named in SIMULATE file!')

      END SUBROUTINE ReadSIMULATEfile



END MODULE FcstUtil
