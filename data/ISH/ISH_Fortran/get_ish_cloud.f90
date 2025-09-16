!-----------------------------------------------------------------------------------------
!  Special-purpose program to extract every cloud cover data field from the ISH dataset
!  that is available from NOAA's ftp site.  I am running into all sorts of issues with the
!  cloud cover data, and this tool will let me look at every possible field and hopefully
!  determine what we should actually use for our model driver.
!
!  The field I originally used (sky cover summations) started throwing all missing or zero
!  on August 1, 2013 (for many/most stations). Deeper investigation confirmed that the issue
!  is in the data set, and not a coding error on my part. So I tried using cloud layer summations, 
!  which initially seemed to be better, and is how Greg Lang said he does it. But now I am finding 
!  that for a lot of stations, those are all missing or 100, or missing or 0. So that approach 
!  failed (or I coded something wrong). This program should help me determine:
!    a) Do I have a coding error?
!    b) Are there widespread problems in the underlying data set?
!    c) What field is consistent/useable?
!
!  Tim Hunter - 2015 Oct 30
!-----------------------------------------------------------------------------------------

   MODULE Get_ISH_Cloud_Subs
      USE GlerlUtil
      USE MetDataTypesAndUnits
      USE DailyMetStationDataset
      USE ReadWriteMetCsv

      TYPE ObsTimeValue
         INTEGER :: ObsDate             !  YYYYMMDD as a single integer value
         INTEGER :: ObsTime             !  HHMM as a single integer value
         REAL    :: DVal
      END TYPE ObsTimeValue

      TYPE CloudDataFromOneLine
         CHARACTER (LEN=6) :: USAF
         CHARACTER (LEN=5) :: WBAN
         INTEGER :: ObsDate             !  YYYYMMDD as a single integer value
         INTEGER :: ObsTime             !  HHMM as a single integer value
         INTEGER :: DateSeq             !  Date as a sequence number
         INTEGER :: Time                !  HHMM as a single integer
         REAL    :: Lat                 !  latitude in decimal degrees north of equator
         REAL    :: Long                !  longitude in decimal degrees east of prime meridian
         INTEGER :: Method1, Method2    !  cloud value using alternative methods
         INTEGER :: Method3             !  cloud value using alternative methods
         INTEGER :: SkyCoverLayerPct(6) !
         LOGICAL :: SkyCoverLayerBad(6) !
         INTEGER :: SkyCoverSumPct(6)   !
         LOGICAL :: SkyCoverSumBad(6)   !
         INTEGER :: SkyTotalCoverPct    !
         INTEGER :: SkyTotalOpaquePct   !
         LOGICAL :: SkyConditionBad     !
         CHARACTER(LEN=13) :: RawGA(6)  !
         CHARACTER(LEN=12) :: RawGD(6)  !
         CHARACTER(LEN=5)  :: RawGF     !
      END TYPE CloudDataFromOneLine

      
      LOGICAL :: DetailedOutput
      
   CONTAINS
   
   
!--------------------------------------------------------------------
      SUBROUTINE ParseDataLine(Line, DFOL, ErrStat)
      IMPLICIT NONE
      CHARACTER(LEN=*),            INTENT(IN)    :: Line
      TYPE (CloudDataFromOneLine), INTENT(INOUT) :: DFOL
      INTEGER,                     INTENT(OUT)   :: ErrStat
      
      INTEGER :: I, J, IOS, VarChars, YYYY, MM, DD, HH, NN
      INTEGER :: SkyCode1, SkyCode2, SkyQC, MaxC
      LOGICAL :: OK
      CHARACTER(LEN=3)    :: I3
      CHARACTER(LEN=5)    :: S5
      CHARACTER(LEN=12)   :: S12
      CHARACTER(LEN=13)   :: S13
      CHARACTER(LEN=9999) :: VLine
      
      ErrStat = 0
      !
      !  Initialize the DFOL fields to missing data
      !
      CALL ClearDFOL(DFOL)
      
      !
      !  Read the Mandatory data section. 
      !  No cloud data there, just ID and date/time stuff.
      !
      READ(Line, 1000, IOSTAT=IOS) VarChars, DFOL%USAF, DFOL%WBAN,        &
                 YYYY, MM, DD, HH, NN
      IF (IOS .NE. 0) THEN
         CALL ClearDFOL(DFOL)
         ErrStat = 1
         RETURN
      END IF

      !
      !  Adjust to local time from UTC (simple 5-hour shift for now)
      !  
      CALL TranslateUTC_Local(YYYY, MM, DD, HH)
      
      !
      !  Build the big date and time integer values
      !
      
      DFOL%ObsDate = (YYYY * 10000) + (MM * 100) + DD 
      DFOL%ObsTime = (HH * 100) + NN
                     
      !
      !  Fill the date/time fields in DFOL
      !
      CALL DateSequence(DD, MM, YYYY, DFOL%DateSeq)
      IF (ErrorLevel .NE. 0) THEN
         CALL ClearDFOL(DFOL)
         ErrStat = 2
         RETURN
      END IF
      
      !
      !  Extract the additional data into a separate string variable
      !      
      J = 105 + VarChars
      VLine = TRIM(Line(106:J))

      !
      !  Search the additional data section for 'sky-cover-layer' entries.
      !  These are denoted by identifier GA1-GA6.
      !  I will ignore the height information since all I care about is sky cover AMOUNT.
      !  From the documentation:
      !   00: None, SKC or CLR
      !   01: One okta - 1/10 or less but not zero
      !   02: Two oktas - 2/10 - 3/10, or FEW
      !   03: Three oktas - 4/10
      !   04: Four oktas - 5/10, or SCT
      !   05: Five oktas - 6/10
      !   06: Six oktas - 7/10 - 8/10
      !   07: Seven oktas - 9/10 or more but not 10/10, or BKN
      !   08: Eight oktas - 10/10, or OVC
      !   09: Sky obscured, or cloud amount cannot be estimated
      !   10: Partial obscuration
      !   99: Missing
      !
      !
      DFOL%SkyCoverLayerPct(:) = -99
      DFOL%SkyCoverLayerBad(:) = .FALSE.
      DO I = 1, 6
         WRITE(I3, 1500) 'GA', I      ! 3-char identifier for the field
         J = INDEX(VLine, I3)
         IF (J .GT. 0) THEN
            S13 = VLine(J+3:J+15)
            DFOL%RawGA(I) = S13
            READ(S13, 1101, IOSTAT=IOS) SkyCode1, SkyQC
            IF (IOS .EQ. 0) THEN
               OK = .TRUE.
               !
               !  The Quality Code is used/interpreted as follows:
               !   '0'   :  keep
               !   '1'   :  keep
               !   '2'   :  throw away
               !   '3'   :  throw away
               !   '4'   :  keep
               !   '5'   :  keep
               !   '6'   :  throw away
               !   '7'   :  throw away
               !   '9'   :  keep
               !
               IF (SkyQC .EQ. 2) OK = .FALSE.
               IF (SkyQC .EQ. 3) OK = .FALSE.
               IF (SkyQC .EQ. 6) OK = .FALSE.
               IF (SkyQC .EQ. 7) OK = .FALSE.
               
               IF (OK) THEN
                  IF (SkyCode1 .EQ. 0) DFOL%SkyCoverLayerPct(I) = 0
                  IF (SkyCode1 .EQ. 1) DFOL%SkyCoverLayerPct(I) = 5   ! halfway between 1 and 10
                  IF (SkyCode1 .EQ. 2) DFOL%SkyCoverLayerPct(I) = 25  ! halfway between 20 and 30
                  IF (SkyCode1 .EQ. 3) DFOL%SkyCoverLayerPct(I) = 40  
                  IF (SkyCode1 .EQ. 4) DFOL%SkyCoverLayerPct(I) = 50
                  IF (SkyCode1 .EQ. 5) DFOL%SkyCoverLayerPct(I) = 60
                  IF (SkyCode1 .EQ. 6) DFOL%SkyCoverLayerPct(I) = 75  ! halfway between 60 and 70
                  IF (SkyCode1 .EQ. 7) DFOL%SkyCoverLayerPct(I) = 95  ! halfway between 90 and 99
                  IF (SkyCode1 .EQ. 8) DFOL%SkyCoverLayerPct(I) = 100
               ELSE
                  DFOL%SkyCoverLayerPct(I) = -99
                  DFOL%SkyCoverLayerBad(I) = .TRUE.
               END IF
            END IF
         END IF
      END DO
      
      !
      !  Search the additional data section for 'sky-cover-summation' entries.
      !  These are denoted by identifier GD1-GD6.
      !  I will ignore the height information since all I care about is sky cover AMOUNT.
      !  Look at SkyCode1 first. Then SkyCode2. That way we preferentially use SkyCode2.
      !
      !  For SkyCode1 I will only use values 0-4.
      !     5 and 6 are descriptive values that I cannot interpret to a %
      !     9 = missing
      !
      !  For SkyCode2 I will only use the values from 0-8. 
      !     9     = 'unable to estimate'
      !     10-19 = descriptive stuff that I cannot interpret into a %
      !     99    = missing
      !
      DFOL%SkyCoverSumPct(:) = -99
      DFOL%SkyCoverSumBad(:) = .FALSE.
      DO I = 1, 6
         WRITE(I3, 1500) 'GD', I      ! 3-char identifier for the field
         J = INDEX(VLine, I3)
         IF (J .GT. 0) THEN
            S12 = VLine(J+3:J+14)
            DFOL%RawGD(I) = S12
            READ(S12, 1102, IOSTAT=IOS) SkyCode1, SkyCode2, SkyQC
            IF (IOS .EQ. 0) THEN
               !
               !  The Quality Code is used/interpreted as follows:
               !   '0'   :  keep
               !   '1'   :  keep
               !   '2'   :  throw away
               !   '3'   :  throw away
               !   '4'   :  keep
               !   '5'   :  keep
               !   '6'   :  throw away
               !   '7'   :  throw away
               !   '9'   :  keep
               !
               OK = .TRUE.
               IF (SkyQC .EQ. 2) OK = .FALSE.
               IF (SkyQC .EQ. 3) OK = .FALSE.
               IF (SkyQC .EQ. 6) OK = .FALSE.
               IF (SkyQC .EQ. 7) OK = .FALSE.
               IF (OK) THEN
                  IF (SkyCode1 .EQ. 0) DFOL%SkyCoverSumPct(I) = 0
                  IF (SkyCode1 .EQ. 1) DFOL%SkyCoverSumPct(I) = 13  ! halfway between 1 and 25
                  IF (SkyCode1 .EQ. 2) DFOL%SkyCoverSumPct(I) = 43  ! halfway between 37 and 50
                  IF (SkyCode1 .EQ. 3) DFOL%SkyCoverSumPct(I) = 69  ! halfway between 50 and 88
                  IF (SkyCode1 .EQ. 4) DFOL%SkyCoverSumPct(I) = 100

                  IF (SkyCode2 .EQ. 0) DFOL%SkyCoverSumPct(I) = 0
                  IF (SkyCode2 .EQ. 1) DFOL%SkyCoverSumPct(I) = 5
                  IF (SkyCode2 .EQ. 2) DFOL%SkyCoverSumPct(I) = 25
                  IF (SkyCode2 .EQ. 3) DFOL%SkyCoverSumPct(I) = 40
                  IF (SkyCode2 .EQ. 4) DFOL%SkyCoverSumPct(I) = 50
                  IF (SkyCode2 .EQ. 5) DFOL%SkyCoverSumPct(I) = 60
                  IF (SkyCode2 .EQ. 6) DFOL%SkyCoverSumPct(I) = 75
                  IF (SkyCode2 .EQ. 7) DFOL%SkyCoverSumPct(I) = 95
                  IF (SkyCode2 .EQ. 8) DFOL%SkyCoverSumPct(I) = 100
               ELSE
                  DFOL%SkyCoverSumPct(I) = -99
                  DFOL%SkyCoverSumBad(I) = .TRUE.
               END IF
            END IF
         END IF
      END DO
      

      !
      !  Search the additional data section for 'sky-condition-observation' entries.
      !  These are denoted by identifier GF1.  This is the field that I orginally used
      !  for many years and now has started to be "weird". The guy at NCDC agreed with
      !  me, but had no more info.
      !
      DFOL%SkyTotalCoverPct  = -99
      DFOL%SkyTotalOpaquePct = -99
      DFOL%SkyConditionBad = .FALSE.
      J = INDEX(VLine, 'GF1')
      IF (J .GT. 0) THEN
         S5 = VLine(J+3:J+7)
         DFOL%RawGF = S5
         READ(S5, 1103, IOSTAT=IOS) SkyCode1, SkyCode2, SkyQC
         IF (IOS .EQ. 0) THEN
            !
            !  The Quality Code is used/interpreted as follows:
            !   '0'   :  keep
            !   '1'   :  keep
            !   '2'   :  throw away
            !   '3'   :  throw away
            !   '4'   :  keep
            !   '5'   :  keep
            !   '6'   :  throw away
            !   '7'   :  throw away
            !   '9'   :  keep
            !
            OK = .TRUE.
            IF (SkyQC .EQ. 2) OK = .FALSE.
            IF (SkyQC .EQ. 3) OK = .FALSE.
            IF (SkyQC .EQ. 6) OK = .FALSE.
            IF (SkyQC .EQ. 7) OK = .FALSE.
            IF (OK) THEN
               IF (SkyCode1 .EQ. 0) DFOL%SkyTotalCoverPct = 0
               IF (SkyCode1 .EQ. 1) DFOL%SkyTotalCoverPct = 5
               IF (SkyCode1 .EQ. 2) DFOL%SkyTotalCoverPct = 25
               IF (SkyCode1 .EQ. 3) DFOL%SkyTotalCoverPct = 40
               IF (SkyCode1 .EQ. 4) DFOL%SkyTotalCoverPct = 50
               IF (SkyCode1 .EQ. 5) DFOL%SkyTotalCoverPct = 60
               IF (SkyCode1 .EQ. 6) DFOL%SkyTotalCoverPct = 75
               IF (SkyCode1 .EQ. 7) DFOL%SkyTotalCoverPct = 95
               IF (SkyCode1 .EQ. 8) DFOL%SkyTotalCoverPct = 100

               IF (SkyCode2 .EQ. 0) DFOL%SkyTotalOpaquePct = 0
               IF (SkyCode2 .EQ. 1) DFOL%SkyTotalOpaquePct = 5
               IF (SkyCode2 .EQ. 2) DFOL%SkyTotalOpaquePct = 25
               IF (SkyCode2 .EQ. 3) DFOL%SkyTotalOpaquePct = 40
               IF (SkyCode2 .EQ. 4) DFOL%SkyTotalOpaquePct = 50
               IF (SkyCode2 .EQ. 5) DFOL%SkyTotalOpaquePct = 60
               IF (SkyCode2 .EQ. 6) DFOL%SkyTotalOpaquePct = 75
               IF (SkyCode2 .EQ. 7) DFOL%SkyTotalOpaquePct = 95
               IF (SkyCode2 .EQ. 8) DFOL%SkyTotalOpaquePct = 100
               DFOL%SkyConditionBad = .FALSE.
            ELSE
               DFOL%SkyConditionBad = .TRUE.
            END IF
         END IF
      END IF

      !
      !  Alternative methods of computing the final value:
      !  1) Use just the Sky-Condition-Observation part (my original method):
      !     Total Opaque, if it exists.  Otherwise, Total Cloud.
      !  2) Max value from the SkyCoverSummation section
      !  3) Max value from the SkyCoverLayer section
      !
      IF (DFOL%SkyTotalOpaquePct .GT. -98) THEN
         DFOL%Method1 = DFOL%SkyTotalOpaquePct
      ELSE IF (DFOL%SkyTotalCoverPct .GT. -98) THEN
         DFOL%Method1 = DFOL%SkyTotalCoverPct
      ELSE
         DFOL%Method1 = -99
      END IF
      
      MaxC = -99
      DO I = 1, 6
         IF (DFOL%SkyCoverSumPct(I) .GT. MaxC) MaxC = DFOL%SkyCoverSumPct(I)
      END DO
      DFOL%Method2 = MaxC
      
      MaxC = -99
      DO I = 1, 6
         IF (DFOL%SkyCoverLayerPct(I) .GT. MaxC) MaxC = DFOL%SkyCoverLayerPct(I)
      END DO
      DFOL%Method3 = MaxC
      
      RETURN

     
   
 1000 FORMAT(I4, A6, A5, I4, 4I2) 
! 1020 FORMAT(I2, F4.1, A1, A1)

 1101 FORMAT(I2, I1)
 1102 FORMAT(I1, I2, I1)
 1103 FORMAT(I2, I2, I1)
 
 1500 FORMAT(A2, I1)
   
      END SUBROUTINE ParseDataLine


!--------------------------------------------------------------------
      SUBROUTINE ClearDFOL(DFOL)
      IMPLICIT NONE
      TYPE (CloudDataFromOneLine), INTENT(INOUT) :: DFOL
      INTEGER :: I
      DFOL%USAF     = '------'
      DFOL%WBAN     = '-----'
      DFOL%ObsDate  = MissingData_Int
      DFOL%ObsTime  = MissingData_Int
      DFOL%DateSeq  = MissingData_Int
      DFOL%Time     = MissingData_Int
      DFOL%Lat      = MissingData_Real
      DFOL%Long     = MissingData_Real
      DFOL%Method1  = MissingData_Int
      DFOL%Method2  = MissingData_Int
      DFOL%Method3  = MissingData_Int
      DO I=1,6
         DFOL%SkyCoverLayerPct(I) = MissingData_Int
         DFOL%SkyCoverLayerBad(I) = .FALSE.
         DFOL%SkyCoverSumPct(I)   = MissingData_Int
         DFOL%SkyCoverSumBad(I)   = .FALSE.
      END DO
      DFOL%SkyTotalCoverPct  = MissingData_Int
      DFOL%SkyTotalOpaquePct = MissingData_Int
      DFOL%SkyConditionBad   = .FALSE.

      DO I=1,6
         DFOL%RawGA(I) = ''
         DFOL%RawGD(I) = ''
      END DO
      DFOL%RawGF = ''
      
      END SUBROUTINE ClearDFOL

      
!--------------------------------------------------------------------
!  The time in the file is UTC. 
!  For simplicity of all subsequent operations, I want to adjust 
!  the date/time fields from UTC to local time. 
!
!  Since I am going to be processing data for the Great Lakes, and
!  most of the G.L. basin is Eastern time, I am gonna make a fairly
!  big simplifying assumption -- 
!  ALL stations are converted from UTC to Eastern Standard Time.
!  I'm not even gonna worry about daylight savings time for now.
!  I will just adjust everything by 5 hours.
!
      SUBROUTINE TranslateUTC_Local(YYYY, MM, DD, HH)
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: YYYY, MM, DD, HH
      INTEGER :: Seq
      
      HH = HH - 5
      IF (HH .LT. 0) THEN
         HH = HH + 24
         DD = DD - 1
         IF (DD .LT. 1) THEN
            CALL DateSequence(DD+1, MM, YYYY, Seq)
            CALL SequenceDate(DD, MM, YYYY, Seq-1)
         END IF
      END IF
      RETURN
      END SUBROUTINE TranslateUTC_Local


!--------------------------------------------------------------------
      SUBROUTINE GetStationInfo(HistFileName, USAF_ID, WBAN_ID,    &
                 StnName, Country, StnLat, StnLong)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: HistFileName, USAF_ID, WBAN_ID
      CHARACTER(LEN=*), INTENT(OUT) :: StnName, Country
      REAL,             INTENT(OUT) :: StnLat, StnLong
                 
      INTEGER :: U1, IOS, I
      CHARACTER(LEN=5)  :: ID2
      CHARACTER(LEN=6)  :: ID1
      CHARACTER(LEN=59) :: RestOfLine

      StnName = '---'
      Country = '--'
      StnLat  = -999.999
      StnLong = -999.999
      
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(HistFileName), STATUS='OLD', IOSTAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error opening the station history file. Process Aborted.'
         CALL EXIT(1)
      END IF
      CALL FileWasOpened(U1)

      READ(U1, 1000, IOSTAT=IOS) ID1, ID2, RestOfLine
      DO WHILE (IOS .EQ. 0)
         IF ((ID1 .EQ. USAF_ID) .AND. (ID2 .EQ. WBAN_ID)) THEN
            READ(RestOfLine, 1050, IOSTAT=I) StnName, Country, StnLat, StnLong
            IF (I .NE. 0) THEN
               StnName = '---'
               Country = '--'
               StnLat  = -999.999
               StnLong = -999.999
            END IF
            CLOSE(U1)
            CALL FileWasClosed(U1)
            RETURN            
         END IF
         READ(U1, 1000, IOSTAT=IOS) ID1, ID2, RestOfLine
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      
     
 1000 FORMAT(A6, 1X, A5, 1X, A59)
 1050 FORMAT(A30, A2, 11X, F8.3, F8.3) 
 
      END SUBROUTINE GetStationInfo

!-----------------------------------------------------------------------------      
      SUBROUTINE ProcessStation(HistFileName, DataFileName, RetStat)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: HistFileName, DataFileName
      INTEGER,          INTENT(OUT) :: RetStat
      
      INTEGER :: I, U1, U2, IOS, LN
      INTEGER :: ErrStat
      LOGICAL :: OK
      REAL    :: Latitude, Longitude, StnLat, StnLong
      CHARACTER(LEN=2)     :: Country
      CHARACTER(LEN=5)     :: WBAN_ID
      CHARACTER(LEN=6)     :: USAF_ID
      CHARACTER(LEN=12)    :: FullID
      CHARACTER(LEN=30)    :: StnName
      CHARACTER(LEN=100)   :: OutputFileName
      CHARACTER(LEN=10104) :: DataLine     ! max length is 105 mandatory plus up to 9999 variable 

      TYPE (CloudDataFromOneLine)   :: DFOL

      !
      !  Open the input file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(DataFileName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)

      !
      !  Read first line just to get the station info
      !
      READ(U1, 1101, IOSTAT=IOS) USAF_ID, WBAN_ID, Latitude, Longitude
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error reading station ID and location from line 1.'
         CALL EXIT(1)
      END IF
      CALL GetStationInfo(HistFileName, USAF_ID, WBAN_ID,    &
                 StnName, Country, StnLat, StnLong)

      !
      !  Open the output file and write header lines
      !
      FullID = TRIM(USAF_ID)//'-'//TRIM(WBAN_ID)
      OutputFileName = 'Cloud_'//TRIM(FullID)//'.csv'
      U2 = GetFreeUnitNumber()
      OPEN(UNIT=U2, FILE=TRIM(OutputFileName), STATUS='REPLACE', ERR=821)
      CALL FileWasOpened(U2)
      WRITE(U2, 2000) TRIM(FullID), StnName, StnLat, StnLong
      WRITE(U2, 2001)
      IF (.NOT. DetailedOutput) THEN
         WRITE(U2, 2005)
      ELSE
         WRITE(U2, 2105)
      END IF
      
      !
      !  Read and write raw data
      !
      REWIND(U1)
      LN = 1       ! Line Number
      READ(U1, 1105, IOSTAT=IOS) DataLine
      DO WHILE (IOS .EQ. 0)
         IF (LEN_TRIM(DataLine) .GT. 100) THEN
            CALL ParseDataLine(DataLine, DFOL, ErrStat)
            IF (ErrStat   .NE.       0) OK = .FALSE.
            IF (DFOL%USAF .NE. USAF_ID) OK = .FALSE.
            IF (DFOL%WBAN .NE. WBAN_ID) OK = .FALSE.
            IF (OK) THEN
               IF (.NOT. DetailedOutput) THEN
                  WRITE(U2, 2006) DFOL%ObsDate, DFOL%ObsTime,                                &
                       DFOL%Method1, DFOL%Method2, DFOL%Method3
               ELSE
                  WRITE(U2, 2106) DFOL%ObsDate, DFOL%ObsTime,                                &
                       DFOL%Method1, DFOL%Method2, DFOL%Method3,                             &
                       DFOL%SkyTotalCoverPct, DFOL%SkyTotalOpaquePct, DFOL%SkyConditionBad,  &
                       (DFOL%SkyCoverLayerPct(I), DFOL%SkyCoverLayerBad(I), I=1,6),          &
                       (DFOL%SkyCoverSumPct(I),   DFOL%SkyCoverSumBad(I),   I=1,6),          &
                       DFOL%RawGF, (DFOL%RawGA(I), I=1,6), (DFOL%RawGD(I), I=1,6)
               END IF
            END IF
            LN = LN + 1
         END IF
         READ(U1, 1105, IOSTAT=IOS) DataLine
      END DO
      
      CLOSE(U1)
      CALL FileWasClosed(U1)
      CLOSE(U2)
      CALL FileWasClosed(U2)
      

      ErrorLevel = 0   
      GOTO 999     ! clean up and exit
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file '//TRIM(DataFileName)
      CALL PassMsg; GOTO 898
  821 ErrorMessage = 'Error opening output file '//TRIM(OutputFileName)
      CALL PassMsg; GOTO 898
      
  898 ErrorLevel = 1
      ErrorMessage = 'Program aborted!';  CALL PassMsg


  999 RetStat = ErrorLevel
      
      
 1101 FORMAT(4X, A6, A5, 13X, F6.3, F7.3)
 1105 FORMAT(A10104)
     
     
 2000 FORMAT('"ID=",', A, ',"Name=","', A, '"',      /                &
             '"Latitude=",', F10.3, ',"Longitude=",', F10.3)
 2001 FORMAT('"Method 1 = Sky-Condition-Observation. Total Opaque if it exists, else Total Cloud."',   /   &
             '"Method 2 = Max value from Sky-Cover-Summation (identifiers GD1-GD6)."',                 /   &
             '"Method 3 = Max value from Sky-Cover-Layer (identifiers GA1-GA6)."')
             
 2005 FORMAT('"YearMnDy","HrMi","Method1","Method2","Method3"')
 2006 FORMAT(I8.8, ',', I4.4, ',', 3(I0.3, ','))

 2105 FORMAT('"YearMnDy","HrMi","Method1","Method2","Method3",',                &
             '"SumTotal","SumOpaque","SumBad",',                                &
             '"Layer1","Bad","Layer2","Bad","Layer3","Bad",',                   &
             '"Layer4","Bad","Layer5","Bad","Layer6","Bad",',                   &
             '"CovSum1","Bad","CovSum2","Bad","CovSum3","Bad",',                &
             '"CovSum4","Bad","CovSum5","Bad","CovSum6","Bad",',                &
             '"RawGF","RawGA1","RawGA2","RawGA3","RawGA4","RawGA5","RawGA6",',  &
             '"RawGD1","RawGD2","RawGD3","RawGD4","RawGD5","RawGD6"')
 2106 FORMAT(I8.8, ',', I4.4, ',', 3(I0.3, ','),                  &
           2(I0.3, ','), L1, ',',                                 &
           6(I0.3, ',', L1, ','), 6(I0.3, ',', L1, ','),          &
            A5, ',', 6(A13, ','), 5(A12, ','), A12)

      END SUBROUTINE ProcessStation      
      
      
   END MODULE Get_ISH_Cloud_Subs
   
   
!==============================================================================   
!==============================================================================   
!==============================================================================   
   PROGRAM Get_ISH_Cloud
   USE Get_ISH_Cloud_Subs
   IMPLICIT NONE

   INTEGER :: I, J, ArgC, ErrStat
   INTEGER :: HistFilePosition, FileNameStart
   CHARACTER(LEN=100)  :: HistFileName, DataFileName, Args(9999)
   CHARACTER(LEN=99999) :: CLine
   
   !
   !  Prep for command line processing
   !
   DetailedOutput = .FALSE.
   HistFilePosition = 2
   FileNameStart = 3
   
   !
   !  Process the command line
   !
   CALL GET_COMMAND(CLine)
   CALL ParseCmdLine(CLine, Args, ArgC)
   IF (ErrorLevel .NE. 0) ArgC = 0
   IF (ArgC .LT. 3) THEN
      PRINT*, 'USAGE: Get_ISH_Cloud [-d] histfile filename(s)'
      PRINT*, '     -d = Default output (without -d) is the synthesized cloud percentages.'
      PRINT*, '          If -d is specified, then the output file will contain full details.'
      PRINT*, '     histfile = station metadata file like ish-history.txt from ftp site'
      PRINT*, '     filename(s) = data file(s) containing ish-format dataset (wildcards ok)'
      CALL EXIT(1)
   END IF
   
   IF (Args(2) .EQ. '-d') THEN
      DetailedOutput = .TRUE.
      HistFilePosition = 3
      FileNameStart = 4
   END IF

   HistFileName = Args(HistFilePosition)

   !
   !  main loop
   !   
   DO I = FileNameStart, ArgC
      DataFileName = Args(I)
      J = INDEX(DataFileName, '.', .TRUE.)       ! find last dot
      print*, 'Processing '//TRIM(DataFileName)
      CALL ProcessStation(HistFileName, DataFileName, ErrStat)
      IF (ErrStat .NE. 0) THEN
         PRINT*, 'Program aborted.'
         CALL EXIT(1)
      END IF
   END DO
   
   END PROGRAM Get_ISH_Cloud