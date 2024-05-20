!-------------------------------------------------------------------------
!  Fortran program for processing data in the ISH archive format.
!  Output format is GLERL's met_*.csv format (as designed in 2014 by
!  Tim Hunter.)
!
!  Written by Tim Hunter in late 2015.
!   
!-------------------------------------------------------------------------
   MODULE Process_ISH_Subs
      USE Glshfs_Util
      USE MetDataTypesAndUnits
      USE DailyMetStationDataset
      USE ReadWriteMetCsv

      TYPE ObsTimeValue
         INTEGER :: ObsDate             !  YYYYMMDD as a single integer value
         INTEGER :: ObsTime             !  HHMM as a single integer value
         REAL    :: DVal
      END TYPE ObsTimeValue

      TYPE PrecipObs
         INTEGER :: ObsDate             !  YYYYMMDD as a single integer value
         INTEGER :: ObsTime             !  HHMM as a single integer value
         INTEGER :: Hours               !  Hours of accumulation represented by the value
         REAL    :: DVal                !  millimeters
      END TYPE PrecipObs

      TYPE DataFromOneLine
         CHARACTER (LEN=6) :: USAF
         CHARACTER (LEN=5) :: WBAN
         INTEGER :: ObsDate             !  YYYYMMDD as a single integer value
         INTEGER :: ObsTime             !  HHMM as a single integer value
         INTEGER :: DateSeq             !  Date as a sequence number
         INTEGER :: Time                !  HHMM as a single integer
         REAL    :: Lat                 !  latitude in decimal degrees north of equator
         REAL    :: Long                !  longitude in decimal degrees east of prime meridian
         REAL    :: AirTemp             !  degrees C
         REAL    :: DewPoint            !  degrees C
         REAL    :: WindSpd             !  meters/sec
         REAL    :: Cloud               !  Percent of sky covered by cloud (distilled from detailed info)
         TYPE (PrecipObs), DIMENSION(4) :: Precip
         INTEGER :: SkyCoverLayerPct(6) 
         LOGICAL :: SkyCoverLayerBad(6) 
         INTEGER :: SkyCoverSumPct(6)   
         LOGICAL :: SkyCoverSumBad(6)   
         INTEGER :: SkyTotalCoverPct    
         INTEGER :: SkyTotalOpaquePct   
         LOGICAL :: SkyConditionBad    
      END TYPE DataFromOneLine

      TYPE StnMeta
         CHARACTER(LEN=12) :: StnID     ! 12-character ID comprised of USAF-WBAN numbers
         CHARACTER(LEN=30) :: StnName
         CHARACTER(LEN=2)  :: Country
         REAL              :: Lat, Long
         CHARACTER(LEN=7)  :: UseString
         INTEGER, DIMENSION(:), ALLOCATABLE :: ExcludeYears
      END TYPE StnMeta
      
      TYPE(StnMeta), DIMENSION(:), ALLOCATABLE :: StationMeta
      
   CONTAINS
   
   
!--------------------------------------------------------------------
      SUBROUTINE ParseDataLine(Line, DFOL, ErrStat)
      IMPLICIT NONE
      CHARACTER(LEN=*),       INTENT(IN)    :: Line
      TYPE (DataFromOneLine), INTENT(INOUT) :: DFOL
      INTEGER,                INTENT(OUT)   :: ErrStat
      
      INTEGER :: I, J, IOS, VarChars, YYYY, MM, DD, HH, NN
      INTEGER :: Hrs, SkyCode1, SkyCode2, SkyQC
      INTEGER :: GaMax, GdMax
      LOGICAL :: OK
      REAL    :: Prc
      CHARACTER(LEN=1)    :: CC, QC, AirQC, DewQC, WindQC
      CHARACTER(LEN=3)    :: I3
      CHARACTER(LEN=5)    :: S5
      CHARACTER(LEN=8)    :: S8
      CHARACTER(LEN=12)   :: S12
      CHARACTER(LEN=13)   :: S13
      CHARACTER(LEN=9999) :: VLine
      
      ErrStat = 0
      !
      !  Initialize the DFOL fields to missing data
      !
      CALL ClearDFOL(DFOL)
      
      !
      !  Read the Mandatory data section
      !
      READ(Line, 1000, IOSTAT=IOS) VarChars, DFOL%USAF, DFOL%WBAN,        &
                 YYYY, MM, DD, HH, NN,                                    &
                 DFOL%Lat, DFOL%Long, DFOL%WindSpd, WindQC,               &
                 DFOL%AirTemp, AirQC, DFOL%DewPoint, DewQC
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
      !  If the data value is the "Missing" value, replace it with our
      !  defined MissingData value
      !
      IF (DFOL%WindSpd  .GT. 999.7) DFOL%WindSpd = MissingData_Real
      IF (DFOL%AirTemp  .GT. 999.7) DFOL%AirTemp = MissingData_Real
      IF (DFOL%DewPoint .GT. 999.7) DFOL%DewPoint = MissingData_Real
      
      !
      !  Inspect the quality code for air temp and use it to filter 
      !  the airtemp values.
      !
      OK = .TRUE.
      IF (AirQC .EQ. '2') OK = .FALSE.
      IF (AirQC .EQ. '3') OK = .FALSE.
      IF (AirQC .EQ. '6') OK = .FALSE.
      IF (AirQC .EQ. '7') OK = .FALSE.
      IF (.NOT. OK) DFOL%AirTemp = MissingData_Real
      
               
      !
      !  Inspect the quality code for dewpt temp and use it to filter 
      !  the dewpt values.
      !
      OK = .TRUE.
      IF (DewQC .EQ. '2') OK = .FALSE.
      IF (DewQC .EQ. '3') OK = .FALSE.
      IF (DewQC .EQ. '6') OK = .FALSE.
      IF (DewQC .EQ. '7') OK = .FALSE.
      IF (.NOT. OK) DFOL%Dewpoint = MissingData_Real
               
      !
      !  Inspect the quality code for windspeed and use it to filter 
      !  the windspeed value.
      !
      OK = .TRUE.
      IF (WindQC .EQ. '2') OK = .FALSE.
      IF (WindQC .EQ. '3') OK = .FALSE.
      IF (WindQC .EQ. '6') OK = .FALSE.
      IF (WindQC .EQ. '7') OK = .FALSE.
      IF (.NOT. OK) DFOL%WindSpd = MissingData_Real
               
      !
      !  Extract the additional data into a separate string variable
      !      
      J = 105 + VarChars
      VLine = TRIM(Line(106:J))
                     
      !
      !  Search the additional data section for precip entries
      !
      DO I = 1, 4
         WRITE(I3, 1500) 'AA', I      ! 3-char identifier for the field
         J = INDEX(VLine, I3)
         IF (J .GT. 0) THEN
            S8 = VLine(J+3:J+10)
            READ(S8, 1020, IOSTAT=IOS) Hrs, Prc, CC, QC 
            IF (Prc .GT. 999.7) Prc = MissingData_Real      ! missing data value
            IF (IOS .EQ. 0) THEN
               DFOL%Precip(I)%Hours = Hrs
               DFOL%Precip(I)%DVal  = Prc
               OK = .TRUE.
               
               !
               !  The "Condition Code" (or trace indicator) is a bit confusing.
               !  In particular, the Begin/End period entries seem to make no
               !  sense, because they don't "book end". I asked a guy at NCDC 
               !  about these and he was unable to provide any insight.
               !  Therefore, I am going to ignore the Begin/End entries.
               !  If the value is __ I do __ with the precip data.
               !   '1' (Measurement impossible or inaccurate)   : throw it away
               !   '2' (Trace)                                  : keep it
               !   '3' - '8' (those Begin/End options           : ignore this field
               !   'E' (Estimated, e.g. from nearby station)    : keep it (reluctantly)
               !   'I' (Incomplete, excludes missing reports)   : throw it away
               !   'J' (Incomplete, excludes erroneous reports) : throw it away
               !   '9' (Missing)                                : ignore this field
               !
               IF (CC .EQ. '1') OK = .FALSE.
               IF (CC .EQ. 'I') OK = .FALSE.
               IF (CC .EQ. 'i') OK = .FALSE.
               IF (CC .EQ. 'J') OK = .FALSE.
               IF (CC .EQ. 'j') OK = .FALSE.
               
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
               !   'A'   :  keep
               !   'I'   :  keep
               !   'M'   :  keep
               !   'P'   :  keep
               !   'R'   :  keep
               !   'U'   :  keep
               !
               IF (QC .EQ. '2') OK = .FALSE.
               IF (QC .EQ. '3') OK = .FALSE.
               IF (QC .EQ. '6') OK = .FALSE.
               IF (QC .EQ. '7') OK = .FALSE.
               
               IF (.NOT. OK) THEN
                  DFOL%Precip(I)%Hours = -99
                  DFOL%Precip(I)%DVal  = MissingData_Real
               END IF
            END IF
         END IF
      END DO
                     
      !
      !---------- cloud data section ---------      
      !  #########################################################################
      !  New information as of Nov 9, 2015:
      !  As noted in comments below, the Sky-Condition-Observation field (code GF1)
      !  is what I have used for years, but has recently become very problematic.
      !  Specifically, we noted that starting on Aug 1, 2013 many/most (not all)
      !  stations started reporting only missing or zero for that field. This had
      !  the net effect of a huge downward bias in the averages. Initial attempts
      !  to get an explanation from NCDC were unfruitful, so I did a lot of 
      !  independent analysis in an attempt to figure out what to do. We also had to 
      !  do special processing (eliminate most stations) in order to get reasonable 
      !  results for ongoing studies. Last week I completed enough analysis that I 
      !  had convinced myself to change from the GF1 method to this:
      !  If GA1-GA6 have data, use that.  If not....
      !     If GD1-GD6 have data, use that.  If not....
      !        If GF1 has data, use that.
      !  I also then updated my "bug report" to NCDC with additional info from my
      !  investigations.
      !  Today, I received the following response from Mark Lackey at NCDC:
      !  
      !     Hello Tim,
      !     Sorry it's taken me so long to get back with you. The response I have 
      !     from the Air Force (the source of the cloud cover readings) is this:
      !       "When we switched to the Joint METOC decoder [August 1, 2013] AFWA 
      !        stopped running their ceiling builder routine and they no longer 
      !        sum cloud layers.  There is better/more reliable data in the 
      !        [GA1-GA6 elements]..."
      !     We are now advising users to not use the GF1 element after mid-2013 
      !     but to use the series of GA elements instead.
      !
      !  YES!!!!! That's definitive guidance to do what I had decided to do.
      !  #########################################################################
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
      !  Now that I have all of the cloud data interpreted, assign our chosen
      !  value as the cloud cover. Yes, there is a good deal of additional work being
      !  done above to parse all of that cloud data, and then I don't actually use it 
      !  right now. But this allows me to easily revert/change in the future.
      !
      !  For many years, I have used the GF1 Sky-Condition-Observation field. But that is
      !  the newly problematic one. So now I will use the max value of one set of cloud
      !  layer data as the overall coverage.  This is the way Greg Lang does it from the
      !  NOAAPort METAR data feed.  I just have to decide which set of cloud layer data to
      !  use (GAx or GDx?).  After a bit of anecdotal perusal, comparing the GF1 values
      !  with GA and GD values during the historical period, I have decided this:
      !    If the GAx set has a value, use that one. If not, try to use the GDx value.
      !    If both of those are missing, leave it as missing. I cannot currently use the
      !    GF1 field for anything due to the known major issues.
      !
      GaMax = -99
      GdMax = -99
      DO I = 1, 6
         IF (DFOL%SkyCoverLayerPct(I) .GE. 0) THEN
            IF (DFOL%SkyCoverLayerPct(I) .GT. GaMax) GaMax = DFOL%SkyCoverLayerPct(I)
         END IF
         IF (DFOL%SkyCoverSumPct(I) .GE. 0) THEN
            IF (DFOL%SkyCoverSumPct(I) .GT. GdMax) GdMax = DFOL%SkyCoverSumPct(I)
         END IF
      END DO

      IF (GaMax .GT. -98) THEN
         DFOL%Cloud = GaMax
      ELSE IF (GdMax .GT. -98) THEN
         DFOL%Cloud = GdMax
      ELSE
         DFOL%Cloud = MissingData_Real
      END IF
     
      RETURN

     
   
 1000 FORMAT(I4, A6, A5, I4, 4I2, 1X, F6.3, F7.3, T66, F4.1, A1, T88, 2(F5.1,A1)) 
 1020 FORMAT(I2, F4.1, A1, A1)

 1101 FORMAT(I2, I1)
 1102 FORMAT(I1, I2, I1)
 1103 FORMAT(I2, I2, I1)
 
 1500 FORMAT(A2, I1)
   
      END SUBROUTINE ParseDataLine


!--------------------------------------------------------------------
      SUBROUTINE ClearDFOL(DFOL)
      IMPLICIT NONE
      TYPE (DataFromOneLine), INTENT(INOUT) :: DFOL
      INTEGER :: I
      DFOL%USAF     = '------'
      DFOL%WBAN     = '-----'
      DFOL%ObsDate  = MissingData_Int
      DFOL%ObsTime  = MissingData_Int
      DFOL%DateSeq  = MissingData_Int
      DFOL%Time     = MissingData_Int
      DFOL%Lat      = MissingData_Real
      DFOL%Long     = MissingData_Real
      DFOL%AirTemp  = MissingData_Real
      DFOL%Dewpoint = MissingData_Real
      DFOL%WindSpd  = MissingData_Real
      DFOL%Cloud    = MissingData_Real
      DO I=1,4
         DFOL%Precip(I)%ObsDate = MissingData_Int
         DFOL%Precip(I)%ObsTime = MissingData_Int
         DFOL%Precip(I)%Hours   = -99
         DFOL%Precip(I)%DVal    = MissingData_Real
      END DO
      
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
!  The time in the file is UTC. For this routine I want to compute 
!  a new UTC time by adding the specified number of offset hours.
!  This routine will accept offsets in either direction.
!
      SUBROUTINE TranslateUTCByOffset(YYYY, MM, DD, HH, Off)
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: YYYY, MM, DD, HH
      INTEGER, INTENT(IN)    :: Off
      INTEGER :: Seq
      
      HH = HH + Off
      DO WHILE (HH .GT. 23) 
         CALL DateSequence(DD, MM, YYYY, Seq)
         HH = HH - 24
         Seq = Seq + 1
         CALL SequenceDate(DD, MM, YYYY, Seq)
      END DO
      DO WHILE (HH .LT. 0)
         CALL DateSequence(DD, MM, YYYY, Seq)
         HH = HH + 24
         Seq = Seq - 1
         CALL SequenceDate(DD, MM, YYYY, Seq)
      END DO
      RETURN
      END SUBROUTINE TranslateUTCByOffset

!--------------------------------------------------------------------
      SUBROUTINE Decode_ExcludeYears(YearString, YearArray)
      IMPLICIT NONE
      CHARACTER(LEN=*) :: YearString
      INTEGER, DIMENSION(:), ALLOCATABLE :: YearArray
      
      INTEGER :: I, L, TYr
      CHARACTER(LEN=50) :: YrS
      CHARACTER(LEN=10), DIMENSION(10) :: Segments
      INTEGER, DIMENSION(200) :: TempYrs
      
      TempYrs(:) = -9999
      TYr = 0
      YrStr = TRIM(ADJUSTL(YearString))
      DO WHILE (LEN_TRIM(YrStr) .GT. 0)
         !
         !  Strip off the first year designator (single year or range)
         !  and leave YrStr ready for the next iteration
         !
         J = INDEX(YrStr, ';')
         IF (J .GT. 0) THEN
            ThisStr = YrStr(1:J-1)
            YrStr = YrStr(J+1:)
         ELSE
            ThisStr = TRIM(YrStr)
            YrStr = ''
         END IF
         
         !
         !  Process the year designator that was just stripped off,
         !  adding the designated years to TempYrs
         !
         J = INDEX(ThisStr, '-')
         IF (J .GT. 0) THEN
            S1 = TRIM(ADJUSTL(ThisStr(1:J-1)))
            S2 = TRIM(ADJUSTL(ThisStr(J+1:)))
         ELSE
            S1 = TRIM(ADJUSTL(ThisStr))
            S2 = S1
         END IF
         READ(S1, *, ERR=700) SYear
         READ(S2, *, ERR=700) EYear
         DO Y = SYear, EYear
            TYr = TYr + 1
            TempYrs(TYr) = Y
         END DO
      END DO
      
      !
      !  Now use the TempYrs array to fill the permanent array
      !
      IF (TYr .GT. 0) THEN
         ALLOCATE(YearArray(TYr), IOSTAT=IOS)
         IF (IOS .NE. 0) THEN
            PRINT*, 'Error allocating memory for exclusion years'
            CALL EXIT(1)
         END IF
         DO I = 1, TYr
            YearArray(I) = TempYrs(I)
         END DO
      END IF
      RETURN
         
  700 PRINT*, 'Error parsing exclusion years from string in master file.'
      PRINT*, 'String = ' // TRIM(ADJUSTL(YearString))
      CALL EXIT(1)
      
      END SUBROUTINE Decode_ExcludeYears

!--------------------------------------------------------------------
      SUBROUTINE ComputeDailyMax(RawData, DlyData, SeqStart, SeqFinish)
      IMPLICIT NONE
      TYPE (ObsTimeValue), DIMENSION(:), INTENT(IN)  :: RawData
      REAL,                DIMENSION(:), INTENT(OUT) :: DlyData
      INTEGER, INTENT(IN)  :: SeqStart, SeqFinish
      
      INTEGER :: I, Y, M, D, Seq, DDate
      INTEGER :: DayNum, NumEntries
      REAL    :: DVal
      CHARACTER(LEN=8) :: ST
      
      DlyData(:) = MissingData_Real
      
      NumEntries = UBOUND(RawData, 1)
      DO I = 1, NumEntries
         DDate = RawData(I)%ObsDate
         DVal  = RawData(I)%DVal
         IF ((DDate .GT. 0) .AND. (DVal .GT. MissingData_Real_Test)) THEN
            WRITE(ST, 1000) RawData(I)%ObsDate
            READ(ST, 1001) Y, M, D
            CALL DateSequence(D, M, Y, Seq)
            IF ((Seq .GE. SeqStart) .AND. (Seq .LE. SeqFinish)) THEN
               DayNum = Seq - SeqStart + 1
               IF (DlyData(DayNum) .LE. MissingData_Real_Test) DlyData(DayNum) = DVal
               IF (DVal .GT. DlyData(DayNum)) DlyData(DayNum) = DVal
            END IF
         END IF
      END DO
      
  1000 FORMAT(I8.8)
  1001 FORMAT(I4, 2I2)
   
      END SUBROUTINE ComputeDailyMax
  
!--------------------------------------------------------------------
      SUBROUTINE ComputeDailyMin(RawData, DlyData, SeqStart, SeqFinish)
      IMPLICIT NONE
      TYPE (ObsTimeValue), DIMENSION(:), INTENT(IN)  :: RawData
      REAL,                DIMENSION(:), INTENT(OUT) :: DlyData
      INTEGER, INTENT(IN)  :: SeqStart, SeqFinish
      
      INTEGER :: I, Y, M, D, Seq, DDate
      INTEGER :: DayNum, NumEntries
      REAL    :: DVal
      CHARACTER(LEN=8) :: ST
      
      DlyData(:) = MissingData_Real
      
      NumEntries = UBOUND(RawData, 1)
      DO I = 1, NumEntries
         DDate = RawData(I)%ObsDate
         DVal  = RawData(I)%DVal
         IF ((DDate .GT. 0) .AND. (DVal .GT. MissingData_Real_Test)) THEN
            WRITE(ST, 1000) RawData(I)%ObsDate
            READ(ST, 1001) Y, M, D
            CALL DateSequence(D, M, Y, Seq)
            IF ((Seq .GE. SeqStart) .AND. (Seq .LE. SeqFinish)) THEN
               DayNum = Seq - SeqStart + 1
               IF (DlyData(DayNum) .LE. MissingData_Real_Test) DlyData(DayNum) = DVal
               IF (DVal .LT. DlyData(DayNum)) DlyData(DayNum) = DVal
            END IF
         END IF
      END DO
      
  1000 FORMAT(I8.8)
  1001 FORMAT(I4, 2I2)
   
      END SUBROUTINE ComputeDailyMin
  
!--------------------------------------------------------------------
!  Doing daily max and min are pretty easy. Daily average is more involved because
!  There could potentially be a lot of clustered observations that can throw off 
!  the average.  For example, if we are computing a daily average air temp, and
!  the raw data has 10 mid-day observations, but only 1 night-time obs, that will
!  likely bias the average daily temp by a significant amount. In order to mitigate
!  that problem, I will first dump the raw data into hourly bins. Then compute a
!  simple arithmetic mean for each hour. Then compute simple arithmetic mean from
!  the (1-24) hourly values.
!
      SUBROUTINE ComputeDailyAvg(RawData, DlyData, SeqStart, SeqFinish)
      IMPLICIT NONE
      TYPE (ObsTimeValue), DIMENSION(:), INTENT(IN)  :: RawData
      REAL,                DIMENSION(:), INTENT(OUT) :: DlyData
      INTEGER, INTENT(IN)  :: SeqStart, SeqFinish
      
      INTEGER :: I, Y, M, D, H, Seq, DDate, IOS
      INTEGER :: DayNum, NumEntries, NumDays
      INTEGER :: DlyCount
      REAL    :: DVal, DlyAccum, HlyMean
      CHARACTER(LEN=8) :: ST
      
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: HlyCount     !  dim(1:NumDays, 0:23)
      REAL,    DIMENSION(:,:), ALLOCATABLE :: HlyAccum     !  dim(1:NumDays, 0:23)
      
      
      DlyData(:) = MissingData_Real
      
      !
      !  Allocate the RAM for accumulating raw data into hourly bins
      !
      NumDays = SeqFinish - SeqStart + 1
      ALLOCATE(HlyAccum(NumDays, 0:23), HlyCount(NumDays, 0:23), STAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error allocating RAM for hourly bins of raw data.'
         CALL EXIT(1)
      END IF
      HlyAccum(:,:) = 0.0
      HlyCount(:,:) = 0

      !      
      !  Accumulate the raw data into those hourly bins
      !
      NumEntries = UBOUND(RawData, 1)
      DO I = 1, NumEntries
         DDate = RawData(I)%ObsDate
         DVal  = RawData(I)%DVal
         IF ((DDate .GT. 0) .AND. (DVal .GT. MissingData_Real_Test)) THEN
            WRITE(ST, 1000) RawData(I)%ObsDate
            READ(ST, 1001) Y, M, D
            WRITE(ST, 1004) RawData(I)%ObsTime
            READ(ST, 1005) H
            CALL DateSequence(D, M, Y, Seq)
            IF ((Seq .GE. SeqStart) .AND. (Seq .LE. SeqFinish)) THEN
               DayNum = Seq - SeqStart + 1
               HlyAccum(DayNum, H) = HlyAccum(DayNum, H) + DVal
               HlyCount(DayNum, H) = HlyCount(DayNum, H) + 1
            END IF
         END IF
      END DO
      
      !
      !  For each day...
      !    1. Compute the average value for each of the 24 hourly bins
      !    2. Compute daily average from those 24 values
      !
      DO DayNum = 1, NumDays
         DlyAccum = 0.0
         DlyCount = 0
         DO H = 0, 23
            IF (HlyCount(DayNum,H) .GT. 0) THEN
               HlyMean = HlyAccum(DayNum,H) / HlyCount(DayNum,H)
               DlyAccum = DlyAccum + HlyMean
               DlyCount = DlyCount + 1
            END IF
         END DO
         IF (DlyCount .GT. 0) THEN
            DlyData(DayNum) = DlyAccum / DlyCount
         END IF
      END DO
      
      DEALLOCATE(HlyAccum, HlyCount, STAT=IOS)      
      RETURN      
      
      
  1000 FORMAT(I8.8)
  1001 FORMAT(I4, 2I2)
  1004 FORMAT(I4.4, 4X)
  1005 FORMAT(I2)
   
      END SUBROUTINE ComputeDailyAvg
  

!--------------------------------------------------------------------
!  Computing daily precip is a beast due to the fact that the observations 
!  themselves are accumulations over varying periods of time.  I was curious
!  how NCDC computes daily precip from these various hourly data, so I asked.
!  The person I talked to had no answer for me. When I described the following
!  proposed algorithm he said that it sounded reasonable to him.  On the basis
!  of that ringing endorsement (haha), I am proceeding as follows:
!
!  1. Build a 1-d array representing every hour of the entire data period.
!  2. Step through the raw data, and every time I encounter a 1-hour precip
!     accumulation, assign the data value to that hour in the big 1-d array.
!  3. Step through, looking for any 2-hour accumulations. When found...
!        a. Look at the current and previous hours and see if one of them has 
!           already been assigned a value. If so, subtract that amount of precip
!           from the 2-hour accumulation. The remaining amount (if positive) is 
!           then assumed to have fallen during the other hour, so I assign it 
!           there.  If the remainder is < 0 then I assume some sort of roundoff
!           issue and just assign as if it were zero. Perhaps an "iffy" thing
!           to do, but I don't have any better idea.
!        b. If both of those hours have been assigned, just ignore the 2-hour
!           accumulation. Also an "iffy" option, but I don't have a better idea.
!        c. If neither of the hours has a value, equally apportion the accumulated
!           precip to those 2 hourly bins.
!   4. Do the same thing as number 3, but now for the 3-hour accumulations.
!   5. Do it for 4-hour accumulations.
!   6+ Do it over and over for each successive N-hour accumulation up to 48 hours.
!
!--------------------------------------------------------------------
      SUBROUTINE ComputeDailyPrecip(RawData, DlyData, SeqStart, SeqFinish)
      IMPLICIT NONE
      TYPE (PrecipObs), DIMENSION(:,:), INTENT(IN)  :: RawData
      REAL,             DIMENSION(:),   INTENT(OUT) :: DlyData
      INTEGER, INTENT(IN)  :: SeqStart, SeqFinish
      
      INTEGER :: I, J, IOS, AH, H1, H2, Y, M, D, H
      INTEGER :: NumEntries, NumHours, DayNum, Hours, Seq
      INTEGER :: OpenP, FilledP
      LOGICAL :: Good
      REAL    :: DVal, Tot, PrevP, RemP, EqP
      CHARACTER(LEN=4) :: S4
      CHARACTER(LEN=8) :: S8
      REAL, DIMENSION(:), ALLOCATABLE :: Precip
      
      !
      !  Set up the big array (1 hour bins)
      !  Array indices are 0 to NumHours-1
      !
      NumHours = (SeqFinish - SeqStart + 1) * 24
      ALLOCATE(Precip(0:NumHours-1), STAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error allocating RAM for sequential precipitation accumulation.'
         CALL EXIT(1)
      END IF
      Precip(:) = MissingData_Real
      
      !
      !  Apportion raw precip into the big precip array (1 hour bins)
      !
      NumEntries = UBOUND(RawData, 1)
      
      DO AH = 1, 48
         DO I = 1, NumEntries
            DO J = 1, 4
               Hours = RawData(I,J)%Hours
               IF (Hours .EQ. AH) THEN
                  DVal = RawData(I,J)%DVal
                  IF (DVal .GT. MissingData_Real_Test) THEN
                     WRITE(S8, 1000) RawData(I,J)%ObsDate
                     READ(S8, 1001) Y, M, D
                     WRITE(S4, 1004) RawData(I,J)%ObsTime
                     READ(S4, 1005) H
                     CALL DateSequence(D, M, Y, Seq)
                     H2 = (Seq-SeqStart)*24 + H        ! final hour for this precip accum
                     H1 = MAX(0, H2-AH+1)              ! first hour for this precip accum
                     PrevP = 0.0                       ! previously assigned precip in that period
                     FilledP = 0                       ! # hours already filled/assigned
                     DO H = H1, H2
                        IF (Precip(H) .GT. MissingData_Real_Test) THEN
                           PrevP = PrevP + Precip(H)
                           FilledP = FilledP + 1
                        END IF
                     END DO
                     OpenP = (H2-H1+1) - FilledP       ! # hours not filled (open for assignment)
                     IF (OpenP .GT. 0) THEN
                        RemP = DVal - PrevP               ! remaining precip to be apportioned
                        IF (RemP .LT. 0) RemP = 0.0
                        EqP = RemP / OpenP                ! equal precip amount to be assigned
                        DO H = H1, H2
                           IF (Precip(H) .LT. MissingData_Real_Test) THEN
                              Precip(H) = EqP
                           END IF
                        END DO
                     END IF
                  END IF
               END IF
            END DO
         END DO
      END DO
      
      !
      !  Now compute daily totals from the hourly bins
      !      
      DlyData(:) = MissingData_Real
      DO Seq = SeqStart, SeqFinish
         Tot = 0.0
         Good = .FALSE.
         DayNum = Seq - SeqStart + 1
         H1 = (DayNum-1)*24        ! first hour for this precip accum
         H2 = H1 + 23              ! final hour for this precip accum
         DO H = H1, H2
            IF (Precip(H) .GT. MissingData_Real_Test) THEN
               Tot = Tot + Precip(H)
               Good = .TRUE.
            END IF
         END DO
         IF (Good) DlyData(DayNum) = Tot
      END DO
      DEALLOCATE(Precip, STAT=IOS)
      
      
 1000 FORMAT(I8.8)
 1001 FORMAT(I4, 3I2)
 1004 FORMAT(I4.4, 4X)
 1005 FORMAT(I2)
  
      END SUBROUTINE ComputeDailyPrecip
  

!--------------------------------------------------------------------
      SUBROUTINE ReadStationMeta_MasterList(HistFileName)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: HistFileName
                 
      INTEGER :: U1, IOS, I, J1, J2, SNum, StnCount, NumStr
      REAL    :: SLat, SLong
      CHARACTER(LEN=12) :: StnID
      CHARACTER(LEN=25) :: StnName
      CHARACTER(LEN=50) :: Strings(9)
      CHARACTER(LEN=80) :: Line

      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(HistFileName), STATUS='OLD', IOSTAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error opening the master file. Process Aborted.'
         CALL EXIT(1)
      END IF
      CALL FileWasOpened(U1)

      !
      !  Skip over the header lines. I assume that the header ends when the 
      !  first part of a line is dashes.
      !
      Done = .FALSE.
      DO WHILE (.NOT. Done)
         READ(U1, 1000, IOSTAT=IOS) Line
         IF (IOS .NE. 0) THEN
            PRINT*, 'Error while reading past header in the master file.'
            PRINT*, 'Processing aborted'
            CALL EXIT(1)
         END IF
         IF (Line(1:13) .EQ. '------------,') Done = .TRUE.
      END DO

      !
      !  Now determine the number of valid lines. This way I can allocate
      !  the big array.
      !
      StnCount = 0
      READ(U1, 1000, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         CALL ParseCommaSepLine(Line, Strings, NumStr)
         IF (NumStr .GE. 5) StnCount = StnCount + 1
         READ(U1, 1000, IOSTAT=IOS) Line
      END DO

      !
      !  Allocate the big array
      !
      ALLOCATE(StationMeta(StnCount), STAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error allocating StationMeta array for ISH-format meta data file.'
         PRINT*, 'Station count was ', StnCount
         CALL EXIT(1)
      END IF

      !
      !  Initialize all of the station IDs just in case there are any
      !  issues while processing the supposedly valid lines
      !
      DO I = 1, StnCount
         StationMeta(I)%StnID = 'xxxxxxxxxxxx'
      END DO

      !
      !  Go back to the start of the file and skip over the header again
      !
      REWIND(U1)
      Done = .FALSE.
      DO WHILE (.NOT. Done)
         READ(U1, 1000, IOSTAT=IOS) Line
         IF (IOS .NE. 0) THEN
            PRINT*, 'Error while reading past header in the master file.'
            PRINT*, 'Processing aborted'
            CALL EXIT(1)
         END IF
         IF (Line(1:13) .EQ. '------------,') Done = .TRUE.
      END DO
      
      !
      !  Now read and store the station metadata
      !
      SNum = 0
      READ(U1, 1000, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         CALL ParseCommaSepLine(Line, Strings, NumStr)
         IF (NumStr .GE. 4) THEN
            READ(Strings(2), *, IOSTAT=J1) SLat
            READ(Strings(3), *, IOSTAT=J2) SLong
            IF (J1+J2 .EQ. 0) THEN
               StationMeta(SNum)%StnID   = Strings(1)
               StationMeta(SNum)%StnName = Strings(5)
               StationMeta(SNum)%Country = 'xx'
               StationMeta(SNum)%Lat     = SLat
               StationMeta(SNum)%Long    = SLong
               StationMeta(SNum)%UseString = Strings(4)
               IF (LEN_TRIM(Strings(5)) .GT. 0) THEN
                  CALL Decode_ExcludeYears(Strings(5), StationMeta(SNum)%ExcludeYears)
               END IF
               SNum = SNum + 1
            END IF
         END IF
         READ(U1, 1000, IOSTAT=IOS) Line
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      
 1000 FORMAT(A80)
 
      END SUBROUTINE ReadStationMeta_MasterList

!--------------------------------------------------------------------
      SUBROUTINE ReadStationMeta_NCEI(HistFileName)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: HistFileName
                 
      INTEGER :: U1, IOS, I, SNum, StnCount
      REAL    :: SLat, SLong
      CHARACTER(LEN=2)  :: Country
      CHARACTER(LEN=5)  :: WBAN
      CHARACTER(LEN=6)  :: USAF
      CHARACTER(LEN=30) :: StnName
      CHARACTER(LEN=59) :: RestOfLine

      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(HistFileName), STATUS='OLD', IOSTAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error opening the station history file. Process Aborted.'
         CALL EXIT(1)
      END IF
      CALL FileWasOpened(U1)

      !
      !  First determine the number of valid lines. This way I can allocate
      !  the big array.
      !
      StnCount = 0
      READ(U1, 1000, IOSTAT=IOS) USAF, WBAN, RestOfLine
      DO WHILE (IOS .EQ. 0)
         READ(RestOfLine, 1050, IOSTAT=I) StnName, Country, StnLat, StnLong
         IF (I .EQ. 0) StnCount = StnCount + 1
         READ(U1, 1000, IOSTAT=IOS) USAF, WBAN, RestOfLine
      END DO

      !
      !  Allocate the big array
      !
      ALLOCATE(StationMeta(StnCount), STAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error allocating StationMeta array for ISH-format meta data file.'
         PRINT*, 'Station count was ', StnCount
         CALL EXIT(1)
      END IF

      !
      !  Now read and store the station metadata
      !  We leave the ExcludeYears array unallocated
      !
      SNum = 0
      READ(U1, 1000, IOSTAT=IOS) USAF, WBAN, RestOfLine
      DO WHILE (IOS .EQ. 0)
            READ(RestOfLine, 1050, IOSTAT=I) StnName, Country, StnLat, StnLong
            IF (I .EQ. 0) THEN
               StationMeta(SNum)%StnID     = USAF // '-' // WBAN
               StationMeta(SNum)%StnName   = StnName
               StationMeta(SNum)%Country   = Country
               StationMeta(SNum)%Lat       = SLat
               StationMeta(SNum)%Long      = SLong
               StationMeta(SNum)%UseString = 'YYYYYYY'    ! by default, use everything
               SNum = SNum + 1
            END IF
         END IF
         READ(U1, 1000, IOSTAT=IOS) USAF, WBAN, RestOfLine
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      
     
 1000 FORMAT(A6, 1X, A5, 1X, A59)
 1050 FORMAT(A30, A2, 12X, F7.3, F8.3) 
 
      END SUBROUTINE ReadStationMeta_NCEI

!--------------------------------------------------------------------
!  Detect the format of the station metadata file and then call the
!  appropriate routine to get the info.
!
      SUBROUTINE ReadStationInfo(HistFileName)
      CHARACTER(LEN=*) :: HistFileName
      
      INTEGER :: U1, IOS
      CHARACTER(LEN=11) :: LString
      
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(HistFileName), STATUS='OLD', IOSTAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error opening the station history file. Process Aborted.'
         CALL EXIT(1)
      END IF
      CALL FileWasOpened(U1)
      
      !
      !  Read just the first part of the first line.
      !  If it matches "master list" (case-insensitive) then process it
      !  as a master list file.  Otherwise assume it's in NCEI ISH format.
      !
      READ(U1, 1000) LString
      CLOSE(U1)
      CALL FileWasClosed(U1)
      
      
      CALL Lowercase(LString)
      IF (LString .EQ. 'master list') THEN
         CALL ReadStationMeta_MasterList(HistFileName)
      ELSE
         CALL ReadStationMeta_NCEI(HistFileName)
      END IF

 1000 FORMAT(A11)

      END SUBROUTINE ReadStationInfo

!-----------------------------------------------------------------------------      
      SUBROUTINE ProcessOneStation(DataFileName, EndHour, RetStat)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: DataFileName
      INTEGER,          INTENT(IN)  :: EndHour
      INTEGER,          INTENT(OUT) :: RetStat
      
      INTEGER :: I, J, U1, IOS, LN, NumLines, DaysWritten
      INTEGER :: Year, Month, Day, HH, MM, NumDays
      INTEGER :: ErrStat, Seq, SeqStart, SeqFinish
      INTEGER :: HourOffset, StnMetaIndex
      LOGICAL :: OK
      REAL    :: Latitude, Longitude, StnLat, StnLong
      CHARACTER(LEN=2)     :: Country
      CHARACTER(LEN=5)     :: WBAN_ID
      CHARACTER(LEN=6)     :: USAF_ID
      CHARACTER(LEN=30)    :: StnName
      CHARACTER(LEN=100)   :: FileName
      CHARACTER(LEN=10104) :: DataLine     ! max length is 105 mandatory plus up to 9999 variable 

      TYPE (DataFromOneLine)   :: DFOL
      TYPE (TMetCsvFile)       :: MetCsv
      !
      !  These arrays will be dimensioned (1:NumDays)
      !
      REAL, DIMENSION(:), ALLOCATABLE :: DlyTMax, DlyTMin, DlyPrec
      REAL, DIMENSION(:), ALLOCATABLE :: DlyAirT, DlyDewP, DlyWind, DlyClwd
      
      !
      !  These variables will be used for reading the raw data.
      !  Most are dimensioned (1:NumLines).
      !  Note that RawCloud is not actually raw (straight from file), but
      !  rather a result of analyzing the cloud layers on a single line.
      !
      !  The precip variable will be dimensioned (1:NumLines, 1:4) because
      !  there are, potentially, up to 4 precip observations on a single line
      !
      TYPE (ObsTimeValue), DIMENSION(:), ALLOCATABLE :: RawAirTemp, RawDewPt
      TYPE (ObsTimeValue), DIMENSION(:), ALLOCATABLE :: RawWind, RawCloud
      TYPE (PrecipObs), DIMENSION(:,:), ALLOCATABLE :: RawPrecip

      !
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(DataFileName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)
      
      !
      !  Read first line just to get the station ID
      !
      READ(U1, 1101, IOSTAT=IOS) USAF_ID, WBAN_ID, Latitude, Longitude
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error reading station ID and location from line 1.'
         CALL EXIT(1)
      END IF
      
      !
      !  Do a fast pass through the file, determining the date extent
      !  and the total number of data lines. Data is ignored at this stage.
      !
      !  Note that in order to simplify all of the subsequent processing, I will be
      !  translating the actual observation times right here. I want all of the DATES
      !  to match up with the right day. So, for example, if the user has specified 
      !  an end hour of 12:00 noon, then I need to do an offset of 12 hours forward. 
      !  The net effect of that will be to push a 1 pm observation to 1 am of the next 
      !  day, and that observation will be included in the accumulation for that next
      !  day -- exactly what I want.
      !
      !  How to compute offset?
      !  If the user says 1, that means accumulate for the 24 hours prior to 1 am.
      !  That means I really need to shift the last 23 hours from YESTERDAY into
      !  today and add the first hour of today. So the offset is 23.
      !
      !  If the user says 2, that means accumulate for the 24 hours prior to 2 am.
      !  That means I really need to shift the last 22 hours from YESTERDAY into
      !  today and add the first 2 hours of today. So the offset is 22.
      !  ...
      !  If the user says 23, that means accumulate for the 24 hours prior to 11 pm.
      !  That means I really need to shift the last 1 hour from YESTERDAY into
      !  today and add the first 23 hours of today. So the offset is just 1.
      !
      !  If the user says 24, that means accumulate for the 24 hours prior to midnight.
      !  That means I really need to shift nothing from YESTERDAY into
      !  today and add the first 24 hours of today. So the offset is just 0.
      !
      REWIND(U1)
      NumLines = 0
      CALL DateSequence(31, 12, 2999, SeqStart)
      CALL DateSequence( 1,  1, 1600, SeqFinish)
      HourOffset = 24 - EndHour
      IF (HourOffset .EQ. 24) HourOffset = 0
      READ(U1, 1105, IOSTAT=IOS) DataLine
      DO WHILE (IOS .EQ. 0) 
         IF (LEN_TRIM(DataLine) .GE. 100) THEN
            READ(DataLine, 1102, IOSTAT=J) Year, Month, Day, HH, MM
            IF (J .EQ. 0) THEN           ! in case line was blank or otherwise "screwy"
               NumLines = NumLines + 1
               CALL TranslateUTCByOffset(Year, Month, Day, HH, HourOffset)
               CALL DateSequence(Day, Month, Year, Seq)
               IF (Seq .LT. SeqStart)  SeqStart  = Seq
               IF (Seq .GT. SeqFinish) SeqFinish = Seq
            END IF
         END IF
         READ(U1, 1105, IOSTAT=IOS) DataLine
      END DO
    
      !
      !  Set up the data storage
      !
      ALLOCATE(RawAirTemp(NumLines), RawDewpt(NumLines), RawWind(NumLines),     &
               RawCloud(NumLines), RawPrecip(NumLines,4), STAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error allocating the raw data storage arrays.'
         CALL EXIT(1)
      END IF
      
      NumDays = SeqFinish - SeqStart + 1
      ALLOCATE(DlyTMax(NumDays), DlyTMin(NumDays), DlyAirT(NumDays),       &
               DlyDewP(NumDays), DlyWind(NumDays), DlyClwd(NumDays),       &
               DlyPrec(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error allocating the daily data storage arrays.'
         CALL EXIT(1)
      END IF
      
      !
      !  Initialize all of the data arrays
      !
      DO I = 1, NumLines
         RawAirTemp(I)%ObsDate = -99
         RawDewPt(I)%ObsDate = -99
         RawWind(I)%ObsDate = -99
         RawCloud(I)%ObsDate = -99
         DO J = 1, 4
            RawPrecip(I,J)%ObsDate = -99
         END DO
         
         RawAirTemp(I)%ObsTime = -99
         RawDewPt(I)%ObsTime = -99
         RawWind(I)%ObsTime = -99
         RawCloud(I)%ObsTime = -99
         DO J = 1, 4
            RawPrecip(I,J)%ObsTime = -99
         END DO

         RawAirTemp(I)%DVal = MissingData_Real
         RawDewPt(I)%DVal   = MissingData_Real
         RawWind(I)%DVal    = MissingData_Real
         RawCloud(I)%DVal   = MissingData_Real
         DO J = 1, 4
            RawPrecip(I,J)%DVal = MissingData_Real
         END DO
      END DO

      DlyTMax(:) = MissingData_Real
      DlyTMin(:) = MissingData_Real
      DlyAirT(:) = MissingData_Real
      DlyDewP(:) = MissingData_Real
      DlyWind(:) = MissingData_Real
      DlyClwd(:) = MissingData_Real
      DlyPrec(:) = MissingData_Real
      
      !
      !  Read and store raw data
      !
      REWIND(U1)
      LN = 1       ! Line Number
      READ(U1, 1105, IOSTAT=IOS) DataLine
      DO WHILE (IOS .EQ. 0)
         IF (LEN_TRIM(DataLine) .GT. 100) THEN
            CALL ParseDataLine(DataLine, DFOL, ErrStat)
            OK = .TRUE.
            IF (ErrStat   .NE.       0) OK = .FALSE.
            IF (DFOL%USAF .NE. USAF_ID) OK = .FALSE.
            IF (DFOL%WBAN .NE. WBAN_ID) OK = .FALSE.
            IF (OK) THEN
               RawAirTemp(LN)%ObsDate = DFOL%ObsDate
               RawDewPt(LN)%ObsDate   = DFOL%ObsDate
               RawWind(LN)%ObsDate    = DFOL%ObsDate
               RawCloud(LN)%ObsDate   = DFOL%ObsDate

               RawAirTemp(LN)%ObsTime = DFOL%ObsTime
               RawDewPt(LN)%ObsTime   = DFOL%ObsTime
               RawWind(LN)%ObsTime    = DFOL%ObsTime
               RawCloud(LN)%ObsTime   = DFOL%ObsTime

               RawAirTemp(LN)%DVal = DFOL%AirTemp
               RawDewPt(LN)%DVal   = DFOL%DewPoint
               RawWind(LN)%DVal    = DFOL%WindSpd
               RawCloud(LN)%DVal   = DFOL%Cloud
               DO I = 1, 4
                  RawPrecip(LN,I)%ObsDate = DFOL%ObsDate
                  RawPrecip(LN,I)%ObsTime = DFOL%ObsTime
                  RawPrecip(LN,I)%Hours   = DFOL%Precip(I)%Hours
                  RawPrecip(LN,I)%DVal    = DFOL%Precip(I)%DVal
               END DO
            END IF
            LN = LN + 1
         END IF
         READ(U1, 1105, IOSTAT=IOS) DataLine
      END DO
      
      CLOSE(U1)
      CALL FileWasClosed(U1)
      
      !
      !  Now that the raw data is read, turn it into daily values.
      !  Precip is handled very differently from the others.
      !
      CALL ComputeDailyMax(RawAirTemp, DlyTMax, SeqStart, SeqFinish)
      CALL ComputeDailyMin(RawAirTemp, DlyTMin, SeqStart, SeqFinish)
      CALL ComputeDailyAvg(RawAirTemp, DlyAirT, SeqStart, SeqFinish)
      CALL ComputeDailyAvg(RawDewPt,   DlyDewP, SeqStart, SeqFinish)
      CALL ComputeDailyAvg(RawWind,    DlyWind, SeqStart, SeqFinish)
      CALL ComputeDailyAvg(RawCloud,   DlyClwd, SeqStart, SeqFinish)

      CALL ComputeDailyPrecip(RawPrecip, DlyPrec, SeqStart, SeqFinish)
      
      !
      !  Build/populate the data structure used for writing the output file
      !
      CALL InitializeDlyMetStnDataset(MetCsv%DlyData)

      !
      !  Given the USAF and WBAN ID numbers, scan through the station metadata
      !  that was already read, and get the index for the matching entry.
      !
      I = GetStationMetaIndex(USAF_ID, WBAN_ID)
      IF (I .LT. 0) THEN
         PRINT*, 'Error finding station meta index for ', USAF_ID//'-'//WBAN_ID
         CALL EXIT(1)
      END IF
      
      MetCsv%DlyData%ID      = TRIM(USAF_ID) // '-' // TRIM(WBAN_ID)
      MetCsv%DlyData%Name    = TRIM(StationMeta(I)%StnName)
      MetCsv%DlyData%Country = TRIM(StationMeta(I)%Country)
      MetCsv%DlyData%Lat     = StationMeta(I)%StnLat
      MetCsv%DlyData%Long    = StationMeta(I)%StnLong
      MetCsv%DlyData%SDate   = SeqStart
      MetCsv%DlyData%EDate   = SeqFinish
      MetCsv%DlyData%NumDataTypes = 7

      ALLOCATE(MetCsv%DlyData%DataTypes(7), MetCsv%DlyData%DataUnits(7),      &
               MetCsv%DlyData%MetData(7,NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error allocating RAM for the output data.'
         CALL EXIT(1)
      END IF
      
      MetCsv%DlyData%DataTypes(1) = MDT_AirtempMax
      MetCsv%DlyData%DataTypes(2) = MDT_AirtempMin
      MetCsv%DlyData%DataTypes(3) = MDT_Precipitation
      MetCsv%DlyData%DataTypes(4) = MDT_AirtempMean
      MetCsv%DlyData%DataTypes(5) = MDT_DewpointMean
      MetCsv%DlyData%DataTypes(6) = MDT_WindSpeed
      MetCsv%DlyData%DataTypes(7) = MDT_CloudCover

      MetCsv%DlyData%DataUnits(1) = MDU_Celsius
      MetCsv%DlyData%DataUnits(2) = MDU_Celsius
      MetCsv%DlyData%DataUnits(3) = MDU_Millimeters
      MetCsv%DlyData%DataUnits(4) = MDU_Celsius
      MetCsv%DlyData%DataUnits(5) = MDU_Celsius
      MetCsv%DlyData%DataUnits(6) = MDU_MetersPerSecond
      MetCsv%DlyData%DataUnits(7) = MDU_Percent
      
      DO Seq = SeqStart, SeqFinish
         I = Seq - SeqStart + 1
         MetCsv%DlyData%MetData(1,I) = DlyTMax(I)
         MetCsv%DlyData%MetData(2,I) = DlyTMin(I)
         MetCsv%DlyData%MetData(3,I) = DlyPrec(I)
         MetCsv%DlyData%MetData(4,I) = DlyAirT(I)
         MetCsv%DlyData%MetData(5,I) = DlyDewP(I)
         MetCsv%DlyData%MetData(6,I) = DlyWind(I)
         MetCsv%DlyData%MetData(7,I) = DlyClwd(I)
      END DO      

      !
      !  Write the CSV-format output file
      !
      FileName = 'met_' // TRIM(USAF_ID) // '-' // TRIM(WBAN_ID) // '.csv'
      CALL WriteMetCsvFile(TRIM(FileName), DaysWritten, MetCsv, IOS)
      IF (IOS .NE. 0) THEN
         PRINT*, 'Error writing output CSV file. Aborting.'
         CALL EXIT(1)
      END IF

      ErrorLevel = 0   
      GOTO 999     ! clean up and exit
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file '//TRIM(DataFileName)
      CALL PassMsg; GOTO 898
      
  898 ErrorLevel = 1
      ErrorMessage = 'Program aborted!';  CALL PassMsg


  999 DEALLOCATE(MetCsv%DlyData%MetData, MetCsv%DlyData%DataUnits,       &
                 MetCsv%DlyData%DataTypes, STAT=IOS)
      DEALLOCATE(DlyTMax, DlyTMin, DlyPrec, DlyAirT, DlyDewP,      &
                 DlyWind, DlyClwd, STAT=IOS)
      DEALLOCATE(RawAirTemp, RawDewpt, RawWind,                    &
                 RawCloud, RawPrecip, STAT=IOS)
      RetStat = ErrorLevel
      
      
 1101 FORMAT(4X, A6, A5, 13X, F6.3, F7.3)
 1102 FORMAT(15X, I4, 4I2)
 1105 FORMAT(A10104)
      
      END SUBROUTINE ProcessOneStation      
      
      
   END MODULE Process_ISH_Subs
   
   
!==============================================================================   
!==============================================================================   
!==============================================================================   
   PROGRAM Process_ISH
   USE Process_ISH_Subs
   IMPLICIT NONE

   INTEGER :: I, IOS, ArgC, EndHour
   INTEGER :: ErrStat
   CHARACTER(LEN=100)  :: HistFileName, DataFileName, Args(9999)
   CHARACTER(LEN=99999) :: CLine
   
   !
   !  Get the data file name from the command line
   !
   CALL GET_COMMAND(CLine)
   CALL ParseCmdLine(CLine, Args, ArgC)
   IF (ErrorLevel .NE. 0) ArgC = 0
   IF (ArgC .LT. 3) THEN
      PRINT*, 'USAGE: Process_ISH histfile endhour filename(s)'
      PRINT*, '     histfile = station metadata file'
      PRINT*, '                Either in the NCEI format (e.g. ish-history.txt) or in'
      PRINT*, '                the GLERL-defined "master list" format also used for GHCND.'
      PRINT*, '     endhour = end hour for accumulation. (1-24)'
      PRINT*, '     filename(s) = data file(s) containing ish-format dataset (wildcards ok)'
      PRINT*, 'Note that these files must all be in the current folder/directory.'
      CALL EXIT(1)
   END IF
   
   HistFileName = Args(2)
   READ(Args(3), *, IOSTAT=IOS) EndHour
   IF ((IOS .NE. 0) .OR. ((EndHour .LT. 1) .OR. (EndHour .GT. 24))) THEN
      PRINT*, 'Invalid EndHour entry.  Must be integer in range 1-24.'
      CALL EXIT(1)
   END IF

   !
   !  Read the station metadata file
   !
   CALL ReadStationInfo(HistFileName)
   
   !
   !  Now process each of the specified data files
   !
   DO I = 4, ArgC
      DataFileName = Args(I)
      print*, 'Processing '//TRIM(DataFileName)
      CALL ProcessOneStation(DataFileName, EndHour, ErrStat)
      IF (ErrStat .NE. 0) THEN
         PRINT*, 'Program aborted.'
         CALL EXIT(1)
      END IF
   END DO
   
   END PROGRAM