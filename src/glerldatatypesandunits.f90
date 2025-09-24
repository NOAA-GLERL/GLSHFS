!----------------------------------------------------------------------------------
!  Define a bunch of data types (and names to describe them) and a bunch of
!  data units (and their names).  The intent is that this module can be
!  used for various purposes to make text files more readable/consistent, 
!  internal data storage more consistent, etc.  Consistency being a key point.
!
!  This acts as sort of a surrogate for enumerated types, since Fortran doesn't
!  have those.  Note that there is no Fortran requirement for the assigned values
!  to be in any kind of sequential order, or contiguous, or even unique. Treating
!  these like an enumerated type is something we are forcing by convention. Thus
!  it is 100% the programmer's responsibility to ensure that the values are
!  unique (if/when desired) and that they make sense for us.
!  
!
!  This module full of definitions will likely grow to the point where it feels 
!  a little bit ungainly, but it should make all of  the code that uses it a lot 
!  easier to follow and help reduce coding "oopsies".
!
!  Tim Hunter
!----------------------------------------------------------------------------------

MODULE GlerlDataTypesAndUnits

      PUBLIC

      INTEGER, PARAMETER :: GDT_Undefined            = -1
      INTEGER, PARAMETER :: GDT_AirtempMax           =  2
      INTEGER, PARAMETER :: GDT_AirtempMin           =  3
      INTEGER, PARAMETER :: GDT_AirtempMean          =  4
      INTEGER, PARAMETER :: GDT_Precipitation        =  5
      INTEGER, PARAMETER :: GDT_DewpointMean         =  6
      INTEGER, PARAMETER :: GDT_WindSpeed            =  7
      INTEGER, PARAMETER :: GDT_CloudCover           =  8
      INTEGER, PARAMETER :: GDT_WindDirection        =  9
      INTEGER, PARAMETER :: GDT_SkyCoverTotal        = 10
      INTEGER, PARAMETER :: GDT_SkyCoverOpaque       = 11
      INTEGER, PARAMETER :: GDT_AtmosPressure        = 12
      INTEGER, PARAMETER :: GDT_Streamflow           = 13       ! Streamflow/Runoff are same/similar, but
      INTEGER, PARAMETER :: GDT_Runoff               = 14       ! I want both names available
      INTEGER, PARAMETER :: GDT_UpperSoilMoisture    = 15       ! Upper Soil Zone Moisture Storage (LBRM)
      INTEGER, PARAMETER :: GDT_LowerSoilMoisture    = 16       ! Lower Soil Zone Moisture Storage (LBRM)
      INTEGER, PARAMETER :: GDT_TotalSoilMoisture    = 17       ! Upper + Lower Soil Zone Moisture Storage (LBRM)
      INTEGER, PARAMETER :: GDT_GroundWaterMoisture  = 18       ! Groundwater Zone Moisture Storage (LBRM)
      INTEGER, PARAMETER :: GDT_SurfaceZoneMoisture  = 19       ! Surface Zone Moisture Storage (LBRM)
      INTEGER, PARAMETER :: GDT_SnowWater            = 20       ! Snow Water Equivalent
      INTEGER, PARAMETER :: GDT_TotalMoisture        = 21       ! USZM + LSZM + GZM + Surface + Snow (LBRM)
      INTEGER, PARAMETER :: GDT_SnowDepth            = 22
      INTEGER, PARAMETER :: GDT_Evaporation          = 23
      INTEGER, PARAMETER :: GDT_WaterTemp            = 24
      INTEGER, PARAMETER :: GDT_VaporPressure        = 25
      INTEGER, PARAMETER :: GDT_Humidity             = 26
      INTEGER, PARAMETER :: GDT_IncidentRad          = 27
      INTEGER, PARAMETER :: GDT_ReflectedRad         = 28
      INTEGER, PARAMETER :: GDT_NetLongWaveRad       = 29
      INTEGER, PARAMETER :: GDT_LatentRad            = 30
      INTEGER, PARAMETER :: GDT_SensibleRad          = 31
      INTEGER, PARAMETER :: GDT_Advection            = 32
      INTEGER, PARAMETER :: GDT_NetSurfaceRad        = 33
      INTEGER, PARAMETER :: GDT_TotalHeat            = 34
      INTEGER, PARAMETER :: GDT_IceTemp              = 35
      INTEGER, PARAMETER :: GDT_IceArea              = 36
      INTEGER, PARAMETER :: GDT_IceDepth             = 37
      INTEGER, PARAMETER :: GDT_NetBasinSupply       = 38
      INTEGER, PARAMETER :: GDT_MeanLakeLevel        = 39
      INTEGER, PARAMETER :: GDT_EndLakeLevel         = 40      
      INTEGER, PARAMETER :: GDT_Inflow               = 41
      INTEGER, PARAMETER :: GDT_Outflow              = 42
      INTEGER, PARAMETER :: GDT_TotalBasinSupply     = 43
      INTEGER, PARAMETER :: GDT_OverlakeAirTempMin   = 44       ! In addition to the generic types, I need some specific overLAKE
      INTEGER, PARAMETER :: GDT_OverlakeAirTempMax   = 45       ! and overLAND types in order to differentiate them in certain
      INTEGER, PARAMETER :: GDT_OverlakeAirTempMean  = 46       ! situations. May need to add to this set over time.
      INTEGER, PARAMETER :: GDT_OverlakePrecip       = 47       
      INTEGER, PARAMETER :: GDT_OverlakeDewpoint     = 48
      INTEGER, PARAMETER :: GDT_OverlakeWindSpeed    = 49
      INTEGER, PARAMETER :: GDT_OverlakeCloudCover   = 50
      INTEGER, PARAMETER :: GDT_OverlakeRunoff       = 51
      INTEGER, PARAMETER :: GDT_OverlandAirTempMin   = 52
      INTEGER, PARAMETER :: GDT_OverlandAirTempMax   = 53
      INTEGER, PARAMETER :: GDT_OverlandAirTempMean  = 54
      INTEGER, PARAMETER :: GDT_OverlandPrecip       = 55
      INTEGER, PARAMETER :: GDT_OverlandDewpoint     = 56
      INTEGER, PARAMETER :: GDT_OverlandWindSpeed    = 57
      INTEGER, PARAMETER :: GDT_OverlandCloudCover   = 58
      INTEGER, PARAMETER :: GDT_OverlandRunoff       = 59
     

      INTEGER, PARAMETER :: GDU_Undefined         = 1
      INTEGER, PARAMETER :: GDU_Fahrenheit        = 2
      INTEGER, PARAMETER :: GDU_Celsius           = 3
      INTEGER, PARAMETER :: GDU_Inches            = 4
      INTEGER, PARAMETER :: GDU_Feet              = 5
      INTEGER, PARAMETER :: GDU_Yards             = 6
      INTEGER, PARAMETER :: GDU_Miles             = 7
      INTEGER, PARAMETER :: GDU_Millimeters       = 8
      INTEGER, PARAMETER :: GDU_Centimeters       = 9
      INTEGER, PARAMETER :: GDU_Meters            = 10
      INTEGER, PARAMETER :: GDU_Kilometers        = 11
      INTEGER, PARAMETER :: GDU_MilesPerHour      = 12
      INTEGER, PARAMETER :: GDU_FeetPerSecond     = 13
      INTEGER, PARAMETER :: GDU_Knots             = 14
      INTEGER, PARAMETER :: GDU_KilometersPerHour = 15
      INTEGER, PARAMETER :: GDU_MetersPerSecond   = 16
      INTEGER, PARAMETER :: GDU_Tenths            = 17
      INTEGER, PARAMETER :: GDU_Percent           = 18
      INTEGER, PARAMETER :: GDU_Fraction          = 19
      INTEGER, PARAMETER :: GDU_Direction360      = 20
      INTEGER, PARAMETER :: GDU_HectoPascals      = 21
      INTEGER, PARAMETER :: GDU_Millibars         = 22
      INTEGER, PARAMETER :: GDU_WattsPerM2        = 23
      INTEGER, PARAMETER :: GDU_Calories          = 24
      INTEGER, PARAMETER :: GDU_Joules            = 25
      INTEGER, PARAMETER :: GDU_JoulesPerM2       = 26
      INTEGER, PARAMETER :: GDU_Langleys          = 27
      INTEGER, PARAMETER :: GDU_SquareMillimeters = 28
      INTEGER, PARAMETER :: GDU_SquareCentimeters = 29
      INTEGER, PARAMETER :: GDU_SquareMeters      = 30
      INTEGER, PARAMETER :: GDU_SquareKilometers  = 31
      INTEGER, PARAMETER :: GDU_Hectares          = 32
      INTEGER, PARAMETER :: GDU_SquareInches      = 33
      INTEGER, PARAMETER :: GDU_SquareFeet        = 34
      INTEGER, PARAMETER :: GDU_SquareYards       = 35
      INTEGER, PARAMETER :: GDU_SquareMiles       = 36
      INTEGER, PARAMETER :: GDU_Acres             = 37
      INTEGER, PARAMETER :: GDU_CubicMillimeters  = 38
      INTEGER, PARAMETER :: GDU_CubicCentimeters  = 39
      INTEGER, PARAMETER :: GDU_CubicMeters       = 40
      INTEGER, PARAMETER :: GDU_CubicInches       = 41
      INTEGER, PARAMETER :: GDU_CubicFeet         = 42
      INTEGER, PARAMETER :: GDU_CubicYards        = 43
      INTEGER, PARAMETER :: GDU_CubicMetersPerSec = 44
      INTEGER, PARAMETER :: GDU_CubicFeetPerSec   = 45

      !
      !  These character arrays need to be matched exactly with the 
      !  definitions above.  e.g. Entry 7 in the array must match with
      !  the data type or unit assigned a value of 7.
      !  Note that these arrays are PRIVATE to the module. Access to these
      !  strings will only be allowed via the functions.
      !
      CHARACTER(LEN=4), DIMENSION(59) :: GlerlDataTypeNames4 =          &
         (/'UNDF', 'TMAX', 'TMIN', 'PREC', 'AIRT', 'DEWP',              &
           'WSPD', 'CLDC', 'WDIR', 'SKYT', 'SKYO', 'APRS',              &
           'SFLW', 'RUNF', 'USZM', 'LSZM', 'TSZM', 'GRWM',              &
           'SRFM', 'SNWE', 'TTLM', 'SNWD', 'EVAP', 'WTMP',              &
           'VPRS', 'RHUM', 'ISWR', 'RFLC', 'NLWR', 'LTNT',              &
           'SENS', 'ADVC', 'NSFR', 'TTLH', 'ICET', 'ICEA',              &
           'ICED', 'NBSP', 'MLVL', 'ELVL', 'INFL', 'OTFL',              &
           'TBSP',                                                      &
           'LKTN', 'LKTX', 'LKTA', 'LKPC', 'LKDP', 'LKWS',              &
           'LKCC', 'LKRN',                                              &
           'LDTN', 'LDTX', 'LDTA', 'LDPC', 'LDDP', 'LDWS',              &
           'LDCC', 'LDRN' /)

      CHARACTER(LEN=10), DIMENSION(59) :: GlerlDataTypeNames10 =        &
         (/'Undefined ', 'AirTempMax', 'AirTempMin',                    &
           'AirTemp   ', 'Precip    ', 'Dewpoint  ',          &
           'Windspeed ', 'CloudCover', 'WindDirctn',          &
           'SkyCvrTotl', 'SkyCvrOpaq', 'AtmosPress',          &
           'Streamflow', 'Runoff    ', 'UprSoilMst',          &
           'LwrSoilMst', 'TotalSoilM', 'GroundWatr',          &
           'SurfaceSto', 'SnowWatrEq', 'TotMoistur',          &
           'SnowDepth ', 'Evaporatn ', 'WaterTemp ',          &
           'VaporPresr', 'Humidity  ', 'IncidntRad',          &
           'ReflectRad', 'NetLWRad  ', 'LatentRad ',          &
           'SensiblRad', 'Advection ', 'NetSrfcRad',          &
           'TotalHeat ', 'IceTemp   ', 'IceArea   ',          &
           'IceDepth  ', 'NetBsnSupl', 'AvgLakeLvl',          &
           'EndLakeLvl', 'Inflow    ', 'Outflow   ',          &
           'TotBsnSply',                                      &
           'OvrLkATMin', 'OvrLkATMax', 'OvrLkATAvg',          &
           'OvrLkPrecp', 'OvrLkDewpt', 'OvrLkWndSp',          &
           'OvrLkCloud', 'OvrLkRunof',                        &
           'OvrLdATMin', 'OvrLdATMax', 'OvrLdATAvg',          &
           'OvrLdPrecp', 'OvrLdDewpt', 'OvrLdWndSp',          &
           'OvrLdCloud', 'OvrLdRunof' /)
           
      CHARACTER(LEN=15), DIMENSION(59) :: GlerlDataTypeNames =          &
         (/'Undefined      ', 'AirTempMax     ', 'AirTempMin     ',     &
           'AirTemp        ', 'Precipitation  ', 'Dewpoint       ',     &
           'Windspeed      ', 'CloudCover     ', 'WindDirection  ',     &
           'SkyCoverTotal  ', 'SkyCoverOpaque ', 'AtmosPressure  ',     &
           'Streamflow     ', 'Runoff         ', 'UpperSoilMoist ',     &
           'LowerSoilMoist ', 'TotalSoilMoist ', 'GroundWater    ',     &
           'SurfaceStorage ', 'SnowWaterEquiv ', 'TotalMoisture  ',     &
           'SnowDepth      ', 'Evaporation    ', 'WaterTemp      ',     &
           'VaporPressure  ', 'Humidity       ', 'IncidentRad    ',     &
           'ReflectedRad   ', 'NetLongWaveRad ', 'LatentRadFlux  ',     &
           'SensibleRadFlux', 'Advection      ', 'NetSrfaceRadFlx',     &
           'TotalHeat      ', 'IceTemp        ', 'IceArea        ',     &
           'IceDepth       ', 'NetBasinSupply ', 'MeanLakeLevel  ',     &
           'EndingLakeLevel', 'Inflow         ', 'Outflow        ',     &
           'TotalBasinSply ',                                           &
           'OverLakeATmpMin', 'OverLakeATmpMax', 'OverLakeATmpAvg',     &
           'OverLakePrecip ', 'OverLakeDewpt  ', 'OverLakeWindSpd',     &
           'OverLakeCloud  ', 'OverLakeRunoff ',                        &
           'OverLandATmpMin', 'OverLandATmpMax', 'OverLandATmpAvg',     &
           'OverLandPrecip ', 'OverLandDewpt  ', 'OverLandWindSpd',     &
           'OverLandCloud  ', 'OverLandRunoff ' /)
           
      CHARACTER(LEN=30), DIMENSION(59) :: GlerlDataTypeNamesLong =               &
         (/'Undefined                     ', 'Maximum Air Temperature       ',   &
           'Minimum Air Temperature       ', 'Air Temperature               ',   &
           'Precipitation                 ', 'Dewpoint Temperature          ',   &
           'Wind Speed                    ', 'Cloud Cover                   ',   &
           'Wind Direction                ', 'Total Sky Cover               ',   &
           'Opaque Sky Cover              ', 'Atmospheric Pressure          ',   &
           'Stream Flow                   ', 'Runoff                        ',   &
           'Upper Soil Zone Moisture      ', 'Lower Soil Zone Moisture      ',   &
           'Total Soil Zone Moisture      ', 'Groundwater Zone Moisture     ',   &
           'Surface Zone Moisture         ', 'Snow Water Equivalent         ',   &
           'Total Moisture Storage        ', 'Snow Depth                    ',   &
           'Evaporation                   ', 'Water Temperature             ',   &
           'Vapor Pressure                ', 'Relative Humidity             ',   &
           'Incident Radiation Flux       ', 'Reflected Radiation Flux      ',   &
           'Net Long Wave Radiation Flux  ', 'Latent Radiation Flux         ',   &
           'Sensible Radiation Flux       ', 'Advection                     ',   &
           'Net Surface Radiation Flux    ', 'Total Heat Storage            ',   &
           'Ice Temperature               ', 'Ice Area                      ',   &
           'Ice Depth                     ', 'Net Basin Supply              ',   &
           'Mean Lake Level               ', 'Ending Lake Level             ',   &
           'Inflow                        ', 'Outflow                       ',   &
           'Total Basin Supply            ',                                     &
           'Overlake Minimum Air Temp     ', 'Overlake Maximum Air Temp     ',   &
           'Overlake Mean Air Temperature ', 'Overlake Precipitation        ',   &
           'Overlake Dewpoint Temperature ', 'Overlake Wind Speed           ',   &
           'Overlake Cloud Cover          ', 'Overlake Runoff               ',   &          
           'Overland Minimum Air Temp     ', 'Overland Maximum Air Temp     ',   &
           'Overland Mean Air Temperature ', 'Overland Precipitation        ',   &
           'Overland Dewpoint Temperature ', 'Overland Wind Speed           ',   &
           'Overland Cloud Cover          ', 'Overland Runoff               ' /)

           
      CHARACTER(LEN=6), DIMENSION(45) :: GlerlDataUnitNames6 =            &
         (/'UNDEF ', 'DEG F ', 'DEG C ', 'INCHES', 'FEET  ', 'YARDS ',    &
           'MILES ', 'MM    ', 'CM    ', 'METERS', 'KM    ', 'MPH   ',    &
           'FPS   ', 'KNOTS ', 'KM/HR ', 'M/S   ', 'TENTHS', '%     ',    &
           'FRACT ', 'DIR360', 'HPA   ', 'MBAR  ', 'W/M2  ', 'CALS  ',    &
           'JOULES', 'J/M2  ', 'LANGLY', 'SQ MM ', 'SQ CM ', 'SQ M  ',    &
           'SQ KM ', 'HECTAR', 'SQ IN ', 'SQ FT ', 'SQ YD ', 'SQ MI ',    &
           'ACRES ', 'CU MM ', 'CU CM ', 'CU M  ', 'CU IN ', 'CU FT ',    &
           'CU YD ', 'CMS   ', 'CFS   '  /)

      CHARACTER(LEN=10), DIMENSION(45) :: GlerlDataUnitNames10 =                 &
         (/'Undefined ', 'Fahrenheit', 'Celsius   ',     &
           'Inches    ', 'Feet      ', 'Yards     ',     &
           'Miles     ', 'Millimeter', 'Centimeter',     &
           'Meters    ', 'Kilometers', 'MilesPerHr',     &
           'FeetPerSec', 'Knots     ', 'KmPerHour ',     &
           'Meters/Sec', 'Tenths    ', 'Percent   ',     &
           'Fraction  ', 'Directn360', 'HectoPascl',     &
           'Millibars ', 'Watts/SqM ', 'Calories  ',     &
           'Joules    ', 'Joules/SqM', 'Langleys  ',     &
           'Square-mm ', 'Square-cm ', 'Square-m  ',     &
           'Square-km ', 'Hectares  ', 'Square-in ',     &
           'SquareFeet', 'SquareYard', 'SquareMile',     &
           'Acres     ', 'Cubic-mm  ', 'Cubic-cm  ',     &
           'CubicMeter', 'CubicInch ', 'CubicFeet ',     &
           'CubicYards', 'CubicMtr/S', 'CubicFt/S ' /)
           
      CHARACTER(LEN=18), DIMENSION(45) :: GlerlDataUnitNames =                   &
         (/'Undefined         ', 'Fahrenheit        ', 'Celsius           ',     &
           'Inches            ', 'Feet              ', 'Yards             ',     &
           'Miles             ', 'Millimeters       ', 'Centimeters       ',     &
           'Meters            ', 'Kilometers        ', 'MilesPerHour      ',     &
           'FeetPerSecond     ', 'Knots             ', 'KilometersPerHour ',     &
           'MetersPerSecond   ', 'Tenths            ', 'Percent           ',     &
           'Fraction          ', 'Direction360      ', 'HectoPascals      ',     &
           'Millibars         ', 'WattsPerSqMeter   ', 'Calories          ',     &
           'Joules            ', 'JoulesPerSqMeter  ', 'Langleys          ',     &
           'SquareMillimeters ', 'SquareCentimeters ', 'SquareMeters      ',     &
           'SquareKilometers  ', 'Hectares          ', 'SquareInches      ',     &
           'SquareFeet        ', 'SquareYards       ', 'SquareMiles       ',     &
           'Acres             ', 'CubicMillimeters  ', 'CubicCentimeters  ',     &
           'CubicMeters       ', 'CubicInches       ', 'CubicFeet         ',     &
           'CubicYards        ', 'CubicMetersPerSec ', 'CubicFeetPerSec   ' /)
           

      !
      !  Data Unit Precision
      !  Typical (recommended) precision to use when writing output values.
      !  This number tells how many digits after the decimal point to use.
      !  Each entry corresponds to the Glerl Data Unit matching the index.
      !  e.g. entry 1 corresponds to MDU_Undefined
      !       entry 2 corresponds to MDU_Fahrenheit
      !       entry 3 corresponds to MDU_Celsius
      !       entry 4 corresponds to MDU_Inches
      !       ... etc ...
      !
      !  Note that these values are certainly arbitrary. I choose them based
      !  simply on a gut feeling about typical precision of measurements.
      !  They could easily be revised in the future.
      !
      !  I have broken it into 10 values per line strictly for readability
      !  and quick reference alignment.
      !
      INTEGER, DIMENSION(45) :: DataUnitOutputPrecision =    &
               (/1, 1, 1, 2, 3, 3, 1, 2, 3, 4,               &
                 4, 3, 4, 2, 1, 1, 1, 1, 1, 1,               &
                 2, 2, 3, 1, 2, 2, 2, 2, 2, 2,               &
                 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,               &
                 2, 2, 2, 2, 2 /)
                 
              
      !
      !  Hide some stuff from users of the module.
      !  This helps keep things cleaner, including the namespace.
      !
      PRIVATE :: GlerlDataTypeNames4, GlerlDataTypeNames10, GlerlDataTypeNames, GlerlDataTypeNamesLong
      PRIVATE :: GlerlDataUnitNames6, GlerlDataUnitNames10, GlerlDataUnitNames
           
      !
      !  These are some things that would normally be found in the GlShfs_Util
      !  module, but I want this module to be independent from that one.
      !  So I am defining local private versions that have the same value or
      !  functionality (therefore compatible) without dependency.
      !
      REAL, PRIVATE, PARAMETER :: MissingData_Real = -9.9e29
      REAL, PRIVATE, PARAMETER :: MissingData_Test = -9.8e27
      PRIVATE :: UprCase, IsMissing
        
CONTAINS


!--------------------------------------------------------------
!-------------------------------------------------------------------------
      INTEGER FUNCTION GlerlDataTypeFromString(DTS)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: DTS
      INTEGER :: I
      INTEGER, DIMENSION(1) :: UB
      CHARACTER(LEN=4)  :: S4
      CHARACTER(LEN=10) :: S10
      CHARACTER(LEN=30) :: S1, SS, SL
      
      S1 = UprCase(TRIM(DTS))
      UB = UBOUND(GlerlDataTypeNames)
      
      !
      !  In early versions of this code, I had used slightly different
      !  strings. In case an old file is being used, convert those
      !  obsolete strings into the current equivalent.
      !
      IF (S1 .EQ. 'SNOWWATERE') S1 = 'SNOWWATREQ'
      
      DO I = 1, UB(1)
         S4  = UprCase(GlerlDataTypeNames4(I))
         S10 = UprCase(GlerlDataTypeNames10(I))
         SS  = UprCase(GlerlDataTypeNames(I))
         SL  = UprCase(GlerlDataTypeNamesLong(I))
         IF ((TRIM(S1) .EQ. TRIM(S4))  .OR.          &
             (TRIM(S1) .EQ. TRIM(S10)) .OR.          &
             (TRIM(S1) .EQ. TRIM(SS))  .OR.          &
             (TRIM(S1) .EQ. TRIM(SL))) THEN
            GlerlDataTypeFromString = I
            RETURN
         END IF
      END DO
      GlerlDataTypeFromString = GDT_Undefined
      
      END FUNCTION GlerlDataTypeFromString
   

!--------------------------------------------------------------
!--------------------------------------------------------------
      FUNCTION GlerlDataTypeString(DT)    RESULT(GDTS)
      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: DT
      INTEGER               :: UB
      CHARACTER(LEN=15)     :: GDTS
      
      IF (DT .LE.  0) THEN
         GDTS = '---------------'
         RETURN
      END IF
      
      UB = UBOUND(GlerlDataTypeNames, 1)
      IF (DT .GT. UB) THEN
         GDTS = '---------------'
         RETURN
      END IF
      
      GDTS = GlerlDataTypeNames(DT)
      RETURN
      
      END FUNCTION GlerlDataTypeString

!--------------------------------------------------------------
!--------------------------------------------------------------
      FUNCTION GlerlDataTypeString4(DT)     RESULT(GDTS)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: DT
      INTEGER             :: UB
      CHARACTER(LEN=4)    :: GDTS
      
      UB = UBOUND(GlerlDataTypeNames4, 1)
      IF ((DT .GT. 0) .AND. (DT .LE. UB)) THEN
         GDTS = GlerlDataTypeNames4(DT)
         RETURN
      END IF
      GDTS = '----'
      
      END FUNCTION GlerlDataTypeString4

!--------------------------------------------------------------
!--------------------------------------------------------------
      FUNCTION GlerlDataTypeString10(DT)     RESULT(GDTS)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: DT
      INTEGER             :: UB
      CHARACTER(LEN=10)   :: GDTS
      
      UB = UBOUND(GlerlDataTypeNames10, 1)
      IF ((DT .GT. 0) .AND. (DT .LE. UB)) THEN
         GDTS = GlerlDataTypeNames10(DT)
         RETURN
      END IF
      GDTS = '----------'
      
      END FUNCTION GlerlDataTypeString10

!--------------------------------------------------------------
!--------------------------------------------------------------
      FUNCTION GlerlDataTypeStringLong(DT)    RESULT(GLDTS)
      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: DT
      INTEGER               :: UB
      CHARACTER(LEN=30)     :: GLDTS
      
      IF (DT .LE.  0) THEN
         GLDTS = '------------------------------'
         RETURN
      END IF
      
      UB = UBOUND(GlerlDataTypeNamesLong, 1)
      IF (DT .GT. UB) THEN
         GLDTS = '------------------------------'
         RETURN
      END IF
      
      GLDTS = GlerlDataTypeNamesLong(DT)
      RETURN
      
      END FUNCTION GlerlDataTypeStringLong

!--------------------------------------------------------------
!--------------------------------------------------------------
      INTEGER FUNCTION GlerlDataUnitFromString(DUS)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: DUS
      INTEGER :: I
      INTEGER, DIMENSION(1) :: UB
      CHARACTER(LEN=6)  :: S6
      CHARACTER(LEN=10) :: S10
      CHARACTER(LEN=18) :: S1, SS
      
      S1 = UprCase(TRIM(DUS))
      UB = UBOUND(GlerlDataUnitNames)
      DO I = 1, UB(1)
         S6  = UprCase(GlerlDataUnitNames6(I))
         S10 = UprCase(GlerlDataUnitNames10(I))
         SS  = UprCase(GlerlDataUnitNames(I))
         IF ((TRIM(S1) .EQ. TRIM(S6))  .OR.           &
             (TRIM(S1) .EQ. TRIM(S10)) .OR.           &
             (TRIM(S1) .EQ. TRIM(SS))) THEN
            GlerlDataUnitFromString = I
            RETURN
         END IF
      END DO
      GlerlDataUnitFromString = GDU_Undefined
      
      END FUNCTION GlerlDataUnitFromString
   
!--------------------------------------------------------------
!--------------------------------------------------------------
      FUNCTION GlerlDataUnitString6(DU)  RESULT(GDU)
      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: DU
      INTEGER, DIMENSION(1) :: UB
      CHARACTER(LEN=6)      :: GDU
      
      UB = UBOUND(GlerlDataUnitNames6)
      IF ((DU .GT. 0) .AND. (DU .LE. UB(1))) THEN
         GDU = GlerlDataUnitNames6(DU)
         RETURN
      END IF
      GDU = '------'
      
      END FUNCTION GlerlDataUnitString6

!--------------------------------------------------------------
!--------------------------------------------------------------
      FUNCTION GlerlDataUnitString10(DU)  RESULT(GDU)
      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: DU
      INTEGER, DIMENSION(1) :: UB
      CHARACTER(LEN=10)     :: GDU
      
      UB = UBOUND(GlerlDataUnitNames10)
      IF ((DU .GT. 0) .AND. (DU .LE. UB(1))) THEN
         GDU = GlerlDataUnitNames10(DU)
         RETURN
      END IF
      GDU = '----------'
      
      END FUNCTION GlerlDataUnitString10

!--------------------------------------------------------------
!--------------------------------------------------------------
      FUNCTION GlerlDataUnitString(DU)  RESULT(GDU)
      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: DU
      INTEGER, DIMENSION(1) :: UB
      CHARACTER(LEN=18)     :: GDU
      
      UB = UBOUND(GlerlDataUnitNames)
      IF ((DU .GT. 0) .AND. (DU .LE. UB(1))) THEN
         GDU = GlerlDataUnitNames(DU)
         RETURN
      END IF
      GDU = '------------------'
      
      END FUNCTION GlerlDataUnitString


!---------------------------------------------------------------
      FUNCTION DefaultUnitForType(DT)    RESULT(GDU)
      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: DT
      INTEGER :: GDU

      GDU = GDU_Undefined             ! default in case of bad DT
      IF (DT .EQ. GDT_AirtempMax)           GDU = GDU_Celsius
      IF (DT .EQ. GDT_AirtempMin)           GDU = GDU_Celsius
      IF (DT .EQ. GDT_AirtempMean)          GDU = GDU_Celsius
      IF (DT .EQ. GDT_Precipitation)        GDU = GDU_Millimeters
      IF (DT .EQ. GDT_DewPointMean)         GDU = GDU_Celsius
      IF (DT .EQ. GDT_WindSpeed)            GDU = GDU_MetersPerSecond
      IF (DT .EQ. GDT_CloudCover)           GDU = GDU_Percent
      IF (DT .EQ. GDT_WindDirection)        GDU = GDU_Direction360
      IF (DT .EQ. GDT_SkyCoverTotal)        GDU = GDU_Percent
      IF (DT .EQ. GDT_SkyCoverOpaque)       GDU = GDU_Percent
      IF (DT .EQ. GDT_AtmosPressure)        GDU = GDU_Millibars
      IF (DT .EQ. GDT_Streamflow)           GDU = GDU_CubicMetersPerSec
      IF (DT .EQ. GDT_Runoff)               GDU = GDU_CubicMetersPerSec
      IF (DT .EQ. GDT_UpperSoilMoisture)    GDU = GDU_Millimeters
      IF (DT .EQ. GDT_LowerSoilMoisture)    GDU = GDU_Millimeters
      IF (DT .EQ. GDT_TotalSoilMoisture)    GDU = GDU_Millimeters
      IF (DT .EQ. GDT_GroundWaterMoisture)  GDU = GDU_Millimeters
      IF (DT .EQ. GDT_SurfaceZoneMoisture)  GDU = GDU_Millimeters
      IF (DT .EQ. GDT_SnowWater)            GDU = GDU_Millimeters
      IF (DT .EQ. GDT_TotalMoisture)        GDU = GDU_Millimeters
      IF (DT .EQ. GDT_SnowDepth)            GDU = GDU_Millimeters
      IF (DT .EQ. GDT_Evaporation)          GDU = GDU_Millimeters
      IF (DT .EQ. GDT_WaterTemp)            GDU = GDU_Celsius
      IF (DT .EQ. GDT_VaporPressure)        GDU = GDU_Millibars
      IF (DT .EQ. GDT_Humidity)             GDU = GDU_Percent
      IF (DT .EQ. GDT_IncidentRad)          GDU = GDU_WattsPerM2
      IF (DT .EQ. GDT_ReflectedRad)         GDU = GDU_WattsPerM2
      IF (DT .EQ. GDT_NetLongWaveRad)       GDU = GDU_WattsPerM2
      IF (DT .EQ. GDT_LatentRad)            GDU = GDU_WattsPerM2
      IF (DT .EQ. GDT_SensibleRad)          GDU = GDU_WattsPerM2
      IF (DT .EQ. GDT_Advection)            GDU = GDU_WattsPerM2
      IF (DT .EQ. GDT_NetSurfaceRad)        GDU = GDU_WattsPerM2
      IF (DT .EQ. GDT_TotalHeat)            GDU = GDU_Calories
      IF (DT .EQ. GDT_IceTemp)              GDU = GDU_Celsius
      IF (DT .EQ. GDT_IceArea)              GDU = GDU_SquareMeters
      IF (DT .EQ. GDT_IceDepth)             GDU = GDU_Millimeters
      IF (DT .EQ. GDT_NetBasinSupply)       GDU = GDU_Millimeters
      IF (DT .EQ. GDT_MeanLakeLevel)        GDU = GDU_Meters
      IF (DT .EQ. GDT_EndLakeLevel)         GDU = GDU_Meters
      IF (DT .EQ. GDT_Inflow)               GDU = GDU_CubicMetersPerSec
      IF (DT .EQ. GDT_Outflow)              GDU = GDU_CubicMetersPerSec
      IF (DT .EQ. GDT_TotalBasinSupply)     GDU = GDU_CubicMetersPerSec

      IF (DT .EQ. GDT_OverLakeAirtempMin)   GDU = GDU_Celsius
      IF (DT .EQ. GDT_OverLakeAirtempMax)   GDU = GDU_Celsius
      IF (DT .EQ. GDT_OverLakeAirtempMean)  GDU = GDU_Celsius
      IF (DT .EQ. GDT_OverLakePrecip)       GDU = GDU_Millimeters
      IF (DT .EQ. GDT_OverLakeDewpoint)     GDU = GDU_Celsius
      IF (DT .EQ. GDT_OverLakeWindSpeed)    GDU = GDU_MetersPerSecond
      IF (DT .EQ. GDT_OverLakeCloudCover)   GDU = GDU_Percent
      IF (DT .EQ. GDT_OverLakeRunoff)       GDU = GDU_CubicMetersPerSec
      
      IF (DT .EQ. GDT_OverLandAirtempMin)   GDU = GDU_Celsius
      IF (DT .EQ. GDT_OverLandAirtempMax)   GDU = GDU_Celsius
      IF (DT .EQ. GDT_OverLandAirtempMean)  GDU = GDU_Celsius
      IF (DT .EQ. GDT_OverLandPrecip)       GDU = GDU_Millimeters
      IF (DT .EQ. GDT_OverLandDewpoint)     GDU = GDU_Celsius
      IF (DT .EQ. GDT_OverLandWindSpeed)    GDU = GDU_MetersPerSecond
      IF (DT .EQ. GDT_OverLandCloudCover)   GDU = GDU_Percent
      IF (DT .EQ. GDT_OverLandRunoff)       GDU = GDU_CubicMetersPerSec
      
      END FUNCTION DefaultUnitForType


!----------------------------------------------------------------------------------
      !----------------------------------------------------------------------------
      FUNCTION IsATemperatureUnit(UT)   Result(IsTemp)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: UT
      LOGICAL :: IsTemp
      
      IsTemp = .FALSE.
      IF (UT .EQ. GDU_Celsius)    IsTemp = .TRUE.
      IF (UT .EQ. GDU_Fahrenheit) IsTemp = .TRUE.
      
      END FUNCTION IsATemperatureUnit
      
!----------------------------------------------------------------------------------
      !----------------------------------------------------------------------------
      !  Return the conversion factor necessary to convert a value from OldUnits into NewUnits.
      !
      !  Keep in mind that the user might (accidentally) specify OldUnits=GDU_Inches
      !  and NewUnits=GDU_Celsius. This is, obviously, not a valid conversion
      !  request, and in these cases the code will return MissingData_Real
      !----------------------------------------------------------------------------
      FUNCTION UnitConversionFactor(OldUnits, NewUnits)   RESULT(CFactor)
      IMPLICIT NONE
      INTEGER, INTENT(IN)  :: OldUnits, NewUnits
      REAL :: CFactor

      !
      !  Some constants to make unit conversions easier to read (maybe?)
      !  Just trying to avoid too many numbers that could be mistyped, etc.
      !  You will see that I am inconsistent in my usage of these constants vs just
      !  typing some of the numbers. I could (should?) have defined more of
      !  these.  Yep. Just depends on what I felt like doing. Deal with it.  (LOL)
      !
      REAL, PARAMETER :: InchPerFt   = 12.0
      REAL, PARAMETER :: FtPerMile   = 5280.0
      REAL, PARAMETER :: MMPerInch   = 25.4
      REAL, PARAMETER :: CMPerInch   = 2.54
      REAL, PARAMETER :: MBarPerHPA  = 1.0
      REAL, PARAMETER :: JoulePerCal = 4.184 


      SELECT CASE (OldUnits)
         !
         !  Temperature cannot be done with a simple conversion factor.
         !  It must be done via the function UnitConvertedDataValue()
         !
         !
         !  Length
         !      
         CASE (GDU_Inches)
            IF (NewUnits .EQ. GDU_Inches)      CFactor = 1.0
            IF (NewUnits .EQ. GDU_Feet)        CFactor = InchPerFt
            IF (NewUnits .EQ. GDU_Yards)       CFactor = InchPerFt / 3.0
            IF (NewUnits .EQ. GDU_Miles)       CFactor = InchPerFt * FtPerMile
            IF (NewUnits .EQ. GDU_Millimeters) CFactor = MMPerInch
            IF (NewUnits .EQ. GDU_Centimeters) CFactor = CMPerInch
            IF (NewUnits .EQ. GDU_Meters)      CFactor = CMPerInch / 100.0
            IF (NewUnits .EQ. GDU_Kilometers)  CFactor = CMPerInch / 100000.0
         CASE (GDU_Feet)
            IF (NewUnits .EQ. GDU_Inches)      CFactor = InchPerFt
            IF (NewUnits .EQ. GDU_Feet)        CFactor = 1.0
            IF (NewUnits .EQ. GDU_Yards)       CFactor = 1.0 / 3.0
            IF (NewUnits .EQ. GDU_Miles)       CFactor = 1.0 / FtPerMile
            IF (NewUnits .EQ. GDU_Millimeters) CFactor = MMPerInch * InchPerFt
            IF (NewUnits .EQ. GDU_Centimeters) CFactor = CMPerInch * InchPerFt
            IF (NewUnits .EQ. GDU_Meters)      CFactor = (CMPerInch * InchPerFt) / 100.0
            IF (NewUnits .EQ. GDU_Kilometers)  CFactor = (CMPerInch * InchPerFt) / 100000.0
         CASE (GDU_Yards)
            IF (NewUnits .EQ. GDU_Inches)      CFactor = InchPerFt * 3
            IF (NewUnits .EQ. GDU_Feet)        CFactor = 3.0
            IF (NewUnits .EQ. GDU_Yards)       CFactor = 1.0
            IF (NewUnits .EQ. GDU_Miles)       CFactor = 1.0 / FtPerMile * 3.0
            IF (NewUnits .EQ. GDU_Millimeters) CFactor = (MMPerInch * InchPerFt) * 3
            IF (NewUnits .EQ. GDU_Centimeters) CFactor = (CMPerInch * InchPerFt) * 3
            IF (NewUnits .EQ. GDU_Meters)      CFactor = (CMPerInch * InchPerFt) / 100.0 * 3
            IF (NewUnits .EQ. GDU_Kilometers)  CFactor = (CMPerInch * InchPerFt) / 100000.0 * 3
         CASE (GDU_Miles)
            IF (NewUnits .EQ. GDU_Inches)      CFactor = InchPerFt * FtPerMile
            IF (NewUnits .EQ. GDU_Feet)        CFactor = FtPerMile
            IF (NewUnits .EQ. GDU_Yards)       CFactor = FtPerMile / 3.0
            IF (NewUnits .EQ. GDU_Miles)       CFactor = 1.0
            IF (NewUnits .EQ. GDU_Millimeters) CFactor = MMPerInch * InchPerFt * FtPerMile
            IF (NewUnits .EQ. GDU_Centimeters) CFactor = CMPerInch * InchPerFt * FtPerMile
            IF (NewUnits .EQ. GDU_Meters)      CFactor = (CMPerInch * InchPerFt * FtPerMile) / 100.0
            IF (NewUnits .EQ. GDU_Kilometers)  CFactor = (CMPerInch * InchPerFt * FtPerMile) / 100000.0
         CASE (GDU_Millimeters)
            IF (NewUnits .EQ. GDU_Inches)      CFactor = 1.0 / MMPerInch
            IF (NewUnits .EQ. GDU_Feet)        CFactor = 1.0 / MMPerInch / InchPerFt
            IF (NewUnits .EQ. GDU_Yards)       CFactor = 1.0 / MMPerInch / InchPerFt / 3.0
            IF (NewUnits .EQ. GDU_Miles)       CFactor = 1.0 / MMPerInch / InchPerFt / FtPerMile
            IF (NewUnits .EQ. GDU_Millimeters) CFactor = 1.0
            IF (NewUnits .EQ. GDU_Centimeters) CFactor = 1.0 / 10.0
            IF (NewUnits .EQ. GDU_Meters)      CFactor = 1.0 / 10.0 / 100.0
            IF (NewUnits .EQ. GDU_Kilometers)  CFactor = 1.0 / 10.0 / 100000.0
         CASE (GDU_Centimeters)
            IF (NewUnits .EQ. GDU_Inches)      CFactor = 1.0 / CMPerInch
            IF (NewUnits .EQ. GDU_Feet)        CFactor = 1.0 / CMPerInch / InchPerFt
            IF (NewUnits .EQ. GDU_Yards)       CFactor = 1.0 / CMPerInch / InchPerFt / 3.0
            IF (NewUnits .EQ. GDU_Miles)       CFactor = 1.0 / CMPerInch / InchPerFt / FtPerMile
            IF (NewUnits .EQ. GDU_Millimeters) CFactor = 10.0
            IF (NewUnits .EQ. GDU_Centimeters) CFactor = 1.0
            IF (NewUnits .EQ. GDU_Meters)      CFactor = 1.0 / 100.0
            IF (NewUnits .EQ. GDU_Kilometers)  CFactor = 1.0 / 100000.0
         CASE (GDU_Meters)
            IF (NewUnits .EQ. GDU_Inches)      CFactor = 1.0 / CMPerInch * 100.0
            IF (NewUnits .EQ. GDU_Feet)        CFactor = 1.0 / CMPerInch / InchPerFt * 100.0
            IF (NewUnits .EQ. GDU_Yards)       CFactor = 1.0 / CMPerInch / InchPerFt * 100.0 / 3.0
            IF (NewUnits .EQ. GDU_Miles)       CFactor = 1.0 / CMPerInch / InchPerFt / FtPerMile * 100.0
            IF (NewUnits .EQ. GDU_Millimeters) CFactor = 1000.0
            IF (NewUnits .EQ. GDU_Centimeters) CFactor = 100.0
            IF (NewUnits .EQ. GDU_Meters)      CFactor = 1.0
            IF (NewUnits .EQ. GDU_Kilometers)  CFactor = 1.0 / 1000.0
         CASE (GDU_Kilometers)
            IF (NewUnits .EQ. GDU_Inches)      CFactor = 1.0 / CMPerInch * 100000.0
            IF (NewUnits .EQ. GDU_Feet)        CFactor = 1.0 / CMPerInch / InchPerFt * 100000.0
            IF (NewUnits .EQ. GDU_Yards)       CFactor = 1.0 / CMPerInch / InchPerFt * 100000.0 / 3.0
            IF (NewUnits .EQ. GDU_Miles)       CFactor = 1.0 / CMPerInch / InchPerFt / FtPerMile * 100000.0
            IF (NewUnits .EQ. GDU_Millimeters) CFactor = 1000000.0
            IF (NewUnits .EQ. GDU_Centimeters) CFactor = 100000.0
            IF (NewUnits .EQ. GDU_Meters)      CFactor = 1000.0
            IF (NewUnits .EQ. GDU_Kilometers)  CFactor = 1.0

         !         
         !  Speed
         !    I decided not to use constants here cuz it seemed messier, somehow.
         !    Purely a stylistic choice and I'm probably being hopelessly arbitrary.
         !
         CASE (GDU_MilesPerHour)
            IF (NewUnits .EQ. GDU_MilesPerHour)      CFactor = 1.0
            IF (NewUnits .EQ. GDU_FeetPerSecond)     CFactor = 1.46667
            IF (NewUnits .EQ. GDU_Knots)             CFactor = 0.868976
            IF (NewUnits .EQ. GDU_KilometersPerHour) CFactor = 1.60934
            IF (NewUnits .EQ. GDU_MetersPerSecond)   CFactor = 0.44704
         CASE (GDU_FeetPerSecond)
            IF (NewUnits .EQ. GDU_MilesPerHour)      CFactor = 1.0 / 1.46667
            IF (NewUnits .EQ. GDU_FeetPerSecond)     CFactor = 1.0
            IF (NewUnits .EQ. GDU_Knots)             CFactor = 0.59248
            IF (NewUnits .EQ. GDU_KilometersPerHour) CFactor = 1.09728
            IF (NewUnits .EQ. GDU_MetersPerSecond)   CFactor = 0.3048
         CASE (GDU_Knots)
            IF (NewUnits .EQ. GDU_MilesPerHour)      CFactor = 1.15078
            IF (NewUnits .EQ. GDU_FeetPerSecond)     CFactor = 1.68781
            IF (NewUnits .EQ. GDU_Knots)             CFactor = 1.0
            IF (NewUnits .EQ. GDU_KilometersPerHour) CFactor = 1.852
            IF (NewUnits .EQ. GDU_MetersPerSecond)   CFactor = 0.514444
         CASE (GDU_KilometersPerHour)
            IF (NewUnits .EQ. GDU_MilesPerHour)      CFactor = 0.621371
            IF (NewUnits .EQ. GDU_FeetPerSecond)     CFactor = 0.911344
            IF (NewUnits .EQ. GDU_Knots)             CFactor = 0.539957
            IF (NewUnits .EQ. GDU_KilometersPerHour) CFactor = 1.0
            IF (NewUnits .EQ. GDU_MetersPerSecond)   CFactor = 0.277778
         CASE (GDU_MetersPerSecond)
            IF (NewUnits .EQ. GDU_MilesPerHour)      CFactor = 2.23694
            IF (NewUnits .EQ. GDU_FeetPerSecond)     CFactor = 3.28084
            IF (NewUnits .EQ. GDU_Knots)             CFactor = 1.94384
            IF (NewUnits .EQ. GDU_KilometersPerHour) CFactor = 3.6
            IF (NewUnits .EQ. GDU_MetersPerSecond)   CFactor = 1.0

         !
         !  Coverage of area
         !
         CASE (GDU_Tenths)
            IF (NewUnits .EQ. GDU_Tenths)   CFactor = 1.0
            IF (NewUnits .EQ. GDU_Percent)  CFactor = 10.0
            IF (NewUnits .EQ. GDU_Fraction) CFactor = 0.1
         CASE (GDU_Percent)
            IF (NewUnits .EQ. GDU_Tenths)   CFactor = 0.1
            IF (NewUnits .EQ. GDU_Percent)  CFactor = 1.0
            IF (NewUnits .EQ. GDU_Fraction) CFactor = 0.01
         CASE (GDU_Fraction)
            IF (NewUnits .EQ. GDU_Tenths)   CFactor = 10.0
            IF (NewUnits .EQ. GDU_Percent)  CFactor = 100.0

         !
         !  Compass direction (0-360)
         !
         CASE (GDU_Direction360)
            IF (NewUnits .EQ. GDU_Direction360)  CFactor = 1.0
         
         !
         !  Atmospheric Pressure
         !
         CASE (GDU_HectoPascals)
            IF (NewUnits .EQ. GDU_HectoPascals)  CFactor = 1.0
            IF (NewUnits .EQ. GDU_Millibars)     CFactor = MBarPerHPA
         CASE (GDU_Millibars)
            IF (NewUnits .EQ. GDU_HectoPascals)  CFactor = 1.0 / MBarPerHPA
            IF (NewUnits .EQ. GDU_Millibars)     CFactor = 1.0

         !
         !  Solar Radiation
         !    - not really convertible to anything else
         !
         CASE (GDU_WattsPerM2)
            IF (NewUnits .EQ. GDU_WattsPerM2)    CFactor = 1.0
            
         !
         !  Energy  (Note that Calories is "gram calories", not "kilocalories")
         !
         CASE (GDU_Calories)
            IF (NewUnits .EQ. GDU_Calories)      CFactor = 1.0
            IF (NewUnits .EQ. GDU_Joules)        CFactor = JoulePerCal
         CASE (GDU_Joules)
            IF (NewUnits .EQ. GDU_Calories)      CFactor = 1.0 / JoulePerCal
            IF (NewUnits .EQ. GDU_Joules)        CFactor = 1.0
            
         !
         !  Energy per unit area
         !
         CASE (GDU_JoulesPerM2)
            IF (NewUnits .EQ. GDU_JoulesPerM2)   CFactor = 1.0
            IF (NewUnits .EQ. GDU_Langleys)      CFactor = 1.0 / 41840.0
         CASE (GDU_Langleys)
            IF (NewUnits .EQ. GDU_JoulesPerM2)   CFactor = 41840.0
            IF (NewUnits .EQ. GDU_Langleys)      CFactor = 1.0

         !
         !  Area
         !
         CASE (GDU_SquareMillimeters)
            IF (NewUnits .EQ. GDU_SquareMillimeters)  CFactor = 1.0
            IF (NewUnits .EQ. GDU_SquareCentimeters)  CFactor = 1.0e-2
            IF (NewUnits .EQ. GDU_SquareMeters)       CFactor = 1.0e-6
            IF (NewUnits .EQ. GDU_SquareKilometers)   CFactor = 1.0e-12
            IF (NewUnits .EQ. GDU_Hectares)           CFactor = 1.0e-10
            IF (NewUnits .EQ. GDU_SquareInches)       CFactor = 1.5500e-3 
            IF (NewUnits .EQ. GDU_SquareFeet)         CFactor = 1.0764e-5
            IF (NewUnits .EQ. GDU_SquareYards)        CFactor = 1.1960e-6
            IF (NewUnits .EQ. GDU_SquareMiles)        CFactor = 3.8610e-13
            IF (NewUnits .EQ. GDU_Acres)              CFactor = 2.4711e-10
         CASE (GDU_SquareCentimeters)
            IF (NewUnits .EQ. GDU_SquareMillimeters)  CFactor = 100
            IF (NewUnits .EQ. GDU_SquareCentimeters)  CFactor = 1.0
            IF (NewUnits .EQ. GDU_SquareMeters)       CFactor = 1.0e-4
            IF (NewUnits .EQ. GDU_SquareKilometers)   CFactor = 1.0e-10
            IF (NewUnits .EQ. GDU_Hectares)           CFactor = 1.0e-8
            IF (NewUnits .EQ. GDU_SquareInches)       CFactor = 1.5500e-1
            IF (NewUnits .EQ. GDU_SquareFeet)         CFactor = 1.0764e-3
            IF (NewUnits .EQ. GDU_SquareYards)        CFactor = 1.1960e-4
            IF (NewUnits .EQ. GDU_SquareMiles)        CFactor = 3.8610e-11
            IF (NewUnits .EQ. GDU_Acres)              CFactor = 2.4711e-8
         CASE (GDU_SquareMeters)
            IF (NewUnits .EQ. GDU_SquareMillimeters)  CFactor = 1.0e6
            IF (NewUnits .EQ. GDU_SquareCentimeters)  CFactor = 1.0e4
            IF (NewUnits .EQ. GDU_SquareMeters)       CFactor = 1.0
            IF (NewUnits .EQ. GDU_SquareKilometers)   CFactor = 1.0e-6
            IF (NewUnits .EQ. GDU_Hectares)           CFactor = 1.0e-4
            IF (NewUnits .EQ. GDU_SquareInches)       CFactor = 155
            IF (NewUnits .EQ. GDU_SquareFeet)         CFactor = 10.764
            IF (NewUnits .EQ. GDU_SquareYards)        CFactor = 1.1960
            IF (NewUnits .EQ. GDU_SquareMiles)        CFactor = 3.8610e-7
            IF (NewUnits .EQ. GDU_Acres)              CFactor = 2.4711e-4
         CASE (GDU_SquareKilometers)
            IF (NewUnits .EQ. GDU_SquareMillimeters)  CFactor = 1.0e12
            IF (NewUnits .EQ. GDU_SquareCentimeters)  CFactor = 1.0e10
            IF (NewUnits .EQ. GDU_SquareMeters)       CFactor = 1.0e6
            IF (NewUnits .EQ. GDU_SquareKilometers)   CFactor = 1.0
            IF (NewUnits .EQ. GDU_Hectares)           CFactor = 100
            IF (NewUnits .EQ. GDU_SquareInches)       CFactor = 1.5500e9
            IF (NewUnits .EQ. GDU_SquareFeet)         CFactor = 1.0764e7
            IF (NewUnits .EQ. GDU_SquareYards)        CFactor = 1.1960e6
            IF (NewUnits .EQ. GDU_SquareMiles)        CFactor = 0.3861
            IF (NewUnits .EQ. GDU_Acres)              CFactor = 247.105
         CASE (GDU_Hectares)
            IF (NewUnits .EQ. GDU_SquareMillimeters)  CFactor = 1.0e10
            IF (NewUnits .EQ. GDU_SquareCentimeters)  CFactor = 1.0e8
            IF (NewUnits .EQ. GDU_SquareMeters)       CFactor = 1.0e4
            IF (NewUnits .EQ. GDU_SquareKilometers)   CFactor = 0.01
            IF (NewUnits .EQ. GDU_Hectares)           CFactor = 1.0
            IF (NewUnits .EQ. GDU_SquareInches)       CFactor = 1.5500e7
            IF (NewUnits .EQ. GDU_SquareFeet)         CFactor = 1.0764e5
            IF (NewUnits .EQ. GDU_SquareYards)        CFactor = 1.1960e4
            IF (NewUnits .EQ. GDU_SquareMiles)        CFactor = 3.8610e-3
            IF (NewUnits .EQ. GDU_Acres)              CFactor = 2.47105
         CASE (GDU_SquareInches)
            IF (NewUnits .EQ. GDU_SquareMillimeters)  CFactor = 645.16
            IF (NewUnits .EQ. GDU_SquareCentimeters)  CFactor = 6.4516
            IF (NewUnits .EQ. GDU_SquareMeters)       CFactor = 6.4516e-4
            IF (NewUnits .EQ. GDU_SquareKilometers)   CFactor = 6.4516e-10
            IF (NewUnits .EQ. GDU_Hectares)           CFactor = 6.4518e-8
            IF (NewUnits .EQ. GDU_SquareInches)       CFactor = 1.0
            IF (NewUnits .EQ. GDU_SquareFeet)         CFactor = 6.94444e-3
            IF (NewUnits .EQ. GDU_SquareYards)        CFactor = 7.71605e-4
            IF (NewUnits .EQ. GDU_SquareMiles)        CFactor = 2.491e-10
            IF (NewUnits .EQ. GDU_Acres)              CFactor = 1.5942e-7
         CASE (GDU_SquareFeet)
            IF (NewUnits .EQ. GDU_SquareMillimeters)  CFactor = 9.2903e4
            IF (NewUnits .EQ. GDU_SquareCentimeters)  CFactor = 9.2903e2
            IF (NewUnits .EQ. GDU_SquareMeters)       CFactor = 9.2903e-2
            IF (NewUnits .EQ. GDU_SquareKilometers)   CFactor = 9.2903e-8
            IF (NewUnits .EQ. GDU_Hectares)           CFactor = 9.2903e-6
            IF (NewUnits .EQ. GDU_SquareInches)       CFactor = 144.0
            IF (NewUnits .EQ. GDU_SquareFeet)         CFactor = 1.0
            IF (NewUnits .EQ. GDU_SquareYards)        CFactor = 0.1111
            IF (NewUnits .EQ. GDU_SquareMiles)        CFactor = 3.5870e-8
            IF (NewUnits .EQ. GDU_Acres)              CFactor = 2.2957e-5
         CASE (GDU_SquareYards)
            IF (NewUnits .EQ. GDU_SquareMillimeters)  CFactor = 8.36127e5
            IF (NewUnits .EQ. GDU_SquareCentimeters)  CFactor = 8.36127e3
            IF (NewUnits .EQ. GDU_SquareMeters)       CFactor = 8.36127e-1
            IF (NewUnits .EQ. GDU_SquareKilometers)   CFactor = 8.36127e-7
            IF (NewUnits .EQ. GDU_Hectares)           CFactor = 8.36127e-5
            IF (NewUnits .EQ. GDU_SquareInches)       CFactor = 1296.0
            IF (NewUnits .EQ. GDU_SquareFeet)         CFactor = 9.0
            IF (NewUnits .EQ. GDU_SquareYards)        CFactor = 1.0
            IF (NewUnits .EQ. GDU_SquareMiles)        CFactor = 3.2283e-7
            IF (NewUnits .EQ. GDU_Acres)              CFactor = 2.0661e-4
         CASE (GDU_SquareMiles)
            IF (NewUnits .EQ. GDU_SquareMillimeters)  CFactor = 2.59e12
            IF (NewUnits .EQ. GDU_SquareCentimeters)  CFactor = 2.59e10
            IF (NewUnits .EQ. GDU_SquareMeters)       CFactor = 2.59e6
            IF (NewUnits .EQ. GDU_SquareKilometers)   CFactor = 2.59
            IF (NewUnits .EQ. GDU_Hectares)           CFactor = 2.59e2
            IF (NewUnits .EQ. GDU_SquareInches)       CFactor = 4.014e9
            IF (NewUnits .EQ. GDU_SquareFeet)         CFactor = 2.788e7
            IF (NewUnits .EQ. GDU_SquareYards)        CFactor = 3.098e6
            IF (NewUnits .EQ. GDU_SquareMiles)        CFactor = 1.0 
            IF (NewUnits .EQ. GDU_Acres)              CFactor = 640.0
         CASE (GDU_Acres)
            IF (NewUnits .EQ. GDU_SquareMillimeters)  CFactor = 4.04686e9
            IF (NewUnits .EQ. GDU_SquareCentimeters)  CFactor = 4.04686e7
            IF (NewUnits .EQ. GDU_SquareMeters)       CFactor = 4.04686e3
            IF (NewUnits .EQ. GDU_SquareKilometers)   CFactor = 4.04686e-3
            IF (NewUnits .EQ. GDU_Hectares)           CFactor = 4.04686e-1
            IF (NewUnits .EQ. GDU_SquareInches)       CFactor = 6.273e6
            IF (NewUnits .EQ. GDU_SquareFeet)         CFactor = 4.3560e4
            IF (NewUnits .EQ. GDU_SquareYards)        CFactor = 4.840e3
            IF (NewUnits .EQ. GDU_SquareMiles)        CFactor = 1.5625e-3
            IF (NewUnits .EQ. GDU_Acres)              CFactor = 1.0
            
         !
         !   Volume
         !
         CASE (GDU_CubicMillimeters)
            IF (NewUnits .EQ. GDU_CubicMillimeters)  CFactor = 1.0
            IF (NewUnits .EQ. GDU_CubicCentimeters)  CFactor = 1.0e-3
            IF (NewUnits .EQ. GDU_CubicMeters)       CFactor = 1.0e-9
            IF (NewUnits .EQ. GDU_CubicInches)       CFactor = 6.10237e-5
            IF (NewUnits .EQ. GDU_CubicFeet)         CFactor = 3.53150e-8
            IF (NewUnits .EQ. GDU_CubicYards)        CFactor = 1.30795e-9
         CASE (GDU_CubicCentimeters)
            IF (NewUnits .EQ. GDU_CubicMillimeters)  CFactor = 1.0e3
            IF (NewUnits .EQ. GDU_CubicCentimeters)  CFactor = 1.0
            IF (NewUnits .EQ. GDU_CubicMeters)       CFactor = 1.0e-6
            IF (NewUnits .EQ. GDU_CubicInches)       CFactor = 6.10237e-2
            IF (NewUnits .EQ. GDU_CubicFeet)         CFactor = 3.53150e-5
            IF (NewUnits .EQ. GDU_CubicYards)        CFactor = 1.30795e-6
         CASE (GDU_CubicMeters)
            IF (NewUnits .EQ. GDU_CubicMillimeters)  CFactor = 1.0e9
            IF (NewUnits .EQ. GDU_CubicCentimeters)  CFactor = 1.0e6
            IF (NewUnits .EQ. GDU_CubicMeters)       CFactor = 1.0
            IF (NewUnits .EQ. GDU_CubicInches)       CFactor = 6.10237e4
            IF (NewUnits .EQ. GDU_CubicFeet)         CFactor = 3.53150e1
            IF (NewUnits .EQ. GDU_CubicYards)        CFactor = 1.30795
         CASE (GDU_CubicInches)
            IF (NewUnits .EQ. GDU_CubicMillimeters)  CFactor = 1.63871e4
            IF (NewUnits .EQ. GDU_CubicCentimeters)  CFactor = 1.63871e1
            IF (NewUnits .EQ. GDU_CubicMeters)       CFactor = 1.6387e-5
            IF (NewUnits .EQ. GDU_CubicInches)       CFactor = 1.0
            IF (NewUnits .EQ. GDU_CubicFeet)         CFactor = 5.78704e-4
            IF (NewUnits .EQ. GDU_CubicYards)        CFactor = 2.14335e-5
         CASE (GDU_CubicFeet)
            IF (NewUnits .EQ. GDU_CubicMillimeters)  CFactor = 2.83168e7
            IF (NewUnits .EQ. GDU_CubicCentimeters)  CFactor = 2.83168e4
            IF (NewUnits .EQ. GDU_CubicMeters)       CFactor = 2.83168e-2
            IF (NewUnits .EQ. GDU_CubicInches)       CFactor = 1.728e3
            IF (NewUnits .EQ. GDU_CubicFeet)         CFactor = 1.0
            IF (NewUnits .EQ. GDU_CubicYards)        CFactor = 3.7037e-2
         CASE (GDU_CubicYards)
            IF (NewUnits .EQ. GDU_CubicMillimeters)  CFactor = 7.64555e8
            IF (NewUnits .EQ. GDU_CubicCentimeters)  CFactor = 7.64555e5
            IF (NewUnits .EQ. GDU_CubicMeters)       CFactor = 7.64555e-1
            IF (NewUnits .EQ. GDU_CubicInches)       CFactor = 4.6656e4
            IF (NewUnits .EQ. GDU_CubicFeet)         CFactor = 27.0
            IF (NewUnits .EQ. GDU_CubicYards)        CFactor = 1.0

         !
         !   Volumetric rate
         !
         CASE (GDU_CubicMetersPerSec)
            IF (NewUnits .EQ. GDU_CubicMetersPerSec)  CFactor = 1.0
            IF (NewUnits .EQ. GDU_CubicFeetPerSec)    CFactor = 3.53150e1
         CASE (GDU_CubicFeetPerSec)
            IF (NewUnits .EQ. GDU_CubicMetersPerSec)  CFactor = 2.83168e-2
            IF (NewUnits .EQ. GDU_CubicFeetPerSec)    CFactor = 1.0

            
         !
         !  Anything else as the OldUnits is an invalid value. Should already have 
         !  been caught, but including this for completeness.
         !
         CASE DEFAULT   
            CFactor = MissingData_Real
      END SELECT
      
      END FUNCTION UnitConversionFactor

      
!-------------------------------------------------------------
!--------------------------------------------------------------
      FUNCTION UnitConvertedDataValue(OldVal, OldUnits, NewUnits)   RESULT(NewVal)
      IMPLICIT NONE
      REAL,    INTENT(IN)  :: OldVal
      INTEGER, INTENT(IN)  :: OldUnits, NewUnits
      INTEGER :: UB
      REAL    :: NewVal, CFactor

      !
      !  Some constants to make unit conversions easier to read (maybe?)
      !  Just trying to avoid too many numbers that could be mistyped, etc.
      !  You will see that I am inconsistent in my usage of these constants vs just
      !  typing some of the numbers. I could (should?) have defined more of
      !  these.  Yep. Just depends on what I felt like doing. Deal with it.  (LOL)
      !
      REAL, PARAMETER :: InchPerFt   = 12.0
      REAL, PARAMETER :: FtPerMile   = 5280.0
      REAL, PARAMETER :: MMPerInch   = 25.4
      REAL, PARAMETER :: CMPerInch   = 2.54
      REAL, PARAMETER :: MBarPerHPA  = 1.0
      REAL, PARAMETER :: JoulePerCal = 4.184 

      !
      ! default result in case of mismatches, etc
      !
      NewVal = MissingData_Real
      
      !
      !  If OldVal is missing data indicator, no need to do anything else
      !
      IF (IsMissing(OldVal)) RETURN
      
      !
      !  First verify that the unit specifiers are in valid range
      !
      UB = UBOUND(GlerlDataUnitNames6, 1)
      IF ((OldUnits .LE. 1) .OR. (OldUnits .GT. UB)) RETURN
      IF ((NewUnits .LE. 1) .OR. (NewUnits .GT. UB)) RETURN

      !
      !  Now do the correct conversion operation.
      !  Keep in mind that the user might (accidentally) specify an invalid set
      !  of units, e.g. OldUnits=GDU_Inches and NewUnits=GDU_Celsius. 
      !  In these cases the result will be MissingData_Real.
      !
      SELECT CASE (OldUnits)
         !
         !  Temperature has to be done here, since it is not a simple multiplicative
         !  conversion.  
         !
         CASE (GDU_Fahrenheit)
            IF (NewUnits .EQ. GDU_Fahrenheit) NewVal = OldVal 
            IF (NewUnits .EQ. GDU_Celsius)    NewVal = (OldVal - 32.0) * (5.0/9.0)
         CASE (GDU_Celsius)
            IF (NewUnits .EQ. GDU_Fahrenheit) NewVal = OldVal * (9.0/5.0) + 32.0
            IF (NewUnits .EQ. GDU_Celsius)    NewVal = OldVal

         !
         !  In the case of everything not explicitly handled, we assume a simple
         !  multiplicative conversion factor.  Call the functin to get that factor, 
         !  then apply it here.
         !      
         CASE DEFAULT   
            CFactor = UnitConversionFactor(OldUnits, NewUnits)
            IF (IsMissing(CFactor)) THEN
               NewVal = MissingData_Real
            ELSE
               NewVal = OldVal * CFactor
            END IF
      END SELECT
         
      END FUNCTION UnitConvertedDataValue
   
!-------------------------------------------------------------
!  A private local function to convert to all uppercase.
!-------------------------------------------------------------
      FUNCTION UprCase(S)  Result(US)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN)  :: S
      CHARACTER (LEN=:), ALLOCATABLE :: US
      INTEGER   :: I, J
      INTEGER, PARAMETER :: Diff = 32   ! difference from 'A' to 'a' in ASCII code
      CHARACTER (LEN=:), ALLOCATABLE :: Temp

      J = LEN_TRIM(S)
      ALLOCATE(CHARACTER(J) :: Temp)
      DO I=1,J
         IF ( (S(I:I) .GE. 'a') .AND. (S(I:I) .LE. 'z') ) THEN
            Temp(I:I) = CHAR(ICHAR(S(I:I)) - Diff)
         ELSE
            Temp(I:I) = S(I:I)
         ENDIF
      END DO
      US = TRIM(Temp)
      DEALLOCATE(Temp, STAT=I)
      RETURN
      END FUNCTION UprCase

!-------------------------------------------------------------
!  A private local function to test for missing data
!-------------------------------------------------------------
      FUNCTION IsMissing(R)  Result(IM)
      IMPLICIT   NONE
      REAL, INTENT(IN) :: R
      LOGICAL :: IM
      IM = .FALSE.
      IF (R .LT. MissingData_Test) IM = .TRUE.      
      RETURN
      END FUNCTION IsMissing

      
      
      
END MODULE GlerlDataTypesAndUnits