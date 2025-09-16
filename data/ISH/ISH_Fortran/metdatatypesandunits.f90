
MODULE MetDataTypesAndUnits

   !
   !  Met Data Types
   !
   INTEGER, PARAMETER :: MDT_Undefined      = 1
   INTEGER, PARAMETER :: MDT_AirtempMax     = 2
   INTEGER, PARAMETER :: MDT_AirtempMin     = 3
   INTEGER, PARAMETER :: MDT_Precipitation  = 4
   INTEGER, PARAMETER :: MDT_AirtempMean    = 5
   INTEGER, PARAMETER :: MDT_DewpointMean   = 6
   INTEGER, PARAMETER :: MDT_WindSpeed      = 7
   INTEGER, PARAMETER :: MDT_CloudCover     = 8
   INTEGER, PARAMETER :: MDT_WindDirection  = 9
   INTEGER, PARAMETER :: MDT_SkyCoverTotal  = 10
   INTEGER, PARAMETER :: MDT_SkyCoverOpaque = 11

   !
   !  Met Data Units
   !
   INTEGER, PARAMETER :: MDU_Undefined         = 1
   INTEGER, PARAMETER :: MDU_Fahrenheit        = 2
   INTEGER, PARAMETER :: MDU_Celsius           = 3
   INTEGER, PARAMETER :: MDU_Inches            = 4
   INTEGER, PARAMETER :: MDU_Feet              = 5
   INTEGER, PARAMETER :: MDU_Miles             = 6
   INTEGER, PARAMETER :: MDU_Millimeters       = 7
   INTEGER, PARAMETER :: MDU_Centimeters       = 8
   INTEGER, PARAMETER :: MDU_Meters            = 9
   INTEGER, PARAMETER :: MDU_Kilometers        = 10
   INTEGER, PARAMETER :: MDU_MilesPerHour      = 11
   INTEGER, PARAMETER :: MDU_Knots             = 12
   INTEGER, PARAMETER :: MDU_KilometersPerHour = 13
   INTEGER, PARAMETER :: MDU_MetersPerSecond   = 14
   INTEGER, PARAMETER :: MDU_Tenths            = 15
   INTEGER, PARAMETER :: MDU_Percent           = 16
   INTEGER, PARAMETER :: MDU_Fraction          = 17
   INTEGER, PARAMETER :: MDU_Direction360      = 18

   !
   !  Met Data Precision
   !  Typical (recommended) precision to use when writing output values.
   !  This number tells how many digits after the decimal point to use.
   !  Each entry corresponds to the Met Data Unit matching the index.
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
   !  I have broken it into 10 values per line strictly for readability.
   !
   INTEGER, DIMENSION(18) :: MetDataUnitOutputPrecision =    &
            (/1, 1, 1, 2, 3, 3, 1, 2, 3, 4,                  &
              4, 3, 4, 2, 1, 1, 1, 1/)
   
   !   
   !   
   CHARACTER(LEN=15), DIMENSION(11) :: MetDataTypeNames =            &
      (/'Undefined      ', 'AirTempMax     ', 'AirTempMin     ',     &
        'Precipitation  ', 'AirTemp        ', 'Dewpoint       ',     &
        'Windspeed      ', 'CloudCover     ', 'WindDirection  ',     &
        'SkyCoverTotal  ', 'SkyCoverOpaque '/)
        
   CHARACTER(LEN=4), DIMENSION(11) :: MetDataTypeNames4 =            &
      (/'UNDF', 'TMAX', 'TMIN', 'PREC', 'AIRT', 'DEWP',              &
        'WSPD', 'CLDC', 'WDIR', 'SKYT', 'SKYO'/)
        
   CHARACTER(LEN=15), DIMENSION(18) :: MetDataUnitNames =            &
      (/'Undefined      ', 'Fahrenheit     ', 'Celsius        ',     &
        'Inches         ', 'Feet           ', 'Miles          ',     &
        'Millimeters    ', 'Centimeters    ', 'Meters         ',     &
        'Kilometers     ', 'MilesPerHour   ', 'Knots          ',     &
        'KilometersPerHr', 'MetersPerSecond', 'Tenths         ',     &
        'Percent        ', 'Fraction       ', 'Direction360   '/)
   CHARACTER(LEN=6), DIMENSION(18) :: MetDataUnitNames6 =            &
      (/'UNDEF ', 'DEG F ', 'DEG C ', 'INCHES', 'FEET  ', 'MILES ',  &
        'MM    ', 'CM    ', 'METERS', 'KM    ', 'MPH   ', 'KNOTS ',  &
        'KM/HR ', 'M/S   ', 'TENTHS', '%     ', 'FRACT ', 'DIR360'/)


   PRIVATE :: MetDataTypeNames, MetDataTypeNames4
   PRIVATE :: MetDataUnitNames, MetDataUnitNames6
        
   !
   !  These are some things that would normally be found in the GlerlUtil
   !  module, but I want this module to be independent from that one.
   !  So I am defining local private versions that have the same value or
   !  functionality (therefore compatible) without dependency.
   !
   REAL, PRIVATE, PARAMETER :: MissingData_Real = -9.9e29
   PRIVATE :: UprCase
        
CONTAINS

   !-------------------------------------------------------------------------
   INTEGER FUNCTION MetDataTypeFromString(DTS)
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN)  :: DTS
   INTEGER :: I
   INTEGER, DIMENSION(1) :: UB
   CHARACTER(LEN=4)  :: S4
   CHARACTER(LEN=15) :: S1, S2
   
   S1 = TRIM(DTS)
   CALL UprCase(S1)
   UB = UBOUND(MetDataTypeNames)
   DO I = 1, UB(1)
      S2 = MetDataTypeNames(I)
      S4 = MetDataTypeNames4(I)
      CALL UprCase(S2)
      CALL UprCase(S4)
      IF ((TRIM(S1) .EQ. TRIM(S2)) .OR.          &
          (TRIM(S1) .EQ. TRIM(S4))) THEN
         MetDataTypeFromString = I
         RETURN
      END IF
   END DO
   MetDataTypeFromString = MDT_Undefined
   
   END FUNCTION MetDataTypeFromString
   
   !--------------------------------------------------------------
   CHARACTER(LEN=15) FUNCTION MetDataTypeString(DT)
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: DT
   INTEGER, DIMENSION(1) :: UB
   
   UB = UBOUND(MetDataTypeNames)
   IF ((DT .GT. 0) .AND. (DT .LE. UB(1))) THEN
      MetDataTypeString = MetDataTypeNames(DT)
      RETURN
   END IF
   MetDataTypeString = '---------------'
   
   END FUNCTION MetDataTypeString

   !--------------------------------------------------------------
   CHARACTER(LEN=4) FUNCTION MetDataTypeString4(DT)
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: DT
   INTEGER, DIMENSION(1) :: UB
   
   UB = UBOUND(MetDataTypeNames4)
   IF ((DT .GT. 0) .AND. (DT .LE. UB(1))) THEN
      MetDataTypeString4 = MetDataTypeNames4(DT)
      RETURN
   END IF
   MetDataTypeString4 = '----'
   
   END FUNCTION MetDataTypeString4

   !--------------------------------------------------------------
   INTEGER FUNCTION MetDataUnitFromString(DUS)
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN)  :: DUS
   INTEGER :: I
   INTEGER, DIMENSION(1) :: UB
   CHARACTER(LEN=6)  :: S6
   CHARACTER(LEN=15) :: S1, S2
   
   S1 = TRIM(DUS)
   CALL UprCase(S1)
   UB = UBOUND(MetDataUnitNames)
   DO I = 1, UB(1)
      S2 = MetDataUnitNames(I)
      S6 = MetDataUnitNames6(I)
      CALL UprCase(S2)
      CALL UprCase(S6)
      IF ((TRIM(S1) .EQ. TRIM(S2)) .OR.           &
          (TRIM(S1) .EQ. TRIM(S6))) THEN
         MetDataUnitFromString = I
         RETURN
      END IF
   END DO
   MetDataUnitFromString = MDU_Undefined
   
   END FUNCTION MetDataUnitFromString
   
   !--------------------------------------------------------------
   CHARACTER(LEN=15) FUNCTION MetDataUnitString(DU)
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: DU
   INTEGER, DIMENSION(1) :: UB
   
   UB = UBOUND(MetDataUnitNames)
   IF ((DU .GT. 0) .AND. (DU .LE. UB(1))) THEN
      MetDataUnitString = MetDataUnitNames(DU)
      RETURN
   END IF
   MetDataUnitString = '---------------'
   
   END FUNCTION MetDataUnitString

   !--------------------------------------------------------------
   CHARACTER(LEN=6) FUNCTION MetDataUnitString6(DU)
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: DU
   INTEGER, DIMENSION(1) :: UB
   
   UB = UBOUND(MetDataUnitNames6)
   IF ((DU .GT. 0) .AND. (DU .LE. UB(1))) THEN
      MetDataUnitString6 = MetDataUnitNames6(DU)
      RETURN
   END IF
   MetDataUnitString6 = '------'
   
   END FUNCTION MetDataUnitString6


   !-------------------------------------------------------------
   REAL FUNCTION UnitConvertedDataValue(OldVal, OldUnits, NewUnits)
   IMPLICIT NONE
   REAL,    INTENT(IN)  :: OldVal
   INTEGER, INTENT(IN)  :: OldUnits, NewUnits

   REAL :: NewVal
   INTEGER, DIMENSION(1) :: UB

   !
   !  Some constants to make unit conversions easier to read (maybe?)
   !  Just trying to avoid too many numbers that could be mistyped, etc.
   !
   REAL, PARAMETER :: InchPerFt = 12.0
   REAL, PARAMETER :: FtPerMile = 5280.0
   REAL, PARAMETER :: MMPerInch = 25.4
   REAL, PARAMETER :: CMPerInch = 2.54

   !
   !  First verify that the unit specifiers are in valid range
   !
   UB = UBOUND(MetDataUnitNames6)
   IF ((OldUnits .LE. 1) .OR. (OldUnits .GT. UB(1))) THEN
      UnitConvertedDataValue = MissingData_Real
      RETURN
   END IF
   IF ((NewUnits .LE. 1) .OR. (NewUnits .GT. UB(1))) THEN
      UnitConvertedDataValue = MissingData_Real
      RETURN
   END IF

   !
   !  Now we know that both unit specifiers are valid, but they may not be
   !  matched. e.g. The user might (accidentally) specify OldUnits=MDU_Inches
   !  and NewUnits=MDU_Celsius. This is, obviously, not a valid conversion
   !  request, so I will set a default return value of "missing" that will apply
   !  in cases such as this.
   !
   NewVal = MissingData_Real     ! default result in case of mismatches, etc

   !
   !  Now do the correct conversion operation
   !
   SELECT CASE (OldUnits)
      !
      !  Temperature
      !
      CASE (MDU_Fahrenheit)
         IF (NewUnits .EQ. MDU_Fahrenheit) NewVal = OldVal 
         IF (NewUnits .EQ. MDU_Celsius)    NewVal = (OldVal - 32.0) * (5.0/9.0)
      CASE (MDU_Celsius)
         IF (NewUnits .EQ. MDU_Fahrenheit) NewVal = OldVal * (9.0/5.0) + 32.0
         IF (NewUnits .EQ. MDU_Celsius)    NewVal = OldVal

      !
      !  Length
      !      
      CASE (MDU_Inches)
         IF (NewUnits .EQ. MDU_Inches)      NewVal = OldVal
         IF (NewUnits .EQ. MDU_Feet)        NewVal = OldVal / InchPerFt
         IF (NewUnits .EQ. MDU_Miles)       NewVal = OldVal / (InchPerFt * FtPerMile)
         IF (NewUnits .EQ. MDU_Millimeters) NewVal = OldVal * MMPerInch
         IF (NewUnits .EQ. MDU_Centimeters) NewVal = OldVal * CMPerInch
         IF (NewUnits .EQ. MDU_Meters)      NewVal = OldVal * CMPerInch / 100.0
         IF (NewUnits .EQ. MDU_Kilometers)  NewVal = OldVal * CMPerInch / 100000.0
      CASE (MDU_Feet)
         IF (NewUnits .EQ. MDU_Inches)      NewVal = OldVal * InchPerFt
         IF (NewUnits .EQ. MDU_Feet)        NewVal = OldVal
         IF (NewUnits .EQ. MDU_Miles)       NewVal = OldVal / FtPerMile
         IF (NewUnits .EQ. MDU_Millimeters) NewVal = OldVal * (MMPerInch * InchPerFt)
         IF (NewUnits .EQ. MDU_Centimeters) NewVal = OldVal * (CMPerInch * InchPerFt)
         IF (NewUnits .EQ. MDU_Meters)      NewVal = OldVal * (CMPerInch * InchPerFt) / 100.0
         IF (NewUnits .EQ. MDU_Kilometers)  NewVal = OldVal * (CMPerInch * InchPerFt) / 100000.0
      CASE (MDU_Miles)
         IF (NewUnits .EQ. MDU_Inches)      NewVal = OldVal * (InchPerFt * FtPerMile)
         IF (NewUnits .EQ. MDU_Feet)        NewVal = OldVal * FtPerMile
         IF (NewUnits .EQ. MDU_Miles)       NewVal = OldVal
         IF (NewUnits .EQ. MDU_Millimeters) NewVal = OldVal * (MMPerInch * InchPerFt * FtPerMile)
         IF (NewUnits .EQ. MDU_Centimeters) NewVal = OldVal * (CMPerInch * InchPerFt * FtPerMile)
         IF (NewUnits .EQ. MDU_Meters)      NewVal = OldVal * (CMPerInch * InchPerFt * FtPerMile) / 100.0
         IF (NewUnits .EQ. MDU_Kilometers)  NewVal = OldVal * (CMPerInch * InchPerFt * FtPerMile) / 100000.0
      CASE (MDU_Millimeters)
         IF (NewUnits .EQ. MDU_Inches)      NewVal = OldVal / MMPerInch
         IF (NewUnits .EQ. MDU_Feet)        NewVal = OldVal / MMPerInch / InchPerFt
         IF (NewUnits .EQ. MDU_Miles)       NewVal = OldVal / MMPerInch / InchPerFt / FtPerMile
         IF (NewUnits .EQ. MDU_Millimeters) NewVal = OldVal
         IF (NewUnits .EQ. MDU_Centimeters) NewVal = OldVal / 10.0
         IF (NewUnits .EQ. MDU_Meters)      NewVal = OldVal / 10.0 / 100.0
         IF (NewUnits .EQ. MDU_Kilometers)  NewVal = OldVal / 10.0 / 100000.0
      CASE (MDU_Centimeters)
         IF (NewUnits .EQ. MDU_Inches)      NewVal = OldVal / CMPerInch
         IF (NewUnits .EQ. MDU_Feet)        NewVal = OldVal / CMPerInch / InchPerFt
         IF (NewUnits .EQ. MDU_Miles)       NewVal = OldVal / CMPerInch / InchPerFt / FtPerMile
         IF (NewUnits .EQ. MDU_Millimeters) NewVal = OldVal * 10.0
         IF (NewUnits .EQ. MDU_Centimeters) NewVal = OldVal
         IF (NewUnits .EQ. MDU_Meters)      NewVal = OldVal / 100.0
         IF (NewUnits .EQ. MDU_Kilometers)  NewVal = OldVal / 100000.0
      CASE (MDU_Meters)
         IF (NewUnits .EQ. MDU_Inches)      NewVal = OldVal / CMPerInch * 100.0
         IF (NewUnits .EQ. MDU_Feet)        NewVal = OldVal / CMPerInch / InchPerFt * 100.0
         IF (NewUnits .EQ. MDU_Miles)       NewVal = OldVal / CMPerInch / InchPerFt / FtPerMile * 100.0
         IF (NewUnits .EQ. MDU_Millimeters) NewVal = OldVal * 1000.0
         IF (NewUnits .EQ. MDU_Centimeters) NewVal = OldVal * 100.0
         IF (NewUnits .EQ. MDU_Meters)      NewVal = OldVal
         IF (NewUnits .EQ. MDU_Kilometers)  NewVal = OldVal / 1000.0
      CASE (MDU_Kilometers)
         IF (NewUnits .EQ. MDU_Inches)      NewVal = OldVal / CMPerInch * 100000.0
         IF (NewUnits .EQ. MDU_Feet)        NewVal = OldVal / CMPerInch / InchPerFt * 100000.0
         IF (NewUnits .EQ. MDU_Miles)       NewVal = OldVal / CMPerInch / InchPerFt / FtPerMile * 100000.0
         IF (NewUnits .EQ. MDU_Millimeters) NewVal = OldVal * 1000000.0
         IF (NewUnits .EQ. MDU_Centimeters) NewVal = OldVal * 100000.0
         IF (NewUnits .EQ. MDU_Meters)      NewVal = OldVal * 1000.0
         IF (NewUnits .EQ. MDU_Kilometers)  NewVal = OldVal

      !         
      !  Speed
      !    I decided not to use constants here cuz it seemed messier, somehow.
      !    Purely a stylistic choice and I'm probably being hopelessly arbitrary.
      !
      CASE (MDU_MilesPerHour)
         IF (NewUnits .EQ. MDU_MilesPerHour)      NewVal = OldVal
         IF (NewUnits .EQ. MDU_Knots)             NewVal = OldVal * 0.868976
         IF (NewUnits .EQ. MDU_KilometersPerHour) NewVal = OldVal * 1.60934
         IF (NewUnits .EQ. MDU_MetersPerSecond)   NewVal = OldVal * 0.44704
      CASE (MDU_Knots)
         IF (NewUnits .EQ. MDU_MilesPerHour)      NewVal = OldVal * 1.15078
         IF (NewUnits .EQ. MDU_Knots)             NewVal = OldVal
         IF (NewUnits .EQ. MDU_KilometersPerHour) NewVal = OldVal * 1.852
         IF (NewUnits .EQ. MDU_MetersPerSecond)   NewVal = OldVal * 0.514444
      CASE (MDU_KilometersPerHour)
         IF (NewUnits .EQ. MDU_MilesPerHour)      NewVal = OldVal * 0.621371
         IF (NewUnits .EQ. MDU_Knots)             NewVal = OldVal * 0.539957
         IF (NewUnits .EQ. MDU_KilometersPerHour) NewVal = OldVal
         IF (NewUnits .EQ. MDU_MetersPerSecond)   NewVal = OldVal * 0.277778
      CASE (MDU_MetersPerSecond)
         IF (NewUnits .EQ. MDU_MilesPerHour)      NewVal = OldVal * 2.23694
         IF (NewUnits .EQ. MDU_Knots)             NewVal = OldVal * 1.94384
         IF (NewUnits .EQ. MDU_KilometersPerHour) NewVal = OldVal * 3.6
         IF (NewUnits .EQ. MDU_MetersPerSecond)   NewVal = OldVal

      !
      !  Coverage of area
      !
      CASE (MDU_Tenths)
         IF (NewUnits .EQ. MDU_Tenths)   NewVal = OldVal
         IF (NewUnits .EQ. MDU_Percent)  NewVal = OldVal * 10.0
         IF (NewUnits .EQ. MDU_Fraction) NewVal = OldVal / 10.0
      CASE (MDU_Percent)
         IF (NewUnits .EQ. MDU_Tenths)   NewVal = OldVal / 10.0
         IF (NewUnits .EQ. MDU_Percent)  NewVal = OldVal
         IF (NewUnits .EQ. MDU_Fraction) NewVal = OldVal / 100.0
      CASE (MDU_Fraction)
         IF (NewUnits .EQ. MDU_Tenths)   NewVal = OldVal * 10.0
         IF (NewUnits .EQ. MDU_Percent)  NewVal = OldVal * 100.0

      !
      !  Compass direction (0-360)
      !
      CASE (MDU_Direction360)
         IF (NewUnits .EQ. MDU_Direction360)  NewVal = OldVal
      
      !
      !  Anything else as the OldUnits is an invalid value. Should already have 
      !  been caught, but including this for completeness.
      !
      CASE DEFAULT   
         NewVal = MissingData_Real
   END SELECT
   UnitConvertedDataValue = NewVal
      
   END FUNCTION UnitConvertedDataValue
   
   !-------------------------------------------------------------
   !  Same code as the Caps() procedure in GlerlUtil, but I needed a private 
   !  version here since I don't want to USE GlerlUtil in this module.
   !
   SUBROUTINE UprCase(S)
   IMPLICIT   NONE
   CHARACTER (LEN=*), INTENT(INOUT) :: S
   INTEGER   :: I, Offset

   Offset = ICHAR('a') - ICHAR('A')
   DO I=1, LEN_TRIM(S)
      IF ( (S(I:I) .GE. 'a') .AND. (S(I:I) .LE. 'z') ) THEN
        S(I:I) = CHAR(ICHAR(S(I:I)) - Offset)
      ENDIF
   END DO
   RETURN
   END SUBROUTINE UprCase
   

END MODULE MetDataTypesAndUnits