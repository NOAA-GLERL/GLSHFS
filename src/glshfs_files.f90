!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE GLSHFS_Files
      USE ErrorProcess
      USE StatusMessages
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GlerlDataTypesAndUnits
      USE DailyDataStructures
      USE DailyDataCollections
      IMPLICIT NONE

      PRIVATE
      
      PUBLIC :: ReadJustHeaderInfo
      PUBLIC :: ReadHeaderInfo
      PUBLIC :: ReadFile_DailyStation
      PUBLIC :: WriteFile_DailyStation
      PUBLIC :: ReadFile_OneDataTypeManyStations
      PUBLIC :: WriteFile_OneDataTypeManyStations
      PUBLIC :: ReadFile_OneSubbasin
      PUBLIC :: WriteFile_OneSubbasin
      PUBLIC :: WriteFile_SubbasinSummary
      PUBLIC :: WriteFile_LakeSummary
      PUBLIC :: ReadFile_LBRM
      PUBLIC :: WriteFile_LBRM
      PUBLIC :: ReadFile_LLTM
      PUBLIC :: ReadFile_CGLRRM
      PUBLIC :: WriteFile_CGLRRM
      PUBLIC :: THeaderInfoType
      
      !
      !  These values have to be public so that procedures in other 
      !  modules can use ReadHeaderInfo().
      !
      INTEGER, PARAMETER, PUBLIC :: HFT_Undefined        =   -1
      INTEGER, PARAMETER, PUBLIC :: HFT_SingleStation    = 7001
      INTEGER, PARAMETER, PUBLIC :: HFT_SingleSubbasin   = 7002
      INTEGER, PARAMETER, PUBLIC :: HFT_SingleLake       = 7003
      INTEGER, PARAMETER, PUBLIC :: HFT_MultiStation     = 7004
      INTEGER, PARAMETER, PUBLIC :: HFT_MultiSubbasin    = 7005
      INTEGER, PARAMETER, PUBLIC :: HFT_LBRM             = 7006
      
      !
      !  These files are designed to accommodate large numbers of stations when necessary.
      !  Thus, line lengths and arrays are sized according to a variable named
      !  MaxNumberOfStations that is declared in the glshfs_global module. That
      !  longish variable name makes some things "look" messy in here, so I will
      !  declare a few variables (private to this module) that can be used in
      !  variable declarations and keep them easier to read.
      !
      !  Maximum Station Name Length
      !
      INTEGER, PRIVATE, PARAMETER :: MaxSNL = 50
      
      !
      !  Maximum Number of Stations; Just a short name copy
      !
      INTEGER, PRIVATE, PARAMETER :: MaxNS = MaxNumberOfStations
      
      !
      !
      !  Maximum Line Length
      !     I am assuming that space for commas, blanks, etc will be 
      !     covered by the fact that most station names are actually
      !     a lot less than the max size.
      !
      INTEGER, PRIVATE, PARAMETER :: MaxLL = MaxNS * MaxSNL

      !
      !  This structured type is a generic container for header information.
      !  It is set up to hold header info from ANY of the predefined formats.
      !
      !  The variable named FormatType tells which fields hold actual
      !  defined data values. For example, if the FormatType variable is
      !  assigned the value HFT_SingleStation, then the fields StnID, StnName,
      !  Latitude, Longitude, DataTypes, DataUnits will all be defined. The
      !  fields for basin, subbasin number, etc will be undefined.
      !
      !  The subroutine that fills this structure will need to be informed
      !  which format type to expect.
      !
      TYPE, PUBLIC :: THeaderInfoType
         LOGICAL :: InfoIsValid
         INTEGER :: FormatType, NumLines
         INTEGER :: SDate, EDate, NumDays, NewDataDate
         INTEGER :: SubbasinNumber
         INTEGER :: NumDataColumns
         REAL    :: Elevation, Area
         CHARACTER(LEN=3) :: Bsn
         INTEGER,               DIMENSION(:), ALLOCATABLE :: DataType, DataUnit
         REAL,                  DIMENSION(:), ALLOCATABLE :: Latitude, Longitude, SubArea
         CHARACTER(LEN=9),      DIMENSION(:), ALLOCATABLE :: SubbasinID
         CHARACTER(LEN=25),     DIMENSION(:), ALLOCATABLE :: StnID
         CHARACTER(LEN=MaxSNL), DIMENSION(:), ALLOCATABLE :: StnName
      CONTAINS
         FINAL  :: DeleteHeaderInfo
         PROCEDURE, PASS(this) :: Clear   => ClearHeaderInfo
         PROCEDURE, PASS(this) :: PrintIt => PrintHeaderInfo
      END TYPE THeaderInfoType
      INTERFACE THeaderInfoType
         PROCEDURE CreateHeaderInfo
      END INTERFACE THeaderInfoType

      
      
CONTAINS
!------------------------------------------------------------
!
!
!
      !--------------------------------------------------------
      FUNCTION CreateHeaderInfo() RESULT(this)
         IMPLICIT NONE
         TYPE (THeaderInfoType) :: this
         CALL this%Clear()
      END FUNCTION CreateHeaderInfo
      
      !--------------------------------------------------------
      SUBROUTINE DeleteHeaderInfo(this)
         IMPLICIT NONE
         TYPE (THeaderInfoType) :: this
         CALL ClearHeaderInfo(this)
      END SUBROUTINE DeleteHeaderInfo

      !--------------------------------------------------------
      SUBROUTINE ClearHeaderInfo(this)
         IMPLICIT NONE
         CLASS (THeaderInfoType) :: this
         INTEGER :: IOS
         this%InfoIsValid    = .FALSE.
         this%FormatType     = HFT_Undefined
         this%NumLines       = 0
         this%SubbasinNumber = -1
         this%Elevation      = MissingData_Real
         this%Area           = MissingData_Real
         this%Bsn            = ''
         this%SDate          = MissingData_Date
         this%EDate          = MissingData_Date
         this%NumDays        = 0
         this%NumDataColumns = 0
         IF (ALLOCATED(this%DataType))   DEALLOCATE(this%DataType,   STAT=IOS)
         IF (ALLOCATED(this%DataUnit))   DEALLOCATE(this%DataUnit,   STAT=IOS)
         IF (ALLOCATED(this%Latitude))   DEALLOCATE(this%Latitude,   STAT=IOS)
         IF (ALLOCATED(this%Longitude))  DEALLOCATE(this%Longitude,  STAT=IOS)
         IF (ALLOCATED(this%SubArea))    DEALLOCATE(this%SubArea,    STAT=IOS)
         IF (ALLOCATED(this%SubbasinID)) DEALLOCATE(this%SubbasinID, STAT=IOS)
         IF (ALLOCATED(this%StnID))      DEALLOCATE(this%StnID,      STAT=IOS)
         IF (ALLOCATED(this%StnName))    DEALLOCATE(this%StnName,    STAT=IOS)
      END SUBROUTINE ClearHeaderInfo

      !--------------------------------------------------------
      SUBROUTINE PrintHeaderInfo(this)
         IMPLICIT NONE
         CLASS (THeaderInfoType) :: this
         WRITE(ErrorMessage, 1001)  this%InfoIsValid;     CALL PassMsg
         WRITE(ErrorMessage, 1002)  this%FormatType;      CALL PassMsg
         WRITE(ErrorMessage, 1003)  this%NumLines;        CALL PassMsg
         WRITE(ErrorMessage, 1004)  this%SubbasinNumber;  CALL PassMsg
         WRITE(ErrorMessage, 1005)  this%Elevation;       CALL PassMsg
         WRITE(ErrorMessage, 1006)  this%Area;            CALL PassMsg
         WRITE(ErrorMessage, 1007)  this%Bsn;             CALL PassMsg
         WRITE(ErrorMessage, 1008)  SeqToDateStringYMD(this%SDate);  CALL PassMsg
         WRITE(ErrorMessage, 1009)  SeqToDateStringYMD(this%EDate);  CALL PassMsg
         WRITE(ErrorMessage, 1010)  this%NumDays;         CALL PassMsg
         WRITE(ErrorMessage, 1011)  this%NumDataColumns;  CALL PassMsg

         1001 FORMAT('InfoIsValid = ', L1)
         1002 FORMAT('FormatType  = ', I0)
         1003 FORMAT('NumLines    = ', I0)
         1004 FORMAT('SubbasinNum = ', I0)
         1005 FORMAT('Elevation   = ', E10.4E2)
         1006 FORMAT('Area        = ', E10.4E2)
         1007 FORMAT('Bsn         = ', A)
         1008 FORMAT('SDate       = ', A10)
         1009 FORMAT('EDate       = ', A10)
         1010 FORMAT('NumDays     = ', I0)
         1011 FORMAT('NumDataCols = ', I0)
      END SUBROUTINE PrintHeaderInfo

!-----------------------------------------------------------------------------
      !-----------------------------------------------------------------------
      !  Read the header section of a file, and return all of the data in a special
      !  THeaderInfoType variable.  This routine is intended to be used as the
      !  external interface routine when all we need is the header information
      !  from a file.  The regular ReadHeaderInfo() routine is the module-private
      !  routine used to do the actual reading of the data. It was written to be 
      !  used by the primary read procedures, and it assumes the file is already
      !  open.  This public routine takes care of opening the file and then
      !  calling that private subroutine. In that way we can keep the calling procedures
      !  a bit cleaner.
      !
      !  The HdrInfo variable is an "object", and it is assumed to have been
      !  instantiated by the caller.  Using this routine does not absolve the
      !  calling routine of that responsibility.
      !
      !  The calling procedure must use the FmtType variable to signal to 
      !  this routine what format is expected/required. 
      !
      !  For specific contents of the Hdr variable, see the header for the internal
      !  ReadHeaderInfo() routine.
      !-------------------------------------------------------------------------
      SUBROUTINE ReadJustHeaderInfo(FileName, FmtType, HdrInfo)
      IMPLICIT NONE
      CHARACTER(LEN=*),       INTENT(IN)    :: FileName
      INTEGER,                INTENT(IN)    :: FmtType
      TYPE (THeaderInfoType), INTENT(INOUT) :: HdrInfo
      
      INTEGER :: I, U1

      !
      !  Initialize the HdrInfo structure.
      !  Note that nothing is done with the allocatable arrays. They 
      !  are just left unallocated for the moment. They will be allocated
      !  as they are needed and as we get the necessary info to size
      !  them correctly.
      !      
      CALL HdrInfo%Clear()
      HdrInfo%FormatType = FmtType
      HdrInfo%InfoIsValid = .TRUE.     ! assume ok by default.

      !
      !
      U1 = -1
      U1 = GetFreeUnitNumber();  IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)

      CALL ReadHeaderInfo(Filename, U1, FmtType, HdrInfo);  IF (ErrorLevel .NE. 0) GOTO 899
      
      CLOSE(U1)
      CALL FileWasClosed(U1);  U1 = -1
      RETURN

  811 ErrorMessage = 'Error opening input file: '//TRIM(FileName); CALL PassMsg; GOTO 898   
   
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ReadJustHeaderInfo'; CALL PassMsg
      CALL HdrInfo%Clear()
      HdrInfo%InfoIsValid = .FALSE.
      CLOSE(U1, IOSTAT=I)
      IF (U1 .GT. 0) CALL FileWasClosed(U1)
      RETURN
      
      END SUBROUTINE ReadJustHeaderInfo

!-----------------------------------------------------------------------------
      !------------------------------------------------------------
      !  Read the header section of a file, and return all of the data in a special
      !  THeaderInfoType variable.  The file is assumed to already be opened, with 
      !  unit number in U1.
      !
      !  The calling procedure must use the FmtType variable to signal to 
      !  this routine what format is expected/required. 
      !
      !  The HdrInfo variable is an "object", and it is assumed to have been
      !  instantiated by the caller.
      !
      !  Items in the header depends on the file type:
      !  Example headers are here.  Data types may/will vary.
      !  Note that station names MUST be enclosed within double-quotes because some
      !  station names include imbedded commas.
      !  
      !  HFT_SingleStation:
      !    line 1: Daily data for a single station.
      !    line 2: StationID:, 710514-99999
      !    line 3: StationName:, "Marathon"
      !    line 4: Lat&Long:, 48.75, -86.333
      !    line 5: Elevation:, -999
      !    line 6: Starts(YMD):, 1950-01-01
      !    line 7: Ends(YMD):, 2014-12-31
      !    line 8: Date,AirTempMax,AirTempMin,Precipitation,AirTemp,Dewpoint,Windspeed,CloudCover
      !    line 9: YYYY-MM-DD,Celsius,Celsius,Millimeters,Celsius,Celsius,MetersPerSecond,Percent
      !
      !  HFT_SingleSubbasin:
      !    line 1: <descriptive text>
      !    line 2: Lake:, SUP
      !    line 3: Subbasin:, 1           (instead of subbasin number, can also be lake, land, basin)
      !    line 4: Area:, 9.70700E+09
      !    line 5: Starts(YMD):, 1950-01-01
      !    line 6: Ends(YMD):, 2014-12-31
      !    line 7: NewDataStart(YMD):, 9999-99-99
      !    line 8: Date,AirTempMax,AirTempMin,Precipitation,AirTemp,Dewpoint,Windspeed,CloudCover
      !    line 9: YYYY-MM-DD,Celsius,Celsius,Millimeters,Celsius,Celsius,MetersPerSecond,Percent
      !
      !  HFT_SingleLake:
      !    line 1: Daily data for a single lake.
      !    line 2: Lake:, SUP
      !    line 3: [unused]
      !    line 4: Area:, 9.70700E+09
      !    line 5: Starts(YMD):, 1950-01-01
      !    line 6: Ends(YMD):, 2014-12-31
      !    line 7: [unused]
      !    line 8: Date,AirTempMax,AirTempMin,Precipitation,AirTemp,Dewpoint,Windspeed,CloudCover
      !    line 9: YYYY-MM-DD,Celsius,Celsius,Millimeters,Celsius,Celsius,MetersPerSecond,Percent
      !
      !  HFT_MultiStation:
      !    line 1: Daily data for multiple stations.
      !    line 2: Datatype:, AirtempMax
      !    line 3: Dataunits:, Celsius
      !    line 4: Starts(YMD):, 1950-01-01, NewDataStart(YMD):, 2006-07-01
      !    line 5: Ends(YMD):, 2014-12-31
      !    line 6: StationID:, 710514-99999, 712600-99999, 712603-9999
      !    line 7: StationName:, "Marathon", "Sault Ste Marie A", "Sault Ste Marie &"
      !    line 8: Latitude:, 48.75, 46.483, 46.483
      !    line 9: Longitude:, -86.333, -84.517, -84.5
      !    line 10: YYYY-MM-DD
      !
      !  HFT_MultiSubbasin:
      !    line 1: Daily data for multiple subbasins.
      !    line 2: Datatype:, AirtempMax
      !    line 3: Dataunits:, Celsius
      !    line 4: Starts(YMD):, 1950-01-01
      !    line 5: Ends(YMD):, 2014-12-31
      !    line 6: Lake:, SUP, SUP, SUP, SUP
      !    line 7: Subbasin:, 1, 2, Lake, Basin
      !    line 8: Area:, 9.70700E+09, 4.94300+09, 8.1925e+10, 2.10009E+11
      !    line 9: YYYY-MM-DD
      !
      !  HFT_LBRM:
      !    line 1: Output from LBRM.
      !    line 2: Lake:, SUP
      !    line 3: Subbasin:, 1
      !    line 4: Area:, 9.70700E+09
      !    line 5: Starts(YMD):, 1950-01-01
      !    line 6: Ends(YMD):, 2014-12-31
      !    line 7: Runoff and storages expressed in millimeters over the subbasin.
      !    line 8: Date,Runoff,UpperSoilMoist,LowerSoilMoist,GroundWater,SurfaceStorage,SnowWaterEquiv
      !    line 9: YYYY-MM-DD,Millimeters,Millimeters,Millimeters,Millimeters,Millimeters,Millimeters
      !
      !------------------------------------------------------------
      SUBROUTINE ReadHeaderInfo(FileName, U1, FmtType, HdrInfo)
      IMPLICIT NONE
      CHARACTER(LEN=*),       INTENT(IN)    :: FileName
      INTEGER,                INTENT(IN)    :: U1, FmtType
      TYPE (THeaderInfoType), INTENT(INOUT) :: HdrInfo
      
      INTEGER :: I
      CHARACTER(LEN=MaxLL),  DIMENSION(10) :: HLines

      !
      !  Initialize the HdrInfo structure.
      !  Note that nothing is done with the allocatable arrays. They 
      !  are just left unallocated for the moment. They will be allocated
      !  as they are needed and as we get the necessary info to size
      !  them correctly.
      !      
      CALL HdrInfo%Clear()
      HdrInfo%FormatType = FmtType
      HdrInfo%InfoIsValid = .TRUE.     ! assume ok by default.
      
      !
      !  Read the necessary lines of of the file to get the entire
      !  header for this format
      !
      HLines(:) = ''
      DO I = 1, 9
         READ(U1, 1000, ERR=812) HLines(I)
      END DO
      IF (FmtType .EQ. HFT_MultiStation) THEN
         READ(U1, 1000, ERR=812) HLines(10)
      END IF
      

      !
      !  Call the appropriate parsing routine.
      !
      !  Note of interest is that I first tried implementing all of the
      !  parsing within this routine, using SELECT blocks for each line.
      !  But that became very "messy" and difficult to read with all of the
      !  error messages and combinations of which line had which meaning 
      !  in which file type. It was much cleaner and reader-friendly to do
      !  it this way.
      !
      SELECT CASE (FmtType)
         CASE (HFT_SingleStation)
            CALL ParseSingleStationHeader(Filename, HLines, HdrInfo);   IF (ErrorLevel .NE. 0) GOTO 899
            HdrInfo%NumLines = 9
            
         CASE (HFT_SingleSubbasin)
            CALL ParseSingleSubbasinHeader(Filename, HLines, HdrInfo);  IF (ErrorLevel .NE. 0) GOTO 899
            HdrInfo%NumLines = 9
            
         CASE (HFT_SingleLake)
            CALL ParseSingleLakeHeader(Filename, HLines, HdrInfo);      IF (ErrorLevel .NE. 0) GOTO 899
            HdrInfo%NumLines = 9
            
         CASE (HFT_MultiStation)
            CALL ParseMultiStationHeader(Filename, HLines, HdrInfo);    IF (ErrorLevel .NE. 0) GOTO 899
            HdrInfo%NumLines = 10
            
         CASE (HFT_MultiSubbasin)
            CALL ParseMultiStationHeader(Filename, HLines, HdrInfo);    IF (ErrorLevel .NE. 0) GOTO 899
            HdrInfo%NumLines = 9

         CASE (HFT_LBRM)
            CALL ParseLbrmHeader(Filename, HLines, HdrInfo);            IF (ErrorLevel .NE. 0) GOTO 899
            HdrInfo%NumLines = 9
            
      END SELECT

      IF ((HdrInfo%SDate .NE. MissingData_Date) .AND.              &
          (HdrInfo%EDate .NE. MissingData_Date)) THEN
         HdrInfo%NumDays = HdrInfo%EDate - HdrInfo%SDate + 1
      ELSE
         HdrInfo%NumDays = 0
      END IF
      
      RETURN

  812 ErrorMessage = 'Error reading input file: '//TRIM(FileName); CALL PassMsg; GOTO 898   
   
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ReadHeaderInfo'; CALL PassMsg
      CALL HdrInfo%Clear()
      HdrInfo%InfoIsValid = .FALSE.
      RETURN

 1000 FORMAT(A1500000)      ! Just make sure this is >= MaxLL
     
      END SUBROUTINE ReadHeaderInfo


!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Parse the header section for a single-station file
      !------------------------------------------------------------
      SUBROUTINE ParseSingleStationHeader(Filename, HLines, HdrInfo)
      IMPLICIT NONE
      CHARACTER(LEN=*),                   INTENT(IN)    :: FileName
      CHARACTER(LEN=MaxLL), DIMENSION(:), INTENT(IN)    :: HLines
      TYPE (THeaderInfoType),             INTENT(INOUT) :: HdrInfo
      
      INTEGER :: I, IOS, Seq
      INTEGER :: NumStrings
      LOGICAL :: F
      CHARACTER(LEN=100) :: S, TStr
      CHARACTER(LEN=100), DIMENSION(5000) :: CsvStrings

      !
      !  line 1
      !
      CALL ParseCommaSepLine(HLines(1), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 1) GOTO 301
      TStr = GetLowercase(TRIM(ADJUSTL(CsvStrings(1))))
      IF (TRIM(TStr) .NE. 'daily data for a single station.') GOTO 401
         
      !
      !  Line 2
      !
      CALL ParseCommaSepLine(HLines(2), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 302
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      IF (TRIM(S) .NE. 'stationid:') GOTO 332
      ALLOCATE(HdrInfo%StnID(1), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      S = TRIM(ADJUSTL(CsvStrings(2)))
      HdrInfo%StnID(1) = TRIM(StripDoubleQuotes(S))

      !
      !  Line 3
      !
      CALL ParseCommaSepLine(HLines(3), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 303
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      IF (TRIM(S) .NE. 'stationname:') GOTO 333
      ALLOCATE(HdrInfo%StnName(1), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      HdrInfo%StnName(1) = TRIM(ADJUSTL(CsvStrings(2)))
      S = TRIM(HdrInfo%StnName(1))
      IF (S(1:1) .NE. '"') THEN
         HdrInfo%StnName(1) = '"' // TRIM(HdrInfo%StnName(1)) // '"'     ! ensure station name is enclosed by double-quotes
      END IF
            
      !
      !  Line 4
      !
      CALL ParseCommaSepLine(HLines(4), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 3) GOTO 304
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      F = .FALSE.
      IF (TRIM(S) .EQ. 'lat&long')  F = .TRUE.
      IF (TRIM(S) .EQ. 'lat&long:') F = .TRUE. 
      IF (.NOT. F) GOTO 334
      ALLOCATE(HdrInfo%Latitude(1), HdrInfo%Longitude(1), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      TStr = TRIM(ADJUSTL(CsvStrings(2)));  READ(TStr, *, ERR=402) HdrInfo%Latitude(1)
      TStr = TRIM(ADJUSTL(CsvStrings(3)));  READ(TStr, *, ERR=403) HdrInfo%Longitude(1)
      
      !
      !  Line 5
      !
      CALL ParseCommaSepLine(HLines(5), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 305
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'elevation:') GOTO 335
      TStr = TRIM(ADJUSTL(CsvStrings(2)));  READ(TStr, *, ERR=405) HdrInfo%Elevation
 
      !
      !  Line 6
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(6), 6, 'Starts(ymd)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%SDate = Seq
            
      !
      !  Line 7
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(7), 7, 'Ends(ymd)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%EDate = Seq
            
      !
      !  Assuming valid dates, compute number of days
      !
      IF ((HdrInfo%SDate .NE. MissingData_Date) .AND.       &
          (HdrInfo%EDate .NE. MissingData_Date)) THEN
         HdrInfo%NumDays = HdrInfo%EDate - HdrInfo%SDate + 1
      ELSE
         HdrInfo%NumDays = 0
      END IF
      
      !
      !  Line 8
      !
      CALL ParseCommaSepLine(HLines(8), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 3) GOTO 308
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'date') GOTO 338
      HdrInfo%NumDataColumns = NumStrings - 1
      I = HdrInfo%NumDataColumns
      ALLOCATE(HdrInfo%DataType(I), HdrInfo%DataUnit(I), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         TStr = TRIM(StripDoubleQuotes(TStr))
         HdrInfo%DataType(I) = GlerlDataTypeFromString(TStr)
         IF (HdrInfo%DataType(I) .EQ. GDT_Undefined) THEN
            ErrorMessage = 'Unrecognized data type string = ' // TRIM(TStr);  CALL PassMsg
            GOTO 408
         END IF
      END DO

      !
      !  Line 9
      !
      CALL ParseCommaSepLine(HLines(9), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 3) GOTO 309
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'yyyy-mm-dd') GOTO 339
      I = NumStrings - 1
      IF (I .NE. HdrInfo%NumDataColumns) GOTO 409
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         TStr = TRIM(StripDoubleQuotes(TStr))
         HdrInfo%DataUnit(I) = GlerlDataUnitFromString(TStr)
         IF (HdrInfo%DataUnit(I) .EQ. GDU_Undefined) THEN
            ErrorMessage = 'Unrecognized data unit string = ' // TRIM(TStr);  CALL PassMsg
            GOTO 409
         END IF
      END DO
      
      HdrInfo%InfoIsValid = .TRUE.
      
      RETURN
      
      !
      !  Errors messages
      !      
  301 ErrorMessage = 'Formatting error on line 1 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  302 ErrorMessage = 'Formatting error on line 2 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  303 ErrorMessage = 'Formatting error on line 3 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  304 ErrorMessage = 'Formatting error on line 4 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  305 ErrorMessage = 'Formatting error on line 5 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  308 ErrorMessage = 'Formatting error on line 8 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  309 ErrorMessage = 'Formatting error on line 9 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  332 ErrorMessage = 'Error with label string on line 2 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  333 ErrorMessage = 'Error with label string on line 3 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  334 ErrorMessage = 'Error with label string on line 4 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  335 ErrorMessage = 'Error with label string on line 5 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  338 ErrorMessage = 'Error with label string on line 8 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  339 ErrorMessage = 'Error with label string on line 9 in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  401 ErrorMessage = 'Invalid entry on line 1 of '             //TRIM(FileName); CALL PassMsg
      ErrorMessage = 'Entry should say "Daily data for a single station."';      CALL PassMsg; GOTO 898
  402 ErrorMessage = 'Error parsing Station ID on line 2 in '  //TRIM(FileName); CALL PassMsg; GOTO 898
  403 ErrorMessage = 'Error parsing Station Name on line 3 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  405 ErrorMessage = 'Error parsing Elevation on line 5 in '   //TRIM(FileName); CALL PassMsg; GOTO 898
  408 ErrorMessage = 'Error parsing line 8 in '                //TRIM(FileName); CALL PassMsg; GOTO 898
  409 ErrorMessage = 'Error parsing line 9 in '                //TRIM(FileName); CALL PassMsg; GOTO 898
  
      !
      !  memory allocation error
      !      
  851 ErrorMessage = 'Error allocating memory for temporary storage of header information'; CALL PassMsg
      ErrorMessage = 'while reading '//TRIM(FileName); CALL PassMsg; GOTO 898

    
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ParseSingleStationHeader'; CALL PassMsg
      CALL HdrInfo%Clear()
      RETURN
      
      END SUBROUTINE ParseSingleStationHeader
      
!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Parse the header section for a single-subbasin file
      !------------------------------------------------------------
      SUBROUTINE ParseSingleSubbasinHeader(Filename, HLines, HdrInfo)
      IMPLICIT NONE
      CHARACTER(LEN=*),                   INTENT(IN)    :: FileName
      CHARACTER(LEN=MaxLL), DIMENSION(:), INTENT(IN)    :: HLines
      TYPE (THeaderInfoType),             INTENT(INOUT) :: HdrInfo
      
      INTEGER :: I, IOS, Sub, LkNum, Seq
      INTEGER :: NumStrings
      CHARACTER(LEN=100) :: S, TStr
      CHARACTER(LEN=100), DIMENSION(5000) :: CsvStrings

      !
      !  line 1
      !  Originally I checked this for matching a specific string, but now it is free text.
      !
      CALL ParseCommaSepLine(HLines(1), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 1) GOTO 301
!      TStr = GetLowercase(TRIM(ADJUSTL(CsvStrings(1))))
!      OK = .FALSE.
!      IF (TRIM(TStr) .EQ. 'daily data for a single subbasin.') OK = .TRUE.
!      IF (TRIM(TStr) .EQ. 'output from lbrm.')                 OK = .TRUE.
!      IF (.NOT. OK) GOTO 401
         
      !
      !  Line 2
      !
      CALL ParseCommaSepLine(HLines(2), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 302
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'lake:') GOTO 332
      S = TRIM(ADJUSTL(CsvStrings(2)))
      LkNum = LakeNumberFromName3(S); IF (ErrorLevel .NE. 0) LkNum = -1
      ErrorLevel = 0
      IF (LkNum .LT. 1) THEN
         LkNum = LakeNumberFromName10(S); IF (ErrorLevel .NE. 0) LkNum = -1
         ErrorLevel = 0
      END IF
      IF (LkNum .LT. 1) GOTO 402
      HdrInfo%Bsn = LakeName3(LkNum)     ! save the 3-char lake name
      
      !
      !  Line 3
      !
      CALL ParseCommaSepLine(HLines(3), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 303
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'subbasin:') GOTO 333
      TStr = GetLowercase(TRIM(ADJUSTL(CsvStrings(2))))
      READ(TStr, *, IOSTAT=IOS) Sub
      IF (IOS .EQ. 0) THEN
         IF (Sub .LT.                   0) GOTO 403
         IF (Sub .GT. NumSubbasins(LkNum)) GOTO 403
      ELSE
         Sub = -1
         IF (TRIM(TStr) .EQ. 'lake' ) Sub = 0
         IF (TRIM(TStr) .EQ. 'land' ) Sub = SubNum_OverLand
         IF (TRIM(TStr) .EQ. 'basin') Sub = SubNum_OverBasin
         IF (TRIM(TStr) .EQ. 'overlake' ) Sub = 0
         IF (TRIM(TStr) .EQ. 'overland' ) Sub = SubNum_OverLand
         IF (TRIM(TStr) .EQ. 'overbasin') Sub = SubNum_OverBasin
         IF (Sub .LT. 0) GOTO 403
      END IF
      HdrInfo%SubbasinNumber = Sub
            
      !
      !  Line 4
      !
      CALL ParseCommaSepLine(HLines(4), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 304
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'area:') GOTO 334
      TStr = TRIM(ADJUSTL(CsvStrings(2)));  READ(TStr, *, ERR=404) HdrInfo%Area

      !
      !  Line 5
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(5), 5, 'Starts(ymd)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%SDate = Seq
           
      !
      !  Line 6
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(6), 6, 'Ends(YMD)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%EDate = Seq
            
      !
      !  Line 7 tracks the newest "new data" that has been added/updated.
      !  When GLSHFS updates LBRM output files using this data it will reset
      !  it to 9999-99-99 to indicate no new data.
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(7), 7, 'NewDataStart(YMD)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%NewDataDate = Seq
            

      !
      !  Line 8
      !
      CALL ParseCommaSepLine(HLines(8), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 3) GOTO 308
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'date') GOTO 338
      HdrInfo%NumDataColumns = NumStrings - 1
      I = HdrInfo%NumDataColumns
      ALLOCATE(HdrInfo%DataType(I), HdrInfo%DataUnit(I), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         TStr = TRIM(StripDoubleQuotes(TStr))
         HdrInfo%DataType(I) = GlerlDataTypeFromString(TStr)
         IF (HdrInfo%DataType(I) .EQ. GDT_Undefined) THEN
            ErrorMessage = 'Unrecognized data type string = ' // TRIM(TStr);  CALL PassMsg
            GOTO 408
         END IF
      END DO

      !
      !  Line 9
      !
      CALL ParseCommaSepLine(HLines(9), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 3) GOTO 309
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'yyyy-mm-dd') GOTO 339
      I = NumStrings - 1
      IF (I .NE. HdrInfo%NumDataColumns) GOTO 409
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         TStr = TRIM(StripDoubleQuotes(TStr))
         HdrInfo%DataUnit(I) = GlerlDataUnitFromString(TStr)
         IF (HdrInfo%DataUnit(I) .EQ. GDU_Undefined) THEN
            ErrorMessage = 'Unrecognized data unit string = ' // TRIM(TStr);  CALL PassMsg
            GOTO 409
         END IF
      END DO

      HdrInfo%InfoIsValid = .TRUE.

      RETURN
      
      !
      !  Errors messages
      !      
  301 ErrorMessage = 'Formatting error on line 1 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  302 ErrorMessage = 'Formatting error on line 2 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  303 ErrorMessage = 'Formatting error on line 3 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  304 ErrorMessage = 'Formatting error on line 4 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  308 ErrorMessage = 'Formatting error on line 8 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  309 ErrorMessage = 'Formatting error on line 9 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  332 ErrorMessage = 'Error with label string on line 2 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  333 ErrorMessage = 'Error with label string on line 3 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  334 ErrorMessage = 'Error with label string on line 4 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  338 ErrorMessage = 'Error with label string on line 8 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  339 ErrorMessage = 'Error with label string on line 9 in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  402 ErrorMessage = 'Invalid lake named on line 2 in '                 //TRIM(FileName); CALL PassMsg; GOTO 898
  403 ErrorMessage = 'Invalid subbasin '//TRIM(TStr)//' on line 3 in '  //TRIM(FileName); CALL PassMsg; GOTO 898
  404 ErrorMessage = 'Error parsing area on line 4 in '                 //TRIM(FileName); CALL PassMsg; GOTO 898
  408 ErrorMessage = 'Error parsing line 8 in '                         //TRIM(FileName); CALL PassMsg; GOTO 898
  409 ErrorMessage = 'Error parsing line 9 in '                         //TRIM(FileName); CALL PassMsg; GOTO 898
  
      !
      !  memory allocation error
      !      
  851 ErrorMessage = 'Error allocating memory for temporary storage of header information'; CALL PassMsg
      ErrorMessage = 'while reading '//TRIM(FileName); CALL PassMsg; GOTO 898

    
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ParseSingleSubbasinHeader'; CALL PassMsg
      CALL HdrInfo%Clear()
      RETURN
      
      END SUBROUTINE ParseSingleSubbasinHeader
      
!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Parse the header section for a single-subbasin file
      !------------------------------------------------------------
      SUBROUTINE ParseSingleLakeHeader(Filename, HLines, HdrInfo)
      IMPLICIT NONE
      CHARACTER(LEN=*),                   INTENT(IN)    :: FileName
      CHARACTER(LEN=MaxLL), DIMENSION(:), INTENT(IN)    :: HLines
      TYPE (THeaderInfoType),             INTENT(INOUT) :: HdrInfo
      
      INTEGER :: I, IOS, LkNum, Seq
      INTEGER :: NumStrings
      CHARACTER(LEN=100) :: S, TStr
      CHARACTER(LEN=100), DIMENSION(5000) :: CsvStrings

      !
      !  line 1
      !
      CALL ParseCommaSepLine(HLines(1), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 1) GOTO 301
      TStr = GetLowercase(TRIM(ADJUSTL(CsvStrings(1))))
      IF (TRIM(TStr) .NE. 'daily data for a single lake.') GOTO 401
         
      !
      !  Line 2
      !
      CALL ParseCommaSepLine(HLines(2), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 302
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'lake:') GOTO 332
      S = TRIM(ADJUSTL(CsvStrings(2)))
      LkNum = LakeNumberFromName3(S); IF (ErrorLevel .NE. 0) LkNum = -1
      ErrorLevel = 0
      IF (LkNum .LT. 1) THEN
         LkNum = LakeNumberFromName10(S); IF (ErrorLevel .NE. 0) LkNum = -1
         ErrorLevel = 0
      END IF
      IF (LkNum .LT. 1) GOTO 402
      HdrInfo%Bsn = LakeName3(LkNum)     ! save the 3-char lake name
      
      !
      !  Line 3 is unused/ignored
      !
            
      !
      !  Line 4
      !
      CALL ParseCommaSepLine(HLines(4), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 304
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'area:') GOTO 334
      TStr = TRIM(ADJUSTL(CsvStrings(2)));  READ(TStr, *, ERR=404) HdrInfo%Area

      !
      !  Line 5
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(5), 5, 'Starts(ymd)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%SDate = Seq
            
      !
      !  Line 6
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(6), 6, 'Ends(ymd)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%EDate = Seq
            
      !
      !  Line 7 is unused/ignored
      !
            
      !
      !  Line 8
      !
      CALL ParseCommaSepLine(HLines(8), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 3) GOTO 308
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'date') GOTO 338
      HdrInfo%NumDataColumns = NumStrings - 1
      I = HdrInfo%NumDataColumns
      ALLOCATE(HdrInfo%DataType(I), HdrInfo%DataUnit(I), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         TStr = TRIM(StripDoubleQuotes(TStr))
         HdrInfo%DataType(I) = GlerlDataTypeFromString(TStr)
         IF (HdrInfo%DataType(I) .EQ. GDT_Undefined) THEN
            ErrorMessage = 'Unrecognized data type string = ' // TRIM(TStr);  CALL PassMsg
            GOTO 408
         END IF
      END DO

      !
      !  Line 9
      !
      CALL ParseCommaSepLine(HLines(9), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 3) GOTO 309
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'yyyy-mm-dd') GOTO 339
      I = NumStrings - 1
      IF (I .NE. HdrInfo%NumDataColumns) GOTO 409
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         TStr = TRIM(StripDoubleQuotes(TStr))
         HdrInfo%DataUnit(I) = GlerlDataUnitFromString(TStr)
         IF (HdrInfo%DataUnit(I) .EQ. GDU_Undefined) THEN
            ErrorMessage = 'Unrecognized data unit string = ' // TRIM(TStr);  CALL PassMsg
            GOTO 409
         END IF
      END DO

      HdrInfo%InfoIsValid = .TRUE.
      
      RETURN
      
      !
      !  Errors messages
      !      
  301 ErrorMessage = 'Formatting error on line 1 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  302 ErrorMessage = 'Formatting error on line 2 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  304 ErrorMessage = 'Formatting error on line 4 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  308 ErrorMessage = 'Formatting error on line 8 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  309 ErrorMessage = 'Formatting error on line 9 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  332 ErrorMessage = 'Error with label string on line 2 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  334 ErrorMessage = 'Error with label string on line 4 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  338 ErrorMessage = 'Error with label string on line 8 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  339 ErrorMessage = 'Error with label string on line 9 in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  401 ErrorMessage = 'Invalid entry on line 1 of '             //TRIM(FileName); CALL PassMsg
      ErrorMessage = 'Entry should say "Daily data for a single lake."';     CALL PassMsg; GOTO 898
  402 ErrorMessage = 'Invalid lake named on line 2 in '        //TRIM(FileName); CALL PassMsg; GOTO 898
  404 ErrorMessage = 'Error parsing area on line 4 in '        //TRIM(FileName); CALL PassMsg; GOTO 898
  408 ErrorMessage = 'Error parsing line 8 in '                //TRIM(FileName); CALL PassMsg; GOTO 898
  409 ErrorMessage = 'Error parsing line 9 in '                //TRIM(FileName); CALL PassMsg; GOTO 898
  
      !
      !  memory allocation error
      !      
  851 ErrorMessage = 'Error allocating memory for temporary storage of header information'; CALL PassMsg
      ErrorMessage = 'while reading '//TRIM(FileName); CALL PassMsg; GOTO 898

    
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ParseSingleLakeHeader'; CALL PassMsg
      CALL HdrInfo%Clear()
      RETURN
      
      END SUBROUTINE ParseSingleLakeHeader
      
!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Parse the header section for a multi-station file
      !------------------------------------------------------------
      SUBROUTINE ParseMultiStationHeader(Filename, HLines, HdrInfo)
      IMPLICIT NONE
      CHARACTER(LEN=*),                   INTENT(IN)    :: FileName
      CHARACTER(LEN=MaxLL), DIMENSION(:), INTENT(IN)    :: HLines
      TYPE (THeaderInfoType),             INTENT(INOUT) :: HdrInfo
      
      INTEGER :: I, J, IOS, DType, DUnit, Seq
      INTEGER :: NumStrings
      CHARACTER(LEN=100) :: S, TStr, Stage
      CHARACTER(LEN=MaxSNL), DIMENSION(MaxNS+1) :: CsvStrings

      !
      !  line 1
      !
      CALL ParseCommaSepLine(HLines(1), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 1) GOTO 301
      TStr = GetLowercase(TRIM(ADJUSTL(CsvStrings(1))))
      IF (TRIM(TStr) .NE. 'daily data for multiple stations.') GOTO 401
         
      !
      !  Line 2
      !
      CALL ParseHdrLineDataType(TRIM(FileName), HLines(2), 2, DType); IF (ErrorLevel .NE. 0) GOTO 898
      IF (DType .EQ. GDT_Undefined) GOTO 402
      ALLOCATE(HdrInfo%DataType(1), STAT=IOS)
      HdrInfo%DataType(1) = DType

      !
      !  Line 3
      !
      CALL ParseHdrLineDataUnit(TRIM(FileName), HLines(3), 3, DUnit); IF (ErrorLevel .NE. 0) GOTO 898
      IF (DUnit .EQ. GDU_Undefined) GOTO 403
      ALLOCATE(HdrInfo%DataUnit(1), STAT=IOS)
      HdrInfo%DataUnit(1) = DUnit

      !
      !  Line 4
      !  Additional fields added by Tim Hunter on 9-13-2017 to track new data being added as
      !  part of the provisional station data update in glshfs. This doesn't fit in the standard
      !  ParseHdrLineDateField routine, so I have to do it special here.
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(4), 4, 'Starts(ymd)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%SDate = Seq
      HdrInfo%NewDataDate = MissingData_Date    ! default value
      CALL ParseCommaSepLine(HLines(4), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .GE. 4) THEN
         TStr = GetLowercase(StripAllBlanks(CsvStrings(3)))
         IF (TRIM(TStr) .EQ. 'newdatastart(ymd):') THEN
            Seq = DateStringYMDToSeq(TRIM(ADJUSTL(CsvStrings(4))))
            IF (ErrorLevel .EQ. 0) HdrInfo%NewDataDate = Seq
         END IF
      END IF
      
      !
      !  Line 5
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(5), 5, 'Ends(ymd)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%EDate = Seq
            
      !
      !  Line 6
      !
      Stage = 'Station ID/Name/Lat/Long'
      CALL ParseCommaSepLine(HLines(6), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 306
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'stationid:') GOTO 336
      HdrInfo%NumDataColumns = NumStrings - 1
      I = HdrInfo%NumDataColumns
      ALLOCATE(HdrInfo%StnID(I), HdrInfo%StnName(I), HdrInfo%Latitude(I),   &
               HdrInfo%Longitude(I), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      
      DO I = 1, HdrInfo%NumDataColumns
         HdrInfo%StnID(I) = TRIM(ADJUSTL(CsvStrings(I+1)))
      END DO
            
      !
      !  Line 7
      !
      CALL ParseCommaSepLine(HLines(7), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
 
      IF (NumStrings .LT. 2) GOTO 307
      IF (NumStrings .NE. HdrInfo%NumDataColumns+1) GOTO 407
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'stationname:') GOTO 337
      DO I = 1, HdrInfo%NumDataColumns
         J = MIN(LEN_TRIM(CsvStrings(I+1)), MaxSNL)
         HdrInfo%StnName(I) = TRIM(ADJUSTL(CsvStrings(I+1)(1:J)))
      END DO
            
      !
      !  Line 8
      !
      CALL ParseCommaSepLine(HLines(8), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 308
      IF (NumStrings .NE. HdrInfo%NumDataColumns+1) GOTO 408
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'latitude:') GOTO 338
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         READ(TStr, *, ERR=408) HdrInfo%Latitude(I)
      END DO

      !
      !  Line 9
      !
      CALL ParseCommaSepLine(HLines(9), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 309
      IF (NumStrings .NE. HdrInfo%NumDataColumns+1) GOTO 409
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'longitude:') GOTO 339
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         READ(TStr, *, ERR=409) HdrInfo%Longitude(I)
      END DO
      
      !
      !  Line 10
      !
      S = GetLowercase(HLines(10))
      CALL ParseCommaSepLine(S, CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 1) GOTO 310
      IF (TRIM(CsvStrings(1)) .NE. 'yyyy-mm-dd') GOTO 340
      
      HdrInfo%InfoIsValid = .TRUE.
      
      RETURN
      
      !
      !  Errors messages
      !      
  301 ErrorMessage = 'Formatting error on line 1 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  306 ErrorMessage = 'Formatting error on line 6 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  307 ErrorMessage = 'Formatting error on line 7 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  308 ErrorMessage = 'Formatting error on line 8 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  309 ErrorMessage = 'Formatting error on line 9 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  310 ErrorMessage = 'Formatting error on line 10 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  336 ErrorMessage = 'Error with label string on line 6 in '  //TRIM(FileName); CALL PassMsg; GOTO 898
  337 ErrorMessage = 'Error with label string on line 7 in '  //TRIM(FileName); CALL PassMsg; GOTO 898
  338 ErrorMessage = 'Error with label string on line 8 in '  //TRIM(FileName); CALL PassMsg; GOTO 898
  339 ErrorMessage = 'Error with label string on line 9 in '  //TRIM(FileName); CALL PassMsg; GOTO 898
  340 ErrorMessage = 'Error with column labels on line 10 in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  401 ErrorMessage = 'Invalid entry on line 1 of '                     //TRIM(FileName); CALL PassMsg
      ErrorMessage = 'Entry should say "Daily data for multiple stations."';             CALL PassMsg; GOTO 898
  402 ErrorMessage = 'Error parsing data type on line 2 in '           //TRIM(FileName); CALL PassMsg; GOTO 898
  403 ErrorMessage = 'Error parsing data units on line 3 in '          //TRIM(FileName); CALL PassMsg; GOTO 898
  407 ErrorMessage = 'Error parsing line 7 for station names; file '   //TRIM(FileName); CALL PassMsg; GOTO 898
  408 ErrorMessage = 'Error parsing line 8 for latitudes; file '       //TRIM(FileName); CALL PassMsg; GOTO 898
  409 ErrorMessage = 'Error parsing line 9 for longitudes; file '      //TRIM(FileName); CALL PassMsg; GOTO 898

      !
      !  memory allocation error
      !      
  851 WRITE(ErrorMessage, 5001) TRIM(Stage); CALL PassMsg
      ErrorMessage = 'while reading '//TRIM(FileName); CALL PassMsg; GOTO 898

    
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ParseMultiStationHeader'; CALL PassMsg
      CALL HdrInfo%Clear()
      RETURN

 5001 FORMAT('Error allocating memory for storage of ', A)
      
      END SUBROUTINE ParseMultiStationHeader
      
!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Parse the header section for a multi-subbasin file (single data type)
      !------------------------------------------------------------
      SUBROUTINE ParseMultiSubbasinHeader(Filename, HLines, HdrInfo)
      IMPLICIT NONE
      CHARACTER(LEN=*),                   INTENT(IN)    :: FileName
      CHARACTER(LEN=MaxLL), DIMENSION(:), INTENT(IN)    :: HLines
      TYPE (THeaderInfoType),             INTENT(INOUT) :: HdrInfo
      
      INTEGER :: I, J, IOS, DType, DUnit, Seq
      INTEGER :: NumStrings
      CHARACTER(LEN=100) :: S, TStr
      CHARACTER(LEN=100), DIMENSION(5000) :: CsvStrings
      CHARACTER(LEN=10), DIMENSION(:), ALLOCATABLE :: SubLake, SubNum

      !
      !  line 1
      !
      CALL ParseCommaSepLine(HLines(1), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 1) GOTO 301
      TStr = GetLowercase(TRIM(ADJUSTL(CsvStrings(1))))
      IF (TRIM(TStr) .NE. 'daily data for multiple subbasins.') GOTO 401
         
      !
      !  Line 2
      !
      CALL ParseHdrLineDataType(TRIM(FileName), HLines(2), 2, DType); IF (ErrorLevel .NE. 0) GOTO 898
      IF (DType .EQ. GDT_Undefined) GOTO 402
      ALLOCATE(HdrInfo%DataType(1), STAT=IOS)
      HdrInfo%DataType(1) = DType
      
      !
      !  Line 3
      !
      CALL ParseHdrLineDataUnit(TRIM(FileName), HLines(3), 3, DUnit); IF (ErrorLevel .NE. 0) GOTO 898
      IF (DUnit .EQ. GDU_Undefined) GOTO 403
      ALLOCATE(HdrInfo%DataUnit(1), STAT=IOS)
      HdrInfo%DataUnit(1) = DUnit

      
      ALLOCATE(HdrInfo%StnName(1), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      J = MIN(LEN_TRIM(CsvStrings(2)), MaxSNL)
      HdrInfo%StnName(1) = TRIM(ADJUSTL(CsvStrings(2)(1:J)))
      S = TRIM(HdrInfo%StnName(1))
      IF (S(1:1) .NE. '"') THEN
         HdrInfo%StnName(1) = TRIM('"' // TRIM(HdrInfo%StnName(1)) // '"')     ! ensure station name is enclosed by double-quotes
      END IF
            
      !
      !  Line 4
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(4), 4, 'Starts(ymd)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%SDate = Seq
            
      !
      !  Line 5
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(5), 5, 'Ends(ymd)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%EDate = Seq
            
      !
      !  Line 6
      !
      CALL ParseCommaSepLine(HLines(6), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 4) GOTO 306
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'lake:') GOTO 336
      HdrInfo%NumDataColumns = NumStrings - 1
      I = HdrInfo%NumDataColumns
      ALLOCATE(HdrInfo%SubbasinID(I), SubLake(I), SubNum(I), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      DO I = 1, HdrInfo%NumDataColumns
         S = ADJUSTL(CsvStrings(I+1))
         IF (LEN_TRIM(S) .GT. 10) GOTO 406
         SubLake(I) = TRIM(S)
      END DO
            
      !
      !  Line 7
      !
      CALL ParseCommaSepLine(HLines(7), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 307
      IF (NumStrings .NE. HdrInfo%NumDataColumns+1) GOTO 407
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'Subbasin:') GOTO 337
      DO I = 1, HdrInfo%NumDataColumns
         S = ADJUSTL(CsvStrings(I+1))
         IF (LEN_TRIM(S) .GT. 10) GOTO 407
         SubNum(I) = TRIM(S)
      END DO
            
      !
      !  Combine lake and subbasin into a subbasin ID.  For example:
      !    'SUP' and '0'     -> 'sup_00'
      !    'Sup' and '17'    -> 'sup_17'
      !    'sUP' and 'Lake'  -> 'sup_lake'
      !    'SuP' and 'LAND'  -> 'sup_land'
      !
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(GetLowercase(ADJUSTL(SubNum(I))))
         READ(TStr, *, IOSTAT=IOS) J
         IF (IOS .EQ. 0) THEN
            WRITE(TStr, '(I2.2)') J
         END IF
         HdrInfo%SubbasinID(I) = TRIM(SubLake(I)) // '_' // TRIM(TStr)
      END DO
      DEALLOCATE(SubLake, SubNum, STAT=IOS)
            
      !
      !  Line 8
      !
      ALLOCATE(HdrInfo%SubArea(I), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      CALL ParseCommaSepLine(HLines(8), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 308
      IF (NumStrings .NE. HdrInfo%NumDataColumns+1) GOTO 408
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'area:') GOTO 338
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         READ(TStr, *, ERR=408) HdrInfo%SubArea(I)
      END DO

      !
      !  Line 9
      !
      CALL ParseCommaSepLine(HLines(9), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 1) GOTO 309
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'yyyy-mm-dd') GOTO 339
      
      !
      !  Do not just return. Go to 999 for cleanup of
      !  allocated arrays, if needed.
      HdrInfo%InfoIsValid = .TRUE.
      
      GOTO 999
      
      !
      !  Errors messages
      !      
  301 ErrorMessage = 'Formatting error on line 1 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  306 ErrorMessage = 'Formatting error on line 6 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  307 ErrorMessage = 'Formatting error on line 7 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  308 ErrorMessage = 'Formatting error on line 8 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  309 ErrorMessage = 'Formatting error on line 9 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  336 ErrorMessage = 'Error with label string on line 6 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  337 ErrorMessage = 'Error with label string on line 7 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  338 ErrorMessage = 'Error with label string on line 8 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  339 ErrorMessage = 'Error with label string on line 9 in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  401 ErrorMessage = 'Invalid entry on line 1 of '                  //TRIM(FileName); CALL PassMsg
      ErrorMessage = 'Entry should say "Daily data for multiple stations."';          CALL PassMsg; GOTO 898
  402 ErrorMessage = 'Error parsing line 2 for data type; file '    //TRIM(FileName); CALL PassMsg; GOTO 898
  403 ErrorMessage = 'Error parsing line 2 for data unit; file '    //TRIM(FileName); CALL PassMsg; GOTO 898
  406 ErrorMessage = 'Error parsing line 6 for lake; file '         //TRIM(FileName); CALL PassMsg; GOTO 898
  407 ErrorMessage = 'Error parsing line 7 for subbasin; file '     //TRIM(FileName); CALL PassMsg; GOTO 898
  408 ErrorMessage = 'Error parsing line 8 for area; file '         //TRIM(FileName); CALL PassMsg; GOTO 898

      !
      !  memory allocation error
      !      
  851 ErrorMessage = 'Error allocating memory for temporary storage of header information'; CALL PassMsg
      ErrorMessage = 'while reading '//TRIM(FileName); CALL PassMsg; GOTO 898

    
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ParseMultiSubbasinHeader'; CALL PassMsg
      CALL HdrInfo%Clear()
      
  999 DEALLOCATE(SubLake, SubNum, STAT=IOS)
      RETURN
      
      END SUBROUTINE ParseMultiSubbasinHeader
      

!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Parse the header section for an LBRM output file
      !------------------------------------------------------------
      SUBROUTINE ParseLBRMHeader(Filename, HLines, HdrInfo)
      IMPLICIT NONE
      CHARACTER(LEN=*),                   INTENT(IN)    :: FileName
      CHARACTER(LEN=MaxLL), DIMENSION(:), INTENT(IN)    :: HLines
      TYPE (THeaderInfoType),             INTENT(INOUT) :: HdrInfo
      
      INTEGER :: I, IOS, Sub, LkNum, Seq
      INTEGER :: NumStrings
      CHARACTER(LEN=100) :: S, TStr
      CHARACTER(LEN=100), DIMENSION(5000) :: CsvStrings

      !
      !  line 1
      !
      CALL ParseCommaSepLine(HLines(1), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 1) GOTO 301
      TStr = GetLowercase(TRIM(ADJUSTL(CsvStrings(1))))
      IF (TRIM(TStr) .NE. 'output from lbrm.') GOTO 401
         
      !
      !  Line 2
      !
      CALL ParseCommaSepLine(HLines(2), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 302
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'lake:') GOTO 332
      S = TRIM(ADJUSTL(CsvStrings(2)))
      LkNum = LakeNumberFromName3(S); IF (ErrorLevel .NE. 0) LkNum = -1
      ErrorLevel = 0
      IF (LkNum .LT. 1) THEN
         LkNum = LakeNumberFromName10(S); IF (ErrorLevel .NE. 0) LkNum = -1
         ErrorLevel = 0
      END IF
      IF (LkNum .LT. 1) GOTO 402
      HdrInfo%Bsn = LakeName3(LkNum)     ! save the 3-char lake name
      
      !
      !  Line 3
      !
      CALL ParseCommaSepLine(HLines(3), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 303
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'subbasin:') GOTO 333
      TStr = GetLowercase(TRIM(ADJUSTL(CsvStrings(2))))
      READ(TStr, *, IOSTAT=IOS) Sub
      IF (IOS .NE. 0) GOTO 403
      IF (Sub .LT.                   1) GOTO 403
      IF (Sub .GT. NumSubbasins(LkNum)) GOTO 403
      HdrInfo%SubbasinNumber = Sub
 
      !
      !  Line 4
      !
      CALL ParseCommaSepLine(HLines(4), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 2) GOTO 304
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'area:') GOTO 334
      TStr = TRIM(ADJUSTL(CsvStrings(2)));  READ(TStr, *, ERR=404) HdrInfo%Area

      !
      !  Line 5
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(5), 5, 'Starts(ymd)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%SDate = Seq
           
      !
      !  Line 6
      !
      CALL ParseHdrLineDateField(TRIM(FileName), HLines(6), 6, 'Ends(YMD)', Seq); IF (ErrorLevel .NE. 0) GOTO 898
      HdrInfo%EDate = Seq
            
      !
      !  Line 7 is just an information line, like line 1.
      !
      CALL ParseCommaSepLine(HLines(7), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 1) GOTO 307
      TStr = GetLowercase(TRIM(ADJUSTL(CsvStrings(1))))
      IF (TRIM(TStr) .NE. 'runoff and storages expressed in millimeters over the subbasin.') GOTO 407            

      !
      !  Line 8
      !
      CALL ParseCommaSepLine(HLines(8), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 7) GOTO 308
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'date') GOTO 338
      HdrInfo%NumDataColumns = NumStrings - 1
      I = HdrInfo%NumDataColumns
      ALLOCATE(HdrInfo%DataType(I), HdrInfo%DataUnit(I), STAT=IOS)
      IF (IOS .NE. 0) GOTO 851
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         TStr = TRIM(StripDoubleQuotes(TStr))
         HdrInfo%DataType(I) = GlerlDataTypeFromString(TStr)
         IF (HdrInfo%DataType(I) .EQ. GDT_Undefined) THEN
            ErrorMessage = 'Unrecognized data type string = ' // TRIM(TStr);  CALL PassMsg
            GOTO 408
         END IF
      END DO

      !
      !  Line 9
      !
      CALL ParseCommaSepLine(HLines(9), CsvStrings, NumStrings); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStrings .LT. 3) GOTO 309
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'yyyy-mm-dd') GOTO 339
      I = NumStrings - 1
      IF (I .NE. HdrInfo%NumDataColumns) GOTO 409
      DO I = 1, HdrInfo%NumDataColumns
         TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
         TStr = TRIM(StripDoubleQuotes(TStr))
         HdrInfo%DataUnit(I) = GlerlDataUnitFromString(TStr)
         IF (HdrInfo%DataUnit(I) .EQ. GDU_Undefined) THEN
            ErrorMessage = 'Unrecognized data unit string = ' // TRIM(TStr);  CALL PassMsg
            GOTO 409
         END IF
      END DO

      HdrInfo%InfoIsValid = .TRUE.

      RETURN
      
      !
      !  Errors messages
      !      
  301 ErrorMessage = 'Formatting error on line 1 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  302 ErrorMessage = 'Formatting error on line 2 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  303 ErrorMessage = 'Formatting error on line 3 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  304 ErrorMessage = 'Formatting error on line 4 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  307 ErrorMessage = 'Formatting error on line 7 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  308 ErrorMessage = 'Formatting error on line 8 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  309 ErrorMessage = 'Formatting error on line 9 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  332 ErrorMessage = 'Error with label string on line 2 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  333 ErrorMessage = 'Error with label string on line 3 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  334 ErrorMessage = 'Error with label string on line 4 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  338 ErrorMessage = 'Error with label string on line 8 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  339 ErrorMessage = 'Error with label string on line 9 in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  401 ErrorMessage = 'Invalid entry on line 1 of '             //TRIM(FileName);          CALL PassMsg
      ErrorMessage = 'Entry should say "Output from LBRM."';                              CALL PassMsg; GOTO 898
  402 ErrorMessage = 'Invalid lake named on line 2 in '        //TRIM(FileName);          CALL PassMsg; GOTO 898
  403 ErrorMessage = 'Invalid subbasin on line 3 in '          //TRIM(FileName);          CALL PassMsg; GOTO 898
  404 ErrorMessage = 'Error parsing area on line 4 in '        //TRIM(FileName);          CALL PassMsg; GOTO 898
  407 ErrorMessage = 'Invalid entry on line 7 of '             //TRIM(FileName);          CALL PassMsg
      ErrorMessage = 'Entry should say "Runoff and storages expressed in millimeters over the subbasin."'; CALL PassMsg; GOTO 898
  408 ErrorMessage = 'Error parsing line 8 in '                //TRIM(FileName);          CALL PassMsg; GOTO 898
  409 ErrorMessage = 'Error parsing line 9 in '                //TRIM(FileName);          CALL PassMsg; GOTO 898
  
      !
      !  memory allocation error
      !      
  851 ErrorMessage = 'Error allocating memory for temporary storage of header information'; CALL PassMsg
      ErrorMessage = 'while reading '//TRIM(FileName); CALL PassMsg; GOTO 898

    
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ParseLBRMHeader'; CALL PassMsg
      CALL HdrInfo%Clear()
      RETURN
      
      END SUBROUTINE ParseLBRMHeader

!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Common routines for parsing header section lines
      !------------------------------------------------------------
      SUBROUTINE ParseHdrLineDataType(FileName, Line, LineNum, DType)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: FileName, Line
      INTEGER,          INTENT(IN)  :: LineNum
      INTEGER,          INTENT(OUT) :: DType
      INTEGER :: NumStr
      CHARACTER(LEN=150) :: S, CsvStrings(5000)
      
      CALL ParseCommaSepLine(Line, CsvStrings, NumStr); IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStr .LT. 2) GOTO 801
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'datatype:') GOTO 802
      S = GetLowercase(TRIM(ADJUSTL(CsvStrings(2))))
      DType = GlerlDataTypeFromString(TRIM(S))
      IF (DType .EQ. GDT_Undefined) THEN
         ErrorMessage = 'Unrecognized data type string = ' // TRIM(S);  CALL PassMsg
         GOTO 803
      END IF
      RETURN
      
  801 WRITE(ErrorMessage, 1001) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  802 WRITE(ErrorMessage, 1002) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  803 WRITE(ErrorMessage, 1003) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ParseHdrLineDataType';  CALL PassMsg
      RETURN
      
 1001 FORMAT('Formatting error on line ', I0, ' (too few entries) in ', A)
 1002 FORMAT('Error with label string on line ', I0, ' in ', A)
 1003 FORMAT('Error: invalid data type string on line ', I0, ' in ', A)
      END SUBROUTINE ParseHdrLineDataType

!------------------------------------------------------------------      
      !---------------------------------------------------------------------------
      SUBROUTINE ParseHdrLineDataUnit(FileName, Line, LineNum, DUnit)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: FileName, Line
      INTEGER,          INTENT(IN)  :: LineNum
      INTEGER,          INTENT(OUT) :: DUnit
      INTEGER :: NumStr
      CHARACTER(LEN=150) :: S, CsvStrings(5000)
      
      CALL ParseCommaSepLine(Line, CsvStrings, NumStr)
      IF (NumStr .LT. 2) GOTO 801
      S = GetLowercase(StripAllBlanks(CsvStrings(1)))
      S = TRIM(StripDoubleQuotes(S))
      IF (TRIM(S) .NE. 'dataunits:') GOTO 802
      S = GetLowercase(TRIM(ADJUSTL(CsvStrings(2))))
      DUnit = GlerlDataUnitFromString(TRIM(S))
      IF (DUnit .EQ. GDU_Undefined) THEN
         ErrorMessage = 'Unrecognized data unit string = ' // TRIM(S);  CALL PassMsg
         GOTO 803
      END IF
      RETURN
      
  801 WRITE(ErrorMessage, 1001) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  802 WRITE(ErrorMessage, 1002) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  803 WRITE(ErrorMessage, 1003) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  
  898 ErrorLevel = 1
      RETURN
      
 1001 FORMAT('Formatting error on line ', I0, ' (too few entries) in ', A)
 1002 FORMAT('Error with label string on line ', I0, ' in ', A)
 1003 FORMAT('Error: invalid data unit string on line ', I0, ' in ', A)
      END SUBROUTINE ParseHdrLineDataUnit

!------------------------------------------------------------------      
      !---------------------------------------------------------------------------
      SUBROUTINE ParseHdrLineDateField(FileName, Line, LineNum, IDStr, Seq)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: FileName, Line, IDStr
      INTEGER,          INTENT(IN)  :: LineNum
      INTEGER,          INTENT(OUT) :: Seq
      INTEGER :: NumStr
      LOGICAL :: F
      CHARACTER(LEN=150) :: SF, ST, STC, CsvStrings(5000)
      
      CALL ParseCommaSepLine(Line, CsvStrings, NumStr)
      IF (NumStr .LT. 2) GOTO 801
      SF = GetLowercase(StripAllBlanks(CsvStrings(1)))
      SF = TRIM(StripDoubleQuotes(SF))                  ! string from the file
      ST  = GetLowercase(StripAllBlanks(IDStr))
      ST  = TRIM(StripDoubleQuotes(ST))                 ! target string (no colon) for comparison
      STC = TRIM(ST) // ':'                             ! target string (with colon) for comparison
      F = .FALSE.
      IF (TRIM(SF) .EQ. TRIM(ST))  F = .TRUE.
      IF (TRIM(SF) .EQ. TRIM(STC)) F = .TRUE. 
      IF (.NOT. F) GOTO 802
      Seq = DateStringYMDToSeq(TRIM(ADJUSTL(CsvStrings(2)))); IF (ErrorLevel .NE. 0) GOTO 803
      RETURN
      
  801 WRITE(ErrorMessage, 1001) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  802 WRITE(ErrorMessage, 1002) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  803 WRITE(ErrorMessage, 1003) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  
  898 ErrorLevel = 1
      Seq = MissingData_Date
      RETURN
      
 1001 FORMAT('Formatting error on line ', I0, ' (too few entries) in ', A)
 1002 FORMAT('Error with label string on line ', I0, ' in ', A)
 1003 FORMAT('Error with date string on line ', I0, ' in ', A)
      END SUBROUTINE ParseHdrLineDateField
      
      
!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Determine whether this file is in fixed-format or not.
      !  Call the appropriate read procedure.
      !------------------------------------------------------------
      SUBROUTINE ReadDataSection(FileName, U1, NumHdrLines, SSeq, ESeq, Data2D)
      IMPLICIT NONE
      CHARACTER(LEN=*),     INTENT(IN)    :: FileName
      INTEGER,              INTENT(IN)    :: U1, NumHdrLines, SSeq, ESeq
      REAL, DIMENSION(:,:), INTENT(INOUT) :: Data2D

      INTEGER :: I, IOS
      LOGICAL :: IsFF
      CHARACTER(LEN=MaxLL) :: Line

      !
      !  Read the first line of data so we can parse it.
      !  Then backspace 1 line so we are positioned back at the first data 
      !  line for subsequent reading.
      !
      READ(U1, 1000, IOSTAT=IOS) Line
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error reading first data line in '//TRIM(FileName);  CALL PassMsg
         GOTO 898
      END IF
      BACKSPACE(U1)
      
      !
      !  Parse the line.  Does it have a comma every 11 characters and no other commas?
      !  If so, assume the file is fixed format.
      !
      IsFF = .TRUE.
      DO I = 1, LEN_TRIM(Line)-1
         IF (MOD(I,11) .EQ. 0) THEN
            IF (Line(I:I) .NE. ',') IsFF = .FALSE.
         ELSE
            IF (Line(I:I) .EQ. ',') IsFF = .FALSE.
         END IF
      END DO
      
      IF (IsFF) THEN
         CALL ReadDataSectionFixed(FileName, U1, NumHdrLines, SSeq, ESeq, Data2D)
      ELSE
         CALL ReadDataSectionCSV(FileName, U1, NumHdrLines, SSeq, ESeq, Data2D)
      END IF
      IF (ErrorLevel .NE. 0) GOTO 899
      RETURN
      
      !
      !  Error handling
      !      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ReadDataSection'; CALL PassMsg
      RETURN
      
 1000 FORMAT(A1500000)
      
      END SUBROUTINE ReadDataSection


!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Read the data value section of a CSV file that is set up in fixed-width
      !  format. 
      !  Interpretation of the meaning for each data column will be up to the
      !  calling procedure.
      !
      !  The Data2D array is assumed to already be allocated/sized with
      !  the first index being used for the days (1..numdays) and the second
      !  index being the number of stations, datatypes, or whatever.
      !
      !  The data is read as 1 column of date info (Y-M-D), which is then
      !  converted into a sequence number and assigned into the DateSeqs array.
      !  The rest of the columns will be assigned into the Data2D array, which
      !  is indexed (1..maxrow, 1..maxcol).
      !
      !  Also assumed is that the file pointer is positioned at the correct line,
      !  and we can just start reading data lines (without skipping header lines).
      !------------------------------------------------------------
      SUBROUTINE ReadDataSectionFixed(FileName, U1, NumHdrLines, SSeq, ESeq, Data2D)
      IMPLICIT NONE
      CHARACTER(LEN=*),     INTENT(IN)    :: FileName
      INTEGER,              INTENT(IN)    :: U1, NumHdrLines, SSeq, ESeq
      REAL, DIMENSION(:,:), INTENT(INOUT) :: Data2D
      
      INTEGER :: J, L, Row, Col, LineNum, IOS, IOS2
      INTEGER :: MaxRow, MaxCol, NumCols
      INTEGER :: Seq
      REAL    :: DVal
      CHARACTER(LEN=20) :: TStr, DStr(9999)
      CHARACTER(LEN=40) :: MyFmt
      CHARACTER(LEN=MaxLL) :: Line
      
      MaxRow = UBOUND(Data2D, 1)       ! number of days
      MaxCol = UBOUND(Data2D, 2)       ! number of data types, stations, etc
      
      LineNum = NumHdrLines      
      READ(U1, 1000, IOSTAT=IOS) Line

      !
      !  Construct the format string for reading each data line
      !
      L = LEN_TRIM(Line)
      NumCols = (L - 10) / 11
      WRITE(MyFmt, 1200) NumCols
      
      !
      !  Read and process each line
      !
      DO WHILE (IOS .EQ. 0)
         LineNum = LineNum + 1
         READ(Line, 1101, IOSTAT=IOS2) TStr
         IF (IOS2 .NE. 0) THEN
            WRITE(ErrorMessage, 4000) TRIM(FileName), LineNum;  CALL PassMsg
            GOTO 898
         END IF
         Seq = DateStringYMDToSeq(TRIM(TStr))
         IF ((Seq .GE. SSeq) .AND. (Seq .LE. ESeq)) THEN
            Row = Seq - SSeq + 1
            READ(Line, MyFmt) (DStr(Col), Col=1,NumCols)
            DO Col = 1, NumCols
               READ(DStr(Col), *, IOSTAT=J) DVal
               IF (J .NE. 0) DVal = MissingData_Real
               Data2D(Row,Col) = DVal
            END DO
         END IF
         READ(U1, 1000, IOSTAT=IOS) Line
      END DO
      RETURN
      
      !
      !  Error handling
      !      
  898 ErrorLevel = 1
      ErrorMessage = '[traceback] : ReadDataSectionFixed'; CALL PassMsg
      RETURN
      
 1000 FORMAT(A1500000)
 1101 FORMAT(A10)
 1200 FORMAT('(10X,', I0, '(1X, A10))')
 4000 FORMAT('Error reading ', A ' at line ', I0)
 
      END SUBROUTINE ReadDataSectionFixed
      
!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Read the data value section of any CSV file. They are all the same format.
      !  Interpretation of the meaning for each data column will be up to the
      !  calling procedure.
      !
      !  The Data2D array is assumed to already be allocated/sized with
      !  the first index being used for the days (1..numdays) and the second
      !  index being the number of stations, datatypes, or whatever.
      !
      !  The data is read as 1 column of date info (Y-M-D), which is then
      !  converted into a sequence number and assigned into the DateSeqs array.
      !  The rest of the columns will be assigned into the Data2D array, which
      !  is indexed (1..maxrow, 1..maxcol).
      !
      !  Also assumed is that the file pointer is positioned at the correct line,
      !  and we can just start reading data lines (without skipping header lines).
      !------------------------------------------------------------
      SUBROUTINE ReadDataSectionCSV(FileName, U1, NumHdrLines, SSeq, ESeq, Data2D)
      IMPLICIT NONE
      CHARACTER(LEN=*),     INTENT(IN)    :: FileName
      INTEGER,              INTENT(IN)    :: U1, NumHdrLines, SSeq, ESeq
      REAL, DIMENSION(:,:), INTENT(INOUT) :: Data2D
      
      INTEGER :: J, Row, Col, LineNum, IOS
      INTEGER :: MaxRow, MaxCol, NumStrings
      INTEGER :: Seq
      REAL    :: DVal
      CHARACTER(LEN=100) :: TStr
      CHARACTER(LEN=MaxLL) :: Line
      CHARACTER(LEN=MaxSNL), DIMENSION(MaxNS+1) :: CsvStrings
      
      MaxRow = UBOUND(Data2D, 1)       ! number of days
      MaxCol = UBOUND(Data2D, 2)       ! number of data types, stations, etc
      
      LineNum = NumHdrLines      
      READ(U1, 1000, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         LineNum = LineNum + 1
         CALL ParseCommaSepLine(Line, CsvStrings, NumStrings)
         IF (NumStrings .LT. 2) GOTO 401
         IF (NumStrings .GT. MaxCol+1) GOTO 402
         Seq = DateStringYMDToSeq(TRIM(ADJUSTL(CsvStrings(1))))
         IF ((Seq .GE. SSeq) .AND. (Seq .LE. ESeq)) THEN
            Row = Seq - SSeq + 1
            IF (Row .LE. MaxRow) THEN
               DO Col = 1, NumStrings-1
                  TStr = TRIM(ADJUSTL(CsvStrings(Col+1))); READ(TStr, *, IOSTAT=J) DVal
                  IF (J .NE. 0) DVal = MissingData_Real
                  Data2D(Row,Col) = DVal
               END DO
            ELSE
               WRITE(ErrorMessage, 5001) TRIM(Filename), Row, MaxRow, Seq , SSeq
               CALL PassMsg
               GOTO 898
            END IF
         END IF
         READ(U1, 1000, IOSTAT=IOS) Line
      END DO
      RETURN
      
      !
      !  Error messages
      !      
  401 WRITE(ErrorMessage, 4001) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  402 WRITE(ErrorMessage, 4002) LineNum, TRIM(FileName); CALL PassMsg; GOTO 898
  
  
  898 ErrorLevel = 1
      ErrorMessage = '[traceback] : ReadDataSectionCSV'; CALL PassMsg
      RETURN
      
 1000 FORMAT(A1500000)
 4001 FORMAT('Formatting error on line ', I0, 'of file ', A, '; too few columns.')
 4002 FORMAT('Formatting error on line ', I0, 'of file ', A, '; too many columns.')
 5001 FORMAT('Yikes... File ', A, ';  Row=', I0, ' Max=', I0, ' Seq=', I0, ' SSeq=', I0)
 
      END SUBROUTINE ReadDataSectionCSV
      
      
!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Write the header section of a file. The file should have 
      !  already been opened by the calling procedure and made available
      !  for writing as unit "UNum". This routine will simply write the
      !  appropriate lines to that unit, and return. The calling routine
      !  will then be able to just keep adding lines to the file.
      !
      !  The calling procedure must use the FmtType variable to signal to 
      !  this routine what format is expected/required. 
      !
      !  The HdrInfo variable is an "object", and it is assumed to have been
      !  instantiated and populated by the caller prior to invoking this routine.
      !------------------------------------------------------------
      SUBROUTINE WriteHeaderInfo(FileName, UNum, FmtType, HdrInfo)
      IMPLICIT NONE
      CHARACTER(LEN=*),       INTENT(IN)  :: FileName
      INTEGER,                INTENT(IN)  :: UNum, FmtType
      TYPE (THeaderInfoType), INTENT(IN)  :: HdrInfo
      
      INTEGER :: I, J, IOS, NDC
      LOGICAL :: F
      CHARACTER(LEN=10)  :: CanWrite, DateStr1, DateStr2
      CHARACTER(LEN=100) :: S
      CHARACTER(LEN=10), DIMENSION(:), ALLOCATABLE :: SubLake, SubNum

      !
      !  Verify that UNum has been assigned and is available
      !  for writing.
      !
      INQUIRE(UNIT=UNum, OPENED=F, WRITE=CanWrite)
      IF (.NOT. F) GOTO 811
      CALL Uppercase(CanWrite)
      IF (TRIM(CanWrite) .NE. 'YES') GOTO 812

      !
      !  Verify the header info is valid
      !
      IF (.NOT. HdrInfo%InfoIsValid) THEN
         ErrorMessage = 'Error: Header info reports invalid condition.'; CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Write the header lines
      !
      SELECT CASE (FmtType)
         CASE (HFT_SingleStation)
            WRITE(UNum, 1101, ERR=813)
            WRITE(UNum, 1102, ERR=813) TRIM(HdrInfo%StnID(1))
            WRITE(UNum, 1103, ERR=813) TRIM(HdrInfo%StnName(1))
            WRITE(UNum, 1104, ERR=813) HdrInfo%Latitude(1), HdrInfo%Longitude(1)
            WRITE(UNum, 1105, ERR=813) HdrInfo%Elevation
            WRITE(UNum, 1106, ERR=813) SeqToDateStringYMD(HdrInfo%SDate)
            WRITE(UNum, 1107, ERR=813) SeqToDateStringYMD(HdrInfo%EDate)
            NDC = HdrInfo%NumDataColumns
            WRITE(UNum, 1108, ERR=813, ADVANCE='NO')
            IF (NDC .GT. 0) THEN
               WRITE(UNum, 1120, ERR=813, ADVANCE='NO') (TRIM(GlerlDataTypeString10(HdrInfo%DataType(I))), I=1,NDC-1)
               WRITE(UNum, 1121, ERR=813) TRIM(GlerlDataTypeString10(HdrInfo%DataType(NDC)))
            END IF
            WRITE(UNum, 1109, ERR=813, ADVANCE='NO')
            IF (NDC .GT. 0) THEN
               WRITE(UNum, 1120, ERR=813, ADVANCE='NO') (TRIM(GlerlDataUnitString10(HdrInfo%DataUnit(I))), I=1,NDC-1)
               WRITE(UNum, 1121, ERR=813) TRIM(GlerlDataUnitString10(HdrInfo%DataUnit(NDC)))
            END IF
            
         CASE (HFT_SingleSubbasin)
            WRITE(UNum, 1201, ERR=813)
            WRITE(UNum, 1202, ERR=813) TRIM(HdrInfo%Bsn)
            J = HdrInfo%SubbasinNumber
            IF (J .EQ. 0) THEN
               S = 'lake'
            ELSE IF (J .EQ. 98) THEN
               S = 'land'
            ELSE IF (J .EQ. 99) THEN
               S = 'basin'
            ELSE 
               WRITE(S, '(I0)') J
            END IF
            WRITE(UNum, 1203, ERR=813) TRIM(S)
            WRITE(UNum, 1204, ERR=813) HdrInfo%Area
            WRITE(UNum, 1205, ERR=813) SeqToDateStringYMD(HdrInfo%SDate)
            WRITE(UNum, 1206, ERR=813) SeqToDateStringYMD(HdrInfo%EDate)
            WRITE(UNum, 1207, ERR=813) SeqToDateStringYMD(HdrInfo%NewDataDate)
            NDC = HdrInfo%NumDataColumns
            WRITE(UNum, 1208, ERR=813, ADVANCE='NO') (TRIM(GlerlDataTypeString10(HdrInfo%DataType(I))), I=1,NDC-1)
            WRITE(UNum, 1220, ERR=813) TRIM(GlerlDataTypeString10(HdrInfo%DataType(NDC)))
            WRITE(UNum, 1209, ERR=813, ADVANCE='NO') (TRIM(GlerlDataUnitString10(HdrInfo%DataUnit(I))), I=1,NDC-1)
            WRITE(UNum, 1220, ERR=813) TRIM(GlerlDataUnitString10(HdrInfo%DataUnit(NDC)))

         CASE (HFT_SingleLake)
            WRITE(UNum, 1301, ERR=813)
            WRITE(UNum, 1302, ERR=813) TRIM(HdrInfo%Bsn)
            WRITE(UNum,    *, ERR=813) '[unused line that must be here for correct alignment]'
            WRITE(UNum, 1304, ERR=813) HdrInfo%Area
            WRITE(UNum, 1305, ERR=813) SeqToDateStringYMD(HdrInfo%SDate)
            WRITE(UNum, 1306, ERR=813) SeqToDateStringYMD(HdrInfo%EDate)
            WRITE(UNum,    *, ERR=813) '[unused line that must be here for correct alignment]'
            NDC = HdrInfo%NumDataColumns
            WRITE(UNum, 1308, ERR=813, ADVANCE='NO') (TRIM(GlerlDataTypeString10(HdrInfo%DataType(I))), I=1,NDC-1)
            WRITE(UNum, 1320, ERR=813) TRIM(GlerlDataTypeString10(HdrInfo%DataType(NDC)))
            WRITE(UNum, 1309, ERR=813, ADVANCE='NO') (TRIM(GlerlDataUnitString10(HdrInfo%DataUnit(I))), I=1,NDC-1)
            WRITE(UNum, 1320, ERR=813) TRIM(GlerlDataUnitString10(HdrInfo%DataUnit(NDC)))
            
         CASE (HFT_MultiStation)
            WRITE(UNum, 1401, ERR=813)
            WRITE(UNum, 1402, ERR=813) TRIM(GlerlDataTypeString(HdrInfo%DataType(1)))
            WRITE(UNum, 1403, ERR=813) TRIM(GlerlDataUnitString(HdrInfo%DataUnit(1)))
            DateStr1 = SeqToDateStringYMD(HdrInfo%SDate);       IF (ErrorLevel .NE. 0) GOTO 899
            DateStr2 = SeqToDateStringYMD(HdrInfo%NewDataDate); IF (ErrorLevel .NE. 0) GOTO 899
            WRITE(UNum, 1404, ERR=813) TRIM(DateStr1), TRIM(DateStr2)
            DateStr1 = SeqToDateStringYMD(HdrInfo%EDate);       IF (ErrorLevel .NE. 0) GOTO 899
            WRITE(UNum, 1405, ERR=813) TRIM(DateStr1)
            NDC = HdrInfo%NumDataColumns
            WRITE(UNum, 1406, ERR=813, ADVANCE='NO') (TRIM(HdrInfo%StnID(I)),   I=1,NDC-1)
            WRITE(UNum, 1420, ERR=813) TRIM(HdrInfo%StnID(NDC))
            WRITE(UNum, 1407, ERR=813, ADVANCE='NO') (TRIM(HdrInfo%StnName(I)), I=1,NDC-1)
            WRITE(UNum, 1420, ERR=813) TRIM(HdrInfo%StnName(NDC))
            WRITE(UNum, 1408, ERR=813, ADVANCE='NO') (HdrInfo%Latitude(I),  I=1,NDC-1)
            WRITE(UNum, 1421, ERR=813) HdrInfo%Latitude(NDC)
            WRITE(UNum, 1409, ERR=813, ADVANCE='NO') (HdrInfo%Longitude(I), I=1,NDC-1)
            WRITE(UNum, 1421, ERR=813) HdrInfo%Longitude(NDC)
            WRITE(UNum, 1410, ERR=813)
            
         CASE (HFT_MultiSubbasin)
            WRITE(UNum, 1501, ERR=813)
            WRITE(UNum, 1502, ERR=813) TRIM(GlerlDataTypeString(HdrInfo%DataType(1)))
            WRITE(UNum, 1503, ERR=813) TRIM(GlerlDataUnitString(HdrInfo%DataUnit(1)))
            WRITE(UNum, 1504, ERR=813) SeqToDateStringYMD(HdrInfo%SDate)
            WRITE(UNum, 1505, ERR=813) SeqToDateStringYMD(HdrInfo%EDate)
            
            I = HdrInfo%NumDataColumns
            ALLOCATE(SubLake(I), SubNum(I), STAT=IOS)
            IF (IOS .NE. 0) GOTO 851
            DO I = 1, HdrInfo%NumDataColumns
               S = HdrInfo%SubbasinID(I)
               J = INDEX(S, '_')
               IF (J .EQ. 0) THEN
                  SubLake(I) = 'xxx'
                  SubNum(I)  = 'xx'
               ELSE
                  SubLake(I) = TRIM(S(1:J-1))
                  SubNum(I)  = TRIM(S(J+1:))
               END IF
            END DO
            NDC = HdrInfo%NumDataColumns
            WRITE(UNum, 1506, ERR=813, ADVANCE='NO') (TRIM(SubLake(I)), I=1,NDC-1)
            WRITE(UNum, 1520, ERR=813) TRIM(SubLake(NDC))
            WRITE(UNum, 1507, ERR=813, ADVANCE='NO') (TRIM(SubNum(I)),  I=1,NDC-1)
            WRITE(UNum, 1520, ERR=813) TRIM(SubNum(NDC))
            DEALLOCATE(SubLake, SubNum, STAT=IOS) 
            WRITE(UNum, 1508, ERR=813, ADVANCE='NO') (HdrInfo%SubArea(I),  I=1,NDC-1)
            WRITE(UNum, 1521, ERR=813) HdrInfo%SubArea(NDC)
            WRITE(UNum, 1509, ERR=813)
            
         CASE (HFT_LBRM)
            WRITE(UNum, 1601, ERR=813)
            WRITE(UNum, 1602, ERR=813) TRIM(HdrInfo%Bsn)
            WRITE(UNum, 1603, ERR=813) HdrInfo%SubbasinNumber
            WRITE(UNum, 1604, ERR=813) HdrInfo%Area
            WRITE(UNum, 1605, ERR=813) SeqToDateStringYMD(HdrInfo%SDate)
            WRITE(UNum, 1606, ERR=813) SeqToDateStringYMD(HdrInfo%EDate)
            WRITE(UNum, 1607, ERR=813)
            NDC = HdrInfo%NumDataColumns
            WRITE(UNum, 1608, ERR=813, ADVANCE='NO') (TRIM(GlerlDataTypeString10(HdrInfo%DataType(I))), I=1,NDC-1)
            WRITE(UNum, 1620, ERR=813) TRIM(GlerlDataTypeString(HdrInfo%DataType(NDC)))
            WRITE(UNum, 1609, ERR=813, ADVANCE='NO') (TRIM(GlerlDataUnitString10(HdrInfo%DataUnit(I))), I=1,NDC-1)
            WRITE(UNum, 1620, ERR=813) TRIM(GlerlDataUnitString(HdrInfo%DataUnit(NDC)))

      END SELECT

      
      GOTO 999
      
  
      !
      !
  811 ErrorMessage = 'Error - output file not opened: '//TRIM(FileName);                CALL PassMsg; GOTO 898
  812 ErrorMessage = 'Error - output file not available for writing: '//TRIM(FileName); CALL PassMsg; GOTO 898   
  813 ErrorMessage = 'Error writing to file '//TRIM(FileName);                          CALL PassMsg; GOTO 898   
  
      !
      !  memory allocation error
      !      
  851 ErrorMessage = 'Error allocating memory for temporary storage of header information'; CALL PassMsg
      ErrorMessage = 'while reading '//TRIM(FileName); CALL PassMsg; GOTO 898
    
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : WriteHeaderInfo...'; CALL PassMsg
      
  999 DEALLOCATE(SubLake, SubNum, STAT=IOS)
      RETURN
  
       !
       !  Format statements
       !
  1101 FORMAT('Daily data for a single station.')
  1102 FORMAT('StationID:  ,', A)
  1103 FORMAT('StationName:,', A)
  1104 FORMAT('Lat&Long:   ', 2(',', F10.3))
  1105 FORMAT('Elevation:  ,', F10.3)
  1106 FORMAT('Starts(YMD):,', A10)
  1107 FORMAT('Ends(YMD):  ,', A10)
  1108 FORMAT('Date      ')
  1109 FORMAT('YYYY-MM-DD')
  1120 FORMAT(9999(',', A10))
  1121 FORMAT(A10)

  1201 FORMAT('Daily data for a single subbasin.')
  1202 FORMAT('Lake:    ,', A)
  1203 FORMAT('Subbasin:,', A)
  1204 FORMAT('Area:    ,', E13.5E2)
  1205 FORMAT('Starts(YMD):,', A10)
  1206 FORMAT('Ends(YMD):  ,',   A10)
  1207 FORMAT('NewDataStart(YMD):,', A10)
  1208 FORMAT('Date      ', 9999(',', A10))
  1209 FORMAT('YYYY-MM-DD', 9999(',', A10))
  1220 FORMAT(A10)
  
  1301 FORMAT('Daily data for a single lake.')
  1302 FORMAT('Lake:,', A)
  1304 FORMAT('Area:,', E13.5E2)
  1305 FORMAT('Starts(YMD):,', A10)
  1306 FORMAT('Ends(YMD):  ,', A10)
  1308 FORMAT('Date      ', 9999(',', A10))
  1309 FORMAT('YYYY-MM-DD', 9999(',', A10))
  1320 FORMAT(A10)
  
  1401 FORMAT('Daily data for multiple stations.')
  1402 FORMAT('DataType: ,', A)
  1403 FORMAT('DataUnits:,', A)
  1404 FORMAT('Starts(YMD):,', A10, ',', 'NewDataStart(YMD):,', A10)
  1405 FORMAT('Ends(YMD):  ,', A10)
  1406 FORMAT('StationID:  ', 9999(',', A))
  1407 FORMAT('StationName:', 9999(',', A))
  1408 FORMAT('Latitude: ', 9999(',', F10.3))
  1409 FORMAT('Longitude:', 9999(',', F10.3))
  1410 FORMAT('YYYY-MM-DD')  
  1420 FORMAT(A)
  1421 FORMAT(F10.3)
  
  1501 FORMAT('Daily data for multiple subbasins.')
  1502 FORMAT('DataType: ,', A)
  1503 FORMAT('DataUnits:,', A)
  1504 FORMAT('Starts(YMD):,', A10)
  1505 FORMAT('Ends(YMD):  ,', A10)
  1506 FORMAT('Lake:     ', 9999(',', A10))
  1507 FORMAT('Subbasin: ', 9999(',', A10))
  1508 FORMAT('Area:     ', 9999(',', F10.3))
  1509 FORMAT('YYYY-MM-DD')  
  1520 FORMAT(A10)
  1521 FORMAT(F10.3)
  
  1601 FORMAT('Output from LBRM.')
  1602 FORMAT('Lake:    ,', A)
  1603 FORMAT('Subbasin:,', I0)
  1604 FORMAT('Area:    ,', E13.5E2)
  1605 FORMAT('Starts(YMD):,', A10)
  1606 FORMAT('Ends(YMD):  ,', A10)
  1607 FORMAT('Runoff and storages expressed in millimeters over the subbasin.')
  1608 FORMAT('Date      ', 9999(',', A10))
  1609 FORMAT('YYYY-MM-DD', 9999(',', A10))
  1620 FORMAT(A10)
  
   
      END SUBROUTINE WriteHeaderInfo

      
!------------------------------------------------------------------      
      !------------------------------------------------------------
      !  Write the data value section of any CSV file. They are all the same format.
      !  Interpretation of the meaning for each data column will be up to the
      !  calling procedure.
      !
      !  The data is written as 3 columns of date info (Y, M, D), followed by
      !  N columns of data.  
      !
      !  It is assumed that the file pointer is positioned at the correct 
      !  line, after the header, and we can just start writing data lines.
      !
      !  Note on data value formatting:
      !  The Fortran standard has no mechanism for forcing a leading zero upon writing.
      !  For example, with the number 0.74, there is no way for me to force output
      !  of "0.74". Each compiler can handle it as desired, and some (many?) of them
      !  choose to output just ".74".  Past experience has shown that this can break
      !  some software packages, and we really NEED that leading zero. So I have to do
      !  a bunch of messiness in this code to accomplish that. 
      !
      !  Note that this is a well-known deficiency, with a fair bit of online discussion,
      !  but there seems to be no urgency or consensus on the standards committee or
      !  the compiler developers to address it. "Working as intended" seems to be the
      !  basic response. Annoying.
      !
      !  Note that each data column is now using a fixed width of 10 characters. 
      !  Fixed width is chosen so that I can use read procedures 
      !  that assume fixed width rather than comma-separated data. That is MUCH
      !  faster. The files will still contain the commas so that Excel or other
      !  software can easily import the data. 10 characters is kinda wide, but it
      !  allows for alignment with the headers (data types and units).  This change
      !  was made by Tim Hunter on 2017Oct18.
      !------------------------------------------------------------
      SUBROUTINE WriteDataSection(FileName, U1, SSeq, ESeq, Data2D)
      IMPLICIT NONE
      CHARACTER(LEN=*),     INTENT(IN) :: FileName
      INTEGER,              INTENT(IN) :: U1, SSeq, ESeq
      REAL, DIMENSION(:,:), INTENT(IN) :: Data2D                       !  indexed (Row, Col)
      
      INTEGER :: L, R, C
      INTEGER :: MaxRow, MaxCol, NumDays
      INTEGER :: Seq
      REAL    :: DVal
      CHARACTER(LEN=10) :: DS
      CHARACTER(LEN=20) :: S
      CHARACTER(LEN=10), PARAMETER :: Blanks = '          '
      
      MaxRow = UBOUND(Data2D, 1)      ! number of days
      MaxCol = UBOUND(Data2D, 2)      ! number of data values for each day

      !
      !  Are the dates valid?
      !  
      IF (SSeq .EQ. MissingData_Date) GOTO 501
      IF (ESeq .EQ. MissingData_Date) GOTO 502
      IF (ESeq .LT. SSeq)             GOTO 503
      NumDays = ESeq - SSeq + 1
      
      !
      !  Check agreement between dates and array.
      !  It is ok if the array is larger than the number of days specified 
      !  by the array, but not if it is smaller.
      !
      !  Assumption is that day 1 is the first row in the array.
      !
      IF (MaxRow .LT. NumDays) GOTO 504

      !
      !  Loop to write rows.
      !  Rather than using a simple write statement or two, I use the 
      !  ADVANCE='NO' option to append one item at a time in order to 
      !  force those leading zeroes as well as handle missing data 
      !  values. Ugly and less efficient, but necessary.
      !  Note that my first inclination was to build a big string, 
      !  but that meant using a very large string variable
      !  (e.g. CHARACTER(LEN=1500000)), and the required TRIM()
      !  statements took forever in testing. So I fell back to this method.
      !
      DO Seq = SSeq, ESeq
         DS = SeqToDateStringYMD(Seq); IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(U1, 1000, ERR=813, ADVANCE='NO') DS
         R = Seq - SSeq + 1
         DO C = 1, MaxCol
            DVal = Data2D(R,C)
            IF (IsMissing(DVal)) THEN
               S = TRIM(FixedMissingValueString)
            ELSE
               IF (DVal .LE. 9.999e5) THEN
                  WRITE(S, 1010) DVal           ! floating-point format  (123456.89)
               ELSE
                  WRITE(S, 1011) DVal           ! scientific notation    (12.45e+89)
               END IF
               IF (S(1:1) .EQ. '.') S = '0' // TRIM(ADJUSTL(S))
            END IF
            L = 10 - LEN_TRIM(S)
            S = ',' // Blanks(1:L) // TRIM(S)
            IF (C .LT. MaxCol) THEN
               WRITE(U1, 1001, ERR=813, ADVANCE='NO') TRIM(S)
            ELSE
               WRITE(U1, 1001, ERR=813) TRIM(S)
            END IF
         END DO
      END DO

      RETURN
      
      !
      !  Error messages
      !      
      
  501 WRITE(ErrorMessage, 2001); CALL PassMsg; GOTO 898
  502 WRITE(ErrorMessage, 2002); CALL PassMsg; GOTO 898
  503 WRITE(ErrorMessage, 2003); CALL PassMsg; GOTO 898
  504 WRITE(ErrorMessage, 2004); CALL PassMsg
      WRITE(ErrorMessage, 2005) MaxRow, NumDays; CALL PassMsg; GOTO 898
      
  813 ErrorMessage = 'Error writing to output file '//TRIM(FileName); CALL PassMsg; GOTO 898
  
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : WriteDataSection'; CALL PassMsg
      RETURN

      !
      !  Formats
      !      
 1000 FORMAT(A10)
 1001 FORMAT(A11)
 1010 FORMAT(F9.2)
 1011 FORMAT(E9.2E2)
 
 2001 FORMAT('Error: Missing start date in WriteDataSection().')
 2002 FORMAT('Error: Missing end date in WriteDataSection().')
 2003 FORMAT('Error: Start date is later than end date in WriteDataSection().')
 2004 FORMAT('Error: Mismatch between dates and array size.  ')
 2005 FORMAT('       NumDays=', I0, ' but array only has ', I0, ' entries.')

      END SUBROUTINE WriteDataSection
      
      
!------------------------------------------------------------
      SUBROUTINE ReadFile_DailyStation(FileName, StnData)
      IMPLICIT NONE
      CHARACTER(LEN=*),      INTENT(IN)    :: FileName
      TYPE (TDlyDataMetStn), INTENT(INOUT) :: StnData
      
      INTEGER :: I, IOS, U1
      INTEGER :: NumDT
      INTEGER :: SSeq, ESeq
      LOGICAL :: F
      REAL, DIMENSION(:,:), ALLOCATABLE :: Temp2D     ! (1..NumDays, 1..NumDataTypes)
      TYPE (THeaderInfoType)    :: HdrInfo
      TYPE (TDlyData)           :: TDD
      
      !
      !  Initialize stuff
      !
      U1 = 0
      HdrInfo = THeaderInfoType()
      TDD = TDlyData()
      CALL StnData%Clear()

      !
      !  Open the input file so that we can call the ReadHeaderInfo routine.
      !
      U1 = GetFreeUnitNumber();  IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)

      !
      !  Use the routine for reading the station header info.
      !  Slightly less efficient (computationally) than repeating all of that
      !  code logic here, but better for maintenance, etc.
      !  Then, after we open the file (again), we can just skip over the
      !  header lines.
      !
      CALL ReadHeaderInfo(FileName, U1, HFT_SingleStation, HdrInfo);  IF (ErrorLevel .NE. 0) GOTO 899
      IF (.NOT. HdrInfo%InfoIsValid) THEN
         ErrorMessage = 'Error in status of header info.';  CALL PassMsg
         GOTO 898
      END IF

      !
      !  Assign relevant metadata items.
      !  The members are fully visible within the module, so we 
      !  can do this the simple way.
      !
      !  Note that we DO have the data type and data units at this point, but we will
      !  hold off on using them until we read the data values and assign those into
      !  the StnData object.
      !
      StnData%StnID     = TRIM(HdrInfo%StnID(1))
      StnData%StnName   = TRIM(HdrInfo%StnName(1))
      StnData%Latitude  = HdrInfo%Latitude(1)
      StnData%Longitude = HdrInfo%Longitude(1)
      StnData%Elevation = -9999.                  ! not currently used
      StnData%SDateSeq  = HdrInfo%SDate
      StnData%EDateSeq  = HdrInfo%EDate
      StnData%NumDays   = HdrInfo%NumDays
      
      !
      !  If we determined that there were zero days of data, based on the header info, then
      !  no need to do any of the data value stuff. 
      !
      IF (StnData%NumDays .EQ. 0) GOTO 999
      
      IF (StnData%EDateSeq - StnData%SDateSeq + 1 .NE. StnData%NumDays) THEN
         ErrorMessage = 'Mismatch in date extent sizing from header info.'; CALL PassMsg
         ErrorMessage = 'File='//TRIM(FileName);  CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Assign a few local variables that we need for processing the data
      !  lines. We can get them from the info already read into StnData.
      !
      NumDT = HdrInfo%NumDataColumns
      
      !
      !  Now set up the temporary array that is used for efficiency while reading the data values.
      !
      ALLOCATE(Temp2D(StnData%NumDays, NumDT), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for storage of all daily data from: '//TRIM(FileName)
         CALL PassMsg; GOTO 898
      END IF
      Temp2D(:,:) = MissingData_Real
      
      !
      !  Now read the data section of the file, storing the information in Temp2D.
      !
      SSeq = HdrInfo%SDate
      ESeq = HdrInfo%EDate
      CALL ReadDataSection(FileName, U1, HdrInfo%NumLines, SSeq, ESeq, Temp2D);  IF (ErrorLevel .NE. 0) GOTO 899

      CLOSE(U1)
      CALL FileWasClosed(U1)      

      !
      !  Now add the data to StnData.
      !
      CALL TDD%Clear()
      DO I = 1, HdrInfo%NumDataColumns
         F = TDD%SetDataType(HdrInfo%DataType(I));                             IF (.NOT. F) GOTO 899
         F = TDD%SetDataUnit(HdrInfo%DataUnit(I));                             IF (.NOT. F) GOTO 899
         F = TDD%AssignData(StnData%SDateSeq, StnData%EDateSeq, Temp2D(:,I));  IF (.NOT. F) GOTO 899
         F = StnData%AddDataset(TDD);                                          IF (.NOT. F) GOTO 899
         CALL TDD%Clear()
      END DO

      GOTO 999
     
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file '//TRIM(FileName); CALL PassMsg; GOTO 898
   
    
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ReadFile_DailyStation'; CALL PassMsg
      CALL StnData%Clear()

  999 DEALLOCATE(Temp2D, STAT=IOS)
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
      RETURN

      END SUBROUTINE ReadFile_DailyStation

      
!------------------------------------------------------------------      
      !-----------------------------------------------------------------------------
      !  Subroutine that will accept a filename (for the output file) and a structured
      !  set of station data, then output a comma-separated file of the station data.
      !-----------------------------------------------------------------------------
      SUBROUTINE WriteFile_DailyStation(FileName, StnData)
      IMPLICIT NONE
      CHARACTER(LEN=*),      INTENT(IN) :: FileName
      TYPE (TDlyDataMetStn), INTENT(IN) :: StnData
      
      INTEGER :: I, J, U1, IOS, Seq
      TYPE (THeaderInfoType) :: HdrInfo
      TYPE (TDlyData), POINTER :: TDDP
      REAL, DIMENSION(:,:), ALLOCATABLE :: Temp2D       ! (1..numdays, 1..datatypes)

      U1 = 0
      
      !
      !  Extract relevant header info from StnData
      !
      HdrInfo = THeaderInfoType()
      HdrInfo%FormatType     = HFT_SingleStation
      HdrInfo%SDate          = StnData%SDateSeq
      HdrInfo%EDate          = StnData%EDateSeq
      HdrInfo%NumDays        = StnData%NumDays
      HdrInfo%Elevation      = StnData%Elevation
      HdrInfo%NumDataColumns = StnData%NumDatasets
      
      ALLOCATE(HdrInfo%StnID(1), HdrInfo%StnName(1),                   &
               HdrInfo%Latitude(1), HdrInfo%Longitude(1),              &
               HdrInfo%DataType(HdrInfo%NumDataColumns),               &
               HdrInfo%DataUnit(HdrInfo%NumDataColumns),               &
               STAT=IOS)
      IF (IOS .NE. 0) GOTO 875

      HdrInfo%StnID(1)     = TRIM(StnData%StnID)
      HdrInfo%StnName(1)   = TRIM(StnData%StnName)
      HdrInfo%Latitude(1)  = StnData%Latitude
      HdrInfo%Longitude(1) = StnData%Longitude

      DO I = 1, HdrInfo%NumDataColumns
         TDDP => StnData%GetPointerToDataByIndex(I); IF (Errorlevel .NE. 0) GOTO 899
         HdrInfo%DataType(I) = TDDP%GetDataType()
         HdrInfo%DataUnit(I) = TDDP%GetDataUnit()
      END DO
      HdrInfo%InfoIsValid = .TRUE.

  
      !
      !  Open the output file and write the header info into it.
      !  Note that the subroutine just writes the relevant lines. 
      !  It does not open or close the file.
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)
      CALL WriteHeaderInfo(FileName, U1, HFT_SingleStation, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Create a 2D array for the data values. Assign the values into it.
      !
      ALLOCATE(Temp2D(HdrInfo%NumDays, HdrInfo%NumDataColumns), STAT=IOS)
      IF (IOS .NE. 0) GOTO 875

      IF ((StnData%SDateSeq .NE. MissingData_Date) .AND. (StnData%EDateSeq .NE. MissingData_Date)) THEN
         DO J = 1, StnData%NumDatasets
            TDDP => StnData%GetPointerToDataByIndex(J)
            DO Seq = StnData%SDateSeq, StnData%EDateSeq
               I = Seq - StnData%SDateSeq + 1
               Temp2D(I,J) = TDDP%GetDataVal(I)
            END DO
         END DO
      END IF

      !
      !  Write the data section
      !
      CALL WriteDataSection(FileName, U1, HdrInfo%SDate, HdrInfo%EDate, Temp2D)
      IF (ErrorLevel .NE. 0) GOTO 899
      
      CLOSE(U1)
      CALL FileWasClosed(U1)
      GOTO 999
   
      !
      !
  811 ErrorMessage = 'Error opening output file '//TRIM(FileName); CALL PassMsg; GOTO 899

  875 ErrorMessage = 'Error allocating memory for temporary storage.'; CALL PassMsg; GOTO 899
  
  899 ErrorMessage = '[traceback] : WriteFile_DailyStation...'; CALL PassMsg
      ErrorLevel = 1
      GOTO 999
      
      
  999 CALL HdrInfo%Clear()
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
      DEALLOCATE(Temp2D, STAT=IOS)
      RETURN
 
      END SUBROUTINE WriteFile_DailyStation


!------------------------------------------------------------------      
      !----------------------------------------------------------------------------------
      !  This routine reads a CSV file that contains all of the daily station data 
      !  for a single data type. Typically, the file will have the data for a single lake 
      !  basin or similar area.
      !
      !  The data structure passed in is of type TDlyDataForMultipleStations. Each member
      !  of that object is a TDlyDataMetStn object. Each of those, in turn, will contain
      !  ONLY ONE DATA TYPE for purposes of this routine.
      !----------------------------------------------------------------------------------
      SUBROUTINE ReadFile_OneDataTypeManyStations(Filename, TDDMS)
      IMPLICIT NONE
      CHARACTER (LEN=*),                  INTENT(IN)    :: Filename
      TYPE (TDlyDataForMultipleStations), INTENT(INOUT) :: TDDMS
      
      INTEGER :: IOS, U1, S
      INTEGER :: SSeq, ESeq
      LOGICAL :: OK, Flag
      TYPE (TDlyDataMetStn)          :: DMS
      TYPE (TDlyData)                :: TDD
      TYPE (THeaderInfoType)         :: HdrInfo
      
      REAL, DIMENSION(:,:), ALLOCATABLE :: Temp2D     ! indexed (1:NumDays, 1:NumStations)

      !
      !  Instantiate the objects. This is necessary.
      !  If I don't do it here, and the execution path ends up never executing
      !  the code for instantiation, then upon exit from this routine the FINAL
      !  routine for the objects (called when it goes out of scope) will do
      !  bad things. This seems to me like a bug in the compiler, but I have
      !  learned by painful experience/trial/error that it's how things work
      !  with my current Fortran compiler (gfortran 5.3.0).
      !  
      DMS     = TDlyDataMetStn()
      TDD     = TDlyData()
      HdrInfo = THeaderInfoType()
      
      !
      !  Open the input file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Read the header section
      !  Leave file pointer positioned at the line following the header
      !
      CALL ReadHeaderInfo(TRIM(FileName), U1, HFT_MultiStation, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
      IF (.NOT. HdrInfo%InfoIsValid) THEN
         ErrorMessage = 'Error in status of header info.';  CALL PassMsg
         GOTO 898
      END IF

      !
      !  If we determined that there were zero days of data, based on the header info, then
      !  no need to do any of the data value stuff. 
      !
      IF (HdrInfo%NumDays .EQ. 0) GOTO 999
      
      !
      !  Clear the data object. It is assumed that the object was instantiated
      !  before this routine was called.
      !
      CALL TDDMS%Clear()

      !
      !  Now set up the temporary array that is used for storage while reading the data values.
      !
      ALLOCATE(Temp2D(HdrInfo%NumDays, HdrInfo%NumDataColumns), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for storage of all daily data from: '//TRIM(FileName)
         CALL PassMsg; GOTO 898
      END IF
      Temp2D(:,:) = MissingData_Real
      
      !
      !  Now read the data section of the file, storing the information in Temp2D.
      !
      SSeq = HdrInfo%SDate
      ESeq = HdrInfo%EDate
      CALL ReadDataSection(FileName, U1, HdrInfo%NumLines, SSeq, ESeq, Temp2D); IF (ErrorLevel .NE. 0) GOTO 899
      
      CLOSE(U1)
      CALL FileWasClosed(U1)      

      !
      !  Add each station to the big object. 
      !    Use the TDlyDataMetStn object that we already instantiated at the top
      !    of this routine. It is named DMS.
      !     1) Populate that DMS object with station metadata
      !     2) Populate a TDlyData object with the data for that station
      !     3) Add the TDD object to the DMS object, as it's only dataset
      !     4) Add the DMS object to the big collection of stations
      !
      !  Note that in steps 3 and 4, the addition of the local object is 
      !  actually accomplished by the creation of a new object in the
      !  target object, and then copying all of the data.
      !  
      !  i.e. When adding the TDD object to the DMS object, a new TDlyData
      !       object is created, and then the data from TDD is copied to that new one.
      !       When adding the DMS object to the TDDMS object, a new TDlyDataMetStn
      !       object is created, and then the data from DMS is copied to that new one.
      !  In both cases, by calling the Clear() procedure for the local object, any memory
      !  that was allocated locally is released, and we get a clean slate to work with.
      !
      TDDMS%NewDataStart = MissingData_Date
      DO S = 1, HdrInfo%NumDataColumns
         CALL DMS%Clear()
         DMS%StnID     = TRIM(HdrInfo%StnID(S))
         DMS%StnName   = TRIM(HdrInfo%StnName(S))
         DMS%Latitude  = HdrInfo%Latitude(S)
         DMS%Longitude = HdrInfo%Longitude(S)
         CALL TDD%Clear()
         OK = TDD%SetDataType(HdrInfo%DataType(1));      IF (.NOT. OK) GOTO 899
         OK = TDD%SetDataUnit(HdrInfo%DataUnit(1));      IF (.NOT. OK) GOTO 899
         OK = TDD%AssignData(SSeq, ESeq, Temp2D(:,S))
         IF ((ErrorLevel .NE. 0) .OR. (.NOT. OK)) GOTO 899
         OK = DMS%AddDataset(TDD);          IF (.NOT. OK) GOTO 899
         Flag = TDDMS%AddStation(DMS);      IF (ErrorLevel .NE. 0) GOTO 899
         IF (.NOT. Flag) GOTO 899
      END DO
      CALL TDD%Clear()

      !
      !  Assign the NewDataStart field with the value read from the header info. 
      !  The loop we just finished will have set it based on the data in the file as
      !  a consequence of using the AddStation() routine, but that is actually NOT
      !  what we need to do in this case. Instead, we need to just assign it from the
      !  header info.
      !
      TDDMS%NewDataStart = HdrInfo%NewDataDate
      
      !
      !  Final cleanup of local stuff
      !
      GOTO 999 

      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file '//TRIM(Filename); CALL PassMsg; GOTO 898
      
      

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ReadFile_OneDataTypeManyStations()...'; CALL PassMsg
      CALL TDDMS%Clear()

  999 DEALLOCATE(Temp2D,  STAT=IOS)
      CALL TDD%Clear()
      CALL DMS%Clear()
      CALL HdrInfo%Clear()
      INQUIRE(FILE=TRIM(FileName), OPENED=Flag, NUMBER=U1)
      IF (Flag) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
      RETURN

      END SUBROUTINE ReadFile_OneDataTypeManyStations
      
!------------------------------------------------------------------      
      !----------------------------------------------------------------------------------
      !  This routine writes a CSV file that contains all of the daily station data 
      !  for a single data type. Typically, the file will have the data for a single lake 
      !  basin or similar area.
      !
      !  The data structure passed in is of type TDlyDataForMultipleStations. Each member
      !  of that object is a TDlyDataMetStn object. The specified data type, from each
      !  of the TDlyDataMetStn object that contains data for that datatype, will be output.
      !----------------------------------------------------------------------------------
      SUBROUTINE WriteFile_OneDataTypeManyStations(Filename, Stations, DType, DUnit)
      IMPLICIT NONE
      CHARACTER (LEN=*),                  INTENT(IN) :: Filename
      TYPE (TDlyDataForMultipleStations), INTENT(IN) :: Stations
      INTEGER,                            INTENT(IN) :: DType, DUnit
      
      INTEGER :: I, J, H, IOS, U1, S
      INTEGER :: SSeq, NumDays
      LOGICAL :: Flag, UseIt
      REAL, DIMENSION(:),   ALLOCATABLE :: TDV
      REAL, DIMENSION(:,:), ALLOCATABLE :: Temp2D     ! indexed (1:NumDays, 1:NumStations)

      TYPE (TDlyDataMetStn), POINTER     :: MSP
      TYPE (TDlyData),       POINTER     :: TDDP
      TYPE (TDlyDataMetStn)              :: TStn
      TYPE (TDlyDataForMultipleStations) :: DataOut
      TYPE (THeaderInfoType)             :: HdrInfo

      !
      !  Instantiate objects to avoid problems with the FINAL routine(s)
      !
      TStn = TDlyDataMetStn()
      HdrInfo = THeaderInfoType()
      DataOut = TDlyDataForMultipleStations()

      !
      !  Populate the relevant fields of HdrInfo in prep for writing the header section.
      !  We only allocate 1 entry for datatype and dataunit because all
      !  stations have the same values for that, and it is only used to
      !  write a single entry in the header.
      !
      HdrInfo%NumDataColumns = Stations%NumStations
      I = Stations%NumStations
      ALLOCATE(HdrInfo%DataType(1), HdrInfo%DataUnit(1),             &
               HdrInfo%StnID(I),    HdrInfo%StnName(I),              &
               HdrInfo%Latitude(I), HdrInfo%Longitude(I), STAT=IOS)
      HdrInfo%DataType(1) = DType
      HdrInfo%DataUnit(1) = DUnit
      
      !
      !  Cycle through the Stations object, copying the relevant
      !  data to the DataOut and HdrInfo objects.
      !  Each entry in DataOut will be a TDlyDataMetStn object that 
      !  contains a single time series of data (for the data type
      !  of interest.)
      !
      H = 0
      DO I = 1, Stations%NumStations
         MSP => Stations%GetStationPtrByIndex(I)      ! data for a station
         TDDP => MSP%GetPointerToDataOfType(DType)
         IF (ASSOCIATED(TDDP)) THEN
            CALL TStn%Clear()
            Flag = TStn%CopyMetaFrom(MSP);       IF (.NOT. Flag) GOTO 899
            Flag = TStn%AddDataset(TDDP);        IF (.NOT. Flag) GOTO 899
            H = H + 1
            HdrInfo%StnID(H)     = TRIM(TStn%StnID)
            HdrInfo%StnName(H)   = TRIM(TStn%StnName)
            HdrInfo%Latitude(H)  = TStn%Latitude
            HdrInfo%Longitude(H) = TStn%Longitude
            Flag = DataOut%AddStation(TStn);     IF (.NOT. Flag) GOTO 899
         END IF
      END DO
      HdrInfo%InfoIsValid = .TRUE.
      CALL TStn%Clear()     
      
      !
      !  If no matching data was found, clean up and return.
      !  Do not write a file.
      !
      IF (DataOut%NumStations .EQ. 0) THEN
         ErrorMessage = 'Warning: No relevant station data was found, so no output file is written.'
         CALL PassMsg
         GOTO 999
      END IF
      
      !
      !  Now find the data extents of the DataOut object
      !  
      IF (DataOut%EarliestData .GT. DataOut%LatestData) THEN
         HdrInfo%SDate       = MissingData_Date
         HdrInfo%EDate       = MissingData_Date
         HdrInfo%NewDataDate = MissingData_Date
         NumDays = 0
      ELSE
         HdrInfo%SDate       = DataOut%EarliestData
         HdrInfo%EDate       = DataOut%LatestData
         HdrInfo%NewDataDate = Stations%NewDataStart
         NumDays = HdrInfo%EDate - HdrInfo%SDate + 1
      END IF

      !
      !  Open the output file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Write the header info
      !  Remember that all stations are assumed to have the same data type and units,
      !  so I can get that information from just the first one.
      !
      CALL WriteHeaderInfo(TRIM(FileName), U1, HFT_MultiStation, HdrInfo)
      
      !
      !  Allocate a temporary 2-D data array that will hold a copy of all
      !  the station data values. This may seem a little inefficient, but is
      !  actually better because once we assign the values, we can efficiently
      !  access the data as an array slice for each day.  Much better than
      !  accessing the data objects every day.
      !
      ALLOCATE(Temp2D(NumDays, DataOut%NumStations), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) DataOut%NumStations, NumDays; CALL PassMsg
         GOTO 898
      END IF
      Temp2D(:,:) = MissingData_Real
      
      !
      !  Assign the data from the stations
      !
      SSeq = DataOut%EarliestData            !  assign value to short variable name
      DO S = 1, DataOut%NumStations
         MSP  => DataOut%GetStationPtrByIndex(S)
         TDDP => MSP%GetPointerToDataByIndex(1)
         UseIt = .TRUE.
         IF (TDDP%GetStartDate() .EQ. MissingData_Date)  UseIt = .FALSE.
         IF (TDDP%GetEndDate()   .EQ. MissingData_Date)  UseIt = .FALSE.
         IF (TDDP%GetStartDate() .GT. TDDP%GetEndDate()) UseIt = .FALSE.
         IF (UseIt) THEN
            I = TDDP%GetStartDate() - SSeq + 1
            J = TDDP%GetEndDate()   - SSeq + 1
            TDV = TDDP%GetDataVals()
            Temp2D(I:J, S) = TDV
         END IF
      END DO

      !
      !  Write the data lines
      !
      CALL WriteDataSection(Filename, U1, DataOut%EarliestData, DataOut%LatestData, Temp2D)
      IF (ErrorLevel .NE. 0) GOTO 899
      
      CLOSE(U1)
      CALL FileWasClosed(U1)      
      
      !
      !  Final cleanup of local stuff
      !
      GOTO 999 
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening output file '//TRIM(Filename); CALL PassMsg; GOTO 898

      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] WriteFile_OneDataTypeManyStations()...'; CALL PassMsg
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF

  999 DEALLOCATE(Temp2D, TDV, STAT=IOS)
      CALL DataOut%Clear()
      CALL HdrInfo%Clear()

   
 1071 FORMAT('Error allocating temporary array for ',I0, ' stations and ', I0, ' days.')
      
      END SUBROUTINE WriteFile_OneDataTypeManyStations


!------------------------------------------------------------------      
      !----------------------------------------------------------------------------------
      !  This routine reads a CSV file that contains all of the daily data for a single
      !  subbasin.
      !
      !  The data structure passed in is of type TDlyDataForSubbasin. Each member
      !  of that object is a TDlyData object.
      !----------------------------------------------------------------------------------
      SUBROUTINE ReadFile_OneSubbasin(Filename, SubData)
      IMPLICIT NONE
      CHARACTER (LEN=*),          INTENT(IN)    :: Filename
      TYPE (TDlyDataForSubbasin), INTENT(INOUT) :: SubData
      
      INTEGER :: IOS, U1, C
      INTEGER :: NumDays, SSeq, ESeq
      LOGICAL :: OK
      TYPE (TDlyData)          :: TDD
      TYPE (THeaderInfoType)   :: HdrInfo
      
      REAL, DIMENSION(:,:), ALLOCATABLE :: Temp2D     ! indexed (1:NumDays, 1:NumDataTypes)

      !
      !  Instantiate the objects. This is necessary.
      !  If I don't do it here, and the execution path ends up never executing
      !  the code for instantiation, then upon exit from this routine the FINAL
      !  routine for the objects (called when it goes out of scope) will do
      !  bad things. This seems to me like a bug in the compiler, but I have
      !  learned by painful experience/trial/error that it's how things work
      !  with my current Fortran compiler (gfortran 5.3.0).
      !  
      TDD     = TDlyData()
      HdrInfo = THeaderInfoType()
      
      !
      !  Open the input file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Read the header section  (9 lines)
      !
      CALL ReadHeaderInfo(TRIM(FileName), U1, HFT_SingleSubbasin, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
      IF (.NOT. HdrInfo%InfoIsValid) THEN
         ErrorMessage = 'Error in status of header info.';  CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Clear the data object. It is assumed that the object was instantiated
      !  before this routine was called.
      !
      CALL SubData%Clear()

      !
      !  Assign metadata item(s) from HdrInfo into TDDMS.
      !  The rest of the header info will be used as we build station datasets.
      !
      SubData%Description = 'Daily station data for a single subbasin.'
      SubData%Bsn         = HdrInfo%Bsn 
      SubData%SubNum      = HdrInfo%SubbasinNumber
      SubData%SubArea     = HdrInfo%Area
      SubData%SDateSeq    = HdrInfo%SDate
      SubData%EDateSeq    = HdrInfo%EDate
      SubData%NewDataSSeq = HdrInfo%NewDataDate

      !
      !  Allocate the temporary 2-D data array that will hold the data values
      !  as we read the file. This is far more efficient than using the member
      !  functions of TDlyData to assign the data as it is read.
      !
      NumDays = SubData%EDateSeq - SubData%SDateSeq + 1
      ALLOCATE(Temp2D(NumDays, HdrInfo%NumDataColumns), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) HdrInfo%NumDataColumns, NumDays; CALL PassMsg
         GOTO 898
      END IF
      Temp2D(:,:) = MissingData_Real

      !
      !  Read the data section of the file
      !
      SSeq = SubData%SDateSeq
      ESeq = SubData%EDateSeq
      CALL ReadDataSection(FileName, U1, HdrInfo%NumLines, SSeq, ESeq, Temp2D); IF (ErrorLevel .NE. 0) GOTO 899
      
      CLOSE(U1)
      CALL FileWasClosed(U1)      
      
      !
      !  Add each dataset to the big object. 
      !    Use the TDlyData object that we already instantiated at the top
      !    of this routine. It is named TDD.
      !
      !  Note that when adding the TDD object to the SubData object, a new TDlyData
      !  object is created, and then the data from TDD is copied to that new one.
      !  By calling the Clear() procedure for the local TDD object, any memory
      !  that was allocated locally is released, and we get a clean slate to work with.
      !
      DO C = 1, HdrInfo%NumDataColumns
         CALL TDD%Clear()
         OK = TDD%SetDataType(HdrInfo%DataType(C));    IF (.NOT. OK) GOTO 899
         OK = TDD%SetDataUnit(HdrInfo%DataUnit(C));    IF (.NOT. OK) GOTO 899
         OK = TDD%AssignData(SubData%SDateSeq, SubData%EDateSeq, Temp2D(:,C))
         IF ((ErrorLevel .NE. 0) .OR. (.NOT. OK))      GOTO 899
         OK = SubData%AddDataset(TDD);                 IF (.NOT. OK) GOTO 899
      END DO
      CALL TDD%Clear()

      !
      !  Final cleanup of local stuff
      !
      GOTO 999 
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file '//TRIM(Filename); CALL PassMsg; GOTO 898


  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ReadFile_OneSubbasin()...'; CALL PassMsg
      CALL SubData%Clear()

  999 DEALLOCATE(Temp2D,  STAT=IOS)
      CALL TDD%Clear()
      
      RETURN
      
      !
      !  FORMATs
      !
 1071 FORMAT('Error allocating temporary array for ',I0, ' stations and ', I0, ' days.')

      
      END SUBROUTINE ReadFile_OneSubbasin

!------------------------------------------------------------------      
      !----------------------------------------------------------------------------------
      !  This routine writes a CSV file that contains all of the data for a single subbasin.
      !  Typically, the file will contain AirtempMax, AirtempMin, Precipitation, AirtempMean,
      !  DewpointMean, Windspeed, Cloudcover (or radiation).
      !
      !  The requested time period for the output file is specified by SSeqOut & ESeqOut. 
      !  The SubData object may contain more (or less) data than requested. The actual output
      !  period will be the maximum possible dataset within the confines of the requested
      !  output period. 
      !----------------------------------------------------------------------------------
      SUBROUTINE WriteFile_OneSubbasin(Filename, SubData, SSeqReq, ESeqReq)
      IMPLICIT NONE
      CHARACTER (LEN=*),          INTENT(IN) :: Filename
      INTEGER, OPTIONAL,          INTENT(IN) :: SSeqReq, ESeqReq     ! requested start/end
      TYPE (TDlyDataForSubbasin), INTENT(IN) :: SubData
      
      INTEGER :: I, J, K, L, IOS, U1, S, D, DSI
      INTEGER :: NumDays, TDS, TDE
      INTEGER :: Seq, SSeqOut, ESeqOut, SSeq, ESeq
      LOGICAL :: Found
      CHARACTER(LEN=80)   :: Str
      CHARACTER(LEN=9999) :: Line
      INTEGER, DIMENSION(:), ALLOCATABLE :: DSIndex
      REAL, DIMENSION(:),    ALLOCATABLE :: TDV
      REAL, DIMENSION(:,:),  ALLOCATABLE :: Temp2D     ! indexed (1:NumDays, 1:NumStations)
      CHARACTER(LEN=10), PARAMETER :: Blanks = '          '

      TYPE (TDlyData),       POINTER     :: TDDP
      TYPE (THeaderInfoType)             :: HdrInfo

      !
      !  Instantiate objects to avoid problems with the FINAL routine(s)
      !
      HdrInfo = THeaderInfoType()

      !
      !  If no matching data was found, clean up and return.
      !  Do not write a file.
      !
      IF (SubData%NumDatasets .EQ. 0) THEN
         ErrorMessage = 'Warning: No relevant data was found, so no output file is written.'
         CALL PassMsg
         GOTO 999
      END IF

      !
      !  I want to always use the same ordering for the output data types.
      !  i.e. Assuming all of the standard items are there, the order will be:
      !    Column 1  =  AirtempMin
      !    Column 2  =  AirtempMax
      !    Column 3  =  Precipitation
      !    Column 4  =  AirtempMean
      !    Column 5  =  DewpointMean
      !    Column 6  =  Windspeed
      !    Column 7  =  CloudCover
      !    Column 8  =  Net longwave radiation
      !    Column 9  =  Incident shortwave radiation
      !      
      !  If a data type is missing, then everything shifts left to "fill in the gap".
      !
      !  Once these 9 types are ordered, the rest of the columns can be in any
      !  order, so I will just use them as they are in SubData.
      !
      !  Interpreting the array:
      !   DSIndex(N) is the column of SubData that will be output as the Nth 
      !   data column in the output file.  For example, suppose SubData has the
      !   following 5 data types, in this order:
      !       AirTempMax, AirTempMin, DewpointMean, CloudCover, Precipitation.
      !   I want them reordered as:
      !       AirTempMax, AirTempMin, Precipitation, DewpointMean, CloudCover.
      !   DSIndex will contain:
      !       1, 2, 5, 3, 4
      !
      ALLOCATE(DSIndex(SubData%NumDatasets), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1070) SubData%NumDatasets; CALL PassMsg
         GOTO 898
      END IF
      
      DSI = 0       ! how many entries have been filled in the DSIndex array
      DO I = 1, 9
         IF (I .EQ. 1) D = GDT_AirtempMin
         IF (I .EQ. 2) D = GDT_AirtempMax
         IF (I .EQ. 3) D = GDT_Precipitation
         IF (I .EQ. 4) D = GDT_AirtempMean
         IF (I .EQ. 5) D = GDT_DewpointMean
         IF (I .EQ. 6) D = GDT_WindSpeed
         IF (I .EQ. 7) D = GDT_CloudCover
         IF (I .EQ. 8) D = GDT_NetLongWaveRad
         IF (I .EQ. 9) D = GDT_IncidentRad
         K = SubData%GetIndexForDataOfType(D)
         IF (K .GT. 0) THEN
            DSI = DSI + 1
            DSIndex(DSI) = K
         END IF
      END DO
      
      DO I = 1, SubData%NumDatasets
         Found = .FALSE.
         DO J = 1, DSI
            IF (DSIndex(J) .EQ. I) Found = .TRUE.    ! this DType has already been assigned a column
         END DO
         IF (.NOT. Found) THEN
            DSI = DSI + 1
            DSIndex(DSI) = I
         END IF
      END DO
 
      !
      !  Determine the period of record that we will actually output to the file.
      !
      SSeq = SubData%SDateSeq
      ESeq = SubData%EDateSeq
      IF (PRESENT(SSeqReq)) SSeq = SSeqReq
      IF (PRESENT(ESeqReq)) ESeq = ESeqReq
      SSeqOut = MAX(SSeq, SubData%SDateSeq)
      ESeqOut = MIN(ESeq, SubData%EDateSeq)

      NumDays = ESeqOut - SSeqOut + 1
      IF (NumDays .LE. 0) THEN
         StatusMsg = 'No data output to '//TRIM(FileName);  CALL WriteMsg
         GOTO 999
      END IF

      !
      !  Populate the relevant fields of HdrInfo in prep for writing the header 
      !  section.  Note that we use SSeqOut and ESeqOut for the dates, rather than 
      !  the dates in SubData, because SubData might have additional data that we
      !  will not be outputting to the file.
      !
      HdrInfo%NumDataColumns = SubData%NumDatasets
      S = SubData%NumDatasets
      ALLOCATE(HdrInfo%DataType(S), HdrInfo%DataUnit(S), STAT=IOS)
      HdrInfo%Bsn            = SubData%Bsn
      HdrInfo%SubbasinNumber = SubData%SubNum
      HdrInfo%Area           = SubData%SubArea
      HdrInfo%SDate          = SSeqOut
      HdrInfo%EDate          = ESeqOut
      HdrInfo%NewDataDate    = SubData%NewDataSSeq
      
      DO S = 1, SubData%NumDatasets
         TDDP => SubData%GetPointerToDataByIndex(DSIndex(S))
         HdrInfo%DataType(S) = TDDP%GetDataType()
         HdrInfo%DataUnit(S) = TDDP%GetDataUnit()
      END DO
      HdrInfo%InfoIsValid = .TRUE.
      
      !
      !  Open the output file and write the header
      !
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      CALL WriteHeaderInfo(TRIM(FileName), U1, HFT_SingleSubbasin, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Write the output data values.
      !      
      !  Allocate a temporary 2-D data array that will hold a copy of all
      !  the data values. This may seem a little inefficient, but is
      !  actually better because once we assign the values, we can efficiently
      !  access the data as an array slice for each day.  Much better than
      !  accessing the data objects every day.
      !
      ALLOCATE(Temp2D(NumDays, SubData%NumDatasets), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) SubData%NumDatasets, NumDays; CALL PassMsg
         GOTO 898
      END IF
   
      ALLOCATE(TDV(SubData%NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) SubData%NumDatasets, NumDays; CALL PassMsg
         GOTO 898
      END IF
   
      !
      !  Assign the data into Temp2D
      !  Remember to get the datasets (1 per column) in the same order 
      !  as we wrote the header entries. This uses the DSIndex() array.
      !
      !  For each dataset, we will get the entire array of data and then 
      !  assign the relevant portion of it into Temp2D.
      !  remember that we have already adjusted SSeqOut and ESeqOut to 
      !  be within the range of the data available in SubData, so no
      !  need to check for indexes out of bounds.
      !
      !  TDS/TDE are the start/end indexes into the arrays from SubData
      !
      TDS = SSeqOut - SubData%SDateSeq + 1
      TDE = ESeqOut - SubData%SDateSeq + 1
      DO S = 1, SubData%NumDatasets
         TDDP => SubData%GetPointerToDataByIndex(DSIndex(S))
         TDV  = TDDP%GetDataVals()
         Temp2D(:,S) = TDV(TDS:TDE)
      END DO

      !
      !  The data lines
      !
      DO Seq = SSeqOut, ESeqOut
         Line = SeqToDateStringYMD(Seq)
         D = Seq - SSeqOut + 1
         DO S = 1, SubData%NumDatasets
            IF (IsMissing(Temp2D(D,S))) THEN
               Str = TRIM(FixedMissingValueString)
            ELSE
               WRITE(Str, 1019) Temp2D(D,S)
               Str = TRIM(ADJUSTL(Str))
               IF (Str(1:1) .EQ. '.') Str = '0' // TRIM(Str)
            END IF
            L = 10 - LEN_TRIM(Str)
            Str = ',' // Blanks(1:L) // TRIM(Str)
            Line = TRIM(Line) // TRIM(Str)
         END DO
         WRITE(U1, 1001, ERR=813) TRIM(Line)
      END DO
      
      !
      !  Close output file
      !
      CLOSE(U1)
      CALL FileWasClosed(U1)      
      
      !
      !  Final cleanup of local stuff
      !
      GOTO 999 
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening output file '//TRIM(Filename); CALL PassMsg; GOTO 898
  813 ErrorMessage = 'Error writing output file '//TRIM(Filename); CALL PassMsg; GOTO 898

      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] WriteFile_OneSubbasin()...'; CALL PassMsg
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF

  999 DEALLOCATE(DSIndex, STAT=IOS)
      DEALLOCATE(Temp2D,  STAT=IOS)
      DEALLOCATE(TDV,     STAT=IOS)
      CALL HdrInfo%Clear()

      !
      !  FORMATs
      !
 1001 FORMAT(A)
 
 1019 FORMAT(F10.2)
  
 1070 FORMAT('Error allocating temporary array for ', I0, ' data columns.')
 1071 FORMAT('Error allocating temporary array for ',I0, ' stations and ', I0, ' days.')
      
      END SUBROUTINE WriteFile_OneSubbasin
      
!------------------------------------------------------------------      
      !----------------------------------------------------------------------------------
      !  This routine is identical to WriteFile_OneSubbasin, except that it will contain
      !  a different set of columns, and only those that are in the explicit list.
      !
      !  The file will contain AirtempMin, AirtempMax, Precipitation, AirtempMean,
      !  DewpointMean, Windspeed, CloudCover, Runoff, USZM, LSZM, GZM, Surface and 
      !  SWE.  In that order.
      !
      !  I bet there is a slick way to combine these output routines, but right now 
      !  I just need to get it working ASAP, so I am gonna forego a bunch of restructuring.
      !  Maybe later.
      !----------------------------------------------------------------------------------
      SUBROUTINE WriteFile_SubbasinSummary(Filename, SubData, SSeqReq, ESeqReq)
      IMPLICIT NONE
      CHARACTER (LEN=*),          INTENT(IN) :: Filename
      INTEGER, OPTIONAL,          INTENT(IN) :: SSeqReq, ESeqReq     ! requested start/end
      TYPE (TDlyDataForSubbasin), INTENT(IN) :: SubData
      
      INTEGER :: I, L, IOS, U1, S, D
      INTEGER :: Dy, Mn, Yr, NumDays, TDS, TDE
      INTEGER :: Seq, SSeqOut, ESeqOut, SSeq, ESeq
      CHARACTER(LEN=80)   :: Str
      CHARACTER(LEN=9999) :: Line
      INTEGER, DIMENSION(13) :: DSIndex
      REAL, DIMENSION(:),    ALLOCATABLE :: TDV
      REAL, DIMENSION(:,:),  ALLOCATABLE :: Temp2D     ! indexed (1:NumDays, 1:NumStations)
      CHARACTER(LEN=10), PARAMETER :: Blanks = '          '

      TYPE (TDlyData),       POINTER     :: TDDP
      TYPE (THeaderInfoType)             :: HdrInfo
      
      INTEGER, PARAMETER :: NumDataSets = 13
      INTEGER, DIMENSION(13), PARAMETER :: SubDataTypes =                     &     
        (/ GDT_AirtempMin,          GDT_AirtempMax,          GDT_Precipitation,        &
           GDT_AirtempMean,         GDT_DewpointMean,        GDT_WindSpeed,            &
           GDT_CloudCover,          GDT_Runoff,              GDT_UpperSoilMoisture,    &
           GDT_LowerSoilMoisture,   GDT_GroundWaterMoisture, GDT_SurfaceZoneMoisture,  &
           GDT_SnowWater  /)
           
      INTEGER, DIMENSION(13), PARAMETER :: SubDataUnits =                     &
        (/ GDU_Celsius,             GDU_Celsius,             GDU_Millimeters,          &
           GDU_Celsius,             GDU_Celsius,             GDU_MetersPerSecond,      &
           GDU_Percent,             GDU_Millimeters,         GDU_Millimeters,          &
           GDU_Millimeters,         GDU_Millimeters,         GDU_Millimeters,          &
           GDU_Millimeters  /)
           
      !
      !  Instantiate objects to avoid problems with the FINAL routine(s)
      !
      HdrInfo = THeaderInfoType()

      !
      !  If no matching data was found, clean up and return.
      !  Do not write a file.
      !
      IF (SubData%NumDatasets .EQ. 0) THEN
         ErrorMessage = 'Warning: No relevant data was found, so no output file is written.'
         CALL PassMsg
         GOTO 999
      END IF

      !
      !  Assign the correct index to DSIndex()
      !  If there is no data for this datatype, DSIndex(I) = 0
      !
      DO I = 1, NumDataSets
         DSIndex(I) = SubData%GetIndexForDataOfType(SubDataTypes(I))
      END DO
 
      !
      !  Determine the period of record that we will actually output to the file.
      !
      SSeq = SubData%SDateSeq
      ESeq = SubData%EDateSeq
      IF (PRESENT(SSeqReq)) SSeq = SSeqReq
      IF (PRESENT(ESeqReq)) ESeq = ESeqReq
      SSeqOut = MAX(SSeq, SubData%SDateSeq)
      ESeqOut = MIN(ESeq, SubData%EDateSeq)

      NumDays = ESeqOut - SSeqOut + 1
      IF (NumDays .LE. 0) THEN
         StatusMsg = 'No data output to '//TRIM(FileName);  CALL WriteMsg
         GOTO 999
      END IF

      !
      !  Populate the relevant fields of HdrInfo in prep for writing the header 
      !  section.  Note that we use SSeqOut and ESeqOut for the dates, rather than 
      !  the dates in SubData, because SubData might have additional data that we
      !  will not be outputting to the file.
      !
      HdrInfo%NumDataColumns = NumDataSets
      S = NumDataSets
      ALLOCATE(HdrInfo%DataType(S), HdrInfo%DataUnit(S), STAT=IOS)
      HdrInfo%Bsn            = SubData%Bsn
      HdrInfo%SubbasinNumber = SubData%SubNum
      HdrInfo%Area           = SubData%SubArea
      HdrInfo%SDate          = SSeqOut
      HdrInfo%EDate          = ESeqOut
      HdrInfo%NewDataDate    = SubData%NewDataSSeq
      
      DO S = 1, NumDataSets
         HdrInfo%DataType(S) = SubDataTypes(S)
         HdrInfo%DataUnit(S) = SubDataUnits(S)
      END DO
      HdrInfo%InfoIsValid = .TRUE.
      
      !
      !  Open the output file and write the header
      !
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      CALL WriteHeaderInfo(TRIM(FileName), U1, HFT_SingleSubbasin, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Write the output data values.
      !      
      !  Allocate a temporary 2-D data array that will hold a copy of all
      !  the data values. This may seem a little inefficient, but is
      !  actually better because once we assign the values, we can efficiently
      !  access the data as an array slice for each day.  Much better than
      !  accessing the data objects every day.
      !
      ALLOCATE(Temp2D(NumDays, NumDatasets), TDV(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) NumDatasets, NumDays; CALL PassMsg
         GOTO 898
      END IF
   
      !
      !  Assign the data into Temp2D
      !  Remember to get the datasets (1 per column) in the same order 
      !  as we wrote the header entries. This uses the DSIndex() array.
      !
      !  For each dataset, we will get the entire array of data and then 
      !  assign the relevant portion of it into Temp2D.
      !  remember that we have already adjusted SSeqOut and ESeqOut to 
      !  be within the range of the data available in SubData, so no
      !  need to check for indexes out of bounds.
      !
      !  TDS/TDE are the start/end indexes into the arrays from SubData
      !
      TDS = SSeqOut - SubData%SDateSeq + 1
      TDE = ESeqOut - SubData%SDateSeq + 1
      DO S = 1, NumDataSets
         IF (DSIndex(S) .GT. 0) THEN
            TDDP => SubData%GetPointerToDataByIndex(DSIndex(S))
            TDV  = TDDP%GetDataVals()
            Temp2D(:,S) = TDV(TDS:TDE)
         ELSE
            Temp2D(:,S) = MissingData_Real
         END IF
      END DO

      !
      !  The data lines
      !
      DO Seq = SSeqOut, ESeqOut
         CALL SequenceDate(Dy, Mn, Yr, Seq); IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(Line, 1009, IOSTAT=IOS) Yr, Mn, Dy
         IF (IOS .NE. 0) Line = '9999-99-99'
         D = Seq - SSeqOut + 1
         DO S = 1, NumDatasets
            IF (IsMissing(Temp2D(D,S))) THEN
               Str = TRIM(FixedMissingValueString)
            ELSE
               WRITE(Str, 1019) Temp2D(D,S)
               Str = TRIM(ADJUSTL(Str))
               IF (Str(1:1) .EQ. '.') Str = '0' // TRIM(Str)
            END IF
            L = 10 - LEN_TRIM(Str)
            Str = ',' // Blanks(1:L) // TRIM(Str)
            Line = TRIM(Line) // TRIM(Str)
         END DO
         WRITE(U1, 1001, ERR=813) TRIM(Line)
      END DO
      
      !
      !  Close output file
      !
      CLOSE(U1)
      CALL FileWasClosed(U1)      
      
      !
      !  Final cleanup of local stuff
      !
      GOTO 999 
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening output file '//TRIM(Filename); CALL PassMsg; GOTO 898
  813 ErrorMessage = 'Error writing output file '//TRIM(Filename); CALL PassMsg; GOTO 898

      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] WriteFile_SubbasinSummary()...'; CALL PassMsg
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF

  999 DEALLOCATE(Temp2D,  STAT=IOS)
      DEALLOCATE(TDV,     STAT=IOS)
      CALL HdrInfo%Clear()

      !
      !  FORMATs
      !
 1001 FORMAT(A)
 1009 FORMAT(I4.4, 2('-', I2.2))
 
 1019 FORMAT(F10.2)
  
 1071 FORMAT('Error allocating temporary array for ',I0, ' stations and ', I0, ' days.')
      
      END SUBROUTINE WriteFile_SubbasinSummary
      
      !----------------------------------------------------------------------------------
      !  This routine is very similar to WriteFile_OneSubbasin, except that it will contain
      !  a different set of columns, and only those that are in the explicit list.
      !
      !  The file will contain the 34 columns named in the arrays.  In that order.
      !
      !  I bet there is a slick way to combine these output routines, but right now 
      !  I just need to get it working ASAP, so I am gonna forego a bunch of restructuring.
      !  Maybe later.
      !----------------------------------------------------------------------------------
      SUBROUTINE WriteFile_LakeSummary(Filename, LakeData, SSeqReq, ESeqReq)
      IMPLICIT NONE
      CHARACTER (LEN=*),       INTENT(IN) :: Filename
      INTEGER, OPTIONAL,       INTENT(IN) :: SSeqReq, ESeqReq     ! requested start/end
      TYPE (TDlyDataForLake),  INTENT(IN) :: LakeData
      
      INTEGER :: I, L, IOS, U1, S, D
      INTEGER :: Dy, Mn, Yr, NumDays, TDS, TDE
      INTEGER :: Seq, SSeqOut, ESeqOut, SSeq, ESeq
      CHARACTER(LEN=80)   :: Str
      CHARACTER(LEN=9999) :: Line
      INTEGER, DIMENSION(34) :: DSIndex
      REAL, DIMENSION(:),    ALLOCATABLE :: TDV
      REAL, DIMENSION(:,:),  ALLOCATABLE :: Temp2D     ! indexed (1:NumDays,1:NumStations)
      CHARACTER(LEN=10), PARAMETER :: Blanks = '          '

      TYPE (TDlyData),       POINTER     :: TDDP
      TYPE (THeaderInfoType)             :: HdrInfo
      
      INTEGER, PARAMETER :: NumDataSets = 34
      INTEGER, DIMENSION(34), PARAMETER :: LakeDataTypes =                    &
        (/ GDT_OverlakeRunoff,      GDT_OverlakePrecip,      GDT_Evaporation,          &
           GDT_NetBasinSupply,      GDT_OverlandRunoff,      GDT_OverlandPrecip,       &
           GDT_OverlakeAirtempMin,  GDT_OverlakeAirtempMax,  GDT_OverlakeAirtempMean,  & 
           GDT_OverlakeDewpoint,    GDT_OverlakeWindspeed,   GDT_OverlakeCloudCover,   &
           GDT_OverlandAirtempMin,  GDT_OverlandAirtempMax,  GDT_OverlandAirtempMean,  & 
           GDT_OverlandDewpoint,    GDT_OverlandWindspeed,   GDT_OverlandCloudCover,   &
           GDT_UpperSoilMoisture,   GDT_LowerSoilMoisture,   GDT_GroundWaterMoisture,  &
           GDT_SurfaceZoneMoisture, GDT_SnowWater,           GDT_WaterTemp,            &
           GDT_IceArea,             GDT_IceDepth,            GDT_IceTemp,              &
           GDT_ReflectedRad,        GDT_LatentRad,           GDT_SensibleRad,          &
           GDT_Advection,           GDT_IncidentRad,         GDT_NetLongWaveRad,       &
           GDT_TotalHeat   /)
           
      INTEGER, DIMENSION(34), PARAMETER :: LakeDataUnits =                    &
        (/ GDU_Millimeters,         GDU_Millimeters,         GDU_Millimeters,          &
           GDU_Millimeters,         GDU_Millimeters,         GDU_Millimeters,          &
           GDU_Celsius,             GDU_Celsius,             GDU_Celsius,              &
           GDU_Celsius,             GDU_MetersPerSecond,     GDU_Percent,              &
           GDU_Celsius,             GDU_Celsius,             GDU_Celsius,              &
           GDU_Celsius,             GDU_MetersPerSecond,     GDU_Percent,              &
           GDU_Millimeters,         GDU_Millimeters,         GDU_Millimeters,          &
           GDU_Millimeters,         GDU_Millimeters,         GDU_Celsius,              &
           GDU_Percent,             GDU_Millimeters,         GDU_Celsius,              &
           GDU_WattsPerM2,          GDU_WattsPerM2,          GDU_WattsPerM2,           &
           GDU_WattsPerM2,          GDU_WattsPerM2,          GDU_WattsPerM2,           &
           GDU_Calories   /)
           
      !
      !  Instantiate objects to avoid problems with the FINAL routine(s)
      !
      HdrInfo = THeaderInfoType()

      !
      !  If no matching data was found, clean up and return.
      !  Do not write a file.
      !
      IF (LakeData%NumDatasets .EQ. 0) THEN
         ErrorMessage = 'Warning: No relevant data was found, so no output file is written.'
         CALL PassMsg
         GOTO 999
      END IF

      !
      !  Assign the correct index to DSIndex()
      !  If there is no data for this datatype, DSIndex(I) = 0
      !
      DO I = 1, NumDataSets
         DSIndex(I) = LakeData%GetIndexForDataOfType(LakeDataTypes(I))
      END DO
 
      !
      !  Determine the period of record that we will actually output to the file.
      !
      SSeq = LakeData%SDateSeq
      ESeq = LakeData%EDateSeq
      IF (PRESENT(SSeqReq)) SSeq = SSeqReq
      IF (PRESENT(ESeqReq)) ESeq = ESeqReq
      SSeqOut = MAX(SSeq, LakeData%SDateSeq)
      ESeqOut = MIN(ESeq, LakeData%EDateSeq)

      NumDays = ESeqOut - SSeqOut + 1
      IF (NumDays .LE. 0) THEN
         StatusMsg = 'No data output to '//TRIM(FileName);  CALL WriteMsg
         GOTO 999
      END IF

      !
      !  Populate the relevant fields of HdrInfo in prep for writing the header 
      !  section.  Note that we use SSeqOut and ESeqOut for the dates, rather than 
      !  the dates in SubData, because SubData might have additional data that we
      !  will not be outputting to the file.
      !
      S = NumDataSets
      HdrInfo%NumDataColumns = NumDataSets
      ALLOCATE(HdrInfo%DataType(S), HdrInfo%DataUnit(S), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1070) NumDataSets;    CALL PassMsg
         GOTO 898
      END IF
      HdrInfo%Bsn            = LakeData%Bsn
      HdrInfo%SDate          = SSeqOut
      HdrInfo%EDate          = ESeqOut
      HdrInfo%NewDataDate    = MissingData_Date
      
      DO S = 1, NumDataSets
         HdrInfo%DataType(S) = LakeDataTypes(S)
         HdrInfo%DataUnit(S) = LakeDataUnits(S)
      END DO
      HdrInfo%InfoIsValid = .TRUE.
      
      !
      !  Open the output file and write the header
      !
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      CALL WriteHeaderInfo(TRIM(FileName), U1, HFT_SingleLake, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Write the output data values.
      !      
      !  Allocate a temporary 2-D data array that will hold a copy of all
      !  the data values. This may seem a little inefficient, but is
      !  actually better because once we assign the values, we can efficiently
      !  access the data as an array slice for each day.  Much better than
      !  accessing the data objects every day.
      !
      ALLOCATE(Temp2D(NumDays, NumDataSets), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) NumDataSets, NumDays; CALL PassMsg
         GOTO 898
      END IF
   
      ALLOCATE(TDV(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) NumDataSets, NumDays; CALL PassMsg
         GOTO 898
      END IF
   
      !
      !  Assign the data into Temp2D
      !  Remember to get the datasets (1 per column) in the same order 
      !  as we wrote the header entries. This uses the DSIndex() array.
      !
      !  For each dataset, we will get the entire array of data and then 
      !  assign the relevant portion of it into Temp2D.
      !  remember that we have already adjusted SSeqOut and ESeqOut to 
      !  be within the range of the data available in SubData, so no
      !  need to check for indexes out of bounds.
      !
      !  TDS/TDE are the start/end indexes into the arrays from SubData
      !
      TDS = SSeqOut - LakeData%SDateSeq + 1
      TDE = ESeqOut - LakeData%SDateSeq + 1
      DO S = 1, 34
         IF (DSIndex(S) .GT. 0) THEN
            TDDP => LakeData%GetPointerToDataByIndex(DSIndex(S))
            TDV  = TDDP%GetDataVals()
            Temp2D(:,S) = TDV(TDS:TDE)
         ELSE
            Temp2D(:,S) = MissingData_Real
         END IF
      END DO

      !
      !  The data lines
      !
      DO Seq = SSeqOut, ESeqOut
         CALL SequenceDate(Dy, Mn, Yr, Seq); IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(Line, 1009, IOSTAT=IOS) Yr, Mn, Dy
         IF (IOS .NE. 0) Line = '9999-99-99'
         D = Seq - SSeqOut + 1
         DO S = 1, NumDataSets
            IF (IsMissing(Temp2D(D,S))) THEN
               Str = TRIM(FixedMissingValueString)
            ELSE
               IF ((Temp2D(D,S) .LT.  99999.9) .AND.          &
                   (Temp2D(D,S) .GT. -99999.9)) THEN
                  WRITE(Str, 1018) Temp2D(D,S)
               ELSE
                  WRITE(Str, 1019) Temp2D(D,S)
               END IF
               Str = TRIM(ADJUSTL(Str))
               IF (Str(1:1) .EQ. '.')  Str = '0' // TRIM(Str)
               IF (Str(1:2) .EQ. '-.') Str = '-0' // TRIM(Str)
            END IF
            L = 10 - LEN_TRIM(Str)
            Str = ',' // Blanks(1:L) // TRIM(Str)
            Line = TRIM(Line) // TRIM(Str)
         END DO
         WRITE(U1, 1001, ERR=813) TRIM(Line)
      END DO
      
      !
      !  Close output file
      !
      CLOSE(U1)
      CALL FileWasClosed(U1)      
      
      !
      !  Final cleanup of local stuff
      !
      GOTO 999 
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening output file '//TRIM(Filename); CALL PassMsg; GOTO 898
  813 ErrorMessage = 'Error writing output file '//TRIM(Filename); CALL PassMsg; GOTO 898

      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] WriteFile_LakeSummary()...'; CALL PassMsg
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF

  999 DEALLOCATE(Temp2D,  STAT=IOS)
      DEALLOCATE(TDV,     STAT=IOS)
      CALL HdrInfo%Clear()

      !
      !  FORMATs
      !
 1001 FORMAT(A)
 1009 FORMAT(I4.4, 2('-', I2.2))
 
 1018 FORMAT(F9.2)
 1019 FORMAT(E9.3E2)
  
 1070 FORMAT('Error allocating temporary array for ', I0, ' data columns.')
 1071 FORMAT('Error allocating temporary array for ', I0, ' columns and ', I0, ' days.')
      
      END SUBROUTINE WriteFile_LakeSummary
      
!------------------------------------------------------------------      
      !----------------------------------------------------------------------------------
      !  This routine reads a CSV file that contains all of the daily data for a single
      !  subbasin from the LBRM.
      !
      !  The data structure passed in is of type TDlyDataForSubbasin. Each member
      !  of that object is a TDlyData object.
      !
      !  The data field SubData%NewDataSSeq is ignored/unused.
      !----------------------------------------------------------------------------------
      SUBROUTINE ReadFile_LBRM(Filename, SubData)
      IMPLICIT NONE
      CHARACTER (LEN=*),          INTENT(IN)    :: Filename
      TYPE (TDlyDataForSubbasin), INTENT(INOUT) :: SubData
      
      INTEGER :: IOS, U1, C
      INTEGER :: NumDays, SSeq, ESeq
      LOGICAL :: OK
      TYPE (TDlyData)          :: TDD
      TYPE (THeaderInfoType)   :: HdrInfo
      
      REAL, DIMENSION(:,:), ALLOCATABLE :: Temp2D     ! indexed (1:NumDays, 1:NumStations)

      !
      !  Instantiate the objects. This is necessary.
      !  If I don't do it here, and the execution path ends up never executing
      !  the code for instantiation, then upon exit from this routine the FINAL
      !  routine for the objects (called when it goes out of scope) will do
      !  bad things. This seems to me like a bug in the compiler, but I have
      !  learned by painful experience/trial/error that it's how things work
      !  with my current Fortran compiler (gfortran 5.3.0).
      !  
      TDD     = TDlyData()
      HdrInfo = THeaderInfoType()
      
      !
      !  Open the input file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Read the header section  (9 lines)
      !
      CALL ReadHeaderInfo(TRIM(FileName), U1, HFT_LBRM, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
      IF (.NOT. HdrInfo%InfoIsValid) THEN
         ErrorMessage = 'Error in status of header info.';  CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Clear the data object. It is assumed that the object was instantiated
      !  before this routine was called.
      !
      CALL SubData%Clear()

      !
      !  Assign metadata item(s) from HdrInfo into TDDMS.
      !  The rest of the header info will be used as we build station datasets.
      !
      SubData%Description = 'Output from LBRM.'
      SubData%Bsn      = HdrInfo%Bsn 
      SubData%SubNum   = HdrInfo%SubbasinNumber
      SubData%SubArea  = HdrInfo%Area
      SubData%SDateSeq = HdrInfo%SDate
      SubData%EDateSeq = HdrInfo%EDate

      !
      !  Allocate the temporary 2-D data array that will hold the data values
      !  as we read the file. This is far more efficient than using the member
      !  functions of TDlyData to assign the data as it is read.
      !
      NumDays = SubData%EDateSeq - SubData%SDateSeq + 1
      ALLOCATE(Temp2D(NumDays, HdrInfo%NumDataColumns), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) HdrInfo%NumDataColumns, NumDays; CALL PassMsg
         GOTO 898
      END IF
      Temp2D(:,:) = MissingData_Real

      !
      !  Read the data section of the file
      !
      SSeq = SubData%SDateSeq
      ESeq = SubData%EDateSeq
      CALL ReadDataSection(FileName, U1, HdrInfo%NumLines, SSeq, ESeq, Temp2D); IF (ErrorLevel .NE. 0) GOTO 899
      
      CLOSE(U1)
      CALL FileWasClosed(U1)      
      
      !
      !  Add each dataset to the big object. 
      !    Use the TDlyData object that we already instantiated at the top
      !    of this routine. It is named TDD.
      !
      !  Note that when adding the TDD object to the SubData object, a new TDlyData
      !  object is created, and then the data from TDD is copied to that new one.
      !  By calling the Clear() procedure for the local TDD object, any memory
      !  that was allocated locally is released, and we get a clean slate to work with.
      !
      DO C = 1, HdrInfo%NumDataColumns
         CALL TDD%Clear()
         OK = TDD%SetDataType(HdrInfo%DataType(C));    IF (.NOT. OK) GOTO 899
         OK = TDD%SetDataUnit(HdrInfo%DataUnit(C));    IF (.NOT. OK) GOTO 899
         OK = TDD%AssignData(SubData%SDateSeq, SubData%EDateSeq, Temp2D(:,C))
         IF ((ErrorLevel .NE. 0) .OR. (.NOT. OK))      GOTO 899
         OK = SubData%AddDataset(TDD);                 IF (.NOT. OK) GOTO 899
      END DO
      CALL TDD%Clear()

      !
      !  Final cleanup of local stuff
      !
      GOTO 999 
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file '//TRIM(Filename); CALL PassMsg; GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ReadFile_LBRM()...'; CALL PassMsg
      CALL SubData%Clear()

  999 DEALLOCATE(Temp2D,  STAT=IOS)
      CALL TDD%Clear()
      
      RETURN
      
      !
      !  FORMATs
      !
 1071 FORMAT('Error allocating temporary array for ',I0, ' stations and ', I0, ' days.')

      
      END SUBROUTINE ReadFile_LBRM

!------------------------------------------------------------------      
      !----------------------------------------------------------------------------------
      !  This routine writes a CSV file that contains all of the output data from LBRM for
      !  a single subbasin.  The file will contain Runoff, UpperSoilMoisture, LowerSoilMoisture,
      !  Groundwater, Surface Storage, and Snow Pack (SWE).
      !
      !  The requested time period for the output file is specified by SSeqOut & ESeqOut. 
      !  The SubData object may contain more (or less) data than requested. The actual output
      !  period will be the maximum possible dataset within the confines of the requested
      !  output period. 
      !----------------------------------------------------------------------------------
      SUBROUTINE WriteFile_LBRM(Filename, SubData, SSeqReq, ESeqReq)
      IMPLICIT NONE
      CHARACTER (LEN=*),          INTENT(IN) :: Filename
      INTEGER, OPTIONAL,          INTENT(IN) :: SSeqReq, ESeqReq     ! requested start/end
      TYPE (TDlyDataForSubbasin), INTENT(IN) :: SubData
      
      INTEGER :: I, IOS, U1, S
      INTEGER :: NumDays, TDS, TDE
      INTEGER :: SSeqOut, ESeqOut
      LOGICAL :: OK
      INTEGER, DIMENSION(:), ALLOCATABLE :: DSIndex
      REAL, DIMENSION(:),    ALLOCATABLE :: TDV
      REAL, DIMENSION(:,:),  ALLOCATABLE :: Temp2D     ! indexed (1:NumDays, 1:NumStations)

      TYPE (TDlyData),       POINTER     :: TDDP
      TYPE (THeaderInfoType)             :: HdrInfo

      !
      !  Instantiate objects to avoid problems with the FINAL routine(s)
      !
      HdrInfo = THeaderInfoType()

      !
      !  If we don't have the 6 data types, something is wrong.
      !
      IF (SubData%NumDatasets .NE. 6) THEN
         ErrorMessage = 'Error: Invalid number of data types when attempting to write LBRM output'
         CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  I want to always use the same ordering for the output data types.
      !  I require that all 6 types be there, and the order will be:
      !    Column 1  =  Runoff
      !    Column 2  =  Upper soil zone moisture
      !    Column 3  =  Lower soil zone moisture
      !    Column 4  =  Groundwater zone moisture
      !    Column 5  =  Surface zone moisture
      !    Column 6  =  Snowpack SWE
      !
      ALLOCATE(DSIndex(6), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1070) SubData%NumDatasets; CALL PassMsg
         GOTO 898
      END IF
      DSIndex(1) = SubData%GetIndexForDataOfType(GDT_Runoff);              IF (ErrorLevel .NE. 0) GOTO 899
      DSIndex(2) = SubData%GetIndexForDataOfType(GDT_UpperSoilMoisture);   IF (ErrorLevel .NE. 0) GOTO 899
      DSIndex(3) = SubData%GetIndexForDataOfType(GDT_LowerSoilMoisture);   IF (ErrorLevel .NE. 0) GOTO 899
      DSIndex(4) = SubData%GetIndexForDataOfType(GDT_GroundWaterMoisture); IF (ErrorLevel .NE. 0) GOTO 899
      DSIndex(5) = SubData%GetIndexForDataOfType(GDT_SurfaceZoneMoisture); IF (ErrorLevel .NE. 0) GOTO 899
      DSIndex(6) = SubData%GetIndexForDataOfType(GDT_SnowWater);           IF (ErrorLevel .NE. 0) GOTO 899

      OK = .TRUE.
      DO I = 1, 6
         IF (DSIndex(I) .LT. 1) OK = .FALSE.
      END DO
      IF (.NOT. OK) THEN
         DEALLOCATE(DSIndex, STAT=IOS)
         ErrorMessage = 'One or more required data types is missing from data set.';   CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Determine the period of record that we will actually output to the file.
      !
      IF (PRESENT(SSeqReq)) THEN
         SSeqOut = MAX(SSeqReq, SubData%SDateSeq)
      ELSE
         SSeqOut = SubData%SDateSeq
      END IF
      
      IF (PRESENT(ESeqReq)) THEN
         ESeqOut = MIN(ESeqReq, SubData%EDateSeq)
      ELSE
         ESeqOut = SubData%EDateSeq
      END IF
      
      NumDays = ESeqOut - SSeqOut + 1
      IF (NumDays .LE. 0) THEN
         StatusMsg = 'No data output to '//TRIM(FileName);  CALL WriteMsg
         GOTO 999
      END IF
      

      !
      !  Populate the relevant fields of HdrInfo in prep for writing the header 
      !  section.  Note that we use SSeqOut and ESeqOut for the dates, rather than 
      !  the dates in SubData, because SubData might have additional data that we
      !  will not be outputting to the file.
      !
      HdrInfo%NumDataColumns = SubData%NumDatasets
      S = SubData%NumDatasets
      ALLOCATE(HdrInfo%DataType(S), HdrInfo%DataUnit(S), STAT=IOS)
      HdrInfo%Bsn            = SubData%Bsn
      HdrInfo%SubbasinNumber = SubData%SubNum
      HdrInfo%Area           = SubData%SubArea
      HdrInfo%SDate          = SSeqOut
      HdrInfo%EDate          = ESeqOut
      HdrInfo%NewDataDate    = MissingData_Date      ! unused
      
      DO S = 1, SubData%NumDatasets
         TDDP => SubData%GetPointerToDataByIndex(DSIndex(S))
         HdrInfo%DataType(S) = TDDP%GetDataType()
         HdrInfo%DataUnit(S) = TDDP%GetDataUnit()
      END DO
      HdrInfo%InfoIsValid = .TRUE.
      
      !
      !  Open the output file and write the header
      !
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      CALL WriteHeaderInfo(TRIM(FileName), U1, HFT_LBRM, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Write the output data values.
      !      
      !  Allocate a temporary 2-D data array that will hold a copy of all
      !  the data values. This may seem a little inefficient, but is
      !  actually better because once we assign the values, we can efficiently
      !  access the data as an array slice for each day.  Much better than
      !  accessing the data objects every day.
      !
      ALLOCATE(Temp2D(NumDays, SubData%NumDatasets), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) SubData%NumDatasets, NumDays; CALL PassMsg
         GOTO 898
      END IF
   
      ALLOCATE(TDV(SubData%NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) SubData%NumDatasets, NumDays; CALL PassMsg
         GOTO 898
      END IF
   
      !
      !  Assign the data into Temp2D
      !  Remember to get the datasets (1 per column) in the same order 
      !  as we wrote the header entries. This uses the DSIndex() array.
      !
      !  For each dataset, we will get the entire array of data and then 
      !  assign the relevant portion of it into Temp2D.
      !  Remember that we have already adjusted SSeqOut and ESeqOut to 
      !  be within the range of the data available in SubData, so no
      !  need to check for indexes out of bounds.
      !
      !  TDS/TDE are the start/end indexes into the arrays from SubData
      !
      TDS = SSeqOut - SubData%SDateSeq + 1
      TDE = ESeqOut - SubData%SDateSeq + 1
      DO S = 1, SubData%NumDatasets
         TDDP => SubData%GetPointerToDataByIndex(DSIndex(S))
         TDV  = TDDP%GetDataVals()
         Temp2D(:,S) = TDV(TDS:TDE)
      END DO
      
      !
      !  Write the output data section of the file
      !
      CALL WriteDataSection(Filename, U1, SSeqOut, ESeqOut, Temp2D); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Close output file
      !
      CLOSE(U1)
      CALL FileWasClosed(U1)      
      
      !
      !  Final cleanup of local stuff
      !
      GOTO 999 
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening output file '//TRIM(Filename); CALL PassMsg; GOTO 898

      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] WriteFile_LBRM()...'; CALL PassMsg
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF

  999 DEALLOCATE(DSIndex, STAT=IOS)
      DEALLOCATE(Temp2D,  STAT=IOS)
      DEALLOCATE(TDV,     STAT=IOS)
      CALL HdrInfo%Clear()

      !
      !  FORMATs
      !
 1070 FORMAT('Error allocating temporary array for ', I0, ' data columns.')
 1071 FORMAT('Error allocating temporary array for ',I0, ' stations and ', I0, ' days.')
      
      END SUBROUTINE WriteFile_LBRM
      
      
!------------------------------------------------------------------      
      !----------------------------------------------------------------------------------
      !  This routine reads a CSV file that contains output from the LLTM.
      !
      !  The data structure passed in is of type TDlyDataForSubbasin. Each member
      !  of that object is a TDlyData object.
      !
      !  The data field SubData%NewDataSSeq is ignored/unused.
      !
      !  Note that the LLTM output file is similar in structure to the other standard 
      !  GLSHFS files, but is not identical. Namely, the header is shorter and the
      !  column headers may not be fully compliant.  Should probably be revisisted 
      !  in the future, but just trying to get this done right now.  
      !  (Tim Hunter,  22 June 2017)
      !----------------------------------------------------------------------------------
      SUBROUTINE ReadFile_LLTM(Filename, SubData)
      IMPLICIT NONE
      CHARACTER (LEN=*),          INTENT(IN)    :: Filename
      TYPE (TDlyDataForSubbasin), INTENT(INOUT) :: SubData
      
      INTEGER :: I, IOS, U1, C, D
      INTEGER :: NumEntries, LineNumber, Seq
      LOGICAL :: OK
      TYPE (TDlyData)          :: TDD
      
      CHARACTER(LEN=100), DIMENSION(999) :: Strings
      CHARACTER(LEN=300) :: Line

      REAL, DIMENSION(:,:), ALLOCATABLE :: Temp2D     ! indexed (1:NumDays, 1:NumStations)

      INTEGER, DIMENSION(20), PARAMETER :: MyDataTypes =                                            &
        (/ GDT_Evaporation,         GDT_WaterTemp,        GDT_IceTemp,           GDT_IceArea,       &
           GDT_IceDepth,            GDT_ReflectedRad,     GDT_LatentRad,         GDT_SensibleRad,   &
           GDT_Advection,           GDT_IncidentRad,      GDT_NetLongWaveRad,    GDT_TotalHeat,     &
           GDT_AirtempMean,         GDT_DewPointMean,     GDT_WindSpeed,         GDT_CloudCover,    &
           GDT_OverlakeAirTempMean, GDT_OverlakeDewpoint, GDT_OverlakeWindSpeed, GDT_OverlakeCloudCover /)
        
      INTEGER, DIMENSION(20), PARAMETER :: MyDataUnits =                                        &
        (/ GDU_Millimeters,      GDU_Celsius,         GDU_Celsius,           GDU_Percent,       &
           GDU_Millimeters,      GDU_WattsPerM2,      GDU_WattsPerM2,        GDU_WattsPerM2,    &
           GDU_WattsPerM2,       GDU_WattsPerM2,      GDU_WattsPerM2,        GDU_Calories,      &
           GDU_Celsius,          GDU_Celsius,         GDU_MetersPerSecond,   GDU_Percent,       &
           GDU_Celsius,          GDU_Celsius,         GDU_MetersPerSecond,   GDU_Percent   /)
        
      
      !
      !  Instantiate the TDD object.  This is necessary.
      !  If I don't do it here, and the execution path ends up never executing
      !  the code for instantiation, then upon exit from this routine the FINAL
      !  routine for the objects (called when it goes out of scope) will do
      !  bad things. This seems to me like a bug in the compiler, but I have
      !  learned by painful experience/trial/error that it's how things work
      !  with my current Fortran compiler (gfortran 5.3.0).
      !
      !  Initialize variables and clear the SubData object.
      !  
      TDD = TDlyData()
      U1 = -1
      CALL SubData%Clear()
      
      !
      !  Open the input file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Read header info
      !
      READ(U1, 1000, ERR=812)
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, NumEntries);     IF (ErrorLevel .NE. 0) GOTO 899
      I = LakeNumberFromName3(TRIM(Strings(1)))
      IF (I .LE. 0) THEN
         ErrorMessage = 'Error parsing the lake name from '//TRIM(FileName);  CALL PassMsg
         GOTO 898
      END IF
      SubData%Bsn = LakeName3(I)
      SubData%SubNum  = 0
      SubData%SubArea = CoordLakeArea(I)
      
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, NumEntries);     IF (ErrorLevel .NE. 0) GOTO 899
      I = DateStringYMDToSeq(Strings(1))
      IF (I .EQ. MissingData_Date) THEN
         ErrorMessage = 'Error parsing the start date from '//TRIM(FileName);  CALL PassMsg
         GOTO 898
      END IF
      SubData%SDateSeq = I
      
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, NumEntries);     IF (ErrorLevel .NE. 0) GOTO 899
      I = DateStringYMDToSeq(Strings(1))
      IF (I .EQ. MissingData_Date) THEN
         ErrorMessage = 'Error parsing the end date from '//TRIM(FileName);  CALL PassMsg
         GOTO 898
      END IF
      SubData%EDateSeq = I
      SubData%NumDays = SubData%EDateSeq - SubData%SDateSeq + 1
      
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, NumEntries);     IF (ErrorLevel .NE. 0) GOTO 899
      IF (TRIM(GetLowercase(Strings(1))) .NE. 'yyyy-mm-dd') THEN
         ErrorMessage = 'Error parsing header line 5 from '//TRIM(FileName);  CALL PassMsg
         GOTO 898
      END IF
      
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, NumEntries);     IF (ErrorLevel .NE. 0) GOTO 899
      IF (TRIM(GetLowercase(Strings(1))) .NE. 'yyyy-mm-dd') THEN
         ErrorMessage = 'Error parsing header line 6 from '//TRIM(FileName);  CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Allocate the temporary 2-D data array that will hold the data values
      !  as we read the file. This is far more efficient than using the member
      !  functions of TDlyData to assign the data as it is read.
      !
      ALLOCATE(Temp2D(SubData%NumDays, 20), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) 20, SubData%NumDays; CALL PassMsg
         GOTO 898
      END IF
      Temp2D(:,:) = MissingData_Real

      !
      !  Read the data lines, assuming the defined column structure (data types 
      !  and units of each column)
      !
      LineNumber = 6
      DO Seq = SubData%SDateSeq, SubData%EDateSeq
         D = Seq - SubData%SDateSeq + 1
         LineNumber = LineNumber + 1
         READ(U1, 1000, ERR=811) Line
         CALL ParseCommaSepLine(Line, Strings, NumEntries);     IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumEntries .LT. 21) GOTO 857
         DO C = 1, 20
            READ(Strings(C+1), *, IOSTAT=IOS) Temp2D(D,C)
            IF (IOS .NE. 0) Temp2D(D,C) = MissingData_Real
            IF ((C .EQ. 5) .AND. (.NOT. IsMissing(Temp2D(D,C)))) Temp2D(D,C) = Temp2D(D,C) * 1000      ! meters -> mm
         END DO
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      U1 = -1
      
      !
      !  Add each dataset to the big object. 
      !    Use the TDlyData object that we already instantiated at the top
      !    of this routine. It is named TDD.
      !
      !  Note that when adding the TDD object to the SubData object, a new TDlyData
      !  object is created, and then the data from TDD is copied to that new one.
      !  By calling the Clear() procedure for the local TDD object, any memory
      !  that was allocated locally is released, and we get a clean slate to work with.
      !
      DO C = 1, 20
         CALL TDD%Clear()
         OK = TDD%SetDataType(MyDataTypes(C));         IF (.NOT. OK) GOTO 899
         OK = TDD%SetDataUnit(MyDataUnits(C));         IF (.NOT. OK) GOTO 899
         OK = TDD%AssignData(SubData%SDateSeq, SubData%EDateSeq, Temp2D(:,C))
         IF ((ErrorLevel .NE. 0) .OR. (.NOT. OK))      GOTO 899
         OK = SubData%AddDataset(TDD);                 IF (.NOT. OK) GOTO 899
      END DO
      CALL TDD%Clear()

      !
      !  Final cleanup of local stuff
      !
      GOTO 999 
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file '//TRIM(Filename); CALL PassMsg; GOTO 898
  812 ErrorMessage = 'Error reading input file '//TRIM(Filename); CALL PassMsg; GOTO 898

  857 WRITE(ErrorMessage, 1057) LineNumber, TRIM(Filename);  CALL PassMsg
      GOTO 898
      

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ReadFile_LLTM()...'; CALL PassMsg
      CALL SubData%Clear()

  999 DEALLOCATE(Temp2D, STAT=IOS)
      CALL TDD%Clear()
      IF (U1 .GT. 0) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF
      RETURN
      
      !
      !  FORMATs
      !
 1000 FORMAT(A300)
 1057 FORMAT('Error reading line ', I0, ' of file ', A)
 1071 FORMAT('Error allocating temporary array for ',I0, ' stations and ', I0, ' days.')
      
      END SUBROUTINE ReadFile_LLTM

!------------------------------------------------------------------      
      !----------------------------------------------------------------------------------
      !  This routine reads a text file in the CGLRRM format that contains the daily data
      !  for a single timeseries of data.
      !  
      !  I am assuming that the units are tens of CMS, matching the WriteFile_CGLRRM() routine.
      !  NBS values returned are in CMS.
      !
      !  Note that the DlyData array has been allocated by the calling routine to an
      !  arbitrary size (run-time error if I try to pass an unallocated array), so I
      !  have to first deallocate it, then reallocate it to the correct size.
      !----------------------------------------------------------------------------------
      SUBROUTINE ReadFile_CGLRRM(FName, Bsn, DlyData, SSeq, ESeq, Descript)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN)    :: FName
      CHARACTER(LEN=3),   INTENT(INOUT) :: Bsn
      CHARACTER(LEN=*),   INTENT(INOUT) :: Descript
      REAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: DlyData
      INTEGER,            INTENT(INOUT) :: SSeq, ESeq
      
      INTEGER :: I, J, D, Q, U1, IOS, LkNum, Mn, Yr
      INTEGER :: SMn, SYr, EMn, EYr, NumDays
      INTEGER :: Seq1, SeqD1, DaysQ1, DaysQ2, DaysQ3, DMax
      REAL    :: DVals(8)
      CHARACTER(LEN=10) :: LkName
      CHARACTER(LEN=70) :: S
      CHARACTER(LEN=80) :: Line

      !
      !  Open the input file and read header lines
      ! 
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)

      READ(U1, 1101, ERR=812) LkName
      I = INDEX(LkName, ' ')
      LkName = LkName(1:I-1)
      LkNum = LakeNumberFromName10(LkName)
      IF (LkNum .LT. 1) THEN
         WRITE(ErrorMessage, 5001) TRIM(LkName), TRIM(FName);  CALL PassMsg
         GOTO 898
      END IF
      Bsn = LakeName3(LkNum)
      READ(U1,    *, ERR=812) 
      READ(U1, 1103, ERR=812) S
      I = LEN(Descript)
      J = MIN(I, LEN_TRIM(S))
      Descript = S(1:J)
      READ(U1,    *, ERR=812) 
      READ(U1,    *, ERR=812) 
      READ(U1,    *, ERR=812) 
      READ(U1,    *, ERR=812) 

      !
      !  Skim through the file once to get the date extent.
      !  Assume that all months have full set of 4 quarters present.
      !
      READ(U1, 1110, IOSTAT=IOS) SYr, SMn
      EYr = SYr
      EMn = SMn
      READ(U1, 1110, IOSTAT=IOS) Yr, Mn
      DO WHILE (IOS .EQ. 0)
         IF (Yr .GT. EYr) THEN
            EYr = Yr
            EMn = Mn
         END IF
         IF ((Yr .EQ. EYr) .AND. (Mn .GT. EMn)) EMn = Mn
         READ(U1, 1110, IOSTAT=IOS) Yr, Mn
      END DO
      
      !
      !  Allocate the memory for the data
      !  Deallocate the old memory first.
      !
      CALL DateSequence(1, SMn, SYr, SSeq)
      D = DaysInMonth(EMn, EYr)
      CALL DateSequence(D, EMn, EYr, ESeq)
      NumDays = ESeq - SSeq + 1
      IF (ALLOCATED(DlyData)) DEALLOCATE(DlyData, STAT=I)
      ALLOCATE(DlyData(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5002) NumDays, TRIM(FName);    CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Main loop through all of the data
      !
      REWIND(U1)
      DO I = 1, 7
         READ(U1, *, ERR=812)    ! skip header lines
      END DO
      READ(U1, 1120, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         READ(Line, 1130, ERR=815) Yr, Mn, Q
         DaysQ1 = DaysInQtrMon(1, Mn, Yr)
         DaysQ2 = DaysInQtrMon(2, Mn, Yr)
         DaysQ3 = DaysInQtrMon(3, Mn, Yr)
         DMax   = DaysInQtrMon(Q, Mn, Yr)
         READ(Line, 1130, ERR=815) Yr, Mn, Q, (DVals(D), D=1,DMax)
         CALL DateSequence(1, Mn, Yr, SeqD1)
         SELECT CASE (Q)
            CASE(1) 
                     Seq1 = SeqD1
            CASE(2) 
                     Seq1 = SeqD1 + DaysQ1
            CASE(3) 
                     Seq1 = SeqD1 + DaysQ1 + DaysQ2
            CASE(4) 
                     Seq1 = SeqD1 + DaysQ1 + DaysQ2 + DaysQ3
         END SELECT
         DO I = 1, DMax
            DlyData(Seq1-SSeq+I) = DVals(I) * 10.0           ! tens of cms -> cms
         END DO
         READ(U1, 1120, IOSTAT=IOS) Line
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening output file '//TRIM(FName); CALL PassMsg; GOTO 898
  812 ErrorMessage = 'Error reading output file '//TRIM(FName); CALL PassMsg; GOTO 898
  815 ErrorMessage = 'Error parsing output file '//TRIM(FName); CALL PassMsg; GOTO 898
      
  898 ErrorLevel = 1
      ErrorMessage = '[traceback] ReadFile_CGLRRM...';  CALL PassMsg
  
      IF (U1 .GT. 0) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
      RETURN

 1101 FORMAT(7X, A10)
 1103 FORMAT(2X, A80)
 1110 FORMAT(I5, I3)
 1120 FORMAT(A80)
 1130 FORMAT(I5, I3, I2, 8F8.0)
 
 5001 FORMAT('Invalid lake name [', A, '] in file ', A)
 5002 FORMAT('Error allocating memory for ', I0, ' days of NBS data from ', A)
 
      END SUBROUTINE ReadFile_CGLRRM
      
!------------------------------------------------------------------      
      !----------------------------------------------------------------------------------
      !  This routine writes a text file in the CGLRRM format that contains the daily data
      !  for a single timeseries of data.
      !----------------------------------------------------------------------------------
      SUBROUTINE WriteFile_CGLRRM(FName, Bsn, DlyData, SSeq, ESeq, Descript)
      IMPLICIT NONE
      CHARACTER(LEN=*),   INTENT(IN) :: FName, Bsn, Descript
      REAL, DIMENSION(:), INTENT(IN) :: DlyData
      INTEGER,            INTENT(IN) :: SSeq, ESeq
      
      INTEGER :: I, D, M, Y, Q, D1, D2, U1, LkNum, Mn, Yr, DMax
      INTEGER :: SD, SM, SY, ED, EM, EY, Seq1, Seq2, DSeq
      LOGICAL :: DoMonth
      INTEGER, DIMENSION(31) :: MData

      !
      !  Get lake number from the 3-character lake name passed in
      !
      LkNum = LakeNumberFromName3(Bsn)
      IF (LkNum .LT. 1) THEN
         ErrorMessage = 'Invalid lake name passed to WriteFile_CGLRRM';  CALL PassMsg
         GOTO 898
      END IF

      !
      !  Open the output file and write header lines
      ! 
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)
      
      WRITE(U1, 1001, ERR=813) TRIM(LakeName10(LkNum))
      WRITE(U1, 1002, ERR=813)
      WRITE(U1, 1003, ERR=813) TRIM(Descript)
      WRITE(U1, 1004, ERR=813)
      WRITE(U1, 1005, ERR=813)
      WRITE(U1, 1006, ERR=813)
      WRITE(U1, 1007, ERR=813)

      !
      !  Main loop through all of the data
      !
      CALL SequenceDate(SD, SM, SY, SSeq);  IF (Errorlevel .NE. 0) GOTO 899
      CALL SequenceDate(ED, EM, EY, ESeq);  IF (Errorlevel .NE. 0) GOTO 899
      Mn = SM
      Yr = SY
      DO WHILE (Yr .LE. EY)
         !
         !  Is this month within the valid data period?
         !
         DoMonth = .FALSE.
         IF (Yr .LT. EY) DoMonth = .TRUE.
         IF ((Yr .EQ. EY) .AND. (Mn .LE. EM)) DoMonth = .TRUE.

         !
         !  If so, transfer daily values into the array for a month's worth of 
         !  data, then output per the quarter-monthly boundaries, as needed by
         !  the CGLRRM.
         !
         IF (DoMonth) THEN
            MData(:) = -99999
            DMax = DaysInMonth(Mn, Yr);                 IF (ErrorLevel .NE. 0) GOTO 899
            CALL DateSequence(   1, Mn, Yr, Seq1);      IF (ErrorLevel .NE. 0) GOTO 899
            CALL DateSequence(DMax, Mn, Yr, Seq2);      IF (ErrorLevel .NE. 0) GOTO 899
            DO DSeq = Seq1, Seq2
               I = DSeq - SSeq + 1
               IF ((DSeq .GE. SSeq) .AND. (DSeq .LE. ESeq)) THEN
                  CALL SequenceDate(D, M, Y, DSeq);     IF (ErrorLevel .NE. 0) GOTO 899
                  IF (DlyData(I) .GT. MissingData_Real_Test) THEN
                     MData(D) = NINT(DlyData(I) / 10.0)
                  ELSE
                     MData(D) = -99999
                  END IF
               END IF
            END DO
            DO Q = 1, 4
               D1 = FirstDayOfQtrMon(DMax, Q);  IF(ErrorLevel .NE. 0) GOTO 899
               D2 = LastDayOfQtrMon(DMax, Q);   IF(ErrorLevel .NE. 0) GOTO 899
               WRITE(U1, 1010, ERR=813) Yr, Mn, Q, (MData(D), D=D1,D2)
            END DO
         END IF

         !
         !  Next month
         !
         Mn = Mn + 1
         IF (Mn .GT. 12) THEN
            Mn = Mn - 12
            Yr = Yr + 1
         END IF
      END DO

      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening output file '//TRIM(FName); CALL PassMsg; GOTO 898
  813 ErrorMessage = 'Error writing output file '//TRIM(FName); CALL PassMsg; GOTO 898
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] WriteFile_CGLRRM...';  CALL PassMsg
  
      IF (U1 .GT. 0) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
      RETURN

 1001 FORMAT('# LAKE ', A, ' DAILY NET BASIN SUPPLIES (CMS*10)')
 1002 FORMAT('#        [e.g. value of 149. => 1490 cms]')
 1003 FORMAT('# ', A)
 1004 FORMAT('# Fortran format: I5, I3, I2, 8F8.0')
 1005 FORMAT('INTERVAL: Daily')
 1006 FORMAT('UNITS: 10m3s')
 1007 FORMAT('#  YR  M Q       1       2       3       4       5       6       7       8')
 1010 FORMAT(I5, I3, I2, 8(I7, '.'))
 
      END SUBROUTINE WriteFile_CGLRRM
      
END MODULE GLSHFS_Files