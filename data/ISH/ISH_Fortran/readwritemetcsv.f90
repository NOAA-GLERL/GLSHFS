!-----------------------------------------------------------------------
!  This module provides routines for reading and writing to/from a
!  met_*.csv file, the new (2014) format that I am using as the GLERL
!  standard format for met station data.  
!
!  For more details, see the written file format documentation that should
!  be available elsewhere.
!
!  Notes:
!
!  1) The file format as I defined it allows for the data to be stored 
!  in a variety of units, but the preferred units are what will be 
!  used for the write procedures in this module. The read procedures
!  will handle whatever is thrown at them (within the limits of those I
!  have anticipated).
!
!  2) When writing the files, I will always output the same 7 data types
!  in the same prescribed order. This is not required by the format 
!  definition , but will make writing easier.  The read procedures are
!  accommodating to more variance in the data. Columns may be missing and
!  they may be in a different order.
!
!-----------------------------------------------------------------------

MODULE ReadWriteMetCsv
   USE ErrorProcess
   USE Glshfs_Util
   USE MetDataTypesAndUnits
   USE DailyMetStationDataset


   TYPE TMetCsvFile
      CHARACTER(LEN=150)       :: FileName
      TYPE (TDlyMetStnDataset) :: DlyData
   END TYPE TMetCsvFile
   
   TYPE TMetCsvErrorReport
      INTEGER :: ErrorsFixed          ! how many bad values were changed to missing?
      INTEGER :: ErrorsFatal          ! how many fatal errors were found?
      INTEGER :: NumLines
      CHARACTER(LEN=120), DIMENSION(:), ALLOCATABLE :: Lines
   END TYPE TMetCsvErrorReport
   
   TYPE (TMetCsvErrorReport) :: ErrorReport
   
   LOGICAL, PRIVATE :: BuildErrorReport
   PRIVATE :: InitializeMetCsvErrorReport
   PRIVATE :: AddLineToErrorReport
CONTAINS

   !------------------------------------------------------------
   SUBROUTINE ReadMetCsvFile(FileName, MCF, ErrStat, MakeErrReport)
   IMPLICIT NONE
   CHARACTER(LEN=*),    INTENT(IN)    :: FileName
   TYPE (TMetCsvFile),  INTENT(INOUT) :: MCF
   INTEGER,             INTENT(OUT)   :: ErrStat
   LOGICAL, OPTIONAL,   INTENT(IN)    :: MakeErrReport

   INTEGER :: I, J, IOS, UNum
   INTEGER :: IOSX
   INTEGER :: Year, Month, Day, NumDays, Seq
   INTEGER :: NumDT, LineNum
   REAL    :: DVal
   CHARACTER (LEN=15)  :: DTypeStr
   CHARACTER (LEN=20)  :: DS
   CHARACTER (LEN=100) :: TStr
   CHARACTER (LEN=120) :: ErrLine
   CHARACTER (LEN=250) :: Line

   CHARACTER(LEN=100), DIMENSION(99) :: CsvStrings
   
   !
   !  Use the optional MakeErrReport parameter, if present, to enable
   !  tracking errors, if requested.
   !  By default there will be no reporting.
   !
   BuildErrorReport = .FALSE. 
   IF (PRESENT(MakeErrReport)) BuildErrorReport = MakeErrReport
   IF (BuildErrorReport) CALL InitializeMetCsvErrorReport
   
   !
   !  Initialize the data structure   
   !
   MCF%FileName = TRIM(FileName)
   CALL InitializeDlyMetStnDataset(MCF%DlyData)
   
   !
   !  Read header info so I can allocate RAM, etc
   !
   ErrStat = -1
   CALL ReadMetCsvFileHeaderInfo(FileName, MCF%DlyData, ErrStat)
   IF (ErrStat .NE. 0) GOTO 899
   NumDT = MCF%DlyData%NumDataTypes

   !
   !  Allocate the RAM.
   !  Note that the DataTypes and DataUnits arrays were already allocated 
   !  and filled as part of reading the header info.
   !  Extract all of the data.
   !
   NumDays = MCF%DlyData%EDate - MCF%DlyData%SDate + 1
   ALLOCATE(MCF%DlyData%MetData(NumDT, NumDays), STAT=IOS)
   IF (IOS .NE. 0) THEN
      ErrorMessage = 'Error when allocating RAM for data values.'
      CALL PassMsg
      WRITE(ErrorMessage, *) 'NumDT = ', NumDT
      CALL PassMsg
      WRITE(ErrorMessage, *) 'NumDays = ', NumDays
      CALL PassMsg
      CALL InitializeDlyMetStnDataset(MCF%DlyData)
      ErrorLevel = 1
      GOTO 899
   END IF
   MCF%DlyData%MetData(:,:) = MissingData_Real

   !
   !  Read all of the data.
   !
   UNum = GetFreeUnitNumber();  IF (ErrorLevel .NE. 0) GOTO 899
   OPEN(UNIT=UNum, FILE=TRIM(FileName), STATUS='OLD', ERR=811)
   CALL FileWasOpened(UNum)
   DO I = 1, 6
      READ(UNum, 1000, IOSTAT=IOS)      ! skip header lines
   END DO
   
   LineNum = 7
   READ(UNum, 1000, IOSTAT=IOS) Line
   DO WHILE (IOS .EQ. 0) 
      CALL ParseCommaSepLine(Line, CsvStrings)
      DS = TRIM(ADJUSTL(CsvStrings(1)))
      READ(DS, 1010, IOSTAT=IOS) Year, Month, Day
      IF (IOS .NE. 0) THEN
         IF (BuildErrorReport) THEN
            WRITE(ErrLine, 1101) TRIM(MCF%DlyData%ID), LineNum
            CALL AddLineToErrorReport(ErrLine)
            ErrLine = 'Processing of this station aborted'
            CALL AddLineToErrorReport(ErrLine)
            ErrorReport%ErrorsFatal = ErrorReport%ErrorsFatal + 1
         END IF
         WRITE(ErrorMessage, 1001) LineNum, TRIM(FileName)
         CALL PassMsg
         CALL InitializeDlyMetStnDataset(MCF%DlyData)
         ErrorLevel = 1
         GOTO 899
      ELSE
         CALL DateSequence(Day, Month, Year, Seq)
         IF ((Seq .LT. MCF%DlyData%SDate) .OR. (Seq .GT. MCF%DlyData%EDate)) THEN
            IF (BuildErrorReport) THEN
               WRITE(ErrLine, 1102) TRIM(MCF%DlyData%ID), LineNum
               CALL AddLineToErrorReport(ErrLine)
               ErrorReport%ErrorsFixed = ErrorReport%ErrorsFixed + 1
            END IF
         ELSE
            J = Seq - MCF%DlyData%SDate + 1
            DO I = 1, NumDT
               TStr = TRIM(CsvStrings(I+1))
               IF (TRIM(TStr) .EQ. '#N/A') THEN
                  DVal = MissingData_Real
                  IOSX = 0
               ELSE
                  READ(TStr, *, IOSTAT=IOSX) DVal
               END IF
               IF (IOSX .EQ. 0) THEN
                  MCF%DlyData%MetData(I,J) = DVal
               ELSE
                  MCF%DlyData%MetData(I,J) = MissingData_Real
                  IF (BuildErrorReport) THEN
                     DTypeStr = MetDataTypeString(MCF%DlyData%DataTypes(I))
                     WRITE(ErrLine, 1103) TRIM(MCF%DlyData%ID), LineNum, TRIM(DTypeStr)
                     CALL AddLineToErrorReport(ErrLine)
                     ErrorReport%ErrorsFixed = ErrorReport%ErrorsFixed + 1
                  END IF
               END IF
            END DO
         END IF
      END IF
      LineNum = LineNum + 1
      READ(UNum, 1000, IOSTAT=IOS) Line
   END DO   
   CLOSE(UNum)
   CALL FileWasClosed(UNum)

   !
   !  Future enhancement here....
   !  Trim the data structure so that the data extents correspond to only
   !  the NON-MISSING data. That includes adjusting SDate and EDate.
   !
   ErrStat = 0
   RETURN
   

 811 ErrorMessage = 'Error opening input file '//TRIM(FileName); CALL PassMsg; GOTO 899
 
 899 ErrorMessage = '[traceback] : ReadMetCsvFile'; CALL PassMsg
     RETURN

 1000 FORMAT(A)
 1001 FORMAT('Error parsing date on line ', I0, ' in file ', A)
 1010 FORMAT(I4, 2(1X, I2)) 
     
 1101 FORMAT('Error reading data for ', A, ' at line ', I0, ': Date field format issue')
 1102 FORMAT('Error reading data for ', A, ' at line ', I0, ': Date outside range specified in header')
 1103 FORMAT('Error reading data for ', A, ' at line ', I0, ': Bad data value for ', A)

   END SUBROUTINE ReadMetCsvFile

   !------------------------------------------------------------
   SUBROUTINE ReadMetCsvFileInStandardOrder(FileName, MCF, ErrStat, MakeErrReport)
   IMPLICIT NONE
   CHARACTER(LEN=*),    INTENT(IN)    :: FileName
   TYPE (TMetCsvFile),  INTENT(INOUT) :: MCF
   INTEGER,             INTENT(OUT)   :: ErrStat
   LOGICAL, OPTIONAL,   INTENT(IN)    :: MakeErrReport

   INTEGER :: I, J, K, DT, IOS, UNum
   INTEGER :: IOSX
   INTEGER :: Year, Month, Day, NumDays, Seq
   INTEGER :: NumDT, LineNum, OldU, NewU
   REAL    :: DVal
   CHARACTER (LEN=15)  :: DTypeStr
   CHARACTER (LEN=20)  :: DS
   CHARACTER (LEN=100) :: TStr
   CHARACTER (LEN=120) :: ErrLine
   CHARACTER (LEN=250) :: Line
   TYPE (TDlyMetStnDataset) :: Temp
   
   CHARACTER(LEN=100), DIMENSION(99) :: CsvStrings
   
   !
   !  Use the optional MakeErrReport parameter, if present, to enable
   !  tracking errors, if requested.
   !  By default there will be no reporting.
   !
   BuildErrorReport = .FALSE. 
   IF (PRESENT(MakeErrReport)) BuildErrorReport = MakeErrReport
   IF (BuildErrorReport) CALL InitializeMetCsvErrorReport
   
   !
   !  I will read the data from the file into the temporary structure, 
   !  then re-order it into the "live" one.
   !
   MCF%FileName = TRIM(FileName)
   CALL InitializeDlyMetStnDataset(MCF%DlyData)
   CALL InitializeDlyMetStnDataset(Temp)
   
   !
   !  Read header info so I can allocate RAM, etc
   !
   ErrStat = -1
   CALL ReadMetCsvFileHeaderInfo(FileName, Temp, ErrStat)
   IF (ErrStat .NE. 0) GOTO 899
   NumDT = Temp%NumDataTypes

   !
   !  Allocate the RAM.
   !  Note that the DataTypes and DataUnits arrays were already allocated 
   !  and filled as part of reading the header info.
   !  Extract all of the data.
   !
   NumDays = Temp%EDate - Temp%SDate + 1
   ALLOCATE(Temp%MetData(NumDT, NumDays), STAT=IOS)
   IF (IOS .NE. 0) THEN
      ErrorMessage = 'Error when allocating RAM for temporary storage of data values.'
      CALL PassMsg
      WRITE(ErrorMessage, *) 'NumDT = ', NumDT
      CALL PassMsg
      WRITE(ErrorMessage, *) 'NumDays = ', NumDays
      CALL PassMsg
      CALL InitializeDlyMetStnDataset(Temp)
      ErrorLevel = 1
      GOTO 899
   END IF
   Temp%MetData(:,:) = MissingData_Real

   !
   !  Read all of the data.
   !
   UNum = GetFreeUnitNumber();  IF (ErrorLevel .NE. 0) GOTO 899
   OPEN(UNIT=UNum, FILE=TRIM(FileName), STATUS='OLD', ERR=811)
   CALL FileWasOpened(UNum)
   DO I = 1, 6
      READ(UNum, 1000, IOSTAT=IOS)      ! skip header lines
   END DO
   
   LineNum = 7
   READ(UNum, 1000, IOSTAT=IOS) Line
   DO WHILE (IOS .EQ. 0) 
      CALL ParseCommaSepLine(Line, CsvStrings)
      DS = TRIM(ADJUSTL(CsvStrings(1)))
      READ(DS, 1010, IOSTAT=IOS) Year, Month, Day
      IF (IOS .NE. 0) THEN
         IF (BuildErrorReport) THEN
            WRITE(ErrLine, 1101) TRIM(Temp%ID), LineNum
            CALL AddLineToErrorReport(ErrLine)
            ErrLine = 'Processing of this station aborted'
            CALL AddLineToErrorReport(ErrLine)
            ErrorReport%ErrorsFatal = ErrorReport%ErrorsFatal + 1
         END IF
         WRITE(ErrorMessage, 1001) LineNum, TRIM(FileName)
         CALL PassMsg
         CALL InitializeDlyMetStnDataset(Temp)
         ErrorLevel = 1
         GOTO 899
      ELSE
         CALL DateSequence(Day, Month, Year, Seq)
         IF ((Seq .LT. Temp%SDate) .OR. (Seq .GT. Temp%EDate)) THEN
            IF (BuildErrorReport) THEN
               WRITE(ErrLine, 1102) TRIM(Temp%ID), LineNum
               CALL AddLineToErrorReport(ErrLine)
               ErrorReport%ErrorsFixed = ErrorReport%ErrorsFixed + 1
            END IF
         ELSE
            J = Seq - Temp%SDate + 1
            DO I = 1, NumDT
               TStr = TRIM(CsvStrings(I+1))
               IF (TRIM(TStr) .EQ. '#N/A') THEN
                  DVal = MissingData_Real
                  IOSX = 0
               ELSE
                  READ(TStr, *, IOSTAT=IOSX) DVal
               END IF
               IF (IOSX .EQ. 0) THEN
                  Temp%MetData(I,J) = DVal
               ELSE
                  Temp%MetData(I,J) = MissingData_Real
                  IF (BuildErrorReport) THEN
                     DTypeStr = MetDataTypeString(Temp%DataTypes(I))
                     WRITE(ErrLine, 1103) TRIM(Temp%ID), LineNum, TRIM(DTypeStr)
                     CALL AddLineToErrorReport(ErrLine)
                     ErrorReport%ErrorsFixed = ErrorReport%ErrorsFixed + 1
                  END IF
               END IF
            END DO
         END IF
      END IF
      LineNum = LineNum + 1
      READ(UNum, 1000, IOSTAT=IOS) Line
   END DO   
   CLOSE(UNum)
   CALL FileWasClosed(UNum)

   
   !
   !  Now rearrange the columns into the GLERL standard order
   !
   MCF%DlyData%DataValid    = .TRUE.
   MCF%DlyData%ID           = Temp%ID
   MCF%DlyData%Name         = Temp%Name
   MCF%DlyData%Country      = Temp%Country
   MCF%DlyData%Lat          = Temp%Lat
   MCF%DlyData%Long         = Temp%Long
   MCF%DlyData%SDate        = Temp%SDate
   MCF%DlyData%EDate        = Temp%EDate
   MCF%DlyData%NumDataTypes = 7
   ALLOCATE(MCF%DlyData%DataTypes(7), MCF%DlyData%DataUnits(7), STAT=IOS)
   IF (IOS .NE. 0) THEN
      ErrorMessage = 'Error allocating RAM for updated header info.'
      CALL PassMsg
      ErrorMessage = 'Unable to read file for ' // TRIM(MCF%DlyData%ID)
      CALL PassMsg
      GOTO 899
   END IF
  
   !
   !  Assign type and unit information for each column
   !      
   MCF%DlyData%DataTypes(1) = MDT_AirtempMax
   MCF%DlyData%DataTypes(2) = MDT_AirtempMin
   MCF%DlyData%DataTypes(3) = MDT_Precipitation
   MCF%DlyData%DataTypes(4) = MDT_AirtempMean
   MCF%DlyData%DataTypes(5) = MDT_DewpointMean
   MCF%DlyData%DataTypes(6) = MDT_Windspeed
   MCF%DlyData%DataTypes(7) = MDT_CloudCover

   MCF%DlyData%DataUnits(1) = MDU_Celsius
   MCF%DlyData%DataUnits(2) = MDU_Celsius
   MCF%DlyData%DataUnits(3) = MDU_Millimeters
   MCF%DlyData%DataUnits(4) = MDU_Celsius
   MCF%DlyData%DataUnits(5) = MDU_Celsius
   MCF%DlyData%DataUnits(6) = MDU_MetersPerSecond
   MCF%DlyData%DataUnits(7) = MDU_Percent
   
   ALLOCATE(MCF%DlyData%MetData(7,NumDays), STAT=IOS)
   IF (IOS .NE. 0) THEN
      ErrorMessage = 'Error allocating RAM for reordered data values.'
      CALL PassMsg
      ErrorMessage = 'Unable to read file for ' // TRIM(MCF%DlyData%ID)
      CALL PassMsg
      GOTO 899
   END IF
   MCF%DlyData%MetData(:,:) = MissingData_Real
  
   DO I = 1, 7
      NewU = MCF%DlyData%DataUnits(I)
      DO J = 1, Temp%NumDataTypes
         DT   = Temp%DataTypes(J)
         OldU = Temp%DataUnits(J)
         IF (DT .EQ. MCF%DlyData%DataTypes(I)) THEN
            DO Seq = Temp%SDate, Temp%EDate
               K = Seq - Temp%SDate + 1
               DVal = UnitConvertedDataValue(Temp%MetData(J,K), OldU, NewU)
               MCF%DlyData%MetData(I,K) = DVal
            END DO
         END IF
      END DO
   END DO

   !
   !  Clean up the Temp structure.  This routine deallocates RAM, etc.
   !
   CALL InitializeDlyMetStnDataset(Temp)
   
   !
   !  Future enhancement here....
   !  Trim the data structure so that the data extents correspond to only
   !  the NON-MISSING data. That includes adjusting SDate and EDate.
   !
   ErrStat = 0
   RETURN
   

 811 ErrorMessage = 'Error opening input file '//TRIM(FileName); CALL PassMsg; GOTO 899
 
 899 ErrorMessage = '[traceback] : ReadMetCsvFileInStandardOrder'; CALL PassMsg
     CALL InitializeDlyMetStnDataset(Temp)
     RETURN

 1000 FORMAT(A)
 1001 FORMAT('Error parsing date on line ', I0, ' in file ', A)
 1010 FORMAT(I4, 2(1X, I2)) 
     
 1101 FORMAT('Error reading data for ', A, ' at line ', I0, ': Date field format issue')
 1102 FORMAT('Error reading data for ', A, ' at line ', I0, ': Date outside range specified in header')
 1103 FORMAT('Error reading data for ', A, ' at line ', I0, ': Bad data value for ', A)

   END SUBROUTINE ReadMetCsvFileInStandardOrder
   
   
   !------------------------------------------------------------
   SUBROUTINE ReadMetCsvFileHeaderInfo(FileName, MSD, ErrStat)
   IMPLICIT NONE
   
   CHARACTER(LEN=*),         INTENT(IN)    :: FileName
   TYPE (TDlyMetStnDataset), INTENT(INOUT) :: MSD
   INTEGER,                  INTENT(OUT)   :: ErrStat

   INTEGER :: I, IOS, UNum
   INTEGER :: NumDays
   INTEGER :: NumStrings, NumDT
   LOGICAL :: F
   CHARACTER(LEN=100) :: TStr, S
   CHARACTER(LEN=250) :: Line
   
   CHARACTER(LEN=100), DIMENSION(99) :: CsvStrings

   !
   !  Initialize stuff
   !
   CALL ClearDlyMetStnDatasetHeader(MSD)
   ErrStat = -1

   !
   !  Open the input file
   !
   UNum = GetFreeUnitNumber();  IF (ErrorLevel .NE. 0) GOTO 899
   OPEN(UNIT=UNum, FILE=TRIM(FileName), STATUS='OLD', ERR=811)
   CALL FileWasOpened(UNum)
   
   !
   !  line 1
   !
   READ(UNum, 1000, ERR=812) Line
   CALL ParseCommaSepLine(Line, CsvStrings, NumStrings)
   IF (NumStrings .LT. 1) GOTO 301
   TStr = GetLowercase(TRIM(ADJUSTL(CsvStrings(1))))
   IF (TRIM(TStr) .NE. 'daily data for a single station.') GOTO 401
         
   !
   !  Line 2 - station ID
   !
   READ(UNum, 1000, ERR=812) Line
   CALL ParseCommaSepLine(Line, CsvStrings, NumStrings)
   IF (NumStrings .LT. 2) GOTO 302
   S = GetLowercase(StripAllBlanks(CsvStrings(1)))
   IF (TRIM(S) .NE. 'stationid:') GOTO 332
   MSD%ID = TRIM(ADJUSTL(CsvStrings(2)))
   S = TRIM(ADJUSTL(CsvStrings(2)))
   IF (LEN_TRIM(S) .EQ. 0) S = 'xxxxxxx'
   MSD%ID = TRIM(StripDoubleQuotes(S))

   !
   !  Line 3 - station name
   !
   READ(UNum, 1000, ERR=812) Line
   CALL ParseCommaSepLine(Line, CsvStrings, NumStrings)
   IF (NumStrings .LT. 2) GOTO 303
   S = GetLowercase(StripAllBlanks(CsvStrings(1)))
   IF (TRIM(S) .NE. 'stationname:') GOTO 333
   MSD%Name = TRIM(ADJUSTL(CsvStrings(2)))
   S = TRIM(MSD%Name)
   IF (LEN_TRIM(S) .EQ. 0) S = '"Unknown"'
   IF (S(1:1) .NE. CHAR(34)) THEN
      MSD%Name = '"' // TRIM(MSD%Name) // '"'     ! ensure station name is enclosed by double-quotes
   END IF
   MSD%Country = ''

   !
   !  Line 4 - latitude and longitude
   !
   READ(UNum, 1000, ERR=812) Line
   CALL ParseCommaSepLine(Line, CsvStrings, NumStrings)
   IF (NumStrings .LT. 3) GOTO 304
   S = GetLowercase(StripAllBlanks(CsvStrings(1)))
   S = TRIM(StripDoubleQuotes(S))
   F = .FALSE.
   IF (TRIM(S) .EQ. 'lat&long')  F = .TRUE.
   IF (TRIM(S) .EQ. 'lat&long:') F = .TRUE. 
   IF (.NOT. F) GOTO 334
   TStr = TRIM(ADJUSTL(CsvStrings(2)));  READ(TStr, *, ERR=404) MSD%Lat
   TStr = TRIM(ADJUSTL(CsvStrings(3)));  READ(TStr, *, ERR=405) MSD%Long
            
   !
   !  Line 5 - elevation
   !
   READ(UNum, 1000, ERR=812) Line
   CALL ParseCommaSepLine(Line, CsvStrings, NumStrings)
   IF (NumStrings .LT. 2) GOTO 305
   S = GetLowercase(StripAllBlanks(CsvStrings(1)))
   S = TRIM(StripDoubleQuotes(S))
   IF (TRIM(S) .NE. 'elevation:') GOTO 335
   TStr = TRIM(ADJUSTL(CsvStrings(2)));  READ(TStr, *, ERR=406) MSD%Elevation
            
   !
   !  Line 6 - start date
   !
   READ(UNum, 1000, ERR=812) Line
   CALL ParseCommaSepLine(Line, CsvStrings, NumStrings)
   IF (NumStrings .LT. 4) GOTO 306
   S = GetLowercase(StripAllBlanks(CsvStrings(1)))
   S = TRIM(StripDoubleQuotes(S))
   F = .FALSE.
   IF (TRIM(S) .EQ. 'starts(ymd)')  F = .TRUE.
   IF (TRIM(S) .EQ. 'starts(ymd):') F = .TRUE. 
   IF (.NOT. F) GOTO 336
   MSD%SDate = DateStringYMDToSeq(TRIM(ADJUSTL(CsvStrings(2)))); IF (ErrorLevel .NE. 0) GOTO 407
            
   !
   !  Line 7 - end date
   !
   READ(UNum, 1000, ERR=812) Line
   CALL ParseCommaSepLine(Line, CsvStrings, NumStrings)
   IF (NumStrings .LT. 3) GOTO 307
   S = GetLowercase(StripAllBlanks(CsvStrings(1)))
   S = TRIM(StripDoubleQuotes(S))
   F = .FALSE.
   IF (TRIM(S) .EQ. 'ends(ymd)')  F = .TRUE.
   IF (TRIM(S) .EQ. 'ends(ymd):') F = .TRUE. 
   IF (.NOT. F) GOTO 337
   MSD%EDate = DateStringYMDToSeq(TRIM(ADJUSTL(CsvStrings(2)))); IF (ErrorLevel .NE. 0) GOTO 408
   
   NumDays = MSD%EDate - MSD%SDate + 1
   IF (NumDays .LT. 1) THEN
      ErrorMessage = 'Invalid start/end dates in file: '//TRIM(FileName)
      CALL PassMsg
      ErrorMessage = 'End date prior to start date'; CALL PassMsg
      GOTO 899
   END IF

   !
   !  Line 8 - data types
   !
   READ(UNum, 1000, ERR=812) Line
   CALL ParseCommaSepLine(Line, CsvStrings, NumStrings)
   IF (NumStrings .LT. 3) GOTO 308
   S = GetLowercase(StripAllBlanks(CsvStrings(1)))
   S = TRIM(StripDoubleQuotes(S))
   IF (TRIM(S) .NE. 'date') GOTO 338
   NumDT = NumStrings - 1
   MSD%NumDataTypes = NumDT
   ALLOCATE(MSD%DataTypes(NumDT), MSD%DataUnits(NumDT), STAT=IOS)
   IF (IOS .NE. 0) GOTO 851
   MSD%DataTypes(:) = MDT_Undefined
   MSD%DataUnits(:) = MDU_Undefined
   DO I = 1, NumDT
      TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
      TStr = TRIM(StripDoubleQuotes(TStr))
      MSD%DataTypes(I) = MetDataTypeFromString(TStr)
      IF (MSD%DataTypes(I) .EQ. MDT_Undefined) GOTO 409
   END DO

   !
   !  Line 9
   !
   READ(UNum, 1000, ERR=812) Line
   CALL ParseCommaSepLine(Line, CsvStrings, NumStrings)
   IF (NumStrings .LT. 3) GOTO 309
   S = GetLowercase(StripAllBlanks(CsvStrings(1)))
   S = TRIM(StripDoubleQuotes(S))
   IF (TRIM(S) .NE. 'yyyy-mm-dd') GOTO 339
   I = NumStrings - 1
   IF (I .NE. MSD%NumDataTypes) GOTO 410
   DO I = 1, NumDT
      TStr = TRIM(ADJUSTL(CsvStrings(I+1)))
      TStr = TRIM(StripDoubleQuotes(TStr))
      MSD%DataUnits(I) = MetDataUnitFromString(TStr)
      IF (MSD%DataUnits(I) .EQ. MDU_Undefined) GOTO 410
   END DO

   CLOSE(UNum)
   CALL FileWasClosed(UNum)
   
   ErrStat = 0
   RETURN


  301 ErrorMessage = 'Formatting error on line 1 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  302 ErrorMessage = 'Formatting error on line 2 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  303 ErrorMessage = 'Formatting error on line 3 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  304 ErrorMessage = 'Formatting error on line 4 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  305 ErrorMessage = 'Formatting error on line 5 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  306 ErrorMessage = 'Formatting error on line 6 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  307 ErrorMessage = 'Formatting error on line 7 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  308 ErrorMessage = 'Formatting error on line 8 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  309 ErrorMessage = 'Formatting error on line 9 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  332 ErrorMessage = 'Error with label string on line 2 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  333 ErrorMessage = 'Error with label string on line 3 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  334 ErrorMessage = 'Error with label string on line 4 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  335 ErrorMessage = 'Error with label string on line 5 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  336 ErrorMessage = 'Error with label string on line 6 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  337 ErrorMessage = 'Error with label string on line 7 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  338 ErrorMessage = 'Error with label string on line 8 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  339 ErrorMessage = 'Error with label string on line 9 in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  401 ErrorMessage = 'Invalid entry on line 1 of '             //TRIM(FileName); CALL PassMsg
      ErrorMessage = 'Entry should say "Daily data for a single station."';      CALL PassMsg; GOTO 898
  404 ErrorMessage = 'Error parsing Latitude on line 4 in '    //TRIM(FileName); CALL PassMsg; GOTO 898
  405 ErrorMessage = 'Error parsing Longitude on line 4 in '   //TRIM(FileName); CALL PassMsg; GOTO 898
  406 ErrorMessage = 'Error parsing Elevation on line 5 in '   //TRIM(FileName); CALL PassMsg; GOTO 898
  407 ErrorMessage = 'Error parsing Start Date on line 6 in '  //TRIM(FileName); CALL PassMsg; GOTO 898
  408 ErrorMessage = 'Error parsing End Date on line 7 in '    //TRIM(FileName); CALL PassMsg; GOTO 898
  409 ErrorMessage = 'Error parsing line 8 in '                //TRIM(FileName); CALL PassMsg; GOTO 898
  410 ErrorMessage = 'Error parsing line 9 in '                //TRIM(FileName); CALL PassMsg; GOTO 898


  
  811 ErrorMessage = 'Error opening input file '//TRIM(FileName); CALL PassMsg; GOTO 899
  812 ErrorMessage = 'Error reading input file '//TRIM(FileName); CALL PassMsg; GOTO 899
   
      !
      !  memory allocation error
      !      
  851 ErrorMessage = 'Error allocating memory for temporary storage of header information'; CALL PassMsg
      ErrorMessage = 'while reading '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  898 ErrStat = 1
  899 ErrorMessage = '[traceback] : ReadMetCsvFileHeaderInfo'; CALL PassMsg
      DEALLOCATE(MSD%DataTypes, STAT=IOS)
      DEALLOCATE(MSD%DataUnits, STAT=IOS)
      RETURN

 1000 FORMAT(A)

   END SUBROUTINE ReadMetCsvFileHeaderInfo

   !------------------------------------------------------------
   SUBROUTINE WriteMetCsvFile(FileName, DaysWritten, MCF, ErrStat)
   IMPLICIT NONE
   CHARACTER(LEN=*),    INTENT(IN)    :: FileName
   INTEGER,             INTENT(OUT)   :: DaysWritten
   TYPE (TMetCsvFile),  INTENT(INOUT) :: MCF
   INTEGER,             INTENT(OUT)   :: ErrStat

   INTEGER :: UNum
   
   !
   !  Still need to implement this as an actual count of days written
   !  i.e. the number of days that had actual data.
   !  This will need to happen in WriteTheDataLines().
   !
   !  As a placeholder.... assume all days had data.
   !
   DaysWritten = MCF%DlyData%EDate - MCF%DlyData%SDate + 1
   
   !
   !  Open the output file
   !
   UNum = GetFreeUnitNumber();  IF (ErrorLevel .NE. 0) GOTO 899
   OPEN(UNIT=UNum, FILE=TRIM(FileName), STATUS='REPLACE', ERR=811)
   CALL FileWasOpened(UNum)

   !
   !  Header lines
   !
   CALL WriteTheHeaderInfo(UNum, FileName, MCF%DlyData, ErrStat)
   
   !
   !  Data value rows
   !
   CALL WriteTheDataLines(UNum, MCF%DlyData, ErrStat)
   
   CLOSE(UNum)
   CALL FileWasClosed(UNum)
   ErrStat = 0
   RETURN
   
 811 ErrorMessage = 'Error opening output file '//TRIM(FileName); CALL PassMsg; GOTO 899

 899 ErrorMessage = '[traceback] : WriteMetCsvFile'; CALL PassMsg
     ErrStat = 1
     RETURN
    
   END SUBROUTINE WriteMetCsvFile

   !------------------------------------------------------------
   !
   !  If StandardizedFormat is set .TRUE., then the output file will have the
   !  7 standard output columns, with our standard units. Any other data that
   !  was passed to the write routine will be ignored.
   !------------------------------------------------------------
   SUBROUTINE WriteMetCsvFile2(FileName, MCF, StandardizedFormat, ErrStat)
   IMPLICIT NONE
   CHARACTER(LEN=*),    INTENT(IN)    :: FileName
   TYPE (TMetCsvFile),  INTENT(INOUT) :: MCF
   LOGICAL,             INTENT(IN)    :: StandardizedFormat
   INTEGER,             INTENT(OUT)   :: ErrStat

   INTEGER :: I, J, D, IOS, UNum
   INTEGER :: OldU, NewU, NumDays
   REAL    :: DVal
   
   TYPE (TDlyMetStnDataset) :: StdMSD, OutputMSD
   
   !
   !  If caller requested the output file in standardized format, then build
   !  the StdMSD structure.
   !
   IF (StandardizedFormat) THEN
      CALL InitializeDlyMetStnDataset(StdMSD)
      StdMSD%DataValid    = .TRUE.
      StdMSD%ID           = MCF%DlyData%ID
      StdMSD%Name         = MCF%DlyData%Name
      StdMSD%Country      = MCF%DlyData%Country
      StdMSD%Lat          = MCF%DlyData%Lat
      StdMSD%Long         = MCF%DlyData%Long
      StdMSD%SDate        = MCF%DlyData%SDate
      StdMSD%EDate        = MCF%DlyData%EDate
      StdMSD%NumDataTypes = 7
      ALLOCATE(StdMSD%DataTypes(7), StdMSD%DataUnits(7), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating RAM for temporary data structure.'
         CALL PassMsg
         ErrorMessage = 'Unable to write output file for ' // TRIM(StdMSD%ID)
         CALL PassMsg
         GOTO 899
      END IF
     
      !
      !  Assign type and unit information for each column
      !      
      StdMSD%DataTypes(1) = MDT_AirtempMax
      StdMSD%DataTypes(2) = MDT_AirtempMin
      StdMSD%DataTypes(3) = MDT_Precipitation
      StdMSD%DataTypes(4) = MDT_AirtempMean
      StdMSD%DataTypes(5) = MDT_DewpointMean
      StdMSD%DataTypes(6) = MDT_Windspeed
      StdMSD%DataTypes(7) = MDT_CloudCover

      StdMSD%DataUnits(1) = MDU_Celsius
      StdMSD%DataUnits(2) = MDU_Celsius
      StdMSD%DataUnits(3) = MDU_Millimeters
      StdMSD%DataUnits(4) = MDU_Celsius
      StdMSD%DataUnits(5) = MDU_Celsius
      StdMSD%DataUnits(6) = MDU_MetersPerSecond
      StdMSD%DataUnits(7) = MDU_Percent

      !
      !  Create the data array in the temporary "object"      
      !
      NumDays = StdMSD%EDate - StdMSD%SDate + 1
      ALLOCATE(StdMSD%MetData(7,NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating RAM for temporary data storage.'
         CALL PassMsg
         ErrorMessage = 'Unable to write output file for ' // TRIM(StdMSD%ID)
         CALL PassMsg
         DEALLOCATE(StdMSD%DataTypes, StdMSD%DataUnits, STAT=IOS)
         GOTO 899
      END IF
      StdMSD%MetData(:,:) = MissingData_Real

      !
      !  Copy the data from the old structure into the temporary one.
      !  This is where the reordering and unit conversion is done.
      !
      DO I = 1, 7
         NewU  = StdMSD%DataUnits(I)
         DO J = 1, MCF%DlyData%NumDataTypes
            IF (MCF%DlyData%DataTypes(J) .EQ. StdMSD%DataTypes(I)) THEN
               OldU = MCF%DlyData%DataUnits(J)
               DO D = 1, NumDays
                  DVal = UnitConvertedDataValue(MCF%DlyData%MetData(J,D), OldU, NewU)
                  StdMSD%MetData(I,D) = DVal
               END DO
            END IF
         END DO
      END DO
      OutputMSD = StdMSD
   ELSE
      OutputMSD = MCF%DlyData
   END IF

   !
   !  Open the output file
   !
   UNum = GetFreeUnitNumber();  IF (ErrorLevel .NE. 0) GOTO 899
   OPEN(UNIT=UNum, FILE=TRIM(FileName), STATUS='REPLACE', ERR=811)
   CALL FileWasOpened(UNum)

   CALL WriteTheHeaderInfo(UNum, FileName, OutputMSD, ErrStat)
   IF (ErrStat .NE. 0) RETURN
   
   CALL WriteTheDataLines(UNum, OutputMSD, ErrStat)
   IF (ErrStat .NE. 0) RETURN
   
   CLOSE(UNum)
   CALL FileWasClosed(UNum)
   RETURN
   
   !
   !  Error handling
   !
 811 ErrorMessage = 'Error opening output file '//TRIM(FileName); CALL PassMsg; GOTO 899

 899 ErrorMessage = '[traceback] : WriteMetCsvFile2'; CALL PassMsg
     ErrorLevel = 1
     ErrStat = 1
     RETURN
   
   END SUBROUTINE WriteMetCsvFile2   
   
   !-----------------------------------------------------------------
   !  Write the header info into the output file.  It is assumed that
   !  the output file is already open with unit number UNum.
   !-----------------------------------------------------------------
   SUBROUTINE WriteTheHeaderInfo(UNum, FileName, MSD, ErrStat)
   IMPLICIT NONE
   INTEGER,                  INTENT(IN)    :: UNum
   CHARACTER (LEN=*),        INTENT(IN)    :: FileName
   TYPE (TDlyMetStnDataset), INTENT(INOUT) :: MSD
   INTEGER,                  INTENT(OUT)   :: ErrStat
   
   INTEGER :: I, IOS, Day, Month, Year, LineNum, NDT
   CHARACTER(LEN=80) :: S

   ErrStat = 0      ! assume ok until proven wrong
   
   LineNum = 1
   WRITE(UNum, 1101, IOSTAT=IOS)
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2001) LineNum, TRIM(Filename);  CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF

   !
   !  2) Station ID
   !
   LineNum = 2
   WRITE(UNum, 1102, IOSTAT=IOS) TRIM(MSD%ID)
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2002) LineNum, TRIM(Filename);  CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF
   
   !
   !  3) Station Name
   !
   LineNum = 3
   S = TRIM(ADJUSTL(MSD%Name))
   IF (S(1:1) .NE. CHAR(34)) MSD%Name = CHAR(34) // TRIM(S) // CHAR(34)
   WRITE(UNum, 1103, IOSTAT=IOS) TRIM(MSD%Name)
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2003) LineNum, TRIM(Filename);  CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF
   
   !
   !  4) Lat, Long
   !
   LineNum = 4
   WRITE(UNum, 1104, IOSTAT=IOS) MSD%Lat, MSD%Long
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2004) LineNum, TRIM(Filename);  CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF

   !
   !  5) Elevation.  Just write missing value for now.
   !
   LineNum = 5
   WRITE(UNum, 1105, IOSTAT=IOS) -999.9
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2005) LineNum, TRIM(Filename);  CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF
  
   !
   !  6) Start Date
   !
   LineNum = 6

   IF (MSD%SDate .EQ. MissingData_Date) THEN
      WRITE(UNum, 1106, IOSTAT=IOS) 9999, 99, 99
   ELSE
      CALL SequenceDate(Day, Month, Year, MSD%SDate); IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(UNum, 1106, IOSTAT=IOS) Year, Month, Day
   END IF
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2006) LineNum, TRIM(Filename);   CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF
   
   !
   !  7) End Date
   !
   LineNum = 7
   IF (MSD%EDate .EQ. MissingData_Date) THEN
      WRITE(UNum, 1107, IOSTAT=IOS) 9999, 99, 99
   ELSE
      CALL SequenceDate(Day, Month, Year, MSD%EDate); IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(UNum, 1107, IOSTAT=IOS) Year, Month, Day
   END IF
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2007) LineNum, TRIM(Filename);   CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF
   
   !
   !  8) Column headers (data types)
   !
   LineNum = 8
   NDT = MSD%NumDataTypes
   WRITE(UNum, 1108, IOSTAT=IOS, ADVANCE='NO') (TRIM(MetDataTypeString(MSD%DataTypes(I))), I=1,NDT-1)
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2008) LineNum, TRIM(Filename)
      CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF

   WRITE(UNum, 1120, IOSTAT=IOS) TRIM(MetDataTypeString(MSD%DataTypes(NDT)))
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2008) LineNum, TRIM(Filename)
      CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF


   !
   !  9) Column headers (data units)
   !
   LineNum = 6
   NDT = MSD%NumDataTypes
   WRITE(UNum, 1109, IOSTAT=IOS, ADVANCE='NO') (TRIM(MetDataUnitString(MSD%DataUnits(I))), I=1,NDT-1)
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2009) LineNum, TRIM(Filename)
      CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF

   WRITE(UNum, 1120, IOSTAT=IOS) TRIM(MetDataUnitString(MSD%DataUnits(NDT)))
   IF (IOS .NE. 0) THEN
      WRITE(ErrorMessage, 2009) LineNum, TRIM(Filename)
      CALL PassMsg
      IF (BuildErrorReport) THEN
         CALL AddLineToErrorReport(ErrorMessage)
      END IF
      CLOSE(UNum)
      CALL FileWasClosed(UNum)
      ErrStat = -1
      RETURN
   END IF

   RETURN
   
   
 899 ErrorMessage = '[traceback] : WriteTheHeaderInfo()...'; CALL PassMsg
     ErrorLevel = 1
     ErrStat = 1
     RETURN
   
   
 1101 FORMAT('Daily data for a single station.')
 1102 FORMAT('StationID:,', A)
 1103 FORMAT('StationName:,', A)
 1104 FORMAT('Lat&Long:', 2(',', F0.3))
 1105 FORMAT('Elevation:,', F0.3)
 1106 FORMAT('Starts(YMD):,', I4.4, 2('-',I2.2))
 1107 FORMAT('Ends(YMD):,', I4.4, 2('-',I2.2))
 1108 FORMAT('Date', 9999(',', A))
 1109 FORMAT('YYYY-MM-DD', 9999(',', A))
 1120 FORMAT(A)

 2001 FORMAT('Error writing header line ', I0, ' (format id string) to file [', A, ']')
 2002 FORMAT('Error writing header line ', I0, ' (ID) to file [', A, ']')
 2003 FORMAT('Error writing header line ', I0, ' (station name) to file [', A, ']')
 2004 FORMAT('Error writing header line ', I0, ' (lat, long) to file [', A, ']')
 2005 FORMAT('Error writing header line ', I0, ' (elevation) to file [', A, ']')
 2006 FORMAT('Error writing header line ', I0, ' (start date) to file [', A, ']')
 2007 FORMAT('Error writing header line ', I0, ' (end date) to file [', A, ']')
 2008 FORMAT('Error writing header line ', I0, ' (data type column headers) to file [', A, ']')
 2009 FORMAT('Error writing header line ', I0, ' (data unit column headers) to file [', A, ']')

   END SUBROUTINE WriteTheHeaderInfo

   
   !------------------------------------------------------------------                  
   !  Write the data lines into the output file.  It is assumed that
   !  the output file is already open with unit number UNum.
   !
   !  Note that for each valid numeric value, I want to make sure there is at
   !  least 1 digit in front of the decimal point. Default Fortran formatting,
   !  at least with the gfortran compiler, is annoying in that a number like
   !  0.75 will end up printing as .75 and -0.75 will print as -.75
   !  While this is ok in terms of validity, it is not "pretty" from a user
   !  standpoint. It also seems like it caused issues for me in the past when
   !  trying to read the resulting file with code from other languages (not
   !  100% sure about that one). Anyway, forcing the leading 0 is the best
   !  universal solution in my mind. So I do some messy manipulation of the
   !  output data values in order to accomplish that.
   !
   !  In my opinion, this is a bad omission in the Fortran standard (missing 
   !  any way to force leading zeroes in a REAL output) that I saw
   !  discussed online numerous times from people trying to do similar stuff.
   !
   !  Also note that I am employing the MS Excel convention of #N/A to indicate
   !  missing data values. it allows very clean input to Excel, disambiguation of
   !  what value means missing, and a user of the resulting file can do a global 
   !  replace if they don't like it.
   !
   !  Additionally, due to the way Fortran formatting is implemented, I have to do
   !  special stuff to avoid a trailing comma on the last entry. You would think it
   !  could be done by specifying it as a leading comma and structuring things
   !  appropriately, but it doesn't work quite as you might expect. So I am stuck with
   !  a lot of extra messiness, unfortunately. 
   !
   !------------------------------------------------------------------                  
   SUBROUTINE WriteTheDataLines(UNum, MSD, ErrStat)
   IMPLICIT NONE
   INTEGER,                  INTENT(IN)    :: UNum
   TYPE (TDlyMetStnDataset), INTENT(INOUT) :: MSD
   INTEGER,                  INTENT(OUT)   :: ErrStat

   INTEGER :: I, J, IOS, Dy, Mn, Yr, Seq, NDT, LineNum
   INTEGER :: DU, DP
   REAL    :: DVal
   CHARACTER(LEN=1)   :: Comma
   CHARACTER(LEN=20)  :: MyFmt
   CHARACTER(LEN=30)  :: S
   CHARACTER(LEN=999) :: Line
   
   Comma = ','
   ErrStat = 0      ! assume ok until proven wrong
   
   LineNum = 6      ! assume 6 lines of header already written
   
   NDT = MSD%NumDataTypes
   DO Seq = MSD%SDate, MSD%EDate
      LineNum = LineNum + 1
      I = Seq - MSD%SDate + 1
      CALL SequenceDate(Dy, Mn, Yr, Seq)
      Line = ''
      DO J = 1, NDT
         DVal = MSD%MetData(J,I)         ! the data value
         DU   = MSD%DataUnits(J)         ! the numeric value corresponding to the data units for this value
         DP   = MetDataUnitOutputPrecision(DU)
         IF (IsMissing(DVal)) THEN
            S = '#N/A'
         ELSE
            IF (ABS(DVal) .GT. 1.0) THEN
               WRITE(MyFmt, 1101) DP
               WRITE(S, TRIM(MyFmt)) DVal
            ELSE
               IF (DVal .GE. 0) THEN
                  WRITE(MyFmt, 1102) DP
                  WRITE(S, TRIM(MyFmt)) INT(DVal), DVal-INT(DVal)
               ELSE
                  WRITE(MyFmt, 1103) DP
                  DVal = ABS(DVal)
                  WRITE(S, TRIM(MyFmt)) INT(DVal), DVal-INT(DVal)
               END IF
            END IF
         END IF
         IF (J .LT. NDT) S = TRIM(S) // Comma     ! add trailing comma to all but last entry
         Line = TRIM(Line) // TRIM(S)             ! add entry to end of line
      END DO
      WRITE(UNum, 1010, IOSTAT=IOS) Yr, Mn, Dy, TRIM(Line)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 2000) TRIM(MSD%ID), LineNum
         IF (BuildErrorReport) THEN
            CALL AddLineToErrorReport(ErrorMessage)
         END IF
         CLOSE(UNum)
         CALL FileWasClosed(UNum)
         ErrStat = -1
      END IF
   END DO
   
   CLOSE(UNum)
   CALL FileWasClosed(UNum)
   ErrStat = 0
   RETURN
   
   
! 1001 FORMAT(F0.3)
! 1002 FORMAT(I0.1, F0.3)
! 1003 FORMAT('-', I0.1, F0.3)
 1010 FORMAT(I4.4, 2('-',I2.2), ',', A)
 
 1101 FORMAT('(F0.', I0, ')')
 1102 FORMAT('(I0.1, F0.', I0, ')')
 1103 FORMAT('(''-'', I0.1, F0.', I0, ')')

 2000 FORMAT('Error writing output for ', A, ': Line number ', I0)
 
   END SUBROUTINE WriteTheDataLines

   
   !-------------------------------------------------------------------------------
   CHARACTER(LEN=120) FUNCTION DQ(S)
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN) :: S
   DQ = '"' // TRIM(S) // '"'
   END FUNCTION DQ
   
   !-------------------------------------------------------------------------------
   SUBROUTINE InitializeMetCsvErrorReport
   IMPLICIT NONE 
   INTEGER :: IOS   
   ErrorReport%ErrorsFixed = 0
   ErrorReport%ErrorsFatal = 0
   ErrorReport%NumLines = 0
   ALLOCATE(ErrorReport%Lines(500), STAT=IOS)    ! allocate 500 lines at a time
   END SUBROUTINE InitializeMetCsvErrorReport
   
   !-------------------------------------------------------------------------------
   SUBROUTINE AddLineToErrorReport(Line)
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN) :: Line
   INTEGER :: I, IOS, CurrSize, NewSize
   CHARACTER(LEN=120), DIMENSION(:), ALLOCATABLE :: Temp
   
   CurrSize = SIZE(ErrorReport%Lines)
   IF (ErrorReport%NumLines .EQ. CurrSize) THEN
      NewSize = CurrSize + 500
      ALLOCATE(Temp(NewSize), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating RAM for error report'; CALL PassMsg
         DEALLOCATE(ErrorReport%Lines, STAT=IOS)
         ErrorReport%NumLines = -1
         ErrorLevel = -1
         RETURN
      END IF
      DO I=1,CurrSize
         Temp(I) = TRIM(ErrorReport%Lines(I))
      END DO
      DEALLOCATE(ErrorReport%Lines, STAT=IOS)
      ALLOCATE(ErrorReport%Lines(NewSize), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating RAM for error report'; CALL PassMsg
         DEALLOCATE(ErrorReport%Lines, STAT=IOS)
         ErrorReport%NumLines = -1
         ErrorLevel = -1
         RETURN
      END IF
      DO I=1,CurrSize
         ErrorReport%Lines(I) = TRIM(Temp(I))
      END DO
      DEALLOCATE(ErrorReport%Lines, STAT=IOS)
   END IF
      
   ErrorReport%NumLines = ErrorReport%NumLines + 1
   ErrorReport%Lines(ErrorReport%NumLines) = TRIM(Line)
      
   END SUBROUTINE AddLineToErrorReport
   

END MODULE ReadWriteMetCsv