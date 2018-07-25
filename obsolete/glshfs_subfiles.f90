!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

MODULE GLSHFS_SubFiles
      USE ErrorProcess
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GlerlDataTypesAndUnits
      USE DailyDataStructures
      USE DailyDataCollections
      IMPLICIT NONE

      PRIVATE
      
      PUBLIC :: ReadDailySubbasinFile
      PUBLIC :: ReadDailySubbasinFile_HeaderOnly
      PUBLIC :: WriteDailySubbasinFile
      
      
CONTAINS
!------------------------------------------------------------
!
!
!

!------------------------------------------------------------
!  Sometimes we just need the header info from a subbasin file, and 
!  don't care about the actual data values. This routine will give
!  us that without having to read the entire file.
!------------------------------------------------------------
      SUBROUTINE ReadDailySubbasinFile_HeaderOnly(FileName, StnData)
      IMPLICIT NONE
      CHARACTER(LEN=*),      INTENT(IN)    :: FileName
      TYPE (TDlyDataMetStn), INTENT(INOUT) :: StnData
      INTEGER :: I, J, IOS, U1
      INTEGER :: Year, Month, Day, NumStrings, NumDT
      LOGICAL :: F
      CHARACTER(LEN=100) :: S, TStr
      CHARACTER(LEN=500) :: Line
      CHARACTER(LEN=100), DIMENSION(99) :: CsvStrings
      TYPE(TDlyData) :: TDD
      INTEGER, DIMENSION(:), ALLOCATABLE :: TempDT, TempDU

      !
      !  Initialization
      !      
      CALL StnData%Clear()
      
      !
      !  Open the input file
      !
      U1 = GetFreeUnitNumber();  IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)

      !
      !  Line 1: Subbasin identification
      !   String should look like this: Subbasin_xxxyy, where
      !   xxx = lake basin code (sup, mic, hur, etc)
      !   yy  = 
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, .TRUE., NumStrings)
      IF (NumStrings .LT. 1) GOTO 601
      
      
      
      
      !
      !  Line 2: Station Latitude and Longitude
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, .TRUE., NumStrings)
      IF (NumStrings .LT. 3) GOTO 602
      S = GetUppercase(StripAllBlanks(CsvStrings(1)))
      IF (TRIM(S) .NE. 'LAT&LONG') GOTO 752
      TStr = TRIM(ADJUSTL(CsvStrings(2)));  READ(TStr, *, ERR=701) StnData%Latitude
      TStr = TRIM(ADJUSTL(CsvStrings(3)));  READ(TStr, *, ERR=702) StnData%Longitude
      
      !
      !  Line 3: Data start date
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, .TRUE., NumStrings)
      IF (NumStrings .LT. 3) GOTO 603
      S = GetUppercase(StripAllBlanks(CsvStrings(1)))
      IF (TRIM(S) .NE. 'STARTS(YMD)') GOTO 753
      TStr = TRIM(ADJUSTL(CsvStrings(2))); READ(TStr, *, ERR=703) Year
      TStr = TRIM(ADJUSTL(CsvStrings(3))); READ(TStr, *, ERR=703) Month
      TStr = TRIM(ADJUSTL(CsvStrings(4))); READ(TStr, *, ERR=703) Day
      
      IF ((Year .LT. 1) .OR. (Month .LT. 1) .OR. (Day .LT. 1)) THEN
         StnData%Earliest = MissingData_Date
      ELSE
         CALL DateSequence(Day, Month, Year, StnData%Earliest); IF (ErrorLevel .NE. 0) GOTO 703
      END IF
      
      !
      !  Line 4: Data end date
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, .TRUE., NumStrings)
      IF (NumStrings .LT. 3) GOTO 604
      S = GetUppercase(StripAllBlanks(CsvStrings(1)))
      IF (TRIM(S) .NE. 'ENDS(YMD)') GOTO 754
      TStr = TRIM(ADJUSTL(CsvStrings(2))); READ(TStr, *, ERR=703) Year
      TStr = TRIM(ADJUSTL(CsvStrings(3))); READ(TStr, *, ERR=703) Month
      TStr = TRIM(ADJUSTL(CsvStrings(4))); READ(TStr, *, ERR=703) Day
      
      IF ((Year .LT. 1) .OR. (Month .LT. 1) .OR. (Day .LT. 1)) THEN
         StnData%Earliest = MissingData_Date
      ELSE
         CALL DateSequence(Day, Month, Year, StnData%Latest); IF (ErrorLevel .NE. 0) GOTO 704
      END IF

      !
      !  If either date is missing, then just set NumDays to 0
      !
      IF ((StnData%Earliest .EQ. MissingData_Date) .OR. (StnData%Latest .EQ. MissingData_Date)) THEN
         StnData%NumDays = 0
      ELSE         
         StnData%NumDays = StnData%Latest - StnData%Earliest + 1
         IF (StnData%NumDays .LT. 1) THEN
            ErrorMessage = 'Invalid start/end dates in file: '//TRIM(FileName)
            CALL PassMsg
            ErrorMessage = 'End date prior to start date'; CALL PassMsg
            GOTO 898
         END IF
      END IF
      
      !
      !  Read line 5 to determine the number of data columns.
      !  Then use that to allocate the 2 temporary arrays (TempDT and TempDU) that will
      !  hold the defined data types and data units.
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, .TRUE., NumStrings)
      IF (NumStrings .LT. 3) GOTO 605
      IF (TRIM(GetUppercase(CsvStrings(1))) .NE. 'DATE') GOTO 755
      IF (TRIM(GetUppercase(CsvStrings(2))) .NE. 'DATE') GOTO 755
      IF (TRIM(GetUppercase(CsvStrings(3))) .NE. 'DATE') GOTO 755
      NumDT = NumStrings - 3
      
      ALLOCATE(TempDT(NumDT), TempDU(NumDT), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for temporary storage of metadata from: '//TRIM(FileName)
         CALL PassMsg; GOTO 898
      END IF

      !
      !  Line 5: Data types
      !     Now go back and read line 5 again, assigning the data types into the TempDT array.
      !
      BACKSPACE(U1)
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, .TRUE., NumStrings)
      IF (NumStrings .LT. 3) GOTO 605
      IF (TRIM(GetUppercase(CsvStrings(1))) .NE. 'DATE') GOTO 755
      IF (TRIM(GetUppercase(CsvStrings(2))) .NE. 'DATE') GOTO 755
      IF (TRIM(GetUppercase(CsvStrings(3))) .NE. 'DATE') GOTO 755
      DO I = 4, NumStrings
         J = I - 3
         TStr = TRIM(ADJUSTL(CsvStrings(I)))
         TempDT(J) = GlerlDataTypeFromString(TRIM(TStr))
      END DO

      !
      !  Line 6: Data units for each column in file
      !     Read line 6, assigning the data units into the TempDU array
      !
      READ(U1, 1000, ERR=812) Line
      CALL UpperCase(Line)
      CALL ParseCommaSepLine(Line, CsvStrings, .TRUE., NumStrings)
      IF (NumStrings .LT. 3) GOTO 606
      IF (TRIM(CsvStrings(1)) .NE. 'YYYY') GOTO 756
      IF (TRIM(CsvStrings(2)) .NE. 'MM')   GOTO 756
      IF (TRIM(CsvStrings(3)) .NE. 'DD')   GOTO 756
      DO I = 4, NumStrings
         J = I - 3
         TStr = TRIM(ADJUSTL(CsvStrings(I)))
         TempDU(J) = GlerlDataUnitFromString(TRIM(TStr))
      END DO

      !
      !  Assign the contents of TempDT and TempDU into StnData. This requires creating a
      !  TDlyData object for each station. But these TDlyData objects will have zero
      !  days of data in them. The data values will be assigned if/when the data lines
      !  are read and processed.  Not the job of this routine.
      !
      DO I = 1, NumDT
         TDD = TDlyData()
         F = TDD%SetDataType(TempDT(I));  IF (.NOT. F) GOTO 899
         F = TDD%SetDataUnits(TempDU(I)); IF (.NOT. F) GOTO 899
         F = StnData%AddDataset(TDD)
         IF (.NOT. F) THEN
            ErrorMessage = 'Error adding daily data objects for: '//TRIM(StnData%StnID)
            CALL PassMsg; GOTO 898
         END IF
         CALL TDD%Clear()
      END DO
      
      CLOSE(U1)
      CALL FileWasClosed(U1)
      
      GOTO 999
      
      !
      !
      !      
  601 ErrorMessage = 'Formatting error on line 1 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  602 ErrorMessage = 'Formatting error on line 2 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  603 ErrorMessage = 'Formatting error on line 3 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  604 ErrorMessage = 'Formatting error on line 4 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  605 ErrorMessage = 'Formatting error on line 5 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
  606 ErrorMessage = 'Formatting error on line 6 (too few entries) in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  701 ErrorMessage = 'Error parsing Latitude on line 2 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  702 ErrorMessage = 'Error parsing Longitude on line 2 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  703 ErrorMessage = 'Error parsing start date on line 3 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  704 ErrorMessage = 'Error parsing end date on line 4 in '//TRIM(FileName); CALL PassMsg; GOTO 898
   
  752 ErrorMessage = 'Invalid format for line 2 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  753 ErrorMessage = 'Invalid format for line 3 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  754 ErrorMessage = 'Invalid format for line 4 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  755 ErrorMessage = 'Invalid format for line 5 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  756 ErrorMessage = 'Invalid format for line 5 in '//TRIM(FileName); CALL PassMsg; GOTO 898
  
  811 ErrorMessage = 'Error opening input file '//TRIM(FileName); CALL PassMsg; GOTO 898
  812 ErrorMessage = 'Error reading input file '//TRIM(FileName); CALL PassMsg; GOTO 898
   
    
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ReadDailyStationFile_HeaderOnly'; CALL PassMsg
      CALL StnData%Clear()
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
      
  999 DEALLOCATE(TempDT, TempDU, STAT=IOS)
      CALL TDD%Clear()
      RETURN

 1000 FORMAT(A500)
     
      END SUBROUTINE ReadDailyStationFile_HeaderOnly



!------------------------------------------------------------
      SUBROUTINE ReadDailyStationFile(FileName, StnData)
      IMPLICIT NONE
      CHARACTER(LEN=*),      INTENT(IN)    :: FileName
      TYPE (TDlyDataMetStn), INTENT(INOUT) :: StnData
      INTEGER :: I, J, K, IOS, U1, ErrD, ErrM, ErrY
      INTEGER :: NumStrings, NumDT, DType
      INTEGER :: Day, Month, Year, Seq, SSeq
      LOGICAL :: F
      REAL :: DVal
      CHARACTER(LEN=100) :: TStr, TStrD, TStrM, TStrY
      CHARACTER(LEN=500) :: Line
      CHARACTER(LEN=100), DIMENSION(99) :: CsvStrings
      REAL,    DIMENSION(:,:), ALLOCATABLE :: Temp2D
      TYPE (TDlyData), POINTER :: TDD
      
      !
      !  Initialize stuff
      !
      U1 = 0
      CALL StnData%Clear()

      !
      !  Use the routine for reading the station header info.
      !  Slightly less efficient (computationally) than repeating all of that
      !  code logic here, but better for maintenance, etc.
      !  Then, after we open the file (again), we can just skip over the
      !  header lines.
      !
      CALL ReadDailyStationFile_HeaderOnly(FileName, StnData);  IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  If we determined that there were zero days of data, based on the header info, then
      !  no need to do any of the data value read stuff.
      !
      IF (StnData%NumDays .EQ. 0) RETURN
      
      !
      !  Open the input file, and skip those 6 header lines
      !
      U1 = GetFreeUnitNumber();  IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)
      DO I = 1, 6
         READ(U1, *)
      END DO

      !
      !  Assign a few local variables that we need for processing the data
      !  lines. We can get them from the info already read into StnData.
      !
      NumDT = StnData%NumDatasets
      
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
      !  Read the data values, storing them in Temp2D
      !
      READ(U1, 1000, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         CALL ParseCommaSepLine(Line, CsvStrings, .TRUE., NumStrings)
         TStrY = TRIM(ADJUSTL(CsvStrings(1)))
         TStrM = TRIM(ADJUSTL(CsvStrings(2)))
         TStrD = TRIM(ADJUSTL(CsvStrings(3)))
         READ(TStrY, *, IOSTAT=ErrY) Year
         READ(TStrM, *, IOSTAT=ErrM) Month
         READ(TStrD, *, IOSTAT=ErrD) Day
         IF (ErrY*ErrM*ErrD .EQ. 0) THEN
            CALL DateSequence(Day, Month, Year, Seq)
            IF (ErrorLevel .EQ. 0) THEN
               Day = Seq - StnData%Earliest + 1
               DO K = 4, NumStrings
                  J = K - 3
                  TStr = TRIM(ADJUSTL(CsvStrings(K)))
                  READ(TStr, *, IOSTAT=IOS) DVal
                  IF (IOS .NE. 0) DVal = MissingData_REAL
                  Temp2D(Day,J) = DVal
               END DO
            ELSE
               ErrorLevel = 0      ! reset it for next line
            END IF
         END IF
         READ(U1, 1000, IOSTAT=IOS) Line
      END DO

      !
      !  Now rewind the file and read just the data type strings (line 5)
      !  Then, as I work across those columns, match up the data with the TDlyData
      !  objects that were already created and assigned metadata. Put the
      !  right data into those.
      !
      REWIND(U1)
      DO I = 1, 4
         READ(U1, *)
      END DO
      READ(U1, 1000) Line
      CALL ParseCommaSepLine(Line, CsvStrings, .TRUE., NumStrings)
      DO I = 4, NumStrings
         J = I - 3
         TStr = TRIM(ADJUSTL(CsvStrings(I)))
         DType = GlerlDataTypeFromString(TRIM(TStr))      ! data type for column I (J'th data column)
         TDD => StnData%GetPointerToDataOfType(DType)
         IF (ASSOCIATED(TDD)) THEN
            F = TDD%AssignData(StnData%Earliest, StnData%Latest, Temp2D(:,J))  ! the array slice for this data type
            IF (.NOT. F) THEN
               ErrorMessage = 'Error assigning daily data to object for: '//TRIM(StnData%StnID)
               CALL PassMsg; GOTO 898
            END IF
         ELSE
            WRITE(ErrorMessage, 5001) TRIM(GlerlDataTypeString(DType)), TRIM(StnData%StnID)
            CALL PassMsg
            ErrorMessage = 'Continuing anyway.'; CALL PassMsg
         END IF
      END DO

      CLOSE(U1)
      CALL FileWasClosed(U1)
      
      DEALLOCATE(Temp2D, STAT=IOS)
      
      
   
   CALL WriteDailyStationFile('met_mytest.csv', StnData)
      
      
      RETURN
     
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file '//TRIM(FileName); CALL PassMsg; GOTO 898
   
    
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : ReadDailyStationFile'; CALL PassMsg
      CALL StnData%Clear()
      DEALLOCATE(Temp2D, STAT=IOS)
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
      RETURN

 1000 FORMAT(A500)
 5001 FORMAT('Unable to find TDlyData object for ', A, ' for station: ', A)
      END SUBROUTINE ReadDailyStationFile

!-----------------------------------------------------------------------------
!  Subroutine that will accept a filename (for the output file) and a structured
!  set of station data, then output a comma-separated file of the station data.
!-----------------------------------------------------------------------------
      SUBROUTINE WriteDailyStationFile(FileName, StnData)
      IMPLICIT NONE
      CHARACTER(LEN=*),      INTENT(IN) :: FileName
      TYPE (TDlyDataMetStn), INTENT(IN) :: StnData
      
      INTEGER :: I, J, K, U1, Dy, Mn, Yr, Seq
      INTEGER :: NDT
      REAL    :: DVal
      CHARACTER(LEN=1)   :: Comma, DblQ
      CHARACTER(LEN=15)  :: LabelLatLong, LabelSDate, LabelEDate
      CHARACTER(LEN=30)  :: S
      CHARACTER(LEN=22)  :: QuotedID
      CHARACTER(LEN=82)  :: QuotedName
      CHARACTER(LEN=999) :: Line
      TYPE (TDlyData), POINTER :: TDD

      Comma = ','
      DblQ  = '"'
      
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)
      
      !
      !  Some of the metadata items could potentially have imbedded spaces in them. We want
      !  those imbedded spaces to be ignored and the item treated as a single entity by
      !  any software that reads this file. So we will surround those items with double
      !  quotes. Excel strips off the double quotes when displaying the data.
      !
      !  Note that for consistency we will also surround the text labels with 
      !  double-quotes.
      !
      QuotedID     = DblQ // TRIM(StnData%StnID)   // DblQ
      QuotedName   = DblQ // TRIM(StnData%StnName) // DblQ
      LabelLatLong = DblQ // 'Lat&Long'            // DblQ
      LabelSDate   = DblQ // 'Starts (YMD)'        // DblQ
      LabelEDate   = DblQ // 'Ends (YMD)'          // DblQ
      
      !
      !  
      !
      WRITE(U1, 1001, ERR=813) TRIM(QuotedID), Comma, TRIM(QuotedName)
      WRITE(U1, 1002, ERR=813) LabelLatLong, Comma, StnData%Latitude, Comma, StnData%Longitude

      IF (StnData%Earliest .NE. MissingData_Date) THEN
         CALL SequenceDate(Dy, Mn, Yr, StnData%Earliest)
      ELSE
         Dy=-99; Mn=-99; Yr = -9999
      ENDIF
      WRITE(U1, 1003, ERR=813) LabelSDate, Comma, Yr, Comma, Mn, Comma, Dy
   
      IF (StnData%Latest .NE. MissingData_Date) THEN
         CALL SequenceDate(Dy, Mn, Yr, StnData%Latest)
      ELSE
         Dy=-99; Mn=-99; Yr = -9999
      ENDIF
      WRITE(U1, 1003, ERR=813) LabelEDate, Comma, Yr, Comma, Mn, Comma, Dy
   
      NDT = StnData%NumDatasets
      Line = ''
      DO I = 1, 3
         Line = TRIM(Line) // DblQ//'Date'//DblQ//Comma
      END DO
      
      DO I = 1, NDT-1
         TDD => StnData%GetPointerToDataByIndex(I)
         S = DblQ // TRIM(GlerlDataTypeString(TDD%GetDataType())) // DblQ
         Line = TRIM(Line) // TRIM(S) // Comma
      END DO
      TDD => StnData%GetPointerToDataByIndex(NDT)
      S = DblQ // TRIM(GlerlDataTypeString(TDD%GetDataType())) // DblQ
      Line = TRIM(Line) // TRIM(S)
      WRITE(U1, 1005, ERR=813) TRIM(Line)

      Line = DblQ//'YYYY'//DblQ//Comma
      Line = TRIM(Line) // DblQ//'MM'//DblQ//Comma
      Line = TRIM(Line) // DblQ//'DD'//DblQ//Comma
      DO I = 1, NDT-1
         TDD => StnData%GetPointerToDataByIndex(I)
         S = DblQ // TRIM(GlerlDataUnitString(TDD%GetDataUnits())) // DblQ
         Line = TRIM(Line) // TRIM(S) // Comma
      END DO
      TDD => StnData%GetPointerToDataByIndex(NDT)
      S = DblQ // TRIM(GlerlDataUnitString(TDD%GetDataUnits())) // DblQ
      Line = TRIM(Line) // TRIM(S)
      WRITE(U1, 1005, ERR=813) TRIM(Line)
      
      !
      !  Only do this section if we have actual data to write.
      !
      !  Data value rows
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

      !  This is a sad omission in the Fortran standard that I saw discussed
      !  online numerous places from people trying to do similar stuff.  Lots of
      !  complaints about it, but no indication that the Fortran standards committee
      !  thinks it should be changed. It is left as a compiler-dependent option with no
      !  hint that there might be any addition to the FORMAT specification standard.
      !
      !  Additionally, due to the way Fortran formatting is implemented, I have to do
      !  special stuff to avoid a trailing comma on the last entry. You would think it
      !  could be done by specifying it as a leading comma and structuring things
      !  appropriately, it doesn't work quite as you might expect. So I am stuck with
      !  a lot of extra messiness, unfortunately. 
      !
      IF ((StnData%Earliest .NE. MissingData_Date) .AND. (StnData%Latest .NE. MissingData_Date)) THEN
         DO Seq = StnData%Earliest, StnData%Latest
            I = Seq - StnData%Earliest + 1
            CALL SequenceDate(Dy, Mn, Yr, Seq)
            Line = ''
            DO J = 1, NDT
               TDD => StnData%GetPointerToDataByIndex(J)
!  print*, 'ID,SSeq,ESeq: ', StnData%StnID, TDD%GetStartDate(), TDD%GetEndDate()
               S = ''
               IF ((TDD%GetStartDate() .LE. Seq) .AND. (Seq .LE. TDD%GetEndDate())) THEN
                  K = Seq - TDD%GetStartDate() + 1
                  DVal = TDD%GetDataVal(K)
                  IF (IsMissing(DVal)) THEN
                     S = TRIM(FixedMissingValueString)                ! from GLSHFS_Global
                  ELSE
                     IF (ABS(DVal) .GT. 1.0) THEN
                        WRITE(S, 1050) DVal
                     ELSE
                        IF (DVal .GE. 0) THEN
                           WRITE(S, 1051) INT(DVal), DVal-INT(DVal)
                        ELSE
                           DVal = ABS(DVal)
                           WRITE(S, 1052) INT(DVal), DVal-INT(DVal)
                        END IF
                     END IF
                  END IF
               ELSE
                  S = TRIM(FixedMissingValueString)                ! from GLSHFS_Global
               END IF
               IF (J .LT. NDT) S = TRIM(S) // ','
               Line = TRIM(Line) // TRIM(S)
            END DO
            WRITE(U1, 1010, ERR=813) Yr, Mn, Dy, TRIM(Line)
         END DO
      END IF
      
      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN
   
      !
      !
  811 ErrorMessage = 'Error opening output file '//TRIM(FileName); CALL PassMsg; GOTO 899
  813 ErrorMessage = 'Error writing output file '//TRIM(FileName); CALL PassMsg; GOTO 899

  899 ErrorMessage = '[traceback] : WriteDailyStationFile'; CALL PassMsg
      ErrorLevel = 1
      RETURN
   
   
 1001 FORMAT(A, A1, A)
 1002 FORMAT(A, A1, F0.3, A1, F0.3)
 1003 FORMAT(A, A1, I0, A1, I0, A1, I0)
 1005 FORMAT(A)
 1010 FORMAT(I4.4, 2(',',I2.2), ',', A)
 1050 FORMAT(F0.2)
 1051 FORMAT(I0.1, F0.2)
 1052 FORMAT('-', I0.1, F0.2)
 
 
      END SUBROUTINE WriteDailyStationFile


!----------------------------------------------------------------------------------
!  This routine reads a CSV file that contains all of the daily station data 
!  for a single data type. Typically, the file will have the data for a single lake 
!  basin or similar area.
!
!  The data structure passed in is of type TDlyDataForMultipleStations. Each member
!  of that object is a TDlyDataMetStn object. Each of those, in turn, will contain
!  ONLY ONE DATA TYPE for purposes of this routine.
!
!----------------------------------------------------------------------------------
      SUBROUTINE ReadFile_OneDataTypeManyStations(Filename, TDDMS)
      IMPLICIT NONE
      CHARACTER (LEN=*),                  INTENT(IN)    :: Filename
      TYPE (TDlyDataForMultipleStations), INTENT(INOUT) :: TDDMS
      
      INTEGER, PARAMETER :: MaxNumStns = 3000
      INTEGER :: I, J, IOS, U1, DType, DUnit, S, D
      INTEGER :: NumEntries, LineNumber, ColumnNumber
      INTEGER :: Seq, SSeq, ESeq, NumDays, NumStations
      LOGICAL :: OK, Flag
      REAL    :: Lt, Ln
      TYPE (TDlyDataMetStn)          :: DMS
      TYPE (TDlyDataMetStn), POINTER :: MSP
      TYPE (TDlyData),       POINTER :: TDD
      
      CHARACTER(LEN=100), DIMENSION(MaxNumStns+1) :: Strings
      CHARACTER(LEN=9999) :: Line
      
      REAL, DIMENSION(:,:), ALLOCATABLE :: StnData     ! indexed (1:NumStations, 1:NumDays)
      
      !
      !  Open the input file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(Filename), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Clear the data object. It is assumed that the object was instantiated
      !  before this routine was called.
      !
      CALL TDDMS%Clear()
      READ(U1, 1000, ERR=812) TDDMS%Description
      
      !
      !  Get the data type of data that we are reading
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, .TRUE., NumEntries); IF (ErrorLevel .NE. 0) GOTO 899
      DType = GlerlDataTypeFromString(TRIM(Strings(1)))
      IF (DType .EQ. GDT_Undefined) GOTO 851

      !
      !  Get the units of the data that we are reading
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, .TRUE., NumEntries); IF (ErrorLevel .NE. 0) GOTO 899
      DUnit = GlerlDataUnitFromString(TRIM(Strings(1)))
      IF (DUnit .EQ. GDU_Undefined) GOTO 852

      !
      !  Get the Station IDs
      !  We use this to determine how many stations are in the file.  Column 1 is
      !  just the label, but the rest of the columns are stations.
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, .TRUE., NumEntries); IF (ErrorLevel .NE. 0) GOTO 899
      NumStations = NumEntries - 1

      !
      !  Create a spot for each station in the big object. Assign the station ID values.
      !      
      DMS = TDlyDataMetStn()      ! "template object"
      DO I = 2, NumEntries
         J = I - 1
         Flag = TDDMS%AddStation(DMS);   IF (ErrorLevel .NE. 0) GOTO 899
         IF (.NOT. Flag) GOTO 899
         MSP => TDDMS%GetStationPtr(J);  IF (ErrorLevel .NE. 0) GOTO 899
         MSP%StnID = TRIM(Strings(I))
      END DO
      
      !
      !  Get and assign the station names
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, .TRUE., NumEntries); IF (ErrorLevel .NE. 0) GOTO 899
      DO I = 2, NumEntries
         J = I -1
         MSP => TDDMS%GetStationPtr(J);  IF (ErrorLevel .NE. 0) GOTO 899
         MSP%StnName = TRIM(Strings(I))
      END DO

      !
      !  Get and assign the latitudes
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, .TRUE., NumEntries); IF (ErrorLevel .NE. 0) GOTO 899
      DO I = 2, NumEntries
         ColumnNumber = I
         READ(Strings(I), *, ERR=853) Lt
         J = I -1
         MSP => TDDMS%GetStationPtr(J);  IF (ErrorLevel .NE. 0) GOTO 899
         MSP%Latitude = Lt
      END DO

      !
      !  Get and assign the longitudes
      !
      READ(U1, 1000, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, .TRUE., NumEntries); IF (ErrorLevel .NE. 0) GOTO 899
      DO I = 2, NumEntries
         ColumnNumber = I
         READ(Strings(I), *, ERR=854) Ln
         J = I -1
         MSP => TDDMS%GetStationPtr(J);  IF (ErrorLevel .NE. 0) GOTO 899
         MSP%Longitude = Ln
      END DO

      !
      !  Skip a line
      !
      READ(U1, *, ERR=812)
      
      !
      !  Find the date extents
      !
      SSeq = DateSeq_MaxValue
      ESeq = DateSeq_MinValue
      LineNumber = 9
      READ(U1, 1000, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         CALL ParseCommaSepLine(Line, Strings, .TRUE., NumEntries); IF (ErrorLevel .NE. 0) GOTO 899
         Seq = DateStringMDYToSeq(Strings(1)); IF (ErrorLevel .NE. 0) GOTO 855
         IF (Seq .EQ. MissingData_Date) GOTO 855
         IF (Seq .LT. SSeq) SSeq = Seq
         IF (Seq .GT. SSeq) ESeq = Seq
         LineNumber = LineNumber + 1
         READ(U1, 1000, IOSTAT=IOS) Line
      END DO
      
      NumDays = ESeq - SSeq + 1
      IF (NumDays .LE. 0) GOTO 856

      !
      !  Allocate the temporary 2-D data array that will hold the station data
      !  as we read the file. This is far more efficient than using the member
      !  functions of TDlyData to assign the data as it is read.
      !
      ALLOCATE(StnData(NumStations, NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) NumStations, NumDays; CALL PassMsg
         GOTO 898
      END IF
      StnData(:,:) = MissingData_Real

      !
      !  Now read the data and store into that temporary array
      !  
      REWIND(U1)
      DO I = 1, 8
         READ(U1, *, ERR=812)
      END DO
      DO D = 1, NumDays
         LineNumber = D + 8
         READ(U1, 1000, ERR=812) Line
         CALL UpperCase(Line)    !  useful to do this once, rather than for each column
         CALL ParseCommaSepLine(Line, Strings, .TRUE., NumEntries); IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumEntries .NE. NumStations + 1) GOTO 857
         Seq = DateStringMDYToSeq(Strings(1)); IF (ErrorLevel .NE. 0) GOTO 855
         IF (Seq .EQ. MissingData_Date) GOTO 855
         DO S = 1, NumStations
            READ(Strings(S+1), *, IOSTAT=IOS) StnData(S,D)
            IF (IOS .NE. 0) StnData(S,D) = MissingData_Real
         END DO
      END DO
      
      CLOSE(U1)
      CALL FileWasClosed(U1)      
      
      !
      !  Now transfer the data into the data storage object.
      !
      DO S = 1, NumStations
         MSP => TDDMS%GetStationPtr(S);  IF (ErrorLevel .NE. 0) GOTO 899
         TDD => MSP%GetPointerToDataByIndex(1)
         OK = TDD%AssignData(SSeq, ESeq, StnData(S,:))
         IF ((ErrorLevel .NE. 0) .OR. (.NOT. OK)) GOTO 899
      END DO

      !
      !  Final cleanup of local stuff
      !
      GOTO 999 
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file '//TRIM(Filename); CALL PassMsg; GOTO 898
  812 ErrorMessage = 'Error reading input file '//TRIM(Filename); CALL PassMsg; GOTO 898

  851 WRITE(ErrorMessage, 1051) TRIM(Strings(1)), TRIM(Filename);  CALL PassMsg
      GOTO 898
  852 WRITE(ErrorMessage, 1052) TRIM(Strings(1)), TRIM(Filename);  CALL PassMsg
      GOTO 898
  853 WRITE(ErrorMessage, 1053) TRIM(Filename), ColumnNumber;  CALL PassMsg
      GOTO 898
  854 WRITE(ErrorMessage, 1054) TRIM(Filename), ColumnNumber;  CALL PassMsg
      GOTO 898
  855 WRITE(ErrorMessage, 1055) TRIM(Filename), LineNumber;  CALL PassMsg
      GOTO 898
  856 WRITE(ErrorMessage, 1056) TRIM(Filename);  CALL PassMsg
      GOTO 898
  857 WRITE(ErrorMessage, 1057) LineNumber, TRIM(Filename);  CALL PassMsg
      GOTO 898
      
      

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ReadFile_OneDataTypeManyStations()...'; CALL PassMsg
      CALL TDDMS%Clear()

  999 DEALLOCATE(StnData, STAT=IOS)
      RETURN
      
      !
      !  FORMATs
      !
 1000 FORMAT(A9999)
 1051 FORMAT('Invalid data type string [', A, '] read from file ', A)
 1052 FORMAT('Invalid data units string [', A, '] read from file ', A)
 1053 FORMAT('Invalid Latitude entry in file ',  A, '; Column ', I0)
 1054 FORMAT('Invalid Longitude entry in file ', A, '; Column ', I0)
 1055 FORMAT('Invalid date entry in file ', A, '; Line ', I0)
 1056 FORMAT('Invalid dates in file ', A, '; End < Start.')
 1057 FORMAT('Error reading line ', I0, ' of file ', A)
 1071 FORMAT('Error allocating temporary array for ',I0, ' stations and ', I0, ' days.')

      
      END SUBROUTINE ReadFile_OneDataTypeManyStations
      
      
!----------------------------------------------------------------------------------
!  This routine writes a CSV file that contains all of the daily station data 
!  for a single data type. Typically, the file will have the data for a single lake 
!  basin or similar area.
!
!  The data structure passed in is of type TDlyDataForMultipleStations. Each member
!  of that object is a TDlyDataMetStn object. The specified data type, from each
!  of the TDlyDataMetStn object that contains data for that datatype, will be output.
!----------------------------------------------------------------------------------
      SUBROUTINE WriteFile_OneDataTypeManyStations(Filename, Stations, DType)
      IMPLICIT NONE
      CHARACTER (LEN=*),                  INTENT(IN) :: Filename
      TYPE (TDlyDataForMultipleStations), INTENT(IN) :: Stations
      INTEGER,                            INTENT(IN) :: DType
      
      INTEGER :: I, J, IOS, U1, S, D, Indx
      INTEGER :: Seq, SSeq, ESeq, Dy, Mn, Yr, SD, ED, NumDays
      LOGICAL :: Flag
      CHARACTER(LEN=80)   :: Str
      CHARACTER(LEN=9999) :: Line
      REAL, DIMENSION(:),   ALLOCATABLE :: TDV
      REAL, DIMENSION(:,:), ALLOCATABLE :: StnData     ! indexed (1:NumStations, 1:NumDays)

      TYPE (TDlyDataMetStn), POINTER     :: MSP
      TYPE (TDlyData), POINTER           :: TDD
      TYPE (TDlyDataMetStn)              :: TStn
      TYPE (TDlyDataForMultipleStations) :: DataOut
      
      !
      !  Create a temporary object that will hold the USEFUL data from the Stations object
      !
      DataOut = TDlyDataForMultipleStations(); IF (ErrorLevel .NE. 0) GOTO 899
      DataOut%Description = 'Daily station data for multiple stations.'

      !
      !  Cycle through the Stations object, copying the relevant
      !  data to the DataOut object.
      !  Each entry in DataOut will be a TDlyDataMetStn object that 
      !  contains a single time series of data (for the data type
      !  of interest.)
      !
      !  Note that MSP is a POINTER variable, and we never allocate any RAM for it.
      !  Thus, no clean up is required for this variable.
      !  TStn, on the other hand, is an instantiated variable, and DOES need to be
      !  cleaned up after the loop.
      !  
      TStn = TDlyDataMetStn(); IF (ErrorLevel .NE. 0) GOTO 899
      DO I = 1, Stations%NumStations
         MSP => Stations%GetStationPtr(I)      ! data for a station
         TDD => MSP%GetPointerToDataOfType(DType)
         IF (ASSOCIATED(TDD)) THEN
            CALL TStn%Clear()
            Flag = TStn%CopyMetaFrom(MSP);       IF (.NOT. Flag) GOTO 899
            Flag = TStn%AddDataset(TDD);         IF (.NOT. Flag) GOTO 899
            Flag = DataOut%AddStation(TStn);     IF (.NOT. Flag) GOTO 899
         END IF
      END DO
      CALL TStn%Clear()     

      !
      !  If no matching data was found, clean up and return.
      !  Do not write a file.
      !
      IF (DataOut%NumStations .EQ. 0) THEN
         ErrorMessage = 'Warning: No relevant station data was found, so no output file is written.'
         CALL PassMsg
         CALL DataOut%Clear()
         RETURN
      END IF
      
      !
      !  Now find the data extents of the DataOut object
      !  
      MSP => DataOut%GetStationPtr(1)
      SSeq = MSP%Earliest
      ESeq = MSP%Latest
      IF ((SSeq .EQ. MissingData_Date) .OR. (ESeq .EQ. MissingData_Date)) THEN
         NumDays = 0
      ELSE
         DO I = 2, Dataout%NumStations
            MSP => DataOut%GetStationPtr(I)
            SD = MSP%Earliest
            ED = MSP%Latest
            IF (SD .LT. SSeq) SSeq = SD
            IF (ED .GT. ESeq) ESeq = ED
         END DO
         NumDays = ESeq - SSeq + 1
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
      MSP => DataOut%GetStationPtr(1)
      TDD => MSP%GetPointerToDataByIndex(1)
      WRITE(U1, 1001, ERR=813) TRIM(DataOut%Description)
      WRITE(U1, 1002, ERR=813) TRIM(GlerlDataTypeString(TDD%GetDataType()))
      WRITE(U1, 1003, ERR=813) TRIM(GlerlDataUnitString(TDD%GetDataUnits()))

      WRITE(Line, 1004, IOSTAT=IOS) TRIM(MSP%StnID)
      IF (IOS .NE. 0) Line = 'StationID:,???'
      DO I = 2, DataOut%NumStations
         MSP => DataOut%GetStationPtr(I)
         WRITE(Str, 1014, IOSTAT=IOS) TRIM(MSP%StnID)
         IF (IOS .EQ. 0) THEN
            Line = TRIM(Line) // TRIM(Str)
         ELSE
            Line = TRIM(Line) // ',???'
         END IF
      END DO
      WRITE(U1, 1001, ERR=813) TRIM(Line)
      
      MSP => DataOut%GetStationPtr(1)
      WRITE(Line, 1005, IOSTAT=IOS) TRIM(MSP%StnName)
      IF (IOS .NE. 0) Line = 'StationName:,---'
      DO I = 2, DataOut%NumStations
         MSP => DataOut%GetStationPtr(I)
         WRITE(Str, 1015, IOSTAT=IOS) TRIM(MSP%StnName)
         IF (IOS .EQ. 0) THEN
            Line = TRIM(Line) // TRIM(Str)
         ELSE
            Line = TRIM(Line) // ',-----'
         END IF
      END DO
      WRITE(U1, 1001, ERR=813) TRIM(Line)

      MSP => DataOut%GetStationPtr(1)
      WRITE(Line, 1006, IOSTAT=IOS) MSP%Latitude
      IF (IOS .NE. 0) Line = 'Latitude:,'//TRIM(FixedMissingValueString)
      DO I = 2, DataOut%NumStations
         MSP => DataOut%GetStationPtr(I)
         WRITE(Str, 1016, IOSTAT=IOS) MSP%Latitude
         IF (IOS .EQ. 0) THEN
            Line = TRIM(Line) // TRIM(Str)
         ELSE
            Line = TRIM(Line) // ','//TRIM(FixedMissingValueString)
         END IF
      END DO
      WRITE(U1, 1001, ERR=813) TRIM(Line)

      MSP => DataOut%GetStationPtr(1)
      WRITE(Line, 1007, IOSTAT=IOS) MSP%Longitude
      IF (IOS .NE. 0) Line = 'Longitude:,'//TRIM(FixedMissingValueString)
      DO I = 2, DataOut%NumStations
         MSP => DataOut%GetStationPtr(I)
         WRITE(Str, 1016, IOSTAT=IOS) MSP%Longitude
         IF (IOS .EQ. 0) THEN
            Line = TRIM(Line) // TRIM(Str)
         ELSE
            Line = TRIM(Line) // ','//TRIM(FixedMissingValueString)
         END IF
      END DO
      WRITE(U1, 1001, ERR=813) TRIM(Line)

      WRITE(U1, 1008, ERR=813)      

      !
      !  Allocate a temporary 2-D data array that will hold a copy of all
      !  the station data values. This may seem a little inefficient, but is
      !  actually better because once we assign the values, we can efficiently
      !  access the data as an array slice for each day.  Much better than
      !  accessing the data objects every day.
      !
      ALLOCATE(StnData(DataOut%NumStations, NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 1071) DataOut%NumStations, NumDays; CALL PassMsg
         GOTO 898
      END IF
      StnData(:,:) = MissingData_Real
      
      !
      !  Assign the data from the stations
      !
      DO S = 1, DataOut%NumStations
         MSP => DataOut%GetStationPtr(S)
         TDD => MSP%GetPointerToDataByIndex(1)
         I = TDD%GetStartDate() - SSeq + 1
         J = TDD%GetEndDate()   - SSeq + 1
         TDV = TDD%GetDataVals()
         StnData(S,I:J) = TDV
      END DO

      !
      !  The data lines
      !
      DO Seq = SSeq, ESeq
         CALL SequenceDate(Dy, Mn, Yr, Seq); IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(Line, 1009, IOSTAT=IOS) Mn, Dy, Yr
         IF (IOS .NE. 0) Line = '99/99/9999'
         D = Seq - SSeq + 1
         DO S = 1, DataOut%NumStations
            IF (IsMissing(StnData(S,D))) THEN
               Str = ',' // TRIM(FixedMissingValueString)
            ELSE
               WRITE(Str, 1019, IOSTAT=IOS) StnData(S,D)
            END IF
            Line = TRIM(Line) // TRIM(Str)
         END DO
         WRITE(U1, 1001, ERR=813) TRIM(Line)
      END DO
      
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
  899 ErrorMessage = '[traceback] WriteFile_OneDataTypeManyStations()...'; CALL PassMsg
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF

  999 DEALLOCATE(StnData, TDV, STAT=IOS)
      CALL TDD%Clear()
      !
      !  FORMATs
      !
 1001 FORMAT(A)
 1002 FORMAT(A, ', <-DataType')
 1003 FORMAT(A, ', <-Units')
 1004 FORMAT('StationID:,', A)
 1005 FORMAT('StationName:,', '"', A, '"')
 1006 FORMAT('Latitude:,', F8.3)
 1007 FORMAT('Longitude:,', F8.3)
 1008 FORMAT('MM/DD/YYYY')
 1009 FORMAT(I2.2,'/',I2.2,'/',I4.4)
 
 1014 FORMAT(',', A) 
 1015 FORMAT(',"', A, '"') 
 1016 FORMAT(',', F8.3)  
 1019 FORMAT(',', F0.2)
  
 1071 FORMAT('Error allocating temporary array for ',I0, ' stations and ', I0, ' days.')
      
      END SUBROUTINE WriteFile_OneDataTypeManyStations
      
END MODULE GLSHFS_StnFiles