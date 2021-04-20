!------------------------------------------------------------------------------
!  Process station meteorology files (met_*.csv format) into one file per data
!  type, where each of those files contains the data (that type) for all stations.
!------------------------------------------------------------------------------

MODULE StationData_Aggregation

      USE ErrorProcess
      USE StatusMessages
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GlerlDataTypesAndUnits
      USE DailyDataStructures
      USE DailyDataCollections
      USE GLSHFS_ConfigFile
      USE GLSHFS_Files
      USE BasinInfo

      !
      !  By default, hide everything. Things that need to be public outside
      !  the module will be explicitly declared PUBLIC.
      !
      PRIVATE
      PUBLIC :: AggregateTheStationData

      !
      !  Collection of multiple station file data sets.
      !  All of the stations, all data types.
      !
      TYPE (TDlyDataForMultipleStations) :: AllStations


CONTAINS
      SUBROUTINE AggregateTheStationData(LakeCode)
      IMPLICIT NONE
      CHARACTER(LEN=3) :: LakeCode

      INTEGER :: I, U1, IOS, LkNum, StationsToProcess, NumStrings
      LOGICAL :: FExist, Flag
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=10)  :: Lake10
      CHARACTER(LEN=200) :: SFName, File1, StnDir, Line, Strings(3)
      CHARACTER(LEN=200), DIMENSION(MaxNumberOfStations) :: StationFileNames
      
      TYPE (TDlyDataMetStn) :: StnD
      
      !
      !  Initialization
      ! 
      StnD = TDlyDataMetStn(); IF (ErrorLevel .NE. 0) GOTO 899
      StnDir = TRIM(GLSHFS_Config%StnDir)       ! just get a shorter name to work with
      Bsn    = GetLowercase(LakeCode)
      LkNum  = LakeNumberFromName3(Bsn)
      Lake10 = LakeName10(LkNum)
      
      I = LakeNumberFromName3(Bsn)
      IF (I .LT. 1) THEN
         ErrorMessage = 'Error: Invalid basin specified ['//TRIM(Bsn)//']'; CALL PassMsg
         GOTO 898
      END IF

      !
      !  Open the station list file. It contains the filenames of the stations to be
      !  processed.  All stations listed in the file will be used.  Any filtering based
      !  on station location or other criteria needs to be done prior to this routine.
      !
      !  Skip the first line of the file (just a header line)
      !
      File1 = TRIM(StnDir) // 'list_of_stations_' // Bsn // '.csv'
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(File1), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      READ(U1, *, ERR=812)     
      
      !
      !  Read the list file and save the file names.
      !  Make sure that the files exist.
      !
      StationsToProcess = 0
      StationFileNames(:) = ''
      READ(U1, 1101, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         CALL ParseCommaSepLine(Line, Strings, NumStrings)
         IF (NumStrings .GE. 1) THEN
            SFName = Strings(1)
            INQUIRE(FILE=TRIM(SFName), EXIST=FExist)
            IF (FExist) THEN
               StationsToProcess = StationsToProcess + 1
               StationFileNames(StationsToProcess) = TRIM(SFName)
            END IF
         END IF
         READ(U1, 1101, IOSTAT=IOS) Line
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  If there are zero stations to process, skip the rest of the routine.
      !
      IF (StationsToProcess .EQ. 0) GOTO 999
 
      !
      !  Set up the object for all stations, all data types
      !
      AllStations = TDlyDataForMultipleStations(); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Read all of the station files, creating an object for each station and then
      !  adding that object to the AllStations object (if the station had valid data).
      !
      !  At this stage we are just keeping all of the data, and not filtering anything.
      !  Each station has an independent period of record, etc.
      !  Also keep in mind that the TDlyDataMetStn structure contains ID, 
      !  Name, Lat/Long, and other header info as well as the data.
      !
      StnD = TDlyDataMetStn(); IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(StatusMsg, 1201) LakeCode, 1, StationsToProcess; CALL WriteMsg(.TRUE., .FALSE.)
      DO I = 1, StationsToProcess
         WRITE(StatusMsg, 1201) LakeCode, I, StationsToProcess; CALL WriteMsg(.TRUE., .TRUE.)
         File1 = StationFileNames(I)
         CALL ReadFile_DailyStation(File1, StnD); IF (ErrorLevel .NE. 0) GOTO 899
         IF (StnD%NumDays .GT. 0) THEN
            Flag = AllStations%AddStation(StnD); IF (.NOT. Flag) GOTO 899
         END IF
      END DO
      CALL StnD%Clear()
      WRITE(StatusMsg, 1201) LakeCode, StationsToProcess, StationsToProcess
      StatusMsg = CHAR(13) // TRIM(StatusMsg)
      CALL WriteMsg()      ! drop to the next line on screen

      !
      !  For GLSHFS, all we care about are these data types:
      !     GDT_AirtempMax       (for LBRM)
      !     GDT_AirtempMin       (for LBRM)
      !     GDT_Precipitation    (for LBRM)
      !
      !     GDT_AirtempMean      (for LLTM)
      !     GDT_DewpointMean     (for LLTM)
      !     GDT_Windspeed        (for LLTM)
      !     GDT_CloudCover       (for LLTM)
      !     GDT_NetLongWaveRad   (for LLTM)
      !     GDT_IncidentRad      (for LLTM)
      !
      !  So now process the station data, consolidating the data from all stations
      !  into a single file for that data type. We will specify the desired output 
      !  units here, too.
      !
      !  This routine consolidates the data for a particular type, then writes the
      !  csv-format output file for that type.
      !  Output file name is: stndata_datatype.csv
      !
      WRITE(ErrorMessage, 1150) TRIM(Lake10), GlerlDataTypeString(GDT_AirtempMax); CALL PassMsg
      CALL ConsolidateDataType(Bsn, GDT_AirtempMax,     GDU_Celsius);         IF (ErrorLevel .NE. 0) GOTO 899

      WRITE(ErrorMessage, 1150) TRIM(Lake10), GlerlDataTypeString(GDT_AirtempMin); CALL PassMsg
      CALL ConsolidateDataType(Bsn, GDT_AirtempMin,     GDU_Celsius);         IF (ErrorLevel .NE. 0) GOTO 899

      WRITE(ErrorMessage, 1150) TRIM(Lake10), GlerlDataTypeString(GDT_Precipitation); CALL PassMsg
      CALL ConsolidateDataType(Bsn, GDT_Precipitation,  GDU_Millimeters);     IF (ErrorLevel .NE. 0) GOTO 899

      WRITE(ErrorMessage, 1150) TRIM(Lake10), GlerlDataTypeString(GDT_AirtempMean); CALL PassMsg
      CALL ConsolidateDataType(Bsn, GDT_AirtempMean,    GDU_Celsius);         IF (ErrorLevel .NE. 0) GOTO 899

      WRITE(ErrorMessage, 1150) TRIM(Lake10), GlerlDataTypeString(GDT_DewpointMean); CALL PassMsg
      CALL ConsolidateDataType(Bsn, GDT_DewpointMean,   GDU_Celsius);         IF (ErrorLevel .NE. 0) GOTO 899

      WRITE(ErrorMessage, 1150) TRIM(Lake10), GlerlDataTypeString(GDT_Windspeed); CALL PassMsg
      CALL ConsolidateDataType(Bsn, GDT_Windspeed,      GDU_MetersPerSecond); IF (ErrorLevel .NE. 0) GOTO 899

      WRITE(ErrorMessage, 1150) TRIM(Lake10), GlerlDataTypeString(GDT_CloudCover); CALL PassMsg
      CALL ConsolidateDataType(Bsn, GDT_CloudCover,     GDU_Percent);         IF (ErrorLevel .NE. 0) GOTO 899

      WRITE(ErrorMessage, 1150) TRIM(Lake10), GlerlDataTypeString(GDT_NetLongWaveRad); CALL PassMsg
      CALL ConsolidateDataType(Bsn, GDT_NetLongWaveRad, GDU_WattsPerM2);      IF (ErrorLevel .NE. 0) GOTO 899

      WRITE(ErrorMessage, 1150) TRIM(Lake10), GlerlDataTypeString(GDT_IncidentRad); CALL PassMsg
      CALL ConsolidateDataType(Bsn, GDT_IncidentRad,    GDU_WattsPerM2);      IF (ErrorLevel .NE. 0) GOTO 899

      
      GOTO 999

      !
      !  Handlers for I/O errors
      !
  811 ErrorMessage = 'Error opening file '//TRIM(File1); CALL PassMsg; GOTO 898
  812 ErrorMessage = 'Error reading file '//TRIM(File1); CALL PassMsg; GOTO 898
  

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] AggregateTheStationData()...'; CALL PassMsg
     
      !
      !  Final cleanup
      !      
  999 CALL AllStations%Clear()
      CALL StnD%Clear()
      RETURN
      
      !
      !  FORMATs
      !
 1101 FORMAT(A200)
 1150 FORMAT('Aggregating station data for Lake ', A, ': ', A)
 1201 FORMAT('Reading station data for ', A3, ': ', I0, '/', I0)
  
      END SUBROUTINE AggregateTheStationData


!---------------------------------------------------------------------------------------      
!
      SUBROUTINE ConsolidateDataType(Bsn, DType, DUnit)
      IMPLICIT NONE
      CHARACTER(LEN=3), INTENT(IN) :: Bsn
      INTEGER,          INTENT(IN) :: DType, DUnit
      
      INTEGER :: I, J, K, IOS
      INTEGER :: Seq, SSeq, ESeq, EarliestDataAdded
      LOGICAL :: Flag, Found, FExist, AddIt
      REAL    :: ConvFactor
      REAL, DIMENSION(:), ALLOCATABLE :: Temp
      CHARACTER(LEN=25)  :: ID1, ID2, DTStr
      CHARACTER(LEN=200) :: DirName, FName
      TYPE (TDlyDataMetStn)              :: TmpStn
      TYPE (TDlyData)                    :: TmpTDD
      TYPE (TDlyDataMetStn), POINTER     :: StnOld, StnNew, Stn1
      TYPE (TDlyData),       POINTER     :: TDD
      TYPE (TDlyDataForMultipleStations) :: OldData, NewData

      !
      !  Instantiate objects that we MIGHT use.
      !  This seems really strange to do, but it seems from testing with valgrind 
      !  that if we never instantiate them, the FINAL procedure still tries to
      !  run on them, causing a mess in memory. As soon as I added these two
      !  lines, the memory problems went away. Seems like a problem with the
      !  way the Fortran compiler is handling it to me, but I have no control
      !  over that, so I use this work-around.  I have removed the lines where
      !  I originally instantiated them just prior to using them.
      !
      !  Note that this is a very small penalty. We use almost no memory for 
      !  these objects by just instantiating them.
      !
      TmpTDD = TDlyData()
      TmpStn = TDlyDataMetStn()

      !
      !  Create empty objects for containing data
      !
      OldData = TDlyDataForMultipleStations()
      NewData = TDlyDataForMultipleStations()
      
      !
      !   Assign file name
      !
      DTStr = GetLowercase(GlerlDataTypeString(DType))
      WRITE(DirName, 1000) TRIM(GLSHFS_Config%BaseDir), Bsn, FilePathSeparator
      WRITE(FName, 1001) TRIM(DirName), TRIM(DTStr), Bsn
      
      !
      !  Build the NewData object, by creating a TDlyDataMetStn entry 
      !  for each relevant station.
      !      
      !  Each entry will have just a single TDlyData object, for the 
      !  data type of interest. The entry for this station in AllStations 
      !  may have multiple data types, but for this object we only want 
      !  the data for that one data type that was requested.
      !
      !  If a station has no data for that data type, then it will have no 
      !  entry in the NewData object.
      !
      DO I = 1, AllStations%NumStations
         Stn1 => AllStations%GetStationPtrByIndex(I)
         TDD => Stn1%GetPointerToDataOfType(DType)
         IF (ASSOCIATED(TDD)) THEN
            CALL TmpStn%Clear()
            Flag = TmpStn%CopyMetaFrom(Stn1); IF (.NOT. Flag) GOTO 899

            !
            !  The data passed in may already be in the correct unit.
            !  If that is the case, then just copy it. 
            !
            !  But if it is NOT, then we need to do unit conversion. I think 
            !  the most efficient way to accomplish that will be to create 
            !  a new TDlyData object to hold the converted data (we don't 
            !  want to modify the original), then use a temporary data array
            !  to convert the data en masse. Each assignment to the object
            !  incurs an overhead of checking indexes, etc. By copying the
            !  array as a unit, we greatly reduce that overhead penalty.
            !
            IF (TDD%GetDataUnit() .EQ. DUnit) THEN
               Flag = TmpStn%AddDataSet(TDD);  IF (.NOT. Flag) GOTO 899
            ELSE
               CALL TmpTDD%Clear()
               Flag = TmpTDD%CopyFrom(TDD);  IF (.NOT. Flag) GOTO 899
               ConvFactor = UnitConvertedDataValue(1.0, TmpTDD%GetDataUnit(), DUnit)
               
               !
               !  Only add station files with at least 1 day of valid data
               !
               AddIt = .TRUE.
               IF (SSeq .EQ. MissingData_Date) AddIt = .FALSE.
               IF (ESeq .EQ. MissingData_Date) AddIt = .FALSE.
               IF (ESeq .LT. SSeq) AddIt = .FALSE.
               IF (AddIt) THEN
                  SSeq = TmpTDD%GetStartDate()
                  ESeq = TmpTDD%GetEndDate()
                  ALLOCATE(Temp(ESeq-SSeq+1), STAT=IOS); IF (IOS .NE. 0) GOTO 801
                  Temp = TmpTDD%GetDataVals()
                  DO Seq = SSeq, ESeq
                     K = Seq - SSeq + 1
                     Temp(K) = Temp(K) * ConvFactor
                  END DO
                  Flag = TmpTDD%AssignData(SSeq, ESeq, Temp);  IF (.NOT. Flag) GOTO 899
                  DEALLOCATE(Temp, STAT=IOS)
                  Flag = TmpStn%AddDataSet(TmpTDD);  IF (.NOT. Flag) GOTO 899
               END IF
               CALL TmpTDD%Clear()
            END IF               
            Flag = NewData%AddStation(TmpStn);           IF (.NOT. Flag) GOTO 899
            CALL TmpStn%Clear()
         END IF
      END DO

      !
      !  If no stations had data for this type, we can exit early.
      !
      IF (NewData%NumStations .EQ. 0) GOTO 999
      
       
      !
      !  Is there an existing file for this data type?
      !  If not, we can shortcut the rest of the processing, and just write the
      !  new file, using the data in object NewData.
      !
      INQUIRE(FILE=TRIM(FName), EXIST=FExist)
      IF (.NOT. FExist) THEN
         CALL WriteFile_OneDataTypeManyStations(TRIM(FName), NewData, DType, DUnit); IF (ErrorLevel .NE. 0) GOTO 899
         GOTO 999
      END IF

      !
      !  Getting to here means that we DO have new data, and we DO have an existing file.
      !  We need to merge the new data into the data from the old file and then write a new file.
      !
      !  Note that in both objects (NewData,OldData), each TDlyDataMetStn object will contain
      !  only a single data type.
      !
      !  First, read the old data. This will be the "master" data set. For each NewData station,
      !  see if there is data for that station in OldData. If so, overwrite it with the new data.
      !  If not, then just add the new station.
      !
      CALL ReadFile_OneDataTypeManyStations(TRIM(FName), OldData); IF (ErrorLevel .NE. 0) GOTO 899
      EarliestDataAdded = OldData%NewDataStart
      IF (EarliestDataAdded .EQ. MissingData_Date) EarliestDataAdded = OldData%LatestData+1
      
      DO I = 1, NewData%NumStations        ! For each new station...
         StnNew => NewData%GetStationPtrByIndex(I)
         IF (ASSOCIATED(StnNew)) THEN
            ID1 = TRIM(GetUppercase(StnNew%StnID))
            Found = .FALSE.
            J = 1
            DO WHILE ((.NOT. Found) .AND. (J .LE. OldData%NumStations))
               StnOld => OldData%GetStationPtrByIndex(J)
               IF (ASSOCIATED(StnOld)) THEN
                  ID2 = TRIM(GetUppercase(StnOld%StnID))
                  IF (TRIM(ID1) .EQ. TRIM(ID2)) THEN
                     Flag = StnOld%MergeData(StnNew); IF (.NOT. Flag) GOTO 899
                     Found = .TRUE.
                     EarliestDataAdded = MIN(EarliestDataAdded, StnNew%GetStartDate())
                  END IF
               END IF
               J = J + 1
            END DO

            !
            !  If we never found a match, that means this is a new station and we need
            !  to append it.
            !
            IF (.NOT. Found) THEN
               Flag = OldData%AddStation(StnNew); IF (.NOT. Flag) GOTO 899
               EarliestDataAdded = MIN(EarliestDataAdded, StnNew%GetStartDate())
            END IF
         ELSE
         END IF
      END DO
      OldData%NewDataStart = EarliestDataAdded

      CALL WriteFile_OneDataTypeManyStations(TRIM(FName), OldData, DType, DUnit); IF (ErrorLevel .NE. 0) GOTO 899
      CALL NewData%Clear()
      GOTO 999

      !
      !  Error handling
      !
  801 ErrorMessage = 'Error allocating memory for unit conversion.'; CALL PassMsg; GOTO 898
     
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ConsolidateDataType()...'; CALL PassMsg
     
      !
      !  Final cleanup
      !      
  999 CALL NewData%Clear()
      CALL OldData%Clear() 
      CALL TmpTDD%Clear()     
      CALL TmpStn%Clear()
      
 1000 FORMAT(A, A3, A1)
 1001 FORMAT(A, 'stndata_', A, '_', A3, '.csv')
      
      END SUBROUTINE ConsolidateDataType
      
      
     
END MODULE StationData_Aggregation
