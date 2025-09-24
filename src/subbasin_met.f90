!-----------------------------------------------------------------
!  Compute the subbasin average meteorology using a thiessen-polygon-like method.
!  This corresponds to the program DISAVMET in the old AHPS package.
!
!  What do I mean by "thiessen-polygon-like"?
!    This program (like DISAVMET before it) does not actually build polygons.
!    Instead, it simply steps through the map and counts up how many cells (of
!    the target subbasin) are the nearest neighbor to the station. It effectively
!    computes the same result as something that would build polygons, etc.
!    But it is a much simpler (albeit kind of "brute force") process, and 
!    therefore the code is much easier to debug/verify. Efficiency *may* be a
!    bit lower, but I'm not sure about that. Even if so, I still think it is
!    a worthy trade-off to keep the code simple.
!
!
!  Remember that for GLSHFS, all we care about are these data types:
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
!  Tim Hunter, January, 2017
!    
!-----------------------------------------------------------------

MODULE Subbasin_Met
      USE ErrorProcess
      USE StatusMessages
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GlerlDataTypesAndUnits
      USE DailyDataStructures
      USE DailyDataCollections
      USE GLSHFS_StructuredData
      USE GLSHFS_ConfigFile
      USE GLSHFS_Files
      USE BasinInfo
      USE GlerlMaps

      !
      !  By default, hide everything. Things that need to be public outside
      !  the module will be explicitly declared PUBLIC.
      !
      PRIVATE
      PUBLIC :: ComputeSubbasinMet

      INTEGER, PARAMETER :: NumDataTypes = 9

      !
      !  These items are used for storing a cached set of thiessen weights
      !  for a number of location networks.  Note that the locations may represent
      !  a single station or multiple co-located stations. That doesn't matter for
      !  purposes of how we do the weights; weights just represent the weights for 
      !  each location.
      !
      !  I am setting the MaxNumLocations to match the max number of stations specified 
      !  elsewhere in the GLSHFS code.
      !
      !  Each entry in NetworkIDStrings is a concatenation of the the X,Y coordinates 
      !  for all locations in that network. The X and Y coordinates are expressed as 
      !  an integer offset from 0, in cells. Since we are dealing with a map cell
      !  size of 1 kilometer right now, that means the range of values for X and Y is 
      !  going to always be well within the range of about -1000:1000, which is easily 
      !  represented by a 2-byte integer. We can concatenate those two 2-byte integer 
      !  values into a 4-byte bit string, and then treat that as a 4-byte character 
      !  variable by use of the Fortran 90 TRANSFER() function, which is a functional
      !  match to the Fortran 77 EQUIVALENCE statement, but more flexible. We string
      !  all of them together (using 0,0 as the location for all unused slots) to get
      !  the network id as a long character string.
      !
      INTEGER, PARAMETER :: MaxNumLocations = MaxNumberOfStations
      INTEGER, PARAMETER :: CacheSize = 300          ! how many weight sets will we store (and write to the file?)
      TYPE ThiessenWeightCache
         INTEGER :: ValidNetworks
         INTEGER, DIMENSION(CacheSize) :: NetworkSize                                 ! how many locations in this weight set?
         CHARACTER(LEN=MaxNumLocations*4), DIMENSION(CacheSize) :: NetworkIDStrings   ! the network ID string
         REAL, DIMENSION(MaxNumLocations, 0:30, CacheSize)      :: NetworkWeights     ! weight for each location
         INTEGER, DIMENSION(CacheSize) :: SavePriority                                ! used to determine which one to overwrite
      END TYPE ThiessenWeightCache

      !
      TYPE (ThiessenWeightCache) :: ThsnWgts
      INTEGER :: UsedOldWeights, ComputedNewWeights
      
      !
      !  These module-global variables are used for producing the detailed met
      !  output files when the user has specified in the configuration
      !  file that OutputMetDetails=TRUE
      !
      !  ThsnCellMap has the same size/shape as the lake map. Whereas the lake map 
      !  stores the subbasin number as a 1-byte value, ThsnCellMap stores an index value
      !  as a 2-byte integer value. This value is an index (1..NumLocations) that
      !  indicates which station location is being used for each cell. Thus, the 
      !  ThsnCellMap effectively becomes a thiessen polygon map.
      !
      LOGICAL :: OutputTheMetDetails
      INTEGER :: Thsn_NumStns, Thsn_DType, Thsn_Seq
      CHARACTER(LEN=25), DIMENSION(MaxNumLocations) :: Thsn_StnID
      INTEGER, DIMENSION(MaxNumLocations) :: Thsn_StnX, Thsn_StnY
      INTEGER (KIND=INTEGER2), DIMENSION(:,:), ALLOCATABLE :: ThsnCellMap

CONTAINS

!---------------------------------------------------------------------------      
      !---------------------------------------------------------------------
      !  Compute areal averages for each subbasin for the available met data,
      !  from the supplied station data in files stndata_<dt>_<bsn>.csv
      !
      !  This will also create/update the files subdata_<bsn><sub>.csv
      !  (e.g. subdata_sup01.csv)
      !---------------------------------------------------------------------
      !
      SUBROUTINE ComputeSubbasinMet(Bsn)
      IMPLICIT NONE
      CHARACTER(LEN=3), INTENT(IN) :: Bsn
      
      INTEGER :: I, J, DT, LkNum, IOS, U1
      INTEGER :: SSeq, ESeq, Stn, Sub, NumDays
      INTEGER :: Seq, SWD, Dy, Mn, Yr
      INTEGER :: SSeqData, ESeqData, SSeqOut, ESeqOut, SSeqProcess, ESeqProcess
      INTEGER :: DType, DUnit, NumActiveStations
      INTEGER :: SubSSeq, SubESeq, StnSSeq, StnESeq, UpdSSeq, NewDataSeq
      LOGICAL :: OK, FExist
      REAL    :: DVal
      CHARACTER(LEN=20)  :: DTStr
      CHARACTER(LEN=200) :: FName, TFName, BFName, DirName, DebugFile
      
      REAL, DIMENSION(0:30) :: SubVals
      
      !
      !  These arrays are a matching set with info for all of the raw station
      !  data (entire period).  Saves me from reading the files over and over.
      !
      REAL, DIMENSION(:),   ALLOCATABLE  :: AllStnLats, AllStnLongs    ! lat/long info for all stations (1:stns)
      REAL, DIMENSION(:,:), ALLOCATABLE  :: AllStnData                 ! station data values for all stations, entire period (1:days, 1:stns)
      CHARACTER(LEN=25), DIMENSION(:),  ALLOCATABLE  :: AllStnIDs      ! station IDs; only needed for detailed output
      
      !
      !  These arrays contain the specific info needed on a single
      !  day to compute the weights (and weighted value) for that day.
      !
      REAL, DIMENSION(:),   ALLOCATABLE  :: StnLat, StnLon, StnVal     ! input station data, copied from AllStn___ arrays
      REAL, DIMENSION(:,:), ALLOCATABLE  :: StnWeights                 ! computed thiessen weight for each station
      CHARACTER(LEN=25), DIMENSION(:), ALLOCATABLE :: StnID

      !
      TYPE (BasinInfoData)   :: BData
      TYPE (TGlerlMap)       :: GMap
      TYPE (THeaderInfoType) :: HdrInfo
 
      TYPE (TDlyData)                    :: TDD 
      TYPE (TDlyData),           POINTER :: TDDP
      TYPE (TDlyDataMetStn),     POINTER :: TDDMSP
      TYPE (TDlyDataForSubbasin)         :: SubData(0:30)
      TYPE (TDlyDataForMultipleStations) :: AllStnDataObj
      INTEGER, DIMENSION(NumDataTypes)   :: MyDataTypes
      
      TYPE (Met_Subbasin), DIMENSION(0:30) :: SubMet

      !  debugging  17Jul2020
      REAL    :: X,Y
      INTEGER :: TX,TY
      
      
      !
      !  Assign MyDataTypes array entries.
      !  Could have been done as a PARAMETER thing in the declaration, but I didn't.
      !  This way is a TINY bit less efficient, computationally, but I
      !  think it is more readable.
      !
      MyDataTypes(1) = GDT_AirtempMax
      MyDataTypes(2) = GDT_AirtempMin
      MyDataTypes(3) = GDT_Precipitation
      MyDataTypes(4) = GDT_AirtempMean
      MyDataTypes(5) = GDT_DewpointMean
      MyDataTypes(6) = GDT_Windspeed
      MyDataTypes(7) = GDT_CloudCover
      MyDataTypes(8) = GDT_NetLongWaveRad
      MyDataTypes(9) = GDT_IncidentRad

      !
      !  Convert Bsn into a lake number for use when accessing arrays
      !
      LkNum = LakeNumberFromName3(Bsn)
      IF (LkNum .LT. 1) THEN
         ErrorMessage = 'Error: Invalid basin specified ['//TRIM(Bsn)//']'; CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Set DirName to the correct value for this lake
      !
      DirName = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator
      
      !
      !  Determine if we need to do anything here.  If there is no new station data,
      !  then we don't.
      !
      SSeqData = MissingData_Date
      ESeqData = MissingData_Date
      NewDataSeq = MissingData_Date
      DO I = 1, 9
         DType = MyDataTypes(I)
         DTStr = GetLowercase(GlerlDataTypeString(DType))
         WRITE(FName, 1005) TRIM(DirName), TRIM(DTStr), Bsn
         INQUIRE(FILE=TRIM(FName), EXIST=FExist)
         IF (FExist) THEN
            CALL ReadJustHeaderInfo(FName, HFT_MultiStation, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
            IF (SSeqData   .EQ. MissingData_Date) SSeqData   = HdrInfo%SDate
            IF (ESeqData   .EQ. MissingData_Date) ESeqData   = HdrInfo%EDate
            IF (NewDataSeq .EQ. MissingData_Date) NewDataSeq = HdrInfo%NewDataDate
            IF (HdrInfo%SDate       .LT. SSeqData)   SSeqData   = HdrInfo%SDate
            IF (HdrInfo%EDate       .GT. ESeqData)   ESeqData   = HdrInfo%EDate
            IF (HdrInfo%NewDataDate .GT. NewDataSeq) NewDataSeq = HdrInfo%EDate
         END IF
      END DO
      IF (NewDataSeq .GT. ESeqData) RETURN
      
      !
      !  Do the initial creation of the data objects so that if an error causes
      !  short-circuit execution and exit from the routine, the resulting finalization
      !  steps do not cause an error (e.g. trying to Clear() a non-existent object).
      !
      TDD = TDlyData()
      AllStnDataObj = TDlyDataForMultipleStations()
      DO Sub = 0, 30
         SubData(Sub) = TDlyDataForSubbasin()
      END DO
      
      !
      !  Read the basininfo.txt file to get subbasin areas, etc
      !
      WRITE(FName, 1001) TRIM(DirName), 'basininfo.txt'
      CALL ReadBasinInfoFile(FName, BData); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Read the GLERL digital map for this basin
      !
      GMap = TGlerlMap()
      CALL GMap%Setup(Bsn); IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(FName, 1002) TRIM(DirName), Bsn
      CALL GMap%ReadMap(TRIM(FName)); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Initialize the thiessen weights cache stuff, then
      !  read the stored thiessen weights for this lake, if it exists.
      !
      CALL InitializeThiessenWeights
      CALL ReadThsnWeights(GMap); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Prepare the data structures that will hold the averaged meteorology
      !  data for each subbasin.  First read the existing data for each 
      !  subbasin, then assign a few metadata items.
      !
      !  If the file does not yet exist, then we just leave the dates and
      !  data stuff at the initialized values.
      !
      SubSSeq = MissingData_Date
      SubESeq = MissingData_Date
      DO Sub = 0, NumSubbasins(LkNum)
         CALL SubData(Sub)%Clear()
         WRITE(FName, 1003) TRIM(DirName), Bsn, Sub, 'csv'
         INQUIRE(FILE=TRIM(FName), EXIST=FExist)
         IF (FExist) THEN
            CALL ReadFile_OneSubbasin(FName, SubData(Sub)); IF (ErrorLevel .NE. 0) GOTO 899
            SubData(Sub)%Bsn     = Bsn
            SubData(Sub)%SubNum  = Sub
            SubData(Sub)%SubArea = BData%SubbasinArea(Sub)
            SSeq = SubData(Sub)%GetStartDate()
            ESeq = SubData(Sub)%GetEndDate()
            IF (SSeq .LT. SubSSeq) SubSSeq = SSeq
            IF (ESeq .LT. SubESeq) SubESeq = ESeq
         ELSE
            SubData(Sub)%Bsn     = Bsn
            SubData(Sub)%SubNum  = Sub
            SubData(Sub)%SubArea = BData%SubbasinArea(Sub)
         END IF
      END DO
      
      !
      !  What is the date extent for the station data files?
      !  I will only read the header section of each file.  
      !  The ReadHeaderInfo() routine assumes the file is already open using
      !  the specified unit number.
      !
      StnSSeq = MissingData_Date
      StnESeq = MissingData_Date
      UpdSSeq = MissingData_Date
      DO DT = 1, NumDataTypes
         DType = MyDataTypes(DT)
         DTStr = GetLowercase(GlerlDataTypeString(DType))
         StatusMsg = 'Computing date extents for ' // TRIM(DTStr) // ' in ' // Bsn; CALL WriteMsg()
         WRITE(FName, 1005) TRIM(DirName), TRIM(DTStr), Bsn
         INQUIRE(FILE=TRIM(FName), EXIST=FExist)
         IF (FExist) THEN
            U1 = GetFreeUnitNumber()
            OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', ERR=811)
            CALL FileWasOpened(U1)
            CALL ReadHeaderInfo(FName, U1, HFT_MultiStation, HdrInfo); IF (ErrorLevel .NE. 0) GOTO 899
            CLOSE(U1, IOSTAT=IOS)
            CALL FileWasClosed(U1)
            IF (StnSSeq .EQ. MissingData_Date) StnSSeq = HdrInfo%SDate
            IF (StnESeq .EQ. MissingData_Date) StnESeq = HdrInfo%EDate
            IF (HdrInfo%SDate .LT. StnSSeq) StnSSeq = HdrInfo%SDate
            IF (HdrInfo%EDate .GT. StnESeq) StnESeq = HdrInfo%EDate
            IF (UpdSSeq .EQ. MissingData_Date) UpdSSeq = HdrInfo%NewDataDate
            IF (HdrInfo%NewDataDate .LT. UpdSSeq) UpdSSeq = HdrInfo%NewDataDate
         END IF
      END DO

      !
      !  Now that I have the date extents for both the existing subbasin
      !  data and the station data, determine the size of the arrays that I 
      !  will need for the updated subbasin data, and allocate the memory.
      !
      !  Note that this is gonna be a big chunk of memory.  
      !  But my original method, which used a lot less memory, took a huge
      !  amount of time to process. It had to do a lot of extra I/O.
      !  read - update - write - read(again) - update - write - read(again) - .....
      !
      SSeqOut = StnSSeq
      ESeqOut = StnESeq
      IF (SubSSeq .NE. MissingData_Date) SSeqOut = MIN(SubSSeq, StnSSeq)
      IF (SubESeq .NE. MissingData_Date) ESeqOut = MAX(SubESeq, StnESeq)
      NumDays = ESeqOut - SSeqOut + 1
      DO Sub = 0, NumSubbasins(LkNum)
         ALLOCATE(SubMet(Sub)%Values(9,NumDays), STAT=IOS)
         IF (IOS .NE. 0) THEN
            WRITE(ErrorMessage, 5001) NumDays, Bsn, Sub;  CALL PassMsg;  GOTO 898
         END IF
         SubMet(Sub)%Values(:,:) = MissingData_Real
      END DO

      !
      !  Allocate arrays that can hold information for all stations (for a single
      !  data type). These arrays will be cleared and reused as needed.
      !
      !  I am allocating these to the max possible number of stations.  That
      !  is "wasteful" of memory, but means I can allocate them once and be 
      !  done with it.  I am trying to tweak performance here, because it was
      !  was poor in code version 1.
      !
      !  I will always allocate the station ID array, even if we aren't doing
      !  the detailed output. A waste of memory, I know, but it keeps the
      !  subsequent code a lot cleaner.  If memory usage proves to be an issue,
      !  this could be trimmed to only occur when needed.  That will require a
      !  number of additional changes, of course.      
      !
      Stn = MaxNumberOfStations
      Sub = NumSubbasins(LkNum)
      ALLOCATE(AllStnLats(Stn), AllStnLongs(Stn), AllStnIDs(Stn),     &
               AllStnData(NumDays,Stn), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5002) Bsn;               CALL PassMsg
         WRITE(ErrorMessage, 5003) NumDays, Stn;      CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Allocate arrays used for computing the weights and weighted values.
      !  Again, for a single data type, and the arrays will be cleared and reused as needed.
      !
      !  I am allocating these to the max possible number of stations.  That
      !  is "wasteful" of memory, but means I can allocate them once and be 
      !  done with it.  I am trying to tweak performance here, because it was
      !  was very poor in code version 1.
      !
      !  Again, I will always do the ID array, even though it is only really needed when
      !  doing detailed output.
      !
      ALLOCATE(StnLat(Stn), StnLon(Stn), StnID(Stn), StnVal(Stn), StnWeights(Stn,0:Sub+2), STAT=IOS)      
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5005) Bsn;             CALL PassMsg
         WRITE(ErrorMessage, 5006) Stn, Sub;        CALL PassMsg
         GOTO 898
      END IF

      !
      !  Now assign the data already read from the subbasin files into the array
      !  for each subbasin
      !
      DO Sub = 0, NumSubbasins(LkNum)
         DO DT = 1, 9
            SubMet(Sub)%Values(DT,:) = MissingData_Real         ! initialize entire output series to missing
            DType = MyDataTypes(DT)
            TDDP => SubData(Sub)%GetPointerToDataOfType(DType)
            IF (ASSOCIATED(TDDP)) THEN
               SSeq = TDDP%GetStartDate()
               ESeq = TDDP%GetEndDate()
               I = SSeq - SSeqOut + 1
               J = ESeq - SSeqOut + 1
               SubMet(Sub)%Values(DT,I:J) = TDDP%GetDataVals()    ! the entire time series in the existing file
            END IF
         END DO
      END DO

      !
      !  Track usage/computation of thiessen weight
      !
      UsedOldWeights = 0
      ComputedNewWeights = 0

      !
      !  If we are planning to output ANY of the detailed met files, allocate
      !  the big map array
      !
      DEALLOCATE(ThsnCellMap, STAT=IOS)    ! ensure it's cleared
      OutputTheMetDetails = .FALSE.
      DO I = 1, 7
         IF (GLSHFS_Config%OutputMetDetails(I) .EQ. 'Y') OutputTheMetDetails = .TRUE.
      END DO
      IF (OutputTheMetDetails) THEN
         ALLOCATE(ThsnCellMap(0:GMap%MapWidth-1,0:GMap%MapHeight-1), STAT=IOS)
         IF (IOS .NE. 0) THEN
            WRITE(ErrorMessage, 5020) Bsn;   CALL PassMsg
            GOTO 898
         END IF
      END IF     
      
      !
      !  For each data type....
      !     1) Read the input station data, if it exists. If not, no need to do step 2.
      !     2) For each subbasin, compute areal averaged value from the 
      !        station data for each day.
      !
      !  Things to note:
      !  a) The order of the stations may be the same from one data type to the 
      !     next, but it is not required.
      !  b) All stations have "data" for the entire period contained in the file.
      !     It may be 99% missing data, but there is a value for each day.  This
      !     simplifies things quite a bit.
      !
      DO DT = 1, NumDataTypes
         DType = MyDataTypes(DT)
         DTStr = GetLowercase(GlerlDataTypeString(DType))
         StatusMsg = 'Processing ' // TRIM(DTStr) // ' for ' // Bsn; CALL WriteMsg()
         WRITE(FName, 1005) TRIM(DirName), TRIM(DTStr), Bsn
         INQUIRE(FILE=TRIM(FName), EXIST=FExist)
         IF (FExist) THEN
            CALL ReadFile_OneDataTypeManyStations(FName, AllStnDataObj); IF (ErrorLevel .NE. 0) GOTO 899
            NumActiveStations = AllStnDataObj%NumStations
            !
            !  Date extent of the data in the arrays (all stations are same)
            !
            SSeqData = AllStnDataObj%EarliestData
            ESeqData = AllStnDataObj%LatestData

            !
            !  Determine the date extent of the data to be processed here.
            !
            SSeqProcess = AllStnDataObj%NewDataStart
            ESeqProcess = AllStnDataObj%LatestData

            !
            !  Clear the arrays that will hold information for ALL stations in the file. 
            !
            AllStnLats(:)   = MissingData_Real
            AllStnLongs(:)  = MissingData_Real
            AllStnData(:,:) = MissingData_Real
            AllStnIDs(:)    = ''

            !
            !  Transfer data from the AllStnDataObj object into the simple arrays.
            !  TDDMSP is a pointer to a variable of type TDlyDataMetStn.
            !
            DO Stn = 1, NumActiveStations
               TDDMSP => AllStnDataObj%GetStationPtrByIndex(Stn); IF (ErrorLevel .NE. 0) GOTO 899    ! pointer to a single station's data (TDlyDataMetStn)
               TDDP   => TDDMSP%GetPointerToDataByIndex(1);       IF (ErrorLevel .NE. 0) GOTO 899    ! TDDMSP always has just 1 dataset in this setting
               IF (ASSOCIATED(TDDP)) THEN
                  AllStnLats(Stn)   = TDDMSP%Latitude
                  AllStnLongs(Stn)  = TDDMSP%Longitude
                  AllStnIDs(Stn)    = TDDMSP%StnID
                  AllStnData(:,Stn) = TDDP%GetDataVals()               ! the entire time-series for this station
               ELSE
                  AllStnLats(Stn)   = -99.999
                  AllStnLongs(Stn)  = -999.999
                  AllStnIDs(Stn)    = ''
                  AllStnData(:,Stn) = MissingData_Real
               END IF
            END DO

            !
            !  Determine if we need to do the detailed met output stuff for this data type.
            !
            OutputTheMetDetails = .FALSE.
            I = -1            
            IF (DType .EQ. GDT_AirtempMax)     I = 1
            IF (DType .EQ. GDT_AirtempMin)     I = 2
            IF (DType .EQ. GDT_Precipitation)  I = 3
            IF (DType .EQ. GDT_AirtempMean)    I = 4
            IF (DType .EQ. GDT_DewpointMean)   I = 5
            IF (DType .EQ. GDT_WindSpeed)      I = 6
            IF (DType .EQ. GDT_CloudCover)     I = 7
            DO J = 1, 7
               IF ((I .EQ. J) .AND. (GLSHFS_Config%OutputMetDetails(J) .EQ. 'Y')) THEN
                  OutputTheMetDetails = .TRUE.
               END IF
            END DO
            Thsn_DType = DType
            

            !
            !  For each day
            !
            CALL SequenceDate(Dy, Mn, Yr, SSeqProcess); IF (ErrorLevel .NE. 0) GOTO 899
            WRITE(StatusMsg, 2001) Yr, Mn, Dy;  CALL WriteMsg(.TRUE., .TRUE.)
            DO Seq = SSeqProcess, ESeqProcess
               I = Seq - SSeqData + 1
               CALL SequenceDate(Dy, Mn, Yr, Seq); IF (ErrorLevel .NE. 0) GOTO 899
               IF ((Dy .EQ. 1) .OR. (Dy .EQ. 15)) THEN
                  WRITE(StatusMsg, 2001) Yr, Mn, Dy;  CALL WriteMsg(.TRUE., .TRUE.)
               END IF

               !
               !  We will need this info if we output detailed met...
               !
               Thsn_Seq   = Seq
               
               !
               !  Clear the station location and weights arrays, then refill them
               !  with the info from only those stations with valid data for this day.
               !
               !  Note that the StnWeights array contains a spot for OutOfBasin as 
               !  well as an unused spot for the deprecated bridge code.
               !  I will be ignoring those.
               !
               StnLat(:) = MissingData_Real
               StnLon(:) = MissingData_Real
               StnVal(:) = MissingData_Real
               StnID(:)  = ''
               SWD = 0                            ! Stations With Data
               DO Stn = 1, NumActiveStations
                  DVal = AllStnData(I,Stn)
                  IF (DVal .GT. MissingData_Real_Test) THEN
                     SWD = SWD + 1
                     StnVal(SWD) = DVal
                     StnLat(SWD) = AllStnLats(Stn)
                     StnLon(SWD) = AllStnLongs(Stn)
                     StnID(SWD)  = AllStnIDs(Stn)
                  END IF
               END DO

               !
               !  If we have valid data for this day then compute subbasin values.
               !  Then assign those values into the SubMet()%Values() array for
               !  each subbasin, 0..NumSubs (ignoring the OutOfBasin area).
               !  This will, when needed, overwrite subbasin values read from the file.
               !
               !  Note that if we are outputting the detailed met info (i.e. theissen
               !  cell map, etc), we have to do it from ComputeSubbasinVals() because
               !  the details are not passed back to here.
               !
               IF (SWD .GT. 0) THEN
                  CALL ComputeSubbasinVals(StnVal, StnLat, StnLon, StnID, GMap, SubVals); IF (ErrorLevel .NE. 0) GOTO 899
                  I = Seq - SSeqOut + 1     ! index into the "master" arrays
                  DO Sub = 0, NumSubbasins(LkNum)
                     SubMet(Sub)%Values(DT,I) = SubVals(Sub)
                  END DO
               ELSE
                  DO Sub = 0, NumSubbasins(LkNum)
                     SubMet(Sub)%Values(DT,I) = MissingData_Real
                  END DO
               END IF
            END DO
            CALL SequenceDate(Dy, Mn, Yr, ESeqProcess); IF (ErrorLevel .NE. 0) GOTO 899
            WRITE(StatusMsg, 2001) Yr, Mn, Dy
            StatusMsg = CHAR(13) // TRIM(StatusMsg);   CALL WriteMsg(.FALSE., .TRUE.)

            !
            !  Now we have daily subbasin values for the entire period, stored 
            !  in SubMet(). 
            !  For each subbasin, build a TDlyData object with the new/revised data
            !  for the subbasin then merge that with SubData(Sub).
            !
            !  The merge procedure will correctly handle either a brand new 
            !  datatype or new data for an existing one.
            !
            CALL TDD%Clear()
            DO Sub = 0, NumSubbasins(LkNum)
               OK = TDD%SetDataType(DType);                                          IF (.NOT. OK) GOTO 899
               OK = TDD%SetDataUnit(DefaultUnitForType(DType));                      IF (.NOT. OK) GOTO 899
               OK = TDD%AssignData(SSeqOut, ESeqOut, SubMet(Sub)%Values(DT,:));      IF (.NOT. OK) GOTO 899
               OK = SubData(Sub)%MergeData(TDD);                                     IF (.NOT. OK) GOTO 899
               Seq = SubData(Sub)%NewDataSSeq;                                       IF (.NOT. OK) GOTO 899
               IF (Seq .EQ. MissingData_Date) Seq = SSeqProcess
               SubData(Sub)%NewDataSSeq = MIN(Seq, SSeqProcess)
               CALL TDD%Clear()
            END DO

         END IF
      END DO      

      !
      !  Write the new output file for each subbasin.
      !  First write all of the data into a new temporary file.
      !  Then, when that is successful, overwrite the old one.
      !
      DO Sub = 0, NumSubbasins(LkNum)
         WRITE(FName,  1003) TRIM(DirName), Bsn, Sub, 'csv'
         WRITE(TFName, 1003) TRIM(DirName), Bsn, Sub, 'tmp'
         WRITE(BFName, 1003) TRIM(DirName), Bsn, Sub, 'bkp'
         CALL WriteFile_OneSubbasin(TFName, SubData(Sub), SSeqOut, ESeqOut); IF (ErrorLevel .NE. 0) GOTO 899
         CALL DeleteFile(TRIM(BFName))
         INQUIRE(FILE=TRIM(FName), EXIST=FExist)
         IF (FExist) THEN
            CALL Rename_System(FName, BFName, OK)
            IF (.NOT. OK) THEN
               ErrorMessage = 'Error attempting to rename '//TRIM(FName)//' to '//TRIM(BFName);  CALL PassMsg
               GOTO 898
            END IF
         END IF
         CALL Rename_System(TFName, FName, OK)
         IF (.NOT. OK) THEN
            ErrorMessage = 'Error attempting to rename temp data file to '//TRIM(FName);  CALL PassMsg
            GOTO 898
         END IF
         CALL DeleteFile(TRIM(BFName))
      END DO         
      
      !
      !  Now update the station data files to reflect the fact that we have 
      !  processed all of the data in those files.
      !  (We just use station #1 to get existing date extents, because all 
      !   stations have same period of record.)
      !
      DO DT = 1, NumDataTypes
         CALL AllStnDataObj%Clear()
         DType = MyDataTypes(DT)
         DTStr = GetLowercase(GlerlDataTypeString(DType))
         WRITE(FName, 1005) TRIM(DirName), TRIM(DTStr), Bsn
         INQUIRE(FILE=TRIM(FName), EXIST=FExist)
         IF (FExist) THEN
            StatusMsg = 'Updating new data date in '//TRIM(FName); CALL WriteMsg()
            CALL ReadFile_OneDataTypeManyStations(FName, AllStnDataObj); IF (ErrorLevel .NE. 0) GOTO 899
            TDDMSP => AllStnDataObj%GetStationPtrByIndex(1);             IF (ErrorLevel .NE. 0) GOTO 899    ! pointer to a single station's data (TDlyDataMetStn)
            TDDP   => TDDMSP%GetPointerToDataByIndex(1);                 IF (ErrorLevel .NE. 0) GOTO 899    ! TDDMSP always has just 1 dataset in this setting
            DUnit = TDDP%GetDataUnit()
            AllStnDataObj%NewDataStart = TDDP%GetEndDate() + 1
            TFName = TRIM(DirName) // 'temporary_stndata.csv'
            CALL WriteFile_OneDataTypeManyStations(TFName, AllStnDataObj, DType, DUnit); IF (ErrorLevel .NE. 0) GOTO 899
            CALL DeleteFile(TRIM(FName))
            CALL Rename_System(TFName, FName, OK)
            IF (.NOT. OK) THEN
               ErrorMessage = 'Error attempting to rename temporary_stndata.csv to '//TRIM(FName);  CALL PassMsg
               GOTO 898
            END IF
         END IF
      END DO
      
      !
      !  Summary info output
      !
      WRITE(ErrorMessage, 2021) UsedOldWeights;       CALL PassMsg
      WRITE(ErrorMessage, 2022) ComputedNewWeights;   CALL PassMsg
      IF (ComputedNewWeights .GT. 0) THEN
         CALL WriteThsnWeights(GMap); IF (ErrorLevel .NE. 0) GOTO 899
      END IF
  
      GOTO 999

      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening file ' // TRIM(FName);  CALL PassMsg;  GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ComputeSubbasinMet...'; CALL PassMsg

  999 DEALLOCATE(StnLat,      STAT=IOS)
      DEALLOCATE(StnLon,      STAT=IOS)
      DEALLOCATE(StnVal,      STAT=IOS)
      DEALLOCATE(StnWeights,  STAT=IOS)
      DEALLOCATE(AllStnLats,  STAT=IOS)
      DEALLOCATE(AllStnLongs, STAT=IOS)
      DEALLOCATE(AllStnData,  STAT=IOS)
      DEALLOCATE(ThsnCellMap, STAT=IOS)
      DO I = 1, 30
         CALL SubData(I)%Clear()
         DEALLOCATE(SubMet(I)%Values, STAT=IOS)
      END DO
      CALL AllStnDataObj%Clear()
      CALL TDD%Clear()

      
 1001 FORMAT(A, A)
 1002 FORMAT(A, A3, 'bytcd.map')
 1003 FORMAT(A, 'subdata_', A3, I2.2, '.', A3)
 1005 FORMAT(A, 'stndata_', A, '_', A3, '.csv')
 2001 FORMAT(' Processing data for ', I4.4, 2('-', I2.2))
 2021 FORMAT(' Used old weights ', I0, ' times')
 2022 FORMAT(' Computed new weights ', I0, ' times')
 
 5001 FORMAT('Error allocating memory for ', I0, ' days of data from ', A3, ', subbasin ', I0)
 5002 FORMAT('Error allocating memory to hold all station data for ', A3)
 5003 FORMAT('NumDays=', I0, '; NumStations=', I0)
 5005 FORMAT('Error allocating memory used to compute thiessen weights for ', A3)
 5006 FORMAT('NumStations=', I0, ';  NumSubbasins=', I0)
 5020 FORMAT('Error allocating theissen cell map for ', A3)
 5021 FORMAT('Error allocating memory for station IDs for ', A3)
 
      END SUBROUTINE ComputeSubbasinMet


!------------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Compute "thiessen-weighted" subbasin values from a set of station data.
      !
      !  The station data is passed in via 3 arrays, and it is assumed that they 
      !  are aligned. i.e. StnVals(I), StnLats(I), StnLons(I) all refer to the
      !  same station.
      !
      !  Input:
      !    StnVals    = 1-D array of station values (for a single timestep)
      !    StnLats    = 1-D array of latitudes
      !    StnLons    = 1-D array of longitudes
      !    GMap       = GLERL digital map
      !
      !  Output:
      !    SubVals    = 1-D array with one value per subbasin
      !                 Note that this array is ALWAYS declared with indices (0:30)
      !                 That avoids the issue with Fortran in which the lower bound
      !                 of arrays passed as arguments are implicitly assumed to
      !                 be 1. I want the index to correspond to the subbasin number,
      !                 which by GLERL convention is 1..n for subbasins, and 0 for 
      !                 the lake surface.
      !                 The calling routine needs to use this same ordering and
      !                 indexing, ideally.
      !                 I am arbitrarily using 30 as the upper bound just as a 
      !                 round number greater than the max needed with our current
      !                 maps.
      !------------------------------------------------------------------------
      SUBROUTINE ComputeSubbasinVals(StnVals, StnLats, StnLons, StnIDs, GMap, SubVals)
      IMPLICIT NONE
      REAL, DIMENSION(:),               INTENT(IN) :: StnVals, StnLats, StnLons
      CHARACTER(LEN=25) , DIMENSION(:), INTENT(IN) :: StnIDs
      TYPE (TGlerlMap),      INTENT(INOUT) :: GMap
      REAL, DIMENSION(0:30), INTENT(OUT)   :: SubVals
      
      INTEGER :: I, J, K, Sub
      INTEGER :: NumStns, NumLocs, Cnt
      LOGICAL :: DataFound
      REAL    :: Tot, X, Y

      INTEGER, DIMENSION(MaxNumLocations)  :: TX, TY   !  cell offset from (0,0) SW corner of map
      REAL,    DIMENSION(MaxNumLocations)  :: TVals
      REAL,    DIMENSION(0:30, MaxNumLocations)  :: Weights
      
      !
      !  Default subbasin values
      !
      SubVals(:) = MissingData_Real

      !
      !  Set this module-global value to zero.  It will be used as a flag later.
      !  If it still is zero, no detailed met output is needed.
      !
      Thsn_NumStns = 0

      !
      !  If no station data, there is nothing to do.
      !
      !  The first test is real simple. Size zero array means no data.
      !  The second one is a bit more complicated. We test for any valid 
      !  data in the period of interest. I do this using a DO WHILE loop 
      !  rather than a simple interative loop so that as soon as we find 
      !  valid data we can move on. It makes this section of code slightly 
      !  more "messy", but should save a LOT of processing time in most cases.
      !      
      NumStns = UBOUND(StnVals, 1)     ! size of the array
      IF (NumStns .EQ. 0) RETURN
      DataFound = .FALSE.
      DO I = 1, NumStns
         IF (StnVals(I) .GT. MissingData_Real_Test) DataFound = .TRUE.
      END DO
      IF (.NOT. DataFound) RETURN
      
      !
      !  Build working arrays of the station data that we can manipulate (TVals, TX, TY)
      !  Note that we convert Lat/Long to X/Y cell coordinates here because I need 
      !  to know the cell location for each station. Plus I need to "bin" them by
      !  cell in preparation for determining co-location.
      !
      !  During this assignment, eliminate stations that are missing data, then
      !  adjust the NumStns variable to reflect the revised number of stations.
      !
      !  During this step, we will also assign variables for detailed met output.
      !  Note that the Thsn_* arrays refer to the actual stations reporting.
      !  The next step may combine multiple stations into a single location
      !  prior to computing weights, but by keeping this "original" list, we
      !  will be able to report the actual stations in that location.
      !  
      TX(:)    = MissingData_Int
      TY(:)    = MissingData_Int
      TVals(:) = MissingData_Real
      J = 0
      DO I = 1, NumStns
         IF (StnVals(I) .GT. MissingData_Real_Test) THEN
            J = J + 1
            TVals(J) = StnVals(I)
            CALL GMap%LatLong2XY(StnLats(I), StnLons(I), X, Y); IF (ErrorLevel .NE. 0) GOTO 899
            TX(J) = INT(X)
            TY(J) = INT(Y)
            Thsn_StnID(J) = StnIDs(I)    ! Save these for potential use in
            Thsn_StnX(J)  = INT(X)       ! detailed output file,
            Thsn_StnY(J)  = INT(Y)       ! if needed.
         END IF
      END DO
      NumStns = J 
      Thsn_NumStns = NumStns      

      !
      !  When 2+ stations are co-located on the same cell location, we will 
      !  average their data values and treat them all as a single station for 
      !  this process.
      !
      !  Notice that I am modifying this array "in-place" as I combine data.
      !  The NumStns variable is being changed, and after this step does NOT reflect
      !  the number of stations, but rather the number of unique locations that will
      !  be used in computing the thiessen weights.
      !
      !  Also note that in order to be as clear as possible I am assigning the number
      !  of *stations* to a new variable named NumLocs. After this step, the computations
      !  are all dealing with unique *locations* rather than stations. To help reinforce
      !  that distinction, I am using this new variable.
      !
      NumLocs = NumStns
      I = 0
      DO WHILE (I .LT. NumLocs)
         I = I + 1
         Tot = TVals(I)
         Cnt = 1
         !
         !  Find any duplicate locations and add them to
         !  the temporary accumulator info
         !
         DO J = I+1, NumLocs
            IF ((TX(J) .EQ. TX(I)) .AND. (TY(J) .EQ. TY(I))) THEN
               Tot = Tot + TVals(J)
               Cnt = Cnt + 1
            END IF
         END DO
         
         !
         !  Compute the new value for TVals(I)
         !
         TVals(I) = Tot / Cnt
         
         !
         !  Eliminate the duplicate locations from the working array
         !
         J = I + 1
         DO WHILE (J .LE. NumStns)
            IF ((TX(J) .EQ. TX(I)) .AND. (TY(J) .EQ. TY(I))) THEN
               K = J 
               DO WHILE (K .LT. NumLocs)    ! shift each entry by 1
                  TVals(K) = TVals(K+1)
                  TX(K)    = TX(K+1)
                  TY(K)    = TY(K+1)
                  K = K + 1
               END DO
               TVals(NumLocs) = MissingData_Real   ! remove the last entry
               TX(NumLocs)    = MissingData_Int
               TY(NumLocs)    = MissingData_Int
               NumLocs = NumLocs - 1
            ELSE
               J = J + 1
            END IF
         END DO
      END DO

      !
      !  Sort the locations by increasing X then Y.
      !  This sorting step is required in order to ensure that the network ID
      !  constructed for this set of locations is always the same.
      !
      CALL SortTheLocations(TX, TY, TVals, NumLocs); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Get thiessen weights (all subbasins including lake surface) for this set of locations
      !
      Weights(:,:) = MissingData_Real
      CALL GetThsnWeights(TX, TY, NumLocs, GMap, Weights); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Output the detailed info, if needed
      !
      IF (OutputTheMetDetails) THEN
          CALL WriteMetDetails(GMap, NumLocs, TX, TY, TVals, Weights)
      END IF

      !
      !  Now, for each subbasin, apply weights to get a subbasin value.
      !
      DO Sub = 0, GMap%Subbasins
         SubVals(Sub) = 0.0
         DO I = 1, NumLocs
            IF (Weights(Sub, I) .GT. 0.000) THEN
               SubVals(Sub) = SubVals(Sub) + (TVals(I) * Weights(Sub,I))
            END IF
         END DO
      END DO

      GOTO 999
      
      !
      !  Error handling
      !
!  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ComputeSubbasinVals...';  CALL PassMsg
  
      !
      !  Final cleanup, regardless of status
      !
  999 RETURN

      END SUBROUTINE ComputeSubbasinVals
      

!------------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Initialize the thiessen weights cache.
      !  This should be done ONCE when starting to process for a particular lake.
      !------------------------------------------------------------------------
      SUBROUTINE InitializeThiessenWeights
      IMPLICIT NONE
      INTEGER :: I, Sub, Loc
      CHARACTER(LEN=MaxNumLocations*4) :: BlankID
      
      DO I = 1, MaxNumLocations*4
         BlankID(I:I) = CHAR(0)
      END DO
      DO I = 1, CacheSize
         ThsnWgts%NetworkSize(I) = 0
         ThsnWgts%NetworkIDStrings(I) = BlankID
         DO Sub = 0, 30
            DO Loc = 1, MaxNumLocations
               ThsnWgts%NetworkWeights(Loc,Sub,I) = 0.0
            END DO
         END DO
         ThsnWgts%SavePriority(I) = 0
      END DO
      ThsnWgts%ValidNetworks = 0

      END SUBROUTINE InitializeThiessenWeights

      
!------------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Retrieve a set of weights to be applied to each location.
      !  This means either getting the weights from the cache or computing them.
      !  If this is the first pass, and the cache of stored weights is empty, then
      !  we will read the file here.
      !
      !  The LocX and LocY arrays contain the cell coordinates of each location
      !  as an integer offset from the map origin of (0,0) at the SW corner.
      !
      !  Input:
      !    LocX   = 1-D array of X coordinates; indexed (1:NumLocations)
      !    LocY   = 1-D array of Y coordinates; indexed (1:NumLocations)
      !    GMap   = GLERL digital map object (TGlerlMap)
      !
      !  Output:
      !    Weights  = 2-D array of weights for each location;
      !               indexed (0:30, 1:NumLocations)   [0:30 is the subbasin number]
      !               Note that the declaration of the array forces the lower 
      !               bound of the first index to be 0, matching the
      !               declaration I assume to have been made in the caller.
      !
      !  Also, this module-global variable is filled, if detailed output is chosen:
      !    ThsnCellMap  = 2-D array (0:MapWidth-1, 0:MapHeight-1) of index values where 
      !                   the index corresponds to the LocX/LocY arrays. 
      !                   i.e. each cell of the map tells which station location is 
      !                   the source of data for that cell. Values are 1..NumLocations.
      !------------------------------------------------------------------------
      SUBROUTINE GetThsnWeights(LocX, LocY, NumLocs, GMap, Weights)
      IMPLICIT NONE
      INTEGER, DIMENSION(MaxNumLocations),   INTENT(IN)    :: LocX, LocY      !  cell offset from (0,0) SW corner of map
      INTEGER,                               INTENT(IN)    :: NumLocs         !  # of valid locations in the arrays
      TYPE (TGlerlMap),                      INTENT(IN)    :: GMap
      REAL, DIMENSION(0:30,MaxNumLocations), INTENT(INOUT) :: Weights         !  indexed (subbasin, loc)

      INTEGER (KIND=INTEGER2) :: X2, Y2
      INTEGER :: I, J, K, Sub, Loc
      CHARACTER(LEN=2) :: C2
      CHARACTER(LEN=MaxNumLocations*4) :: BlankID, ThisID

      !
      !  Handle the trivial case when there is only a single location
      !
      IF (NumLocs .EQ. 1) THEN
         DO Sub = 0, 30
            DO Loc = 1, NumLocs
               Weights(Sub,Loc) = 1.0
            END DO
         END DO
         IF (OutputTheMetDetails) THEN
             ThsnCellMap(:,:) = 1
         END IF
         RETURN
      END IF

      !
      !  If the user has chosen to output met details, we will always have to 
      !  rebuild the map.  We can't just read from the cache file, because it
      !  doesn't save the map.  So we will build the map and then save the
      !  weights, if appropriate.
      !
      !  Prior to making that distinction, though, and in prep for saving the 
      !  weights we will compute, we need to do the prep steps...
      !       
      !  If we haven't yet initialized the weights stuff, read the cached
      !  file to get all of the old weights.  Then initialize a network
      !  ID string that will be used to clear entries.
      !
      IF (ThsnWgts%ValidNetworks .EQ. 0) THEN
         CALL ReadThsnWeights(GMap);  IF (ErrorLevel .NE. 0) GOTO 899
      END IF
      DO I = 1, MaxNumLocations*4
         BlankID(I:I) = CHAR(0)
      END DO
      
      !
      !  Build the network ID string for this set of locations.
      !  Assumption is that X and Y coordinates are within range (-32000:32000).
      !
      ThisID = BlankID
      DO I = 1, NumLocs
         J = (I-1) * 4 + 1
         X2 = INT2(LocX(I))
         Y2 = INT2(LocY(I))         
         ThisID(J:J+3) = TRANSFER(X2, C2) // TRANSFER(Y2, C2)
      END DO

      !
      !  If we are not outputting met details, then we can search for a 
      !  matching entry in the cached weight sets. If we find one then 
      !  all we need to do is:
      !    1) assign those weights
      !    2) update the SavePriority values
      !    3) return
      !
      IF (.NOT. OutputTheMetDetails) THEN
         K = NumLocs*4     ! valid length of the string for comparison
         DO I = 1, ThsnWgts%ValidNetworks
            IF (ThisID(1:K) .EQ. ThsnWgts%NetworkIDStrings(I)(1:K)) THEN
               DO Sub = 0, 30
                  DO Loc = 1, NumLocs
                     Weights(Sub,Loc) = ThsnWgts%NetworkWeights(Loc,Sub,I)
                  END DO
               END DO
               !
               !  Any entries with a higher (closer to 0) priority get moved
               !  down one spot (+1 to value). Then this entry gets assigned top
               !  priority (1).
               !
               DO J = 1, ThsnWgts%ValidNetworks
                  IF (ThsnWgts%SavePriority(J) .LT. ThsnWgts%SavePriority(I)) THEN
                     ThsnWgts%SavePriority(J) = ThsnWgts%SavePriority(J) + 1
                  END IF
               END DO
               ThsnWgts%SavePriority(I) = 1
               UsedOldWeights = UsedOldWeights + 1
               RETURN
            END IF
         END DO
      END IF

      !
      !  If we get to here, that means we need to compute weights for this network.
      !  Compute weights for each station, for each subbasin.
      !
      CALL ComputeThsnWeights(LocX, LocY, NumLocs, GMap, Weights); IF (ErrorLevel .NE. 0) GOTO 899
      ComputedNewWeights = ComputedNewWeights + 1

      !
      !  We computed new weights, but that might only be because we are doing
      !  the met details. Therefore, this new set of weights might be the same as
      !  an existing entry in the cached weights. So before we add an entry, we
      !  will search for a matching entry in the cached weight sets. If we find 
      !  one then all we need to do is:
      !    1) update the SavePriority values
      !    2) return
      !
      K = NumLocs*4     ! valid length of the string for comparison
      DO I = 1, ThsnWgts%ValidNetworks
         IF (ThisID(1:K) .EQ. ThsnWgts%NetworkIDStrings(I)(1:K)) THEN
            !
            !  Any entries with a higher (closer to 0) priority get moved
            !  down one spot (+1 to value). Then this entry gets assigned top
            !  priority (1).
            !
            DO J = 1, ThsnWgts%ValidNetworks
               IF (ThsnWgts%SavePriority(J) .LT. ThsnWgts%SavePriority(I)) THEN
                  ThsnWgts%SavePriority(J) = ThsnWgts%SavePriority(J) + 1
               END IF
            END DO
            ThsnWgts%SavePriority(I) = 1
            RETURN
         END IF
      END DO

      !
      !  Getting to here means that we computed new weights that were NOT already
      !  in the cached set. So we need to update the cache.
      !  Find the spot where this one goes, and update the SavePriority entries.
      !  If we have one or more empty slots, we just use the next empty one.
      !  If all slots are filled, then we find the "oldest" one (i.e. the one with
      !  SavePriority = CacheSize) and then overwrite that one.
      !  The SavePriority values are adjusted up (+1) for all existing entries, and
      !  this entry is assigned a value of 1.
      !
      IF (ThsnWgts%ValidNetworks .LT. CacheSize) THEN
         !
         !  We have an empty slot
         !
         DO J = 1, ThsnWgts%ValidNetworks
            ThsnWgts%SavePriority(J) = ThsnWgts%SavePriority(J) + 1
         END DO
         I = ThsnWgts%ValidNetworks + 1
         ThsnWgts%SavePriority(I) = 1
         ThsnWgts%ValidNetworks = ThsnWgts%ValidNetworks + 1
      ELSE
         !
         !  We need to overwrite an old one
         !
         DO J = 1, CacheSize
            IF (ThsnWgts%SavePriority(J) .EQ. CacheSize) I = J     ! this is the one to overwrite
         END DO
         DO J = 1, ThsnWgts%ValidNetworks                          ! increment priority for all entries
            ThsnWgts%SavePriority(J) = ThsnWgts%SavePriority(J) + 1
         END DO
         ThsnWgts%SavePriority(I) = 1
      END IF
      
      !
      !  Update the rest of the values for this "slot" in the cache
      !   (network size and weights)
      !
      ThsnWgts%NetworkIDStrings(I) = ThisID
      ThsnWgts%NetworkSize(I) = NumLocs
      DO Sub = 0, 30
         DO Loc = 1, MaxNumLocations
            ThsnWgts%NetworkWeights(Loc,Sub,I) = Weights(Sub,Loc)
         END DO
      END DO
      RETURN

      !
      !  Error handling
      !
  899 ErrorMessage = '[traceback] GetThsnWeights... ';  CALL PassMsg

      END SUBROUTINE GetThsnWeights
      
!------------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Read the file with a stored set of theissen-weights.
      !------------------------------------------------------------------------
      SUBROUTINE ReadThsnWeights(GMap)
      IMPLICIT NONE
      TYPE (TGlerlMap), INTENT(IN) :: GMap

      INTEGER :: I, U1, IOS, Loc, Sub, RNum, RLength
      INTEGER :: Subs, MaxLocs, CSize
      LOGICAL :: OK
      CHARACTER(LEN=150) :: DirName, FName
      CHARACTER(LEN=3) :: Bsn

      !
      !  File name
      !
      DirName = TRIM(GLSHFS_Config%BaseDir) // GMap%Bsn // FilePathSeparator
      FName   = TRIM(DirName) // 'theissenweights_' // GMap%Bsn // '.dat'
      INQUIRE(FILE=TRIM(FName), EXIST=OK)
      IF (.NOT. OK) THEN
         CALL InitializeThiessenWeights
         RETURN
      END IF

      !
      !  First open the file with incorrect (too short) record length info.
      !  We just need to get the header info at this point.
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', RECL=50,    &
           FORM='UNFORMATTED', ACCESS='DIRECT', ERR=811)
      CALL FileWasOpened(U1)
      READ(U1, REC=1, ERR=851) Bsn, Subs, MaxLocs, CSize
      CLOSE(U1)
      CALL FileWasClosed(U1)

      !
      !  Does the file match the current program settings?
      !  If not, then we cannot use the file, and just need to initialize
      !  all of the array stuff.
      !
      OK = .TRUE.
      IF (Bsn     .NE. GMap%Bsn)        OK = .FALSE.
      IF (Subs    .NE. GMap%Subbasins)  OK = .FALSE.
      IF (MaxLocs .NE. MaxNumLocations) OK = .FALSE.
      IF (CSize   .NE. CacheSize)       OK = .FALSE.
      
      IF (.NOT. OK) THEN
         OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', RECL=50,    &
              FORM='UNFORMATTED', ACCESS='DIRECT')
         CLOSE(U1, STATUS='DELETE')
         RETURN
      END IF
      
      !
      !  
      RLength =  4 + 4 + MaxNumLocations*4
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', RECL=RLength,    &
           FORM='UNFORMATTED', ACCESS='DIRECT', ERR=811)
      CALL FileWasOpened(U1)

      !
      !  Read all of the network strings. Each record will also have how many
      !  valid locations were used to build the string.
      !
      DO I = 1, CacheSize
         RNum = 1 + I
         READ(U1, REC=RNum, ERR=852) ThsnWgts%SavePriority(I), ThsnWgts%NetworkSize(I),      &
                                     ThsnWgts%NetworkIDStrings(I)
      END DO

      !
      !  And finally, the actual weights for each network
      !
      DO I = 1, CacheSize
         DO Sub = 0, GMap%Subbasins
            RNum = (1 + CacheSize) + ((I-1)*(GMap%Subbasins+1)) + Sub+1
            READ(U1, REC=RNum, ERR=853) (ThsnWgts%NetworkWeights(Loc,Sub,I), Loc=1,MaxNumLocations)  ! REAL values
         END DO
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)

      !
      !  Determine how many valid sets were in the file
      !
      ThsnWgts%ValidNetworks = 0
      DO I = 1, CacheSize
         IF (ThsnWgts%NetworkSize(I) .GT. 0) ThsnWgts%ValidNetworks = ThsnWgts%ValidNetworks + 1
      END DO
      
      RETURN
      
  811 ErrorMessage = 'Error opening file '             // TRIM(FName);   CALL PassMsg;  GOTO 898
  
  851 ErrorMessage = 'Error reading header record in ' // TRIM(FName);   CALL PassMsg;  GOTO 898
  852 ErrorMessage = 'Error reading network ID from '  // TRIM(FName);   CALL PassMsg;  GOTO 898
  853 ErrorMessage = 'Error reading weights from '     // TRIM(FName);   CALL PassMsg;  GOTO 898
      
  898 ErrorLevel = 1
      ErrorMessage = '[traceback] ReadThsnWeights...';  CALL PassMsg
      CLOSE(U1, IOSTAT=IOS)
      CALL FileWasClosed(U1)
      
      END SUBROUTINE ReadThsnWeights
      
!------------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Write the file with a stored set of theissen-weights.
      !------------------------------------------------------------------------
      SUBROUTINE WriteThsnWeights(GMap)
      IMPLICIT NONE
      TYPE (TGlerlMap), INTENT(IN) :: GMap

      INTEGER :: I, J, U1, IOS, Loc, Sub, RNum, RLength
      CHARACTER(LEN=150) :: DirName, FName
      CHARACTER(LEN=MaxNumLocations*4 + 8) :: NullPad
      
      RLength = 4 + 4 + MaxNumLocations*4
      DO I = 1, RLength
         NullPad(I:I) = CHAR(0)
      END DO

      DirName = TRIM(GLSHFS_Config%BaseDir) // GMap%Bsn // FilePathSeparator
      FName   = TRIM(DirName) // 'theissenweights_' // GMap%Bsn // '.dat'
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='REPLACE', RECL=RLength,    &
           FORM='UNFORMATTED', ACCESS='DIRECT', ERR=811)
      CALL FileWasOpened(U1)
      
      !
      !  Write the first record, which has the basin name and info about 
      !  the network parameters.
      !
      J = RLength - (3 + 4 + 4 + 4)
      WRITE(U1, REC=1, ERR=851) GMap%Bsn, GMap%Subbasins, MaxNumLocations, CacheSize, NullPad(1:J)
      
      !
      !  Now write all of the network strings. Each record will also have how many
      !  valid locations were used to build the string and the "save priority", which
      !  is used to determine which entry to throw out whenever we need to overwrite
      !  an existing entry (i.e. when the array is full but we need to save a newly
      !  computed set of weights).
      !
      !  SavePriority is a descending value. i.e. when a weight set is used, it gets a 
      !  value of 1, and the rest of the entries get their SavePriority value decremented
      !  by 1. This way, the recently-used entries always have low numbers, while the
      !  ones that haven't been used recently have a high value.  We will always
      !  overwrite whichever one has SavePriority = CacheSize.
      !
      DO I = 1, CacheSize
         RNum = 1 + I
         WRITE(U1, REC=RNum, ERR=852) ThsnWgts%SavePriority(I), ThsnWgts%NetworkSize(I),      &
                                      ThsnWgts%NetworkIDStrings(I)
      END DO

      !
      !  And finally, the actual weights for each network
      !
      DO I = 1, CacheSize
         DO Sub = 0, GMap%Subbasins
            RNum = (1 + CacheSize) + ((I-1)*(GMap%Subbasins+1)) + Sub+1
            WRITE(U1, REC=RNum, ERR=853) (ThsnWgts%NetworkWeights(Loc,Sub,I), Loc=1,MaxNumLocations)  ! REAL values
         END DO
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)

      RETURN
      
  811 ErrorMessage = 'Error opening file '             // TRIM(FName);   CALL PassMsg;  GOTO 898
  851 ErrorMessage = 'Error writing header record to ' // TRIM(FName);   CALL PassMsg;  GOTO 898
  852 ErrorMessage = 'Error writing network ID to '    // TRIM(FName);   CALL PassMsg;  GOTO 898
  853 ErrorMessage = 'Error writing weights to '       // TRIM(FName);   CALL PassMsg;  GOTO 898
      
  898 ErrorLevel = 1
      ErrorMessage = '[traceback] WriteThsnWeights...';  CALL PassMsg;  GOTO 898
      CLOSE(U1, IOSTAT=IOS)
      CALL FileWasClosed(U1)
      
      END SUBROUTINE WriteThsnWeights
      
!------------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Compute a set of weights to be applied to each location represented in
      !  the Lats and Lons arrays.
      !
      !  The LocX and LocY arrays contain the cell coordinates of each location
      !  as an integer offset from the map origin of (0,0) at the SW corner.
      !
      !  Input:
      !    LocX   = 1-D array of location X coordinates; indexed (1:NumStations)
      !    LocY   = 1-D array of location Y coordinates; indexed (1:NumStations)
      !    GMap   = GLERL digital map object (TGlerlMap)
      !
      !  Output:
      !    Weights  = 2-D array of weights for each station;
      !               indexed (0:30, 1:NumStations)
      !               Note that the declaration of the array forces the lower 
      !               bound of the first index to be 0, matching the
      !               declaration I assume to have been made in the caller.
      !
      !  Addition in 2020 by Tim Hunter:
      !  We now also build the ThsnCellMap that has a location index in every 
      !  grid cell. That index corresponds to the location that is used
      !  for the cell.
      !
      !------------------------------------------------------------------------
      SUBROUTINE ComputeThsnWeights(LocX, LocY, NumLocs, GMap, Weights)
      IMPLICIT NONE
      INTEGER, DIMENSION(:), INTENT(IN)    :: LocX, LocY
      INTEGER,               INTENT(IN)    :: NumLocs
      TYPE (TGlerlMap),      INTENT(IN)    :: GMap
      REAL, DIMENSION(0:,:), INTENT(INOUT) :: Weights

      INTEGER :: I, IOS, MC, MR, TCC, SX, SY, SB
      INTEGER :: Subbasin, NearestLoc
      REAL    :: Dist, MinDist

      INTEGER, DIMENSION(:), ALLOCATABLE :: CellCount
      INTEGER (KIND=INTEGER2), DIMENSION(:,:), ALLOCATABLE :: NearestMap
      
      !
      !
      MR = GMap%MapHeight-1 
      MC = GMap%MapWidth-1  
      ALLOCATE(CellCount(NumLocs), NearestMap(0:MC,0:MR), STAT=IOS)
      IF (IOS .NE. 0) THEN
         ErrorMessage = 'Error allocating memory for local arrays.'; CALL PassMsg
         GOTO 898
      END IF

      !
      !  Build the NearestMap.  This is a grid, matching the basin cell map, that
      !  tells which location is closest to that grid cell. 
      !  This is exactly what we need for the ThsnCellMap, and it also will save
      !  a bunch of computation when building the subbasin weight stuff, since we
      !  won't have to do all of those distance calculations again and again.
      !  We just do them here, and then use this NearestMap to build the
      !  subbasin weights.
      !
      DO MR = 0, GMap%MapHeight-1
         DO MC = 0, GMap%MapWidth-1
            SB = ICHAR(GMap%Map2D(MC,MR))      ! subbasin for this cell
            MinDist = 9.9e19
            NearestLoc = -1
            DO I = 1, NumLocs
               SX = LocX(I)
               SY = LocY(I)
               Dist = DistanceCellXY(SX, SY, MC, MR, GMap)
               IF (Dist .LT. MinDist) THEN
                  NearestLoc = I
                  MinDist = Dist
               END IF
            END DO
            NearestMap(MC,MR) = NearestLoc
         END DO
      END DO

      IF (OutputTheMetDetails) THEN
         ThsnCellMap(:,:) = NearestMap(:,:)
      END IF
      
      !
      !  For each subbasin...
      !     Fill the CellCount array (1..NumLocs) with a count of how many times
      !     that location was the closest location to a cell of the subbasin.
      !
      DO Subbasin = 0, GMap%Subbasins
         CellCount(:) = 0
         DO MR = 0, GMap%MapHeight-1
            DO MC = 0, GMap%MapWidth-1
               SB = ICHAR(GMap%Map2D(MC,MR))      ! subbasin for this cell
               IF (SB .EQ. Subbasin) THEN
                  NearestLoc = NearestMap(MC,MR)
                  CellCount(NearestLoc) = CellCount(NearestLoc) + 1
               END IF
            END DO
         END DO

         !
         !  Now, how many cells in total?
         !
         TCC = 0      ! total cell count
         DO I = 1, NumLocs
            TCC = TCC + CellCount(I)
         END DO

         !
         !  Compute weight for each station
         !  Each value will be in the range 0..1, and the 
         !  sum of all weights = 1.0
         !
         DO I = 1, NumLocs
            Weights(Subbasin,I) = (CellCount(I) * 1.0) / TCC
         END DO
      END DO
      
      GOTO 999
      
      !
      !  Error handling
      !
  898 ErrorLevel = 1
      ErrorMessage = '[traceback] ComputeThsnWeights...';  CALL PassMsg
  
  999 DEALLOCATE(CellCount, NearestMap, STAT=IOS)
      RETURN
      END SUBROUTINE ComputeThsnWeights

!------------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Sort the arrays by increasing X, then Y.
      !  Be sure to keep all arrays synchronized.
      !------------------------------------------------------------------------
      SUBROUTINE SortTheLocations(TX, TY, TVals, NumStns)
      IMPLICIT NONE
      INTEGER, DIMENSION(:), INTENT(INOUT) :: TX, TY
      REAL,    DIMENSION(:), INTENT(INOUT) :: TVals
      INTEGER,               INTENT(IN)    :: NumStns
      
      INTEGER (KIND=INTEGER2) :: X2, Y2
      INTEGER :: I, J, Swap_X, Swap_Y
      REAL    :: Swap_Val
      CHARACTER(LEN=2) :: C2
      CHARACTER(LEN=4) :: Swap_XY
      CHARACTER(LEN=4), DIMENSION(MaxNumLocations) :: XY
      
      !
      !  Construct an array with a single combined X,Y value
      !  This will greatly simplify the sort.
      !  I am assuming that the values in TX/TY are within range(-32000:32000)
      !  That should be valid as long as the locations are expressed in kilometers.
      !
      DO I = 1, NumStns
         X2 = INT2(TX(I))
         Y2 = INT2(TY(I))
         XY(I) = TRANSFER(X2, C2) // TRANSFER(Y2, C2)
      END DO

      !
      !  Now do the sorting
      !
      DO I = 1, NumStns-1
         DO J = I+1, NumStns
            IF (XY(J) .LT. XY(I)) THEN
               Swap_XY  = XY(J)
               Swap_X   = TX(J)
               Swap_Y   = TY(J)
               Swap_Val = TVals(J)
               
               XY(J)    = XY(I)
               TX(J)    = TX(I)
               TY(J)    = TY(I)
               TVals(J) = TVals(I)
               
               XY(I)    = Swap_XY
               TX(I)    = Swap_X
               TY(I)    = Swap_Y
               TVals(I) = Swap_Val
            END IF
         END DO
      END DO
      
      END SUBROUTINE SortTheLocations
      
!------------------------------------------------------------------------------
      !----------------------------------------------------------------------
      !  Output detailed information about the thiessen weights.
      !  Some of the info is passed as arguments, and some of it is in the
      !  module global variables named thsn_*
      !----------------------------------------------------------------------
      SUBROUTINE WriteMetDetails(GMap, NumLocs, TX, TY, TVals, Weights)
      IMPLICIT NONE
      TYPE (TGlerlMap), INTENT(IN) :: GMap
      INTEGER,          INTENT(IN) :: NumLocs
      INTEGER,           DIMENSION(MaxNumLocations),       INTENT(IN) :: TX, TY   !  cell offset from (0,0) SW corner of map
      REAL,              DIMENSION(MaxNumLocations),       INTENT(IN) :: TVals
      REAL,              DIMENSION(0:30, MaxNumLocations), INTENT(IN) :: Weights
      
      INTEGER :: I, J, U, C, R, IOS, Sub
      INTEGER :: NumInList, NumSubs
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=7)   :: MapEntry
      CHARACTER(LEN=10)  :: DateStr
      CHARACTER(LEN=20)  :: DTStr
      CHARACTER(LEN=30)  :: StnStr
      CHARACTER(LEN=200) :: FName, BsnDirName, MetDirName, ListStr
      CHARACTER(LEN=780*8) :: Line             ! max columns * entry size (incl commas plus a little extra)
      
      Bsn = GMap%Bsn
      NumSubs = GMap%Subbasins
      
      BsnDirName = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator 
      MetDirName = TRIM(BsnDirName) // 'met_details' // FilePathSeparator
      
      !
      !  We need to ensure that the met_details directory exists.  Fortran
      !  does not have a clean method for doing this (INQUIRE doesn't work)
      !  so here is a somewhat ugly workaround....
      !  Try to create a new file in the desired directory.  
      !  If that works, then the directory DOES exist and we can proceed.  
      !  If it fails, we need to create the directory.
      !
      FName = TRIM(MetDirName) // 'deleteme.txt'
      U = GetFreeUnitNumber()
      OPEN(UNIT=U, FILE=TRIM(FName), STATUS='REPLACE', IOSTAT=IOS)
      IF (IOS .EQ. 0) THEN
          CLOSE(U, STATUS='DELETE')
      ELSE
          CALL CreateDirectory(MetDirName)
      END IF
      
      !
      !  Build filename and open file for detailed output
      !
      DTStr = GetLowercase(GlerlDataTypeString(Thsn_DType))
      DateStr = SeqToDateStringYMD(Thsn_Seq)
      FName = TRIM(MetDirName) // 'details_' // TRIM(DTStr) // '_' // DateStr // '.txt'
      U = GetFreeUnitNumber()
      OPEN(UNIT=U, FILE=TRIM(FName), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U)
      
      !
      !  Basic header info
      !
      WRITE(U, 1100, ERR=813) 
      WRITE(U, 1101, ERR=813) Bsn, DtStr
      WRITE(U, 1102, ERR=813) DateStr
      WRITE(U, 1103, ERR=813) NumLocs     ! the number of *LOCATIONS* used in computing weights
      
      !
      ! The info for each location used...
      !
      WRITE(U, *, ERR=813) '------------ Location Info ----------------'
      WRITE(U, 1110, ERR=813)
      DO I = 1, NumLocs
          !
          !  Build a list of the stations that fall on this location.
          !  This is where those Thsn_StnX, Thsn_StnY and Thsn_StnID arrays
          !  come into play.  Those arrays contain the info for the original
          !  stations, not the collapsed set of locations. As such, we can
          !  cycle through all of those STATIONS looking for all of them
          !  that match this location. 
          !
          ListStr = '['
          NumInList = 0
          DO J = 1, Thsn_NumStns
              IF ((Thsn_StnX(J) .EQ. TX(I)) .AND. (Thsn_StnY(J) .EQ. TY(I))) THEN
                 IF (NumInList .EQ. 0) THEN
                    StnStr = TRIM(Thsn_StnID(J))
                 ELSE
                    StnStr = ',' // TRIM(Thsn_StnID(J))
                 END IF
                 ListStr = TRIM(ListStr) // TRIM(StnStr)
                 NumInList = NumInList + 1
              END IF
          END DO
          ListStr = TRIM(ListStr) // ']'

          !      
          WRITE(U, 1111, ERR=813) I, TX(I), TY(I), TVals(I), TRIM(ListStr)      
      END DO

      !
      !  Write the weights for each location subbasin
      !
      WRITE(U, *, ERR=813) '------------ subbasin weights ----------------'
      WRITE(U, 1120, ERR=813) (Sub, Sub=0,NumSubs)
      DO I = 1, NumLocs
          WRITE(U, 1121, ERR=813) I, TVals(I), (Weights(Sub,I), Sub=0,NumSubs)
      END DO

      !
      !  Write the cell map
      !  Note that the map arrays are arranged South to North, so for this output
      !  file I will go in reverse row order (North to South) so that it is visually
      !  congruent with human expectations.
      !
      WRITE(U, 1132, ERR=813) '------------ Thiessen Map ----------------'
      WRITE(U, 1130, ERR=813) GMap%MapHeight, GMap%MapWidth
      WRITE(U, 1132, ERR=813) 'Each cell entry is in the format subbasin:index'
      WRITE(U, 1132, ERR=813) 'e.g.  00:0136 means subbasin 0 (the lake) and location index 136'
      WRITE(U, 1132, ERR=813) '      subbasin number 99 means outside the basin   '
      DO R = GMap%MapHeight-1, 0, -1
         Line = ''
         DO C = 0, GMap%MapWidth-1
            Sub = ICHAR(GMap%Map2d(C,R))
            IF (Sub .GT. NumSubs) Sub = 99
            WRITE(MapEntry, 1131) Sub, ThsnCellMap(C,R)
            IF (C .EQ. 0) THEN
                Line = MapEntry
            ELSE
                Line = TRIM(Line) // ',' // MapEntry
            END IF      
         END DO
         WRITE(U, 1132, ERR=813) TRIM(Line)
      END DO
      
      GOTO 999
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening file '    // TRIM(FName);   CALL PassMsg;  GOTO 898
  813 ErrorMessage = 'Error writing to file ' // TRIM(FName);   CALL PassMsg;  GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] WriteMetDetails...';  CALL PassMsg

  999 CLOSE(U, IOSTAT=IOS)
      IF (IOS .EQ. 0) CALL FileWasClosed(U)

      !
      !  FORMAT statements
      !
 1100 FORMAT('Station and weight info for processing of met data.')
 1101 FORMAT('Lake:,', A3, ',  DType:,', A)
 1102 FORMAT('Date:, ', A10)
 1103 FORMAT('Number of locations:,', I4)
 1110 FORMAT('Index,    X,   Y,   Value, [stations at this location]')
 1111 FORMAT(I5, 2(',', I5), ',', F8.2, ',', A)
 1120 FORMAT('Index,    Val', 30(', Sub', I2.2))
 1121 FORMAT(I5, ',', F7.2, 30(',', F6.4))
 1130 FORMAT('rows:,', I3, ',  cols:,', I3)
 1131 FORMAT(I2.2, ':', I4.4)
 1132 FORMAT(A)

      END SUBROUTINE WriteMetDetails

END MODULE Subbasin_Met

      
