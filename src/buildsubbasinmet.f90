!--------------------------------------------------------------
!  Assumptions:
!     1) Station data in GLERL's standard met_*.csv format are ready to be processed.
!     2) Those station files are in the current folder.
!     3) The following files also exist in the current folder:
!        BasinInfo.txt
!
!      
!      
!--------------------------------------------------------------
      
MODULE BuildSubbasinMet
      USE ErrorProcess
      USE StatusMessages
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GL_Constants
      USE GLSHFS_ConfigFile
      USE GLSHFS_Files
      USE GlerlDataTypesAndUnits
      USE GlerlMaps
      USE BasinInfo
      USE DailyDataStructures
      USE Subbasin_Met
      USE StnData_RangeCheck
      USE StationData_Aggregation
     
CONTAINS

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
      SUBROUTINE BuildFileListForEachLake(StnDir)
      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(IN) :: StnDir
      
      INTEGER :: IOS, Lk, U1, U2, U3, I, Good
      LOGICAL :: FExist
      REAL    :: Dist, Lat, Long
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=200) :: BigListFile, MapFile, File3, FileOut, LakeDir
      TYPE (TDlyDataMetStn)  :: SData
      TYPE (TGlerlMap)       :: LakeMap
      TYPE (THeaderInfoType) :: HdrInfo
      
      !
      !  Instantiate objects used in this routine.  
      !
      SData   = TDlyDataMetStn()
      LakeMap = TGlerlMap()
      HdrInfo = THeaderInfoType()

      !
      !  Clear out old files. New versions will be created, but right here I want 
      !  to be absolutely certain any old version is removed so that there is no
      !  possibility of using the old one for subsequent operations.
      !
      DO Lk = 1, 7
         FileOut = TRIM(StnDir) // 'list_of_stations_' // TRIM(LakeName3(Lk)) // '.csv'
         CALL DeleteFile_SYSTEM(FileOut)
      END DO
      
      
      !
      !  Specify the "master list" for the next step. This list was built during a
      !  previous step and only includes the stations that had 0 fatal errors.
      !
      !  Note that the file names in this file include the entire file specification,
      !  including the full file path.
      !
      BigListFile = TRIM(StnDir) // 'station_file_list_good.txt'

      !
      !  For each lake, build an output file listing the station files that are relevant
      !  for the lake. We do that by reading the header info of each station file
      !  and comparing the location to the lake map. If the station is within the
      !  specified maximum distance, then it gets added to the lake's list.
      !
      !  Note that we only do this for the 7 main basins (Sup, Mic, Hur, Geo, 
      !  Stc, Eri, Ont), and not for the aggregations (Mhg, Hgb)
      !
      !  Change made on 2017Aug23...
      !  Rather than require a duplicate copy of the map files to exist in StnDir, I will
      !  use the map file that is already required to exist in the lake's directory.
      !
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(BigListFile), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      DO Lk = 1, 7
         REWIND(U1)
         
         !
         !  Make sure the necessary lake map file exists, then read it.
         !
         Bsn = TRIM(LakeName3(Lk))
         LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator
         MapFile = TRIM(LakeDir) // Bsn // 'bytcd.map'
         INQUIRE(FILE=TRIM(MapFile), EXIST=FExist)
         IF (.NOT. FExist) THEN
            ErrorMessage = 'Missing lake map file: ['//TRIM(MapFile)//']'; CALL PassMsg
            GOTO 898
         END IF
         CALL LakeMap%Clear()
         CALL ReadGlerlMap_Old(MapFile, Bsn, LakeMap); IF (ErrorLevel .NE. 0) GOTO 899
         
         !
         !  Build a list of stations within X km of the basin boundary  
         !  This will be a CSV file, with filename and distance info
         !
         FileOut = TRIM(StnDir) // 'list_of_stations_' // TRIM(LakeName3(Lk)) // '.csv'
         U2 = GetFreeUnitNumber()
         OPEN(UNIT=U2, FILE=TRIM(FileOut), STATUS='REPLACE', ERR=821)
         CALL FileWasOpened(U2); IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(U2, 1200)

         READ(U1, 1101, IOSTAT=IOS) File3
         I = 0
         Good = 0
         StatusMsg = 'stationid'; CALL WriteMsg(.TRUE., .TRUE.)
         DO WHILE (IOS .EQ. 0)
            I = I + 1
            WRITE(StatusMsg, 2001) Bsn, I; CALL WriteMsg(.TRUE., .TRUE.)
            U3 = GetFreeUnitNumber()
            OPEN(UNIT=U3, FILE=TRIM(File3), STATUS='OLD', ERR=831)
            CALL FileWasOpened(U3); IF (ErrorLevel .NE. 0) GOTO 899
            CALL HdrInfo%Clear()
            CALL ReadHeaderInfo(File3, U3, HFT_SingleStation, HdrInfo);  IF (ErrorLevel .NE. 0) GOTO 899
            CLOSE(U3)
            CALL FileWasClosed(U3); IF (ErrorLevel .NE. 0) GOTO 899
            Lat  = HdrInfo%Latitude(1)
            Long = HdrInfo%Longitude(1)
            Dist = DistanceFromNearestSubbasin(Lat, Long, LakeMap, SubNum_OverBasin);    IF (ErrorLevel .NE. 0) GOTO 899
            IF (Dist .LE. GLSHFS_Config%BasinBoundaryBuffer) THEN
               WRITE(U2, 1201, ERR=823) TRIM(File3), Dist
               Good = Good + 1
            END IF
            READ(U1, 1101, IOSTAT=IOS) File3
         END DO
         WRITE(StatusMsg, *) '';              CALL WriteMsg(.FALSE., .FALSE.)    ! Finish the line we are on and drop to next one for summary info  
         WRITE(StatusMsg, 2002) Bsn, Good, I; CALL WriteMsg(.FALSE., .FALSE.)    ! Write the summary info
         
         CLOSE(U2)
         CALL FileWasClosed(U2)
      END DO

      CLOSE(U1)
      CALL FileWasClosed(U1)
      GOTO 999
      
      !
      !  Handlers for I/O errors
      !
  811 ErrorMessage = 'Error opening input file '  // TRIM(BigListFile); CALL PassMsg; GOTO 898
  821 ErrorMessage = 'Error opening output file ' // TRIM(FileOut);     CALL PassMsg; GOTO 898
  823 ErrorMessage = 'Error writing output file ' // TRIM(FileOut);     CALL PassMsg; GOTO 898
  831 ErrorMessage = 'Error opening input file '  // TRIM(File3);       CALL PassMsg; GOTO 898
      

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] BuildFileListForEachLake()...'; CALL PassMsg
      GOTO 999
      
  999 CALL SData%Clear()
      CALL LakeMap%Clear()
      RETURN
      
      !
      !  FORMAT statements
      !
  1101 FORMAT(A)
  1200 FORMAT('Station Filename, Distance from basin (km)')
  1201 FORMAT(A, ',', F0.2)
      
  2001 FORMAT(3X, 'Sorting ', A3, ': station #', I0)
  2002 FORMAT(3X, 'For ', A3, ': ', I0, '/', I0, ' stations were selected')
  
      END SUBROUTINE BuildFileListForEachLake

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
      SUBROUTINE BuildSubbasinMeteorology()
      IMPLICIT NONE
      INTEGER :: Lk

      IF (GLSHFS_Config%AddStationData) THEN
         StatusMsg = 'Adding station data to the stndata_*.csv files';   CALL WriteMsg()
      
         !
         !  Process station data files, removing any range errors
         !  This checks every station file for obvious data errors, mainly
         !  checking for values falling outside an assigned valid range.
         !  Invalid data is removed when possible. Errors that cannot be
         !  corrected are flagged as "fatal", and that station file will be
         !  ignored during all subsequent processing.
         !  
         !  Two file lists are also built during this phase.
         !  List 1 is the files that pass (no fatal errors), named station_file_list_good.txt
         !  List 2 is the files that fail (fatal errors >0), named station_file_list_bad.txt
         !
         !  Subsequent processing will rely on the station_file_list_good.txt file.
         !
         ErrorLevel = 0
         StatusMsg = 'Range checking station data';   CALL WriteMsg()
         CALL DoStnDataRangeChecking(TRIM(GLSHFS_Config%StnDir));  IF (ErrorLevel .NE. 0) GOTO 899
         
         !
         !  Do cross-checking (comparison between stations)
         !  Not yet implemented.
         !
         
         !
         !  Sort the station list by lake basin.
         !  Use the value in the configuration file (MetStationBufferSize) along with the
         !  lake basin maps to do this sorting.
         !  This routine just builds sorted file lists, in the "DirStn" directory. 
         !  Actual processing of the station data is the next step.
         !
         ErrorLevel = 0
         CALL BuildFileListForEachLake(TRIM(GLSHFS_Config%StnDir));  IF (ErrorLevel .NE. 0) GOTO 899

         !
         !  For each lake...
         !  Add station data to large historical datasets of station values.
         !  Upon completion of this loop we have updated versions of:
         !     stndata_<datatype>_<bsn>.csv
         !
         !  For simplicity right now, I will just assume ALL lakes need to be processed.
         !  Future refinement might be to change this based on user-choices about what lakes
         !  are of interest.
         !
         !  Within the subroutine, the station data will be read from the 
         !  "DirStn" directory (from config file data), and the resulting
         !  output files will be in each of the lake directories.
         !
         ErrorLevel = 0
         ErrorMessage = 'Now aggregating station files for each lake...'; CALL PassMsg
         DO Lk = 1, 7 
            CALL AggregateTheStationData(LakeName3(Lk));  IF (ErrorLevel .NE. 0) GOTO 899
         END DO
      ELSE
         ErrorMessage = 'As specified in configuration file, bypassing step to add station data.';  CALL PassMsg
      END IF
      
      IF (GLSHFS_Config%BuildSubbasinMet) THEN
         !
         !  Compute areal averages for each subbasin for the available met data,
         !  from the supplied station data in files stndata_<dt>_<bsn>.csv
         !  (e.g. stndata_airtempmax_sup.csv)
         !
         !  This will create/update the files subdata_<bsn><sub>.csv
         !  (e.g. subdata_sup01.csv)
         !  
         DO Lk = 1, 7
            CALL ComputeSubbasinMet(LakeName3(Lk));  IF (ErrorLevel .NE. 0) GOTO 899
!            Bsn = LakeName3(Lk)
!            FName = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator // 'basininfo.txt'
!            CALL ReadBasinInfoFile(FName, BData);  IF (ErrorLevel .NE. 0) GOTO 899
!            BData%NewDataDate_LBRM = EarliestNewStationData
!            BData%NewDataDate_LLTM = EarliestNewStationData
!            CALL WriteBasinInfoFile(FName, BData);  IF (ErrorLevel .NE. 0) GOTO 899
         END DO
      ELSE
         ErrorMessage = 'As specified in configuration file, bypassing step to build subbasin met data.';  CALL PassMsg
      END IF

      RETURN
      
      !
      !  Handlers for I/O errors
      !
  

!  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] BuildSubbasinMeteorology...'; CALL PassMsg

  
      END SUBROUTINE BuildSubbasinMeteorology
      
END MODULE BuildSubbasinMet
      