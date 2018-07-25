MODULE GLSHFS_LLTM
   USE ErrorProcess
   USE GLSHFS_Util
   USE GLSHFS_Global
   USE GLSHFS_Files
   USE DailyDataStructures
   USE GlerlDataTypesAndUnits
   USE BasinInfo
   USE LLTM              ! for structured data types and some i/o procedures
   
   PRIVATE
   PUBLIC :: UpdateLLTM_Simulation
   PUBLIC :: RunLLTM_Forecasts
   
!INTEGER :: DbgIndx
   
   !
   !  This array specifies water surface temp values to use when we are unable
   !  to find a heat layer file to use for intialization.
   !  These are VERY rough and arbitrary values.
   !
   REAL, DIMENSION(12), PARAMETER :: DefaultWaterSurfTemps =                  &
      (/ 0.5, 0.5, 1.5, 3.0, 4.0, 8.0, 13.0, 14.0, 13.0, 8.0, 3.0, 1.0 /)

CONTAINS
!------------------------------------------------------------------------
      !----------------------------------------------------------------------------      
      !   This routine assumes the existence of the standard GLSHFS overlake
      !   meteorology files; all lakes. 
      !   It sets up and runs the LLTM for all lakes, entire period.
      !----------------------------------------------------------------------------      
      SUBROUTINE UpdateLLTM_Simulation
      IMPLICIT NONE

      INTEGER :: Lk, U2, SSeq, ESeq
      LOGICAL :: OK, FillTheDataType(6)
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=200) :: LakeDir, CfgFile, ParmFile, MetFile, MissFile, OutFile
      CHARACTER(LEN=200) :: LayerFile, OldFile, NewFile, MrgFile, FName
      TYPE (THeaderInfoType) :: Hdr
      TYPE (TDlyDataForSubbasin) :: SubData

      !
      !  Create/initialize the objects used when reading the overlake 
      !  meteorology "subbasin" file.
      !
      Hdr = THeaderInfoType()
      SubData = TDlyDataForSubbasin()

      !
      !  For each lake basin...
      !
      DO Lk = 1, 7
         Bsn = LakeName3(Lk)                                                        ! LakeName3 defined in glshfs_global
         WRITE(LakeDir, 1501) TRIM(GLSHFS_Config%BaseDir), Bsn, FilePathSeparator 

         !
         !  Construct LLTM file names.
         !
         !  Note that we are specifying MetFile=MissFile.  The named file is assumed
         !  to contain the entire historical period of record, and is the best
         !  source for long-term data to use when filling in missing data.
         !
         !  The output file is the output from this run, and is really just a temporary
         !  file. The contents will eventually be merged with the archival output file.
         !
         !  Similarly, the heat layer file is treated the same way.
         !
         CfgFile = TRIM(LakeDir) // 'lltm.cfg'
         WRITE(ParmFile,  1701) TRIM(LakeDir), Bsn
         WRITE(MetFile,   1702) TRIM(LakeDir), Bsn
         WRITE(MissFile,  1702) TRIM(LakeDir), Bsn
         WRITE(OutFile,   1703) TRIM(LakeDir), Bsn      ! temporary file for output from this run only
         WRITE(LayerFile, 1704) TRIM(LakeDir), Bsn

         !
         !  Read the header info from the overlake meteorology data file (subdata_xxx00.csv)
         !  to get the date extent of the data.
         !
         WRITE(FName, 1502) TRIM(LakeDir), Bsn
         CALL ReadJustHeaderInfo(FName, HFT_SingleSubbasin, Hdr); IF (ErrorLevel .NE. 0) GOTO 899
         
         !
         !  Now calculate the necessary period for running to update the historical LLTM outputs.  
         !
         SSeq = Hdr%NewDataDate
         ESeq = Hdr%EDate                  ! end of actual data available (from subdata_xxx00.csv)
         IF (SSeq .LT. Hdr%SDate) SSeq = Hdr%SDate

         !
         !  Do we need to do anything?
         !  If the value of SSeq > ESeq it means that we have already done this step since
         !  the last time we updated the subbasin met data, and we really do not need to 
         !  run the LLTM.
         !
         IF (SSeq .LE. ESeq) THEN
            WRITE(ErrorMessage, 5001) Bsn;   CALL PassMsg
            
            !
            !  Build input files for LLTM that are appropriate for the data.
            !    1) Parameter file
            !    2) Met input file
            !    3) Control (configuration) file
            !
            OK = BuildParameterFile_LLTM(ParmFile, Bsn, SSeq, ESeq);     IF (ErrorLevel .NE. 0) GOTO 899
            
            WRITE(FName, 1502) TRIM(LakeDir), Bsn
            CALL ConvertMet_GLSHFS_to_LLTM(TRIM(FName), TRIM(MetFile));  IF (ErrorLevel .NE. 0) GOTO 899

            !
            !  Fill in any missing data in the input met file by replacing missing
            !  values with the long-term mean for that day of the year as calculated
            !  from the existing valid data in the file.
            !
            !  Only do this for the 4 historical data values (airtemp,
            !  dewpoint, wind, cloud). Radiation forcings are not being used
            !  for the historical period right now (June, 2017), and I do
            !  not have them in the historical data file. If I tried to fill
            !  in that data it would cause an error due to having no valid
            !  data to use in computing the long-term mean values.
            !  If this changes in the future, and we DO have radiation forcings
            !  in the historical period, then we might want to revisit this.
            !
            FillTheDataType(1) = .TRUE.       ! GDT_AirtempMean
            FillTheDataType(2) = .TRUE.       ! GDT_DewpointMean
            FillTheDataType(3) = .TRUE.       ! GDT_Windspeed
            FillTheDataType(4) = .TRUE.       ! GDT_CloudCover
            FillTheDataType(5) = .FALSE.      ! GDT_IncidentRad
            FillTheDataType(6) = .FALSE.      ! GDT_NetLongWaveRad
            CALL FillAnyMissingMetData(TRIM(MetFile), FillTheDataType);  IF (ErrorLevel .NE. 0) GOTO 899
            
            !
            !  Write the LLTM config file
            !
            U2 = GetFreeUnitNumber()
            OPEN(UNIT=U2, FILE=TRIM(CfgFile), STATUS='REPLACE')
            CALL FileWasOpened(U2)
            WRITE(U2, 1801) 'GLSHFS LLTM run (historical update)'
            WRITE(U2, 1802) TRIM(ParmFile)
            WRITE(U2, 1803) TRIM(MetFile)
            WRITE(U2, 1804) TRIM(MissFile)
            WRITE(U2, 1805) TRIM(OutFile)
            WRITE(U2, 1806) TRIM(LayerFile)
            WRITE(U2, 1807) TRIM(LogicalToText(GLSHFS_Config%LltmApplyDataCorrectionsHist))
            WRITE(U2, 1808) GLSHFS_Config%LltmMethodRadiationHist
            CLOSE(U2)
            CALL FileWasClosed(U2)

            !
            !  Run the LLTM for the identified update period
            !
            CALL DO_LLTM(TRIM(CfgFile));  IF (ErrorLevel .NE. 0) GOTO 899
            
            !
            !  Merge the new output (for the update period) with the data from the 
            !  entire period, overwriting data in the overlap portion.
            !  The merged file will end at the end of the new file, even if the old
            !  file extends further, because any results in the old file after the
            !  end of the new file would be discontinuous/obsolete.
            !
            !  Whenever there is overlap in the archival file and the output from the 
            !  run just completed, the new value will overwrite the archival value.
            !
            !  OldFile = the archival permanent file.
            !  NewFile = The new data just created by this run of LLTM
            !  MrgFile = The merged results (temporary file)
            !
            WRITE(OldFile, 1705) TRIM(LakeDir), Bsn
            NewFile = TRIM(OutFile)
            WRITE(MrgFile, 1711) TRIM(LakeDir), Bsn
            CALL MergeOutputFiles_LLTM(OldFile, NewFile, MrgFile);  IF (ErrorLevel .NE. 0) GOTO 899
               
            !
            !  Replace the old file with the merged one.
            !
            CALL DeleteFile(OldFile)
            CALL RenameFile(MrgFile, OldFile, OK);  IF (.NOT. OK) GOTO 899
               
            !
            !  Merge the heat layer info. 
            !  Whenever there is overlap in the archival file and the output from the 
            !  run just completed, the new value will overwrite the archival value.
            !
            !  OldFile = the archival permanent file
            !  NewFile = The new data just created by this run of LBRM
            !  MrgFile = The merged results (temporary file)
            !
            WRITE(OldFile, 1706) TRIM(LakeDir), Bsn
            NewFile = TRIM(LayerFile)
            WRITE(MrgFile, 1712) TRIM(LakeDir), Bsn
            CALL MergeLayerFiles_LLTM(OldFile, NewFile, MrgFile);  IF (ErrorLevel .NE. 0) GOTO 899
               
            !
            !  Replace the old file with the merged one.
            !
            CALL DeleteFile(OldFile)
            CALL RenameFile(MrgFile, OldFile, OK);  IF (.NOT. OK) GOTO 899
            
            !
            !  Update the subbasin meteorology files to change the "NewDataStart(YMD):" entry.
            !  We have just finished processing data through the end of the file, so update
            !  the NewDataStart entry to reflect that.
            !
            CALL SubData%Clear()
            WRITE(FName, 1502) TRIM(LakeDir), Bsn
            CALL ReadFile_OneSubbasin(TRIM(FName), SubData);  IF (ErrorLevel .NE. 0) GOTO 899
            SubData%NewDataSSeq = ESeq + 1
            CALL WriteFile_OneSubbasin(TRIM(FName), SubData);  IF (ErrorLevel .NE. 0) GOTO 899
            CALL SubData%Clear()
         END IF
      END DO

      RETURN
      
      !
      !  Error handling
      !
!  811 ErrorMessage = 'Error opening input file ' //TRIM(FName);  CALL PassMsg; GOTO 898
  
      
!  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] UpdateLLTM_Simulation...'; CALL PassMsg
      RETURN


 1501 FORMAT(A, A3, A1)
 1502 FORMAT(A, 'subdata_', A3, '00.csv')

 1701 FORMAT(A, 'lltm_parm_',        A3, '.csv')
 1702 FORMAT(A, 'lltm_meteorology_', A3, '.csv') 
 1703 FORMAT(A, 'glshfs_lltm_out_',  A3, '.csv') 
 1704 FORMAT(A, 'glshfs_lltm_heat_', A3, '.csv') 
 1705 FORMAT(A, 'lltm_output_',      A3, '.csv') 
 1706 FORMAT(A, 'lltm_heatlayers_',  A3, '.csv') 
 1711 FORMAT(A, 'lltm_output_mrg_',  A3, '.csv')
 1712 FORMAT(A, 'lltm_layers_mrg_',  A3, '.csv')
 
 
 1801 FORMAT('ID        = ', A)
 1802 FORMAT('PARMFILE  = ', A)
 1803 FORMAT('METFILE   = ', A)
 1804 FORMAT('MISSFILE  = ', A)
 1805 FORMAT('OUTFILE   = ', A)
 1806 FORMAT('LAYERFILE = ', A)
 1807 FORMAT('ApplyOverwaterCorrection = ', A)
 1808 FORMAT('RadiationMethod = ', I0)
         
 5001 FORMAT('Running the LLTM to update ', A3, ' for the historical data period.')

      END SUBROUTINE UpdateLLTM_Simulation


      !----------------------------------------------------------------------------      
      !   This routine assumes the existence of the standard GLSHFS overlake
      !   meteorology files; all lakes. 
      !   It sets up and runs the LLTM for the outlook period, for each of the
      !   user-specified ensemble members. This might be a list of historical years,
      !   a specifier for ALL possible years, or a bunch of user-supplied met scenarios.
      !----------------------------------------------------------------------------      
      SUBROUTINE RunLLTM_Forecasts
      IMPLICIT NONE

      INTEGER :: I, Lk, U1, U2, SSeq, ESeq
      INTEGER :: Dy, Mn, Yr, ScenCount
      LOGICAL :: OK
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=50)  :: SN
      CHARACTER(LEN=200) :: LakeDir, OtlkDir, FName
      CHARACTER(LEN=200) :: CfgFile, ParmFile, MetFile, MissFile, OutFile
      CHARACTER(LEN=200) :: MetGLSHFS, MetLLTM
      TYPE(THeaderInfoType) :: Hdr

      !
      !  Create/initialize the Hdr object used when reading the overlake 
      !  meteorology "subbasin" file.
      !
      Hdr = THeaderInfoType()
 
      !
      !  Verify the existence of the desired output directory as specified
      !  in the GLSHFS_Config%ForecastName variable.
      !
      OtlkDir = TRIM(GLSHFS_Config%BaseDir) // TRIM(GLSHFS_Config%ForecastName) // FilePathSeparator
      FName = TRIM(OtlkDir) // '.'
      INQUIRE(FILE=FName, EXIST=OK)
      IF (.NOT. OK) THEN
         
      END IF
 
      !
      !  If ForecastStartDate = 9999-12-31 (MissingData_Date) then the user did 
      !  not specify a valid value.  This is ok if we are just processing station 
      !  data or updating for the historical data period, but it will cause an error
      !  if we try to do a forecast.
      !
      IF (GLSHFS_Config%ForecastStartSeq .EQ. MissingData_Date) THEN
         ErrorMessage = 'Unable to run forecasts due to missing forecast start date';  CALL PassMsg
         GOTO 898
      END IF

      !
      !  If ForecastStartDate = 8888-08-08 then the user wants the forecast to 
      !  start at the end of the historical data. We need to find the end of 
      !  the common data period from the historical data sets (subbasin 0).
      !  If the value was NOT 8888-08-08, then assume the user specified the
      !  exact desired forecast start date.
      !
      SSeq = MissingData_Date
      CALL SequenceDate(Dy, Mn, Yr, GLSHFS_Config%ForecastStartSeq); IF (ErrorLevel .NE. 0) GOTO 899
      IF (Yr .EQ. 8888) THEN
         SSeq = FcstStartAtEndOfData();         IF (ErrorLevel .NE. 0) GOTO 899
      ELSE
         SSeq = GLSHFS_Config%ForecastStartSeq
      END IF
      
      !
      !  Verify that the requested start date will work for all lakes.
      !
      DO I = 1, 7
         Bsn = LakeName3(I)                                                         ! LakeName3 defined in glshfs_global
         LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator          ! the lake directory

         !
         !  Read the overlake meteorology data file to get period of record
         !
         WRITE(FName, 1502) TRIM(LakeDir), Bsn
         U1 = GetFreeUnitNumber()
         OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', ERR=811)
         CALL FileWasOpened(U1)
         CALL ReadHeaderInfo(FName, U1, HFT_SingleSubbasin, Hdr); IF (ErrorLevel .NE. 0) GOTO 899
         CLOSE(U1)
         CALL FileWasClosed(U1)
         IF ((SSeq .LT. Hdr%SDate) .OR. (SSeq .GT. Hdr%EDate+1)) THEN
            WRITE(ErrorMessage, 5010) Bsn;                                                             CALL PassMsg
            WRITE(ErrorMessage, 5011) SeqToDateStringYMD(Hdr%SDate), SeqToDateStringYMD(Hdr%EDate);    CALL PassMsg
            ErrorMessage = 'Specified start date is '//SeqToDateStringYMD(SSeq);                       CALL PassMsg
            ErrorMessage = 'Forecasts may begin any day from start date to one day after end date.';   CALL PassMsg
            GOTO 898
         END IF
      END DO

      !
      !  Determine end date by adding the right number of months.  Remember to
      !  adjust 1 more month into the future if start day is not 1. We always
      !  do FULL months.
      !  Remember that a 1-month forecast starting on day 1 of the month equals
      !  an ending date at the end of the start month. That's why the line
      !  "Mn = Mn + GLSHFS_Config%ForecastLen - 1" has the "- 1" at the end.
      !
      CALL SequenceDate(Dy, Mn, Yr, SSeq);   IF (ErrorLevel .NE. 0) GOTO 899
      Mn = Mn + GLSHFS_Config%ForecastLen - 1
      IF (Dy .GT. 1) Mn = Mn + 1
      DO WHILE (Mn .GT. 12)
         Yr = Yr + 1
         Mn = Mn - 12
      END DO
      Dy = DaysInMonth(Mn, Yr)
      CALL DateSequence(Dy, Mn, Yr, ESeq);   IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  For each lake basin...
      !
      DO Lk = 1, 7
         Bsn = LakeName3(Lk)                                                        ! LakeName3 defined in glshfs_global
         WRITE(ErrorMessage, 5001) Bsn;   CALL PassMsg
         LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator                                 ! the lake directory
         OtlkDir = TRIM(GLSHFS_Config%BaseDir) // TRIM(GLSHFS_Config%ForecastName) // FilePathSeparator    ! where the otlkdata files are found

         !
         !  Create the parameter file that will be used for all ensemble members
         !
         WRITE(ParmFile, 1701) TRIM(OtlkDir), Bsn
         OK = BuildParameterFile_LLTM(ParmFile, Bsn, SSeq, ESeq);  IF (.NOT. OK) GOTO 899
         
         !
         !  For each ensemble member...
         !
         ScenCount = UBOUND(GLSHFS_Config%ScenarioNames, 1)
         DO I = 1, ScenCount
            SN = TRIM(GLSHFS_Config%ScenarioNames(I))

            !
            !  Convert met file from the GLSHFS format into the native LLTM format.
            !
            WRITE(MetGLSHFS, 1702) TRIM(OtlkDir), Bsn, TRIM(SN)
            WRITE(MetLLTM,   1710) TRIM(OtlkDir), Bsn, TRIM(SN)
            CALL ConvertMet_GLSHFS_to_LLTM(MetGLSHFS, MetLLTM);  IF (ErrorLevel .NE. 0) GOTO 899
            
            !
            !  Construct LLTM file names.
            !
            !  The met file is the otlkdata_xxx00_????.csv file in the otlk directory.
            !  The missing data file is the historical subbasin data file in the lake directory (lltm_meteorology_xxx.csv)
            !  We do not want to save a heat layer file.  That may change in the future, but
            !  we certainly do not want to merge it with the historical file.
            !
            CfgFile = TRIM(OtlkDir) // 'lltm.cfg'
            MetFile = TRIM(MetLLTM)
            WRITE(MissFile, 1703) TRIM(LakeDir), Bsn
            WRITE(OutFile,  1704) TRIM(OtlkDir), Bsn, TRIM(SN)

            !
            !  Write the LLTM config file
            !
            U2 = GetFreeUnitNumber()
            OPEN(UNIT=U2, FILE=TRIM(CfgFile), STATUS='REPLACE')
            CALL FileWasOpened(U2)
            WRITE(U2, 1801) 'GLSHFS LLTM run (forecast)'
            WRITE(U2, 1802) TRIM(ParmFile)
            WRITE(U2, 1803) TRIM(MetFile)
            WRITE(U2, 1804) TRIM(MissFile)
            WRITE(U2, 1805) TRIM(OutFile)
            WRITE(U2, 1806)
            WRITE(U2, 1807) TRIM(LogicalToText(GLSHFS_Config%LltmApplyDataCorrectionsFcst))
            WRITE(U2, 1808) GLSHFS_Config%LltmMethodRadiationFcst
            CLOSE(U2)
            CALL FileWasClosed(U2)
            
            !
            !  Run the LLTM for the identified forecast
            !
            CALL DO_LLTM(TRIM(CfgFile));  IF (ErrorLevel .NE. 0) GOTO 899
         END DO
      END DO

      RETURN
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file ' //TRIM(FName);  CALL PassMsg; GOTO 898
  
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] RunLLTM_Forecasts...'; CALL PassMsg
      RETURN


! 1501 FORMAT(A, A3, A1)
 1502 FORMAT(A, 'subdata_', A3, '00.csv')

 1701 FORMAT(A, 'lltm_parm_', A3, '.csv')
 1702 FORMAT(A, 'otlkdata_',  A3, '00_', A, '.csv') 
 1703 FORMAT(A, 'lltm_meteorology_', A3, '.csv') 
 1704 FORMAT(A, 'lltm_out_',  A3, '_', A, '.csv') 
 1710 FORMAT(A, 'lltm_met_', A3, '00_', A, '.csv') 
 
 
 1801 FORMAT('ID       = ', A)
 1802 FORMAT('PARMFILE = ', A)
 1803 FORMAT('METFILE  = ', A)
 1804 FORMAT('MISSFILE = ', A)
 1805 FORMAT('OUTFILE  = ', A)
 1806 FORMAT('LAYERFILE  = ')
 1807 FORMAT('ApplyOverwaterCorrection = ', A)
 1808 FORMAT('RadiationMethod = ', I0)
         
 5001 FORMAT('Building the LLTM outlook ensemble for lake ', A3)
 5010 FORMAT('Specified forecast start date invalid for subbasin ', A3, '00')
 5011 FORMAT('Data file period is ', A10, ' to ', A10)

      END SUBROUTINE RunLLTM_Forecasts

      !------------------------------------------------------------------
      !  Given the desired start/end dates:
      !    1) Read the master parameter file for GLSHFS.
      !    2) Read the heat layer info for the day before start date.
      !       (These are the heat layer conditions at the start of the run.)
      !    3) Output a new parameter file with the correct information
      !
      !  Note: MaxNoOfLayers is a global variable specifying the maximum 
      !        number of heat layers allowed.  It is defined in module LLTM.
      !------------------------------------------------------------------
      FUNCTION BuildParameterFile_LLTM(PFName, Bsn, SSeq, ESeq)    Result(Success)
      IMPLICIT NONE
      CHARACTER(LEN=*),  INTENT(IN) :: PFName, Bsn
      INTEGER,           INTENT(IN) :: SSeq, ESeq
      LOGICAL :: Success
      
      INTEGER :: I, J, U1, U2, U3, IOS, NumStr
      INTEGER :: Dy, Mn, Yr, LayerIndex, TSeq
      INTEGER :: SSeqFile, ESeqFile
      LOGICAL :: FExist
      REAL    :: Parms(10)
      CHARACTER(LEN=3)   :: B1, B2
      CHARACTER(LEN=25)  :: CsvStrings(MaxNoOfLayers*3 + 10)
      CHARACTER(LEN=100) :: BsnDir
      CHARACTER(LEN=150) :: File1, File2, File3, Line
      
      TYPE(TLayerInfo) :: LayerData
      
      !
      !  Open the master parameter file for this lake.
      !  This file should always exist and never be changed unless the LLTM is recalibrated.
      !  It contains the valid parameters plus "dummy" values for everything else.
      !  We will only read through the 10 parameter values that we need.  The rest of
      !  the file will be skipped.
      !
      U1 = GetFreeUnitNumber()
      B1 = GetLowercase(Bsn)
      BsnDir = TRIM(GLSHFS_Config%BaseDir) // B1 // FilePathSeparator
      File1 = TRIM(BsnDir) // 'lltm_parameters_glshfs_'//B1//'.csv'
      OPEN(UNIT=U1, FILE=TRIM(File1), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)
      
      READ(U1, *, ERR=812)       ! skip the 
      READ(U1, *, ERR=812)       ! three lines
      READ(U1, *, ERR=812)       ! of arbitrary header
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
      B2 = GetLowercase(CsvStrings(1))
      IF (B1 .NE. B2) THEN
         WRITE(ErrorMessage, 5001) B1, B2, TRIM(File1);   CALL PassMsg
         GOTO 898
      END IF
      DO I = 1, 10
         READ(U1, 1001, ERR=812) Line
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
         READ(CsvStrings(1), *, IOSTAT=IOS) Parms(I)
         IF (IOS .NE. 0) THEN
            WRITE(ErrorMessage, 5002) I;    CALL PassMsg
            GOTO 898
         END IF
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      U1 = -1
      
      !
      !  Now attempt to read the appropriate data from the glshfs file that stores
      !  heat layer info for each day.  
      !
      !  Once we have read the file, find the index of the heat layer that we will use.
      !  We can accept one of 2 options:
      !   1. The preferred option is the day prior to the forecast start.
      !      e.g. If the forecast start date is 1950-01-01, then we want
      !      the end-of-day heat layer data from 1949-12-31.
      !   2. If the data in option 1 is not available, check to see if
      !      we have data for the actual forecast start date. 
      !      e.g. If the forecast start date is 1950-01-01, but we were not
      !      able to get data from 1949-12-31, can we get it for 1950-01-01?
      !      If so, we will consider that to be "close enough". In practice,
      !      this allows us to re-run simulations from the file start date.
      !
      LayerIndex = -1
      File2 = TRIM(BsnDir) // 'lltm_heatlayers_'//Bsn//'.csv'

      INQUIRE(FILE=TRIM(File2), EXIST=FExist)
      IF (FExist) THEN
         !
         !  Read just the file start/end dates
         !
         U2 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
         OPEN(UNIT=U2, FILE=TRIM(File2), STATUS='OLD', ERR=821)
         CALL FileWasOpened(U2); IF (ErrorLevel .NE. 0) GOTO 899
         READ(U2, *)
         READ(U2, *)
         READ(U2, *)
         READ(U2, 1001, ERR=812) Line
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr); IF (NumStr .LT. 1) GOTO 852
         SSeqFile = DateStringYMDToSeq(CsvStrings(1)); IF (ErrorLevel .NE. 0) GOTO 899
         READ(U2, 1001, ERR=812) Line
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr); IF (NumStr .LT. 1) GOTO 853
         ESeqFile = DateStringYMDToSeq(CsvStrings(1)); IF (ErrorLevel .NE. 0) GOTO 899
         CLOSE(U2)
         CALL FileWasClosed(U2)

         TSeq = SSeq - 1                         ! day prior to forecast start; the Target date
         IF (TSeq .LT. SSeqFile) TSeq = SSeq     ! alternate target date
         IF ((TSeq .GE. SSeqFile) .AND. (TSeq .LE. ESeqFile)) THEN
            CALL ReadHeatLayerFile_LLTM(TRIM(File2), LayerData, TSeq);  IF (ErrorLevel .NE. 0) GOTO 899
            LayerIndex = TSeq - LayerData%SSeq + 1
         END IF
      END IF

      !
      !  If we were unable to find initial heat layer data from the file, either
      !  because the file did not exist or it did not contain the day we needed, then
      !  we will be using some very arbitrary heat layer values that probably 
      !  make very little sense, but allow the LLTM to run.
      !
      IF (LayerIndex .LT. 1) THEN
         CALL SequenceDate(Dy, Mn, Yr, SSeq-1);  IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(ErrorMessage, 5003) Yr, MonCode3(Mn), Dy;   CALL PassMsg
         WRITE(ErrorMessage, 5004) ;                       CALL PassMsg
         LayerData%Bsn  = Bsn
         LayerData%SSeq = SSeq-1
         LayerData%ESeq = SSeq-1
         LayerData%NumDays = 1
         ALLOCATE(LayerData%Dly(1), STAT=IOS)
         IF (IOS .NE. 0) THEN
            ErrorMessage = 'Error allocating memory for a single day of initializing layer data';  CALL PassMsg; GOTO 898
         END IF
         LayerData%Dly(1)%Seq = SSeq -1
         LayerData%Dly(1)%SurfTemp = DefaultWaterSurfTemps(Mn)
         LayerData%Dly(1)%IceArea  = 0.01        ! fraction of the lake 0.0 to 1.0
         LayerData%Dly(1)%IceDepth = 0.01        ! 1 cm
         LayerData%Dly(1)%IceTemp  = -1.0
         LayerData%Dly(1)%AgeOfCurrentLayer = 1.0
         LayerData%Dly(1)%NumLayers = 1
         LayerData%Dly(1)%AgeOfLayer(:)        = 1.0
         LayerData%Dly(1)%SurfTempFromLayer(:) = 1.0
         LayerData%Dly(1)%TotalHeat(:)         = 1.0
         LayerIndex = 1
      END IF
       
      !
      !  Now we can write the new parameter file
      !
      File3 = TRIM(PFName)
      U3 = GetFreeUnitNumber()
      OPEN(UNIT=U3, FILE=TRIM(File3), STATUS='REPLACE', ERR=831)
      CALL FileWasOpened(U3)

      !
      !  Header/Comment lines
      !
      WRITE(U3, 3001, ERR=833) 
      WRITE(U3, 3002, ERR=833) 
      WRITE(U3, 3003, ERR=833) SeqToDateStringYMD(SSeq)

      !
      !  Lake name/code (SUP, MIC, HUR, etc)
      !
      WRITE(U3, 3004, ERR=833) Bsn
         
      !
      !  Parameters
      !
      WRITE(U3, 3011, ERR=833) Parms(1)
      WRITE(U3, 3012, ERR=833) Parms(2)
      WRITE(U3, 3013, ERR=833) Parms(3)
      WRITE(U3, 3014, ERR=833) Parms(4)
      WRITE(U3, 3015, ERR=833) Parms(5)
      WRITE(U3, 3016, ERR=833) Parms(6)
      WRITE(U3, 3017, ERR=833) Parms(7)
      WRITE(U3, 3018, ERR=833) Parms(8)
      WRITE(U3, 3019, ERR=833) Parms(9)
      WRITE(U3, 3020, ERR=833) Parms(10)
      WRITE(U3, 3021, ERR=833) MissingData_Real, 11
      WRITE(U3, 3021, ERR=833) MissingData_Real, 12
      WRITE(U3, 3021, ERR=833) MissingData_Real, 13

      !
      !  Dates
      !
      WRITE(U3, 3031, ERR=833) SeqToDateStringYMD(SSeq)
      WRITE(U3, 3032, ERR=833) SeqToDateStringYMD(ESeq)
      
      !
      !  State variables
      !
      I = LayerIndex
      WRITE(U3, 3041, ERR=833) LayerData%Dly(I)%SurfTemp             !  deg C
      WRITE(U3, 3042, ERR=833) -9.999E29
      WRITE(U3, 3043, ERR=833) LayerData%Dly(I)%IceArea              !  fraction (0.0 - 1.0)
      WRITE(U3, 3044, ERR=833) LayerData%Dly(I)%IceDepth             !  meters
      WRITE(U3, 3045, ERR=833) LayerData%Dly(I)%IceTemp              !  deg C
      WRITE(U3, 3046, ERR=833) -9.999E29

      !
      !  Layer data
      !
      WRITE(U3, 3047, ERR=833) LayerData%Dly(I)%AgeOfCurrentLayer
      WRITE(U3, 3050, ERR=833) LayerData%Dly(I)%NumLayers
      WRITE(U3, 3051, ERR=833)
      DO J = 1, LayerData%Dly(I)%NumLayers
         WRITE(U3, 3052, ERR=833) J, LayerData%Dly(I)%AgeOfLayer(J),          &
                                     LayerData%Dly(I)%SurfTempFromLayer(J),   &
                                     LayerData%Dly(I)%TotalHeat(J)
      END DO
      CLOSE (U3)
      CALL FileWasClosed(U3); IF (ErrorLevel .NE. 0) GOTO 899
      U3 = -1

      Success = .TRUE.
      RETURN

      !
      !  Error handling
      !  
  811 ErrorMessage = 'Error opening file '      // TRIM(File1);   CALL PassMsg;  GOTO 898
  812 ErrorMessage = 'Error reading from file ' // TRIM(File1);   CALL PassMsg;  GOTO 898
  821 ErrorMessage = 'Error opening file '      // TRIM(File2);   CALL PassMsg;  GOTO 898
!  822 ErrorMessage = 'Error reading from file ' // TRIM(File2);   CALL PassMsg;  GOTO 898
  831 ErrorMessage = 'Error opening file '      // TRIM(File3);   CALL PassMsg;  GOTO 898
  833 ErrorMessage = 'Error writing to file '   // TRIM(File3);   CALL PassMsg;  GOTO 898

  852 ErrorMessage = 'Error parsing start date from ' // TRIM(File2); CALL PassMsg; GOTO 898
  853 ErrorMessage = 'Error parsing end date from '   // TRIM(File2); CALL PassMsg; GOTO 898
  
  898 Errorlevel = 1
  899 ErrorMessage = '[traceback] BuildParameterFile_LLTM...'; CALL PassMsg
      IF (FileIsOpen(U1)) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
      IF (FileIsOpen(U3)) THEN
         CLOSE(U3)
         CALL FileWasClosed(U3)
      END IF
      Success = .FALSE.
      RETURN
      
 1001 FORMAT(A)      

 3001 FORMAT('Parameter file for Large Lake Thermodynamics Model')
 3002 FORMAT('Created on-the-fly by the GLSHFS package.')
 3003 FORMAT('Used for a run starting on ', A10)
 3004 FORMAT(A3, ', 3-character lake code (valid values are SUP MIC HUR GEO STC ERI ONT HGB)')
 3011 FORMAT(E12.5E2, ',   Ice-model-related parameter, "tau sub a"')
 3012 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "a prime"')
 3013 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "b prime"')
 3014 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "F prime"')
 3015 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "a"')
 3016 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "b"')
 3017 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "F"')
 3018 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "V sub e"')
 3019 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "p"')
 3020 FORMAT(E12.5E2, ',   Ice-model-related parameter, "tau sub w"')
 3021 FORMAT(E12.5E2, ',   Unused parameters value ', I0)
 
 3031 FORMAT(2X, A10, ',   Start Date (YYYY-MM-DD)')
 3032 FORMAT(2X, A10, ',   End   Date (YYYY-MM-DD)')
 
 3041 FORMAT(E12.5E2, ',   Surface Temperature at start of run (deg C)')
 3042 FORMAT(E12.5E2, ',   unused')
 3043 FORMAT(E12.5E2, ',   Ice Area (fraction from 0.0 to 1.0)')
 3044 FORMAT(E12.5E2, ',   Ice Depth (meters)')
 3045 FORMAT(E12.5E2, ',   Ice Temperature (deg C)')
 3046 FORMAT(E12.5E2, ',   unused')
 3047 FORMAT(E12.5E2, ',   Age of Current Layer')
 
 3050 FORMAT(I0,  ',   Number of wind/heat layers')
 3051 FORMAT('Layer, AgeOfLayer, SurfTempFromLayer, TotalHeat')
 3052 FORMAT(I5, 3(',', E12.5E2))
 
 5001 FORMAT('Mismatched basin names: '//B1//' vs '//B2//' in ', A)
 5002 FORMAT('Error parsing parameter entry ', I0, ' in master parameter file.')
 5003 FORMAT('Unable to get heat layer data for ', I4.4, '-', A3, '-', I2.2)
 5004 FORMAT('Using arbitrary (very poor) initial heat layer values.')
 
      END FUNCTION BuildParameterFile_LLTM
      

      !------------------------------------------------------------------
      !  Reads a meteorology data file in GLSHFS single-subbasin format and 
      !  outputs a met data file in the LLTM native format.
      !------------------------------------------------------------------
      SUBROUTINE ConvertMet_GLSHFS_to_LLTM(FileGLSHFS, FileLLTM)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: FileGLSHFS, FileLLTM

      INTEGER :: U1, I, DT, IOS, Seq, SSeq, ESeq, NumDays
      REAL    :: Cnv
      CHARACTER(LEN=10)  :: DateYMD, ValStr(6)
      
      REAL, DIMENSION(:),   ALLOCATABLE :: OData
      REAL, DIMENSION(:,:), ALLOCATABLE :: MetValues      ! (6, NumDays)
      
      TYPE (TDlyDataForSubbasin)  :: SubData
      TYPE (TDlyData), POINTER    :: TDDP

      INTEGER, DIMENSION(6), PARAMETER :: ReqType =                 &
         (/ GDT_AirTempMean, GDT_DewPointMean, GDT_WindSpeed,       &
            GDT_CloudCover,  GDT_IncidentRad,  GDT_NetLongWaveRad /)
      
      INTEGER, DIMENSION(6), PARAMETER :: ReqUnit =                  &
         (/ GDU_Celsius,  GDU_Celsius,     GDU_MetersPerSecond,      &
            GDU_Percent,  GDU_WattsPerM2,  GDU_WattsPerM2 /)
      
      !
      !
      U1 = -1
      SubData = TDlyDataForSubbasin()
      
      !
      !  Use the standard GLSHFS routine to read the input file.
      !
      CALL ReadFile_OneSubbasin(FileGLSHFS, SubData); IF (ErrorLevel .NE. 0) GOTO 899
      IF (SubData%NumDays .EQ. 0) THEN
         WRITE(ErrorMessage, 5001) TRIM(FileGLSHFS);   CALL PassMsg
         GOTO 898
      END IF

      SSeq = SubData%GetStartDate()
      ESeq = SubData%GetEndDate()
      IF ((SSeq .EQ. MissingData_Date) .OR. (ESeq .EQ. MissingData_Date)) THEN
         WRITE(ErrorMessage, 5001) TRIM(FileGLSHFS);   CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Transfer the data from SubData into a temporary 2D array, doing any
      !  necessary data unit conversion as we go.
      !
      NumDays = ESeq - SSeq + 1
      ALLOCATE(MetValues(6, NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5002) NumDays;   CALL PassMsg
         GOTO 898
      END IF
      MetValues(:,:) = MissingData_Real
      
      DO DT = 1, 6
         TDDP => SubData%GetPointerToDataOfType(ReqType(DT))
         IF (ASSOCIATED(TDDP)) THEN
            OData = TDDP%GetDataVals()
            Cnv = UnitConvertedDataValue(1.0, TDDP%GetDataUnit(), ReqUnit(DT))
            IF (ErrorLevel .NE. 0) GOTO 899
            DO Seq = SSeq, ESeq
               I = Seq - SSeq + 1
               MetValues(DT, I) = OData(I) * Cnv
            END DO
         END IF
         IF (ALLOCATED(OData)) DEALLOCATE(OData, STAT=IOS)
      END DO

      !
      !  Write output file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FileLLTM), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)

      WRITE(U1, 1001, ERR=812)
      WRITE(U1, 1002, ERR=812)
      WRITE(U1, 1003, ERR=812)
      
      DO Seq = SSeq, ESeq
         I = Seq - SSeq + 1
         DO DT = 1, 6
            IF (MetValues(DT,I) .LE. MissingData_Real_Test) THEN
               ValStr(DT) = '        NA'
            ELSE
               WRITE(ValStr(DT), 1050) MetValues(DT,I)
            END IF
         END DO
         DateYMD = SeqToDateStringYMD(Seq)
         WRITE(U1, 1010, ERR=812) DateYMD, (ValStr(DT), DT=1,6)
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      U1 = -1
      
      GOTO 999
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening '    // TRIM(FileLLTM);   CALL PassMsg;  GOTO 898
  812 ErrorMessage = 'Error writing to ' // TRIM(FileLLTM);   CALL PassMsg;  GOTO 898
  
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ConvertMet_GLSHFS_to_LLTM...';  CALL PassMsg
     
  999 IF (ALLOCATED(OData))     DEALLOCATE(OData,     STAT=IOS)
      IF (ALLOCATED(MetValues)) DEALLOCATE(MetValues, STAT=IOS)
      IF (U1 .GT. 0) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF
      RETURN    

      !
      !  FORMATs
      !
 1001 FORMAT('Meteorological forcing data for the Large Lake Thermodynamics Model.')
 1002 FORMAT('          ,   AirTemp,  DewPoint,   WindSpd,     Cloud,  Incident,  NetLongW')
 1003 FORMAT('YYYY-MM-DD,     deg C,     deg C,  meters/s,         %,  watts/m2,  watts/m2')
 1010 FORMAT(A10, 6(',', A10))
 1050 FORMAT(F10.3)
 
 5001 FORMAT(' No valid data read from ', A)
 5002 FORMAT(' Error allocating memory for ', I0, ' days of met data')
      
      END SUBROUTINE ConvertMet_GLSHFS_to_LLTM


      !------------------------------------------------------------------
      !  Reads an output file from the LLTM and store the data in a standard
      !  GLSHFS single-subbasin type (TDlyDataForSubbasin).
      !  The LLTM output file contains 2 sets of meteorology (airtemp, dewpt,
      !  wind, cloud). The first set is the values that were supplied as input
      !  to the LLTM.  We will be skipping those here, since they are available
      !  in the GLSHFS lakewide meteorology file. We will, however, process
      !  the "adjusted" values of those 4 variables, since they represent
      !  the actual values used to drive the thermodynamic calculations.
      !------------------------------------------------------------------
      SUBROUTINE ConvertOutput_LLTM_to_GLSHFS(FileLLTM, FileGLSHFS)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: FileGLSHFS, FileLLTM

      INTEGER :: U1, I, DT, IOS, Seq, SSeq, ESeq, NumDays
      INTEGER :: NumStr, LkNum, LineNum, SeqF
      LOGICAL :: OK
      REAL    :: RVal
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=30)  :: CsvStrings(25)
      CHARACTER(LEN=150) :: Line, S
      
      REAL, DIMENSION(:,:), ALLOCATABLE :: LLTM_Values     ! (16, NumDays)
      
      TYPE (TDlyDataForSubbasin) :: SubData
      TYPE (TDlyData)            :: TDD

      !
      !  Types and units for the output
      !
      INTEGER, DIMENSION(16), PARAMETER :: OutputType =                               &
         (/ GDT_AirTempMean, GDT_DewPointMean, GDT_WindSpeed,      GDT_CloudCover,    &
            GDT_Evaporation, GDT_WaterTemp,    GDT_IceTemp,        GDT_IceArea,       &
            GDT_IceDepth,    GDT_ReflectedRad, GDT_LatentRad,      GDT_SensibleRad,   &
            GDT_Advection,   GDT_IncidentRad,  GDT_NetLongWaveRad, GDT_TotalHeat/)
      
      INTEGER, DIMENSION(16), PARAMETER :: OutputUnit =                               &
         (/ GDU_Celsius,     GDU_Celsius,    GDU_MetersPerSecond, GDU_Percent,        &
            GDU_Millimeters, GDU_Celsius,    GDU_Celsius,         GDU_Percent,        &
            GDU_Millimeters, GDU_WattsPerM2, GDU_WattsPerM2,      GDU_WattsPerM2,     &
            GDU_WattsPerM2,  GDU_WattsPerM2, GDU_WattsPerM2,      GDU_Calories /)
      
      !
      !  The index in LLTM_Values corresponding to the data type/unit in the above arrays
      !  e.g. 17 = index into LLTM_Values() array for mean air temp
      !       18 = index into LLTM_Values() array for mean dewpt
      !        1 = index into LLTM_Values() array for daily total evap
      !
      INTEGER, DIMENSION(16), PARAMETER :: DtIndexInArray =                           &
         (/ 17, 18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 /)
      
      !
      !  Create the blank subbasin object
      !
      SubData = TDlyDataForSubbasin()
      TDD     = TDlyData()
      
      !
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FileLLTM), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)

      !
      !  
      READ(U1, 1001, ERR=812) Line
      S = TRIM(ADJUSTL(Line))
      S = TRIM(S(1:100))
      OK = SubData%SetDescription(TRIM(S))
      
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStr .LT. 1) GOTO 702
      S     = TRIM(CsvStrings(1))
      Bsn   = TRIM(S(1:3))
      LkNum = LakeNumberFromName3(Bsn)
      SubData%Bsn     = Bsn
      SubData%SubNum  = 0
      SubData%SubArea = CoordLakeArea(LkNum)

      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
      SSeq = DateStringYMDToSeq(CsvStrings(1))
      IF (SSeq .EQ. MissingData_Date) GOTO 703
      
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
      ESeq = DateStringYMDToSeq(CsvStrings(1))
      IF (ESeq .EQ. MissingData_Date) GOTO 704

      NumDays = ESeq - SSeq + 1
      ALLOCATE(LLTM_Values(16, NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5001) NumDays, TRIM(FileLLTM);   CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Skip the 2 column header lines.
      !  I am assuming that the types and units are the ones I expect, and I
      !  do not do a verify step here. Laziness, perhaps, but it also helps 
      !  slightly in the performance department.  They OUGHT to be a match
      !  to what I am expecting.
      !
      READ(U1, *, ERR=812)      ! skip this column header line (data types)
      READ(U1, *, ERR=812)      ! skip this column header line (data units)
      
      !
      LineNum = 7
      DO Seq = SSeq, ESeq
         READ(U1, 1001, ERR=812) Line
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumStr .NE. 21) GOTO 707
         
         SeqF = DateStringYMDToSeq(CsvStrings(1))
         IF (SeqF .NE. Seq) GOTO 721
         
         !
         !  Convert relevant items to numeric.
         !  We skip data items 13-16 -- the input met data.
         !  We store into 16 contiguous items, though.
         !
         DO I = 1, 12
            READ(CsvStrings(I+1), *, IOSTAT=IOS) RVal
            IF (IOS .NE. 0) RVal = MissingData_Real
            LLTM_Values(I, NumDays) = RVal
         END DO
         DO I = 17, 20
            READ(CsvStrings(I+1), *, IOSTAT=IOS) RVal
            IF (IOS .NE. 0) RVal = MissingData_Real
            LLTM_Values(I-4, NumDays) = RVal
         END DO
         LineNum = LineNum + 1
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      U1 = -1

      !
      !  Now build TDlyData objects for each data type and add to the big object
      !
      DO DT = 1, 16
         CALL TDD%Clear()
         OK = TDD%SetDataType(OutputType(DT));                IF (.NOT. OK) GOTO 899
         OK = TDD%SetDataUnit(OutputUnit(DT));                IF (.NOT. OK) GOTO 899
         I  = DtIndexInArray(DT)
         OK = TDD%AssignData(SSeq, ESeq, LLTM_Values(I,:));   IF (.NOT. OK) GOTO 899
         OK = SubData%AddDataset(TDD);                        IF (.NOT. OK) GOTO 899
      END DO
      CALL TDD%Clear()

      !
      !  And now write the new output file with the standard routine
      !
      CALL WriteFile_OneSubbasin(FileGLSHFS, SubData); IF (ErrorLevel .NE. 0) GOTO 899
      
      GOTO 999
      
      !
      !  Error handling
      !
  702 ErrorMessage = 'Too few entries on line 2 of ' // TRIM(FileLLTM); CALL PassMsg; GOTO 898
  703 ErrorMessage = 'Too few entires on line 3 of ' // TRIM(FileLLTM); CALL PassMsg; GOTO 898
  704 ErrorMessage = 'Too few entries on line 4 of ' // TRIM(FileLLTM); CALL PassMsg; GOTO 898
  707 WRITE(ErrorMessage, 5020) LineNum, TRIM(FileLLTM);                CALL PassMsg; GOTO 898  
  721 WRITE(ErrorMessage, 5021) LineNum, TRIM(FileLLTM);                CALL PassMsg; GOTO 898  
      
      
  811 ErrorMessage = 'Error opening ' // TRIM(FileLLTM);        CALL PassMsg;  GOTO 898
  812 ErrorMessage = 'Error reading from ' // TRIM(FileLLTM);   CALL PassMsg;  GOTO 898
  
  
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ConvertOutput_LLTM_to_GLSHFS...';  CALL PassMsg
     
  999 IF (ALLOCATED(LLTM_Values))  DEALLOCATE(LLTM_Values, STAT=IOS)
      IF (U1 .GT. 0) THEN
         CLOSE(U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF
      RETURN    

      !
      !  FORMATs
      !
 1001 FORMAT(A150)
      
      
 5001 FORMAT('Error allocating memory for reading ', I0, ' days of data from ', A)
 5020 FORMAT('Too few entries on line ', I0, ' of ', A)
 5021 FORMAT('Error parsing date information on line ', I0, ' of ', A)
      
      END SUBROUTINE ConvertOutput_LLTM_to_GLSHFS

!-----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !
      !
      !------------------------------------------------------------------------
      SUBROUTINE FillAnyMissingMetData(MFile, FillTheDataType)
      IMPLICIT NONE
      CHARACTER(LEN=*),      INTENT(IN) :: MFile
      LOGICAL, DIMENSION(6), INTENT(IN) :: FillTheDataType
      
      INTEGER :: I, J, U1, IOS, IOS2, DOY, NumStr
      INTEGER :: NumDays, Seq, SSeqFile, ESeqFile
      LOGICAL :: SkipIt , NeedToFill(6)
      REAL    :: DVal
      CHARACTER(LEN=10)  :: DateYMD, ValStr(6)
      CHARACTER(LEN=50)  :: CsvStrings(10)
      CHARACTER(LEN=200) :: Line

      INTEGER, DIMENSION(365,6) :: VCount
      REAL, DIMENSION(:,:), ALLOCATABLE :: MetData
      REAL, DIMENSION(365,6) :: LTMean, VTotal
      
      !
      !  Open the file for input
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(MFile), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)
      
      !
      !  Read once just to get the date extent and determine if 
      !  there is any missing data that needs to be filled.
      !  Invalid/bad/ugly lines where the date fails to parse correctly will just be ignored.
      !
      NeedToFill(:) = .FALSE.
      CALL DateSequence(31, 12, 2999, SSeqFile);  IF (ErrorLevel .NE. 0) GOTO 899
      CALL DateSequence( 1,  1,  999, ESeqFile);  IF (ErrorLevel .NE. 0) GOTO 899
      READ(U1, *, ERR=812)     ! skip 3 lines
      READ(U1, *, ERR=812)     ! of header
      READ(U1, *, ERR=812)
      READ(U1, 1000, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumStr .GE. 1) THEN
            Seq = DateStringYMDToSeq(CsvStrings(1))
            IF (Seq .NE. MissingData_Date) THEN
               IF (Seq .LT. SSeqFile) SSeqFile = Seq
               IF (Seq .GT. ESeqFile) ESeqFile = Seq
            END IF
            DO J = 1, 6
               IF (FillTheDataType(J)) THEN         ! If we want to fill missing data of this type...
                  IF (.NOT. NeedToFill(J)) THEN     ! If we have not already found any missing data of this type...
                     READ(CsvStrings(J+1), *, IOSTAT=IOS2) DVal
                     IF (DVal .LT. MissingData_Real_Test) NeedToFill(J) = .TRUE.    ! found missing data that needs to be filled
                  END IF
               END IF
            END DO
         END IF
         READ(U1, 1000, IOSTAT=IOS) Line
      END DO

      !
      !  If there is no missing data that needs to be filled, then there is 
      !  no reason to continue this routine. We can just return.
      !
      SkipIt = .TRUE.      
      DO J = 1, 6
         IF (NeedToFill(J)) SkipIt = .FALSE.
      END DO
      IF (SkipIt) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
         RETURN
      END IF
      
      !
      !  Allocate the storage array
      !
      NumDays = ESeqFile - SSeqFile + 1
      IF (NumDays .LT. 1) THEN
         ErrorMessage = 'No valid data lines (dates) found in ' // TRIM(MFile);  CALL PassMsg
         GOTO 898
      END IF
      ALLOCATE(MetData(NumDays, 6), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5001) NumDays;   CALL PassMsg
         GOTO 898
      END IF
      MetData(:,:) = MissingData_Real
      
      !
      !  Now read again, storing the data
      !
      REWIND(U1)
      READ(U1, *, ERR=812)     ! skip 3 lines
      READ(U1, *, ERR=812)     ! of header
      READ(U1, *, ERR=812)
      READ(U1, 1000, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumStr .GE. 7) THEN
            Seq = DateStringYMDToSeq(CsvStrings(1))
            IF (Seq .NE. MissingData_Date) THEN
               I = Seq - SSeqFile + 1
               DO J = 1, 6
                  IF (FillTheDataType(J)) THEN
                     READ(CsvStrings(J+1), *, IOSTAT=IOS2) DVal
                     IF (IOS2 .EQ. 0) MetData(I,J) = DVal
                  END IF
               END DO
            END IF
         END IF
         READ(U1, 1000, IOSTAT=IOS) Line
      END DO
      CLOSE(U1);   U1 = -1

      !
      !  Compute long-term mean values
      !
      LTMean(:,:) = MissingData_Real
      VTotal = 0.0
      VCount = 0
      DO Seq = SSeqFile, ESeqFile
         I   = Seq - SSeqFile + 1
         DOY = DayOfYear(Seq)
         IF (DOY .GT. 0) THEN
            IF (DOY .GE. 366) DOY = 365
            DO J = 1, 6
               IF (FillTheDataType(J)) THEN
                  DVal = MetData(I,J)
                  IF (DVal .GT. MissingData_Real_Test) THEN
                     VTotal(DOY,J) = VTotal(DOY,J) + DVal
                     VCount(DOY,J) = VCount(DOY,J) + 1
                  END IF
               END IF
            END DO
         END IF
      END DO
      DO DOY = 1, 365
         DO J = 1, 6
            IF (VCount(DOY,J) .GT. 0) LTMean(DOY,J) = VTotal(DOY,J) / VCount(DOY,J)
         END DO
      END DO
      
      !
      !  Fill in the missing data in the MetData array
      !
      DO J = 1, 6
         IF (FillTheDataType(J)) THEN
            DO Seq = SSeqFile, ESeqFile
               I = Seq - SSeqFile + 1
               DOY = DayOfYear(Seq)
               IF (MetData(I,J) .LT. MissingData_Real_Test) MetData(I,J) = LTMean(DOY,J)
            END DO
         END IF
      END DO
      
      !
      !  Overwrite the old file with the updated data
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(MFile), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)
      WRITE(U1, 1001, ERR=813)
      WRITE(U1, 1002, ERR=813)
      WRITE(U1, 1003, ERR=813)
      DO Seq = SSeqFile, ESeqFile
         I = Seq - SSeqFile + 1
         DO J = 1, 6
            IF (MetData(I,J) .LE. MissingData_Real_Test) THEN
               ValStr(J) = '        NA'
            ELSE
               WRITE(ValStr(J), 1050) MetData(I,J)
            END IF
         END DO
         DateYMD = SeqToDateStringYMD(Seq)
         WRITE(U1, 1010, ERR=813) DateYMD, (ValStr(J), J=1,6)
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      U1 = -1
      GOTO 999
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening '      // TRIM(MFile);   CALL PassMsg;  GOTO 898
  812 ErrorMessage = 'Error reading from ' // TRIM(MFile);   CALL PassMsg;  GOTO 898
  813 ErrorMessage = 'Error writing to '   // TRIM(MFile);   CALL PassMsg;  GOTO 898
  
  
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] FillAnyMissingMetData...';  CALL PassMsg
      IF (U1 .GT. 0) THEN
         CLOSE(U1)
         CALL FileWasClosed(U1)
      END IF
  999 IF (ALLOCATED(MetData)) DEALLOCATE(MetData, STAT=IOS)
      RETURN

      !
      ! FORMATs
      !
 1000 FORMAT(A200)
 1001 FORMAT('Meteorological forcing data for the Large Lake Thermodynamics Model.')
 1002 FORMAT('          ,   AirTemp,  DewPoint,   WindSpd,     Cloud,  Incident,  NetLongW')
 1003 FORMAT('YYYY-MM-DD,     deg C,     deg C,  meters/s,         %,  watts/m2,  watts/m2')
 1010 FORMAT(A10, 6(',', A10))
 1050 FORMAT(F10.3)

 5001 FORMAT('Error allocating memory for storage of data. NumDays = ', I0)

      END SUBROUTINE FillAnyMissingMetData
      
!-----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Merge two LLTM output files (CSV format).
      !
      !  Data in File2 will take precedence whenever there is overlap.
      !------------------------------------------------------------------------
      SUBROUTINE MergeOutputFiles_LLTM(File1, File2, MrgFile)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: File1, File2, MrgFile

      INTEGER :: U1, U2, U3, I, J, K, IOS, Seq, SeqF
      INTEGER :: LineNum, NumDays, NumStr
      INTEGER :: SSeq1, ESeq1, SSeq2, ESeq2, SSeq3, ESeq3
      LOGICAL :: Exist1, Exist2
      REAL    :: DVal
      CHARACTER (LEN=3)   :: Bsn1, Bsn2, Bsn3
      CHARACTER (LEN=11)  :: OutStr(20)
      CHARACTER (LEN=150) :: S, CsvStrings(25)
      CHARACTER (LEN=250) :: Line

      REAL, DIMENSION(:,:), ALLOCATABLE :: DataVals1, DataVals2, DataVals3     ! indexed(20, NumDays)

      U1 = -1;   U2 = -1;   U3 = -1
      
      !
      !  Read file1
      !
      INQUIRE(FILE=TRIM(File1), EXIST=Exist1)
      IF (Exist1) THEN
         U1 = GetFreeUnitNumber()
         OPEN(UNIT=U1, FILE=TRIM(File1), STATUS='OLD', ERR=811)
         CALL FileWasOpened(U1)
      
         READ(U1, *, ERR=812)       ! skip header line 1
         READ(U1, 1001, ERR=812) Line
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);   IF (ErrorLevel .NE. 0) GOTO 899
         S = TRIM(CsvStrings(1))
         Bsn1 = TRIM(S(1:3))
         
         READ(U1, 1001, ERR=812) Line
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);   IF (ErrorLevel .NE. 0) GOTO 899
         S = TRIM(CsvStrings(1))
         SSeq1 = DateStringYMDToSeq(S);  IF (SSeq1 .EQ. MissingData_Date) GOTO 711
         
         READ(U1, 1001, ERR=812) Line
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);   IF (ErrorLevel .NE. 0) GOTO 899
         S = TRIM(CsvStrings(1))
         ESeq1 = DateStringYMDToSeq(S);  IF (ESeq1 .EQ. MissingData_Date) GOTO 712
         
         NumDays = ESeq1 - SSeq1 + 1
         ALLOCATE(DataVals1(20, NumDays), STAT=IOS)
         IF (IOS .NE. 0) GOTO 713
         
         READ(U1, *, ERR=812)       ! skip column header line
         READ(U1, *, ERR=812)       ! skip column header line
         
         LineNum = 7
         DO Seq = SSeq1, ESeq1
            READ(U1, 1001, ERR=812) Line
            CALL ParseCommaSepLine(Line, CsvStrings, NumStr);   IF (ErrorLevel .NE. 0) GOTO 899
            IF (NumStr .LT. 21) GOTO 714
            S = TRIM(CsvStrings(1))
            SeqF = DateStringYMDToSeq(S);  IF (SeqF .NE. Seq) GOTO 715
            I = Seq - SSeq1 + 1
            DO J = 1, 20
               READ(CsvStrings(J+1), *, IOSTAT=IOS) DVal
               IF (IOS .NE. 0) DVal = MissingData_Real
               DataVals1(J,I) = DVal
            END DO
            LineNum = LineNum + 1
         END DO

         CLOSE(U1)
         CALL FileWasClosed(U1)
      ELSE
         SSeq1 = MissingData_Date
         ESeq1 = MissingData_Date
      END IF
      
      !
      !  Read file2
      !
      INQUIRE(FILE=TRIM(File2), EXIST=Exist2)
      IF (Exist2) THEN
         U2 = GetFreeUnitNumber()
         OPEN(UNIT=U2, FILE=TRIM(File2), STATUS='OLD', ERR=821)
         CALL FileWasOpened(U2)
         
         READ(U2, *, ERR=822)       ! skip header line 1
         READ(U2, 1001, ERR=822) Line
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);   IF (ErrorLevel .NE. 0) GOTO 899
         S = TRIM(CsvStrings(1))
         Bsn2 = TRIM(S(1:3))
         
         READ(U2, 1001, ERR=822) Line
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);   IF (ErrorLevel .NE. 0) GOTO 899
         S = TRIM(CsvStrings(1))
         SSeq2 = DateStringYMDToSeq(S);  IF (SSeq2 .EQ. MissingData_Date) GOTO 721
         
         READ(U2, 1001, ERR=822) Line
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);   IF (ErrorLevel .NE. 0) GOTO 899
         S = TRIM(CsvStrings(1))
         ESeq2 = DateStringYMDToSeq(S);  IF (ESeq2 .EQ. MissingData_Date) GOTO 722
         
         NumDays = ESeq2 - SSeq2 + 1
         ALLOCATE(DataVals2(20, NumDays), STAT=IOS);    IF (IOS .NE. 0) GOTO 723
         
         READ(U2, *, ERR=822)       ! skip column header line
         READ(U2, *, ERR=822)       ! skip column header line
         
         LineNum = 7
         DO Seq = SSeq2, ESeq2
            READ(U2, 1001, ERR=822) Line
            CALL ParseCommaSepLine(Line, CsvStrings, NumStr);   IF (ErrorLevel .NE. 0) GOTO 899
            IF (NumStr .LT. 21) GOTO 724
            S = TRIM(CsvStrings(1))
            SeqF = DateStringYMDToSeq(S);  IF (SeqF .NE. Seq) GOTO 725
            I = Seq - SSeq2 + 1
            DO J = 1, 20
               READ(CsvStrings(J+1), *, IOSTAT=IOS) DVal
               IF (IOS .NE. 0) DVal = MissingData_Real
               DataVals2(J,I) = DVal
            END DO
            LineNum = LineNum + 1
         END DO

         CLOSE(U2)
         CALL FileWasClosed(U2)
      ELSE
         SSeq2 = MissingData_Date
         ESeq2 = MissingData_Date
      END IF

      !
      !  Verify that we have SOME data
      !
      IF ((SSeq1 .EQ. MissingData_Date) .AND. (SSeq2 .EQ. MissingData_Date)) THEN
         ErrorMessage = 'Both of the files specified for merging are missing.';  CALL PassMsg
         ErrorMessage = 'File1 = ' // TRIM(File1);  CALL PassMsg
         ErrorMessage = 'File2 = ' // TRIM(File2);  CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Make sure there is no gap in the record
      !
      IF ((SSeq1 .NE. MissingData_Date) .AND. (SSeq2 .NE. MissingData_Date)) THEN
         IF ((SSeq1 .GT. ESeq2+1) .OR. (SSeq2 .GT. ESeq1+1)) THEN
            ErrorMessage = 'Merging two output files requires that they be contiguous or overlapping.';  CALL PassMsg
            ErrorMessage = 'These two files do not meet that requirement.';  CALL PassMsg
            ErrorMessage = 'File1 = ' // TRIM(File1);  CALL PassMsg
            ErrorMessage = 'File2 = ' // TRIM(File2);  CALL PassMsg
            GOTO 898
         END IF
      END IF
      
      !
      !  Merge the data
      !
      IF (SSeq1 .NE. MissingData_Date) Bsn3 = Bsn1
      IF (SSeq2 .NE. MissingData_Date) Bsn3 = Bsn2
      
      SSeq3 = MissingData_Date
      IF (SSeq1 .EQ. MissingData_Date) SSeq3 = SSeq2
      IF (SSeq2 .EQ. MissingData_Date) SSeq3 = SSeq1
      IF (SSeq3 .EQ. MissingData_Date) SSeq3 = MIN(SSeq1, SSeq2)

      ESeq3 = MissingData_Date
      IF (ESeq1 .EQ. MissingData_Date) ESeq3 = ESeq2
      IF (ESeq2 .EQ. MissingData_Date) ESeq3 = ESeq1
      IF (ESeq3 .EQ. MissingData_Date) ESeq3 = MAX(ESeq1, ESeq2)
      
      NumDays = ESeq3 - SSeq3 + 1
      ALLOCATE(DataVals3(20, NumDays), STAT=IOS);   IF (IOS .NE. 0) GOTO 733
      DataVals3(:,:) = MissingData_Real
      
      IF (SSeq1 .NE. MissingData_Date) THEN
         DO Seq = SSeq1, ESeq1
            I = Seq - SSeq1 + 1
            J = Seq - SSeq3 + 1
            DO K = 1, 20
               DataVals3(K,J) = DataVals1(K,I)
            END DO
         END DO
      END IF

      IF (SSeq2 .NE. MissingData_Date) THEN
         DO Seq = SSeq2, ESeq2
            I = Seq - SSeq2 + 1
            J = Seq - SSeq3 + 1
            DO K = 1, 20
               DataVals3(K,J) = DataVals2(K,I)
            END DO
         END DO
      END IF

      !
      !  Write the new output file
      !
      U3 = GetFreeUnitNumber()
      OPEN(UNIT=U3, FILE=TRIM(MrgFile), STATUS='REPLACE', ERR=831)
      CALL FileWasOpened(U3)
      WRITE(U3, 1301, ERR=833)
      WRITE(U3, 1302, ERR=833) Bsn3
      WRITE(U3, 1303, ERR=833) SeqToDateStringYMD(SSeq3)
      WRITE(U3, 1304, ERR=833) SeqToDateStringYMD(ESeq3)
      WRITE(U3, 1305, ERR=833)
      WRITE(U3, 1306, ERR=833)
      DO Seq = SSeq3, ESeq3
         J = Seq - SSeq3 + 1
         
         !
         ! The first 12 columns are model output and are never missing,
         ! so we can process them simply
         !
         DO K = 1, 11
            WRITE(OutStr(K), '(F9.3)') DataVals3(K,J)
         END DO
         WRITE(OutStr(12), '(E11.4E2)') DataVals3(12,J)
         
         !
         !  13-20 are the input meteorology, raw and adjusted.
         !  These might be missing.
         !
         DO K = 13, 20
            IF (DataVals3(K,J) .GT. -998.00) THEN
               WRITE(OutStr(K), '(F9.3)') DataVals3(K,J)
            ELSE
               OutStr(K) = '       NA'
            END IF
         END DO
         WRITE(U3, 1310, ERR=833) SeqToDateStringYMD(Seq), (TRIM(OutStr(K)), K=1,20)
      END DO
      CLOSE(U3)
      CALL FileWasClosed(U3)
      
      !
      !  Go to the final cleanup section
      !
      GOTO 999

      !
      !  Error handling
      !
  711 WRITE(ErrorMessage, 5011) TRIM(File1);           CALL PassMsg;  GOTO 898
  712 WRITE(ErrorMessage, 5012) TRIM(File1);           CALL PassMsg;  GOTO 898
  713 WRITE(ErrorMessage, 5013) TRIM(File1);           CALL PassMsg;  GOTO 898
  714 WRITE(ErrorMessage, 5014) NumDays, TRIM(File1);  CALL PassMsg;  GOTO 898
  715 WRITE(ErrorMessage, 5015) LineNum, TRIM(File1);  CALL PassMsg;  GOTO 898
!  716 WRITE(ErrorMessage, 5016) LineNum, TRIM(File1);  CALL PassMsg;  GOTO 898
      
  721 WRITE(ErrorMessage, 5011) TRIM(File2);           CALL PassMsg;  GOTO 898
  722 WRITE(ErrorMessage, 5012) TRIM(File2);           CALL PassMsg;  GOTO 898
  723 WRITE(ErrorMessage, 5013) TRIM(File2);           CALL PassMsg;  GOTO 898
  724 WRITE(ErrorMessage, 5014) NumDays, TRIM(File2);  CALL PassMsg;  GOTO 898
  725 WRITE(ErrorMessage, 5015) LineNum, TRIM(File2);  CALL PassMsg;  GOTO 898
!  726 WRITE(ErrorMessage, 5016) LineNum, TRIM(File2);  CALL PassMsg;  GOTO 898

  733 WRITE(ErrorMessage, 5020);   CALL PassMsg;  GOTO 898
      
      
  811 ErrorMessage = 'Error opening '      // TRIM(File1);   CALL PassMsg;  GOTO 898
  812 ErrorMessage = 'Error reading from ' // TRIM(File1);   CALL PassMsg;  GOTO 898
  
  821 ErrorMessage = 'Error opening '      // TRIM(File2);   CALL PassMsg;  GOTO 898
  822 ErrorMessage = 'Error reading from ' // TRIM(File2);   CALL PassMsg;  GOTO 898
      
  831 ErrorMessage = 'Error opening '    // TRIM(MrgFile);   CALL PassMsg;  GOTO 898
  833 ErrorMessage = 'Error writing to ' // TRIM(MrgFile);   CALL PassMsg;  GOTO 898
      
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] MergeOutputFiles_LLTM...';  CALL PassMsg
 
  999 DEALLOCATE(DataVals1, STAT=IOS)
      DEALLOCATE(DataVals2, STAT=IOS)
      DEALLOCATE(DataVals3, STAT=IOS)
      IF (U1 .GT. 0) THEN
         CLOSE(U1, IOSTAT=IOS);  CALL FileWasClosed(U1)
      END IF
      IF (U2 .GT. 0) THEN
         CLOSE(U2, IOSTAT=IOS);  CALL FileWasClosed(U2)
      END IF
      IF (U3 .GT. 0) THEN
         CLOSE(U3, IOSTAT=IOS);  CALL FileWasClosed(U3)
      END IF

      RETURN
 
      !
      !  FORMATs
      !      
 1001 FORMAT(A)
 
 1301 FORMAT('Output from the NOAA/GLERL Large Lake Evaporation Model.')
 1302 FORMAT(A3, ',  3-character lake code (valid values are SUP MIC HUR GEO STC ERI ONT HGB)')
 1303 FORMAT(A10, ',  Start date (Y-M-D)')
 1304 FORMAT(A10, ',  End   date (Y-M-D)')
 1305 FORMAT('YYYY-MM-DD,     Evap,  WtrTemp,  IceTemp,  IceArea, IceDepth, ReflctdR,',     &
                        '  LatentR, SensbleR, AdvectnR, IncidntR, NetLngWR,   StrdHeat,',   &
                        '  AirTemp,    DewPt,  WindSpd, CloudCvr, AdjATemp, AdjDewPt,',     &
                        '  AdjWSpd,   AdjCld')
 1306 FORMAT('YYYY-MM-DD,     (mm),      (C),      (C),      (%), (meters),   (w/m2),',     &
                        '   (w/m2),   (w/m2),   (w/m2),   (w/m2),   (w/m2), (calories),',   &
                        '      (C),      (C),    (m/s),      (%),      (C),      (C),',     &
                        '    (m/s),      (%)')
 1310 FORMAT(A10, 11(',',A9), ',', A11, 9(',',A9))

 5011 FORMAT('Invalid start date in file ', A)
 5012 FORMAT('Invalid end date in file ', A)
 5013 FORMAT('Error allocating memory for the data in ', A)
 5014 FORMAT('Too few entries on line ', I0, ' of file ', A)
 5015 FORMAT('Date mismatch on line ', I0, ' of file ', A)
! 5016 FORMAT('Error parsing line ', I0, ' of file ', A)
 5020 FORMAT('Error allocating memory for the data to go into the output file')
      
      END SUBROUTINE MergeOutputFiles_LLTM


!-----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Read the LLTM thermal structure file (CSV format).
      !
      !  If no sequence number is passed, then the returned LayerInfo will have
      !  the entire set of data from the file.
      !
      !  If the caller passes a single sequence number (date) then the LayerInfo
      !  structure that is returned will contain only that single day's data.
      !
      !  If the caller passes two sequence numbers (date) then the LayerInfo
      !  structure that is returned will contain the data for that range.
      !------------------------------------------------------------------------
      SUBROUTINE ReadHeatLayerFile_LLTM(LayerFile, LayerInfo, Seq1, Seq2)
      IMPLICIT   NONE
      CHARACTER (LEN=*),  INTENT(IN)    :: LayerFile
      TYPE (TLayerInfo),  INTENT(INOUT) :: LayerInfo
      INTEGER, OPTIONAL,  INTENT(IN)    :: Seq1, Seq2

      INTEGER :: U1, I, J, K, IOS, LkNum, LineNum
      INTEGER :: Seq, DS, NumStr, SSeqFile, ESeqFile
      LOGICAL :: FExist, Valid
      REAL    :: rCV
      CHARACTER(LEN=20), DIMENSION(MaxNoOfLayers*3 + 10) :: CsvStrings
      CHARACTER(LEN=23*(MaxNoOfLayers*3+10)) :: Line

      !
      !  Initialize the LayerInfo structure
      !
      LayerInfo%SSeq = MissingData_Date
      LayerInfo%ESeq = MissingData_Date
      LayerInfo%NumDays = 0
      DEALLOCATE(LayerInfo%Dly, STAT=I)

      !
      !  Check for existence of the file
      !  If it does not exist, we will NOT flag it as an error.
      !  We will just leave the structure in the blank initialized state.
      !
      INQUIRE(FILE=TRIM(LayerFile), EXIST=FExist)
      IF (.NOT. FExist) RETURN
      
      !
      !  Open the file
      !
      U1 = -1
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(LayerFile), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Header lines
      !
      LineNum = 1
      READ(U1, *)
      LineNum = LineNum + 1
      READ(U1, *)

      !
      !  Lake name/code (SUP, MIC, HUR, etc)
      !
      LineNum = LineNum + 1
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, NumStr)
      IF (NumStr .LT. 1) GOTO 851
      LayerInfo%Bsn = GetLowerCase(CsvStrings(1))
      LkNum = LakeNumberFromName3(LayerInfo%Bsn)
      IF (LkNum .LT. 1) THEN
         WRITE(ErrorMessage, 5003) LayerInfo%Bsn, TRIM(LayerFile);  CALL PassMsg
         GOTO 898
      END IF
         
      !
      !  Dates
      !
      LineNum = LineNum + 1
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, NumStr)
      IF (NumStr .LT. 1) GOTO 852
      SSeqFile = DateStringYMDToSeq(CsvStrings(1)); IF (ErrorLevel .NE. 0) GOTO 899
      IF (SSeqFile .EQ. MissingData_Date) THEN
         WRITE(ErrorMessage, 9052) TRIM(CsvStrings(1));    CALL PassMsg
         GOTO 898
      END IF
    
      LineNum = LineNum + 1
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, NumStr)
      IF (NumStr .LT. 1) GOTO 853
      ESeqFile = DateStringYMDToSeq(CsvStrings(1)); IF (ErrorLevel .NE. 0) GOTO 899
      IF (ESeqFile .EQ. MissingData_Date) THEN
         WRITE(ErrorMessage, 9053) TRIM(CsvStrings(1));    CALL PassMsg
         GOTO 898
      END IF

      !
      !  Set dates appropriately.
      !  Did the caller pass in date(s)?
      !
      IF (PRESENT(Seq1)) THEN
         LayerInfo%SSeq = Seq1
         IF (PRESENT(Seq2)) THEN
            LayerInfo%ESeq = Seq2
         ELSE
            LayerInfo%ESeq = Seq1         
         END IF
         IF (LayerInfo%SSeq .GT. LayerInfo%ESeq) THEN
            ErrorMessage = 'Requested end date is earlier than requested start date';  CALL PassMsg
            ErrorMessage = 'Req Start Date = ' // SeqToDateStringYMD(LayerInfo%SSeq);  CALL PassMsg
            ErrorMessage = 'Req End   Date = ' // SeqToDateStringYMD(LayerInfo%ESeq);  CALL PassMsg
            GOTO 898
         END IF
         IF (LayerInfo%SSeq .LT. SSeqFile) THEN
            WRITE(ErrorMessage, 5001) SeqToDateStringYMD(LayerInfo%SSeq), SeqToDateStringYMD(SSeqFile), TRIM(LayerFile)
            CALL PassMsg;  GOTO 898
         END IF
         IF (LayerInfo%ESeq .GT. ESeqFile) THEN
            WRITE(ErrorMessage, 5002) SeqToDateStringYMD(LayerInfo%ESeq), SeqToDateStringYMD(ESeqFile), TRIM(LayerFile)
            CALL PassMsg;  GOTO 898
         END IF
      ELSE
         LayerInfo%SSeq = SSeqFile
         LayerInfo%ESeq = ESeqFile         
      END IF
      LayerInfo%NumDays = LayerInfo%ESeq - LayerInfo%SSeq + 1
      
      !
      !  Allocate the array of one-day structures
      !
      ALLOCATE(LayerInfo%Dly(LayerInfo%NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 9054) LayerInfo%NumDays;    CALL PassMsg
         GOTO 898
      END IF

      !
      !  Skip early lines of data if we don't need them
      !
      DO Seq = SSeqFile, LayerInfo%SSeq-1
         LineNum = LineNum + 1
         READ(U1, 1001, ERR=812) Line
      END DO
      
      !
      !  Read each relevant day from the file
      !
      DO Seq = LayerInfo%SSeq, LayerInfo%ESeq
         LineNum = LineNum + 1
         READ(U1, 1001, ERR=812) Line
         I = Seq - LayerInfo%SSeq + 1
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr)
         IF (NumStr .LT. 10) GOTO 855
         Valid = .TRUE.
         DS = DateStringYMDToSeq(CsvStrings(1));
         IF (DS .EQ. Seq) THEN
            LayerInfo%Dly(I)%Seq = DS
            READ(CsvStrings(2), *, IOSTAT=IOS) LayerInfo%Dly(I)%SurfTemp; IF (IOS .NE. 0) Valid = .FALSE.
            READ(CsvStrings(3), *, IOSTAT=IOS) LayerInfo%Dly(I)%IceArea;  IF (IOS .NE. 0) Valid = .FALSE.
            READ(CsvStrings(4), *, IOSTAT=IOS) LayerInfo%Dly(I)%IceDepth; IF (IOS .NE. 0) Valid = .FALSE.
            READ(CsvStrings(5), *, IOSTAT=IOS) LayerInfo%Dly(I)%IceTemp;  IF (IOS .NE. 0) Valid = .FALSE.
            READ(CsvStrings(6), *, IOSTAT=IOS) LayerInfo%Dly(I)%AgeOfCurrentLayer; IF (IOS .NE. 0) Valid = .FALSE.
            READ(CsvStrings(7), *, IOSTAT=IOS) LayerInfo%Dly(I)%NumLayers; IF (IOS .NE. 0) Valid = .FALSE.
            IF (Valid) THEN
               rCV = CoordLakeVolume(LkNum) * 1000000. * 1.0 * 1.0 / 1.0e+20
               LayerInfo%Dly(I)%SurfTempFromLayer(0) = 3.98
               LayerInfo%Dly(I)%TotalHeat(0)         = 3.98 * rCV
               DO J = 1, LayerInfo%Dly(I)%NumLayers
                  K = 7 + J
                  READ(CsvStrings(K), *, IOSTAT=IOS) LayerInfo%Dly(I)%AgeOfLayer(J); IF (IOS .NE. 0) Valid = .FALSE.
               END DO
               DO J = 1, LayerInfo%Dly(I)%NumLayers
                  K = 7 + LayerInfo%Dly(I)%NumLayers + J
                  READ(CsvStrings(K), *, IOSTAT=IOS) LayerInfo%Dly(I)%SurfTempFromLayer(J); IF (IOS .NE. 0) Valid = .FALSE.
               END DO
               DO J = 1, LayerInfo%Dly(I)%NumLayers
                  K = 7 + (LayerInfo%Dly(I)%NumLayers * 2) + J
                  READ(CsvStrings(K), *, IOSTAT=IOS) LayerInfo%Dly(I)%TotalHeat(J); IF (IOS .NE. 0) Valid = .FALSE.
               END DO
            END IF
            IF (.NOT. Valid) THEN
               LayerInfo%Dly(I)%NumLayers = 0
               LayerInfo%Dly(I)%SurfTempFromLayer(:) = MissingData_Real
               LayerInfo%Dly(I)%TotalHeat(:)         = MissingData_Real
               LayerInfo%Dly(I)%AgeOfLayer(:)        = MissingData_Real
            END IF
         ELSE
            ErrorMessage = 'Date gap or other sequencing issue in ' // TRIM(LayerFile);    CALL PassMsg
            WRITE(ErrorMessage, 5005) LineNum;  CALL PassMsg
            GOTO 898
         END IF
      END DO

      CLOSE (U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      RETURN

      !
      !  Error handling
      !
 811  ErrorMessage = 'Error opening ' // TRIM(LayerFile);        CALL PassMsg;  GOTO 898
 812  ErrorMessage = 'Error reading from ' // TRIM(LayerFile);   CALL PassMsg;  GOTO 898

 
 851  ErrorMessage = 'Error parsing lake name from '  // TRIM(LayerFile);    CALL PassMsg; GOTO 898
 852  ErrorMessage = 'Error parsing start date from ' // TRIM(LayerFile);    CALL PassMsg; GOTO 898
 853  ErrorMessage = 'Error parsing end date from '   // TRIM(LayerFile);    CALL PassMsg; GOTO 898
 855  ErrorMessage = 'Error parsing layer info from ' // TRIM(LayerFile);    CALL PassMsg; GOTO 898

      
 898  ErrorLevel = 1
 899  ErrorMessage = '[traceback] ReadHeatLayerFile_LLTM...';  CALL PassMsg
      IF (U1 .GT. 0) THEN
         CLOSE (U1, IOSTAT=IOS)
         CALL FileWasClosed(U1)
      END IF
      RETURN

 1001 FORMAT(A)
 
 5001 FORMAT(' Requested date of ', A, ' prior to start date (', A, ') in ', A)
 5002 FORMAT(' Requested date of ', A, ' after end date (', A, ') in ', A)
 5003 FORMAT(' Invalid lake specifier [', A3, '] in the file ', A)
 5005 FORMAT(' Issue manifested at line ', I0)
 
 9052 FORMAT(' Error parsing start date from entry [', A, ']')
 9053 FORMAT(' Error parsing end date from entry [', A, ']')
 9054 FORMAT(' Error allocating memory for ', I0, ' days of layer info.')

      END SUBROUTINE ReadHeatLayerFile_LLTM


!-----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Merge two LLTM thermal structure files (CSV format).
      !
      !  Data in File2 will take precedence whenever there is overlap.
      !------------------------------------------------------------------------
      SUBROUTINE MergeLayerFiles_LLTM(File1, File2, MrgFile)
      IMPLICIT   NONE
      CHARACTER (LEN=*), INTENT(IN) :: File1, File2, MrgFile

      INTEGER :: I, J, K, IOS, Seq
      LOGICAL :: OK, FExist1, FExist2
      TYPE (TLayerInfo) :: Layers1, Layers2, LayersMrg
      
      !
      !  Read the 2 data files
      !  Remember that memory is allocated for both of these, and it needs to 
      !  be deallocated at the end of this routine.
      !
      INQUIRE(FILE=TRIM(File1), EXIST=FExist1)
      INQUIRE(FILE=TRIM(File2), EXIST=FExist2)

      !
      !  If one file exists, but not the other, then we can simply copy the
      !  good file as the merged file.  Then we can exit this subroutine.
      !
      IF (FExist1 .AND. (.NOT. FExist2)) THEN
         CALL CopyFile(TRIM(File1), TRIM(MrgFile));  IF (ErrorLevel .NE. 0) GOTO 899
         RETURN
      END IF
      IF (FExist2 .AND. (.NOT. FExist1)) THEN
         CALL CopyFile(TRIM(File2), TRIM(MrgFile));  IF (ErrorLevel .NE. 0) GOTO 899
         RETURN
      END IF

      IF ((.NOT. FExist1) .AND. (.NOT. FExist2)) THEN
         ErrorMessage = 'Both of the specified source files are missing.';  CALL PassMsg
         ErrorMessage = 'Unable to create merged file.';                    CALL PassMsg
         ErrorMessage = 'File1 = '//TRIM(File1);                            CALL PassMsg
         ErrorMessage = 'File2 = '//TRIM(File2);                            CALL PassMsg
         GOTO 898
      END IF 
      
      !
      !  Getting to here in the code means that both source files exist.  Read them.
      !  Remember that memory is allocated for both of these, and it needs to 
      !  be deallocated at the end of this routine.
      !
      CALL ReadHeatLayerFile_LLTM(File1, Layers1); IF (ErrorLevel .NE. 0) GOTO 899
      CALL ReadHeatLayerFile_LLTM(File2, Layers2); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Basin name in the merged file
      !
      LayersMrg%Bsn = 'xxx'
      IF (LEN_TRIM(Layers1%Bsn) .EQ. 3) LayersMrg%Bsn = Layers1%Bsn
      IF (LEN_TRIM(Layers2%Bsn) .EQ. 3) LayersMrg%Bsn = Layers2%Bsn
      
      !
      !  Verify validity of dates
      !
      OK = .TRUE.
      IF (Layers1%SSeq .EQ. MissingData_Date) OK = .FALSE.
      IF (Layers1%ESeq .EQ. MissingData_Date) OK = .FALSE.
      IF (.NOT. OK) THEN
         ErrorMessage = 'Missing date info after reading '//TRIM(File1);  CALL PassMsg
         GOTO 898
      END IF
      IF (Layers2%SSeq .EQ. MissingData_Date) OK = .FALSE.
      IF (Layers2%ESeq .EQ. MissingData_Date) OK = .FALSE.
      IF (.NOT. OK) THEN
         ErrorMessage = 'Missing date info after reading '//TRIM(File2);  CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Dates and memory allocation
      !
      LayersMrg%SSeq = MIN(Layers1%SSeq, Layers2%SSeq)
      LayersMrg%ESeq = MAX(Layers1%ESeq, Layers2%ESeq)
      LayersMrg%NumDays = LayersMrg%ESeq - LayersMrg%SSeq + 1
      ALLOCATE(LayersMrg%Dly(LayersMrg%NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5001) LayersMrg%NumDays;   CALL PassMsg
         GOTO 898
      END IF
      DO I = 1, LayersMrg%NumDays
         LayersMrg%Dly(I)%AgeOfLayer(:)        = MissingData_Real
         LayersMrg%Dly(I)%SurfTempFromLayer(:) = MissingData_Real
         LayersMrg%Dly(I)%TotalHeat(:)         = MissingData_Real
      END DO

      !
      !  Copy the layers from File1
      !
      DO Seq = Layers1%SSeq, Layers1%ESeq
         I = Seq - Layers1%SSeq + 1          ! index into Layers1 (source)
         J = Seq - LayersMrg%SSeq + 1        ! index into LayersMrg (dest)
         LayersMrg%Dly(J)%Seq               = Seq
         LayersMrg%Dly(J)%SurfTemp          = Layers1%Dly(I)%SurfTemp
         LayersMrg%Dly(J)%IceArea           = Layers1%Dly(I)%IceArea
         LayersMrg%Dly(J)%IceDepth          = Layers1%Dly(I)%IceDepth
         LayersMrg%Dly(J)%IceTemp           = Layers1%Dly(I)%IceTemp
         LayersMrg%Dly(J)%AgeOfCurrentLayer = Layers1%Dly(I)%AgeOfCurrentLayer
         LayersMrg%Dly(J)%NumLayers         = Layers1%Dly(I)%NumLayers
         K = LayersMrg%Dly(J)%NumLayers
         LayersMrg%Dly(J)%AgeOfLayer(1:K)        = Layers1%Dly(I)%AgeOfLayer(1:K)
         LayersMrg%Dly(J)%SurfTempFromLayer(1:K) = Layers1%Dly(I)%SurfTempFromLayer(1:K)
         LayersMrg%Dly(J)%TotalHeat(1:K)         = Layers1%Dly(I)%TotalHeat(1:K)
      END DO
      
      !
      !  Copy the layers from File2
      !
      DO Seq = Layers2%SSeq, Layers2%ESeq
         I = Seq - Layers2%SSeq + 1          ! index into Layers2 (source)
         J = Seq - LayersMrg%SSeq + 1        ! index into LayersMrg (dest)
         LayersMrg%Dly(J)%Seq               = Seq
         LayersMrg%Dly(J)%SurfTemp          = Layers2%Dly(I)%SurfTemp
         LayersMrg%Dly(J)%IceArea           = Layers2%Dly(I)%IceArea
         LayersMrg%Dly(J)%IceDepth          = Layers2%Dly(I)%IceDepth
         LayersMrg%Dly(J)%IceTemp           = Layers2%Dly(I)%IceTemp
         LayersMrg%Dly(J)%AgeOfCurrentLayer = Layers2%Dly(I)%AgeOfCurrentLayer
         LayersMrg%Dly(J)%NumLayers         = Layers2%Dly(I)%NumLayers
         K = LayersMrg%Dly(J)%NumLayers
         LayersMrg%Dly(J)%AgeOfLayer(1:K)        = Layers2%Dly(I)%AgeOfLayer(1:K)
         LayersMrg%Dly(J)%SurfTempFromLayer(1:K) = Layers2%Dly(I)%SurfTempFromLayer(1:K)
         LayersMrg%Dly(J)%TotalHeat(1:K)         = Layers2%Dly(I)%TotalHeat(1:K)
      END DO
      
      !
      !  Write the merged file
      !
      CALL WriteHeatLayerFile_LLTM(MrgFile, LayersMrg); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Go to the final cleanup section
      !
      GOTO 999

      !
      !  Error handling
      !
 898  ErrorLevel = 1
 899  ErrorMessage = '[traceback] MergeLayerFiles_LLTM...';  CALL PassMsg
 
 999  DEALLOCATE(Layers1%Dly,   STAT=IOS)
      DEALLOCATE(Layers2%Dly,   STAT=IOS)
      DEALLOCATE(LayersMrg%Dly, STAT=IOS)

      RETURN

 5001 FORMAT(' Error allocating memory for ', I0, ' days of layer info.')

      END SUBROUTINE MergeLayerFiles_LLTM


!-----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Determine what the forecast start date should be when the user specified
      !  that it is to be at the end of the historical data.
      !------------------------------------------------------------------------
      FUNCTION FcstStartAtEndOfData()     Result(FcSeq)
      IMPLICIT NONE
      INTEGER :: FcSeq
      INTEGER :: I, U1, MinESeq
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=200) :: DirName, FName
      INTEGER, DIMENSION(7) :: LkSSeq, LkESeq
      TYPE(THeaderInfoType) :: Hdr
      
      FcSeq = MissingData_Date
      
      !
      !  Create/initialize the Hdr object used when reading the overlake 
      !  meteorology "subbasin" file.
      !
      Hdr = THeaderInfoType()
      
      !
      !  Get data period from each lake, only looking at the subdata_xxx00.csv file
      !
      DO I = 1, 7
         Bsn = LakeName3(I)                                                         ! LakeName3 defined in glshfs_global
         DirName = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator          ! the lake directory

         !
         !  Read the overlake meteorology data file to get period of record
         !
         WRITE(FName, 1502) TRIM(DirName), Bsn
         U1 = GetFreeUnitNumber()
         OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='OLD', ERR=811)
         CALL FileWasOpened(U1)
         CALL ReadHeaderInfo(FName, U1, HFT_SingleSubbasin, Hdr); IF (ErrorLevel .NE. 0) GOTO 899
         CLOSE(U1)
         CALL FileWasClosed(U1)
         LkSSeq(I) = Hdr%SDate
         LkESeq(I) = Hdr%EDate
      END DO

      !
      !  What is the earliest end date for any lake?
      !
      MinESeq = LkESeq(1)
      DO I = 2, 7
         IF (LkESeq(I) .LT. MinESeq) MinESeq = LkESeq(I)
      END DO
      
      !
      !  Ensure that end date is not prior to any start date.
      !  Should NEVER be the case if the data sets are built correctly, but I am
      !  being overly cautious.
      !
      DO I = 1, 7
         IF (MinESeq .LT. LkSSeq(I)) THEN
            ErrorMessage = 'Unable to find a valid forecast start date.';                        CALL PassMsg
            ErrorMessage = 'Historical lake meteorology data sets do not overlap sufficiently.'; CALL PassMsg
            GOTO 898
         END IF
      END DO
      
      FcSeq = MinESeq + 1
      RETURN
      
      !
      !  Error handling
      !
 811  ErrorMessage = 'Error opening ' // TRIM(FName);        CALL PassMsg;  GOTO 898
 
 898  ErrorLevel = 1
 899  ErrorMessage = '[traceback] FcstStartAtEndOfData...';  CALL PassMsg

 1502 FORMAT(A, 'subdata_', A3, '00.csv')
 
      END FUNCTION FcstStartAtEndOfData
      
      
      
END MODULE GLSHFS_LLTM