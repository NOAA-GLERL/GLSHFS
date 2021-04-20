
MODULE GLSHFS_LBRM
      USE ErrorProcess
      USE StatusMessages
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GLSHFS_Files
      USE GL_Constants
      USE GlerlDataTypesAndUnits
      USE DailyDataStructures
      USE BasinInfo
      USE LBRM_Global
      USE LBRM_Main

      !
      !  In the event we are unable to find valid initial conditions for a run from using
      !  the conventional methods (prior LBRM results and/or boundary conditions file), then
      !  we will set the initial conditions to some arbitrary semi-reasonable values. This should
      !  not happen in normal operation, but I want to prepare for any eventuality.
      !
      !  Let me be clear that these values are really garbage, and don't reflect anything of the
      !  actual dynamics of any subbasin. They are only used so that we can get a semi-reasonable 
      !  starting point when we have nothing else. This method will at least allow the LBRM 
      !  to run, and I am hoping that the subbasin will adjust relatively quickly when these 
      !  arbitrary values are a poor match for the hydrology of the subbasin.  
      !
      !  Note that when this happens, you should really go back and manually edit the master
      !  boundary conditions file to insert decent values for the necessary start date and then
      !  run the LBRM again.
      !
      !  Note that these variables are being defined as PRIVATE to the module, and that
      !  the values are in CENTIMETERS of water over the subbasin area (same units as the
      !  boundary conditions file).
      !  
      REAL, PARAMETER, PRIVATE :: DefaultUSZ = 1.0
      REAL, PARAMETER, PRIVATE :: DefaultLSZ = 5.0
      REAL, PARAMETER, PRIVATE :: DefaultGZM = 30.0
      REAL, PARAMETER, PRIVATE :: DefaultSrf = 3.0
      REAL, PARAMETER, PRIVATE :: DefaultSnw = 0.0
   
CONTAINS

      !----------------------------------------------------------------------------      
      !   This routine assumes the existence of the standard GLSHFS subbasin
      !   meteorology files; all lakes. 
      !   It sets up and runs the LBRM for all subbasins, entire period.
      !----------------------------------------------------------------------------      
      SUBROUTINE UpdateLBRM_Simulation
      IMPLICIT NONE

      INTEGER :: Lk, Sub, U2, SSeq
      INTEGER :: SSeqData, ESeqData, NewDataSSeq
      LOGICAL :: OK
      REAL    :: InitStorages(30, 5)
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=200) :: LakeDir, FName, PFName, BFName, MFName, OFName, CFName
      CHARACTER(LEN=200) :: OldFile, NewFile, MrgFile
      TYPE (THeaderInfoType)     :: Hdr
      TYPE (TDlyDataForSubbasin) :: SubData
      TYPE (TDlyData), POINTER   :: TDDP
      
      !
      !  Create/initialize things
      !
      Hdr     = THeaderInfoType()
      SubData = TDlyDataForSubbasin()
      InitStorages = -999.999

      !
      !  For each lake basin...
      !
      DO Lk = 1, 7
         Bsn = LakeName3(Lk)                                                        ! LakeName3 defined in glshfs_global
         WRITE(LakeDir, 1501) TRIM(GLSHFS_Config%BaseDir), Bsn, FilePathSeparator 

         !
         !  Read the subbasin meteorology data files in order to determine the
         !  necessary period for running to update the historical LBRM outputs.  
         !  We assume all subbasins are in sync, but will verify that here, and if
         !  they are not, we will abort the process.
         !
         !  Get the available data extent from the headers of subdata_xxx??.csv
         !  We ignore subbasin 0 (the lake surface), since we don't use that one for LBRM.
         !  Verify that all files have the same period of record before proceeding.
         !
         NewDataSSeq = DateSeq_MaxValue
         WRITE(FName, 1502) TRIM(LakeDir), Bsn, 1
         CALL ReadJustHeaderInfo(FName, HFT_SingleSubbasin, Hdr); IF (ErrorLevel .NE. 0) GOTO 899
         SSeqData = Hdr%SDate
         ESeqData = Hdr%EDate
         IF (Hdr%NewDataDate .NE. MissingData_Date) NewDataSSeq = MIN(NewDataSSeq, Hdr%NewDataDate)
         DO Sub = 2, NumSubbasins(Lk)                                    ! NumSubbasins defined in glshfs_global
            WRITE(FName, 1502) TRIM(LakeDir), Bsn, Sub
            CALL ReadJustHeaderInfo(FName, HFT_SingleSubbasin, Hdr); IF (ErrorLevel .NE. 0) GOTO 899
            OK = .TRUE.
            IF (Hdr%SDate .NE. SSeqData) OK = .FALSE.
            IF (Hdr%EDate .NE. ESeqData) OK = .FALSE.
            IF (.NOT. OK) THEN
               ErrorMessage = 'Mismatch in date extents of subbasin meteorology files.';   CALL PassMsg
               ErrorMessage = 'Dates in ' // TRIM(FName) // ' do not match subbasin 01';   CALL PassMsg
               GOTO 898
            END IF
            IF (Hdr%NewDataDate .NE. MissingData_Date) NewDataSSeq = MIN(NewDataSSeq, Hdr%NewDataDate)
         END DO

         !
         !  Set the revised SSeq value to the later of the data start date (SSeqData) or the 
         !  earliest new data date (NewDataSSeq).
         !
         SSeq = MAX(SSeqData, NewDataSSeq)
         
         !
         !  Do we need to do anything?
         !  If the new data start date is after the end of the subbasin data, that
         !  is assumed to infer that we have already done this process for the subbasin
         !  data and there is no need to do it again.  
         !
         IF (SSeq .LE. ESeqData) THEN
            !
            !  If SSeq is earlier than the start of data, that makes no sense. Something
            !  odd has occurred (bad user edit of the file? something programmatically wrong?)
            !  and we will just re-run from the start of the data.
            !
            IF (SSeq .LT. SSeqData) SSeq = SSeqData
         
            !
            !  For each subbasin...
            !
            LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator
            DO Sub = 1, NumSubbasins(Lk)                                               ! NumSubbasins defined in glshfs_global
               WRITE(ErrorMessage, 5001) Bsn, Sub;  CALL PassMsg
               
               !
               !  Build input files for LBRM that are appropriate for the subbasin data.
               !    1) Parameter file
               !    2) Boundary (initial) conditions file
               !    3) Met input file
               !    4) Control (configuration) file
               !
               WRITE(PFName, 1701) TRIM(LakeDir), Bsn, Sub
               OK = BuildLbrmParameterFile(PFName, Bsn, Sub, SSeq, ESeqData);         IF (ErrorLevel .NE. 0) GOTO 899
               
               WRITE(BFName, 1702) TRIM(LakeDir), Bsn, Sub
               OK = BuildLbrmBoundaryFile(BFName, Bsn, Sub, SSeq, ESeqData, .TRUE.);  IF (ErrorLevel .NE. 0) GOTO 899         
            
               WRITE(MFName, 1703) TRIM(LakeDir), Bsn, Sub
               OK = BuildLbrmMeteorologyFile(MFName, Bsn, Sub, SSeq, ESeqData);       IF (ErrorLevel .NE. 0) GOTO 899         
            
               WRITE(OFName, 1711) TRIM(LakeDir), Bsn, Sub     ! temporary file for output from this run only

               CFName = TRIM(LakeDir) // 'lbrm.cfg'
            
               !
               !  Write the LBRM config file
               !
               U2 = GetFreeUnitNumber()
               OPEN(UNIT=U2, FILE=TRIM(CFName), STATUS='REPLACE')
               CALL FileWasOpened(U2)
               WRITE(U2, 1801) TRIM(GLSHFS_Config%ConfigName)
               WRITE(U2, 1802) TRIM(PFName)
               WRITE(U2, 1803) TRIM(BFName)
               WRITE(U2, 1804) TRIM(MFName)
               WRITE(U2, 1805) TRIM(OFName)
               WRITE(U2, 1806) TRIM(LakeDir)
               CLOSE(U2)
               CALL FileWasClosed(U2)

               !
               !  Run the LBRM for the identified update period
               !
               CALL RunTheLBRM(TRIM(CFName));  IF (ErrorLevel .NE. 0) GOTO 899
               
               !
               !  Merge the new output (for the update period) with the data from the 
               !  entire period, overwriting data in the overlap portion.
               !  The merged file will end at the end of the new file, even if the old
               !  file extends further, because any results in the old file after the
               !  end of the new file would be discontinuous/obsolete.
               !
               !  OldFile = All of the old/existing data. This file has the name of 
               !            the "permanent" file.
               !  NewFile = The new data just created by this run of LBRM
               !  MrgFile = The merged results
               !
               NewFile = TRIM(OFName)
               WRITE(OldFile, 1704) TRIM(LakeDir), Bsn, Sub
               WRITE(MrgFile, 1712) TRIM(LakeDir), Bsn, Sub
               
               CALL MergeOutputFiles(OldFile, NewFile, MrgFile);  IF (ErrorLevel .NE. 0) GOTO 899
               
               !
               !  Replace the old file with the merged one.
               !
               CALL DeleteFile(OldFile)
               CALL RenameFile(MrgFile, OldFile, OK);  IF (.NOT. OK) GOTO 899
               
               !
               !  **** Special case handler ****
               !  When we are doing an initial run of GLSHFS, we create a brand new LBRM output file
               !  that starts on the requested start date. The initial conditions for this run are
               !  the default storage values, and likely do not reflect anything realistic.  
               !  The values on day 1 are the result after 1 day of LBRM simulation from those
               !  arbitrary starting conditions, and if we ever add more station data for that
               !  same starting date, we will end up using those resulting storages as the initial
               !  conditions for the new run. This can result in a "creep" of the output values.
               !  In order to avoid that, I want to duplicate the values from day 1 as a new entry
               !  for the master boundary conditions file. This will "lock down" those storages as 
               !  the initial storages for any run starting on day 1, and eliminate the potential 
               !  for future "creep".
               !
               !  Note that the LBRM output file values are MILLIMETERS, which we need to immediately
               !  convert into centimeters in order to be compatible with the boundary conditions file.
               !
               WRITE(FName, 1704) TRIM(LakeDir), Bsn, Sub
               CALL ReadJustHeaderInfo(FName, HFT_LBRM, Hdr); IF (ErrorLevel .NE. 0) GOTO 899
               IF (SSeq .EQ. Hdr%SDate) THEN
                  CALL ReadFile_LBRM(TRIM(FName), SubData);  IF (ErrorLevel .NE. 0) GOTO 899
                  TDDP => SubData%GetPointerToDataOfType(GDT_UpperSoilMoisture);     IF (ErrorLevel .NE. 0) GOTO 899
                  InitStorages(Sub, 1) = TDDP%GetDataVal(1) / 10.0  
                  TDDP => SubData%GetPointerToDataOfType(GDT_LowerSoilMoisture);     IF (ErrorLevel .NE. 0) GOTO 899
                  InitStorages(Sub, 2) = TDDP%GetDataVal(1) / 10.0
                  TDDP => SubData%GetPointerToDataOfType(GDT_GroundWaterMoisture);   IF (ErrorLevel .NE. 0) GOTO 899
                  InitStorages(Sub, 3) = TDDP%GetDataVal(1) / 10.0
                  TDDP => SubData%GetPointerToDataOfType(GDT_SurfaceZoneMoisture);   IF (ErrorLevel .NE. 0) GOTO 899
                  InitStorages(Sub, 4) = TDDP%GetDataVal(1) / 10.0
                  TDDP => SubData%GetPointerToDataOfType(GDT_SnowWater);             IF (ErrorLevel .NE. 0) GOTO 899
                  InitStorages(Sub, 5) = TDDP%GetDataVal(1) / 10.0
               END IF
          
               !
               !  Clear the SubData object before continuing
               !
               CALL SubData%Clear()
            END DO
            IF (SSeq .EQ. Hdr%SDate) THEN
               CALL AddEntriesToMasterBoundaryFile(Bsn, SSeq-1, InitStorages);    IF (ErrorLevel .NE. 0) GOTO 899
            END IF
            
            !
            !  Update the subbasin meteorology files to change the "NewDataStart(YMD):" entry.
            !  We have just finished processing data through the end of the file, so update
            !  the NewDataStart entry to reflect that.
            !
            DO Sub = 1, NumSubbasins(Lk)
               CALL SubData%Clear()
               WRITE(FName, 1502) TRIM(LakeDir), Bsn, Sub
               CALL ReadFile_OneSubbasin(TRIM(FName), SubData);  IF (ErrorLevel .NE. 0) GOTO 899
               SubData%NewDataSSeq = ESeqData + 1
               CALL WriteFile_OneSubbasin(TRIM(FName), SubData);  IF (ErrorLevel .NE. 0) GOTO 899
            END DO
            CALL SubData%Clear()
         END IF
      END DO

      
      !
      !  Clean up the temporary working files
      !
      DO Lk = 1, 7
         Bsn = LakeName3(Lk)                                                        ! LakeName3 defined in glshfs_global
         WRITE(LakeDir, 1501) TRIM(GLSHFS_Config%BaseDir), Bsn, FilePathSeparator 
         DO Sub = 1, NumSubbasins(Lk)                                               ! NumSubbasins defined in glshfs_global
            WRITE(PFName, 1701) TRIM(LakeDir), Bsn, Sub
            WRITE(BFName, 1702) TRIM(LakeDir), Bsn, Sub
            WRITE(MFName, 1703) TRIM(LakeDir), Bsn, Sub
            WRITE(OFName, 1711) TRIM(LakeDir), Bsn, Sub
            
            CALL DeleteFile(TRIM(PFName))
            CALL DeleteFile(TRIM(BFName))
            CALL DeleteFile(TRIM(MFName))
            CALL DeleteFile(TRIM(OFName))
         END DO
      END DO
      
      RETURN
      
      !
      !  Error handling
      !
!  811 ErrorMessage = 'Error opening input file ' //TRIM(FName);  CALL PassMsg; GOTO 898
  
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] UpdateLBRM_Simulation...'; CALL PassMsg
      RETURN


 1501 FORMAT(A, A3, A1)
 1502 FORMAT(A, 'subdata_', A3, I2.2, '.csv')

 1701 FORMAT(A, 'lbrm_parm_', A3, I2.2, '.txt')
 1702 FORMAT(A, 'lbrm_bounds_', A3, I2.2, '.csv')
 1703 FORMAT(A, 'lbrm_meteorology_', A3, I2.2, '.csv') 
 1704 FORMAT(A, 'lbrm_output_', A3, I2.2, '.csv') 
 1711 FORMAT(A, 'lbrm_new_', A3, I2.2, '.csv') 
 1712 FORMAT(A, 'lbrm_mrg_', A3, I2.2, '.csv') 

 1801 FORMAT('ID       = ', A)
 1802 FORMAT('PARMFILE = ', A)
 1803 FORMAT('BCFILE   = ', A)
 1804 FORMAT('METFILE  = ', A)
 1805 FORMAT('OUTFILE  = ', A)
 1806 FORMAT('WORKDIR  = ', A)

 5001 FORMAT('Running the LBRM to update ', A3, I2.2, ' for the historical data period.')

      END SUBROUTINE UpdateLBRM_Simulation

!----------------------------------------------------------------------------------
      !----------------------------------------------------------------------------      
      !   This routine assumes the existence of the standard GLSHFS subbasin
      !   meteorology files; all lakes. 
      !   It sets up and runs the LBRM for all subbasins, forecast period
      !----------------------------------------------------------------------------      
      SUBROUTINE RunLBRM_Forecasts
      IMPLICIT NONE

      INTEGER :: I, Lk, Sub, U2, SSeq, ESeq
      INTEGER :: Dy, Mn, Yr, ScenCount
      LOGICAL :: OK
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=4)   :: SN
      CHARACTER(LEN=200) :: OtlkDir, LakeDir, FName, PFName, BFName, OFName, CFName
      CHARACTER(LEN=200) :: MetGLSHFS, MetLBRM
      TYPE(THeaderInfoType) :: Hdr
      
      !
      !  Create/initialize the Hdr object
      !
      Hdr = THeaderInfoType()

      !
      !  Get the scenario count
      !  Note that the ScenarioNames array is assumed to have been sized EXACTLY
      !  to the number of scenarios that are being processed. That would not be the case if
      !  it were statically declared, but it is dynamically allocated.
      !
      ScenCount = UBOUND(GLSHFS_Config%ScenarioNames, 1)  
      
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
      !  If the values was NOT 8888-08-08, then assume the user specified the
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
      DO Lk = 1, 7
         Bsn = LakeName3(Lk)                                                        ! LakeName3 defined in glshfs_global
         LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator          ! the lake directory

         !
         !  Read the subbasin 1 meteorology data file to get period of record
         !  Then compare the desired forecast start date to that data period.
         !  Forecasts can only start from the data start date until one day after
         !  the data end date.
         !
         WRITE(FName, 1502) TRIM(LakeDir), Bsn, 1
         CALL ReadJustHeaderInfo(FName, HFT_SingleSubbasin, Hdr); IF (ErrorLevel .NE. 0) GOTO 899
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
         Bsn = LakeName3(Lk)                                                                               ! LakeName3 defined in glshfs_global
         LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator                                 ! the lake directory
         OtlkDir = TRIM(GLSHFS_Config%BaseDir) // TRIM(GLSHFS_Config%ForecastName) // FilePathSeparator    ! where the otlkdata files are found
         
         !
         !  For each subbasin
         !
         DO Sub = 1, NumSubbasins(Lk)                                               ! NumSubbasins defined in glshfs_global
            WRITE(StatusMsg, 5001) Bsn, Sub;     CALL WriteMsg(.TRUE., .TRUE.)

            !
            !  Build the parameter and boundary condition files that will be used for
            !  all outlook scenarios.  Each scenario needs to have identical starting points.
            !  These files provide that.
            !
            WRITE(PFName, 1701) TRIM(OtlkDir), Bsn, Sub
            WRITE(BFName, 1702) TRIM(OtlkDir), Bsn, Sub
            OK = BuildLbrmParameterFile(PFName, Bsn, Sub, SSeq, ESeq);           IF (ErrorLevel .NE. 0) GOTO 899
            OK = BuildLbrmBoundaryFile (BFName, Bsn, Sub, SSeq, ESeq, .FALSE.);  IF (ErrorLevel .NE. 0) GOTO 899         
            
            !
            !  For each ensemble member...
            !
            DO I = 1, ScenCount
               SN = TRIM(GLSHFS_Config%ScenarioNames(I))
               WRITE(StatusMsg, 5002) Bsn, Sub, I, ScenCount;    CALL WriteMsg(.TRUE., .TRUE.)

               !
               !  Build the met file with data for the specified scenario.
               !
               WRITE(MetGLSHFS, 1721) TRIM(OtlkDir), Bsn, Sub, TRIM(SN)
               WRITE(MetLBRM,   1722) TRIM(OtlkDir), Bsn, Sub, TRIM(SN)
               CALL ConvertMet_GLSHFS_to_LBRM(MetGLSHFS, MetLBRM, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899

               WRITE(OFName, 1704) TRIM(OtlkDir), Bsn, Sub, SN        ! output from this run only
               CFName = TRIM(OtlkDir) // 'lbrm.cfg'
         
               !
               !  Write the LBRM config file
               !
               U2 = GetFreeUnitNumber()
               OPEN(UNIT=U2, FILE=TRIM(CFName), STATUS='REPLACE')
               CALL FileWasOpened(U2)
               WRITE(U2, 1801) TRIM(GLSHFS_Config%ConfigName)
               WRITE(U2, 1802) TRIM(PFName)
               WRITE(U2, 1803) TRIM(BFName)
               WRITE(U2, 1804) TRIM(MetLBRM)
               WRITE(U2, 1805) TRIM(OFName)
               WRITE(U2, 1806) TRIM(LakeDir)
               CLOSE(U2)
               CALL FileWasClosed(U2)

               !
               !  Run the LBRM for the identified update period
               !
               CALL RunTheLBRM(TRIM(CFName));  IF (ErrorLevel .NE. 0) GOTO 899
            END DO
         END DO
      END DO

      RETURN
      
      !
      !  Error handling
      !
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] RunLBRM_Forecasts...'; CALL PassMsg
      RETURN


 1502 FORMAT(A, 'subdata_', A3, I2.2, '.csv')

 1701 FORMAT(A, 'lbrm_parm_', A3, I2.2, '.txt')
 1702 FORMAT(A, 'lbrm_bounds_', A3, I2.2, '.csv')
 1704 FORMAT(A, 'lbrm_output_', A3, I2.2, '_', A4, '.csv') 
 1721 FORMAT(A, 'otlkdata_', A3, I2.2, '_', A4, '.csv')
 1722 FORMAT(A, 'lbrmdata_', A3, I2.2, '_', A4, '.csv')              

 1801 FORMAT('ID       = ', A)
 1802 FORMAT('PARMFILE = ', A)
 1803 FORMAT('BCFILE   = ', A)
 1804 FORMAT('METFILE  = ', A)
 1805 FORMAT('OUTFILE  = ', A)
 1806 FORMAT('WORKDIR  = ', A)

 5001 FORMAT('Generating LBRM outlook ensemble for ', A3, I2.2, ';               ')
 5002 FORMAT('Generating LBRM outlook ensemble for ', A3, I2.2, ';   ', I0, '/', I0, '     ')
 5010 FORMAT('Specified forecast start date invalid for subbasin ', A3, '01')
 5011 FORMAT('Data file period is ', A10, ' to ', A10)

      END SUBROUTINE RunLBRM_Forecasts

      !----------------------------------------------------------------------------      
      !
      !----------------------------------------------------------------------------      
      FUNCTION BuildLbrmParameterFile(FNameOut, Bsn, Sub, SSeq, ESeq)  Result(Success)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: FNameOut
      CHARACTER(LEN=3), INTENT(IN)  :: Bsn
      INTEGER,          INTENT(IN)  :: Sub, SSeq, ESeq
      LOGICAL :: Success
      
      INTEGER :: I, IOS, U1, U2, LastLine
      INTEGER :: SD, SM, SY, ED, EM, EY
      INTEGER :: FormatOption
      LOGICAL :: OK
      CHARACTER(LEN=200) :: Line, LakeDir, FNameIn

      !
      !  Build the file names
      !
      LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator
      FNameIn = ''
      IF (TRIM(GLSHFS_Config%LbrmMethodET) .EQ. '1982') WRITE(FNameIn,  2001) TRIM(LakeDir), Bsn
      IF (TRIM(GLSHFS_Config%LbrmMethodET) .EQ. '2016') WRITE(FNameIn,  2002) TRIM(LakeDir), Bsn
      IF (LEN_TRIM(FNameIn) .EQ. 0) THEN
         ErrorMessage = 'Unable to determine the LBRM PET method from configuration information.'
         CALL PassMsg;   GOTO 898
      END IF

      !
      !  Open the input file that is an aggregation of all the parameter files,
      !  concatenated together.  
      !  The contents of the file are really just a template, since we have to update
      !  the date information for every run.
      !  Purpose of that file is to keep "clutter" to a minimum, rather than having 
      !  one template file per subbasin.
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FNameIn), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)
      
      !
      !  First order of business is simply to determine which format we
      !  have for the input file.  We are only interested in line 30 of
      !  the first subbasin block (29th data line for the subbasin).
      !
      READ(U1, 1001, ERR=812) Line          !  This should be a line of '======='
      IF (Line(1:3) .NE. '===') THEN
         ErrorMessage = 'Problem with ' // TRIM(FNameIn);                              CALL PassMsg
         ErrorMessage = 'Line 1 should be a a string of "=" characters, but is not.';  CALL PassMsg
         GOTO 898
      END IF
      
      DO I = 2, 29
         READ (U1, 1001, ERR=812) Line     ! skip lines of no interest at the moment
      END DO

      !
      !  Now try to read line 30. If we are able to read it, and it matches the string
      !  defined for the 2016 format, then set the  format choice as 2016.
      !
      FormatOption = 1982              ! default assumption
      READ (U1, 1001, IOSTAT=IOS) Line
      IF (IOS .NE. 0) THEN
         FormatOption = 1982
      ELSE
         Line = TRIM(ADJUSTL(Line))
         CALL Lowercase(Line)
         IF (Line(1:25) .EQ. 'long term air temperature') FormatOption = 2016
      END IF
         
      !
      !  Does the format of the file match the config file specification?
      !
      OK = .FALSE.
      IF ((TRIM(GLSHFS_Config%LbrmMethodET) .EQ. '1982') .AND. (FormatOption .EQ. 1982)) OK = .TRUE.
      IF ((TRIM(GLSHFS_Config%LbrmMethodET) .EQ. '2016') .AND. (FormatOption .EQ. 2016)) OK = .TRUE.
      IF ((TRIM(GLSHFS_Config%LbrmMethodET) .EQ. '2017') .AND. (FormatOption .EQ. 1982)) OK = .TRUE.
      IF (.NOT. OK) THEN
         ErrorMessage = 'Problem with ' // TRIM(FNameIn);                                  CALL PassMsg
         ErrorMessage = 'It does not conform to the prescribed format for this version.';  CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Format is good, so rewind and skip over the subbasins we do not need.
      !  This will leave us with the file pointer positioned on the first good
      !  line for the subbasin requested.
      !
      REWIND(U1)
      I = 0
      DO WHILE (I .LT. Sub)
         READ(U1, 1001, ERR=812) Line
         IF (Line(1:3) .EQ. '===') I = I + 1
      END DO
      
      !
      !  Open the output file
      !
      U2 = GetFreeUnitNumber()
      OPEN(UNIT=U2, FILE=TRIM(FNameOut), STATUS='REPLACE', ERR=821)
      CALL FileWasOpened(U2)

      !
      !  Read/write the first 2 line
      !
      READ (U1, 1001, ERR=812) Line
      WRITE(U2, 1102, ERR=823) Bsn, Sub
      READ (U1, 1001, ERR=812) Line
      WRITE(U2, 1101, ERR=823) TRIM(Line)       ! subbasin area
      
      !
      !  Process line 3 (the dates)
      !
      READ(U1, 1002, ERR=812) Line
      CALL SequenceDate(SD, SM, SY, SSeq); IF (ErrorLevel .NE. 0) GOTO 899
      CALL SequenceDate(ED, EM, EY, ESeq); IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(U2, 1103, ERR=823) SY, SM, SD, EY, EM, ED
      
      !
      !  Read/write lines 4-28
      !
      DO I = 4, 28
         READ (U1, 1001, ERR=812) Line
         WRITE(U2, 1101, ERR=823) TRIM(Line)
      END DO
      
      !
      !  Line 29 is where the differences start.
      !
      IF (FormatOption .EQ. 1982) LastLine = 28
      IF (FormatOption .EQ. 2016) THEN
         LastLine = 61
         DO I = 29, LastLine
            READ (U1, 1001, ERR=812) Line
            WRITE(U2, 1101, ERR=823) TRIM(Line)
         END DO
      END IF
 
      CLOSE(U1)
      CALL FileWasClosed(U1)
      CLOSE(U2)
      CALL FileWasClosed(U2)
      Success = .TRUE.
      RETURN
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file ' //TRIM(FNameIn);  CALL PassMsg; GOTO 898
  812 ErrorMessage = 'Error reading input file ' //TRIM(FNameIn);  CALL PassMsg; GOTO 898
  821 ErrorMessage = 'Error opening output file '//TRIM(FNameOut); CALL PassMsg; GOTO 898
  823 ErrorMessage = 'Error writing output file '//TRIM(FNameOut); CALL PassMsg; GOTO 898
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] BuildLbrmParameterFile...'; CALL PassMsg
      Success = .FALSE.
      RETURN

      !
      !  Formats
      !      
 1001 FORMAT(A200)
 1002 FORMAT(57X, A100)
 
 1101 FORMAT(A)
 1102 FORMAT(A3, ',', I3.2, ',',  26X, '(lake and subbasin)')
 1103 FORMAT(2(1X, I4.4, 2('-',I2.2), ','), 10X, '(start date (y-m-d), end date (y-m-d))')

 2001 FORMAT(A, 'lbrm_parameters_1982_', A3, '.txt')
 2002 FORMAT(A, 'lbrm_parameters_2016_', A3, '.txt')
 
      END FUNCTION BuildLbrmParameterFile


      !----------------------------------------------------------------------------      
      !  Build the initial conditions / boundary conditions file for a single
      !  subbasin, for the period specified.
      !
      !  The variable UseBounds specifies if we want values from the master boundary
      !  conditions file (lbrm_boundaryconditions_xxx.csv) used throughout the run.
      !  When running this for a historical period it is appropriate to set the
      !  variable UseBounds to TRUE, so that information from the user-supplied
      !  boundary file will be used. When running for a forecast period, we do NOT 
      !  want to use those values, so UseBounds should be set to FALSE.  But we will
      !  always use the values from the master file IF they are for the day prior
      !  to the forecast start date. In that case, they are the best thing to use
      !  for the initial conditions.
      !----------------------------------------------------------------------------      
      FUNCTION BuildLbrmBoundaryFile(FNameOut, Bsn, Sub, RunSSeq, RunESeq, UseBounds)  Result(Success)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: FNameOut
      CHARACTER(LEN=3), INTENT(IN)  :: Bsn
      INTEGER,          INTENT(IN)  :: Sub, RunSSeq, RunESeq
      LOGICAL,          INTENT(IN)  :: UseBounds
      LOGICAL :: Success

      INTEGER :: I, J, K, IOS
      INTEGER :: Dy, Mn, Yr, Seq, DayCount
      INTEGER :: UOut, NumBounds, DBRD
      LOGICAL :: OK, FExist, UseHistData, OOR, StillInvalid
      REAL    :: SVals(5), InitStorages(5)
      CHARACTER(LEN=10)  :: SDStr, EDStr
      CHARACTER(LEN=200) :: LakeDir, FNameIn, BFName

      INTEGER, DIMENSION(MaxLbrmBC)    :: BoundDates
      REAL,    DIMENSION(MaxLbrmBC, 5) :: BoundVals
      
      TYPE (TDlyDataForSubbasin) :: SubStorages
      TYPE (TDlyData), POINTER   :: TDDP_USZ, TDDP_LSZ, TDDP_GZM, TDDP_Srf, TDDP_Snw

      !
      !  Working directory name
      !
      LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator

      !
      !  Initialize the TDlyDataForSubbasin object so that the automatic
      !  finalize procedure doesn't crash in case of early exit.
      !
      SubStorages = TDlyDataForSubbasin()
      UOut = -1

      !
      !  Read the pre-defined boundary conditions for this subbasin from the
      !  user-supplied file;  lbrm_boundaryconditions_xxx.txt
      !  We always need to read this file, even if UseBounds=FALSE, because
      !  it might have an entry appropriate for the run start.
      !  Values in the file are CENTIMETERS over the subbasin.
      ! 
      WRITE(BFName, 1101, ERR=897) TRIM(LakeDir), Bsn
      INQUIRE(FILE=TRIM(BFName), EXIST=FExist)
      IF (.NOT. FExist) THEN
         WRITE(ErrorMessage, 5001, ERR=897) Bsn;  CALL PassMsg
         GOTO 898
      END IF
      CALL ReadMasterBoundaryFile(Bsn, Sub, BoundDates, BoundVals, NumBounds)
      IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Read the standard LBRM output file for this subbasin.  We may extract 
      !  modeled moisture storage data from this file.  If this file
      !  does not exist, we can continue without it.
      !  Values in this file are MILLIMETERS over the subbasin.
      !
      WRITE(FNameIn, 1102, ERR=897) TRIM(LakeDir), Bsn, Sub
      INQUIRE(FILE=TRIM(FNameIn), EXIST=FExist)
      IF (FExist) THEN
         CALL ReadFile_LBRM(TRIM(FNameIn), SubStorages); IF (ErrorLevel .NE. 0) GOTO 899
         UseHistData = .TRUE.
      ELSE
         UseHistData = .FALSE.
      END IF
      
      !
      !  If the lbrm_output_xxxnn.csv file contains data for the period of interest, we will
      !  extract the data for the day prior to the specified start date. That will give us
      !  the valid initial conditions we require.  Our specific day of interest is the
      !  day just prior to the specified start date, because the values being read from the 
      !  file represent END-OF-DAY storages. Therefore, initial conditions for the run come 
      !  from the day BEFORE RunSSeq.
      !  
      !  If the specified run start date happens to be on the same day as the file
      !  start date, then we obviously cannot get storages from the day before.
      !  In this special case, we will just use the values on the requested start
      !  date as the initial condition values. Yes, if this were to be done over and 
      !  over for a single day run it could possibly have a noticeable effect on results,
      !  but I don't expect that to happen, and we need some clean method for running
      !  from the period of record start.
      !
      !  If the lbrm_output_xxxnn.csv file does not exist or does not have the required data, 
      !  then we will just set the initial storage values to default values.
      !
      !  In both cases, we will subsequently check for entries in the master boundary conditions
      !  file and overwrite the initial storages value if appropriate.
      !
      !  Note that values from the boundary conditions file are expressed in centimeters over the
      !  watershed, but the values in the LBRM output file are in millimeters.  We need to reconcile
      !  that difference before moving on.  I will do that by converting any values read from the 
      !  LBRM output file into centimeters.  That allows me to use values from the boundary conditions 
      !  file as supplied, and also matches with the units I used for the DefaultXXX values.
      !
      OK = UseHistData
      IF (RunSSeq-1 .GT. SubStorages%EDateSeq)  OK = .FALSE.
      IF (RunESeq   .LT. SubStorages%SDateSeq)  OK = .FALSE.
      IF (OK) THEN
         TDDP_USZ => SubStorages%GetPointerToDataOfType(GDT_UpperSoilMoisture);   IF (ErrorLevel .NE. 0) OK = .FALSE.
         TDDP_LSZ => SubStorages%GetPointerToDataOfType(GDT_LowerSoilMoisture);   IF (ErrorLevel .NE. 0) OK = .FALSE.
         TDDP_GZM => SubStorages%GetPointerToDataOfType(GDT_GroundWaterMoisture); IF (ErrorLevel .NE. 0) OK = .FALSE.
         TDDP_Srf => SubStorages%GetPointerToDataOfType(GDT_SurfaceZoneMoisture); IF (ErrorLevel .NE. 0) OK = .FALSE.
         TDDP_Snw => SubStorages%GetPointerToDataOfType(GDT_SnowWater);           IF (ErrorLevel .NE. 0) OK = .FALSE.
         IF (OK) THEN
            IF (RunSSeq .EQ. SubStorages%SDateSeq) THEN
               I = 1                                    ! first day of file data
            ELSE
               I = RunSSeq - SubStorages%SDateSeq       ! index of day BEFORE RunSSeq
            END IF
         ELSE
            I = -1
         END IF
         DayCount = TDDP_USZ%GetNumDays()
         IF ((I .GE. 1) .AND. (I .LE. DayCount)) THEN
            InitStorages(1) = TDDP_USZ%GetDataVal(I) / 10.0        ! mm -> cm
            InitStorages(2) = TDDP_LSZ%GetDataVal(I) / 10.0        ! mm -> cm
            InitStorages(3) = TDDP_GZM%GetDataVal(I) / 10.0        ! mm -> cm
            InitStorages(4) = TDDP_Srf%GetDataVal(I) / 10.0        ! mm -> cm
            InitStorages(5) = TDDP_Snw%GetDataVal(I) / 10.0        ! mm -> cm
         ELSE
            InitStorages(1) = DefaultUSZ
            InitStorages(2) = DefaultLSZ
            InitStorages(3) = DefaultGZM
            InitStorages(4) = DefaultSrf
            InitStorages(5) = DefaultSnw
            WRITE(ErrorMessage, 5003) TRIM(FNameIn);  CALL PassMsg
            WRITE(ErrorMessage, 5004);                CALL PassMsg
         END IF
      ELSE
         InitStorages(1) = DefaultUSZ
         InitStorages(2) = DefaultLSZ
         InitStorages(3) = DefaultGZM
         InitStorages(4) = DefaultSrf
         InitStorages(5) = DefaultSnw
      END IF
         
      !
      !  Do we have an entry in the boundary conditions set for the initial
      !  conditions day? If so, then overwrite the values we just assigned
      !  into InitStorages.
      !
      DO I = 1, NumBounds
         IF (BoundDates(I) .EQ. RunSSeq-1) THEN
            DO J = 1, 5
               IF (.NOT. IsMissing(BoundVals(I,J))) THEN
                  InitStorages(J) = BoundVals(I,J)
               END IF
            END DO
         END IF
      END DO
      
      !
      !  If InitStorages are still invalid, that means the lbrm_output_xxxnn.csv file
      !  did not have data for the required date, and no entry in the master boundary 
      !  conditions file matched the date. We may end up falling back to simple default
      !  values, but let's try one more thing first. If there are 1+ entries in the master
      !  boundary conditions file that are BEFORE the required date, then we will use 
      !  the nearest entry as our starting conditions. The rationale for this is that those
      !  are assumed to at least be reasonable values FOR THIS SUBBASIN.  The basic
      !  default values might be quite different from the behavior for this subbasin.
      !  We will, however, always set snowpack to 0.
      !
      StillInvalid = .FALSE.
      DO J = 1, 5
         IF ((InitStorages(J) .LT.    0.0) .OR.            &
             (InitStorages(J) .GT. 9999.0)) THEN
            StillInvalid = .TRUE.
         END IF
      END DO
      IF (StillInvalid) THEN
         DBRD = 9999999              ! # days before required date of nearest entry
         K = -1                      ! index of the closest entry
         DO I = 1, NumBounds
            IF (BoundDates(I) .LT. RunSSeq-1) THEN
               DayCount = (RunSSeq-1) - DBRD
               IF (DayCount .LT. DBRD) THEN
                  DBRD = DayCount
                  K = I
               END IF
            END IF
         END DO
         IF (K .GT. 0) THEN
            DO J = 1, 4
               IF (.NOT. IsMissing(BoundVals(K,J))) THEN
                  InitStorages(J) = BoundVals(K,J)
               END IF
            END DO
            InitStorages(5) = 0.00
         END IF
      END IF
      
      !
      !  If InitStorages are invalid, replace with default values.
      !  Warn the user if this happens.
      !
      OOR = .FALSE.
      DO J = 1, 5
         IF ((InitStorages(J) .LT.    0.0) .OR.            &
             (InitStorages(J) .GT. 9999.0)) THEN
            OOR = .TRUE.
            IF (J .EQ. 1) InitStorages(J) = DefaultUSZ
            IF (J .EQ. 2) InitStorages(J) = DefaultLSZ
            IF (J .EQ. 3) InitStorages(J) = DefaultGZM
            IF (J .EQ. 4) InitStorages(J) = DefaultSrf
            IF (J .EQ. 5) InitStorages(J) = DefaultSnw
         END IF
      END DO
      IF (OOR) THEN
         ErrorMessage = 'Warning: Invalid boundary condition(s) replaced with default values'
         CALL PassMsg
      END IF
      
      
      !
      !  Open the output file and write the header plus the initial 
      !  conditions entry.
      !
      UOut = GetFreeUnitNumber()
      OPEN(UNIT=UOut, FILE=TRIM(FNameOut), STATUS='REPLACE', ERR=821)
      SDStr = SeqToDateStringYMD(RunSSeq)
      EDStr = SeqToDateStringYMD(RunESeq)
      WRITE(UOut, 4001, ERR=823)
      WRITE(UOut, 4002, ERR=823)
      WRITE(UOut, 4003, ERR=823)
      WRITE(UOut, 4004, ERR=823)
      WRITE(UOut, 4005, ERR=823) Bsn, Sub, SDStr, EDStr
      WRITE(UOut, 4006, ERR=823)
      CALL SequenceDate(Dy, Mn, Yr, RunSSeq-1);   IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(UOut, 4010, ERR=823) Yr, Mn, Dy, (InitStorages(J), J=1,5)
      
      !
      !  If appropriate, add additional boundary conditions from the file that
      !  cover other dates within the entire specified period.
      !  If UseBounds is false, the output file will only contain values for
      !  the first day.
      !
      IF (UseBounds) THEN
         DO Seq = RunSSeq+1, RunESeq
            DO I = 1, NumBounds
               IF (BoundDates(I) .EQ. Seq) THEN
                  SVals(:) = -999.99
                  DO J = 1, 5
                     IF (.NOT. IsMissing(BoundVals(I,J))) THEN
                        SVals(J) = BoundVals(I,J)
                     END IF
                  END DO
                  CALL SequenceDate(Dy, Mn, Yr, BoundDates(I));   IF (ErrorLevel .NE. 0) GOTO 899
                  WRITE(UOut, 4010, ERR=823) Yr, Mn, Dy, (SVals(J), J=1,5)
               END IF
            END DO
         END DO
      END IF
      CLOSE(UOut)
      CALL FileWasClosed(UOut)
      
      Success = .TRUE.
      GOTO 999

      !
      !  Error handling
      !
  821 ErrorMessage = 'Error opening output file '//TRIM(FNameOut); CALL PassMsg; GOTO 898
  823 ErrorMessage = 'Error writing output file '//TRIM(FNameOut); CALL PassMsg; GOTO 898
      
      
  897 ErrorMessage = 'Unexpected and unhandled error.';  CALL PassMsg
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] BuildLbrmBoundaryFile...'; CALL PassMsg
      Success = .FALSE.
      
  999 IF (UOut .GT. 0) THEN
         CLOSE(UOut, IOSTAT=IOS)
         CALL FileWasClosed(UOut)
      END IF
      CALL SubStorages%Clear()
      RETURN

      !
      !  Formats
      !      
 1101 FORMAT(A, 'lbrm_boundaryconditions_', A3, '.txt')
 1102 FORMAT(A, 'lbrm_output_', A3, I2.2, '.csv')
      
 4001 FORMAT('LBRM moisture storage boundary conditions to be used as non-conservative')
 4002 FORMAT('values; overwriting end-of-day values calculated by LBRM.')
 4003 FORMAT('Values are specified in centimeters over the subbasin area.')
 4004 FORMAT('Missing values are denoted by -999.999')
 4005 FORMAT(A3, ', ', I2.2, 2(', ', A), ',   <- basin, subbasin, start date (Y-M-D), end date (Y-M-D)')
 4006 FORMAT('YYYY-MM-DD,   UpperSoil,   LowerSoil, GroundWater, SurfaceZone, SnowWaterEq')
 4010 FORMAT(I4, 2('-', I2.2), 5(',', F12.3))      
         
 
 5001 FORMAT('Error: lbrm_boundaryconditions_', A3, '.txt missing from lake directory.')
! 5002 FORMAT('Error: lbrm_output_', A3, I2.2, '.csv missing from lake directory.')
 5003 FORMAT('Warning: Unable to find required data in ', A)
 5004 FORMAT('         Using default initial storage values')
 
      END FUNCTION BuildLbrmBoundaryFile

      !----------------------------------------------------------------------------      
      !  Build a meteorology file in the native LBRM format for the specified
      !  subbasin, for the period specified.
      !  Data is extracted from the GLSHFS-format file.
      !----------------------------------------------------------------------------      
      FUNCTION BuildLbrmMeteorologyFile(FNameOut, Bsn, Sub, RunSSeq, RunESeq)  Result(Success)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: FNameOut
      CHARACTER(LEN=3), INTENT(IN)  :: Bsn
      INTEGER,          INTENT(IN)  :: Sub, RunSSeq, RunESeq
      LOGICAL :: Success

      INTEGER :: I, J, IOS, UOut, DC
      INTEGER :: Dy, Mn, Yr, Seq, NumDays
      REAL    :: ConvFactor, SwapVal
      CHARACTER(LEN=200) :: LakeDir, FNameIn
      
      TYPE (TDlyData), POINTER    :: TDDP
      TYPE (TDlyDataForSubbasin)  :: SubMetData
      REAL, DIMENSION(:),   ALLOCATABLE :: TMet
      REAL, DIMENSION(:,:), ALLOCATABLE :: MetData             ! indexed (3, NumDays)
      !
      !  Working directory name
      !
      LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator

      !
      !  Initialize the TDlyDataForSubbasin object so that the automatic
      !  finalize procedure doesn't crash in case of early exit.
      !
      SubMetData = TDlyDataForSubbasin()
      
      !
      !  Read the historic meteorology for this subbasin
      !
      WRITE(FNameIn, 1101) TRIM(LakeDir), Bsn, Sub
      CALL ReadFile_OneSubbasin(TRIM(FNameIn), SubMetData); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Verify that the file contains data for the entire period of interest
      !
      IF (SubMetData%SDateSeq .GT. RunSSeq)  GOTO 701
      IF (SubMetData%EDateSeq .LT. RunESeq)  GOTO 701
      
      !
      !  Allocate the arrays for storing met data
      !
      NumDays = RunESeq - RunSSeq + 1                             ! days in the output period
      ALLOCATE(MetData(3, NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5001) NumDays, Bsn, Sub;   CALL PassMsg
         GOTO 898
      END IF

      !
      !  Transfer data from SubMetData to the temporary array
      !  Also do any necessary unit conversions to get air temps into deg C and 
      !  precipitation into millimeters.
      !
      !  Note that the data are trimmed here.  MetData(:,:) contains only the
      !  period of interest from SubMetData.
      !
      I = RunSSeq - SubMetData%SDateSeq + 1
      J = RunESeq - SubMetData%SDateSeq + 1
      IF (J-I+1 .NE. NumDays) THEN
         ErrorMessage = 'Strange mismatch error in dates for LBRM met.'; CALL PassMsg
         GOTO 898
      END IF
      
      TDDP => SubMetData%GetPointerToDataOfType(GDT_AirtempMin);    IF (ErrorLevel .NE. 0) GOTO 899
      ConvFactor = UnitConvertedDataValue(1.0, TDDP%GetDataUnit(), GDU_Celsius)
      
      DC = TDDP%GetEndDate() - TDDP%GetStartDate() + 1
      ALLOCATE(TMet(DC), STAT=IOS)
      TMet = TDDP%GetDataVals()
      MetData(1,:) = TMet(I:J) * ConvFactor
      DEALLOCATE(TMet, STAT=IOS)
      
      TDDP => SubMetData%GetPointerToDataOfType(GDT_AirtempMax);    IF (ErrorLevel .NE. 0) GOTO 899
      ConvFactor = UnitConvertedDataValue(1.0, TDDP%GetDataUnit(), GDU_Celsius)
      DC = TDDP%GetEndDate() - TDDP%GetStartDate() + 1
      ALLOCATE(TMet(DC), STAT=IOS)
      TMet = TDDP%GetDataVals()
      MetData(2,:) = TMet(I:J) * ConvFactor
      DEALLOCATE(TMet, STAT=IOS)
      
      TDDP => SubMetData%GetPointerToDataOfType(GDT_Precipitation);    IF (ErrorLevel .NE. 0) GOTO 899
      ConvFactor = UnitConvertedDataValue(1.0, TDDP%GetDataUnit(), GDU_Millimeters)
      DC = TDDP%GetEndDate() - TDDP%GetStartDate() + 1
      ALLOCATE(TMet(DC), STAT=IOS)
      TMet = TDDP%GetDataVals()
      MetData(3,:) = TMet(I:J) * ConvFactor
      DEALLOCATE(TMet, STAT=IOS)
      
      !
      !  The LBRM cannot accept missing data values.
      !  If there are missing values in MetData, we need to fill them in somehow.
      !  For purposes of doing something "reasonable", I will be using the long-term
      !  average daily value.  For example, if the TMin value for April 16 is missing,
      !  we will get the average of all valid April 16 TMin values in the file. That 
      !  will be filled in as the value for this missing day.  Note that the ENTIRE
      !  period is used (all of SubMetData), not just the period of direct interest.
      !
      !  This is not a great solution, but it's the best I know to do with the data 
      !  we have.  The main issues:
      !
      !  1) It will force temperatures toward mean values. This might interrupt warm
      !     or cold spells. But what else can we do? 
      !  2) It will likely produce a non-zero precip value for every day it is done.
      !     That is not good, but probably better than randomly choosing between zero
      !     and some kind of scaled value.
      !
      !  If a better approach is devised, please implement it.
      !
      CALL FillMissingData(MetData, RunSSeq, RunESeq, SubMetData); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Check to see if we were able to fill in the values for all missing days
      !
      DO Seq = RunSSeq, RunESeq
         I = Seq - RunSSeq + 1
         IF (MetData(1,I) .LE. MissingData_Real_Test) THEN
            WRITE(ErrorMessage, 5011) DayOfYear(Seq);       CALL PassMsg
            ErrorMessage = 'Unable to continue with LBRM';  CALL PassMsg
            GOTO 898
         END IF
         IF (MetData(2,I) .LE. MissingData_Real_Test) THEN
            WRITE(ErrorMessage, 5012) DayOfYear(Seq);       CALL PassMsg
            ErrorMessage = 'Unable to continue with LBRM';  CALL PassMsg
            GOTO 898
         END IF
         IF (MetData(3,I) .LE. MissingData_Real_Test) THEN
            WRITE(ErrorMessage, 5013) DayOfYear(Seq);       CALL PassMsg
            ErrorMessage = 'Unable to continue with LBRM';  CALL PassMsg
            GOTO 898
         END IF
      END DO

      !
      !  Make sure that TMin < TMax for all days.
      !  If we encounter a day when that is false, then simply flip the numbers.
      !
      DO Seq = RunSSeq, RunESeq
         I = Seq - RunSSeq + 1
         IF (MetData(1,I) .GE. MetData(2,I)) THEN
            SwapVal = MetData(1,I)
            MetData(1,I) = MetData(2,I)
            MetData(2,I) = SwapVal
         END IF
      END DO
      
      !
      !  Write the output file
      !  Note that this output file is in the relatively ugly format that is currently required
      !  by the LBRM. Revision of this (to a csv-type format) is on my long-term "to-do" list.
      !
      !
      UOut = GetFreeUnitNumber()
      OPEN(UNIT=UOut, FILE=TRIM(FNameOut), STATUS='REPLACE', ERR=821)
      CALL FileWasOpened(UOut)
      WRITE(UOut, 2001, ERR=823)
      WRITE(UOut, 2002, ERR=823) Bsn, Sub
      WRITE(UOut, 2003, ERR=823)
      DO Seq = RunSSeq, RunESeq
         CALL SequenceDate(Dy, Mn, Yr, Seq); IF (ErrorLevel .NE. 0) GOTO 899
         I = Seq - RunSSeq + 1
         WRITE(UOut, 2005, ERR=823) Yr, Mn, Dy, (MetData(J,I), J=1,3)
      END DO
      CLOSE(UOut)
      CALL FileWasClosed(UOut)

      Success = .TRUE.
      GOTO 999

      !
      !  Error handling
      !
  701 WRITE(ErrorMessage, 5005) TRIM(FNameIn);                     CALL PassMsg; GOTO 898
      
  821 ErrorMessage = 'Error opening output file '//TRIM(FNameOut); CALL PassMsg; GOTO 898
  823 ErrorMessage = 'Error writing output file '//TRIM(FNameOut); CALL PassMsg; GOTO 898
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] BuildLbrmMeteorologyFile...'; CALL PassMsg
      Success = .FALSE.

      
  999 DEALLOCATE(TMet,    STAT=IOS)
      DEALLOCATE(MetData, STAT=IOS)
      RETURN
      
      !
      !  Formats
      !      
      
 1101 FORMAT(A, 'subdata_', A3, I2.2, '.csv')
      
 2001 FORMAT('Meteorology data for use in LBRM')
 2002 FORMAT(A3, ', ', I2.2, ',   <- lake basin, subbasin')
 2003 FORMAT('yyyy-mm-dd, Tmin(C), Tmax(C), Prec(mm)')
 2005 FORMAT(I4.4, 2('-',I2.2), 3(',', F7.2))
      
 5001 FORMAT('Error allocating memory for ', I0, ' days of met data;  subbasin ', A3, I2.2)       
 5005 FORMAT('Error: ', A, ' has insufficient data for the requested LBRM run.')
 
 5011 FORMAT('Unable to fill missing TMin data for day of year ', I0)
 5012 FORMAT('Unable to fill missing TMax data for day of year ', I0)
 5013 FORMAT('Unable to fill missing precipitation data for day of year ', I0)
      
      END FUNCTION BuildLbrmMeteorologyFile

!------------------------------------------------------------------------      
      !------------------------------------------------------------------
      !  Reads a meteorology data file in GLSHFS single-subbasin format and 
      !  outputs a met data file in the LBRM native format.
      !------------------------------------------------------------------
      SUBROUTINE ConvertMet_GLSHFS_to_LBRM(FileGLSHFS, FileLBRM, ReqSSeq, ReqESeq)
      IMPLICIT NONE
      CHARACTER(LEN=*),  INTENT(IN) :: FileGLSHFS, FileLBRM
      INTEGER, OPTIONAL, INTENT(IN) :: ReqSSeq, ReqESeq

      INTEGER :: U1, I, DT, IOS, Seq, SSeq, ESeq, NumDays
      INTEGER :: SSeqCutoff, ESeqCutoff, SSeqOut, ESeqOut
      REAL    :: Cnv
      CHARACTER(LEN=8)  :: ValStr(3)
      CHARACTER(LEN=10) :: DateYMD
      
      REAL, DIMENSION(:),   ALLOCATABLE :: OData
      REAL, DIMENSION(:,:), ALLOCATABLE :: MetValues      ! (3, NumDays)
      
      TYPE (TDlyDataForSubbasin)  :: SubData
      TYPE (TDlyData), POINTER    :: TDDP

      INTEGER, DIMENSION(3), PARAMETER :: ReqType =                 &
         (/ GDT_AirTempMin, GDT_AirTempMax, GDT_Precipitation /)
      
      INTEGER, DIMENSION(3), PARAMETER :: ReqUnit =                  &
         (/ GDU_Celsius,  GDU_Celsius,     GDU_Millimeters/)
      
      !
      !
      U1 = -1
      SubData = TDlyDataForSubbasin()

      !
      !  Did user specify a requested start date? End date?
      !
      IF (PRESENT(ReqSSeq)) THEN
         SSeqCutoff = ReqSSeq
      ELSE
         CALL DateSequence(1, 1, 1000, SSeqCutoff)
      END IF
      
      IF (PRESENT(ReqESeq)) THEN
         ESeqCutoff = ReqESeq
      ELSE
         CALL DateSequence(31, 12, 3000, ESeqCutoff)
      END IF
      
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
      !  Order of items in the output file is:  TMin, TMax, Precip
      !
      NumDays = ESeq - SSeq + 1
      ALLOCATE(MetValues(3, NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5002) NumDays;   CALL PassMsg
         GOTO 898
      END IF
      DO DT = 1, 3
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
      OPEN(UNIT=U1, FILE=TRIM(FileLBRM), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)

      WRITE(U1, 1001, ERR=812)
      WRITE(U1, 1002, ERR=812) SubData%Bsn, SubData%SubNum
      WRITE(U1, 1003, ERR=812)

      SSeqOut = MAX(SSeqCutoff, SSeq)
      ESeqOut = MIN(ESeqCutoff, ESeq)
      
      DO Seq = SSeqOut, ESeqOut
         I = Seq - SSeq + 1
         DO DT = 1, 3
            IF (MetValues(DT,I) .LE. MissingData_Real_Test) THEN
               ValStr(DT) = '      NA'
            ELSE
               WRITE(ValStr(DT), 1050) MetValues(DT,I)
            END IF
         END DO
         DateYMD = SeqToDateStringYMD(Seq)
         WRITE(U1, 1010, ERR=812) DateYMD, (ValStr(DT), DT=1,3)
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      U1 = -1
      
      GOTO 999
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening '    // TRIM(FileLBRM);   CALL PassMsg;  GOTO 898
  812 ErrorMessage = 'Error writing to ' // TRIM(FileLBRM);   CALL PassMsg;  GOTO 898
  
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ConvertMet_GLSHFS_to_LBRM...';  CALL PassMsg
     
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
 1001 FORMAT('Meteorological forcing data for the Large Basin Runoff Model.')
 1002 FORMAT(A3, ', ', I2.2, ',   <- lake basin, subbasin')
 1003 FORMAT('yyyy-mm-dd, Tmin(C), Tmax(C), Prec(mm)')
 1010 FORMAT(A10, 3(',', A8))
 1050 FORMAT(F8.3)
 
 5001 FORMAT(' No valid data read from ', A)
 5002 FORMAT(' Error allocating memory for ', I0, ' days of met data')
      
      END SUBROUTINE ConvertMet_GLSHFS_to_LBRM



      !----------------------------------------------------------------------------      
      !  Read boundary conditions for a single subbasin (Sub) from the master
      !  boundary conditions file for the lake (Bsn).
      !----------------------------------------------------------------------------      
      SUBROUTINE ReadMasterBoundaryFile(Bsn, Sub, BDates, BValues, NumBC)
      IMPLICIT NONE
      CHARACTER(LEN=3),                INTENT(IN)    :: Bsn
      INTEGER,                         INTENT(IN)    :: Sub
      INTEGER, DIMENSION(MaxLbrmBC),   INTENT(INOUT) :: BDates       ! DateSequence numbers
      REAL,    DIMENSION(MaxLbrmBC,5), INTENT(INOUT) :: BValues      ! Values (USZ, LSZ, GZM, Srf, Snw)
      INTEGER,                         INTENT(OUT)   :: NumBC        ! # of values
      
      INTEGER :: I, J, K, IOS, IOS2
      INTEGER :: Seq
      INTEGER :: UBnd, NumStr, SB
      LOGICAL :: FExist
      REAL    :: SVals(5)
      CHARACTER(LEN=3)   :: S3, BB
      CHARACTER(LEN=150) :: Line, Strings(12)
      CHARACTER(LEN=200) :: LakeDir, BndFile

      !
      !  Working directory name
      !
      LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator

      !
      !  Initialize stuff
      !    
      NumBC        = 0
      BDates(:)    = MissingData_Date
      BValues(:,:) = MissingData_Real
      
      !
      !  Build file name and test for existence
      !
      WRITE(BndFile, 1101) TRIM(LakeDir), Bsn
      INQUIRE(FILE=TRIM(BndFile), EXIST=FExist)
      IF (.NOT. FExist) THEN
         WRITE(ErrorMessage, 5001) TRIM(BndFile); CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Open it and skip 6 header lines
      !
      UBnd = GetFreeUnitNumber()
      OPEN(UNIT=UBnd, FILE=TRIM(BndFile), STATUS='OLD', ERR=811)
      CALL FileWasOpened(UBnd)
      DO I = 1, 6
         READ(UBnd, *, ERR=812)      ! skip 6 lines of header
      END DO

      !
      !  Read and process lines until we hit EOF.
      !
      S3 = GetLowerCase(Bsn)                               ! lowercase version of the 3-char basin name
      READ(UBnd, 1001, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         CALL LowerCase(Line)
         CALL ParseCommaSepLine(Line, Strings, NumStr)
         IF (ErrorLevel .NE. 0) NumStr = 0                 ! If error, skip this line
         IF (NumStr .EQ. 8) THEN
            BB = Strings(1)(1:3)                           ! basin name (3-char)
            READ(Strings(2), *, IOSTAT=J) SB               ! subbasin number
            IF (J .NE. 0) SB = -99
            IF ((BB .EQ. S3) .AND. (SB .EQ. Sub)) THEN     ! this is correct bsn and subbasin
               Seq = DateStringYMDToSeq(Strings(3))        ! date
               IF (Seq .NE. MissingData_Date) THEN
                  J = 0
                  DO K = 1, 5
                     READ(Strings(3+K), *, IOSTAT=IOS2) SVals(K)
                     J = J + IOS2
                  END DO
                  IF (J .EQ. 0) THEN     ! No errors parsing the numeric values
                     NumBC = NumBC + 1
                     IF (NumBC .LE. MaxLbrmBC) THEN
                        BDates(NumBC) = Seq
                        DO K = 1, 5
                           BValues(NumBC, K) = SVals(K)
                        END DO
                     ELSE
                        WRITE(ErrorMessage, 5002) TRIM(BndFile), Bsn, Sub
                     END IF
                  END IF
               END IF
            END IF
         END IF                  
         READ(UBnd, 1001, IOSTAT=IOS) Line
      END DO
      CLOSE(UBnd)
      CALL FileWasClosed(UBnd)
      RETURN

      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening input file ' //TRIM(BndFile);  CALL PassMsg; GOTO 898
  812 ErrorMessage = 'Error reading input file ' //TRIM(BndFile);  CALL PassMsg; GOTO 898
      
  898 ErrorLevel = 1
      ErrorMessage = '[traceback] ReadMasterBoundaryFile...'; CALL PassMsg
      RETURN

      !
      !  Formats
      !      
 1001 FORMAT(A)
      
 1101 FORMAT(A, 'lbrm_boundaryconditions_', A3, '.txt')
 5001 FORMAT('Missing file: ', A)
 5002 FORMAT('Too many boundary conditions in ', A, ' for ', A3, I2.2)
      
      END SUBROUTINE ReadMasterBoundaryFile

      !----------------------------------------------------------------------------      
      !  Add new boundary conditions for a single day (assumed to be the new earliest
      !  date) into the master boundary conditions file for the lake.
      !
      !  Values need to be in CENTIMETERS
      !----------------------------------------------------------------------------      
      SUBROUTINE AddEntriesToMasterBoundaryFile(Bsn, Seq, Storages)
      IMPLICIT NONE
      CHARACTER(LEN=3),       INTENT(IN) :: Bsn
      INTEGER,                INTENT(IN) :: Seq            ! date sequence number
      REAL, DIMENSION(30, 5), INTENT(IN) :: Storages       ! centimeters

      INTEGER :: I, J, U1, Lk, Sub, NumBC
      CHARACTER(LEN=200) :: LakeDir, FName
      INTEGER, DIMENSION(MaxLbrmBC)    :: BDates       ! DateSequence numbers
      REAL,    DIMENSION(MaxLbrmBC, 5) :: BValues      ! Values (USZ, LSZ, GZM, Srf, Snw)
      INTEGER, DIMENSION(30, MaxLbrmBC)    :: AllDates       ! DateSequence numbers
      REAL,    DIMENSION(30, MaxLbrmBC, 5) :: AllValues      ! Values (USZ, LSZ, GZM, Srf, Snw)
      
      !
      !  Working directory name
      !
      LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator
      WRITE(FName, 1101) TRIM(LakeDir), Bsn

      !
      !  Get the existing data.
      !  Future enhancement -- Add a routine to read/store the entire file rather than just
      !  one subbasin. Should be trivial, but I don't feel like doing it ATM.
      !
      Lk = LakeNumberFromName3(Bsn)
      DO Sub = 1, NumSubbasins(Lk)
         CALL ReadMasterBoundaryFile(Bsn, Sub, BDates, BValues, NumBC)
         I = MIN(NumBC, MaxLbrmBC-1)
         AllDates (Sub, 2:I+1)    = BDates(1:I)
         AllValues(Sub, 2:I+1, :) = BValues(1:I, :)
      END DO
      NumBC = I + 1

      !
      !  Assign the new values into location 1.
      !
      DO Sub = 1, NumSubbasins(Lk)
         AllDates (Sub, 1)    = Seq
         AllValues(Sub, 1, :) = Storages(Sub, :)
      END DO
      
      !
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(FName), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1)
      WRITE(U1, 1201, ERR=813)
      WRITE(U1, 1202, ERR=813)
      WRITE(U1, 1203, ERR=813)
      WRITE(U1, 1204, ERR=813)
      WRITE(U1, 1205, ERR=813)
      WRITE(U1, 1206, ERR=813)
      DO Sub = 1, NumSubbasins(Lk)
         DO I = 1, NumBC
            WRITE(U1, 1210, ERR=813) Bsn, Sub, SeqToDateStringYMD(AllDates(Sub,I)), (AllValues(Sub,I,J), J=1,5)
         END DO
         IF (Sub .LT. NumSubbasins(Lk)) WRITE(U1, *, ERR=813) ' '
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening file ' //TRIM(FName);  CALL PassMsg; GOTO 898
  813 ErrorMessage = 'Error writing file ' //TRIM(FName);  CALL PassMsg; GOTO 898
      
  898 ErrorLevel = 1
      ErrorMessage = '[traceback] AddEntriesToMasterBoundaryFile...'; CALL PassMsg
      RETURN

      !
      !  Formats
      !      
 1101 FORMAT(A, 'lbrm_boundaryconditions_', A3, '.txt')
      
 1201 FORMAT('LBRM moisture storage boundary conditions to be used as non-conservative')
 1202 FORMAT('values, overwriting end-of-day values calculated by LBRM.')
 1203 FORMAT('Values are specified in centimeters over the subbasin area.')
 1204 FORMAT('Missing values are denoted by -999.999')
 1205 FORMAT('12 columns per numeric value')
 1206 FORMAT('Bsn, Sub, YYYY-MM-DD,   UpperSoil,   LowerSoil, GroundWater, SurfaceZone, SnowWaterEq')
 1210 FORMAT(A3, ',', I4, ', ', A10, 5(',', F12.3))
      
      END SUBROUTINE AddEntriesToMasterBoundaryFile
      
      !----------------------------------------------------------------------------      
      !  Merge the contents of two LBRM output files. The data in NewFile will
      !  overwrite any simultaneous data in OldFile.
      !  Stipulations:
      !    1. The new data file must start on or before the day after the old file.
      !       No "empty" days are allowed.
      !    2. The merged file will end at the end of the new file. Any data beyond
      !       that date from the old file will be discarded, because the resulting
      !       time series would be discontinuous.
      !    3. If the old data file does not exist, then this is essentially just 
      !       a copy (new -> merged).
      !    4. Data in the new file will overwrite data from the same date in the 
      !       old file.
      !----------------------------------------------------------------------------      
      SUBROUTINE MergeOutputFiles(OldFile, NewFile, MrgFile)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: OldFile, NewFile, MrgFile
      
      INTEGER :: I, J, K, IOS, DT, NumDays
      INTEGER :: Seq, SSeqO, ESeqO, SSeqN, ESeqN, SSeqM, ESeqM
      LOGICAL :: OK, ExistOld, ExistNew
      REAL, DIMENSION(:), ALLOCATABLE :: DatArr, TDat
      
      TYPE (TDlyDataForSubbasin) :: OldData, NewData, MrgData
      TYPE (TDlyData)            :: TDD
      TYPE (TDlyData), POINTER   :: TDDP
      
      INTEGER, DIMENSION(6), PARAMETER :: DataTypesOut = (/                         &
         GDT_Runoff,              GDT_UpperSoilMoisture,   GDT_LowerSoilMoisture,   &
         GDT_GroundWaterMoisture, GDT_SurfaceZoneMoisture, GDT_SnowWater /)
         
      INTEGER, DIMENSION(6), PARAMETER :: DataUnitsOut = (/              &
         GDU_Millimeters, GDU_Millimeters, GDU_Millimeters,              &
         GDU_Millimeters, GDU_Millimeters, GDU_Millimeters /)
         
      !
      !  Initialize the data structures so that automatic finalization
      !  destructor will not crash the program.
      !
      OldData = TDlyDataForSubbasin()
      NewData = TDlyDataForSubbasin()
      MrgData = TDlyDataForSubbasin()
      TDD = TDlyData()
      
      !
      !  Do the two input files exist?
      !  The old file is not required, but the new one is.
      !
      INQUIRE(FILE=TRIM(OldFile), EXIST=ExistOld)
      INQUIRE(FILE=TRIM(NewFile), EXIST=ExistNew)
      IF (.NOT. ExistNew) THEN
         WRITE(ErrorMessage, 5001) TRIM(NewFile);   CALL PassMsg
         GOTO 898
      END IF

      !
      !  Use standard GLSHFS read procedures to read the file(s)
      !
      IF (ExistOld) THEN
         CALL ReadFile_LBRM(TRIM(OldFile), OldData);  IF (ErrorLevel .NE. 0) GOTO 899
      END IF
      CALL ReadFile_LBRM(TRIM(NewFile), NewData);  IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Determine the period of record for the merged file.
      !
      SSeqM = NewData%GetStartDate()
      ESeqM = NewData%GetEndDate()
      IF (ExistOld) SSeqM = MIN(SSeqM, OldData%GetStartDate())
      
      NumDays = ESeqM - SSeqM + 1
      ALLOCATE(DatArr(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5002) NumDays
      END IF
      
      !
      !  Build the merged data set, one data type at a time.
      !
      DO I = 1, 6
         DT = DataTypesOut(I)
         OK = TDD%SetDataType(DataTypesOut(I));  IF (.NOT. OK) GOTO 899
         OK = TDD%SetDataUnit(DataUnitsOut(I));  IF (.NOT. OK) GOTO 899
         IF (ExistOld) THEN
            TDDP => OldData%GetPointerToDataOfType(DT)
            SSeqO = TDDP%GetStartDate()
            ESeqO = TDDP%GetEndDate()
            TDat  = TDDP%GetDataVals()                   ! get a 1-d array of all values 
            DO Seq = MAX(SSeqM, SSeqO), MIN(ESeqM, ESeqO)
               J = Seq - SSeqM  + 1
               K = Seq - SSeqO + 1
               DatArr(J) = TDat(K)
            END DO
         END IF
         TDDP => NewData%GetPointerToDataOfType(DT)
         SSeqN = TDDP%GetStartDate()
         ESeqN = TDDP%GetEndDate()
         TDat  = TDDP%GetDataVals()                      ! get a 1-d array of all values 
         DO Seq = MAX(SSeqM, SSeqN), MIN(ESeqM, ESeqN)
            J = Seq - SSeqM  + 1
            K = Seq - SSeqN + 1
            DatArr(J) = TDat(K)
         END DO
         OK = TDD%AssignData(SSeqM, ESeqM, DatArr);  IF (.NOT. OK) GOTO 899
         OK = MrgData%AddDataSet(TDD);               IF (.NOT. OK) GOTO 899
         CALL TDD%Clear()
      END DO

      !
      !  Write the merged data set
      !  Use Bsn and Subbasin from the *new* dataset, because the
      !  old one may not have existed.
      !
      MrgData%Description = 'Output from LBRM.'
      MrgData%Bsn       = NewData%Bsn
      MrgData%SubNum    = NewData%SubNum
      MrgData%SubArea   = NewData%SubArea
      CALL WriteFile_LBRM(MrgFile, MrgData);    IF (ErrorLevel .NE. 0) GOTO 899
      
      GOTO 999

      !
      !  Error handling
      !
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] MergeOutputFiles...'; CALL PassMsg
      
  999 DEALLOCATE(DatArr, STAT=IOS)
      DEALLOCATE(TDat,   STAT=IOS)
      CALL MrgData%Clear()
      CALL NewData%Clear()
      CALL OldData%Clear()
      RETURN

      !
      !  Formats
      !      
 5001 FORMAT('Required file missing when attempting to merge LBRM output files: ', A)
 5002 FORMAT('Error when allocating memory for LBRM results. Number of days = ', I0)

      END SUBROUTINE MergeOutputFiles


      !----------------------------------------------------------------------------      
      !  Fill in any missing data that is found in MetData.
      !
      !  For purposes of doing something "reasonable", I will be using the long-term
      !  average daily value.  For example, if the TMin value for April 16 is missing,
      !  we will get the average of all valid April 16 TMin values in the file. That 
      !  will be filled in as the value for this missing day.  Note that the ENTIRE
      !  period is used (all of SubMetData), not just the period of direct interest.
      !
      !  This is not a great solution, but it's the best I know to do with the data 
      !  we have.  The main issues:
      !
      !  1) It will force temperatures toward mean values. This might interrupt warm
      !     or cold spells. But what else can we do? 
      !  2) It will likely produce a non-zero precip value for every day it is done.
      !     That is not good, but probably better than randomly choosing between zero
      !     and some kind of scaled value.
      !  If a better approach is devised, please implement it.
      !
      !----------------------------------------------------------------------------      
      
      SUBROUTINE FillMissingData(MetData, SSeq, ESeq, SubMetData)
      IMPLICIT NONE
      REAL, DIMENSION(:,:),         INTENT(INOUT) :: MetData
      INTEGER,                      INTENT(IN)    :: SSeq, ESeq
      TYPE (TDlyDataForSubbasin),   INTENT(IN)    :: SubMetData
      
      INTEGER :: I, J, DC, IOS
      INTEGER :: Dy, Seq, FSSeq, FESeq
      LOGICAL :: MustFix, Miss_TMin, Miss_TMax, Miss_Prec
      
      INTEGER, DIMENSION(366)  :: DlyCnt
      REAL,    DIMENSION(366)  :: DlyTot, DlyAvg
      
      REAL, DIMENSION(:), ALLOCATABLE :: TMet
      TYPE (TDlyData), POINTER        :: TDDP
      
      !
      !  First, find out if we need to even bother with this...
      !
      Miss_Tmin = .FALSE.
      Miss_Tmax = .FALSE.
      Miss_Prec = .FALSE.
      DO Seq = SSeq, ESeq
         I = Seq - SSeq + 1
         IF (IsMissing(MetData(1,I))) Miss_TMin = .TRUE.
         IF (IsMissing(MetData(2,I))) Miss_TMax = .TRUE.
         IF (IsMissing(MetData(3,I))) Miss_Prec = .TRUE.
      END DO
      MustFix = Miss_TMin .OR. Miss_TMax .OR. Miss_Prec
      IF (.NOT. MustFix) RETURN
      
      !
      !  We have to fill in 1 or more TMin values
      !
      IF (Miss_TMin) THEN
         TDDP => SubMetData%GetPointerToDataOfType(GDT_AirtempMin)
         FSSeq = TDDP%GetStartDate()     ! start of data in the file
         FESeq = TDDP%GetEndDate()       ! end of data in the file
         DC    = FESeq - FSSeq + 1       ! compute # days, then use on next line to eliminate compiler warning
         ALLOCATE(TMet(DC), STAT=IOS)
         TMet(1:DC) = TDDP%GetDataVals()
         
         !
         !  Compute the long-term average values for each day of the year
         !
         DlyTot(:) = 0.0
         DlyCnt(:) = 0
         DO Seq = FSSeq, FESeq
            I = Seq - FSSeq + 1
            J = DayOfYear(Seq)
            IF (.NOT. IsMissing(TMet(I))) THEN
               DlyTot(J) = DlyTot(J) + TMet(I)
               DlyCnt(J) = DlyCnt(J) + 1
            END IF
         END DO
         DlyTot(366) = DlyTot(366) + DlyTot(365)
         DlyCnt(366) = DlyCnt(366) + DlyCnt(365)
         DlyAvg(:) = MissingData_Real
         DO J = 1, 366
            IF (DlyCnt(J) .GT. 0) THEN
               DlyAvg(J) = DlyTot(J) / DlyCnt(J)
            END IF
         END DO
         
         DO Seq = SSeq, ESeq
            I = Seq - SSeq + 1
            IF (IsMissing(MetData(1,I))) THEN
               Dy = DayOfYear(Seq)
               MetData(1,I) = DlyAvg(Dy)
            END IF
         END DO      
      END IF
         
      !
      !  We have to fill in 1 or more TMax values
      !
      IF (Miss_TMax) THEN
         TDDP => SubMetData%GetPointerToDataOfType(GDT_AirtempMax)
         FSSeq = TDDP%GetStartDate()     ! start of data in the file
         FESeq = TDDP%GetEndDate()       ! end of data in the file
         TMet  = TDDP%GetDataVals()
         
         !
         !  Compute the long-term average values for each day of the year
         !
         DlyTot(:) = 0.0
         DlyCnt(:) = 0
         DO Seq = FSSeq, FESeq
            I = Seq - FSSeq + 1
            J = DayOfYear(Seq)
            IF (.NOT. IsMissing(TMet(I))) THEN
               DlyTot(J) = DlyTot(J) + TMet(I)
               DlyCnt(J) = DlyCnt(J) + 1
            END IF
         END DO
         DlyTot(366) = DlyTot(366) + DlyTot(365)
         DlyCnt(366) = DlyCnt(366) + DlyCnt(365)
         DlyAvg(:) = MissingData_Real
         DO J = 1, 366
            IF (DlyCnt(J) .GT. 0) THEN
               DlyAvg(J) = DlyTot(J) / DlyCnt(J)
            END IF
         END DO
         
         DO Seq = SSeq, ESeq
            I  = Seq - SSeq + 1
            IF (IsMissing(MetData(2,I))) THEN
               J = DayOfYear(Seq)
               MetData(2,I) = DlyAvg(J)
            END IF
         END DO      
      END IF
         
      !
      !  We have to fill in 1 or more Precip values
      !
      IF (Miss_Prec) THEN
         TDDP => SubMetData%GetPointerToDataOfType(GDT_Precipitation)
         FSSeq = TDDP%GetStartDate()     ! start of data in the file
         FESeq = TDDP%GetEndDate()       ! end of data in the file
         TMet  = TDDP%GetDataVals()
         
         !
         !  Compute the long-term average values for each day of the year
         !
         DlyTot(:) = 0.0
         DlyCnt(:) = 0
         DO Seq = FSSeq, FESeq
            I = Seq - FSSeq + 1
            J = DayOfYear(Seq)
            IF (.NOT. IsMissing(TMet(I))) THEN
               DlyTot(J) = DlyTot(J) + TMet(I)
               DlyCnt(J) = DlyCnt(J) + 1
            END IF
         END DO
         DlyTot(366) = DlyTot(366) + DlyTot(365)
         DlyCnt(366) = DlyCnt(366) + DlyCnt(365)
         DlyAvg(:) = MissingData_Real
         DO J = 1, 366
            IF (DlyCnt(J) .GT. 0) THEN
               DlyAvg(J) = DlyTot(J) / DlyCnt(J)
            END IF
         END DO
         
         DO Seq = SSeq, ESeq
            I  = Seq - SSeq + 1
            IF (IsMissing(MetData(3,I))) THEN
               J = DayOfYear(Seq)
               MetData(3,I) = DlyAvg(J)
            END IF
         END DO      
      END IF
      
      GOTO 999   
         
!  899 ErrorMessage = '[traceback] FillMissingData...';   CALL PassMsg
!      GOTO 999
         
  999 DEALLOCATE(TMet, STAT=IOS)
      RETURN 
      
      END SUBROUTINE FillMissingData
      
      
      !----------------------------------------------------------------------------      
      !  READ an LBRM output file (GLSHFS format), using the standard GLSHFS 
      !  read procedure for a TDlyDataForSubbasin object.
      !  Store the data in a TLbrmOutputData structure.
      !----------------------------------------------------------------------------      
      SUBROUTINE ReadLBRMFile(FileName, LData)
      IMPLICIT NONE
      CHARACTER(LEN=*),                  INTENT(IN)    :: FileName
      TYPE (TLbrmOutputData), INTENT(INOUT) :: LData
      
      TYPE (TDlyDataForSubbasin) :: TDFS
      
      !
      !  Initialize/clear the object
      !
      TDFS = TDlyDataForSubbasin();  IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Use standard GLSHFS procedures to read the data
      !
      CALL ReadFile_LBRM(TRIM(FileName), TDFS);  IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Copy the data from the TDlyDataForSubbasin object into the TLbrmData structure.
      !
      CALL CopyData_TDFS_LBRM(TDFS, LData);  IF (ErrorLevel .NE. 0) GOTO 899
      CALL TDFS%Clear()
      RETURN
      
      !
      !  Error handling
      !
  899 ErrorMessage = '[traceback] : ReadLBRMFile...'
      CALL PassMsg
      RETURN
 
      END SUBROUTINE ReadLBRMFile

      !----------------------------------------------------------------------------      
      !  WRITE an LBRM output file (GLSHFS format).
      !----------------------------------------------------------------------------      
      SUBROUTINE WriteLBRMFile(FileName, LData)
      IMPLICIT NONE
      CHARACTER(LEN=*),                  INTENT(IN) :: FileName
      TYPE (TLbrmOutputData), INTENT(IN) :: LData
      
      INTEGER :: SSeq, ESeq
      TYPE (TDlyDataForSubbasin) :: TDFS
      
      !
      !  Initialize/clear the object
      !
      TDFS = TDlyDataForSubbasin();  IF (ErrorLevel .NE. 0) GOTO 899
     
      !
      !  Copy the data from the TLbrmData structure into the TDlyDataForSubbasin object.
      !
      CALL CopyData_LBRM_TDFS(LData, TDFS);  IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Use standard GLSHFS procedure to write the file
      !  SSeqReq is set to the day after the end of the data
      !
      SSeq = LData%SDateSeq
      ESeq = LData%EDateSeq
      CALL WriteFile_LBRM(TRIM(FileName), TDFS, SSeq, ESeq);  IF (ErrorLevel .NE. 0) GOTO 899
      CALL TDFS%Clear()
      RETURN
      
      !
      !  Error handling
      !
  899 ErrorMessage = '[traceback] : WriteLBRMFile...'
      CALL PassMsg
      RETURN
 
      END SUBROUTINE WriteLBRMFile
      

      !----------------------------------------------------------------------------      
      !  Copy data from a TDlyDataForSubbasin object into a 
      !  TLbrmOutputData structure.
      !----------------------------------------------------------------------------      
      SUBROUTINE CopyData_TDFS_LBRM(TDFS, LOutput)
      IMPLICIT NONE
      TYPE (TDlyDataForSubbasin),        INTENT(IN)  :: TDFS
      TYPE (TLbrmOutputData), INTENT(OUT) :: LOutput

      INTEGER :: I, IOS, NDays, DT
      REAL, DIMENSION(:), ALLOCATABLE :: TDat
      TYPE (TDlyData), POINTER :: TDDP
      
      INTEGER, DIMENSION(6), PARAMETER :: DataTypesOut = (/              &
         GDT_Runoff,              GDT_UpperSoilMoisture,   GDT_LowerSoilMoisture,                &
         GDT_GroundWaterMoisture, GDT_SurfaceZoneMoisture, GDT_SnowWater /)
      INTEGER, DIMENSION(6), PARAMETER :: DataUnitsOut = (/              &
         GDU_CubicMeters, GDU_CubicMeters, GDU_CubicMeters,              &
         GDU_CubicMeters, GDU_CubicMeters, GDU_CubicMeters /)
    
      !
      IF (ALLOCATED(LOutput%Values)) DEALLOCATE(LOutput%Values)
      
      LOutput%Bsn      = TDFS%Bsn
      LOutput%Subbasin = TDFS%SubNum
      LOutput%Area     = TDFS%SubArea
      LOutput%SDateSeq = TDFS%GetStartDate()
      LOutput%EDateSeq = TDFS%GetEndDate()
      IF (LOutput%SDateSeq .EQ. MissingData_Date) RETURN
      IF (LOutput%EDateSeq .EQ. MissingData_Date) RETURN
      
      NDays = LOutput%EDateSeq
      IF (NDays .LT. 1) THEN
         WRITE(ErrorMessage, 5001) NDays;   CALL PassMsg
         GOTO 898
      END IF
      ALLOCATE(LOutput%Values(6,NDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5002) NDays;   CALL PassMsg
         GOTO 898
      END IF

      DO I = 1, 6
         DT = DataTypesOut(I)
         TDDP => TDFS%GetPointerToDataOfType(DT); IF (ErrorLevel .NE. 0) GOTO 899
         TDat = TDDP%GetDataVals()
         LOutput%Values(I,:) = TDat(:)
      END DO
      
      GOTO 999

      !
      !  Error handling
      !
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : CopyData_TDFS_LBRM...';  CALL PassMsg
      IF (ALLOCATED(LOutput%Values)) DEALLOCATE(LOutput%Values, STAT=IOS)
      
  999 IF (ALLOCATED(TDat)) DEALLOCATE(TDat, STAT=IOS)
      RETURN 
      
 5001 FORMAT('Invalid number of days of data.  NumDays=', I0)
 5002 FORMAT('Error attempting to allocate memory for LBRM output data.  NumDays=', I0)

      END SUBROUTINE CopyData_TDFS_LBRM
      

      !----------------------------------------------------------------------------      
      !  Copy data from an LbrmData structure into a TDlyDataForSubbasin object.
      !  The TDlyDataForSubbasin object MUST be initialized/constructed prior to
      !  calling this routine.
      !----------------------------------------------------------------------------      
      SUBROUTINE CopyData_LBRM_TDFS(LOutput, TDFS)
      IMPLICIT NONE
      TYPE (TLbrmOutputData), INTENT(IN)    :: LOutput
      TYPE (TDlyDataForSubbasin),        INTENT(INOUT) :: TDFS

      INTEGER :: I, IOS, NDays, SSeq, ESeq
      LOGICAL :: OK
      REAL, DIMENSION(:), ALLOCATABLE :: TDat
      TYPE (TDlyData)  :: TDD

      INTEGER, DIMENSION(6), PARAMETER :: DataTypesOut = (/              &
         GDT_Runoff,              GDT_UpperSoilMoisture,   GDT_LowerSoilMoisture,                &
         GDT_GroundWaterMoisture, GDT_SurfaceZoneMoisture, GDT_SnowWater /)
      
      !
      TDD  = TDlyData()
      CALL TDFS%Clear()
      
      !
      !  Verify valid date range info
      !
      SSeq = LOutput%SDateSeq
      ESeq = LOutput%EDateSeq
      OK = .TRUE.
      IF (SSeq .EQ. MissingData_Date) OK = .FALSE.
      IF (ESeq .EQ. MissingData_Date) OK = .FALSE.
      IF (.NOT. OK) THEN
         WRITE(ErrorMessage, 5001);   CALL PassMsg
         GOTO 898
      END IF
      NDays = ESeq - SSeq + 1
      IF (NDays .LT. 1) THEN
         WRITE(ErrorMessage, 5002) NDays;   CALL PassMsg
         GOTO 898
      END IF
      
      !
      !
      OK = TDFS%SetDescription('Output from LBRM.');   IF (.NOT. OK) GOTO 899
      TDFS%Bsn         = LOutput%Bsn
      TDFS%SubNum      = LOutput%Subbasin
      TDFS%SubArea     = LOutput%Area
      TDFS%NewDataSSeq = MissingData_Date         ! unused for LBRM output
      
      DO I = 1, 6
         OK = TDD%SetDataType(DataTypesOut(I));                 IF (.NOT. OK) GOTO 899
         OK = TDD%SetDataUnit(GDU_CubicMeters);                 IF (.NOT. OK) GOTO 899
         OK = TDD%AssignData(SSeq, ESeq, LOutput%Values(I,:));  IF (.NOT. OK) GOTO 899
         OK = TDFS%AddDataSet(TDD);                             IF (.NOT. OK) GOTO 899
         CALL TDD%Clear()
      END DO

      GOTO 999

      !
      !  Error handling
      !
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : CopyData_LBRM_TDFS...';  CALL PassMsg
      CALL TDFS%Clear()
      CALL TDD%Clear()
      
  999 IF (ALLOCATED(TDat)) DEALLOCATE(TDat, STAT=IOS)
      RETURN 
      
 5001 FORMAT('Start and/or End date(s) is missing.')
 5002 FORMAT('Invalid number of days of data.  NumDays=', I0)

      END SUBROUTINE CopyData_LBRM_TDFS
      
!------------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Determine what the forecast start date should be when the user specified
      !  that it is to be at the end of the historical data.
      !------------------------------------------------------------------------
      FUNCTION FcstStartAtEndOfData()     Result(FcSeq)
      IMPLICIT NONE
      INTEGER :: FcSeq
      INTEGER :: I, U1, MinESeq
      CHARACTER(LEN=3)   :: Bsn
      CHARACTER(LEN=200) :: LakeDir, FName
      INTEGER, DIMENSION(7) :: LkSSeq, LkESeq
      TYPE(THeaderInfoType) :: Hdr
      
      FcSeq = MissingData_Date
      !
      !  Create/initialize the Hdr object used when reading the overlake 
      !  meteorology "subbasin" file.
      !
      Hdr = THeaderInfoType()
      
      !
      !  Get data period from each lake, only looking at the subdata_xxx01.csv file
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
            ErrorMessage = 'Historical land meteorology data sets do not overlap sufficiently.'; CALL PassMsg
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

 1502 FORMAT(A, 'subdata_', A3, '01.csv')
 
      END FUNCTION FcstStartAtEndOfData
      
      
      
      
      
      
END MODULE GLSHFS_LBRM
