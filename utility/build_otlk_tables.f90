MODULE OutlookTables
      USE ErrorProcess
      USE StatusMessages
      USE GLSHFS_Util
      USE GLSHFS_Global
      USE GL_Constants
      USE GlerlDataTypesAndUnits
      USE DailyDataStructures
      USE GLSHFS_StructuredData
      USE GLSHFS_ConfigFile
      USE GLSHFS_Files

      !
      !  Define the quantile percentages.
      !  Would be nice to make this a variable rather than hardcoded, but 
      !  this is much simpler for now.
      !
      REAL, DIMENSION(9), PARAMETER :: QntlPrcnt =                        &
           (/0.05, 0.10, 0.20, 0.30, 0.50, 0.70, 0.80, 0.90, 0.95/)

      !
      !  Set maximum number of each type of entry in the historic data array
      !  Note that the historic data is only used to "backfill" to the beginning of
      !  the specified forecast period (month, week, etc), so we will never have 
      !  to go back more than 1 year.
      !
      INTEGER, PARAMETER :: MaxHistYears = 1 
      INTEGER, PARAMETER :: MaxHistMons  = MaxHistYears * 12
      INTEGER, PARAMETER :: MaxHistQMons = MaxHistMons  * 4
      INTEGER, PARAMETER :: MaxHistWeeks = MaxHistYears * 53
      INTEGER, PARAMETER :: MaxHistDays  = MaxHistYears * 366

      !
      !  Set maximum number of each type of entry in the outlook data array
      !
      INTEGER, PARAMETER :: MaxOtlkYears = MaxForecastLengthInYears
      INTEGER, PARAMETER :: MaxOtlkMons  = MaxForecastLengthInMonths
      INTEGER, PARAMETER :: MaxOtlkQMons = MaxOtlkMons  * 4
      INTEGER, PARAMETER :: MaxOtlkWeeks = MaxOtlkYears * 53
      INTEGER, PARAMETER :: MaxOtlkDays  = MaxForecastLengthInDays

      !
      INTEGER, PARAMETER :: MaxOtlks = 117       ! max # of outlook years (scenarios)
      INTEGER, PARAMETER :: DT       = 29        ! max # of data types in output
      INTEGER, PARAMETER :: MaxTotDays  = MaxHistDays + MaxOtlkDays

      !
      !  Data types to process
      !
      INTEGER, DIMENSION(29), PARAMETER :: DataTypesToProcess     =                    &
        (/  GDT_OverLakeRunoff,      GDT_OverLakePrecip,    GDT_Evaporation,           &
            GDT_NetBasinSupply,      GDT_OverLandAirTemp,   GDT_WaterTemp,             &
            GDT_OverLakeAirTemp,     GDT_VaporPressure,     GDT_WindSpeed,             &
            GDT_CloudCover,          GDT_IncidentRad,       GDT_ReflectedRad,          &
            GDT_NetLongWaveRad,      GDT_LatentRad,         GDT_SensibleRad,           &
            GDT_NetSurfaceRad,       GDT_IceTemp,           GDT_IceCover,              &
            GDT_TotalHeat,           GDT_TotalSoilMoisture, GDT_GroundwaterMoisture,   &
            GDT_SurfaceZoneMoisture, GDT_SnowWater,         GDT_TotalMoisture,         &
            GDT_EndLakeLevel,        GDT_MeanLakeLevel,     GDT_Inflow,                &
            GDT_Outflow,             GDT_TotalBasinSupply /)
            
            

      !
      !  What type of value is output for each data type?  
      !  Total         = 1
      !  Average       = 2
      !  End-of-period = 3
      !
      INTEGER, PARAMETER, DIMENSION(29) :: OutputType =     &
        (/ 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,     &
           2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2/)

!
!  Is the data allowed to be negative?  If not we will force all
!  negative values to zero.
!
      LOGICAL, PARAMETER, DIMENSION(DT) :: AllowNeg =                    &
     &  (/.FALSE., .FALSE.,  .TRUE.,  .TRUE.,  .TRUE.,                   &  ! run,prc,evp,nbs,air
     &    .FALSE.,  .TRUE., .FALSE., .FALSE., .FALSE.,                   &  ! wtp,lka,vap,wnd,cld
     &     .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,  .TRUE.,                   &  ! inc,rfl,nlw,lat,sen
     &     .TRUE.,  .TRUE., .FALSE., .FALSE., .FALSE.,                   &  ! nsf,ist,ice,hts,slm
     &    .FALSE., .FALSE., .FALSE., .FALSE., .FALSE.,                   &  ! grm,srf,snw,tlm,elv
     &    .FALSE., .FALSE., .FALSE.,  .TRUE./)                              ! mlv,inf,otf,tbs


   CONTAINS

!-------------------------------------------------------------------------------------------   
      !-------------------------------------------------------------------------------------
      SUBROUTINE BuildOutlookTables(Bsn, TimeStep, WeightsFileName)
      IMPLICIT  NONE
      CHARACTER (LEN=3), INTENT(IN) :: Bsn
      INTEGER,           INTENT(IN) :: TimeStep        ! GTS_Annual, GTS_Monthly, etc
      CHARACTER (LEN=*), INTENT(IN) :: WeightsFileName
      
      
      
      CHARACTER(LEN=4)    :: SName
      CHARACTER (LEN=200) :: LakeDir, OtlkDir

      TYPE (THeaderInfoType)     :: HdrInfo
      TYPE (TDlyDataForSubbasin) :: SubData
      
      
      
      !
      !  Initialization
      !
      HdrInfo = THeaderInfoType()
      SubData = TDlyDataForSubbasin()           ! create object so automatic Finalize() doesn't throw error
      
      
      !
      !  Check for existence of, and then attempt to read, the specified weights file
      !
      INQUIRE(FILE=TRIM(WeightsFileName), EXIST=FExist)
      IF (.NOT. FExist) THEN
         ErrorMessage = 'Error: Missing the specified scenario weights file: '//TRIM(WeightsFileName);  CALL PassMsg
         GOTO 899
      END IF

      CALL ReadTheWeights(WeightsFileName, ScenNames, Weights, NumWeights); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  The directories of interest for this lake
      !
      LakeDir = TRIM(GLSHFS_Config%BaseDir) // Bsn // FilePathSeparator
      OtlkDir = TRIM(GLSHFS_Config%BaseDir) // TRIM(GLSHFS_Config%ForecastName) // FilePathSeparator
      
      !
      !  Get a lake number from the Bsn value. Useful for subsequent operations where
      !  we need to access the various global info arrays like number of subbasins, etc.
      !
      LkNum = LakeNumberFromName3(Bsn)
      
      !
      !  Now that we have the 4-character scenario names and their associated weights,
      !  we need to read the various files that contain the scenario data.
      !
      !  We will have to read them twice:
      !  First time is just to read the header info and get forecast dates.
      !  Then, after we have allocated the necessary memory, we can read the files
      !  a second time. This time we will actually read/store the data values.
      !
      DO W = 1, NumWeights
         SName = ScenNames(W)
         
         !
         !  LBRM output files for this scenario
         !
         DO Sub = 1, NumSubbasins(LkNum)
            WRITE(FName, 1010) TRIM(OtlkDir), Bsn, Sub, SName
            CALL ReadJustHeaderInfo(FName, HFT_LBRM, HdrInfo);   IF (ErrorLevel .NE. 0) GOTO 899
            
            
            
            
            CALL ReadFile_LBRM(TRIM(FName), SubData);   IF (ErrorLevel .NE. 0) GOTO 899

         END DO
      
      END DO

      !
      !
 
 898  ErrorLevel = 1
 899  ErrorMessage = '[traceback] BuildOutlookTables...';    CALL PassMsg
      RETURN

 1001 FORMAT(A)
 1002 FORMAT(2X, A4, F10.6)
 1010 FORMAT(A, 'lbrm_output_', A3, I2.2, '_', A4, '.csv'


      END SUBROUTINE BuildOutlookTables


!------------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Routine to input the scenario weights from a file such
      !  as OTLKWGTS.xxx
      !------------------------------------------------------------------------
      SUBROUTINE ReadTheWeights(Filename, ScIDs, Weights, NumWgts)
      IMPLICIT NONE
      CHARACTER(LEN=*),               INTENT(IN)    :: Filename
      CHARACTER(LEN=4), DIMENSION(:), INTENT(INOUT) :: ScIDs
      REAL,             DIMENSION(:), INTENT(INOUT) :: Weights
      INTEGER,                        INTENT(INOUT) :: NumWgts

      INTEGER :: K, L, U1, IOS
      INTEGER :: MaxNumWgts, MaxNI, MaxNW
      REAL    :: Wt
      CHARACTER (LEN=4)   :: ScName
      CHARACTER (LEN=99)  :: Line
      CHARACTER (LEN=150) :: FileName

      !
      !  Open the input file
      !
      U1 = GetFreeUnitNumber();  IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(FileName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Read the header lines.
      !
      READ(U1, 1001, ERR=812) Line
      K = INDEX(Line, 'Maximum)')
      IF (K .GT. 0) THEN
         ErrorMessage = '     Error!!! When the outlook weights file,';                             CALL PassMsg
         ErrorMessage = '     '//TRIM(FileName);                                                    CALL PassMsg
         ErrorMessage = '     was created, the program was unable to find a minimum';               CALL PassMsg
         ErrorMessage = '     solution to the problem.  It indicated this in the';                  CALL PassMsg
         ErrorMessage = '     header of the file by putting MAXIMUM at the end of the first line.'; CALL PassMsg
         ErrorMessage = '     The weights in the file are not valid.';                              CALL PassMsg
         ErrorMessage = '     Please create a new weights file and rerun the forecast.';            CALL PassMsg
         CLOSE(U1)
         CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899
         GOTO 898
      END IF
      READ(U1, *, ERR=812)

      !
      !  What is the max allowed number of entries?
      !
      MaxNI = UBOUND(ScIDs, 1)
      MaxNW = UBOUND(Weights, 1)
      MaxNumWgts = MIN(MaxNI, MaxNW)
      
      !
      !
      K = 0
      READ(U1, 1002, IOSTAT=IOS) ScName, Wt
      DO WHILE ((IOS .EQ. 0) .AND. (K .LT. MaxNumWgts))
         IF ((Yr .GE. 1000) .AND. (Yr .LE. 2999) .AND. (Wt .GE. 0.0)) THEN      ! TSH 25apr2003
            K = K + 1
            ScIDs(K)   = ScName
            Weights(K) = Wt
         END IF
         READ(U1, 1002, IOSTAT=IOS) ScName, Wt
      END DO
      CLOSE (U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      NumWgts = K
      RETURN

      !
      !
 811  ErrorMessage = 'Error opening file ' // TRIM(FileName);     CALL PassMsg;  GOTO 898
 812  ErrorMessage = 'Error reading file ' // TRIM(FileName);     CALL PassMsg;  GOTO 898

 898  ErrorLevel = 1
 899  ErrorMessage = '[traceback] ReadTheWeights...';    CALL PassMsg
      RETURN

 1001 FORMAT(A)
 1002 FORMAT(2X, A4, F10.6)

      END SUBROUTINE ReadTheWeights

!------------------------------------------------------------------------
!     This routine sorts the values in array A from smallest to largest and
!     also arranges the corresponding values in array B in the same order.
!     Thus, B is not sorted based on its own contents but rather by the 
!     contents of A.  Each element of B just "tags along" with it's partner
!     element in A and the final state of B will, in general, appear to
!     be unsorted.
!
      SUBROUTINE SortTwo (A, B, Number)
      IMPLICIT NONE
      REAL   , INTENT(INOUT) :: A(:)
      INTEGER, INTENT(INOUT) :: B(:), Number

      INTEGER ::  I, J, K, L, M, P
      REAL    ::  X

      P = Number
      DO WHILE (P .GT. 0)
        K = Number - P
        J = 1
        DO WHILE (J .LE. K)
          I = J
          M = I + P
          DO WHILE ((I .GT. 0) .AND. (A(I) .GT. A(M)))
            X = A(I)
            A(I) = A(M)
            A(M) = X

            L = B(I)
            B(I) = B(M)
            B(M) = L

            M = I
            I = I - P
          END DO
          J = J + 1
        END DO
        P = P / 2
      END DO
      RETURN

      END SUBROUTINE SortTwo

      
