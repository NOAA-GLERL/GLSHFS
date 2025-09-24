!
!     Program to calculate daily overlake evaporation, for use in the FORECAST
!     PACKAGE, by using a zero-dimensional heat budget and a one-dimensional
!     heat storage model.  Version: 08feb2013.
!
!     Heat balance, heat storage function, and program by TEC II
!        development during 7/87-7/88 reported in WRR 25(5):781-792.
!        further developed 1/89-6/89, 1/90, 5/90, 11/90, 2/91.
!        development during 8/88-2/91 reported in WRR 28(1):69-81.
!        further developed 5/91, 6/91, 8/91-2/92 (daily ice computations).
!        development during 5/91-2/92 reported in WRR 30(3):625-639.
!
!     4-byte date handling and move to UNIX, TSH & TEC II 14oct92.
!     Missing data handling by TSH 31dec92, 11may93.
!     Unused parameters removed by TEC II 15jun94.
!     Updated by TEC II 21mar95 for change in BSNNM structure (addition of
!      forecast start date in record 5 and displacement of later records by
!      one to accomodate the addition) and to suppress writing of status info
!      if file: "QUIET.INT" exists in default directory.
!     Updated by TEC II 1aug95 to input/output ice cover as a decimal fraction
!      of the lake area rather than as an area; also use this to compute
!      "IceAmount" instead of reading in this (redundant) info.
!     Updated by TSH 23oct97 to handle ALLTEMPS.xxx file more cleanly and
!      correctly.
!     Translated to FORTRAN95 by TECII 30mar01.
!     Added capability to compute temperature-depth profiles by TEC 24apr01.
!     Restructured to improve modularity and allow integration into AHPS
!      by TSH 08feb2013.
!     StoredHeatAtEndOfDay calculation tweaked on 02Dec2013 by TSH.
!
!     New stand-alone version by Tim Hunter, February 2015.  This version has
!     numerous changes from the original XEVAPOUT used in AHPS.
!       1) It does not rely on AHPS-specific input files like EBSNNM, etc.
!       2) It uses plain-text files for both the input meteorology and the 
!          output files. Specifically, it uses CSV-format files for input met 
!          and the output data. 
!       3) The various "modes" of operation (setup, provisional, forecast)
!          are eliminated. 
!       4) Note that the parameter file format is left consistent with the AHPS 
!          package (for now, anyway). But only PARAMEVP.xxx is valid.
!
!
!     Evaporation theory by Frank Quinn (WRR 15(4):935-940)
!        modified by TEC II from "JDEVAP.FOR", 3/87
!     Earlier versions revised, debugged, and/or tested
!        by TEC II 5/87 - 7/88
!        by TSH 12/87, 6/89, 7/90, 2/91
!        by Cynthia Sellinger 3/88
!        by Forrest McLaughlin 5/87 - 7/87
!        by Lauri Huff 6/90
!
!     Input files: eBSNNM, PARAMEVP.xxx, EVPPARAM.xxx, xxxMD.CLM, xxxMD.PRV,
!     ZSEVAPPR.xxx, ALLTEMPS.xxx, MET_DATA.xxx, and DATEFILE.INT
!     where "xxx" denotes the 3 letter lake abbrev. - SUP, ERI, ...
!
!     Output files: ZSEVAP.xxx, ZSEVAPPR.xxx, ZSTEMP.xxx, ZSEVyyyy.INT,
!     PARAMEVP.xxx, EVPPARAM.xxx, and eBSNNM.
!
!     The parameter files for this program, PARAMEVP.xxx and EVPPARAM.xxx (xxx
!     = 'SUP', 'MIC', 'HUR', 'NHU', 'GEO', 'STC', 'ERI', 'ONT'), and
!     EVPPRMzz.INT (zz = 2-digit outlook year specifier, e.g., 48, 49, ...),
!     are formatted, sequential access files with the following structure:
!
!     1st record:      Ext Comment           FORMAT(A3, A77)
!                      | | |     |
!                      | | |_____|___documentation comment
!                      |_|_________string denoting provisional ('PRV') or
!                                  climatic ('CLM') data file to use
!
!     2nd through 14th records: evaporation parameters 1 - 13 respectively
!                               FORMAT(E10.4E1)
!
!     15th record:     Day Month Year        FORMAT(2I3, I5)
!                      | | |   | |  |
!                      | | |   | |__|___year of simulation start date
!                      | | |___|______month of simulation start date
!                      |_|__________day of simulation start date
!
!     16th record:     Day Month Year        FORMAT(2I3, I5)
!                      | | |   | |  |
!                      | | |   | |__|___year of simulation end date
!                      | | |___|______month of simulation end date
!                      |_|__________day of simulation end date
!
!     17th record:     SurfaceTemperature    FORMAT(E13.7E2)
!                      |
!                      |___initial water surface temperature, degrees Celcius
!
!     18th record:     IceAmount             FORMAT(E13.7E2)  NOT USED!!!
!                      |                                      NOT USED!!!
!                      |___initial ice cover, cubic meters    NOT USED!!!
!
!     19th record:     IceArea               FORMAT(E13.7E2)
!                      |
!                      |___initial ice area, decimal fraction of lake area
!
!     20th record:     IceDepth              FORMAT(E13.7E2)
!                      |
!                      |___initial ice depth, meters
!
!     21st record:     IceTemp               FORMAT(E13.7E2)
!                      |
!                      |___initial ice pack temperature, deg.C
!
!     22nd record:     NumberOfBlocks        FORMAT(E13.7E2)  NOT USED!!!
!                      |                                      NOT USED!!!
!                      |___initial number of ice blocks       NOT USED!!!
!
!     23rd record:     AgeOfCurrentLayer     FORMAT(E13.7E2)
!                      |
!                      |___wind history of the current layer (on first day of
!                          simulation), meters/sec
!
!     24th record:     NoOfLayers            FORMAT(I11)
!                      |
!                      |___number of layers in the heat superposition model on
!                          the first day of the simulation
!
!     25th through 24+3*NoOfLayers records: heat storage parameters for all
!                                           layers in the heat superposition
!                                           model on the first day of the
!                                           simulation; each record has the
!                                           FORMAT(E13.7E2)
!
!     NOTE THAT RECORDS 17 THROUGH 24 + 3 * NoOfLayers ARE FILLED IN BY A
!     PREVIOUS RUN OF THIS PROGRAM WITH AN END SIMULATION DATE ON THE DAY
!     BEFORE THE START SIMULATION DATE IN THIS PARAMETER FILE.  THESE REPRESENT
!     INITIAL CONDITIONS FOR THE PROGRAM SIMULATION AND ARE ARBITRARY FOR A
!     LONG SIMULATION WHERE THE INTEREST IS ONLY IN THE LATTER PART OF THE
!     SIMULATION.
!
!------------------------------------------------------------------------------

MODULE LLTM
      USE ErrorProcess
      USE GL_Constants
      USE GLSHFS_Global
      USE GLSHFS_Util
      USE GLSHFS_StructuredData
      USE DailyDataStructures
      USE GlerlDataTypesAndUnits

      !
      !  Make EVERYTHING private to this module, unless we explicitly
      !  declare it to be public.
      !
      PRIVATE
      PUBLIC :: DO_LLTM
      PUBLIC :: TLayerInfo, TLayerOneDay
      PUBLIC :: MaxNoOfLayers
      PUBLIC :: WriteHeatLayerFile_LLTM

      !
      !  Global constants
      !  Order of lakes for things ordered that way is:
      !     SUP, MIC, HUR, GEO, STC, ERI, ONT
      !      
      INTEGER, PARAMETER :: MaxNoOfLayers = 750
      INTEGER, PARAMETER :: NumLakes      = 7

      REAL, DIMENSION(NumLakes), PARAMETER :: AtmosphericPressure =       &
          (/     991.80,     992.58,     992.58,     992.58,              &
                 992.75,     992.86,     1004.53/)

      REAL, DIMENSION(NumLakes), PARAMETER :: AirDensity =                &
          (/   0.001204,   0.001205,   0.001205,   0.001205,              &
               0.001205,   0.001205,   0.001216/)
               
      REAL, DIMENSION(NumLakes), PARAMETER :: DepthVolumeA =              &
           (/  92666.,  26534.,  2622.0,  18937.,  3361.0,                &
             .56667e9,  600.34/)

      REAL, DIMENSION(NumLakes), PARAMETER :: DepthVolumeB =              &
           (/ 3.11256, 3.37654, 3.86907, 3.45945, 3.77684,                &
                   1., 4.93110/)

      REAL, DIMENSION(NumLakes), PARAMETER :: DepthVolumeM =              &
           (/    405.,    281.,    229.,    229.,    164.,                &
                   6.,     64./)


      INTEGER, DIMENSION(6), PARAMETER :: ReqType =                       &
         (/ GDT_AirTempMean, GDT_DewPointMean, GDT_WindSpeed,             &
            GDT_CloudCover,  GDT_IncidentRad,  GDT_NetLongWaveRad /)
      
      INTEGER, DIMENSION(6), PARAMETER :: ReqUnit =                       &
         (/ GDU_Celsius,  GDU_Celsius,     GDU_MetersPerSecond,           &
            GDU_Percent,  GDU_WattsPerM2,  GDU_WattsPerM2 /)
      
      !
      !  Info in the config file
      !  RadiationMethod specifies how to compute the Incident and Net 
      !  Longwave radiation values.
      !  1 = Read cloudcover and compute both Incident and NetLW.
      !      This is the historical method that allows us to use the data
      !      most readily available at station locations.
      !  2 = Read Incident and Longwave radiation directly. The Longwave radiation
      !      is interpreted as INCOMING longwave. Net longwave will be computed
      !      by LLTM.
      !      This is the option that we need to choose when using the
      !      output from CMIP models. The NetLW radiation that comes out of
      !      those models apparently does not properly account for the lake, 
      !      and the net value that is calculated and output is significantly
      !      wrong.  Seasonality is reversed, etc.
      !  3 = Read Incident and Longwave directly. Treat the longwave radiation
      !      as a correct computation of the net value and use both radiation 
      !      values as direct inputs.
      !      This will probably be used only when we are using output from
      !      some sort of coupled atmosphere-water model that correctly 
      !      computes the net LW value.
      !
      !  ****** IMPORTANT CAUTION ******
      !  The LLTM and associated data files were designed with the assumption
      !  that we would be able to handle LW radiation as described in option 3.
      !  Thus, the longwave data is labelled as "Net" longwave radiation. Only
      !  after getting far along in the process did it come to light that the
      !  CMIP net LW values were problematic.  In the interest of trying to
      !  adhere closer to the target deadline (already pushed back a few times)
      !  for completion of a working version of GLSHFS, I am leaving the code
      !  with minimal revisions. At some future point, it would be good to 
      !  revisit this issue, perhaps adding an additional longwave radiation 
      !  column.   -Tim Hunter,  15May2017
      !  ********************************
      !
      TYPE TEvapConfig
         CHARACTER (LEN=150) :: RunID                   ! optional string describing this run
         CHARACTER (LEN=150) :: ParmFile
         CHARACTER (LEN=150) :: MetFile, MissDataFile
         CHARACTER (LEN=150) :: OutputFile
         CHARACTER (LEN=150) :: LayerFile
         CHARACTER (LEN=150) :: BulkFile
         CHARACTER (LEN=150) :: TDepthFile
         LOGICAL             :: ApplyOverwaterCorrection
         LOGICAL             :: WriteBulkTransferFile
         LOGICAL             :: WriteTempDepthProfile
         INTEGER             :: RadiationMethod         
      END TYPE TEvapConfig

      !
      !  This type contains the information read from the parameter file.
      !
      TYPE TEvapParms
         CHARACTER (LEN=3)  :: Bsn
         CHARACTER (LEN=99) :: Comments(3)
         REAL    :: Parms(13)                   !  1-10 are relevant, 11-13 are unused
         INTEGER :: SSeq, ESeq
         REAL    :: SurfTemp              ! deg C
         REAL    :: IceArea               ! fraction (0.0 - 1.0)
         REAL    :: IceDepth              ! meters
         REAL    :: IceTemp               ! deg C
         REAL    :: AgeOfCurrentLayer     
         INTEGER :: NumLayers
         REAL, DIMENSION(1:MaxNoOfLayers) :: AgeOfLayer
         REAL, DIMENSION(0:MaxNoOfLayers) :: SurfTempFromLayer, TotalHeat
      END TYPE TEvapParms

      !
      !  TLayerOneDay is a container for the heat layer information
      !  from a single day. We will be using these to build/update
      !  a file with the history of the layer info.
      !
      TYPE TLayerOneDay
         INTEGER :: Seq
         REAL    :: SurfTemp              ! deg C
         REAL    :: IceArea               ! fraction (0.0 - 1.0)
         REAL    :: IceDepth              ! meters
         REAL    :: IceTemp               ! deg C
         REAL    :: AgeOfCurrentLayer
         INTEGER :: NumLayers
         REAL, DIMENSION(1:MaxNoOfLayers) :: AgeOfLayer
         REAL, DIMENSION(0:MaxNoOfLayers) :: SurfTempFromLayer, TotalHeat
      END TYPE TLayerOneDay
      
      TYPE TLayerInfo
         CHARACTER (LEN=3) :: Bsn
         INTEGER :: SSeq, ESeq
         INTEGER :: NumDays
         TYPE (TLayerOneDay), DIMENSION(:), ALLOCATABLE :: Dly
      END TYPE TLayerInfo

      !
      !  This type is a container for the forcing meteorology info.
      !  It contains the met (and radiation) values that are passed
      !  to the BalanceElevAndTemp() routine.
      !
      !  Note that the values assigned into this structure just prior
      !  to calling that routine are the filled-in values.
      !  Then, during the routine, adjustments may be made to these
      !  values (overwater corrections, etc).  The new values will
      !  be passed back from the Balance() routine.
      !
      TYPE TMetData
         REAL    :: AirTemp, DewPt      ! deg C
         REAL    :: WindSpeed           ! m/s
         REAL    :: Cloud               ! %
         REAL    :: Incident, NetLW     ! watts/m2
      END TYPE TMetData
      
      !
      !  Declare two global variables that will contain info that keeps things
      !  a little bit easier for the subroutines.  Coding purists might argue
      !  with me, and say that these should be declared local in the main 
      !  routine and then passed as arguments whenever needed, but I want to 
      !  keep the argument lists a little shorter for readability.
      ! 
      !  1) ModelCfg will contain the info from the config file.
      !     File names and some global choices made by the user.
      !  2) ModelState will carry around the current state of the lake.
      !     This includes the thermal structure stuff.
      !
      TYPE (TEvapConfig) :: ModelCfg
      TYPE (TEvapParms)  :: ModelParms

      !
      !  Global variables.
      !
      !  Standard "Best Practices" would advise all/most of these should be 
      !  passed around as arguments rather than being simple globals.  
      !  And I agree, in principle. The original code from Croley did a lot
      !  of that, but it makes the argument lists long and hard to follow,
      !  in my opinion. Especially since variable names often changed between 
      !  caller and callee.  I tried encapsulating them within a structured 
      !  variable, but that just made the code much more difficult to read
      !  when the variables were used, in my opinion. So I have 
      !  reverted to just making them globals.
      !
      !  Due to the number of them, I am concerned that these global
      !  variables may get confused with local variables. Thus, I am
      !  adding a convention to all of these variable names. I will start
      !  them all with "g_".  Typically, my variable names start
      !  with a capital letter, so this prepended g_ should be a good flag.
      !
      REAL    :: g_EvapUnitsCoeff, g_HeatUnitsCoeff, g_MonitoringDepth
      INTEGER :: g_LkNum
      REAL    :: g_LkArea
      
      !
      !  These items are calculated at the start of the run based on
      !  which lake we are doing, then are constant.
      !
      REAL :: g_rCv, g_Hd, g_Mxs, g_Ms1, g_Ms2, g_Mxf, g_Mf1, g_Mf2

      !
      !  These items change from day to day. 
      !    Radiation fluxes
      !    water surface temp
      !    ice condition
      !    thermal structure
      !
      REAL    :: g_Incident, g_NetLW       ! watts/m2
      REAL    :: g_Reflect,  g_Latent      ! watts/m2
      REAL    :: g_Sensible, g_Advection   ! watts/m2
      REAL    :: g_SurfTemp                ! deg C
      REAL    :: g_IceAmount               ! cubic meters
      REAL    :: g_IceArea                 ! sq meters
      REAL    :: g_IceDepth                ! meters
      REAL    :: g_IceTemp                 ! deg C
      REAL    :: g_AgeOfCurrentLayer
      REAL    :: g_StoredHeat              ! calories
      INTEGER :: g_NumLayers
      REAL, DIMENSION(0:MaxNoOfLayers) :: g_SurfTempFromLayer, g_TotalHeat
      REAL, DIMENSION(1:MaxNoOfLayers) :: g_AgeOfLayer
      
      INTEGER :: g_MaxNumLayersUsed
      
CONTAINS

!------------------------------------------------------------------------------

      SUBROUTINE DO_LLTM(ConfigFilename)
      IMPLICIT NONE
      
      CHARACTER (LEN=*), INTENT(IN) :: ConfigFileName
      
      INTEGER :: I, J, K, M, IOS, Seq
      INTEGER :: Dy, Mn, Yr
      INTEGER :: NumberOfDays, MetDay
      INTEGER :: PrintInterval
      INTEGER :: UnitOut, UnitBulk, UnitTD, MaxD, DayNum, Depth

      LOGICAL :: FExist, WriteTheHeatLayerFile

      REAL :: Runoff, Precipitation, Inflow, Outflow
      REAL :: SurfaceElevation, OtherTerms, LakeEvaporation
      REAL :: P1, P2, P3, P4, P5, P6, P7, P8, P9, P10
      REAL :: DD1, DD2, TT1, A, B, Delta, Temp
      REAL :: OutVal(20)
      REAL :: VapPres, XDewPt                 ! inline function and the variable used for it
      REAL, DIMENSION(0:999) :: TempAtDepth

      REAL, DIMENSION(MaxNoOfLayers*2 + 1) :: ProfileT, ProfileD

      CHARACTER (LEN=11)  :: OutStr(20)
      CHARACTER (LEN=150) :: DirName, File1

      TYPE (Met_LLTM)     :: RawMetData          ! entire timeseries of input met data
      TYPE (Met_LLTM)     :: FilledMetData       ! entire timeseries of met data after filling missing values
      TYPE (TMetData)     :: DailyMet            ! met data for a single day (in & out)
      
      !
      !  We will store the complete thermal structure (heat layers) for every
      !  day during the run, then output that to a file.
      !  Any existing info in the file will be preserved for days not
      !  part of this run, and overwritten for days that ARE part of this run.
      !
      TYPE (TLayerInfo) :: HeatLayerData       ! heat layer info for the entire run
      
      !
      !  Inline function to compute vapor pressure from dew point temperature
      !
      VapPres(XDewPt) =                                              &
          10.0**(-7.90298 * ((373.16 / (XDewPt + 273.16)) - 1.0)     &
          + 5.02808 * LOG10((373.16 / (XDewPt + 273.16))) - 1.3816   &
          * 0.0000001 * (10.0**(11.334 * (1.0 - (1.0 / (373.16       &
          / (XDewPt + 273.16))))) - 1.0) + 8.1328 * 0.001            &
          * (10.0**(-3.49149 * ((373.16 / (XDewPt + 273.16)) - 1.0)) &
          - 1.0) + 3.0057149)


      !
      !  Initialize things
      !
      CALL ClearFilesOpen()
      ErrorLevel = 0
      g_MonitoringDepth = 0.         ! depth of water-temperatures-to-report-in-simulation (24apr01)
      UnitOut   = -1
      UnitBulk  = -1
      UnitTD    = -1
      g_MaxNumLayersUsed = 0
      WriteTheHeatLayerFile = .FALSE.

      !
      !  Read the config file to get the names of the data files
      !
      File1 = TRIM(ConfigFilename)
      INQUIRE(FILE=TRIM(File1), EXIST=FExist)
      IF (.NOT. FExist) THEN
         ErrorMessage = 'LLTM: Specified configuration file ('//TRIM(File1)//') does not exist.'
         CALL PassMsg;  GOTO 898
      END IF
      CALL ReadConfigFile_LLTM(ConfigFileName, ModelCfg); IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Read the data from the parameter file
      !
      CALL ReadParmFile_LLTM(ModelCfg%ParmFile, ModelParms);  IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Check Lakename for validity
      !
      CALL Lowercase(ModelParms%Bsn)
      g_LkNum = -1
      IF (ModelParms%Bsn .EQ. 'sup' ) g_LkNum = 1
      IF (ModelParms%Bsn .EQ. 'mic' ) g_LkNum = 2
      IF (ModelParms%Bsn .EQ. 'hur' ) g_LkNum = 3
      IF (ModelParms%Bsn .EQ. 'geo' ) g_LkNum = 4
      IF (ModelParms%Bsn .EQ. 'stc' ) g_LkNum = 5
      IF (ModelParms%Bsn .EQ. 'eri' ) g_LkNum = 6
      IF (ModelParms%Bsn .EQ. 'ont' ) g_LkNum = 7
      IF (ModelParms%Bsn .EQ. 'hgb' ) g_LkNum = -1    ! not valid at this time
      IF (g_LkNum .EQ. -1) THEN
         ErrorMessage = 'Invalid lake name in parameter file - '// ModelParms%Bsn;   CALL PassMsg
         GOTO 898
      END IF

      !
      !  Working directory name.
      !  Note that I am forcing/assuming a lowercase Bsn name.
      !  This is irrelevant for MS-Windows systems, which are case-insensitive.
      !  But linux-based systems are case-sensitive. 
      !
      DirName = '.' // FilePathSeparator
      
      !
      !  Compute some constant coefficients, based on lake:
      !
      !     E' = r Ce (qw - q) U' / rw
      !     |    | |   |    |  |    |
      !     |    | |   |    |  |    |__water density, gm/cm3
      !     |    | |   |    |  |__wind speed, cm/s
      !     |    | |   |    |__specific humidity of air, gm/gm
      !     |    | |   |__spec. humid. of saturated air at water temperature, gm/gm
      !     |    | |__evaporation mass transfer coefficient, dimensionless
      !     |    |__air density, gm/cm3
      !     |__evaporation, cm/s
      !
      !     E = (86400 * 10 * 100 * .622 / p * r / rw) Ce (ew - e) U
      !     |      |     |     |      |    |               |    |  |
      !     |      |     |     |      |    |               |    |  |__wind speed, m/s
      !     |      |     |     |      |    |               |    |__vapor pressure, mb
      !     |      |     |     |      |    |               |__vap. pres. @ surface,mb
      !     |      |     |     |      |    |__atmospheric pressure, mb
      !     |      |     |     |      |__(gm/cm3 water vapor)/(gm/cm3 dry air),
      !     |      |     |     |__cm/m
      !     |      |     |__mm/cm
      !     |      |__sec/day
      !     |__evaporation, mm/day
      !
      !     Q' = -r Cp Ch (Tw - T) U'
      !     |       |  |   |    |
      !     |       |  |   |    |__air temperature, deg. C
      !     |       /  |   |__water surface temperature, deg. C
      !     |      /   |__sensible heat mass transfer coefficient, dimensionless
      !     |     |__specific heat of air at constant pressure = 0.22 cal/(gm deg.C)
      !     |__sensible heat rate, cal/(cm2 s)
      !
      !     Q = -(86400 * 100 * 0.22 * r) Ch (Tw - T) U
      !     |__sensible heat rate, cal/(cm2 d)
      !
      g_EvapUnitsCoeff = 86400.0 * 10.0 * 100.0 * 0.622                                  &
             / AtmosphericPressure(g_LkNum) * AirDensity(g_LkNum) / 1.0
      g_HeatUnitsCoeff = 86400.0 * 100.0 * 0.22 * AirDensity(g_LkNum)

      !
      NumberOfDays = ModelParms%ESeq - ModelParms%SSeq + 1

      !
      !  Read the input meteorology file
      !
      CALL ReadMetData_LLTM(ModelCfg%MetFile, RawMetData); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Is the data complete for the run period specified in the parameter file?
      !  If not, attempt to fill in missing values.
      !  Remember that the two arrays (RawMetData and FilledMetData) will have data for the
      !  entire period covered by the input met file, not just the period of ModelParms%SSeq
      !  to ModelParms%ESeq.
      !
      CALL VerifyAndFillMetData(RawMetData, FilledMetData, ModelParms%SSeq, ModelParms%ESeq)
      IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Do we need to output a heat layer file?
      !  In the context of GLSHFS, we will want this for the historical simulation runs, but
      !  we will NOT want to do this for the outlook/forecast runs. This is handled simply
      !  by specifying a file name in the config file or leaving it blank.
      !
      WriteTheHeatLayerFile = .FALSE.
      IF (LEN_TRIM(ModelCfg%LayerFile) .GT. 0) WriteTheHeatLayerFile = .TRUE.

      !
      !  Initialization of the variable containing the current state of 
      !  the lake, which will be updated as the simulation progresses.
      !  
      g_LkArea  = CoordLakeArea(g_LkNum)
      g_rCV     = CoordLakeVolume(g_LkNum) * 1000000. * 1.0 * 1.0 / 1.0e+20
      g_Hd      = g_rCV * 3.98

      g_SurfTemp           = ModelParms%SurfTemp                              ! deg C
      g_IceArea            = ModelParms%IceArea * CoordLakeArea(g_LkNum)      ! fraction -> sq meters
      g_IceDepth           = ModelParms%IceDepth                              ! meters
      g_IceAmount          = ModelParms%IceArea * ModelParms%IceDepth         ! cubic meters
      g_IceTemp            = ModelParms%IceTemp                               ! deg C
      g_AgeOfCurrentLayer  = ModelParms%AgeOfCurrentLayer
      g_NumLayers          = ModelParms%NumLayers
      g_MaxNumLayersUsed   = g_NumLayers

      DO I = 1, g_NumLayers
         g_SurfTempFromLayer(I) = ModelParms%SurfTempFromLayer(I)
         g_TotalHeat(I)         = ModelParms%TotalHeat(I)
         g_AgeOfLayer(I)        = ModelParms%AgeOfLayer(I)
      END DO
      g_SurfTempFromLayer(0)                 = 3.98
      g_TotalHeat(0)                         = g_Hd
      g_TotalHeat(g_NumLayers + 1)  = 0.0    ! dummy value avoids undefined use later
      g_AgeOfLayer(g_NumLayers + 1) = 0.0    ! dummy value avoids undefined use later
      
      !
      !  Copy the required parameter values  into short variable names for 
      !  the purpose of keeping the equations more readable 
      !  (e.g. "P4" is easier to read than "ModelParms(4)")
      !
      P1  = ModelParms%Parms(1)
      P2  = ModelParms%Parms(2)
      P3  = ModelParms%Parms(3)
      P4  = ModelParms%Parms(4)
      P5  = ModelParms%Parms(5)
      P6  = ModelParms%Parms(6)
      P7  = ModelParms%Parms(7)
      P8  = ModelParms%Parms(8)
      P9  = ModelParms%Parms(9)
      P10 = ModelParms%Parms(10)

      !
      !  Calculate some global constants based on the value of the parameters.
      !  These are used in various calculations of heat storage, surface temp, etc.
      !
      IF (P4 .EQ. 0) THEN
         g_Mf1 = 0.0
         g_Mf2 = g_rCV
      ELSE
         g_Mf1 = MIN(P4,  2.0 * g_rCV / (1.0 + g_rCV / P4))
         g_Mf2 = MAX(g_rCV, 2.0 * g_rCV / (1.0 + g_rCV / P4))
      END IF

      IF ((P8 .GT. g_rCV) .AND. (P8 .GT. g_Mf2)) THEN
         g_Mxf = -LOG((P8 - g_Mf2) / P2 / g_Mf2) / P3
      ELSE
         g_Mxf = 3.40E+38
      END IF
      
      IF (P7 .EQ. 0) THEN
         g_Ms1 = 0.0
         g_Ms2 = g_rCV
      ELSE
         g_Ms1 = MIN(P7,  2.0 * g_rCV / (1.0 + g_rCV / P7))
         g_Ms2 = MAX(g_rCV, 2.0 * g_rCV / (1.0 + g_rCV / P7))
      END IF

      IF (P8 .GT. g_rCV .AND. P8 .GT. g_Ms2) THEN
         g_Mxs = -LOG((P8 - g_Ms2) / P5 / g_Ms2) / P6
      ELSE
         g_Mxs = 3.40E+38
      END IF
      
      !
      !  Force conformity to rule that surface temp must always be >= 0.
      !  Note that this check is being added by Tim Hunter on 08Jan2014 in
      !  an attempt to eliminate random crashing of the program for
      !  unknown (right now) reasons. The Georgian Bay output file showed
      !  that surface temperature was going below 0.0 (not much, but some).
      !
      !  Numerical instabilities like this are troubling, and this "fix" is
      !  not really a fix. Instead, the root cause should really be tracked
      !  down and corrected.  But in the absence of available resources
      !  (both science expertise and coder time), this is the temporary 
      !  approach.  (TSH)
      !
      IF (g_SurfTemp .LT. 0.0001) g_SurfTemp = 0.0001

      !
      !  If requested, open file for the Bulk Transfer Coefficient values
      !
      !      IF (ModelCfg%WriteBulkTransferFile) THEN
      !         BulkFile = TRIM(DirName) // TRIM(ModelCfg%BulkFile)
      !         UnitBulk = GetFreeUnitNumber()
      !         OPEN(UNIT=UnitBulk, FILE=TRIM(BulkFile), STATUS='REPLACE')
      !         CALL FileWasOpened(UnitBulk); IF (ErrorLevel .NE. 0) GOTO 899
      !         WRITE(UnitBulk, 4701)
      !      END IF
  
      !
      !  If requested, open file for the Temperature/Depth Profile data.
      !
      !      IF (ModelCfg%WriteTempDepthProfile) THEN
      !         g_MonitoringDepth = LakeDepthMaxMeters(g_LkNum)
      !         TDFile = TRIM(DirName) // TRIM(ModelCfg%TDepthFile)
      !         UnitTD = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      !         OPEN(UNIT=UnitTD, FILE=TRIM(TDFile), STATUS='REPLACE')
      !         CALL FileWasOpened(UnitTD); IF (ErrorLevel .NE. 0) GOTO 899
      !         WRITE(UnitTD, 4801)
      !      END IF
  
      !
      !  Set constant values for some terms that might have an effect on the
      !  heat budget of the lake. For the current formulation of the LLTM
      !  we are essentially ignoring them, presumably because they are
      !  assumed to either be small for the Great Lakes or so uncertain
      !  (from a heat budget perspective) that we cannot estimate them with 
      !  any skill. They are all being set to zero.
      !
      !  This is what Dr. Thomeas E. Croley did in the model. I am simply
      !  duplicating that in this reorganization of the code.
      !  (Tim Hunter,  March, 2017)
      !
      Runoff = 0.        
      Precipitation = 0. 
      Inflow = 0.        
      Outflow = 0.       
      OtherTerms = 0.    
      SurfaceElevation = 0.

      !
      !  Compute the initial stored heat in the lake (based on the
      !  data read from the parameter file)
      !
      g_StoredHeat = HeatInStorage(g_SurfTemp); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Open the output file and write header
      !
      UnitOut = GetFreeUnitNumber()
      OPEN(UNIT=UnitOut, FILE=TRIM(ModelCfg%OutputFile), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(UnitOut)
      WRITE(UnitOut, 1301) 
      WRITE(UnitOut, 1302) ModelParms%Bsn
      WRITE(UnitOut, 1303) SeqToDateStringYMD(ModelParms%SSeq)
      WRITE(UnitOut, 1304) SeqToDateStringYMD(ModelParms%ESeq)
      WRITE(UnitOut, 1305)
      WRITE(UnitOut, 1306)
      
      !
      !  Prepare the heat layer object, if necessary
      !
      IF (WriteTheHeatLayerFile) THEN
         HeatLayerData%Bsn  = ModelParms%Bsn
         HeatLayerData%SSeq = ModelParms%SSeq
         HeatLayerData%ESeq = ModelParms%ESeq
         HeatLayerData%NumDays = ModelParms%ESeq - ModelParms%SSeq + 1
         ALLOCATE(HeatLayerData%Dly(HeatLayerData%NumDays), STAT=IOS)
         IF (IOS .NE. 0) THEN
            WRITE(ErrorMessage, 5001) HeatLayerData%NumDays;  CALL PassMsg
            GOTO 898
         END IF    
      END IF

      !
      !  Now we are ready to run the simulation.
      !  Daily loop
      !
      NumberOfDays = ModelParms%ESeq - ModelParms%SSeq + 1
      PrintInterval = MIN(NINT(NumberOfDays / 10.0), 400)
      PrintInterval = MAX(PrintInterval, 1)
      DO Seq = ModelParms%SSeq, ModelParms%ESeq
         CALL SequenceDate(Dy, Mn, Yr, Seq); IF (ErrorLevel .NE. 0) GOTO 899
         
         !
         !  The FilledMetData array has previously been verified to contain data
         !  for the entire period required. It may also contain data prior to
         !  or after the loop control period.
         !
         MetDay = Seq - FilledMetData%SSeq + 1

         !
         !  Assign values to container for today's met variables
         !
         DailyMet%AirTemp    = FilledMetData%Values(1, MetDay)
         DailyMet%DewPt      = FilledMetData%Values(2, MetDay)
         DailyMet%WindSpeed  = FilledMetData%Values(3, MetDay)
         IF (DailyMet%WindSpeed .LT. 0.01) DailyMet%WindSpeed = 0.01

         !
         !  If we are using cloud cover to compute radiation, then we need to
         !  fill in any missing cloud data, but we can set radiation to missing.
         !  Conversely, if we are using radiation, set cloud to missing.
         !  This simplifies the program flow logic later.
         !
         IF (ModelCfg%RadiationMethod .EQ. 1) THEN
            DailyMet%Cloud = FilledMetData%Values(4, MetDay)
            DailyMet%Incident = MissingData_Real
            DailyMet%NetLW    = MissingData_Real
         ELSE
            DailyMet%Incident = FilledMetData%Values(5, MetDay)
            DailyMet%NetLW    = FilledMetData%Values(6, MetDay)
            DailyMet%Cloud    = -99.999
         END IF
         
         !
         !  Call the main calculation subroutine
         !         
         CALL BalanceElevAndTempAndEvap(                    &
             Dy, Mn, Yr, DailyMet,                          &
             Runoff, Precipitation, Inflow, Outflow,        &
             OtherTerms, SurfaceElevation,                  &
             LakeEvaporation)
         IF (ErrorLevel .NE. 0) GOTO 899
         
         IF (g_MonitoringDepth .GT. 0.0) THEN                                    ! 24apr01
            !
            !  GetTemperatureProfile interprets the heat layer superposition 
            !  information in terms of a vertical temperature profile defined 
            !  at various points along the depth, where ProfileT(1) is the 
            !  temperature at the surface (depth = ProfileD(1) = 0) and
            !  ProfileT(K) is the water temperature at the bottom 
            !  (depth = ProfileD(K) = MaxDepth).
            !  On return, K is the number of valid entries in the ProfileT
            !  and ProfileD arrays.
            !
            CALL GetTemperatureProfile(ProfileT, ProfileD, K)                  ! 24apr01
            M = 1                                                              ! 24apr01
            DO WHILE ((M .LE. K) .AND. (ProfileD(M) .LE. g_MonitoringDepth))     ! 24apr01
               M = M + 1                                                       ! 24apr01
            END DO                                                             ! 24apr01
            IF ((M .GT. K) .OR. (M .LE. 1)) THEN
               ErrorMessage = 'Temperature-Depth Interpolation Error'          ! 24apr01
               CALL PassMsg
               GOTO 898
            ELSE
               IF (M .EQ. K) THEN
                  MaxD = INT(ProfileD(M))
               ELSE
                  MaxD = INT(ProfileD(M-1))
               END IF
               MaxD = INT(g_MonitoringDepth)
               TempAtDepth(:) = -999.9
               DO I = 1, K
                  Temp  = ProfileT(I)           ! temperature of the entry, deg C
                  Depth = NINT(ProfileD(I))     ! depth of the entry, in meters
                  TempAtDepth(Depth) = Temp
               END DO
              
               !
               !  Fill any missing entries by interpolation
               !  Assumption is made that no more than 1 entry per depth
               !  exists (i.e. each ProfileD() entry is unique).
               !
               DO I = 2, MaxD-1
                  IF (TempAtDepth(I) .LT. 0.0) THEN
                     A = TempAtDepth(I-1)
                     J = I + 1
                     DO WHILE (TempAtDepth(J) .LT. 0) 
                        J = J + 1
                     END DO
                     IF (J .LE. MaxD) THEN
                        B = TempAtDepth(J)
                        Delta = (B-A) / (ProfileD(J)-ProfileD(I))    ! degrees per meter
                        TempAtDepth(I) = TempAtDepth(I-1) + Delta
                     END IF
                  END IF
               END DO
              
               !
               !  Write the entries to output file
               !
               !    DO I = 0, MaxD
               !       WRITE(UnitTD, 4802) Year, Month, Day, I, TempAtDepth(I)
               !    END DO
               !    WRITE(UnitTD, *)
              
               !
               !  Computation for continuing
               !
               DD1         = g_MonitoringDepth - ProfileD(M-1)
               DD2         = ProfileD(M)       - ProfileD(M-1)
               TT1         = ProfileT(M)       - ProfileT(M-1)
               g_SurfTemp  = ProfileT(M-1 )    + (DD1/DD2 * TT1)
            END IF
         END IF

         !
         !  Force conformity to the rule that surface temp must always be >= 0.
         !  Note that this check is being added by Tim Hunter on 08Jan2014 in
         !  an attempt to eliminate random crashing of the program for
         !  unknown (right now) reasons. Looking at the ZSEVAPPR.GEO file showed
         !  that surface temperature was going below 0.0 (not much, but some).
         !
         IF (g_SurfTemp .LT. 0.0001) g_SurfTemp = 0.0001

         !
         !  Prepare values for output
         !
         OutVal(1)  = LakeEvaporation / g_LkArea * 1000.0       ! m3 -> mm
         OutVal(2)  = g_SurfTemp                                ! deg. C.
         OutVal(3)  = g_IceTemp                                 ! deg. C.
         OutVal(4)  = g_IceArea / g_LkArea * 100.0              ! m3 -> %
         OutVal(5)  = g_IceDepth                                ! m
         OutVal(6)  = g_Reflect                                 ! watt/m2
         OutVal(7)  = g_Latent                                  ! watt/m2
         OutVal(8)  = g_Sensible                                ! watt/m2
         OutVal(9)  = g_Advection                               ! watt/m2
         OutVal(10) = g_Incident                                ! watt/m2
         OutVal(11) = g_NetLW                                   ! watt/m2
         OutVal(12) = g_StoredHeat                              ! cal
         OutVal(13) = RawMetData%Values(1,MetDay)               ! airtemp, deg. C.
         OutVal(14) = RawMetData%Values(2,MetDay)               ! dewpt,   deg. C.
         OutVal(15) = RawMetData%Values(3,MetDay)               ! windspd, m/s
         OutVal(16) = RawMetData%Values(4,MetDay)               ! cloud,   %
         OutVal(17) = DailyMet%AirTemp                          ! deg. C.
         OutVal(18) = DailyMet%DewPt                            ! deg. C.
         OutVal(19) = DailyMet%WindSpeed                        ! m/s
         OutVal(20) = DailyMet%Cloud                            ! %

         !
         !  The first 12 values are model output, and are never missing
         !
         DO I = 1, 11
            WRITE(OutStr(I), '(F9.3)') OutVal(I)
         END DO
         WRITE(OutStr(12), '(E11.4E2)') OutVal(12)
         
         !
         !  13-20 are the input meteorology, raw and adjusted.
         !  These might be missing.
         !
         DO I = 13, 20
            IF (OutVal(I) .GT. -998.00) THEN
               WRITE(OutStr(I), '(F9.3)') OutVal(I)
            ELSE
               OutStr(I) = '       NA'
            END IF
         END DO
         
         WRITE(UnitOut, 1310) SeqToDateStringYMD(Seq),         &    ! yyyy-mm-dd
                              (OutStr(I), I= 1,20)
                              
         !
         !  If desired, store data about the current heat layer state.
         !  The file will be written at the end.
         !
         IF (WriteTheHeatLayerFile) THEN
            DayNum = Seq - ModelParms%SSeq + 1
            HeatLayerData%Dly(DayNum)%Seq               = Seq
            HeatLayerData%Dly(DayNum)%SurfTemp          = g_SurfTemp
            HeatLayerData%Dly(DayNum)%IceArea           = g_IceArea / g_LkArea      ! fraction from 0.0 to 1.0
            HeatLayerData%Dly(DayNum)%IceDepth          = g_IceDepth
            HeatLayerData%Dly(DayNum)%IceTemp           = g_IceTemp
            HeatLayerData%Dly(DayNum)%AgeOfCurrentLayer = g_AgeOfCurrentLayer
            HeatLayerData%Dly(DayNum)%NumLayers         = g_NumLayers
            HeatLayerData%Dly(DayNum)%AgeOfLayer(:)        = MissingData_Real
            HeatLayerData%Dly(DayNum)%SurfTempFromLayer(:) = MissingData_Real
            HeatLayerData%Dly(DayNum)%TotalHeat(:)         = MissingData_Real
            DO I = 1, g_NumLayers
               HeatLayerData%Dly(DayNum)%AgeOfLayer(I) = g_AgeOfLayer(I)
            END DO
            DO I = 0, g_NumLayers
               HeatLayerData%Dly(DayNum)%SurfTempFromLayer(I) = g_SurfTempFromLayer(I)
               HeatLayerData%Dly(DayNum)%TotalHeat(I)         = g_TotalHeat(I)
            END DO
         END IF
      END DO

      IF (FileIsOpen(UnitBulk)) THEN
         CLOSE(UnitBulk)
         CALL FileWasClosed(UnitBulk); IF (ErrorLevel .NE. 0) GOTO 899
         UnitBulk = -1
      END IF
      
      IF (FileIsOpen(UnitOut)) THEN
         CLOSE(UnitOut)
         CALL FileWasClosed(UnitOut); IF (ErrorLevel .NE. 0) GOTO 899
         UnitOut = -1
      END IF
      
      IF (FileIsOpen(UnitTD)) THEN
         CLOSE(UnitTD)
         CALL FileWasClosed(UnitTD); IF (ErrorLevel .NE. 0) GOTO 899
         UnitTD = -1
      END IF
      
      !  
      !  Write a new output file with the full series of heat layer info, if needed
      !  This will replace the existing file of the same name.
      !
      IF (WriteTheHeatLayerFile) THEN
         CALL WriteHeatLayerFile_LLTM(TRIM(ModelCfg%LayerFile), HeatLayerData);  IF (ErrorLevel .NE. 0) GOTO 899
         DEALLOCATE(HeatLayerData%Dly,  STAT=IOS)
      END IF
      
      !
      !  Informational messages
      !
!      WRITE(ErrorMessage, 9710) g_MaxNumLayersUsed;  CALL PassMsg
!      WRITE(ErrorMessage, 9701) 100;  CALL PassMsg
      
      GOTO 999

      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening file '    // TRIM(File1)
      CALL PassMsg; GOTO 898


  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback]  DO_LLTM....';   CALL PassMsg
      GOTO 999

      !
      !  Force the closure of open files (in case they are still 
      !  open due to early exit) then return.
      !
 999  IF (UnitBulk .GT. 0) THEN
         IF (FileIsOpen(UnitBulk)) THEN
            CLOSE(UnitBulk)
            CALL FileWasClosed(UnitBulk)
         END IF
      END IF
      IF (UnitTD .GT. 0) THEN
         IF (FileIsOpen(UnitTD)) THEN
            CLOSE(UnitTD)
            CALL FileWasClosed(UnitTD)
         END IF
      END IF
      IF (UnitOut .GT. 0) THEN
         IF (FileIsOpen(UnitOut)) THEN
            CLOSE(UnitOut)
            CALL FileWasClosed(UnitOut); IF (ErrorLevel .NE. 0) GOTO 899
         END IF
      END IF
      
      IF (WriteTheHeatLayerFile) THEN
         DEALLOCATE(HeatLayerData%Dly,  STAT=IOS)
      END IF
      
      RETURN

      !
      !  FORMATs
      !
 1301 FORMAT('Output from the NOAA/GLERL Large Lake Evaporation Model.')
 1302 FORMAT(A3, ',  3-character lake code (valid values are SUP MIC HUR GEO STC ERI ONT HGB)')
 1303 FORMAT(A10, ',  Start date (Y-M-D)')
 1304 FORMAT(A10, ',  End   date (Y-M-D)')
 1305 FORMAT('YYYY-MM-DD,     Evap,  WtrTemp,  IceTemp,  IceArea, IceDepth, ReflctdR,',     &
                        '  LatentR, SensbleR, AdvectnR, IncidntR, NetLngWR, StoredHeat,',   &
                        '  AirTemp,    DewPt,  WindSpd, CloudCvr, AdjATemp, AdjDewPt,',     &
                        '  AdjWSpd,   AdjCld')
 1306 FORMAT('YYYY-MM-DD,     (mm),      (C),      (C),      (%), (meters),   (w/m2),',     &
                        '   (w/m2),   (w/m2),   (w/m2),   (w/m2),   (w/m2), (calories),',   &
                        '      (C),      (C),    (m/s),      (%),      (C),      (C),',     &
                        '    (m/s),      (%)')
 1310 FORMAT(A10, 11(',',A9), ',', A11, 8(',',A9))
            
 5001 FORMAT('Error allocating memory to store heat layer data from ', I0, ' days.') 

      END SUBROUTINE DO_LLTM

!------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Read the LLTM configuration file.
      !  This just has file names.  Most "configuration" information is 
      !  provided by the parameter file.
      !
      !  Note that this is a pure standalone routine.  i.e. It does
      !  not make use of the global variables.  The main routine can
      !  use this to fill the global variable by passing the global one as
      !  the argument to this routine.
      !------------------------------------------------------------------------
      SUBROUTINE ReadConfigFile_LLTM(CnfgFile, LLTM_Config)
      IMPLICIT NONE
      CHARACTER (LEN=*),     INTENT(IN)    :: CnfgFile
      TYPE (TEvapConfig), INTENT(INOUT) :: LLTM_Config
      
      INTEGER :: IOS, U1
      LOGICAL :: FExist
      CHARACTER (LEN=30)  :: ItemName
      CHARACTER (LEN=150) :: ItemValue
      CHARACTER (LEN=200) :: Line

      !
      !  Initialize the global variables
      !
      LLTM_Config%RunID         = ''           ! user-supplied description of run (optional)
      LLTM_Config%ParmFile      = '---'        ! input parameters, dates, etc
      LLTM_Config%MetFile       = '---'        ! input forcing meteorology
      LLTM_Config%OutputFile    = '---'        ! output file
      LLTM_Config%MissDataFile  = '---'        ! file used to fill in any missing data (optional)
      LLTM_Config%LayerFile     = '---'        ! file with thermal structure info for first day of each month.
      LLTM_Config%BulkFile      = '---'        ! file for output of bulk transfer values (optional)
      LLTM_Config%TDepthFile    = '---'        ! file for temp/depth profile output (optional)
      LLTM_Config%ApplyOverWaterCorrection = .TRUE.    ! Do we apply overwater correction to the met data?     default is yes
      LLTM_Config%WriteBulkTransferFile    = .FALSE.   ! Do we write a file with the bulk coefficient?         default is no
      LLTM_Config%WriteTempDepthProfile    = .FALSE.   ! Do we write a file with the temp/depth profile info?  default is no
      LLTM_Config%RadiationMethod = -1

      !
      !  Verify existence and then open the config file
      !
      U1 = -1
      INQUIRE(FILE=TRIM(CnfgFile), EXIST=FExist)
      IF (.NOT. FExist) GOTO 811
      
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(CnfgFile), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      READ(U1, 1101, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         Line = TRIM(ADJUSTL(Line))
         IF (Line(1:1) .NE. '#') THEN
            CALL ParseCfgLine(Line, ItemName, ItemValue); IF (Errorlevel .NE. 0) GOTO 899
            CALL Uppercase(ItemName); IF (ErrorLevel .NE. 0) GOTO 899
            IF (TRIM(ItemName) .EQ. 'ID')              LLTM_Config%RunID         = TRIM(ItemValue)
            IF (TRIM(ItemName) .EQ. 'PARMFILE')        LLTM_Config%ParmFile      = TRIM(ItemValue)
            IF (TRIM(ItemName) .EQ. 'METFILE')         LLTM_Config%MetFile       = TRIM(ItemValue)
            IF (TRIM(ItemName) .EQ. 'OUTFILE')         LLTM_Config%OutputFile    = TRIM(ItemValue)
            IF (TRIM(ItemName) .EQ. 'MISSFILE')        LLTM_Config%MissDataFile  = TRIM(ItemValue)
            IF (TRIM(ItemName) .EQ. 'LAYERFILE')       LLTM_Config%LayerFile     = TRIM(ItemValue)
            IF (TRIM(ItemName) .EQ. 'BULKFILE')        LLTM_Config%BulkFile      = TRIM(ItemValue)
            IF (TRIM(ItemName) .EQ. 'TDP_FILE')        LLTM_Config%TDepthFile    = TRIM(ItemValue)
            IF (TRIM(ItemName) .EQ. 'APPLYOVERWATERCORRECTION') LLTM_Config%ApplyOverWaterCorrection = TextToLogical(ItemValue)
            IF (TRIM(ItemName) .EQ. 'RADIATIONMETHOD') LLTM_Config%RadiationMethod = TextToInteger(ItemValue)
         END IF
         READ(U1, 1101, IOSTAT=IOS) Line
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      IF (LLTM_Config%ParmFile(1:3) .EQ. '---') THEN
         ErrorMessage = 'No PARMFILE entry found in ' // TRIM(CnfgFile);   CALL PassMsg; GOTO 898
      ENDIF
      IF (LLTM_Config%MetFile(1:3) .EQ. '---') THEN
         ErrorMessage = 'No METFILE entry found in ' // TRIM(CnfgFile);    CALL PassMsg; GOTO 898
      ENDIF
      IF (LLTM_Config%OutputFile(1:3) .EQ. '---') THEN
         ErrorMessage = 'No OUTFILE entry found in ' // TRIM(CnfgFile);    CALL PassMsg; GOTO 898
      ENDIF
      IF (LLTM_Config%LayerFile(1:3) .EQ. '---') THEN
         ErrorMessage = 'No LAYERFILE entry found in ' // TRIM(CnfgFile);    CALL PassMsg; GOTO 898
      ENDIF
      IF (LLTM_Config%BulkFile(1:3)   .NE. '---') LLTM_Config%WriteBulkTransferFile = .TRUE.
      IF (LLTM_Config%TDepthFile(1:3) .NE. '---') LLTM_Config%WriteTempDepthProfile = .TRUE.
      
      RETURN
      !
      !  Error handling
      !
  811 ErrorMessage = 'LLTM: Error opening file '//TRIM(CnfgFile);   CALL PassMsg;  GOTO 898
      
  898 ErrorLevel = 1         ! Return with an error indication.
  899 ErrorMessage = '[traceback] : ReadConfigFile_LLTM...';        CALL PassMsg
      IF (U1 .GT. 0) THEN
         IF (FileIsOpen(U1)) THEN
            CLOSE(U1, IOSTAT=IOS)
            CALL FileWasClosed(U1)
         END IF
      END IF
      RETURN

 1101 FORMAT(A)
      END SUBROUTINE ReadConfigFile_LLTM
      
!-----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Read the LLTM parameter file (CSV format).
      !
      !  Note that this is a pure standalone routine.  i.e. It does
      !  not make use of the global variables.  The main routine can
      !  use this to fill the global variable by passing the global one as
      !  the argument to this routine.
      !------------------------------------------------------------------------
      SUBROUTINE ReadParmFile_LLTM(ParmFile, EParms)
      IMPLICIT   NONE
      CHARACTER (LEN=*),   INTENT(IN)  :: ParmFile
      TYPE (TEvapParms), INTENT(OUT) :: EParms

      INTEGER :: U1, I
      INTEGER :: Seq, Entries
      CHARACTER(LEN=150) :: Line, CsvStrings(15)

      !
      !  Initialize the structure contents
      !
      EParms%Parms(:) = -9.9e29
      
      
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(ParmFile), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Header/Comment lines
      !
      DO I = 1, 3
         READ(U1, 1001, ERR=812) EParms%Comments(I)
      END DO

      !
      !  Lake name/code (SUP, MIC, HUR, etc)
      !
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, Entries)
      IF (Entries .LT. 1) GOTO 828
      EParms%Bsn = GetLowerCase(CsvStrings(1))
         
      !
      !  Parameters
      !
      DO I = 1, 13
         READ(U1, 1001, ERR=812) Line
         CALL ParseCommaSepLine(Line, CsvStrings, Entries)
         READ(CsvStrings(1), *, ERR=830) EParms%Parms(I)
      END DO
      
      !
      !  Dates
      !
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, Entries)
      Seq = DateStringYMDToSeq(CsvStrings(1)); IF (ErrorLevel .NE. 0) GOTO 899
      IF (Seq .EQ. MissingData_Date) THEN
         WRITE(ErrorMessage, 9021) TRIM(CsvStrings(1));    CALL PassMsg
         GOTO 898
      END IF
      EParms%SSeq = Seq
    
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, Entries)
      Seq = DateStringYMDToSeq(CsvStrings(1)); IF (ErrorLevel .NE. 0) GOTO 899
      IF (Seq .EQ. MissingData_Date) THEN
         WRITE(ErrorMessage, 9022) TRIM(CsvStrings(1));    CALL PassMsg
         GOTO 898
      END IF
      EParms%ESeq = Seq
      
      !
      !  State variables
      !
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, Entries)
      READ(CsvStrings(1), *, ERR=851) EParms%SurfTemp          ! deg C
      READ(U1, 1001, ERR=812) Line                             ! unused as of 1aug95
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, Entries)
      READ(CsvStrings(1), *, ERR=852) EParms%IceArea           ! fraction (0-1.0)
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, Entries)
      READ(CsvStrings(1), *, ERR=853) EParms%IceDepth          ! meters
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, Entries)
      READ(CsvStrings(1), *, ERR=854) EParms%IceTemp           ! deg C
      READ(U1, 1001, ERR=812) Line                             ! unused as of 15jun94

      !
      !  Layer data
      !
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, Entries)
      READ(CsvStrings(1), *, ERR=861) EParms%AgeOfCurrentLayer
      READ(U1, 1001, ERR=812) Line
      CALL ParseCommaSepLine(Line, CsvStrings, Entries)
      READ(CsvStrings(1), *, ERR=862) EParms%NumLayers
      IF (EParms%NumLayers .GT. MaxNoOfLayers) THEN
         WRITE(ErrorMessage, 9051) TRIM(ParmFile)
         CALL PassMsg
         WRITE(ErrorMessage, 9052) EParms%NumLayers, MaxNoOfLayers
         CALL PassMsg
         GOTO 898
      END IF
      READ(U1, 1001, ERR=812)       ! column header line

      DO I = 1, EParms%NumLayers
         READ(U1, 1001, ERR=812) Line
         CALL ParseCommaSepLine(Line, CsvStrings, Entries)
         READ(CsvStrings(2), *, ERR=863) EParms%AgeOfLayer(I)
         READ(CsvStrings(3), *, ERR=864) EParms%SurfTempFromLayer(I)
         READ(CsvStrings(4), *, ERR=865) EParms%TotalHeat(I)
      END DO

      CLOSE (U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      RETURN

 811  ErrorMessage = 'LLTM: Error opening ' // TRIM(ParmFile)
      CALL PassMsg;  GOTO 898
 812  ErrorMessage = 'LLTM: Error reading ' // TRIM(ParmFile)
      CALL PassMsg;  GOTO 898

 
 828  ErrorMessage = 'Error parsing lake name from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898
      
 830  ErrorMessage = 'Error parsing parameter value from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898
      
      
 851  ErrorMessage = 'Error parsing initial surface temperature from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898
 852  ErrorMessage = 'Error parsing initial ice area from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898
 853  ErrorMessage = 'Error parsing initial ice depth from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898
 854  ErrorMessage = 'Error parsing initial ice temperature from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898

 861  ErrorMessage = 'Error parsing Age of Current Layer from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898
 862  ErrorMessage = 'Error parsing Number of Layers from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898
 863  ErrorMessage = 'Error parsing AgeOfLayer entry from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898
 864  ErrorMessage = 'Error parsing SurfTempFromLayer entry from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898
 865  ErrorMessage = 'Error parsing TotalHeat entry from ' // TRIM(ParmFile)
      CALL PassMsg; GOTO 898
      
      
 898  CLOSE (U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899
      ErrorLevel = 1
 899  ErrorMessage = '[traceback] ReadParmFile_LLTM...';  CALL PassMsg
      RETURN

 1001 FORMAT(A150)
 9021 FORMAT(' Error parsing start date from entry [', A, ']')
 9022 FORMAT(' Error parsing end date from entry [', A, ']')
 9051 FORMAT(' Too many layers specified in ', A)
 9052 FORMAT(' NumberOfLayers = ', I0, '  MaxAllowed = ', I0)

      END SUBROUTINE ReadParmFile_LLTM

!------------------------------------------------------------------------
      !------------------------------------------------------------------
      !------------------------------------------------------------------
      SUBROUTINE ReadMetData_LLTM(MetFile, MetData)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: MetFile
      TYPE (Met_LLTM),  INTENT(INOUT) :: MetData

      INTEGER :: I, J, D, IOS, NumStr, U1
      INTEGER :: Seq, NumDays, SSeq, ESeq
      LOGICAL :: ISW, NLW
      REAL    :: Dat(6)
      CHARACTER(LEN=30)  :: CsvStrings(10)
      CHARACTER(LEN=120) :: Line
      
      INTEGER, DIMENSION(6), PARAMETER :: ReqType =                 &
         (/ GDT_AirTempMean, GDT_DewPointMean, GDT_WindSpeed,     &
            GDT_CloudCover,  GDT_IncidentRad,  GDT_NetLongWaveRad /)
      
      INTEGER, DIMENSION(6), PARAMETER :: ReqUnit =                  &
         (/ GDU_Celsius,  GDU_Celsius,     GDU_MetersPerSecond,      &
            GDU_Percent,  GDU_WattsPerM2,  GDU_WattsPerM2 /)
      
      !
      !  Initialize MetData
      !
      MetData%SSeq = MissingData_Date
      MetData%ESeq = MissingData_Date
      MetData%CloudIsValid = .FALSE.
      MetData%RadIsValid   = .FALSE.
      DO I = 1, 6
         MetData%Types(I) = ReqType(I)
         MetData%Units(I) = ReqUnit(I)
      END DO

      !
      !  Open the file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(MetFile), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)
      
      !
      !  Read through the file to find the date extents. Don't save data yet.
      !
      SSeq = MissingData_Date
      ESeq = MissingData_Date
      DO I = 1, 3
         READ(U1, 1001, IOSTAT=IOS) Line     ! skip 3 header lines
      END DO
      READ(U1, 1001, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0) 
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumStr .GT. 1) THEN
            Seq = DateStringYMDToSeq(TRIM(CsvStrings(1)));  IF (ErrorLevel .NE. 0) GOTO 899
         END IF
         IF (Seq .NE. MissingData_Date) THEN
            IF (SSeq .EQ. MissingData_Date) SSeq = Seq
            IF (ESeq .EQ. MissingData_Date) ESeq = Seq
            IF (Seq .LT. SSeq) SSeq = Seq
            IF (Seq .GT. ESeq) ESeq = Seq
         END IF
         READ(U1, 1001, IOSTAT=IOS) Line
      END DO
      MetData%SSeq = SSeq
      MetData%ESeq = ESeq
      
      !
      !  Allocate the RAM
      !
      NumDays = ESeq - SSeq + 1
      ALLOCATE(MetData%Values(6,NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN 
         WRITE(ErrorMessage, 5001) NumDays;   CALL PassMsg
         GOTO 898
      END IF
      MetData%Values(:,:) = MissingData_Real

      !
      !  Back to the start of the file
      !  This time, read and store data values
      !
      REWIND(U1)
      DO I = 1, 3
         READ(U1, 1001, IOSTAT=IOS) Line     ! skip 3 header lines
      END DO
      READ(U1, 1001, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0) 
         CALL ParseCommaSepLine(Line, CsvStrings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumStr .GE. 5) THEN
            Seq = DateStringYMDToSeq(TRIM(CsvStrings(1)));  IF (ErrorLevel .NE. 0) GOTO 899
            Dat(:) = MissingData_Real
            IF (Seq .NE. MissingData_Date) THEN
               DO I = 1, MIN(6, NumStr-1)
                  READ(CsvStrings(I+1), *, IOSTAT=J) Dat(I)
                  IF (J .NE. 0) Dat(I) = MissingData_Real
               END DO
            END IF
            D = Seq - SSeq + 1
            MetData%Values(1:6, D) = Dat(1:6)
         END IF
         READ(U1, 1001, IOSTAT=IOS) Line
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1)
      U1 = -1
      
      !
      !  Do we have valid cloud cover data?
      !  If at least half of the days have valid data, consider 
      !  it to be valid.
      !
      MetData%CloudIsValid = .FALSE.
      J = 0
      I = 1
      DO WHILE ((.NOT. MetData%CloudIsValid) .AND. (I .LE. NumDays))
         IF (MetData%Values(4,I) .GT. MissingData_Real_Test) J = J + 1
         I = I + 1
      END DO
      IF (J .GT. I/2) MetData%CloudIsValid = .TRUE.

      !
      !  How many days have both *BOTH* kinds of radiation data?
      !  If at least half of the days have valid data, consider 
      !  it to be valid.
      !
      MetData%RadIsValid = .FALSE.
      ISW = .FALSE.
      NLW = .FALSE.
      J = 0
      I = 1
      DO WHILE ((.NOT. MetData%RadIsValid) .AND. (I .LE. NumDays))
         IF (MetData%Values(5,I) .GT. MissingData_Real_Test) ISW = .TRUE.
         IF (MetData%Values(6,I) .GT. MissingData_Real_Test) NLW = .TRUE.
         IF (ISW .AND. NLW) J = J + 1
         I = I + 1
      END DO
      IF (J .GT. I/2) MetData%RadIsValid = .TRUE.
      
      GOTO 999
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening file '//TRIM(MetFile);   CALL PassMsg;  GOTO 898
  
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ReadMetData_LLTM...';  CALL PassMsg
      IF (U1 .GT. 0) THEN
         IF (FileIsOpen(U1)) THEN
            CLOSE(U1, IOSTAT=IOS)
            CALL FileWasClosed(U1)
         END IF
      END IF
      MetData%SSeq = MissingData_Date
      MetData%ESeq = MissingData_Date
      DEALLOCATE(MetData%Values, STAT=IOS)
     
  999 RETURN    
 
 1001 FORMAT(A120) 
 5001 FORMAT('Error allocating space for ', I0, ' days of data')

      END SUBROUTINE ReadMetData_LLTM
      
!------------------------------------------------------------------------
      !------------------------------------------------------------------
      !  Given a single time-series of data, is every entry a valid value?
      !------------------------------------------------------------------
      FUNCTION MetDataIsComplete(MetD)  Result(Flag)
      IMPLICIT NONE
      REAL, DIMENSION(:), INTENT(IN) :: MetD
      LOGICAL :: Flag

      INTEGER :: I
      
      Flag = .TRUE.
      DO I = LBOUND(MetD,1), UBOUND(MetD,1)
         IF (MetD(I) .LT. MissingData_Real_Test) THEN
            Flag = .FALSE.
            RETURN
         END IF
      END DO
      RETURN
      
      END FUNCTION MetDataIsComplete

!------------------------------------------------------------------------
      !------------------------------------------------------------------
      !  Check validity of the met data and fill in as needed.  
      !  Note that we will use a temp array with the routine FillMissingData.
      !  That gives us the option to use the values computed, or not.
      !
      !  The time-period of both the input (MetData) and output (FilledMetData)
      !  arrays may cover a larger period of time than the requested date extents. 
      !  But the time-period of those arrays must match.
      !------------------------------------------------------------------
      SUBROUTINE VerifyAndFillMetData(MetData, FilledMetData, SSeqReq, ESeqReq)
      IMPLICIT NONE
      TYPE (Met_LLTM),  INTENT(INOUT) :: MetData, FilledMetData
      INTEGER,          INTENT(IN)    :: SSeqReq, ESeqReq
      
      INTEGER :: I, J1, J2, K, IOS, DT, DU, NumDays
      LOGICAL :: OK
      
      CHARACTER(LEN=150) :: FillFile
      REAL, DIMENSION(:), ALLOCATABLE :: TMet


      !
      !  Allocate the permanent FilledMetData array
      !
      NumDays = MetData%ESeq - MetData%SSeq + 1
      ALLOCATE(FilledMetData%Values(6, NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5001) NumDays;  CALL PassMsg
         GOTO 898
      END IF
      FilledMetData%SSeq = MetData%SSeq
      FilledMetData%ESeq = MetData%ESeq
      FilledMetData%CloudIsValid = MetData%CloudIsValid
      FilledMetData%RadIsValid   = MetData%RadIsValid
      DO I = 1, 6
         FilledMetData%Types(I) = MetData%Types(I)
         FilledMetData%Units(I) = MetData%Units(I)
      END DO
      FilledMetData%Values(:,:) = MetData%Values(:,:)
      
      !
      !  Verify valid date values
      !
      NumDays = ESeqReq - SSeqReq + 1
      IF (NumDays .LT.       1)      GOTO 701
      IF (NumDays .GT. 365*250)      GOTO 702
      IF (SSeqReq .LT. MetData%SSeq) GOTO 703
      IF (ESeqReq .GT. MetData%ESeq) GOTO 704
            
      !
      !  Allocate the temporary TMet array
      !
      ALLOCATE(TMet(NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5001) NumDays;  CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Assign the file name for filling in missing data (if needed)
      !
      IF (ModelCfg%MissDataFile(1:3) .NE. '---') THEN
         FillFile = TRIM(ModelCfg%MissDataFile)
      ELSE
         FillFile = TRIM(ModelCfg%MetFile)
      END IF
      
      !
      !  We always do the airtemp, dewpoint, windspeed
      !
      DO I = 1, 3
         DT = ReqType(I)
         DU = ReqUnit(I)
         J1 = SSeqReq - MetData%SSeq + 1
         J2 = ESeqReq - MetData%SSeq + 1
         TMet(1:NumDays) = MetData%Values(I, J1:J2)
         OK = MetDataIsComplete(TMet)
         IF (.NOT. OK) THEN
            CALL FillMissingData(TMet, SSeqReq, ESeqReq, DT, FillFile); IF (ErrorLevel .NE. 0) GOTO 899
            FilledMetData%Values(I, J1:J2) = TMet(1:NumDays)
         END IF
      END DO

      !
      !  If we are using cloud cover data, fill in the entire series.
      !  Keep the units as percent coverage (0-100)
      !
      IF (ModelCfg%RadiationMethod .EQ. 1) THEN
         DT = ReqType(4)                   ! GDT_CloudCover
         DU = ReqUnit(4)                   ! GDU_Percent
         J1 = SSeqReq - MetData%SSeq + 1
         J2 = ESeqReq - MetData%SSeq + 1
         TMet(1:NumDays) = MetData%Values(4, J1:J2)
         OK = MetDataIsComplete(TMet)
         IF (.NOT. OK) THEN
            CALL FillMissingData(TMet, SSeqReq, ESeqReq, DT, FillFile)
            IF (ErrorLevel .NE. 0) GOTO 899
            FilledMetData%Values(4, J1:J2) = TMet(1:NumDays)
         END IF
      END IF

      !
      !  Fill in radiation values, if we need them.
      !  If a particular day has missing radiation data, but DOES have 
      !  cloud cover data, then we will leave the radiation value as
      !  missing. At the time of doing the daily calculations, the 
      !  cloud cover value will be used to compute the radiation, just 
      !  like in the original LLTM.
      !  
      IF (ModelCfg%RadiationMethod .GE. 2) THEN
         DO I = 5, 6
            DT = ReqType(I)                  ! 5 = GDT_IncidentRad;   6 = GDT_NetLongWaveRad
            DU = ReqUnit(I)                  ! GDU_WattsPerM2
            J1 = SSeqReq - MetData%SSeq + 1
            J2 = ESeqReq - MetData%SSeq + 1
            TMet(1:NumDays) = MetData%Values(I, J1:J2)
            OK = MetDataIsComplete(TMet)
            IF (.NOT. OK) THEN
               CALL FillMissingData(TMet, SSeqReq, ESeqReq, DT, FillFile)
               IF (ErrorLevel .NE. 0) GOTO 899
               DO K = J1, J2
                  IF (IsMissing(MetData%Values(4,K))) THEN
                     FilledMetData%Values(I,K) = TMet(K-J1+1)
                  END IF
               END DO
            END IF
         END DO
      END IF
      
      GOTO 999

      !
      !  Error handling
      !
  701 ErrorMessage = 'Requested end date before requested start date.';  CALL PassMsg;  GOTO 898
  702 ErrorMessage = 'Requested data period > 250 years.';               CALL PassMsg;  GOTO 898
  703 ErrorMessage = 'Requested start date before available met data';   CALL PassMsg;  GOTO 898
  704 ErrorMessage = 'Requested end date after available met data.';     CALL PassMsg;  GOTO 898

  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] VerifyAndFillMetData...';  CALL PassMsg
      
  999 IF (ALLOCATED(TMet)) DEALLOCATE(TMet, STAT=IOS)
      RETURN
      
      !
      !
 5001 FORMAT('Error allocating memory for ', I0, ' days of met data.')

      END SUBROUTINE VerifyAndFillMetData
      
!------------------------------------------------------------------------
      !---------------------------------------------------------------------------
      !  FillMissingData is used to fill in missing data that is encountered
      !  within the input data set.  It determines the day-of-the-year for the
      !  missing value, and then aggregates all of the values from the specified
      !  FillFile for that same day-of-the-year.  The average of those values 
      !  will be the value filled in for the missing data.
      ! 
      !  e.g.  If the date is March 15, 1989 (day-of-year = 74), then we will grab
      !        all of the valid values from each year for day 74 (either 3/14 or 
      !        3/15, depending on leap year).  We will then determine the average 
      !        of all those data values.  This is the value to be used.
      !
      !  TMet     = The daily data values.  It is a 1-D scalar array of REAL values.
      !             Period of record needs to match SSeq to ESeq.  This array is 
      !             modified by the routine.
      !  SSeq     = start date (sequence number) of the data in TMet
      !  ESeq     = end date (sequence number) of the data in TMet
      !  DType    = Data type of the data in TMet.  Uses the GLSHFS GDT_* definitions.
      !  FillFile = Name of the input file (GLSHFS standard single subbasin format)
      !             that will be used as the source for computing the long-term 
      !             daily average values to fill in TMet.
      !---------------------------------------------------------------------------
      SUBROUTINE FillMissingData(TMet, SSeq, ESeq, DType, FillFile)
      IMPLICIT NONE
      REAL, DIMENSION(:), INTENT(INOUT) :: TMet
      INTEGER,            INTENT(IN)    :: SSeq, ESeq, DType
      CHARACTER(LEN=*),   INTENT(IN)    :: FillFile
      
      INTEGER :: I, J, IOS, Knt, Seq, DT
      INTEGER :: SSeqF, ESeqF, SeqF
      INTEGER :: MissDOY, FDOY
      INTEGER :: Dy, Mn, SYr, EYr, NumYrs
      LOGICAL :: ComputeIt
      REAL    :: FVal, Tot
      TYPE (Met_LLTM)  :: FillData
      
      !
      !  Get the DataType index
      !
      DT = -1
      IF (DType .EQ. GDT_AirtempMean)    DT = 1
      IF (DType .EQ. GDT_DewpointMean)   DT = 2
      IF (DType .EQ. GDT_Windspeed)      DT = 3
      IF (DType .EQ. GDT_Cloudcover)     DT = 4
      IF (DType .EQ. GDT_IncidentRad)    DT = 5
      IF (DType .EQ. GDT_NetLongWaveRad) DT = 6
      IF (DT .EQ. -1) THEN
         WRITE(ErrorMessage, 5001) GlerlDataTypeString(DType), TRIM(FillFile);  CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Get the source data that will be used to fill
      !
      CALL ReadMetData_LLTM(FillFile, FillData); IF (ErrorLevel .NE. 0) GOTO 899
      SSeqF = FillData%SSeq
      ESeqF = FillData%ESeq

      !
      !  How many years of data are we using to compute the long-term averages?
      !  We will require that at least 33% of the years report a value for a
      !  particular day in order to compute a long-term mean for that day.
      !  I am ignoring some "edge case" conditions here (incomplete years, etc.)
      !  Just going for the "ballpark" number.
      !
      CALL SequenceDate(Dy, Mn, SYr, SSeqF); IF (ErrorLevel .NE. 0) GOTO 899
      CALL SequenceDate(Dy, Mn, EYr, ESeqF); IF (ErrorLevel .NE. 0) GOTO 899
      NumYrs = EYr - SYr + 1
      
      !
      !  Fill missing values in TMet
      !  If the missing day happens to be 12/31 of a leap year (day 366), then
      !  use the average of all the day 365 values as the fill in value.
      !  That gives us a larger sample.
      !
      !  
      !
      DO Seq = SSeq, ESeq
         I = Seq - SSeq + 1
         IF (IsMissing(TMet(I))) THEN
            MissDOY = DayOfYear(Seq)
            IF (MissDOY .EQ. 366) MissDOY = 365
            Tot = 0.0
            Knt = 0
            DO SeqF = SSeqF, ESeqF
               FDOY = DayOfYear(SeqF)
               IF (FDOY .EQ. MissDOY) THEN
                  J = SeqF - SSeqF + 1
                  FVal = FillData%Values(DT,J)
                  IF (.NOT. IsMissing(FVal)) THEN
                     Tot = Tot + FVal
                     Knt = Knt + 1
                  END IF
               END IF
            END DO
            ComputeIt = .FALSE.
            IF (NumYrs .LT. 5) THEN
               IF (Knt .GT. 0) ComputeIt = .TRUE.
            ELSE
               IF (Knt .GT. NumYrs * 0.33) ComputeIt = .TRUE.
            END IF
            
            IF (ComputeIt) THEN
               TMet(I) = (Tot / Knt)
            ELSE
               WRITE(ErrorMessage, 5002) TRIM(GlerlDataTypeString(DType)),    &
                                  SeqToDateStringYMD(Seq), TRIM(FillFile)
               CALL PassMsg;  GOTO 898
            END IF
         END IF
      END DO
      
      GOTO 999
      
      !
      !  Error handling
      !
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] FillMissingData...';  CALL PassMsg
      
  999 IF (ALLOCATED(FillData%Values)) DEALLOCATE(FillData%Values, STAT=IOS)
      RETURN
      
      !
      !
 5001 FORMAT('Error: Unable to find [', A, '] data in file ', A)
 5002 FORMAT('Error: Unable to fill [', A, '] data for ', A, ' from file ', A)
      
      END SUBROUTINE FillMissingData
      
      !------------------------------------------------------------------------
      !  Write a LLTM parameter file (CSV format).
      !
      !  Note that this is a pure standalone routine.  i.e. It does
      !  not make use of the global variables.  The main routine can
      !  use this to output the global variable values by passing the 
      !  global variable as the argument to this routine.
      !------------------------------------------------------------------------
      SUBROUTINE WriteParmFile_LLTM(ParmFile, EParms)
      IMPLICIT   NONE
      CHARACTER (LEN=*),   INTENT(IN) :: ParmFile
      TYPE (TEvapParms), INTENT(IN) :: EParms

      INTEGER :: U1, I

      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(ParmFile), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Header/Comment lines
      !
      DO I = 1, 3
         WRITE(U1, 1001, ERR=813) EParms%Comments(I)
      END DO

      !
      !  Lake name/code (SUP, MIC, HUR, etc)
      !
      WRITE(U1, 1002, ERR=813) EParms%Bsn
         
      !
      !  Parameters
      !
      WRITE(U1, 1011, ERR=813) EParms%Parms(1)
      WRITE(U1, 1012, ERR=813) EParms%Parms(2)
      WRITE(U1, 1013, ERR=813) EParms%Parms(3)
      WRITE(U1, 1014, ERR=813) EParms%Parms(4)
      WRITE(U1, 1015, ERR=813) EParms%Parms(5)
      WRITE(U1, 1016, ERR=813) EParms%Parms(6)
      WRITE(U1, 1017, ERR=813) EParms%Parms(7)
      WRITE(U1, 1018, ERR=813) EParms%Parms(8)
      WRITE(U1, 1019, ERR=813) EParms%Parms(9)
      WRITE(U1, 1020, ERR=813) EParms%Parms(10)
      WRITE(U1, 1021, ERR=813) MissingData_Real, 11
      WRITE(U1, 1021, ERR=813) MissingData_Real, 12
      WRITE(U1, 1021, ERR=813) MissingData_Real, 13

      !
      !  Dates
      !
      WRITE(U1, 1031, ERR=813) SeqToDateStringYMD(EParms%SSeq)
      WRITE(U1, 1032, ERR=813) SeqToDateStringYMD(EParms%ESeq)
      
      !
      !  State variables
      !
      WRITE(U1, 1041, ERR=813) EParms%SurfTemp             !  deg C
      WRITE(U1, 1042, ERR=813) -9.999E29
      WRITE(U1, 1043, ERR=813) EParms%IceArea              !  fraction (0.0 - 1.0)
      WRITE(U1, 1044, ERR=813) EParms%IceDepth             !  meters
      WRITE(U1, 1045, ERR=813) EParms%IceTemp              !  deg C
      WRITE(U1, 1046, ERR=813) -9.999E29

      !
      !  Layer data
      !
      WRITE(U1, 1047, ERR=813) EParms%AgeofCurrentLayer
      WRITE(U1, 1050, ERR=813) EParms%NumLayers
      WRITE(U1, 1051, ERR=813)
      DO I = 1, EParms%NumLayers
         WRITE(U1, 1052, ERR=813) I, EParms%AgeOfLayer(I),          &
               EParms%SurfTempFromLayer(I), EParms%TotalHeat(I)
      END DO
      CLOSE (U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      RETURN

 811  ErrorMessage = 'LLTM: Error opening ' // TRIM(ParmFile)
      CALL PassMsg;  GOTO 898
 813  ErrorMessage = 'LLTM: Error writing ' // TRIM(ParmFile)
      CALL PassMsg;  GOTO 898
      
 898  ErrorLevel = 1
 899  ErrorMessage = '[traceback] WriteParmFile_LLTM...';  CALL PassMsg
      IF (FileIsOpen(U1)) THEN
         CLOSE (U1)
         CALL FileWasClosed(U1)
      END IF
      RETURN

 1001 FORMAT(A)
 1002 FORMAT(A3, ', 3-character lake code (valid values are SUP MIC HUR GEO STC ERI ONT HGB)')
 1011 FORMAT(E12.5E2, ',   Ice-model-related parameter, "tau sub a"')
 1012 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "a prime"')
 1013 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "b prime"')
 1014 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "F prime"')
 1015 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "a"')
 1016 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "b"')
 1017 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "F"')
 1018 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "V sub e"')
 1019 FORMAT(E12.5E2, ',   Temperature-model-related parameter, "p"')
 1020 FORMAT(E12.5E2, ',   Ice-model-related parameter, "tau sub w"')
 1021 FORMAT(E12.5E2, ',   Unused parameters value ', I0)

 1031 FORMAT(A, ',   Start Date (YYYY-MM-DD)')
 1032 FORMAT(A, ',   End   Date (YYYY-MM-DD)')

 1041 FORMAT(E12.5E2, ',   Surface Temperature at start of run (deg C)')
 1042 FORMAT(E12.5E2, ',   unused')
 1043 FORMAT(E12.5E2, ',   Ice Area (fraction from 0.0 to 1.0)')
 1044 FORMAT(E12.5E2, ',   Ice Depth (meters)')
 1045 FORMAT(E12.5E2, ',   Ice Temperature (deg C)')
 1046 FORMAT(E12.5E2, ',   unused')
 1047 FORMAT(E12.5E2, ',   Age of Current Layer')
 
 1050 FORMAT(I0,  ',   Number of wind/heat layers')
 1051 FORMAT('Layer, AgeOfLayer, SurfTempFromLayer, TotalHeat')
 1052 FORMAT(I5, 3(',', E12.5E2))
 
      END SUBROUTINE WriteParmFile_LLTM

!-----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Write a new LLTM thermal structure file (CSV format).
      !  This new file will contain only the data in LayerInfo, and will not 
      !  preserve data that may exist in a previous version. Therefore, the
      !  period of record may also differ from any existing file.
      !
      !  Note that this is a pure standalone routine.  i.e. It does
      !  not make use of the global variables.  The main routine can
      !  use this to fill the global variable by passing the global one as
      !  the argument to this routine.
      !------------------------------------------------------------------------
      SUBROUTINE WriteHeatLayerFile_LLTM(LayerFile, LayerInfo)
      IMPLICIT   NONE
      CHARACTER (LEN=*),  INTENT(IN) :: LayerFile
      TYPE (TLayerInfo),  INTENT(IN) :: LayerInfo

      INTEGER :: U1, I, J, Seq

      !
      !  Open the file
      !
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(LayerFile), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899

      !
      !  Header lines
      !
      WRITE(U1, 1001)
      WRITE(U1, 1002)

      !
      !  Lake name/code (SUP, MIC, HUR, etc)
      !
      WRITE(U1, 1003, ERR=812) LayerInfo%Bsn
         
      !
      !  Dates
      !
      WRITE(U1, 1004, ERR=812) SeqToDateStringYMD(LayerInfo%SSeq)
      WRITE(U1, 1005, ERR=812) SeqToDateStringYMD(LayerInfo%ESeq)
      
      !
      !   Write the layer info.
      !   layer 0 is not output to the file.
      !
      DO Seq = LayerInfo%SSeq, LayerInfo%ESeq
         I = Seq - LayerInfo%SSeq + 1
         WRITE(U1, 1011, ERR=812, ADVANCE='NO') SeqToDateStringYMD(Seq)
         WRITE(U1, 1012, ERR=812, ADVANCE='NO') LayerInfo%Dly(I)%SurfTemp
         WRITE(U1, 1012, ERR=812, ADVANCE='NO') LayerInfo%Dly(I)%IceArea
         WRITE(U1, 1012, ERR=812, ADVANCE='NO') LayerInfo%Dly(I)%IceDepth
         WRITE(U1, 1012, ERR=812, ADVANCE='NO') LayerInfo%Dly(I)%IceTemp
         WRITE(U1, 1012, ERR=812, ADVANCE='NO') LayerInfo%Dly(I)%AgeOfCurrentLayer
         WRITE(U1, 1015, ERR=812, ADVANCE='NO') LayerInfo%Dly(I)%NumLayers
         DO J = 1, LayerInfo%Dly(I)%NumLayers
            WRITE(U1, 1012, ERR=812, ADVANCE='NO') LayerInfo%Dly(I)%AgeOfLayer(J)
         END DO
         DO J = 1, LayerInfo%Dly(I)%NumLayers
            WRITE(U1, 1013, ERR=812, ADVANCE='NO') LayerInfo%Dly(I)%SurfTempFromLayer(J)
         END DO
         DO J = 1, LayerInfo%Dly(I)%NumLayers
            WRITE(U1, 1014, ERR=812, ADVANCE='NO') LayerInfo%Dly(I)%TotalHeat(J)
         END DO
         WRITE(U1, *)   ! end the line
      END DO
      CLOSE (U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      RETURN

 811  ErrorMessage = 'Error opening ' // TRIM(LayerFile)
      CALL PassMsg;  GOTO 898
 812  ErrorMessage = 'Error writing ' // TRIM(LayerFile)
      CALL PassMsg;  GOTO 898

      
 898  CLOSE (U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899
      ErrorLevel = 1
 899  ErrorMessage = '[traceback] WriteHeatLayerFile_LLTM...';  CALL PassMsg
      RETURN

 1001 FORMAT('Daily heat layer information for the Large Lake Thermodynamics Model.  Not particularly "human-readable".')
 1002 FORMAT('See the LLTM model code for format description.')
 1003 FORMAT(A3, ', lake name')
 1004 FORMAT(A10, ', start date')
 1005 FORMAT(A10, ', end date')
 1011 FORMAT(A10)
 1012 FORMAT(',', E12.5E2)
 1013 FORMAT(',', E10.3E2)
 1014 FORMAT(',', E12.5E2)
 1015 FORMAT(',', I4)

      END SUBROUTINE WriteHeatLayerFile_LLTM



!-----------------------------------------------------------------------------
      !---------------------------------------------------------------------------------------
      !  Derived from Phillips and Irbe's IFYGL study on Lake Ontario, but
      !  with the fetch (and derived quantities) replaced with averages
      !  over their data set for each stability class in their equations.
      !  (Fetch of 10.5 nautical miles assumed for St. Clair.)
      !  19 January 1988, TEC II
      !
      !  Modified by TSH/TEC 10sep90 to keep NoOfLayers >= 0
      !  Mixing corrected by TEC 02nov90 and truncation of aging function omitted.
      !  Computation of ice pack added by TEC 01jul91, 22aug91, 10dec91.
      !  Updated for 4-byte dates and move to UNIX by TSH & TEC 14oct92.
      !  Simplified to eliminate unused parameters by TEC 15jun94.
      !  Modified by TSH to add/rearrange basin designations in late 1990s.
      !  Modified by TSH to make small performance improvements, 15Mar2017
      !---------------------------------------------------------------------------------------
      SUBROUTINE OverWaterCorrection(WindSpeedOverLand,   AirTempOverLand,       &
                                     DewPtTempOverLand,   WaterTemp,             &
                                     WindRatio,           AirTempCorrection,     &
                                     DewPtTempCorrection)
      IMPLICIT NONE
      REAL,    INTENT(IN)  :: WindSpeedOverLand, AirTempOverLand, DewPtTempOverLand, WaterTemp
      REAL,    INTENT(OUT) :: WindRatio, AirTempCorrection, DewPtTempCorrection
      
      INTEGER  ::  Stability
      INTEGER, DIMENSION(7), PARAMETER :: StabilityIndex= (/ 1, 2, 2, 3, 4, 4, 5 /)

      REAL  :: WindSpeedOverWater, AirTempOverWater, DewPtTempOverWater
      REAL  :: Ta, Tw

      REAL, DIMENSION(5,3) :: C
      REAL, DIMENSION(5,3), PARAMETER :: CSup = RESHAPE (                  &
                   (/ 3.132,  2.795,  1.607,  2.740,  3.374,               &
                     -1.333, -0.321,  0.290,  1.485,  1.822,               &
                     -4.499,  0.484, -0.350, -0.160, -0.037 /) , (/5,3/) )
     
      REAL, DIMENSION(5,3), PARAMETER :: CMic = RESHAPE (                  &
                   (/ 3.132,  2.795,  1.607,  2.740,  3.374,               &
                     -1.333, -0.321,  0.290,  1.485,  1.822,               &
                     -4.499,  0.484, -0.350, -0.160, -0.037 /) , (/5,3/) )
     
      REAL, DIMENSION(5,3), PARAMETER :: CHur = RESHAPE (                  &
                   (/ 3.132,  2.795,  1.607,  2.740,  3.374,               &
                     -1.333, -0.321,  0.290,  1.485,  1.822,               &
                     -4.499,  0.484, -0.350, -0.160, -0.037 /) , (/5,3/) )
     
      REAL, DIMENSION(5,3), PARAMETER :: CGeo = RESHAPE (                  &
                   (/ 3.132,  2.795,  1.607,  2.740,  3.374,               &
                     -1.333, -0.321,  0.290,  1.485,  1.822,               &
                     -4.499,  0.484, -0.350, -0.160, -0.037 /) , (/5,3/) )
     
      REAL, DIMENSION(5,3), PARAMETER :: CStc = RESHAPE (                  &
                   (/ 2.640,  2.350,  1.141,  2.687,  3.317,               &
                     -2.034, -0.696,  0.290,  2.110,  2.945,               &
                     -5.115,  0.240, -0.350, -0.160,  0.790 /) , (/5,3/) )

      REAL, DIMENSION(5,3), PARAMETER :: CEri = RESHAPE (                  &
                   (/ 3.132,  2.795,  1.607,  2.740,  3.374,               &
                     -1.333, -0.321,  0.290,  1.485,  1.822,               &
                     -4.499,  0.484, -0.350, -0.160, -0.037 /) , (/5,3/) )

      REAL, DIMENSION(5,3), PARAMETER :: COnt = RESHAPE (                  &
                   (/ 3.132,  2.795,  1.607,  2.740,  3.374,               &
                     -1.333, -0.321,  0.290,  1.485,  1.822,               &
                     -4.499,  0.484, -0.350, -0.160, -0.037 /) , (/5,3/) )

      REAL, DIMENSION(5,3), PARAMETER :: CHGb = RESHAPE (                  &
                   (/ 3.132,  2.795,  1.607,  2.740,  3.374,               &
                     -1.333, -0.321,  0.290,  1.485,  1.822,               &
                     -4.499,  0.484, -0.350, -0.160, -0.037 /) , (/5,3/) )

      !
      !  Inline function
      !
      Stability(Ta, Tw) = StabilityIndex(MIN(MAX(INT((Ta-Tw)/3.5),-3),3)+4)

      !
      !  Assign into the active C() array
      !      
      SELECT CASE (g_LkNum)
         CASE (1)
              C(:,:) = CSup
         CASE (2)
              C(:,:) = CMic
         CASE (3)
              C(:,:) = CHur
         CASE (4)
              C(:,:) = CGeo
         CASE (5)
              C(:,:) = CStc
         CASE (6)
              C(:,:) = CEri
         CASE (7)
              C(:,:) = COnt
         CASE (8)
              C(:,:) = CHGb
         CASE DEFAULT
              GOTO 701
      END SELECT

      !
      !  Compute the corrections, based on the Stability function
      !
      SELECT CASE (Stability(AirTempOverLand, WaterTemp))

         CASE (1)
           WindSpeedOverWater = C(1,1) + 1.05 * WindSpeedOverLand
           AirTempOverWater   = C(1,2) + 0.60 * AirTempOverLand             &
                                       + 0.54 * WaterTemp
           DewPtTempOverWater = C(1,3) + 0.56 * DewPtTempOverLand           &
                                       + 0.46 * WaterTemp

         CASE (2)
           WindSpeedOverWater = C(2,1) + 1.01 * WindSpeedOverLand
           AirTempOverWater   = C(2,2) + 0.67 * AirTempOverLand             &
                                       + 0.42 * WaterTemp
           DewPtTempOverWater = C(2,3) + 0.94 * DewPtTempOverLand           &
                                       + 0.11 * WaterTemp

         CASE (3)
           WindSpeedOverWater = C(3,1) + 0.92 * WindSpeedOverLand           &
                                       - 0.28 * (AirTempOverLand-WaterTemp)
           AirTempOverWater   = C(3,2) + 0.47 * AirTempOverLand             &
                                       + 0.52 * WaterTemp
           DewPtTempOverWater = C(3,3) + 0.72 * DewPtTempOverLand           &
                                       + 0.31 * WaterTemp

         CASE (4)
           WindSpeedOverWater = C(4,1) + 0.49 * WindSpeedOverLand           &
                                       - 0.02 * AirTempOverLand
           AirTempOverWater   = C(4,2) + 0.29 * AirTempOverLand             &
                                       + 0.65 * WaterTemp
           DewPtTempOverWater = C(4,3) + 0.44 * DewPtTempOverLand           &
                                       + 0.55 * WaterTemp

         CASE (5)
           WindSpeedOverWater = C(5,1) + 0.32 * WindSpeedOverLand           &
                                       - 0.02 * AirTempOverLand
           AirTempOverWater   = C(5,2) + 0.30 * AirTempOverLand             &
                                       + 0.56 * WaterTemp
           DewPtTempOverWater = C(5,3) + 0.43 * DewPtTempOverLand           &
                                       + 0.53 * WaterTemp

      END SELECT

      WindRatio = WindSpeedOverWater / WindSpeedOverLand
      IF (WindRatio .LT. 0.01) WindRatio = 0.01
      AirTempCorrection = AirTempOverLand - AirTempOverWater
      DewPtTempCorrection = DewPtTempOverLand - DewPtTempOverWater
      RETURN

      !
      !
  701 ErrorMessage = 'Invalid lake number in routine OverWaterCorrection.';   CALL PassMsg
  
      ErrorLevel = 1
      ErrorMessage = '[traceback]  OverWaterCorrection...';   CALL PassMsg
      RETURN
      
      END SUBROUTINE OverWaterCorrection

!-----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !     Coefficient - BULK AERODYNAMIC COEFFICIENT FOR HEAT
      !     Z0 - ROUGHNESS HEIGHT IN METERS
      !     Al - STABILITY LENGTH IN METERS
      !  FROM
      !     U  - WIND SPEED IN METERS/SEC
      !     Td - AIR-WATER TEMPERATURE DIFFERENCE IN DEGREES K
      !     Z  - REFERENCE HEIGHT IN METERS
      !     Ak - VON KARMANS COEFFICIENT# 0.41
      !     Alph - FROM CHARNOCKS RELATIONSHIP# 0.0101
      !------------------------------------------------------------------------
      REAL FUNCTION Coefficient (U, Td)
      IMPLICIT NONE
      REAL, INTENT(IN) :: U, Td

      REAL, PARAMETER  :: A1 = 16.00
      REAL, PARAMETER  :: A2 = 16.00
      REAL, PARAMETER  :: A3 =  5.20
      REAL, PARAMETER  :: A4 =  5.20
      REAL, PARAMETER  :: Ak =  0.41
      REAL, PARAMETER  :: B  =  1.00
      REAL, PARAMETER  :: G  =  9.81
      REAL, PARAMETER  :: Z  =  8.00
      REAL, PARAMETER  :: Alph = 0.0101
      REAL, PARAMETER  :: Tbar = 276.5

      INTEGER :: K
      REAL    :: Ustar, Z0, S, Al, X, X2, Sy2, Sy1, TeeDee

      Ustar = 0.1
      
      !
      !     Initial guess for Z0.
      !
      Z0 = Alph * Ustar * Ustar / G
      TeeDee = Td
      IF (ABS(Td) .LT. 1.0E-7) TeeDee = SIGN(1.0E-7, Td)
      S = U * U * Tbar / (G * TeeDee)
      
      !
      !     Initial guess for L.
      !
      Al = S / LOG(Z / Z0)
      IF (ABS(Al) .GT. 3.E6) Al = SIGN(3.E6, Al)
      DO K = 1, 10
         IF (Al .LE. 0.) THEN
            !
            !  Unstable section.
            !
             X = (1.0 - A1 * Z / Al)**0.25
             X2 = (1.0 - A2 * Z / Al)**0.25
             Sy2 = 2. * LOG((1. + X2 * X2) / 2.)
             Sy1 = 2. * LOG((1. + X) / 2.) + LOG((1. + X * X) / 2.)          &
                   - 2. * ATAN(X) + 1.570796
         ELSE
            !
            !  Stable section.
            !
            Sy1 = -A3 * Z / Al
            Sy2 = -A4 * Z / Al
            IF (Z / Al .GT. 1.0) THEN
               !
               !  Strongly stable section.
               !
               Sy1 = -A3 * (1.0 + LOG(Z / Al))
               Sy2 = -A4 * (1.0 + LOG(Z / Al))
            END IF
         END IF
         !
         !       Calculate Ustar, Z0, Coefficient, and Al.
         !
         Ustar = U * Ak / (LOG(Z / Z0) - Sy1)
         Z0 = Alph * Ustar * Ustar / G
         Coefficient = Ak * Ustar / (U * (LOG(Z / Z0) - Sy2) * B)
         Al = S * (LOG(Z / Z0) - Sy2) / ((LOG(Z / Z0) - Sy1)**2) * B
      END DO
      
      RETURN

      END FUNCTION Coefficient


!-----------------------------------------------------------------------------
      !-----------------------------------------------------------------------------
      !
      !  By Thomas E. Croley II, using material developed by Jan Derecki, 6/87.
      !
      !  Determine water surface elevation and temperature at the end of the day
      !  and evaporation amount during the day by balancing the heat budget, the
      !  aerodynamic law (relationship of evaporation to water surface
      !  temperature), the mass budget, average surface temperature during the
      !  day, and average surface elevation during the day.
      !
      !  The date (used to determine date-dependent things like average 
      !  incoming radiation amounts).
      !     Day, Month, Year = calendar day, month, & year
      !
      !  The met data values (airtemp, dewpoint, wind speed) that are passed 
      !  in may come from a variety of sources. If they come from shore-based 
      !  stations (the way the LLTM originally was designed) then we should 
      !  apply corrections to translate the values from overland to overwater 
      !  estimates.
      !  But they also may come from direct overwater measurements (e.g. the 
      !  flux towers or ships), in which case we don't want to modify them.
      !  The ApplyCorrection variable will tell us if we need to apply these
      !  standard corrections.
      !     WindSpeed        = average daily windspeed in meters per second   
      !     AirTemperature   = temperature of the air in degrees Celsius      
      !     DewPtTemperature = dew point temperature of the air (degrees C)   
      !     ApplyCorrection  = flag
      !
      !  This version can use either direct estimates of radiation (incident 
      !  and net longwave) or the historical method of using cloud cover to
      !  compute the radition estimates.  If the radiation values are
      !  supplied, then it will use those, falling back to cloud cover if they
      !  are absent.
      !     IncidentRad     = average daily incident radiation (watts/m2)
      !     NetLongWaveRad  = average daily NLW radiation (watts/m2)
      !     CloudCover      = average daily cloud cover fraction (0 to 1.0)
      !
      !  Other terms that I think were included by TEC mainly in anticipation of 
      !  potential future use.  They are all currently being set to 0.0
      !     Runoff        = overland flow into the lake for the day in cubic meters
      !     Precipitation = overlake precipitation for the day in cubic meters
      !     Inflow        = sum of other inflows and diversions into the lake 
      !                     for the day in cubic meters
      !     Outflow       = sum of outflows from the lake for the day in cubic meters
      !     OtherTerms    = heat flux terms not otherwise considered in calories
      !     SurfaceElevation = elevation of the lake surface at the 
      !                        BEGINNING OF DAY in meters (on input)
      !
      !  OUTPUTS:
      !     SurfaceElevation = elevation of the lake surface at END OF DAY in meters
      !     Evaporation      = Evaporation from lake for day in cubic meters
      !
      !
      !  Note that all of the heat calculations being done here use units of 
      !  cal/cm2/day (a.k.a. langleys).  The units used for input/output, though,
      !  are watts/m2.  If you change anything in this code, don't accidentally
      !  use the wrong units (like I first did when rearranging things in 2017).
      !
      !-----------------------------------------------------------------------------
      SUBROUTINE BalanceElevAndTempAndEvap(               &
             Day, Month, Year, DailyMet,                  & 
             Runoff, Precipitation, Inflow, Outflow,      & 
             OtherTerms, SurfaceElevation, Evaporation)
     
      IMPLICIT NONE
      INTEGER, INTENT(IN)    :: Day, Month, Year
      REAL,    INTENT(IN)    :: Runoff, Precipitation, Inflow, Outflow, OtherTerms
      REAL,    INTENT(INOUT) :: SurfaceElevation
      REAL,    INTENT(OUT)   :: Evaporation
      TYPE (TMetData), INTENT(INOUT)  :: DailyMet        ! met data for a single day (in & out)

      INTEGER :: IterationCount
      REAL    :: H, EOI, Snw, IceA, VpA, DRRI, T1, T2
      REAL    :: Area1, Depth1, Amount1, Temp1
      REAL    :: AirTemperature, DewPtTemperature, WindSpeed
      REAL    :: IncidentRad, NetLongWaveRad, CloudCover
      REAL    :: TIceArea, TIceDepth, TIceAmnt, TIceTemp
      REAL    :: OverIceTemp, OverIceWind, IceHeatT
      REAL    :: SurfaceTemp_End, SurfaceTemp_Avg
      REAL    :: StoredHeatAtEndOfDay
      REAL    :: TrialHeat, TestVal
      REAL    :: EvapAdvect, Incident, NetLongWave, IncomingLW
      REAL    :: TrialValue
      REAL    :: WindRatio, AirTempAdjustment, DewPtAdjustment
      REAL    :: VaporPressureOfAir, VaporPressureOnLake
      REAL    :: FractionInIce, FractionInBareIce, EvapOverIce
      REAL    :: FractionInNewSnow, FractionInOldSnow, FractionInMeltingSnow
      REAL    :: FractionInOpenWater, AtmosphereIceFlux
      REAL    :: VaporPressureOnIce, VaporPressureOverIceSurface
      REAL    :: TransferCoefficientW
      REAL    :: TransferCoefficientI, OverLakeTemp, AIceTemp
      REAL    :: WindRatioI, AirTempAdjustmentI, DewPtAdjustmentI, OverLakeWind
      REAL    :: WaterIceFlux, ExcessFromIceMelt
      REAL    :: DeltaStoredHeat
      
      REAL    :: AirTempNew, DewPtTempNew, WindSpeedNew

      !
      !  Function and dummy variable names for the inline functions
      !
      REAL    :: VaporPressureOverWater, VaporPressureOverIce, IceHeat
      REAL    :: XTemp, XIceAmount

      !
      !  inline functions
      !
      VaporPressureOverWater(XTemp) =                                 &
         10.0**(-7.90298 * ((373.16 / (XTemp + 273.16)) - 1.0)        &
         + 5.02808 * LOG10((373.16 / (XTemp + 273.16))) - 1.3816      &
         * 0.0000001 * (10.0**(11.334 * (1.0 - (1.0 / (373.16               &
         / (XTemp + 273.16))))) - 1.0) + 8.1328 * 0.001               &
         * (10.0**(-3.49149 * ((373.16 / (XTemp + 273.16)) - 1.0))    &
         - 1.0) + 3.0057149)

      VaporPressureOverIce(XTemp) =                                   &
         10.0**(-9.09718 * (273.16 / (XTemp + 273.16) - 1.0)          &
         -3.56654 * LOG10(273.16 / (XTemp + 273.16)) + 0.876793       &
         * (1.0 - 1.0 / (273.16 / (XTemp + 273.16))) + 0.785835)

      IceHeat(XIceAmount) = (XIceAmount * 79.7 * 1000000.0)
         
      !
      !  Assign global and DailyMet values into local variables.
      !  These met variables have already been "filled in" if missing.
      !  Cloud cover is converted from % to a fraction (0.0-1.0) because that is
      !  how Dr. Croley programmed things in the original LLTM, and subsequent 
      !  uses of the value assume those units.
      !
      AirTemperature   = DailyMet%AirTemp
      DewPtTemperature = DailyMet%DewPt
      WindSpeed        = DailyMet%WindSpeed
      IF (DailyMet%Cloud .GT. -0.1) THEN
         CloudCover    = DailyMet%Cloud / 100.0       !  convert from % to a fraction (0.0-1.0)
      ELSE
         CloudCover    = -99.999
      END IF
      IncidentRad      = DailyMet%Incident            !  watts/m2
      NetLongWaveRad   = DailyMet%NetLW               !  watts/m2

      !
      !  Assign values from the global ModelState variable into local
      !  simple variables that are used to keep things more readable.
      !  We just need to be sure that ModelState is kept up-to-date as 
      !  things progress, otherwise subroutines and functions will be
      !  using incorrect values.
      !  
      !  These first few variables represent the initial guesses at the end
      !  state, and will almost certainly change as we do this routine, since
      !  they assume the lake stays exactly constant.
      !
      TIceArea              = g_IceArea
      TIceAmnt              = g_IceAmount
      TIceTemp              = g_IceTemp
      SurfaceTemp_End       = g_SurfTemp
      SurfaceTemp_Avg       = g_SurfTemp       ! mean of start and end temps
      StoredHeatAtEndOfDay  = g_StoredHeat

      !
      !  The big iteration loop. This is done up to 6 times in
      !  an attempt to reach a kind of "steady state".
      !
      IceHeatT = IceHeat(TIceAmnt)
      TrialHeat = -100. * (StoredHeatAtEndOfDay - IceHeatT)
      IterationCount = 0
      TestVal = ABS(((StoredHeatAtEndOfDay - IceHeatT) - TrialHeat) /    &
                     (StoredHeatAtEndOfDay - IceHeatT))
      DO WHILE (TestVal .GT. 0.001)
         IceHeatT = IceHeat(TIceAmnt)
         TrialHeat = StoredHeatAtEndOfDay - IceHeatT
         TrialValue = SurfaceTemp_End
         
         !
         !  If needed, adjust the meteorology data
         !
         IF (ModelCfg%ApplyOverwaterCorrection) THEN
            CALL OverWaterCorrection(WindSpeed, AirTemperature,          &
                 DewPtTemperature, SurfaceTemp_Avg, WindRatio,           &
                 AirTempAdjustment, DewPtAdjustment)
            IF (ErrorLevel .NE. 0) GOTO 701
         ELSE
            WindRatio         = 1.0
            AirTempAdjustment = 0.0
            DewPtAdjustment   = 0.0
         END IF
         OverLakeWind = WindSpeed * WindRatio
         OverLakeTemp = AirTemperature - AirTempAdjustment

         !
         !  
         VaporPressureOfAir  = VaporPressureOverWater(DewPtTemperature - DewPtAdjustment)
         VaporPressureOnLake = VaporPressureOverWater(SurfaceTemp_Avg)
         TransferCoefficientW = Coefficient(OverLakeWind,                   &
                                OverLakeTemp - SurfaceTemp_Avg)
         IF (ErrorLevel .NE. 0) GOTO 899
         
         Evaporation = g_EvapUnitsCoeff * TransferCoefficientW              &
                       * (VaporPressureOnLake - VaporPressureOfAir)         &
                       * OverLakeWind

         !
         !  Calculate, if needed, the radiation values.
         !   Method 1: input file contains cloud, calculate radiation from that.
         !   Method 2: input file contains incident and INCOMING longwave.  Use 
         !             incident as read, and calculate NetLW.
         !   Method 3: input file contains incident and NET longwave, Use directly.
         !
         !  Note that the resulting radiation flux values
         !  are expressed in langleys (cal/cm2), not watts/m2.
         !  (2.06346 cal/cm2/day = 1 watt/m2)
         !
         IF (ModelCfg%RadiationMethod .EQ. 1) THEN
            IF (CloudCover .LT. 0.0) THEN
               WRITE(ErrorMessage, 5001) Year, Month, Day;   CALL PassMsg
               GOTO 898
            END IF
            Incident = DailyIncidentSolarRadiation(Day, Month, Year, CloudCover)
            IF (ErrorLevel .NE. 0) GOTO 899                             
            
            VpA = VaporPressureOfAir
            NetLongWave = DailyNetLongwaveRadiation(SurfaceTemp_Avg,       &
                              OverLakeTemp, CloudCover, VpA, -9.9) 
            IF (ErrorLevel .NE. 0) GOTO 899
         ELSE IF (ModelCfg%RadiationMethod .EQ. 2) THEN
            Incident    = IncidentRad    * 2.06346       ! watts/m2 -> langleys
            IncomingLW  = NetLongWaveRad * 2.06346       ! watts/m2 -> langleys 
            NetLongWave = DailyNetLongwaveRadiation(SurfaceTemp_Avg,       &
                              -9.9, -9.9, -9.9, IncomingLW) 
         ELSE
            Incident    = IncidentRad    * 2.06346       ! watts/m2 -> langleys
            NetLongWave = NetLongWaveRad * 2.06346       ! watts/m2 -> langleys
         END IF

         !
         !  Estimate the air temperature over the ice area by using the
         !  overwater correction algorithm.
         !
         !  This is done regardless of the value of ApplyCorrection.  Is this
         !  the correct approach???   (TSH, 16Mar2017)
         !
         T1 = OverLakeTemp
         T2 = OverLakeTemp - 1.0       ! force at least one iteration        
         DO WHILE (ABS(T1-T2) .GT. 0.01)
            CALL OverWaterCorrection(WindSpeed, AirTemperature,               &
                 DewPtTemperature, MIN(T1, 0.0), WindRatioI,                  &
                 AirTempAdjustmentI, DewPtAdjustmentI)
            IF (ErrorLevel .NE. 0) GOTO 899
            T2 = T1
            T1 = AirTemperature - AirTempAdjustmentI
         END DO
         OverIceTemp = T1
         
         !
         TIceTemp = MIN(OverIceTemp, 0.0)                      ! trial value of ice temp
         AIceTemp = (g_IceTemp + TIceTemp) / 2.0               ! daily average value
         OverIceWind = WindSpeed * WindRatioI
         VaporPressureOverIceSurface =                                           &
               VaporPressureOverWater(DewPtTemperature - DewPtAdjustmentI)
         VaporPressureOnIce = VaporPressureOverIce(AIceTemp)
         TransferCoefficientI = Coefficient(OverIceWind, OverIceTemp-AIceTemp);   IF (ErrorLevel .NE. 0) GOTO 899
         EvapOverIce = g_EvapUnitsCoeff * TransferCoefficientI                   &
                        * (VaporPressureOnIce - VaporPressureOverIceSurface)     &
                        * OverIceWind
                        
         FractionInBareIce = 1.0
         FractionInNewSnow = 0.0
         FractionInOldSnow = 0.0
         FractionInMeltingSnow = 0.0
         FractionInIce = (g_IceArea + TIceArea) / 2.0 / g_LkArea
         FractionInOpenWater = 1.0 - FractionInIce

         DeltaStoredHeat = (Incident * 0.90)                                          &
                * (g_LkArea - (g_IceArea + TIceArea) / 2.) * 10000.                   &
              + NetLongWave * g_LkArea*10000.                                         &
              - (597.3 - 0.56 * SurfaceTemp_Avg + SurfaceTemp_Avg * 1.0)              &
                * Evaporation / 10.                                                   &
                * (g_LkArea - (g_IceArea + TIceArea) / 2.) * 10000. * 1.0             &
              - g_HeatUnitsCoeff * TransferCoefficientW                               &
                * (SurfaceTemp_Avg - OverLakeTemp) * OverLakeWind                     &
                * (g_LkArea - (g_IceArea + TIceArea) / 2.) * 10000.                   &
              + (Runoff + Inflow) * 1000000.                                          &
                * 1.0 * MAX(0., SurfaceTemp_Avg) * 1.0                                &
              + Precipitation * 1000000. * 1.0                                        &
                * (OverLakeTemp * 1.0 + MIN(SIGN(79.7, OverLakeTemp),0.))             &
                * FractionInOpenWater                                                 &
              - Outflow * 1000000. * 1.0 * SurfaceTemp_Avg * 1.0                      &
              + OtherTerms

         StoredHeatAtEndOfDay = g_StoredHeat + DeltaStoredHeat                ! calories

         !
         ! Somehow, StoredHeatAtEndofDay was going negative for StClair and
         ! crashing the program due to invalid floating point op.
         ! This is a kludge fix. Needs to be done better, but I just need 
         ! to get things working again. All I am trying to accomplish
         ! here is to keep the StoredHeatAtEndofDay "reasonable" when 
         ! things go bad. My assumption is that there was a fairly large
         ! heat loss calculated in order for this to trigger. I am gonna
         ! just call it a 3% loss of heat, which would seem to be fairly 
         ! significant to me, based on the typical values I saw calculated.
         ! Tim Hunter  03Dec2013
         !
         IF (StoredHeatAtEndOfDay .LT. 0) THEN
             StoredHeatAtEndOfDay = g_StoredHeat * 0.97
         END IF

         !
         DRRI = DailyReflectedRadiationFromIce(Incident,                                   &
                FractionInBareIce, FractionInNewSnow, FractionInOldSnow,                   &
                FractionInMeltingSnow)
         IF (ErrorLevel .NE. 0) GOTO 899
         
         IceA = (g_IceArea + TIceArea) / 2.0 * 10000.
         AtmosphereIceFlux = (Incident - DRRI) * IceA                                      &
              - (597.3 - 0.56 * AIceTemp + 79.7 + AIceTemp * 1.0)                          &
                * EvapOverIce / 10. * IceA * 1.0                                           &
              - g_HeatUnitsCoeff * TransferCoefficientI                                    &
                * (AIceTemp - OverIceTemp) * OverIceWind * IceA                            &
              + Precipitation * 1000000. * 1.0                                             &
                * OverIceTemp * 1.0 * FractionInIce

         !
         !
         OverLakeWind = WindSpeed * WindRatio
         g_AgeOfCurrentLayer = OverLakeWind * FractionInOpenWater
          
         !
         !  If negative, WaterIceFlux is the heat taken, by freezing the water,
         !  to keep water temperatures from going below freezing, in calories.
         !
         !  If positive, WaterIceFlux is the heat available to melt ice by lowering
         !  the water temperature toward freezing temperature, in calories.
         !
         H = HeatInStorage(0.0);                     IF (ErrorLevel .NE. 0) GOTO 899
         WaterIceFlux = StoredHeatAtEndOfDay - H
         StoredHeatAtEndOfDay = StoredHeatAtEndOfDay - WaterIceFlux

         !
         !  Compute ice cover.
         !  Trial values (TIceArea, TIceDepth, TIceAmnt and TIceTemp) were already
         !  assigned. IceCover() will modify them.
         !
         Area1   = g_IceArea
         Depth1  = g_IceDepth
         Amount1 = g_IceAmount
         Temp1   = g_IceTemp
         EOI = EvapOverIce / 10 * (Area1 + TIceArea) / 2.0 * 10000. / 1000000.
         
         Snw = Precipitation * MIN(SIGN(1., OverIceTemp), 0.) * FractionInIce
         CALL IceCover(Area1, Depth1, Amount1, Temp1,                             &
               WaterIceFlux, AtmosphereIceFlux,                                   &
               EOI, Snw,                                                          &
               TIceArea, TIceDepth, TIceAmnt, TIceTemp,                           &
               ExcessFromIceMelt);   IF (ErrorLevel .NE. 0) GOTO 899
         StoredHeatAtEndOfDay = StoredHeatAtEndOfDay + ExcessFromIceMelt

         SurfaceTemp_End = WaterSurfaceTemperature(StoredHeatAtEndOfDay);    IF (ErrorLevel .NE. 0) GOTO 899
         IterationCount = IterationCount + 1
         SurfaceTemp_Avg = (g_SurfTemp + SurfaceTemp_End) / 2.
         IF (IterationCount .EQ. 6) THEN
            SurfaceTemp_End = (SurfaceTemp_End + TrialValue) / 2.0
            StoredHeatAtEndOfDay = (StoredHeatAtEndOfDay + TrialHeat          &
                                   + IceHeat(TIceAmnt)) / 2.0
         END IF
         
         !
         !  Force exit condition if we have reached max iterations
         !
         IceHeatT = IceHeat(TIceAmnt)
         IF (IterationCount .GT. 6) TrialHeat = StoredHeatAtEndOfDay - IceHeatT
         
         !
         !  Determine if we have reached exit condition
         !
         TestVal = ABS(((StoredHeatAtEndOfDay - IceHeatT) - TrialHeat) /    &
                        (StoredHeatAtEndOfDay - IceHeatT))
      END DO

      g_IceArea   = TIceArea
      g_IceDepth  = TIceDepth
      g_IceTemp   = TIceTemp
      g_IceAmount = TIceAmnt
      
      !
      !  Define the heat flux variables in watts/m2
      !    (2.06346 cal/cm2/day = 1 watt/m2)
      !  These are the values that will be written to the output file.
      !
      g_Incident = Incident / 2.06346
      
      DRRI = DailyReflectedRadiationFromIce(Incident,                      &
            FractionInBareIce, FractionInNewSnow, FractionInOldSnow,       &
            FractionInMeltingSnow);   IF (ErrorLevel .NE. 0) GOTO 899
      g_Reflect = 0.0 - (Incident * 0.10 * FractionInOpenWater             &
                       + DRRI * FractionInIce) / 2.06346
      
      g_NetLW = NetLongWave / 2.06346

      g_Latent = 0.0 - ((597.3 - 0.56 * SurfaceTemp_Avg)                   &
           * Evaporation / 10. * 1.0 * FractionInOpenWater                 &
           + (597.3 - 0.56 * AIceTemp + 79.7)                              &
           * EvapOverIce / 10. * 1.0 * FractionInIce) / 2.06346
           
      g_Sensible = 0.0 - g_HeatUnitsCoeff * (TransferCoefficientW          &
           * (SurfaceTemp_Avg - OverLakeTemp) * OverLakeWind               &
           * FractionInOpenWater + TransferCoefficientI                    &
           * (AIceTemp - OverIceTemp) * OverIceWind * FractionInIce)       &
           / 2.06346
      
      EvapAdvect = 0.0 - (SurfaceTemp_Avg * 1. * Evaporation / 10.         &
           * FractionInOpenWater + AIceTemp * 1.0 * EvapOverIce / 10.      &
           * FractionInIce) / 2.06346
      g_Advection = (((Runoff + Inflow) * MAX(0.,SurfaceTemp_Avg)          &
           * 1.0 - Outflow * SurfaceTemp_Avg * 1.0                         &
           + Precipitation * (OverLakeTemp * 1.0                           &
           + MIN(SIGN(79.7, OverLakeTemp), 0.)) * FractionInOpenWater)     &
           + Precipitation * OverIceTemp * 1.0 * FractionInIce)            &
           * 1000000. * 1.0 / g_LkArea / 10000. / 2.06346 + EvapAdvect

      Evaporation = (Evaporation * FractionInOpenWater                     &
                    + EvapOverIce * FractionInIce) / 1000. * g_LkArea

      !
      !  Compute new surface elevation of the lake
      !  
      SurfaceElevation = SurfaceElevation                                  &
            + (Precipitation + Runoff + Inflow - Outflow - Evaporation)    &
            / g_LkArea
            
      g_SurfTemp   = SurfaceTemp_End
      g_StoredHeat = StoredHeatAtEndOfDay

      !
      !  Superimpose and age heat or heat deficiency layers.
      !
      CALL Superposition();   IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Final transformed values of met data variables
      !
      AirTempNew = (AirTemperature - AirTempAdjustment) * FractionInOpenWater      &
                 + (AirTemperature - AirTempAdjustmentI) * FractionInIce
      DewPtTempNew = (DewPtTemperature - DewPtAdjustment) * FractionInOpenWater    &
                   + (DewPtTemperature - DewPtAdjustmentI) * FractionInIce
      WindSpeedNew = WindSpeed * WindRatio * FractionInOpenWater                   &
                   + WindSpeed * WindRatioI * FractionInIce

            
      !
      !  Assign values to the DailyMet variable to reflect any adjustments
      !  applied during this subroutine. The main program can then use these
      !  (possibly adjusted) values in the output file. These are the values
      !  actually used to perform the thermodynamic calcs.
      !
      DailyMet%AirTemp    = AirTempNew
      DailyMet%DewPt      = DewPtTempNew
      DailyMet%WindSpeed  = WindSpeedNew
      
      RETURN
      
      !
      !  Error handling
      !
  701 ErrorMessage = 'Error while attempting to compute overwater corrections.';   CALL PassMsg
      GOTO 899


  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] BalanceElevAndTempAndEvap...';  CALL PassMsg
      RETURN

 5001 FORMAT('Missing cloud cover AND radiation is invalid.  Date=', I4, 2('-', I2.2))
      END SUBROUTINE BalanceElevAndTempAndEvap

!----------------------------------------------------------------------------
      SUBROUTINE Superposition()
      IMPLICIT NONE
      REAL :: SH, W, TotH
      
      SH  = g_StoredHeat
      W   = g_AgeOfCurrentLayer
      

      IF (g_TotalHeat(g_NumLayers) .LE. g_Hd) THEN
         !
         !  Yesterday, lake was pre-spring-turnover...
         !
         IF (SH / 1.E+20 .LE. g_Hd) THEN
            !
            !  Today, lake is still pre-spring-turnover...
            !
            !     Remove heat (deficiency) superposition layers if required...
            !
            TotH = g_TotalHeat(g_NumLayers)
            DO WHILE ((SH / 1.E+20 .GE. TotH) .AND. (g_NumLayers .GT. 0))
               g_NumLayers = g_NumLayers - 1
               TotH = g_TotalHeat(g_NumLayers)
            END DO
            
            !
            !     Superimpose todays heat deficiency layer, "age" all layers, and
            !     collapse earlier additions that are older than mixing function limit.
            !
            CALL AddAgeAndCollapse(W, g_Mxf); IF (ErrorLevel .NE. 0) RETURN
         ELSE
            !
            !  Today, lake is now just post-spring-turnover
            !  (and pre-fall-turnover)...
            !
            g_NumLayers     = 1
            g_TotalHeat(1)  = g_StoredHeat / 1.0E+20
            g_AgeOfLayer(1) = W
         END IF
      ELSE
         !
         !  Yesterday, lake was pre-fall-turnover...
         !
         IF (SH / 1.E+20 .GT. g_Hd) THEN
            !
            !   Today, lake is still pre-fall-turnover...
            !
            !      Remove heat superposition layers if required...
            !
            TotH = g_TotalHeat(g_NumLayers)
            DO WHILE ((SH / 1.E+20 .LE. TotH) .AND. (g_NumLayers .GT. 0))
               g_NumLayers = g_NumLayers - 1
               TotH = g_TotalHeat(g_NumLayers)
            END DO
            !
            !       Superimpose todays heat addition layer, "age" all layers, and
            !       collapse earlier additions that are older than mixing function limit.
            !
            CALL AddAgeAndCollapse(W, g_Mxs);  IF (ErrorLevel .NE. 0) GOTO 899
         ELSE
            !
            !  Today, lake is now just post-fall-turnover
            !  (and pre-spring-turnover)...
            !
            g_NumLayers     = 1
            g_TotalHeat(1)  = g_StoredHeat / 1.0E+20
            g_AgeOfLayer(1) = W
         END IF
      END IF

      g_AgeOfLayer(g_NumLayers + 1) = 0.
      g_TotalHeat(g_NumLayers + 1) = 0.

      RETURN

  899 ErrorMessage = '[traceback] SuperPosition...';  CALL PassMsg
      RETURN
      
      END SUBROUTINE Superposition

!----------------------------------------------------------------------------
      SUBROUTINE AddAgeAndCollapse (W, AgeLimit)
      IMPLICIT NONE
      REAL, INTENT(IN) :: W, AgeLimit

      INTEGER :: LayerNo

      !
      !  Superimpose todays heat addition layer.
      !      a) Reserve one layer for zero on top
      !      b) See end of routine: SUPERIMPOSE.
      !
      g_NumLayers = g_NumLayers + 1
      IF (g_NumLayers .GT. MaxNoOfLayers - 1) THEN       ! 
         WRITE(ErrorMessage, 5001) MaxNoOfLayers;  CALL PassMsg;  GOTO 898
      END IF
      if (g_NumLayers .GT. g_MaxNumLayersUsed) g_MaxNumLayersUsed = g_NumLayers

      g_TotalHeat(g_NumLayers) = g_StoredHeat / 1.0E+20

      !
      !  "Age" all layers...
      !
      DO LayerNo = 1, g_NumLayers
         g_AgeOfLayer(LayerNo) = g_AgeOfLayer(LayerNo) + W
      END DO

      !
      !  Collapse earlier additions that are older than mixing function limit.
      !
      IF (g_NumLayers .GT. 1 .AND. g_AgeOfLayer(1) .GE. AgeLimit) THEN
         DO WHILE (g_NumLayers .GT. 1 .AND. g_AgeOfLayer(2) .GE. AgeLimit)
            g_NumLayers = g_NumLayers - 1
            DO LayerNo = 1, g_NumLayers
               g_TotalHeat(LayerNo)  = g_TotalHeat(LayerNo + 1)
               g_AgeOfLayer(LayerNo) = g_AgeOfLayer(LayerNo + 1)
            END DO
         END DO
      END IF

      RETURN

  898 ErrorLevel = 1
      ErrorMessage = '[traceback] AddAgeAndCollapse...';  CALL PassMsg
      RETURN
      
 5001 FORMAT('MaxNoOfLayers (', I0, ') exceeded.')
      
      END SUBROUTINE AddAgeAndCollapse

!----------------------------------------------------------------------------
      !----------------------------------------------------------------------
      !
      !-----------------------------------------------------------------------
      FUNCTION HeatInStorage (WSTemp)    Result(HeatIS)
      IMPLICIT NONE
      REAL, INTENT(IN) :: WSTemp      ! water surface temperature (deg C)
      REAL             :: HeatIS
      INTEGER :: R
      REAL    :: P2, P3, P4, P5, P6, P7, P8
      REAL    :: M, N, T, TodaysWind
      
      HeatIS = MissingData_Real           ! eliminate compiler warning about possible uninitialized value

      P2  = ModelParms%Parms(2)
      P3  = ModelParms%Parms(3)
      P4  = ModelParms%Parms(4)
      P5  = ModelParms%Parms(5)
      P6  = ModelParms%Parms(6)
      P7  = ModelParms%Parms(7)
      P8  = ModelParms%Parms(8)
      TodaysWind = g_AgeofCurrentLayer
      
      IF (g_TotalHeat(g_NumLayers) .LE. g_Hd) THEN

         !
         !      Yesterday, lake was pre-spring-turnover...
         !
         IF (WSTemp .LE. 3.98) THEN

            !
            !  Today, lake is still pre-spring-turnover...
            !
            N = g_AgeOfLayer(1) + TodaysWind
            M = MixingFunction(g_rCV, g_Mf1, g_Mf2, P2, P3, P4, P8, N);     IF (ErrorLevel .NE. 0) GOTO 899
            T = 3.98 + (g_TotalHeat(1) - g_Hd) * M
            R = 1
            DO WHILE (R .LE. g_NumLayers .AND. WSTemp .LT. T)
               R = R + 1
               N = g_AgeOfLayer(R) + TodaysWind
               M = MixingFunction(g_rCV, g_Mf1, g_Mf2, P2, P3, P4, P8, N);  IF (ErrorLevel .NE. 0) GOTO 899
               T = T + (g_TotalHeat(R) - g_TotalHeat(R-1)) * M 
            END DO
            M = MixingFunction(g_rCV, g_Mf1, g_Mf2, P2, P3, P4, P8, N);     IF (ErrorLevel .NE. 0) GOTO 899
            T = T - (g_TotalHeat(R) - g_TotalHeat(R - 1)) * M
            M = MixingFunction(g_rCV, g_Mf1, g_Mf2, P2, P3, P4, P8, N);     IF (ErrorLevel .NE. 0) GOTO 899
            HeatIS = (g_TotalHeat(R-1) + (WSTemp - T) / M) * 1.0E+20
         ELSE
            !
            !  Today, lake is just now post-spring-turnover...
            !
            M = MixingFunction(g_rCV, g_Ms1, g_Ms2, P5, P6, P7, P8, TodaysWind);     IF (ErrorLevel .NE. 0) GOTO 899
            HeatIS = (g_Hd + (WSTemp - 3.98) / M) * 1.0e+20
         END IF
      ELSE
         !
         !  Yesterday, lake was pre-fall-turnover...
         !
         IF (WSTemp .GE. 3.98) THEN
            !
            !  Today, lake is still pre-fall-turnover...
            !
            N = g_AgeOfLayer(1) + TodaysWind
            M = MixingFunction(g_rCV, g_Ms1, g_Ms2, P5, P6, P7, P8, N);      IF (ErrorLevel .NE. 0) GOTO 899
            T = 3.98 + (g_TotalHeat(1) - g_Hd)* M
            R = 1
            DO WHILE (R .LE. g_NumLayers .AND. WSTemp .GT. T)
               R = R + 1
               N = g_AgeOfLayer(R) + TodaysWind
               M = MixingFunction(g_rCV, g_Ms1, g_Ms2, P5, P6, P7, P8, N);   IF (ErrorLevel .NE. 0) GOTO 899
               T = T + (g_TotalHeat(R) - g_TotalHeat(R-1)) * M
            END DO
            M = MixingFunction(g_rCV, g_Ms1, g_Ms2, P5, P6, P7, P8, N);      IF (ErrorLevel .NE. 0) GOTO 899
            T = T - (g_TotalHeat(R) - g_TotalHeat(R-1)) * M 
            M = MixingFunction(g_rCV, g_Ms1, g_Ms2, P5, P6, P7, P8, N);      IF (ErrorLevel .NE. 0) GOTO 899
            HeatIS = (g_TotalHeat(R-1) + (WSTemp - T) / M) * 1.0E+20
         ELSE
            !
            !  Today, lake is just now post-fall-turnover...
            !
            M = MixingFunction(g_rCV, g_Mf1, g_Mf2, P2, P3, P4, P8, TodaysWind);  IF (ErrorLevel .NE. 0) GOTO 899
            HeatIS = (g_Hd + (WSTemp - 3.98) / M) * 1.0E+20
         END IF
      END IF
      RETURN

  899 ErrorMessage = '[traceback] HeatInStorage...';  CALL PassMsg
      RETURN      
      END FUNCTION HeatInStorage

!----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !     WaterSurfaceTemperature in degrees Celsius
      !     StoredHeat in Calories
      !------------------------------------------------------------------------
      FUNCTION WaterSurfaceTemperature(StoredHt)     Result(WSTemp)
      IMPLICIT NONE
      REAL, INTENT(IN) :: StoredHt
      REAL             :: WSTemp
      INTEGER :: R
      REAL  :: P2, P3, P4, P5, P6, P7, P8
      REAL  :: M, N, T, SHeat20, TodaysWind

      WSTemp = MissingData_Real           ! eliminate compiler warning about possible uninitialized value
      
      P2  = ModelParms%Parms(2)
      P3  = ModelParms%Parms(3)
      P4  = ModelParms%Parms(4)
      P5  = ModelParms%Parms(5)
      P6  = ModelParms%Parms(6)
      P7  = ModelParms%Parms(7)
      P8  = ModelParms%Parms(8)
      SHeat20    = StoredHt / 1.E+20
      TodaysWind = g_AgeOfCurrentLayer
      
      !      
      IF (g_TotalHeat(g_NumLayers) .LE. g_Hd) THEN
         !
         !      Yesterday, lake was pre-spring-turnover...
         !
         IF (SHeat20 .LE. g_Hd) THEN
            !
            !       Today, lake is still pre-spring-turnover...
            !
            T = 3.98
            R = 1
            DO WHILE ((R .LE. g_NumLayers) .AND. (SHeat20 .LT. g_TotalHeat(R)))
               N = g_AgeOfLayer(R) + TodaysWind
               M = MixingFunction(g_rCV, g_Mf1, g_Mf2, P2, P3, P4, P8, N);          IF (ErrorLevel .NE. 0) GOTO 899
               T = T + (g_TotalHeat(R) - g_TotalHeat(R-1)) * M
               R = R + 1
            END DO
            N = g_AgeOfLayer(R) + TodaysWind
            M = MixingFunction(g_rCV, g_Mf1, g_Mf2, P2, P3, P4, P8, N);             IF (ErrorLevel .NE. 0) GOTO 899
            WSTemp = T + (SHeat20 - g_TotalHeat(R-1)) * M
         ELSE
            !
            !  Today, lake is just now post-spring-turnover...
            !
            M = MixingFunction(g_rCV, g_Ms1, g_Ms2, P5, P6, P7, P8, TodaysWind);    IF (ErrorLevel .NE. 0) GOTO 899
            WSTemp = 3.98 + (SHeat20 - g_Hd) * M
         END IF
      ELSE
         !
         !  Yesterday, lake was pre-fall-turnover...
         !
         IF (SHeat20 .GE. g_Hd) THEN
            !
            !  Today, lake is still pre-fall-turnover...
            !
            T = 3.98
            R = 1
            DO WHILE (R .LE. g_NumLayers .AND. SHeat20 .GT. g_TotalHeat(R))
               N = g_AgeOfLayer(R) + TodaysWind
               M = MixingFunction(g_rCV, g_Ms1, g_Ms2, P5, P6, P7, P8, N);         IF (ErrorLevel .NE. 0) GOTO 899
               T = T + (g_TotalHeat(R) - g_TotalHeat(R - 1)) * M
               R = R + 1
            END DO
            N = g_AgeOfLayer(R) + TodaysWind
            M = MixingFunction(g_rCV, g_Ms1, g_Ms2, P5, P6, P7, P8, N);            IF (ErrorLevel .NE. 0) GOTO 899
            WSTemp = T + (SHeat20 - g_TotalHeat(R-1)) * M
         ELSE
            !
            !  Today, lake is just now post-fall-turnover...
            !
            M = MixingFunction(g_rCV, g_Mf1, g_Mf2, P2, P3, P4, P8, TodaysWind);   IF (ErrorLevel .NE. 0) GOTO 899
            WSTemp = 3.98 + (SHeat20 - g_Hd) * M
         END IF
      END IF

      RETURN

  899 ErrorMessage = '[traceback] WaterSurfaceTemperature...';  CALL PassMsg
      RETURN
      
      END FUNCTION WaterSurfaceTemperature

!------------------------------------------------------------------
      REAL FUNCTION MixingFunction(rCV, M1, M2, A, B, C, D, Age)
      IMPLICIT NONE
      REAL, INTENT(IN) :: rCV, M1, M2, A, B, C, D, Age
      REAL :: M

      M = D / (1.0 + A * EPN(-B * Age))

      IF (M .LT. M1) THEN
         MixingFunction = (2.0 - M / C) / M
      ELSE IF (M .LT. M2) THEN
         MixingFunction = 1.0 / M
      ELSE
         MixingFunction = 1.0 / rCV
      END IF
      RETURN
      END FUNCTION MixingFunction

!----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  DailyIncidentSolarRadiation = Daily total solar radiation incident to the
      !     water surface in langleys (calories/square cm)
      !  Dy, Mn, Yr = Calendar day, month, and year of the date.
      !  CloudCover = average daily fraction of sky covered with clouds
      !------------------------------------------------------------------------
      REAL FUNCTION DailyIncidentSolarRadiation (Dy, Mn, Yr, CloudCover)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: Dy, Mn, Yr
      REAL,    INTENT(IN) :: CloudCover       ! (range = 0.0 - 1.0)
      
      INTEGER :: II
      REAL    :: X, NDYS, NDY

      !
      !  INDPM  = number of days per month cumulative from the 1st of the year, days
      !
      INTEGER, PARAMETER :: INDPM(13) = (/  0,  31,  59,  90, 120, 151,       &
                                     181, 212, 243, 273, 304, 334, 365/)
      
      !
      !  R = AVERAGE MID-MONTH CLOUDLESS-DAY INSOLATION in langleys
      !          (note that before aug99 this was declared as a REAL array,
      !           but the DATA statement contains integers.  This violates
      !           good practice [and maybe the Fortran standard], and it
      !           was not necessary that R be a REAL, so I've changed the
      !           declaration to an INTEGER.   TSH 04aug99]
      !
      !         Dec,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Jan
      INTEGER, DIMENSION(14,8), PARAMETER :: R = RESHAPE(                 &
             (/ 150,190,330,490,640,770,790,770,670,510,360,200,150,190,   &  ! SUP
                190,220,340,480,620,700,730,700,610,490,360,240,190,220,   &  ! MIC
                150,190,330,490,640,770,790,770,670,510,360,200,150,190,   &  ! HUR
                150,190,330,490,640,770,790,770,670,510,360,200,150,190,   &  ! GEO
                190,220,335,470,610,710,755,725,635,515,380,250,190,220,   &  ! STC
                190,220,335,470,610,710,755,725,635,515,380,250,190,220,   &  ! ERI
                180,200,320,460,595,695,740,705,615,495,360,230,180,200,   &  ! ONT
                150,190,330,490,640,770,790,770,670,510,360,200,150,190/), &  ! HGB   
               SHAPE(R))


      !
      !  LkNum = 1 (Superior), 2 (Michigan), 3 (Huron without Georgian Bay),
      !          4 (Georgian Bay), 5 (St. Clair), 6 (Erie), 7 (Ontario),
      !          8 (Huron with Georgian Bay).
      !
      IF (Dy .GT. 15) THEN
         II = INDPM(Mn + 1) - INDPM(Mn)
         IF (II .EQ. 28 .AND. INT((FLOAT(Yr) + .5) / 4.) * 4 .EQ. Yr) II = 29
         NDYS = FLOAT(II)
         NDY  = FLOAT(Dy) - 15.
         X    = (R(Mn+2, g_LkNum) - R(Mn+1, g_LkNum)) / NDYS * NDY + R(Mn+1, g_LkNum)
      ELSE
         IF (Mn .EQ. 1) THEN
            II = 31
         ELSE
            II = INDPM(Mn) - INDPM(Mn-1)
            IF (II .EQ. 28 .AND. INT((FLOAT(Yr) + .5) / 4.) * 4 .EQ. Yr) II = 29
         END IF
         NDYS = FLOAT(II)
         NDY  = NDYS - 15. + FLOAT(Dy)
         X    = (R(Mn+1,g_LkNum) - R(Mn,g_LkNum)) / NDYS * NDY + R(Mn,g_LkNum)
      END IF
      DailyIncidentSolarRadiation = X * (0.355 + 0.68 * (1. - CloudCover))
      RETURN

      END FUNCTION DailyIncidentSolarRadiation

!----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  DailyReflectedRadiationFromIce = Daily solar radiation reflected
      !     from the ice surface in langleys (calories/square cm)
      !  Incident = Daily total solar radiation incident to the
      !     ice surface in langleys (calories/square cm)
      !  FractionInBareIce = fraction of ice surface area that is bare
      !  FractionInNewSnow = fraction of ice surface area covered with new snow
      !  FractionInOldSnow = fraction of ice surface area covered with old snow
      !  FractionInMeltingSnow = fraction of ice surface area covered with melting
      !     snow
      !------------------------------------------------------------------------
      REAL FUNCTION DailyReflectedRadiationFromIce                        &
             (Incident, FractionInBareIce, FractionInNewSnow,             &
              FractionInOldSnow, FractionInMeltingSnow)
      IMPLICIT NONE
      REAL, INTENT(IN) :: Incident, FractionInBareIce, FractionInNewSnow
      REAL, INTENT(IN) :: FractionInOldSnow, FractionInMeltingSnow
      REAL :: Residual

      IF (FractionInBareIce .LT. 0.  .OR.                                 &
          FractionInNewSnow .LT. 0.  .OR.                                 &
          FractionInOldSnow .LT. 0.  .OR.                                 &
          FractionInMeltingSnow .LT. 0.) THEN
         ErrorMessage = 'Error: A negative fraction was specified.';       CALL PassMsg
         GOTO 898
      END IF

      Residual = 1.0 - FractionInBareIce - FractionInNewSnow              &
                 - FractionInOldSnow - FractionInMeltingSnow
      IF (ABS(Residual) .GT. 0.0001) THEN
         ErrorMessage = 'Error: Fractions are unbalanced.';                        CALL PassMsg
         WRITE(ErrorMessage, 5001) 'FractionInBareIce    ', FractionInBareIce;     CALL PassMsg
         WRITE(ErrorMessage, 5001) 'FractionInNewSnow    ', FractionInNewSnow;     CALL PassMsg
         WRITE(ErrorMessage, 5001) 'FractionInOldSnow    ', FractionInOldSnow;     CALL PassMsg
         WRITE(ErrorMessage, 5001) 'FractionInMeltingSnow', FractionInMeltingSnow; CALL PassMsg
         WRITE(ErrorMessage, 5001) 'Incident Radiation   ', Incident;              CALL PassMsg
         GOTO 898
      END IF

      DailyReflectedRadiationFromIce = Incident                           &
           * (0.85 * FractionInNewSnow                                    &
            + 0.70 * FractionInOldSnow                                    &
            + 0.50 * FractionInMeltingSnow                                & 
            + 0.45 * FractionInBareIce)
      RETURN

  898 ErrorLevel = 1
      ErrorMessage = '[traceback] DailyReflectedRadiationFromIce...';  CALL PassMsg
      RETURN

 5001 FORMAT(A, ' = ', E13.6E2)      
      END FUNCTION DailyReflectedRadiationFromIce

!----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  DailyNetLongwaveRadiation = Daily net longwave radiation exchange between
      !        the atmosphere and the body of water in langleys (calories/square cm)
      !     AirTemperature = temperature of atmosphere above water body in degrees C
      !     WaterTemperature = (surface) temperature of water body in degrees Celcius
      !     CloudCover = average daily fraction of sky covered with clouds
      !     VaporPressureOfAir = mean vapor pressure in the air (@ 2m) in millibars
      !     IncLW = incoming longwave radiation in langleys (calories/square cm)
      !
      !  RadIn  = radiation air -> water   
      !  RadOut = radiation water -> air 
      !------------------------------------------------------------------------
      REAL FUNCTION DailyNetLongwaveRadiation (WaterTemperature,          &
                    AirTemperature, CloudCover, VaporPressureOfAir, IncLW)
      IMPLICIT NONE
      REAL, INTENT(IN) :: WaterTemperature, AirTemperature, CloudCover
      REAL, INTENT(IN) :: VaporPressureOfAir, IncLW
      REAL :: P9, Emissivity, ATemp, WTemp, RadIn, RadOut

      IF (ModelCfg%RadiationMethod .EQ. 1) THEN
         Emissivity = 0.53 + 0.065 * SQRT(VaporPressureOfAir)
         ATemp = AirTemperature   + 273.16
         P9    = ModelParms%Parms(9)
         RadIn = 1.17e-7 * ATemp ** 4 * Emissivity                        &
                 * (P9 + (1.0 - P9) * (1.0 - CloudCover)) 
      ELSE IF (ModelCfg%RadiationMethod .EQ. 2) THEN
         RadIn = IncLW
      ENDIF
      WTemp = WaterTemperature + 273.16
      RadOut = 0.97 * 1.17e-7 * WTemp ** 4
      DailyNetLongwaveRadiation = RadIn - RadOut
      RETURN

      END FUNCTION DailyNetLongwaveRadiation


!----------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !     Routine to iteratively solve finite difference approximations to the
      !     differential equations describing ice growth as a function of the heat
      !     fluxes to/from the ice pack.
      !
      !     Given:
      !       parameters: Beta1, Beta2, (,and LakeArea)
      !       beginning-of-time-interval values: Area1, Depth1, Volume1
      !       ice pack fluxes and other: WaterIceFlux, AtmosphereIceFlux,
      !                                  EvapOverIce (m3/day), Snow (m3/day)
      !
      !     => Estimate end-of-time-interval values: Area2, Depth2, Volume2
      !     => Estimate residual heat if ice melts completely: ExcessFromIceMelt
      !     => Correct WaterIceFlux & AtmosphereIceFlux: NOT CURRENTLY DONE.
      !
      !     Units: area (m2), depth (m), volume (m3), fluxes (cal/day),
      !            temperature (deg.C)
      !------------------------------------------------------------------------
      SUBROUTINE IceCover (Area1, Depth1, Volume1, T1, WaterIceFlux,         &
             AtmosphereIceFlux, EvapOverIce, Snow,                           &
             Area2, Depth2, Volume2, T2, ExcessFromIceMelt)
      IMPLICIT NONE
      REAL, INTENT(IN)  :: Area1, Depth1, Volume1, T1
      REAL, INTENT(IN)  :: WaterIceFlux, AtmosphereIceFlux
      REAL, INTENT(IN)  :: EvapOverIce, Snow
      REAL, INTENT(OUT) :: Area2, Depth2, Volume2, T2
      REAL, INTENT(OUT) :: ExcessFromIceMelt

      REAL, PARAMETER :: dT = 1.0          ! 1.0 day
      REAL, PARAMETER :: Rho = 1000000.0   ! density of water & ice, gm/m3
      REAL, PARAMETER :: Fusion = 79.7     ! heat of fusion of H2O, cal/gm

      REAL :: Beta1, Beta2
      
      INTEGER :: IterationCount
      REAL :: Area, Depth, X1, X2, dA, dV, Old, Y1, Y2
      REAL :: AIFafterTempRise
      REAL :: Z1, Z2, Z3, Z4

      Beta1    = ModelParms%Parms(1)
      Beta2    = ModelParms%Parms(10)
      
      !
      !     Volume2 is an equivalent volume of the ice pack, in m3, at end of dT
      !     (computed by assuming that ice is at freezing temperature).
      !
      !     Simultaneous solution to these equations,
      !
      !     AIFafterTempRise = AtmosphereIceFlux
      !    &                  - Rho * 1. / 2. * (Volume1 + Volume2) / 2. * (T2 - T1) / dT
      !     dV =  -(WaterIceFlux + AIFafterTempRise) / Fusion / Rho * dT
      !    &      - EvapOverIce + Snow
      !     Volume2 = Volume1 + dV
      !
      !     is:
      !
      AIFafterTempRise = (AtmosphereIceFlux - Rho * 1. / 2. * (T2 - T1)     &
              / dT * (Volume1 - WaterIceFlux / 2. * dT / Rho / Fusion -     &
              EvapOverIce / 2. + Snow / 2.)) / (1. - 1. / 4. / Fusion *     &
              (T2 - T1))
      dV = -(WaterIceFlux + AIFafterTempRise) / Fusion / Rho * dT         &
           - EvapOverIce + Snow
      Volume2 = Volume1 + dV

      IF (Volume2 .LT. 0.0001) THEN
         !
         !  Minimum icepack allowed has area of 1 m2 and depth of 1/10 mm.
         !
         dV = 0.0001 - Volume1
         Volume2 = 0.0001
         AIFafterTempRise = AtmosphereIceFlux                              &
               - Rho * 1. / 2. * (Volume1 + Volume2) / 2. * (T2 - T1) / dT
         Area2 = 1.0
         Depth2 = 0.0001
      ELSE
         !
         !     Solve ice cover differential equations.
         !
         Area2 = Area1
         Depth2 = Depth1
         Area = Area1
         Depth = Depth1
         IterationCount = 0
         Old = Depth2 * 2.0
         DO WHILE ((ABS(Depth2 - Old) / Depth2) .GT. 0.00001)
            IterationCount = IterationCount + 1
            Old = Depth2
            IF (IterationCount .LE. 25) THEN
               X1 = Beta1 * SQRT(Area)
               X2 = Beta2 * SQRT(Area)
               Y1 = Area + Depth * X1
               Y2 = Area + Depth * X2
               Z1 = X2 / Y2 * MIN(AIFafterTempRise, 0.)
               Z2 = X1 / Y1 * MAX(AIFafterTempRise, 0.)
               Z3 = X2 / Y2 * WaterIceFlux
               Z4 = X1 / Y1 * EvapOverIce * dT
               dA = -(Z1 + Z2 + Z3) / Fusion / Rho * dT - Z4
               Area2 = MIN(g_LkArea, MAX((Area2 + Area1 + dA) / 2.0, 1.0))
               Depth2 = Volume2 / Area2
               Area = (Area1 + Area2) / 2.0
               Depth = (Depth1 + Depth2) / 2.0
            END IF
         END DO
      END IF

      ExcessFromIceMelt = (dV + EvapOverIce - Snow) * Fusion * Rho / dT       &
                          + WaterIceFlux + AIFafterTempRise

      RETURN

      END SUBROUTINE IceCover

!----------------------------------------------------------------------------
      !-----------------------------------------------------------------------------
      !     This unit interprets the heat layer superposition information in terms
      !     of a vertical temperature profile defined at various points along the
      !     depth, where T(0) is temperature at the surface (depth = D(0) = 0) and
      !     T(K) is temperature at the bottom (depth = D(K) = MaxDepth).
      !
      !     Added 24apr01 by TECII.  (Based on ExtDlyTD.FOR and T-Index.FOR.)
      !-----------------------------------------------------------------------------
      SUBROUTINE GetTemperatureProfile(T, D, K)
      IMPLICIT NONE
      REAL, DIMENSION(MaxNoOfLayers*2 + 1), INTENT(INOUT) :: T, D
      INTEGER,  INTENT(OUT) :: K

      INTEGER :: J, NumLayers
      REAL    :: P2, P3, P4, P5, P6, P7, P8
      
      REAL    :: Temperature(1:MaxNoOfLayers)
      REAL    :: Depth(0:MaxNoOfLayers), V, M1, MaxDepth
      REAL    :: MaxVol, vF, Vol(0:MaxNoOfLayers), Slope(1:MaxNoOfLayers)

      P2  = ModelParms%Parms(2)
      P3  = ModelParms%Parms(3)
      P4  = ModelParms%Parms(4)
      P5  = ModelParms%Parms(5)
      P6  = ModelParms%Parms(6)
      P7  = ModelParms%Parms(7)
      P8  = ModelParms%Parms(8)
      NumLayers = g_NumLayers

      !
      ! Compute surface temperature contributions...
      !
      IF (g_TotalHeat(NumLayers) .LE. g_Hd) THEN
         DO K = 1, NumLayers
            g_SurfTempFromLayer(K) = g_SurfTempFromLayer(K - 1)                   &
               + (g_TotalHeat(K) - g_TotalHeat(K - 1))                            &
               * MixingFunction(g_rCV, g_Mf1, g_Mf2, P2, P3, P4, P8, g_AgeOfLayer(K))
         END DO
      ELSE
         DO K = 1, NumLayers
            g_SurfTempFromLayer(K) = g_SurfTempFromLayer(K - 1)                   &
               + (g_TotalHeat(K) - g_TotalHeat(K - 1))                            &
               * MixingFunction(g_rCV, g_Ms1, g_Ms2, P5, P6, P7, P8, g_AgeOfLayer(K))
         END DO
      END IF
      
      !
      !  F = volume (measured down from the surface) times r times C, which
      !      when exceeded by rCVm yields a fully-mixed temperature profile;
      !      subscripted n for post-fall turnover but pre-spring turnover;
      !      subscripted p for post-spring turnover but pre-fall turnover.
      !
      !  F = 0 will produce rectangular temperature profiles (with volume) only.
      !
      V = CoordLakeVolume(g_LkNum)
      Depth(0) = 0.0
      Vol(0) = 0.0
      MaxDepth = LakeDepth(0.0, g_LkNum)  ! use actual maximum depth of lake
      MaxVol = V - LakeVolume(MaxDepth, g_LkNum)
      DO K = 1, NumLayers
         Temperature(K) = g_SurfTempFromLayer(NumLayers - K + 1)
         IF (K .EQ. 1) THEN
            IF (Temperature(K) .LT. 3.98) THEN
               vF = P4 / 1000000. / 1.0 / 1.0 * 1.0e+20
            ELSE
               vF = P7 / 1000000. / 1.0 / 1.0 * 1.0E+20
            END IF
         END IF
         M1 = MIN(vF, V)
         Vol(K) = (g_TotalHeat(NumLayers-K+1) - g_TotalHeat(NumLayers-K))   &
                  / (g_SurfTempFromLayer(NumLayers - K + 1)                        &
                  - g_SurfTempFromLayer(NumLayers - K)) / g_rCV * V
         Slope(K) = 0.
         IF (Vol(K) .LT. M1) THEN
            Slope(K) = Vol(K) / vF
            Vol(K) = Vol(K) * 2.0 / (1.0 + Vol(K) / vF)
         END IF
         Depth(K) = LakeDepth(V - Vol(K), g_LkNum)
      END DO
      Temperature(NumLayers + 1) = 3.98
      DO K = 1, NumLayers
         Temperature(K) = Temperature(K) - Temperature(K + 1)
         IF (Slope(K) .GT. 0) Slope(K) = (Slope(K) * Temperature(K) - Temperature(K)) / Vol(K)
      END DO
      K = 1
      DO WHILE ((K .LE. 2 * NumLayers + 1) .AND. (Depth(K/2) .LE. MaxDepth))
         T(K) = 3.98
         DO J = K / 2 + 1, NumLayers
            T(K) = T(K) + Temperature(J) + Slope(J) * Vol(K / 2)
         END DO
         IF (K/2*2 .EQ. K) T(K) = T(K) + Temperature(K / 2)               &
             + Slope(K / 2) * Vol(K / 2)
         IF (T(K) .LT. 0.) T(K) = 0.
         D(K) = Depth(K / 2)
         K = K + 1
      END DO
      K = K - 1
      IF (K .EQ. 2 * NumLayers + 1) THEN
         K = K + 1
         T(K) = 3.98
         D(K) = MaxDepth
      ELSE
         K = K + 1
         T(K) = 3.98
         DO J = K / 2 + 1, NumLayers
            T(K) = T(K) + Temperature(J) + Slope(J) * MaxVol
         END DO
         T(K) = T(K) + Temperature(K / 2) + Slope(K / 2) * MaxVol
         D(K) = MaxDepth
      END IF
      RETURN
      END SUBROUTINE GetTemperatureProfile

!--------------------------------------------------------------------
      REAL FUNCTION LakeDepth(Volume, I)
      REAL,    INTENT(IN) :: Volume
      INTEGER, INTENT(IN) :: I
      IF (Volume .LT. 0.) THEN
         LakeDepth = DepthVolumeM(I)
      ELSE
         LakeDepth = DepthVolumeM(I) - (Volume / DepthVolumeA(I)) ** (1. / DepthVolumeB(I))
      ENDIF
      RETURN
      END FUNCTION LakeDepth

!-------------------------------------------------------------------
      REAL FUNCTION LakeVolume(Depth, I)
      REAL,    INTENT(IN) :: Depth
      INTEGER, INTENT(IN) :: I
      IF (Depth .GT. DepthVolumeM(I)) THEN
         LakeVolume = 0.
      ELSE
         LakeVolume = DepthVolumeA(I) * (DepthVolumeM(I) - Depth) ** DepthVolumeB(I)
      ENDIF
      RETURN
      END FUNCTION LakeVolume

!----------------------------------------------------------------------------
      REAL FUNCTION EPN (X)
      IMPLICIT NONE
      REAL :: X
      IF (X .LT. -87.33) THEN
         EPN = 0.
      ELSE
         EPN = EXP(X)
      END IF
      RETURN

      END FUNCTION EPN


END MODULE LLTM
