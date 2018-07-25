!=============================================================================
!----------------------------------------------------------------------------
!     LARGE BASIN RUNOFF MODEL (08 February 2016)
!
!     Contact Timothy S. Hunter
!             (734) 741-2344
!             (734) 741-2055 (FAX)
!             tim.hunter@noaa.gov
!
!             Great Lakes Environmental Research Laboratory
!             National Oceanic and Atmospheric Administration
!             US Department of Commerce
!             4840 South State Rd
!             Ann Arbor, Michigan  48108
!
!     This program is the general-purpose version of the Large Basin
!     Runoff Model (LBRM) as developed by Dr. Thomas E. Croley II.  The
!     original Fortran 77 model code, as specifically adapted for use in
!     GLERL's Advanced Hydrologic Prediction System (AHPS), has been
!     adapted by Tim Hunter (Dec 1999) to be more generic.  It was also
!     translated to Fortran 95 structure in order to take advantage of
!     MODULEs and eliminate COMMON blocks.  
!
!     This version of the LBRM has been additionally modified to incorporate 
!     a revised formulation of potential evapotranspiration. This revision was
!     prompted by the work of Dr. Brent Lofgren. The essential difference is 
!     that the original LBRM used this formulation for PE:
!
!        PE = ae^((TT)/Tb),         where TT = air temperature, and 
!                                         Tb = a calibrated base temperature parameter
!     
!     Dr. Lofgren pointed out that we could restate this as
!
!        PE = ae^((T + T')/Tb),     where T  = long-term mean temperature for the 
!                                              given time of year, and
!                                         T' = departure from T
!
!     Equivalently,
!       PE = ae^(T/Tb) e^(T'/Tb)
!
!
!     Dr. Lofgren found from his research that the above formulation results in 
!     very high sensitivity to temperature changes. He has suggested (as the easiest,
!     but not the best) the following change to the formulation:
!
!       PE = ae^(T/Tb) e^(T'/15.4)
!    
!     which imitates a Clausius-Clapeyron relationship and reduces the long-term
!     sensitivity to temperature changes.
!            
!     [NOTE: Verify with Brent that this is an accurate description.]
!
!
!     SPECIAL NOTE:
!     This program code is designed to be used in two ways: the first is as a
!     Windows DLL to be called by a Windows program; the second is as a stand-
!     alone DOS program.  Because of the absence of a standard set of condition-
!     al compiler directives, the user will have to compile and link in two dif-
!     ferent ways, to build either a DLL or a DOS program.  Furthermore, the
!     user will have to compile and link differently for DOS depending on use of
!     the program in a simulation setting or a calibration setting.
!
!     The DOS commands to compile this code and link it for the three cases are as
!     follows (using the GNU Compiler Suite gfortran compiler as an example):
!
!     First Case (for use as Windows DLL):
!
!     gfortran mymsgapi.f95        -c -m32
!     gfortran errorprocesswin.f95 -c -m32
!     gfortran GLSHFS_Util.F90     -c -m32
!     gfortran fcstutil.F90        -c -m32
!     gfortran lbrm.F90            -c -m32
!     gfortran mymsgapi.o errorprocesswin.o GLSHFS_Util.o fcstutil.o lbrm.o -shared -static -m32 -o lbrm.dll

!
!     Second Case (for use as a DOS program):
!
!     gfortran errorprocessDOS.f95 -c
!     gfortran GLSHFS_Util.F90     -c
!     gfortran fcstutil.F90        -c
!     gfortran lbrm.F90            -c
!     gfortran DOSmodel.f95        -c
!     gfortran errorprocessdos.o GLSHFS_Util.o fcstutil.o lbrm.o dosmodel.o -static -o dos_lbrm.exe
!
!
!
!     FILE FORMAT DESCRIPTIONS
!     ------------------------
!
!     LBRM.CFG
!        This is the configuration (or control) file which contains the
!        file names.  That's all it has in it.  The format is similar to
!        the older Windows ".INI" files.  That is, all lines will be ignored
!        except for those that start with the key string we are looking for
!        followed by an "=" sign.  The rest of the line will be considered
!        a file name.  An example file might look like the following:
!
!        |** This is the configuration file for the LBRM **
!        |This line will be ignored.
!        |$$$ So will this one.
!        |   spaces at the beginning of a line are insignificant.
!        | The ordering of the lines is also insignificant.
!        |The next 4 lines are the only ones we need to have in here.
!        |PARMFILE=PARAM.TXT
!        |BCFILE = BNDRCOND.TXT
!        |  METFILE =   METDATA.TXT
!        |OUTFILE = c:\mywork\OUTPUT.TXT
!
!
!
!     PARAMETER FILE  (could be named anything)
!        This file contains what would be considered the control information
!        for the LBRM in simulation or calibration.  That includes dates,
!        watershed area, and parameters.  Multiple entries on a line are
!        separated by a comma.  Explanatory comments for a line can exist in 
!        the file as long as they are separated from the data value by a comma.
!        For example, line 2 could be:
!        "  9.0700E+09 ,          subbasin area in sq meters"
!
!        The content of each line is as follows:
!
!        1st record: Identifier
!                    |________|___Any string the user wants to use which will
!                                 help identify this model run. This will be
!                                 echoed into the output file simply for the
!                                 user's help later.  It can be left blank with
!                                 no consequences if the user so desires.
!
!        2nd record: Area
!                    |__|____watershed area in square meters.
!
!        3rd record: StartDate, EndDate
!                    |       |  |_____|__ End date for model run in format YYYY-MM-DD
!                    |_______|___________ Start date for model run in format YYYY-MM-DD
!
!        4th through 12th records: model parameters
!
!                    Parameter
!                    |_______|_model parameter as follows:
!
!             Tbase   base temperature for computation of heat available
!                     for evapotranspiration
!             AlbedS  melt factor (proportionality constant) for computation
!                     of degree-day snowmelt
!             AlpPer  linear reservoir coefficient for percolation from
!                     the upper soil zone moisture storage
!             AlpUEv  partial linear reservoir coefficient for evapo-
!                     transpiration from the upper soil zone
!             AlpInt  linear reservoir coefficient for interflow from the
!                     lower soil zone moisture storage
!             AlpDPr  linear reservoir coefficient for deep percolation
!                     from the lower soil zone moisture storage
!             AlpLEv  partial linear reservoir coefficient for evapo-
!                     transpiration from the lower soil zone
!             AlpGw   linear reservoir coefficient for groundwater flow
!                     from the groundwater zone moisture storage
!             AlpSf   linear reservoir coefficient for basin outflow from
!                     the surface zone moisture storage
!
!        13th through 28th records:
!
!                    Data value
!                    |________|___ data value as follows:
!
!             Cons    proportionality constant for computation of heat
!                     available for evapotranspiration; CANNOT BE ZERO DURING
!                     PRODUCTION RUNS; IF ZERO DURING CALIBRATION, "Cons" IS
!                     COMPUTED FROM THE FOLLOWING 14 VALUES:
!             Rjan    January average mid-monthly daily cloudless solar
! 		                insolation at the surface (langleys)
!             Rfeb    February insolation
!             Rmar    March insolation
!             Rapr    April insolation
!             Rmay    May insolation
!             Rjun    June insolation
!             Rjul    July insolation
!             Raug    August insolation
!             Rsep    September insolation
!             Roct    October insolation
!             Rnov    November insolation
!             Rdec    December insolation
!             B1      empirical linear coefficient [ordinate-intercept]
!		                relating ratio of cloudy insolation/cloudless insolation
!                     to "cloud cover"
!             B2      empirical linear coefficient [slope] relating ratio of
!                     cloudy insolation/cloudless insolation to "cloud cover"
!             USZC    upper soil zone capacity (cm)
!
!        For the circa-1982 method of calculating potential evapotranspiration,
!        the file ends at line 28. If we are using the 2016 method for PET, then
!        the following additional lines must be present:
!    
!        29th record: "Long term air temperature values used for calculation of potential evapotranspiration. deg C"
!                     |___An identifying string (should be this, exactly) that acts as a flag
!                         of what is contained on the subsequent lines.
!
!        30th record: "Day,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec"
!
!        31st through 62nd lines:  Comma-separated daily average air temperatures for the subbasin 
!                                  (12 months, 31 days/month), deg C.
!                                  These values are calculated and then "set in stone" for a calibration
!                                  and subsequent simulation use of LBRM. Each subbasin is independent.
!                                  Missing data is denoted by -99.99
!                                  February 29th should be set to a valid value.
!
!        Day,JanV,FebV,MarV,AprV,MayV,JunV,JulV,AugV,SepV,OctV,NovV,DecV
!        | | |  | |  | |  | |  | |  | |  | |  | |  | |  | |  | |  | |__|__ value for December  on this day
!        | | |  | |  | |  | |  | |  | |  | |  | |  | |  | |  | |__|_______ value for November  on this day
!        | | |  | |  | |  | |  | |  | |  | |  | |  | |  | |__|____________ value for October   on this day
!        | | |  | |  | |  | |  | |  | |  | |  | |  | |__|_________________ value for September on this day
!        | | |  | |  | |  | |  | |  | |  | |  | |__|______________________ value for August    on this day
!        | | |  | |  | |  | |  | |  | |  | |__|___________________________ value for July      on this day
!        | | |  | |  | |  | |  | |  | |__|________________________________ value for June      on this day
!        | | |  | |  | |  | |  | |__|_____________________________________ value for May       on this day
!        | | |  | |  | |  | |__|__________________________________________ value for April     on this day
!        | | |  | |  | |__|_______________________________________________ value for March     on this day
!        | | |  | |__|____________________________________________________ value for February  on this day
!        | | |__|_________________________________________________________ value for January   on this day
!        |_|______________________________________________________________ day of the month
!
!
!     BOUNDARY CONDITIONS FILE (could be named anything)
!        This file contains boundary conditions that the model outputs must adhere
!        to.  During the model run, values of the various storages are (re)set to
!        the boundary conditions on the indicated dates if the boundary conditions
!        are non-negative.  A date, corresponding to the starting date in the
!        parameter file, must be present in this file and would, by definition,
!        contain the initial conditions (which CANNOT be negative).  There may be
!        any number of boundary conditions present in this file, but THEY MUST BE
!        IN CHRONOLOGICAL ORDER!
!
!        1st record: skipped (may be anything)
!
!        2nd record: skipped (may be anything)
!
!        3-? records: iD iM iY Uszm Lszm Gzm Ss Snw (FORMAT: 2I2, I4, 5E13.6E2)
!                     || || || |  | |  | | | || | |
!                     || || || |  | |  | | | || |_|___snowpack moisture (cm.)
!                     || || || |  | |  | | | ||_______surface storage (cm.)
!                     || || || |  | |  | |_|__________groundwater storage (cm.)
!                     || || || |  | |__|______________lower soil zone storage (cm.)
!                     || || || |__|___________________upper soil zone storage (cm.)
!                     || || ||________________________date's year
!                     || ||___________________________date's month
!                     ||______________________________date's day
!
!
!     METEOROLOGICAL DATA FILE  (could be named anything)
!        This file contains the daily air temperatures (maximum and minimum)
!        and precipitation for each day of the model run period specified
!        in the parameter file.  It may contain data for dates outside that
!        period as well.  If so, only the data for the specified period will
!        actually be used.
!
!        Note that NO MISSING DATA IS ALLOWED!!!!!!  If the LBRM encounters
!        missing data during a run it will immediately stop with an error message.
!
!        The format is as follows:
!
!        1st record: Identifier (FORMAT: A)
!                    |        |
!                    |________|___Any string the user wants to use which will
!                                 help identify this data set.  It will not be
!                                 used by the LBRM for anything.
!
!        2nd record: "DDMMYYYY TMIN TMAX PREC" (FORMAT: A)
!                     |                     |
!                     |_____________________|__header info string - not used
!
!        Records 3 through the end of the file have daily data.  These records
!        must be in chronological order with no missing days.
!
!        dd mm yyyy Tmin Tmax Precip (FORMAT: 2I2, I4, 3I5)
!        || || |  | |  | |  | |    |
!        || || |  | |  | |  | |____|__depth of daily precipitation in
!        || || |  | |  | |  |         hundredths of millimeters.
!        || || |  | |  | |__|_________maximum daily air temperature in 
!        || || |  | |  |              hundredths of degrees Celsius.
!        || || |  | |__|______________minimum daily air temperature in
!        || || |  |                   hundredths of degrees Celsius.
!        || || |__|___________________four-digit year of data's date
!        || ||________________________two-digit month of data's date
!        ||___________________________two-digit day of data's date
!
!
!     OUTPUT DATA FILE  (could be named anything)
!        This file contains the daily output from the LBRM for each day
!        of the model run period specified in the parameter file.
!
!        The format is as follows:
!
!        1st record: Identifier (FORMAT: A)
!                    |        |
!                    |________|___This is the string that was read from record
!                                 1 of the PARAMETER file.
!
!        2nd record: "Output from the LBRM.  Runoff and storages are in cm over the basin."
!
!        3rd record: "DDMMYYYY  RUNOFF   USZM   LSZM GROUND SURFAC   SNOW"
!
!        Records 4 through the end of the file have the daily output (end-of-day).
!
!           dd mm yyyy Run USZ LSZ GZM SRF SNW (FORMAT: 2I2, I4, 6E11.4E2)
!           || || |  | | | | | | | | | | | | |
!           || || |  | | | | | | | | | | | |_|__snowpack moisture storage (cm)
!           || || |  | | | | | | | | | |_|______surface moisture storage (cm)
!           || || |  | | | | | | | |_|__________groundwater moisture storage (cm)
!           || || |  | | | | | |_|______________lower soil zone moisture storage (cm)
!           || || |  | | | |_|__________________upper soil zone moisture storage (cm)
!           || || |  | |_|______________________runoff volume for this day (cm)
!           || || |__|__________________________four-digit year of data's date
!           || ||_______________________________two-digit month of data's date
!           ||__________________________________two-digit day of data's date
!
!-----------------------------------------------------------------
!=============================================================================

MODULE LBRM_Global
      USE Glshfs_global

      !
      !  From the configuration file
      !
      CHARACTER(LEN=120) :: LBRM_RunID               ! optional string describing this run
      CHARACTER(LEN=200) :: LBRM_ParmFileName        ! input parameters, dates, etc
      CHARACTER(LEN=200) :: LBRM_BndcFileName        ! input initial & boundary storages
      CHARACTER(LEN=200) :: LBRM_MetFileName         ! input forcing meteorology
      CHARACTER(LEN=200) :: LBRM_OutputFileName      ! output file

      !
      !  A data structure that will contain all of the data from a
      !  parameter file.
      !
      TYPE TLbrmParmData
         CHARACTER(LEN=3)       :: Bsn
         INTEGER                :: Subbasin
         REAL                   :: Area
         INTEGER                :: SDateSeq, EDateSeq
         REAL, DIMENSION(9)     :: ParmValues
         REAL                   :: Cons
         REAL, DIMENSION(12)    :: Insolation
         REAL                   :: B1, B2, USZC
         REAL, DIMENSION(12,31) :: AirTemps        ! unused for 1982 method
         !
         !  Which method will we use for computing potential evapotranspiration?
         !
         !  1 = The original LBRM method used from 1980s to 2015.
         !      The simple daily air temperature value is used for computing 
         !      the heat index.
         !  2 = Read a computed long-term air temperature series (366 values)
         !      from the parameter file. Each day, use the day's air temp to
         !      compute an anomaly from that long-term daily series. Then use
         !      the long-term value and the anomaly as the two parts of a 
         !      heat index calculation.
         !
         INTEGER :: PET_Method   
      END TYPE TLbrmParmData
      
      !
      !  A data structure that will contain all of the data from a
      !  boundary conditions file.
      !     Values(1,:) = USZM     (cm)
      !     Values(2,:) = LSZM     (cm)
      !     Values(3,:) = GZM      (cm)
      !     Values(4,:) = Surf     (cm)
      !     Values(5,:) = Snow     (cm)
      !
      TYPE TLbrmBndcData
         CHARACTER(LEN=3)       :: Bsn
         INTEGER                :: Subbasin
         INTEGER                :: SDateSeq, EDateSeq
         INTEGER                :: NumEntries
         INTEGER, DIMENSION(:),   ALLOCATABLE  :: Dates          !  index (1..n)
         REAL,    DIMENSION(:,:), ALLOCATABLE  :: Values         !  index (1..5, 1..n)
      END TYPE TLbrmBndcData

      !
      !  A data structure that will contain all of the data from a
      !  meteorology file.
      !     Values(1,:)  = TMin  (C)
      !     Values(2,:)  = TMax  (C)
      !     Values(3,:)  = Prec  (mm)
      !
      TYPE TLbrmMetData
         CHARACTER(LEN=3)       :: Bsn
         INTEGER                :: Subbasin
         INTEGER                :: SDateSeq, EDateSeq
         INTEGER                :: NumDays
         REAL, DIMENSION(:,:), ALLOCATABLE  :: Values         !  index (1..3, 1..NumDays)
      END TYPE TLbrmMetData

      !
      !  A simple data structure to store all of the data from an LBRM output file.
      !  This type is a simplified version of TDlyDataForSubbasin that is
      !  implemented in a more "basic" way for improved efficiency as we 
      !  do a run of the LBRM.  We will still be using TDlyDataForSubbasin 
      !  objects to do the actual read/write of files.
      !
      !  Data in the Values array is assumed to be in cubic meters.
      !     (1,:) = total runoff out of the watershed
      !     (2,:) = upper soil zone moisture
      !     (3,:) = lower soil zone moisture
      !     (4,:) = ground zone moistuire
      !     (5,:) = surface zone moisture
      !     (6,:) = snow pack moisture (snow water equivalent)
      !
      TYPE TLbrmOutputData
         CHARACTER(LEN=3) :: Bsn
         INTEGER          :: Subbasin
         REAL             :: Area
         INTEGER          :: SDateSeq, EDateSeq
         REAL, DIMENSION(:,:), ALLOCATABLE :: Values      ! indexed (6,NumDays)
      END TYPE TLbrmOutputData
      
      !
      !  A large data structure that will contain all of the relevant data
      !  for a run. Parameter values, boundary conditions, meteorology and output.
      !  
      TYPE Comprehensive_Lbrm_Data
         TYPE (TLbrmParmData)   :: Parm
         TYPE (TLbrmBndcData)   :: Bndc
         TYPE (TLbrmMetData)    :: Met
         TYPE (TLbrmOutputData) :: Output
      END TYPE Comprehensive_Lbrm_Data

      !  These 6 variables contain the long term volume (cubic meters) of various items.
      !      LTnsg = long-term supply to the ground surface (snowmelt and/or precipitation)
      !      LTrun = long-term surface runoff
      !      LTint = long-term interflow
      !      LTgwf = long-term groundwater flow
      !      LTuev = long-term upper zone evapotranspiration
      !      LTlev = long-term lower zone evapotranspiration
      !
      REAL :: LTnsg, LTrun, LTint, LTgwf, LTuev, LTlev
      
      !
      !  Maximum number of boundary conditions allowed in the master file (per subbasin).
      !  This is used to dimension some arrays.
      !
      INTEGER, PARAMETER :: MaxLbrmBC = 100
      
CONTAINS

      !-----------------------------------------------------------------------
      !  Routine to simply deallocate any allocated arrays, etc
      !  This does NOT clear values assigned to scalar variables or static arrays.
      !  i.e. Only basic cleanup for memory usage
      !  Maybe full initialization/clearance should be added???
      !-----------------------------------------------------------------------
      SUBROUTINE ClearLbrmData(LData)
      IMPLICIT NONE
      TYPE (Comprehensive_Lbrm_Data), INTENT(INOUT) :: LData
      INTEGER :: IOS
      
      IF (ALLOCATED(LData%Bndc%Dates))     DEALLOCATE(LData%Bndc%Dates,    STAT=IOS)
      IF (ALLOCATED(LData%Bndc%Values))    DEALLOCATE(LData%Bndc%Values,   STAT=IOS)
      IF (ALLOCATED(LData%Met%Values))     DEALLOCATE(LData%Met%Values,    STAT=IOS)
      IF (ALLOCATED(LData%Output%Values))  DEALLOCATE(LData%Output%Values, STAT=IOS)
      
      END SUBROUTINE ClearLbrmData
      
         
END MODULE LBRM_Global


!================================================================================
!================================================================================
!================================================================================
MODULE LBRM_Compute
      USE LBRM_Global
      USE GLSHFS_Util

      !
      !   These variables are made global to the computational 
      !   procedures for simplicity.
      !
      !   AlpDPr = linear reservoir constant for deep percolation, inv. days
      !   AlpGEv = partial constant of groundwater evaporation, inv. cub. m.
      !   AlpGw  = linear reservoir constant for grooundwater flow, inv. days
      !   AlpInt = linear reservoir constant for interflow, inv. days
      !   AlpUEv = partial constant of upper zone evap., inv. cub. m.
      !   AlpLEv = partail constant of lower zone evaporation, inv. cub. m.
      !   AlpPer = linear reservoir constant for percolation, inv. days
      !   AlpSEv = partial constant of surface evaporation, inv. cub. m.
      !   AlpSf  = linear reservoir constant for surface flow, inv. days
      !   USZC   = upper soil zone moisture capacity, cub. m.
      !   CID    = time in one group of days (week, month, etc.), days
      !   Uszm   = upper soil zone moisture, cub. m.
      !   Lszm   = lower soil zone moisture, cub. m.
      !   Gzm    = groundwater zone moisture, cub. m.
      !   Ss     = surface water storage, cub. m.
      !   UszmAvg= average upper soil zone moisture storage over the time period.
      !   LszmAvg= average lower soil zone moisture storage over the time period.
      !   vUEv   = upper zone evapotranspiration volume, cub. m.
      !   vLEv   = lower zone evapotranspiration volume, cub. m.
      !   vInt   = interflow volume, cub. m.
      !   vPer   = percolation volume, cub. m.
      !   vGw    = groundwater zone outflow volume, cub. m.
      !   vRun   = surface runoff volume, cub. m.
      !   Evap   = total evapotranspiration volume, cub. m.
      !   Hplse  = total energy out (Evap. + Pot. Evap.) water equ., cub. m.
      !
      !   Epsilon, Dpsilon, Gpsilon, and Hpsilon : convergence criteria
      !
      REAL  :: AlpDPr, AlpGEv, AlpGw, AlpLEv, AlpInt
      REAL  :: AlpPer, AlpSEv, AlpSf, AlpUEv
      REAL  :: USZC, CID
      REAL  :: Uszm, Lszm, Gzm, Ss
      REAL  :: UszmAvg, LszmAvg
      REAL  :: vUEv, vLEv, vInt, vPer, vGw, vRun
      REAL  :: Evap, Hplse
      REAL  :: Epsilon, Dpsilon, Gpsilon, Hpsilon

   CONTAINS

      !--------------------------------------------------------------------------
      !   Main computational engine.
      !     If Mode = 0, then this program is being used in standard
      !     simulation mode, and all of the parameters (including Cons) 
      !     are fixed. The value of ComputedCons will simply be set to
      !     the fixed value read from the parameter file.
      !
      !     If Mode = 1, then this program is in calibration mode and we will be
      !     computing the value of Cons "on the fly". The computed value will be
      !     returned in the variable ComputedCons.
      !
      !     LTnsg, LTrun, LTint, LTgwf, LTuev, and LTlev are respectively long-term
      !     volumes of net supply to the ground surface (snowmelt and/or precipitation),
      !     surface runoff, interflow, groundwater flow, upper zone evapotranspiration,
      !     and lower zone evapotranspiration.
      !--------------------------------------------------------------------------
      SUBROUTINE LBRM_Engine(Mode, ComputedCons, LData)
      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: Mode
      REAL,    INTENT(OUT)  :: ComputedCons
      TYPE (Comprehensive_Lbrm_Data), INTENT(INOUT) :: LData

      INTEGER :: I, J, Dy, Mn, Yr, DD, MM, YY
      INTEGER :: Seq, MetIndx, IOS
      LOGICAL :: OK
      
      REAL :: TBase, AlbedS, Cons, B1, B2
      REAL :: Precip, Snw, Ta, Tmax, Tmin
      REAL :: Melt, Runoff, InFlow, WatSply
      REAL :: AvgRR, AvgHPlE, RR
      REAL :: LTAir, TPrime, HIndx
      REAL :: TDiff

      ErrorLevel = 0
      
      !
      !  Set values for these "local" variables by assigning from the 
      !  values passed in (read from parameter file prior to this routine).
      !
      TBase  = LData%Parm%ParmValues(1)
      AlbedS = LData%Parm%ParmValues(2) * LData%Parm%Area / 100.
      AlpPer = LData%Parm%ParmValues(3)
      AlpUEv = LData%Parm%ParmValues(4)
      AlpInt = LData%Parm%ParmValues(5)
      AlpDpr = LData%Parm%ParmValues(6)
      AlpLEv = LData%Parm%ParmValues(7)
      AlpGw  = LData%Parm%ParmValues(8)
      AlpSf  = LData%Parm%ParmValues(9)
      Cons   = LData%Parm%Cons * LData%Parm%Area * 10000.
      B1     = LData%Parm%B1
      B2     = LData%Parm%B2
      
      !
      !  Convert the upper soil zone capacity from the parameter
      !  set (specified in cm) into units of cubic meters
      !
      USZC = LData%Parm%USZC / 100.0 * LData%Parm%Area
      
      !
      !  The values in the boundary conditions file are END-OF-DAY
      !  values. Thus, we need to have an entry in that file for the
      !  day before the requested start date (from the parameter file).
      !  That will give us the initial moisture storages for the run.
      !  Make sure we have valid conditions for that date.
      !
      !  The values of Uszm, Lszm, etc must be converted from the supplied
      !  units of centimeters into cubic meters
      !
      Uszm = MissingData_Real
      Lszm = MissingData_Real
      Gzm  = MissingData_Real
      Ss   = MissingData_Real
      Snw  = MissingData_Real
      DO I = 1, LData%Bndc%NumEntries
         IF (LData%Bndc%Dates(I) .EQ. LData%Parm%SDateSeq-1) THEN
            Uszm = LData%Bndc%Values(1,I) / 100 * LData%Parm%Area
            Lszm = LData%Bndc%Values(2,I) / 100 * LData%Parm%Area
            Gzm  = LData%Bndc%Values(3,I) / 100 * LData%Parm%Area
            Ss   = LData%Bndc%Values(4,I) / 100 * LData%Parm%Area
            Snw  = LData%Bndc%Values(5,I) / 100 * LData%Parm%Area
         END IF
      END DO
      OK = .TRUE.
      IF (IsMissing(Uszm)) OK = .FALSE.
      IF (IsMissing(Lszm)) OK = .FALSE.
      IF (IsMissing(Gzm))  OK = .FALSE.
      IF (IsMissing(Ss))   OK = .FALSE.
      IF (IsMissing(Snw))  OK = .FALSE.
      IF (Uszm .LT. -0.1)     OK = .FALSE.
      IF (Lszm .LT. -0.1)     OK = .FALSE.
      IF (Gzm  .LT. -0.1)     OK = .FALSE.
      IF (Ss   .LT. -0.1)     OK = .FALSE.
      IF (Snw  .LT. -0.1)     OK = .FALSE.
      IF (.NOT. OK) THEN
         CALL SequenceDate(Dy, Mn, Yr, LData%Parm%SDateSeq);     IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(ErrorMessage, 5001) Yr, Mn, Dy;                   CALL PassMsg
         CALL SequenceDate(Dy, Mn, Yr, LData%Parm%SDateSeq-1);   IF (ErrorLevel .NE. 0) GOTO 899
         WRITE(ErrorMessage, 5002) Yr, Mn, Dy;                   CALL PassMsg
         WRITE(ErrorMessage, 5003);                              CALL PassMsg
         GOTO 898
      END IF

      !
      !  Set initial values for some variables
      !
      Epsilon = 1.e-5
      Dpsilon = 1.e-30
      Gpsilon = 1.e-3
      Hpsilon = 1.e-6
      AlpGEv  = 0.
      AlpSEv  = 0.
      
      !
      !   CID = Computation Interval (Days)
      !   This is a global variable used frequently in the computations.
      !   Always use 1-Day computation interval.
      !
      CID = 1.0

      !
      !  Check and compute, if necessary, a value for ComputedCons
      !  Note that the value assigned to ComputedCons (whether it is
      !  just the value of Cons read from the parameter file or it is
      !  calculated here) is always in calories/cm.
      !  The value of Cons used throughout the LBRM computations, however,
      !  is total calories for the watershed.
      !
      !  Cal / (cm*cm) * Area(m2) * 10000.  ->  Cal/m2 * Area(m2)
      !
      IF (Mode .EQ. 0) THEN                             ! standard simulation run
         ComputedCons = LData%Parm%Cons                 ! "Cons" value in cal/cm2.
      ELSE                                              ! calibration run, so compute Cons
         AvgRR = 0.
         AvgHPlE = 0.
         DO Seq = LData%Parm%SDateSeq, LData%Parm%EDateSeq
            MetIndx = Seq - LData%Met%SDateSeq + 1                             ! index into met data arrays for today
            Tmin    = LData%Met%Values(1, MetIndx)                             ! deg C
            Tmax    = LData%Met%Values(2, MetIndx)                             ! deg C
            Precip  = LData%Met%Values(3, MetIndx) / 1000.0 * LData%Parm%Area  ! mm converted to cubic meters
            DO J = 1, LData%Bndc%NumEntries
               IF (LData%Bndc%Dates(J) .EQ. Seq-1) THEN                        ! end-of-day for yesterday
                  IF (LData%Bndc%Values(5,J) .GE. 0.) THEN
                     Snw = LData%Bndc%Values(5,J) / 100.0 * LData%Parm%Area    ! cm -> cubic meters
                  END IF
               END IF
            END DO
            
            !
            !  Compute daily insolation from minimum and maximum air temperatures and
            !  from average mid-monthly cloudless daily solar insolation at the
            !  surface.
            !  Step 1 is to compute the incoming insolation.
            !  Step 2 is to adjust that based on parameters and temps.
            !
            !  The effective diurnal temperature range is capped at 15 degrees C for
            !  purposes of cloud cover proxy.
            !
            CALL SequenceDate(Dy, Mn, Yr, Seq);                          IF (ErrorLevel .NE. 0) GOTO 899
            RR = InterpolatedInsolation(Seq, LData%Parm%Insolation);     IF (ErrorLevel .NE. 0) GOTO 899
            TDiff = MIN((Tmax-Tmin)/15., 1.0)
            RR = RR * (B1 + B2 * TDiff) * 10000. * LData%Parm%Area
            
            !
            !  Heat balance
            !
            CALL SnowPackHeatBalance(AlbedS, Tmin, Tmax, Precip, Ta, Snw, Melt, Runoff)
            AvgRR = AvgRR + RR - (Melt * 1000000. * 79.7)
            IF (LData%Parm%PET_Method .EQ. 1) THEN
               HIndx = HeatIndexMethod1(Ta, Tbase)
            ELSE IF (LData%Parm%PET_Method .EQ. 2) THEN
               CALL SequenceDate(DD, MM, YY, Seq);  IF (ErrorLevel .NE. 0) GOTO 899
               LTAir  = LData%Parm%AirTemps(MM, DD)
               TPrime = Ta - LTAir
               HIndx  = HeatIndexMethod2(LTair, TPrime, TBase)
            END IF
            AvgHPlE = AvgHPlE + HIndx
         END DO
         Cons = AvgRR / AvgHPlE             ! calories
         IF (Cons .LT. DPSILON) THEN
            Cons = 0
         ELSE
            CALL Round(Cons, 6)    ! this routine cannot handle a "zero"
         END IF
         ComputedCons = Cons / LData%Parm%Area / 10000.        ! ComputedCons is always in cal/cm2.
      END IF
      
      !
      !     Initialize volume summation variables.
      !
      LTnsg = 0.
      LTrun = 0.
      LTint = 0.
      LTgwf = 0.
      LTuev = 0.
      LTlev = 0.

      !
      !  Copy values into the LData%Output structure
      !
      LData%Output%Bsn       = LData%Parm%Bsn
      LData%Output%Subbasin  = LData%Parm%Subbasin
      LData%Output%Area      = LData%Parm%Area
      LData%Output%SDateSeq  = LData%Parm%SDateSeq
      LData%Output%EDateSeq  = LData%Parm%EDateSeq
      
      !
      !  Allocate the memory for storing the daily runoff and storages
      !
      IF (ALLOCATED(LData%Output%Values))  DEALLOCATE(LData%Output%Values, STAT=IOS)
      I = LData%Output%EDateSeq - LData%Output%SDateSeq + 1
      ALLOCATE(LData%Output%Values(6,I), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5010) I;    CALL PassMsg
         GOTO 898
      END IF

      !
      !  Daily loop
      !
      DO Seq = LData%Parm%SDateSeq, LData%Parm%EDateSeq
         MetIndx = Seq - LData%Met%SDateSeq + 1                            ! index into met data arrays for today
         Tmin   = LData%Met%Values(1, MetIndx)                             ! deg C
         Tmax   = LData%Met%Values(2, MetIndx)                             ! deg C
         Precip = LData%Met%Values(3, MetIndx) / 1000.0 * LData%Parm%Area  ! mm converted to cubic meters
         
         !
         !  Use input boundary conditions, if available for this date.
         !  Values are supplied as centimeters over the watershed.
         !
         DO J = 1, LData%Bndc%NumEntries
            IF (LData%Bndc%Dates(J) .EQ. Seq-1) THEN                 ! end-of-day for yesterday
               IF (LData%Bndc%Values(1,J) .GE. 0.) Uszm = LData%Bndc%Values(1,J) / 100. * LData%Parm%Area
               IF (LData%Bndc%Values(2,J) .GE. 0.) Lszm = LData%Bndc%Values(2,J) / 100. * LData%Parm%Area
               IF (LData%Bndc%Values(3,J) .GE. 0.) Gzm  = LData%Bndc%Values(3,J) / 100. * LData%Parm%Area
               IF (LData%Bndc%Values(4,J) .GE. 0.) Ss   = LData%Bndc%Values(4,J) / 100. * LData%Parm%Area
               IF (LData%Bndc%Values(5,J) .GE. 0.) Snw  = LData%Bndc%Values(5,J) / 100. * LData%Parm%Area
            END IF
         END DO
         InFlow = 0           ! cubic meters

         !
         !  Heat balance between snow and liquid precip
         !    WatSply = liquid water reaching the ground.  i.e.,
         !              LIQUID precipitation plus snowmelt.
         !        Any precipitation added to snowpack is not included
         !        in this value.
         !
         CALL SnowPackHeatBalance(AlbedS, Tmin, Tmax, Precip, Ta, Snw, Melt, WatSply)
         IF (LData%Parm%PET_Method .EQ. 1) THEN             ! 1982 method (old)
            HIndx = HeatIndexMethod1(Ta, TBase)
         ELSE IF (LData%Parm%PET_Method .EQ. 2) THEN        ! 2016 method (new)
            CALL SequenceDate(DD, MM, YY, Seq)
            LTAir = LData%Parm%AirTemps(MM, DD)
            TPrime = Ta - LTAir
            HIndx = HeatIndexMethod2(LTair, TPrime, TBase)
         END IF
         HPLSE = HIndx / (596. - .52 * Ta) / 1000000. * Cons 

         !
         !  Accumulate the volume of net supply (liquid precip + snowmelt) to 
         !  the ground surface.
         !     
         LTnsg = LTnsg + WatSply

         !
         !  Mass balance  
         !   Runoff is in cubic meters when Outflow() finishes.
         !
         CALL Outflow(WatSply, Inflow, Runoff) ; IF (ErrorLevel .NE. 0) GOTO 899 
  
         !
         !  Copy output variables into the MyLbrmData structure.
         !  All of these values are in cubic meters.
         !
         I = Seq - LData%Parm%SDateSeq + 1
         LData%Output%Values(1,I) = Runoff
         LData%Output%Values(2,I) = Uszm  
         LData%Output%Values(3,I) = Lszm  
         LData%Output%Values(4,I) = Gzm   
         LData%Output%Values(5,I) = Ss    
         LData%Output%Values(6,I) = Snw   
         
         !
         !  Accumulate flow volumes.  Also in cubic meters.
         !
         LTrun = LTrun + vRun
         LTint = LTint + vInt
         LTgwf = LTgwf + vGW
         LTuev = LTuev + vUEv
         LTlev = LTlev + vLEv
      END DO
  
      RETURN
      
      !
      !  Error handling
      !
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] : LBRM_Engine...'
      CALL PassMsg
      RETURN
      
 5001 FORMAT('LBRM run starting on ', I4.4, 2('-',I2.2), ' requires end-of-day conditions from')
 5002 FORMAT('the previous day (', I4.4, 2('-',I2.2), ') to be used as the initial conditions')
 5003 FORMAT('for the run. Those were not found in the supplied boundary conditions file.')
 5010 FORMAT('Error allocating memory for storage of daily LBRM results. NumDays=', I0)
      
      END SUBROUTINE LBRM_Engine

!--------------------------------------------------------------------------------      
      !--------------------------------------------------------------------------
      !  This is the original method as described by Dr. Thomas Croley when
      !  he developed the LBRM.
      !--------------------------------------------------------------------------
      REAL FUNCTION HeatIndexMethod1(Ta, Tbase)
      IMPLICIT NONE
      REAL, INTENT(IN) :: Ta, Tbase
      HeatIndexMethod1 = EXP(Ta/Tbase)
      END FUNCTION HeatIndexMethod1
      
      !--------------------------------------------------------------------------
      !  This revised method for computation of the heat index is based on the
      !  work of Dr. Brent Lofgren around 2011-2016.  This was added in 2016.
      !--------------------------------------------------------------------------
      REAL FUNCTION HeatIndexMethod2(T, Tprime, Tbase)
      IMPLICIT NONE
      REAL, INTENT(IN) :: T, TPrime, Tbase
      HeatIndexMethod2 = EXP(T/Tbase) * EXP(Tprime/15.4)
      RETURN
      END FUNCTION HeatIndexMethod2
      
!--------------------------------------------------------------------------------      
      !--------------------------------------------------------------------------
      !  Snow pack heat balance.
      !  Given temperature and precipitation values, compute how much gets added
      !  to the snowpack, and how much is liquid water that soaks into the 
      !  ground.
      !
      !  This calculation is interesting...
      !  1) The "average" temperature for the day is actually the mid-point
      !     temperature between TMax and TMin. 
      !
      !  2) If the average temperature for the day is <= 0, all precipitation
      !     is assumed to be snow, and no melt occurs, at all.  Liquid water
      !     supply is zero for the day.  This is a clear simplification, since
      !     there could easily be significant daytime melt, followed by very 
      !     cold night-time refreeze. That is ignored here.
      !
      !  3) If the average temp is > 0, a certain amount of the snowpack gets
      !     melted, and all precipitation is assumed to be liquid.
      !--------------------------------------------------------------------------
      SUBROUTINE SnowPackHeatBalance(AlbedS, Tmin, Tmax, Precip, Ta, Snw, Melt, LiqWS)
      IMPLICIT NONE
      REAL, INTENT(IN) :: AlbedS, Tmin, Tmax, Precip
      REAL, INTENT(INOUT) :: Ta, Snw, Melt, LiqWS
      REAL :: DD

      Ta = (Tmax + Tmin) / 2.
      Melt = 0.
      IF (Ta .GT. 0.) THEN
         IF (Snw .GE. 1.) THEN
            IF (Tmin .LT. 0.) THEN
              DD = Tmax ** 2 / (Tmax - Tmin) / 2.
            ELSE
              DD = Ta
            ENDIF
            Melt = AlbedS * DD
            IF (Melt .GT. Snw) Melt = Snw
         ENDIF
         Snw = Snw - Melt
         LiqWS = Precip + Melt
      ELSE
         Snw = Snw + Precip
         LiqWS = 0.
      ENDIF
      RETURN
      END SUBROUTINE SnowPackHeatBalance

!--------------------------------------------------------------------------------      
      !-----------------------------------------------------------------------------
      !
      !-----------------------------------------------------------------------------
      FUNCTION InterpolatedInsolation(Today, MidMonthI)   Result(InterpI)
      IMPLICIT NONE
      INTEGER,             INTENT(IN) :: Today
      REAL, DIMENSION(12), INTENT(IN) :: MidMonthI          ! mid-month insolation values (Jan..Dec)
      REAL  :: InterpI
      
      INTEGER :: Dy, Mn, Yr, M1, M2, Y1, Y2, Seq1, Seq2
      REAL    :: I1, I2, D

      CALL SequenceDate(Dy, Mn, Yr, Today)
      Y1 = Yr
      Y2 = Yr      
      IF (Dy .LE. 15) THEN
         M1 = Mn - 1        ! previous month
         M2 = Mn            ! this month
      ELSE
         M1 = Mn            ! this month
         M2 = Mn + 1        ! next month
      END IF
        
      IF (M1 .LT. 0) THEN
         M1 = 12
         Y1 = Y1 - 1
      END IF
      
      IF (M2 .GT. 12) THEN
         M2 = 1
         Y2 = Y2 + 1
      END IF

      I1 = MidMonthI(M1)
      I2 = MidMonthI(M2)
      
      CALL DateSequence(15, M1, Y1, Seq1)
      CALL DateSequence(15, M2, Y2, Seq2)
      
      !  
      !  Compute the fractional "distance" to travel on the line between I1 and I2
      !  This is a number between >0 and 1
      !
      D = 1.0 * (Today - Seq1) / (Seq2 - Seq1)
      
      InterpI = I1 + (I2-I1)*D

      END FUNCTION InterpolatedInsolation

      
!--------------------------------------------------------------------------------      
      !-----------------------------------------------------------------
      !
      !  This routine computes the mass balance for the GLERL Large Basin Runoff
      !  Model, a tank cascade model of basin storages and runoff.
      !
      !   NetSupplyIn = net supply volume to the watershed surface, cub. m.
      !   SupplyVol   = supply volume to surface storage from outside, cub. m.
      !   OutFlw      = basin outflow volume, cub. m.
      !
      !   EvPRp = potential evapotranspiration rate, cub. m./ day
      !   NSR   = net supply rate, cub. m. / day
      !   USR   = supply rate to surface storage from outside, cub. m. /day         !tecii09apr02
      !   vDPr  = deep percolation volume, cub. m.
      !   vGEv  = groundwater zone evapotranspiration volume, cub. m.
      !   vInf  = infiltration volume, cub. m.
      !
      !   A, B, C, D, E, F, G, H, L, M,   \ /  dummy variables for storage
      !   N, O, P, R, and T               / \  of intermediate results
      !
      !-----------------------------------------------------------------
      SUBROUTINE OutFlow (NetSupplyIn, SupplyVol, OutFlw)             
      IMPLICIT NONE
      REAL, INTENT(IN)  :: NetSupplyIn, SupplyVol
      REAL, INTENT(OUT) :: OutFlw

      REAL  :: EvPRp, vDPr, vGEv, vInf
      REAL  :: NS, US, NSR, USR
      REAL  :: A, B, C, D, E, F, G, H, L, M, N, O, P, R, T
      
      NS = NetSupplyIn
      US = SupplyVol

      NSR = NS / CID
      USR = US / CID          
      EvPRp = Hplse / 2. / CID
      
      !
      !     Use iterative subroutine to determine EvPRp, if necessary.
      !
      IF (EvPRp .GT. Dpsilon) CALL EvpRate(NSR, EvPRp)
      B = NSR/USZC + AlpPer + AlpUEv * EvPRp
      C = AlpPer * NSR / B
      A = AlpPer * Uszm - C
      T = EPN(-B * CID) * A / AlpPer + NSR / B
      UszmAvg = (A / B * (1. - EPN(-B * CID)) / CID + C) / AlpPer
      R = NS + Uszm - T
      Uszm = T
      vInf = NS - R * NSR / USZC / B
      vPer = R * AlpPer / B
      vRun = NS - vInf
      vUEv = R - vRun - vPer
      D = AlpInt + AlpDPr + Alplev * EvPRp
      IF (ABS((D - B) / D) .LE. Epsilon .OR. ABS(D - B) .LE. Hpsilon) THEN
        T = (Lszm + A * CID - C / D) * EPN(-D * CID) + C / D
        F = A
        G = C / D
        E = Lszm - G
        LszmAvg = (E / D * (1. - EPN(-D * CID)) + F /D ** 2 * (1. -       &
                  (D * CID + 1.) * EPN(-D * CID))) / CID + G
        R = vPer + Lszm - T
        IF (AlpDPr .GE. Dpsilon) THEN
          E = AlpDPr * E
          F = AlpDPr * F
          G = AlpDPr * G
        ENDIF
        Lszm = T
        vInt = R * AlpInt / D
        vDPr = R * AlpDPr / D
        vLEv = R - vInt - vDPr
        H = AlpGw + AlpGEv * EvPRp
        IF (ABS((H - D) / H) .LE. Epsilon .OR. ABS(H - D) .LE. Hpsilon) THEN
          T = (Gzm + E * CID + F/2. * CID**2 - G/H) * EPN(-H * CID) + G/H
          IF (AlpDPr .LT. Dpsilon) T = Gzm * EPN(-H * CID)
          R = vDPr + Gzm - T
          IF (AlpDPr .LT. Dpsilon) THEN
            L = AlpInt * E + A * NSR / USZC / AlpPer
            M = AlpInt * F
            N = 0.
            O = AlpInt * G + C * NSR / USZC / AlpPer
          ELSE
            L = AlpGw * (Gzm - G / H) + AlpInt / AlpDPr * E +             &
                NSR / USZC / AlpPer * A
            M = AlpGw * E + AlpInt / AlpDPr * F
            N = AlpGw * F / 2.
            O = C * NSR / USZC / AlpPer + G * (AlpInt / AlpDPr + AlpGw / H)
          ENDIF
          Gzm = T
          vGw = R * AlpGw / H
          vGEv = R - vGw
          P = AlpSf + AlpSEv * EvPRp
          IF (ABS((P-H) / P) .LE. Epsilon .OR. ABS(P-H) .LE. Hpsilon) THEN
            T = (Ss + L * CID + M / 2. * CID ** 2 + N / 3. * CID ** 3          &
                - O / P) * EPN(-P * CID) + O / P
          ELSE
            T = (Ss - L / (P-H) + M / (P-H) ** 2 - 2. * N / (P-H)**3 -         &
                O/P) * EPN(-P * CID) + (L / (P-H) + M / (P-H)**2 *             &
                ((P-H) * CID - 1.) + N / (P-H) * CID**2 - 2. * N /             &
                (P-H)**3 * ((P - H) * CID - 1.)) * EPN(-H * CID) + O / P
          ENDIF
        ELSE
          T = (Gzm - E / (H-D) + F / (H-D)**2 - G/H) * EPN(-H * CID) +         &
              (E / (H-D) + F / (H-D) ** 2 * ((H-D) * CID - 1.)) *              &
              EPN(-D * CID) + G / H
          IF (AlpDPr .LT. Dpsilon) T = Gzm * EPN(-H * CID)
          R = vDPr + Gzm - T
          IF (AlpDPr .LT. Dpsilon) THEN
            L = 0.
            M = AlpInt * E + A * NSR / USZC / AlpPer
            N = AlpInt * F
            O = AlpInt * G + C * NSR / USZC / AlpPer
          ELSE
            L = AlpGw * (Gzm + F / (H - D) ** 2 - E / (H - D) - G / H)
            M = AlpGw * (E / (H - D) - F / (H - D) ** 2) + AlpInt              &
                / AlpDPr * E + (NSR / USZC / AlpPer) * A
            N = F * (AlpGw / (H - D) + AlpInt / AlpDPr)
            O = C * NSR / USZC / AlpPer + G * (AlpInt/AlpDPr + AlpGw/H)
          ENDIF
          Gzm = T
          vGw = R * AlpGw / H
          vGEv = R - vGw
          P = AlpSf + AlpSEv * EvPRp
          IF (ABS((P-H) / P) .LE. Epsilon .OR. ABS(P-H) .LE. Hpsilon) THEN
            T = (Ss + L * CID - M / (P-D) + N / (P-D) ** 2 - O / P) *          &
                EPN(-P * CID) + (M / (P-D) + N / (P-D)**2 *                    &
                ((P-D) * CID - 1.)) * EPN(-D * CID) + O / P
          ELSE
            IF (ABS((P-D) / P) .LE. Epsilon .OR. ABS(P-D) .LE. Hpsilon) THEN
              T = (Ss - L / (P-H) + M * CID + N / 2. * CID**2 - O/P) *         &
                  EPN(-P * CID) + L / (P - H) * EPN(-H * CID) + O / P
            ELSE
              T = (Ss - L / (P-H) - M / (P-D) + N / (P-D) ** 2 - O/P) *        &
                  EPN(-P * CID) + L / (P - H) * EPN(-H * CID) + O / P +        &
                  (M/(P-D) + N/(P-D)**2 * ((P-D)*CID - 1.)) * EPN(-D*CID)
            ENDIF
          ENDIF
        ENDIF
      ELSE
        F = A / (D - B)
        G = C / D
        E = Lszm - F - G
        T = E * EPN(-D * CID) + F * EPN(-B * CID) + G
        LszmAvg = (E / D * (1. - EPN(-D * CID)) + F / B *                      &
     &            (1. - EPN(-B * CID))) / CID + G
        R = vPer + Lszm - T
        IF (AlpDPr .GE. Dpsilon) THEN
          E = AlpDPr * E
          F = AlpDPr * F
          G = AlpDPr * G
        ENDIF
        Lszm = T
        vInt = R * AlpInt / D
        vDPr = R * AlpDPr / D
        vLEv = R - vInt - vDPr
        H = AlpGw + AlpGEv * EvPRp
        IF (ABS((H-D) / H) .LE. Epsilon .OR. ABS(H-D) .LE. Hpsilon) THEN
          T = (Gzm + E * CID - F / (H - B) - G / H) * EPN(-H * CID) +          &
     &        F / (H - B) * EPN(-B * CID) + G / H
          IF (AlpDPr .LT. Dpsilon) T = Gzm * EPN(-H * CID)
          R = vDPr + Gzm - T
          IF (AlpDPr .LT. Dpsilon) THEN
            L = AlpInt * E
            M = 0.
            N = AlpInt * F + A * NSR / USZC / AlpPer
            O = AlpInt * G + C * NSR / USZC / AlpPer
          ELSE
            L = AlpGw * (Gzm - F / (H - B) - G / H) + AlpInt / AlpDPr * E
            M = AlpGw * E
            N = A * NSR/USZC/AlpPer + F * (AlpInt/AlpDPr + AlpGw / (H-B))
            O = C * NSR/USZC/AlpPer + G * (AlpInt/AlpDPr + AlpGw / H)
          ENDIF
          Gzm = T
          vGw = R * AlpGw / H
          vGEv = R - vGw
          P = AlpSf + AlpSEv * EvPRp
          IF (ABS((P-H) / P) .LE. Epsilon .OR. ABS(P-H) .LE. Hpsilon) THEN
            T = (Ss + L * CID + M / 2. * CID**2 - N / (P-B) - O / P) *         &
     &          EPN(-P * CID) + N / (P - B) * EPN(-B * CID) + O / P
          ELSE
            IF (ABS((P-B) / P) .LE. Epsilon .OR. ABS(P-B) .LE. Hpsilon) THEN
              T = (Ss - L / (P-H) + M / (P-H)**2 + N * CID - O/P) *            &
     &            EPN(-P * CID) + (L / (P-H) + M / (P-H)**2 *                  &
     &            ((P-H) * CID - 1.)) * EPN(-H * CID) + O / P
            ELSE
              T = (Ss - L / (P-H) + M / (P-H)**2 - N / (P-B) - O/P) *          &
     &            EPN(-P * CID) + (L / (P-H) + M / (P-H)**2 *                  &
     &            ((P-H) * CID - 1.)) * EPN(-H * CID) + N / (P - B) *          &
     &            EPN(-B * CID) + O / P
            ENDIF
          ENDIF
        ELSE
          IF (ABS((H-B) / H) .LE. Epsilon .OR. ABS(H-B) .LE. Hpsilon) THEN
            T = (Gzm - E / (H-D) + F * CID - G / H) * EPN(-H * CID) +          &
     &          E / (H - D) * EPN(-D * CID) + G / H
            IF (AlpDPr .LT. Dpsilon) T = Gzm * EPN(-H * CID)
            R = vDPr + Gzm - T
            IF (AlpDPr .LT. Dpsilon) THEN
              L = AlpInt * F + A * NSR / USZC / AlpPer
              M = AlpInt * E
              N = 0.
              O = AlpInt * G + C * NSR / USZC / AlpPer
            ELSE
              L = AlpGw * (Gzm - E / (H-D) - G / H) +                          &
     &            AlpInt / AlpDPr * F + A * NSR / USZC / AlpPer
              M = E * (AlpInt / AlpDPr + AlpGw / (H - D))
              N = AlpGw * F
              O = C * NSR/USZC/AlpPer + G * (AlpInt/AlpDPr + AlpGw/H)
            ENDIF
            Gzm = T
            vGw = R * AlpGw / H
            vGEv = R - vGw
            P = AlpSf + AlpSEv * EvPRp
            IF (ABS((P-H) / P) .LE. Epsilon .OR. ABS(P-H) .LE. Hpsilon) THEN
              T = (Ss + L * CID - M / (P-D) + N / 2. * CID**2 - O/P) *         &
     &            EPN(-P * CID) + M / (P-D) * EPN(-D * CID) + O/P
            ELSE
              IF (ABS((P-D)/P) .LE. Epsilon .OR. ABS(P-D) .LE. Hpsilon) THEN
                T = (Ss - L / (P-H) + M * CID + N / (P-H)**2 - O/P) *          &
     &              EPN(-P * CID) + (L / (P-H) + N / (P-H)**2  *               &
     &              ((P-H) * CID - 1.)) * EPN(-H * CID) + O/P
              ELSE
                T = (Ss - L / (P-H) - M / (P-D) + N / (P-H)**2 - O/P) *        &
     &              EPN(-P * CID) + (L / (P-H) + N / (P-H)**2 *                &
     &              ((P-H) * CID - 1.)) * EPN(-H * CID) + M / (P-D) *          &
     &              EPN(-D * CID) + O / P
              ENDIF
            ENDIF
          ELSE
            T = (Gzm - E / (H-D) - F / (H-B) - G/H) * EPN(-H * CID) +          &
     &          E / (H-D) * EPN(-D * CID) + F/(H-B) * EPN(-B * CID) + G/H
            IF (AlpDPr .LT. Dpsilon) T = Gzm * EPN(-H * CID)
            R = vDPr + Gzm - T
            IF (AlpDPr .LT. Dpsilon) THEN
              L = 0.
              M = AlpInt * E
              N = AlpInt * F + A * NSR / USZC / AlpPer
              O = AlpInt * G + C * NSR / USZC / AlpPer
            ELSE
              L = AlpGw * (Gzm - E / (H - D) - F / (H - B) - G / H)
              M = E * (AlpInt / AlpDPr + AlpGw / (H - D))
              N = A * NSR/USZC/AlpPer + F * (AlpInt/AlpDPr + AlpGw/(H-B))
              O = C * NSR/USZC/AlpPer + G * (AlpInt/AlpDPr + AlpGw/H)
            ENDIF
            Gzm = T
            vGw = R * AlpGw / H
            vGEv = R - vGw
            P = AlpSf + AlpSEv * EvPRp
            IF (ABS((P-H) / P) .LE. Epsilon .OR. ABS(P-H) .LE. Hpsilon) THEN
              T = (Ss + L * CID - M / (P-D) - N / (P-B) - O/P) *               &
     &            EPN(-P * CID) + M / (P-D) * EPN(-D * CID) + N/(P-B) *        &
     &            EPN(-B * CID) + O / P
            ELSE
              IF (ABS((P-D)/P) .LE. Epsilon .OR. ABS(P-D) .LE. Hpsilon) THEN
                T = (Ss - L / (P-H) + M * CID - N / (P-B) - O/P) *             &
     &              EPN(-P * CID) + L / (P-H) * EPN(-H * CID) + N /            &
     &              (P-B) * EPN(-B * CID) + O / P
              ELSE
                IF (ABS((P-B)/P) .LE. Epsilon .OR. ABS(P-B) .LE. Hpsilon) THEN
                  T = (Ss - L / (P-H) - M / (P-D) + N*CID - O/P) *             &
     &                EPN(-P * CID) + L / (P-H) * EPN(-H * CID) +              &
     &                M / (P-D) * EPN(-D * CID) + O / P
                ELSE
                  T = (Ss - L / (P-H) - M / (P-D) - N / (P-B) - O/P) *         &
     &                EPN(-P * CID) + L / (P-H) * EPN(-H * CID) + M /          &
     &                (P-D) * EPN(-D*CID) + N/(P-B) * EPN(-B*CID) + O/P
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      R = vRun + vInt + vGw + Ss - T
      R = R + US - USR * (1. - EPN(-P * CID)) / P  
      Ns = R * AlpSf / P
      Ss = T
      Ss = Ss + USR * (1. - EPN(-P * CID)) / P     
      Evap = vUEv + vLEv + vGEv + R - Ns
      
      OutFlw = NS
      RETURN
      END SUBROUTINE OUTFLOW
      
!--------------------------------------------------------------------------------      
      !-------------------------------------------------------------------------
      !
      !  Determine EvPRp via iteration.
      !
      !-------------------------------------------------------------------------
      SUBROUTINE EvpRate(NSR, Evp)
      IMPLICIT NONE
      REAL, INTENT(INOUT)  ::  NSR, Evp
      REAL (KIND=REAL8) :: A, B, C, D, E, F, G
      REAL (KIND=REAL8) :: uzAvg, lzAvg, EvPRp, EvPRpo
      
      EvPRp  = Evp * 1.0d0
      EvPRpo = 0.0d0
      DO WHILE (ABS((EvPRp - EvPRpo) / EvPRp) .GT. Gpsilon)
        B = NSR / USZC + AlpPer + AlpUEv * EvPRp
        C = NSR / B
        A = Uszm - C
        uzAvg = A / B * (1. - EPX(-B * CID)) / CID + C
        D = AlpInt + AlpDPr + AlpLEv * EvPRp
        IF (ABS((D-B)/D) .LE. Epsilon .OR. ABS(D-B) .LE. Hpsilon) THEN
          F = AlpPer * A
          G = AlpPer * C / D
          E = Lszm - G
          lzAvg = (E / D * (1. - EPX(-D * CID)) + F / D**2 *              &
                  (1. - (D * CID + 1.) * EPX(-D * CID))) / CID + G
        ELSE
          F = AlpPer * A / (D - B)
          G = AlpPer * C / D
          E = Lszm - F - G
          lzAvg = (E / D * (1. - EPX(-D * CID)) + F / B *                 &
                  (1. - EPX(-B * CID))) / CID + G
        END IF
        EvPRpo = EvPRp
        EvPRp = Hplse / CID / (1. + AlpUEv * uzAvg + AlpLEv * lzAvg)
      END DO
      Evp = REAL(EvPRp)
      RETURN
      END SUBROUTINE EvpRate
      
!--------------------------------------------------------------------------------      
      !--------------------------------------------------------------------------
      !
      !--------------------------------------------------------------------------
      REAL FUNCTION EPN(X)  ! designed for use with negative arguments
      IMPLICIT NONE
      REAL, INTENT(IN) :: X
      IF (X .LT. -87.33) THEN
        EPN = 0.
      ELSE
        EPN = EXP(X)
      ENDIF
      RETURN
      END FUNCTION EPN
      
      !--------------------------------------------------------------------------
      !
      !--------------------------------------------------------------------------
      REAL (KIND=REAL8) FUNCTION EPX(X)  ! designed for use with negative arguments
      IMPLICIT NONE
      REAL (KIND=REAL8), INTENT(IN) :: X
      IF (X .LT. -708.39) THEN
         EPX = 0.
      ELSE
         EPX = EXP(X)
      END IF
      RETURN
      END FUNCTION EPX
      
END MODULE LBRM_Compute

!===========================================================================
!===========================================================================
!=============================================================================
MODULE LBRM_IO
      USE LBRM_Global
      USE GLSHFS_Util
      USE GlerlDataTypesAndUnits
      
CONTAINS
!--------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !
      !  Read the LBRM configuration file.
      !  This just has file names.  Most "configuration" information is 
      !  provided by the parameter file.
      !
      !------------------------------------------------------------------------
      SUBROUTINE ReadConfigFile_LBRM(CnfgFile)
      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(IN) :: CnfgFile
      
      INTEGER :: IOS, U1
      LOGICAL :: FExist
      CHARACTER (LEN=20)  :: Item
      CHARACTER (LEN=180) :: FName
      CHARACTER (LEN=200) :: Line

      !
      !  Initialize the global variables
      !
      LBRM_RunID          = ''           ! user-supplied description of run
      LBRM_ParmFileName   = '---'        ! input parameters, dates, etc
      LBRM_BndcFileName   = '---'        ! input initial & boundary storages
      LBRM_MetFileName    = '---'        ! input forcing meteorology
      LBRM_OutputFilename = '---'        ! output file

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
         CALL ParseCfgLine(Line, Item, FName); IF (Errorlevel .NE. 0) GOTO 899
         CALL Uppercase(Item); IF (ErrorLevel .NE. 0) GOTO 899
         IF (TRIM(Item) .EQ. 'ID')       LBRM_RunID          = TRIM(FName)
         IF (TRIM(Item) .EQ. 'PARMFILE') LBRM_ParmFileName   = TRIM(FName)
         IF (TRIM(Item) .EQ. 'BCFILE')   LBRM_BndcFileName   = TRIM(FName)
         IF (TRIM(Item) .EQ. 'METFILE')  LBRM_MetFileName    = TRIM(FName)
         IF (TRIM(Item) .EQ. 'OUTFILE')  LBRM_OutputFileName = TRIM(FName)
         READ(U1, 1101, IOSTAT=IOS) Line
      END DO
      CLOSE(U1)
      CALL FileWasClosed(U1); IF (ErrorLevel .NE. 0) GOTO 899

      IF (LBRM_ParmFileName(1:3) .EQ. '---') THEN
         ErrorMessage = 'No PARMFILE entry in the LBRM configuration file!'
         CALL PassMsg; GOTO 898
      ENDIF
      IF (LBRM_BndcFileName(1:3) .EQ. '---') THEN
         ErrorMessage = 'No BCFILE entry in the LBRM configuration file!'
         CALL PassMsg; GOTO 898
      ENDIF
      IF (LBRM_MetFileName(1:3) .EQ. '---') THEN
         ErrorMessage = 'No METFILE entry in the LBRM configuration file!'
         CALL PassMsg; GOTO 898
      ENDIF
      IF (LBRM_OutputFileName(1:3) .EQ. '---') THEN
         ErrorMessage = 'No OUTFILE entry in the LBRM configuration file!'
         CALL PassMsg; GOTO 898
      ENDIF
      RETURN

  811 ErrorMessage = 'LBRM: Error opening file '//TRIM(CnfgFile);   CALL PassMsg
      GOTO 898
      
  898 ErrorLevel = 1         ! Return with an error indication.
  899 ErrorMessage = 'Configuration file name is: '//TRIM(CnfgFile);    CALL PassMsg
      ErrorMessage = '[traceback] : ReadConfigFile_LBRM...';     CALL PassMsg
      IF (U1 .GT. 0) THEN
         IF (FileIsOpen(U1)) THEN
            CLOSE(U1, IOSTAT=IOS)
            CALL FileWasClosed(U1)
         END IF
      END IF
      
      RETURN

 1101 FORMAT(A)
      END SUBROUTINE ReadConfigFile_LBRM
      
      
!--------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Read the parameter file for LBRM and store the information in the
      !  module global arrays.  This routine makes no assumption about the
      !  PET method, instead determining it from the file format.
      !------------------------------------------------------------------------
      SUBROUTINE ReadParameterFile_LBRM(ParmFileName, Parm)
      IMPLICIT NONE
      CHARACTER(LEN=*),     INTENT(IN)    :: ParmFileName
      TYPE (TLbrmParmData), INTENT(INOUT) :: Parm
      
      INTEGER :: I, J, M, D, IOS, U1, NumStr, LineNum
      LOGICAL :: FExist
      REAL    :: PVal(25), AirT
      CHARACTER(LEN=80)  :: S      
      CHARACTER(LEN=200) :: Line, Strings(15)
      
      !
      !  Initialization
      !
      Parm%Bsn           = 'xxx'
      Parm%Subbasin      = MissingData_Int
      Parm%Area          = MissingData_Real
      Parm%SDateSeq      = MissingData_Date
      Parm%EDateSeq      = MissingData_Date
      Parm%ParmValues(:) = MissingData_Real
      Parm%Cons          = MissingData_Real
      Parm%Insolation(:) = MissingData_Real
      Parm%B1            = MissingData_Real
      Parm%B2            = MissingData_Real
      Parm%USZC          = MissingData_Real
      Parm%AirTemps(:,:) = MissingData_Real
      Parm%PET_Method    = 0
      
      U1 = -1
      
      !
      !  Verify file existence
      !
      INQUIRE(File=TRIM(ParmFileName), EXIST=FExist)
      IF (.NOT. FExist) THEN
         WRITE(ErrorMessage, 5001) TRIM(ParmFileName);  CALL PassMsg
         GOTO 899
      END IF
      
      !
      !  Open the file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(ParmFileName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)
      
      !
      !  Read the first 3 lines
      !    line 1:  basin code and subbasin
      !    line 2:  subbasin area in sq meters
      !    line 3:  start/end dates (Y-M-D format)
      !
      LineNum = 1
      READ(U1, 1100, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStr .LT. 2) THEN
         WRITE(ErrorMessage, 5010) LineNum, TRIM(ParmFileName);   CALL PassMsg
         GOTO 898
      END IF
      Parm%Bsn = GetLowercase(TRIM(ADJUSTL(Strings(1))))
      S = TRIM(ADJUSTL(Strings(2)))
      READ(S, *, IOSTAT=IOS) Parm%Subbasin
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5011) LineNum, TRIM(ParmFileName);   CALL PassMsg
         GOTO 898
      END IF
      
      LineNum = 2
      READ(U1, 1100, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStr .LT. 1) THEN
         WRITE(ErrorMessage, 5010) LineNum, TRIM(ParmFileName);   CALL PassMsg
         GOTO 898
      END IF
      S = TRIM(ADJUSTL(Strings(1)))
      READ(S, *, IOSTAT=IOS) Parm%Area
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5012) LineNum, TRIM(ParmFileName);   CALL PassMsg
         GOTO 898
      END IF
      
      LineNum = 3
      READ(U1, 1100, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStr .LT. 2) THEN
         WRITE(ErrorMessage, 5010) LineNum, TRIM(ParmFileName);   CALL PassMsg
         GOTO 898
      END IF
      S = TRIM(ADJUSTL(Strings(1)))
      Parm%SDateSeq = DateStringYMDToSeq(S)
      IF (Parm%SDateSeq .EQ. MissingData_Date) THEN
         WRITE(ErrorMessage, 5013) LineNum, TRIM(ParmFileName);   CALL PassMsg
         GOTO 898
      END IF
      S = TRIM(ADJUSTL(Strings(2)))
      Parm%EDateSeq = DateStringYMDToSeq(S)
      IF (Parm%EDateSeq .EQ. MissingData_Date) THEN
         WRITE(ErrorMessage, 5014) LineNum, TRIM(ParmFileName);   CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  The next 25 lines all have the same format -- a single value optionally 
      !  followed by explanatory text.
      !  To reduce code size, I will read all 25 values with a single loop, then
      !  assign them out to their respective meaningful variables.
      !  
      DO I = 1, 25
         LineNum = I + 3
         READ(U1, 1100, ERR=812) Line
         CALL ParseCommaSepLine(Line, Strings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumStr .LT. 1) THEN
            WRITE(ErrorMessage, 5010) LineNum, TRIM(ParmFileName);   CALL PassMsg
            GOTO 898
         END IF
         S = TRIM(ADJUSTL(Strings(1)))
         READ(S, *, IOSTAT=IOS) PVal(I)
         IF (IOS .NE. 0) THEN
            WRITE(ErrorMessage, 5015) LineNum, TRIM(ParmFileName);   CALL PassMsg
            GOTO 898
         END IF
      END DO
      
      !
      !  Assign the values into more permanent variables
      !
      DO I = 1, 9
         Parm%ParmValues(I) = PVal(I)
      END DO
      Parm%Cons = PVal(10)
      DO I = 1, 12
         Parm%Insolation(I) = PVal(10 + I)
      END DO
      Parm%B1   = PVal(23)
      Parm%B2   = PVal(24)
      Parm%USZC = PVal(25)

      !
      !  Now attempt to read a section of air temperature values, which means
      !  that this parameter file is for using the 2016 ET method.
      !
      Parm%PET_Method = 1            ! assume 1982 method for the moment
      READ(U1, 1100, IOSTAT=IOS) Line
      IF (IOS .EQ. 0) THEN
         CALL Lowercase(Line)
         I = INDEX(Line, 'long term air temp')
         IF (I .GT. 0) THEN
            Parm%PET_Method = 2      ! use 2016 method
            READ(U1, 1100, IOSTAT=IOS) Line
            DO D = 1, 31
               READ(U1, 1100, IOSTAT=IOS) Line
               IF (IOS .EQ. 0) THEN
                  CALL ParseCommaSepLine(Line, Strings, NumStr); IF (ErrorLevel .NE. 0) GOTO 899
                  IF (NumStr .GE. 13) THEN
                     READ(Strings(1), *, IOSTAT=IOS) J
                     IF (IOS .NE. 0) J = -1
                     IF (J .EQ. D) THEN
                        DO M = 1, 12
                           READ(Strings(M+1), *, IOSTAT=IOS) AirT
                           IF (IOS .EQ. 0) Parm%AirTemps(M,D) = AirT
                        END DO
                     END IF
                  END IF
               END IF
            END DO
         END IF
      END IF
      
      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN
      
      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening file ' // TRIM(ParmFileName);    CALL PassMsg
      GOTO 898
  812 ErrorMessage = 'Error reading file ' // TRIM(ParmFileName);    CALL PassMsg
      GOTO 898
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ReadParameterFile_LBRM...'; CALL PassMsg
      Parm%Bsn           = 'xxx'
      Parm%Subbasin      = MissingData_Int
      Parm%Area          = MissingData_Real
      Parm%SDateSeq      = MissingData_Date
      Parm%EDateSeq      = MissingData_Date
      Parm%ParmValues(:) = MissingData_Real
      Parm%Cons          = MissingData_Real
      Parm%Insolation(:) = MissingData_Real
      Parm%B1            = MissingData_Real
      Parm%B2            = MissingData_Real
      Parm%USZC          = MissingData_Real
      Parm%AirTemps(:,:) = MissingData_Real
      Parm%PET_Method    = 0
      IF (U1 .GT. 0) THEN
         IF (FileIsOpen(U1)) THEN
            CLOSE(U1)
            CALL FileWasClosed(U1)
         END IF
      END IF
      
 1100 FORMAT(A)
      
 5001 FORMAT('The specified parameter file (', A, ') does not exist.')
 5010 FORMAT('Error parsing line number ', I0, ' of parameter file (', A, ')')
 5011 FORMAT('Error parsing subbasin number on line number ', I0, ' of parameter file (', A, ')')
 5012 FORMAT('Error parsing subbasin area on line number ', I0, ' of parameter file (', A, ')')
 5013 FORMAT('Error parsing start date on line number ', I0, ' of parameter file (', A, ')')
 5014 FORMAT('Error parsing end date on line number ', I0, ' of parameter file (', A, ')')
 5015 FORMAT('Error parsing parameter value on line number ', I0, ' of parameter file (', A, ')')
 

      END SUBROUTINE ReadParameterFile_LBRM

      
 !--------------------------------------------------------------------------
     !------------------------------------------------------------------------
      !  Read the boundary conditions file for LBRM and store the information in
      !  the module global variables. 
      !
      !  Note that the values are read in centimeters.
      !------------------------------------------------------------------------
      SUBROUTINE ReadBoundaryFile_LBRM(BoundFileName, Bndc)
      IMPLICIT NONE
      CHARACTER(LEN=*),     INTENT(IN)    :: BoundFileName
      TYPE (TLbrmBndcData), INTENT(INOUT) :: Bndc
      
      
      INTEGER :: I, J, E, IOS, U1, NumStr, LineNum, Seq
      LOGICAL :: FExist
      REAL    :: SVal
      CHARACTER(LEN=80)  :: S      
      CHARACTER(LEN=200) :: Line, Strings(15)
      
      
      !
      !  Initialization
      !  Cannot initialize the allocatable arrays yet.
      !
      Bndc%Bsn         = 'xxx'
      Bndc%Subbasin    = MissingData_Int
      Bndc%SDateSeq    = MissingData_Date
      Bndc%EDateSeq    = MissingData_Date
      Bndc%NumEntries  = 0
      
      U1 = -1
      
      !
      !  Verify file existence
      !
      INQUIRE(File=TRIM(BoundFileName), EXIST=FExist)
      IF (.NOT. FExist) THEN
         WRITE(ErrorMessage, 5001) TRIM(BoundFileName);  CALL PassMsg
         GOTO 899
      END IF
      
      !
      !  Open the file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(BoundFileName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)
      
      !
      !  Skip 4 lines of header
      !
      DO I = 1, 4
         LineNum = I
         READ(U1, 1100, ERR=812) Line
      END DO
      
      !
      !  Read the header line (line 5) with useful data
      !    basin code, subbasin, start/end dates (Y-M-D format)
      !
      LineNum = 5
      READ(U1, 1100, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStr .LT. 4) THEN
         WRITE(ErrorMessage, 5010) LineNum, TRIM(BoundFileName);   CALL PassMsg
         GOTO 898
      END IF
      Bndc%Bsn = GetLowercase(TRIM(ADJUSTL(Strings(1))))
      
      S = TRIM(ADJUSTL(Strings(2)))
      READ(S, *, IOSTAT=IOS) Bndc%Subbasin
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5011) LineNum, TRIM(BoundFileName);   CALL PassMsg
         GOTO 898
      END IF
      
      S = TRIM(ADJUSTL(Strings(3)))
      Bndc%SDateSeq = DateStringYMDToSeq(S)
      IF (Bndc%SDateSeq .EQ. MissingData_Date) THEN
         WRITE(ErrorMessage, 5013) LineNum, TRIM(BoundFileName);   CALL PassMsg
         GOTO 898
      END IF
      
      S = TRIM(ADJUSTL(Strings(4)))
      Bndc%EDateSeq = DateStringYMDToSeq(S)
      IF (Bndc%EDateSeq .EQ. MissingData_Date) THEN
         WRITE(ErrorMessage, 5014) LineNum, TRIM(BoundFileName);   CALL PassMsg
         GOTO 898
      END IF

      !
      !  Now read all of the relevant data lines.
      !  All we are doing here is getting a count of valid entries.
      !  We will then allocate the RAM, then read this again.
      !
      Bndc%NumEntries = 0
      LineNum = 6
      READ(U1, 1100, ERR=812) Line     ! column header line
      LineNum = 7
      READ(U1, 1100, IOSTAT=IOS) Line     ! first valid data line, in theory
      DO WHILE (IOS .EQ. 0)
         CALL ParseCommaSepLine(Line, Strings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumStr .LT. 6) THEN
            WRITE(ErrorMessage, 5010) LineNum, TRIM(BoundFileName);   CALL PassMsg
            GOTO 898
         END IF
         S = TRIM(ADJUSTL(Strings(1)))
         Seq = DateStringYMDToSeq(S)
         IF (Seq .EQ. MissingData_Date) THEN
            WRITE(ErrorMessage, 5015) LineNum, TRIM(BoundFileName);   CALL PassMsg
            GOTO 898
         END IF
         DO I = 2, 6
            READ(Strings(I), *, IOSTAT=J) SVal
            IF (J .NE. 0) THEN
               WRITE(ErrorMessage, 5016) LineNum, TRIM(BoundFileName);   CALL PassMsg
               GOTO 898
            END IF
         END DO
         Bndc%NumEntries = Bndc%NumEntries + 1
         LineNum = LineNum + 1
         READ(U1, 1100, IOSTAT=IOS) Line
      END DO
      
      !
      !  Allocate the RAM
      !
      IF (Bndc%NumEntries .EQ. 0) THEN
         WRITE(ErrorMessage, 5020) TRIM(BoundFileName);   CALL PassMsg
         GOTO 898         
      END IF
      I = Bndc%NumEntries
      ALLOCATE(Bndc%Dates(I), Bndc%Values(5, I), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5025) Bndc%NumEntries, TRIM(BoundFileName);   CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Reread the data section of the file
      !
      REWIND(U1)
      DO I = 1, 6
         LineNum = I
         READ(U1, 1100, ERR=812) Line
      END DO
      LineNum = 6

      DO E = 1, Bndc%NumEntries
         LineNum = LineNum + 1
         READ(U1, 1100, ERR=812) Line
         CALL ParseCommaSepLine(Line, Strings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumStr .LT. 6) THEN
            WRITE(ErrorMessage, 5010) LineNum, TRIM(BoundFileName);   CALL PassMsg
            GOTO 898
         END IF
         S = TRIM(ADJUSTL(Strings(1)))
         Seq = DateStringYMDToSeq(S)
         IF (Seq .EQ. MissingData_Date) THEN
            WRITE(ErrorMessage, 5015) LineNum, TRIM(BoundFileName);   CALL PassMsg
            GOTO 898
         END IF
         Bndc%Dates(E) = Seq
         DO I = 2, 6
            READ(Strings(I), *, IOSTAT=J) SVal
            IF (J .NE. 0) THEN
               WRITE(ErrorMessage, 5016) LineNum, TRIM(BoundFileName);   CALL PassMsg
               GOTO 898
            END IF
            Bndc%Values(I-1, E) = SVal         ! centimeters
         END DO
      END DO
      
      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN

      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening file ' // TRIM(BoundFileName);    CALL PassMsg
      GOTO 898
  812 ErrorMessage = 'Error reading file ' // TRIM(BoundFileName);    CALL PassMsg
      GOTO 898
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ReadBoundaryFile_LBRM...'; CALL PassMsg
      Bndc%Bsn        = 'xxx'
      Bndc%Subbasin   = MissingData_Int
      Bndc%SDateSeq   = MissingData_Date
      Bndc%EDateSeq   = MissingData_Date
      Bndc%NumEntries = 0
      IF (ALLOCATED(Bndc%Dates))  DEALLOCATE(Bndc%Dates,  STAT=IOS)
      IF (ALLOCATED(Bndc%Values)) DEALLOCATE(Bndc%Values, STAT=IOS)
      IF (U1 .GT. 0) THEN
         IF (FileIsOpen(U1)) THEN
            CLOSE(U1)
            CALL FileWasClosed(U1)
         END IF
      END IF

      
 1100 FORMAT(A)
      
 5001 FORMAT('The specified boundary conditions file (', A, ') does not exist.')
 5010 FORMAT('Error parsing line number ', I0, ' of boundary file (', A, ')')
 5011 FORMAT('Error parsing subbasin number on line number ', I0, ' of boundary file (', A, ')')
 5013 FORMAT('Error parsing start date on line number ', I0, ' of boundary file (', A, ')')
 5014 FORMAT('Error parsing end date on line number ', I0, ' of boundary file (', A, ')')
 5015 FORMAT('Error parsing date on line number ', I0, ' of boundary file (', A, ')')
 5016 FORMAT('Error parsing boundary conditions on line number ', I0, ' of boundary file (', A, ')')
 5020 FORMAT('Zero valid boundary conditions found in the file (', A, ').')
 5025 FORMAT('Error allocating memory for ', I0, ' boundary conditions from file (', A, ').')
      
      END SUBROUTINE ReadBoundaryFile_LBRM
      
      
!--------------------------------------------------------------------------
      !------------------------------------------------------------------------
      !  Read the input meteorology file for LBRM and store the information in
      !  the module global variables. 
      !------------------------------------------------------------------------
      SUBROUTINE ReadMeteorologyFile_LBRM(MetFileName, Met)
      IMPLICIT NONE
      CHARACTER(LEN=*),    INTENT(IN)    :: MetFileName
      TYPE (TLbrmMetData), INTENT(INOUT) :: Met
      
      INTEGER :: D, I, J, K, IOS, U1, NumStr, LineNum
      INTEGER :: Seq, SSeq, ESeq
      LOGICAL :: FExist
      REAL    :: SVal
      CHARACTER(LEN=80)  :: S      
      CHARACTER(LEN=200) :: Line, Strings(15)
      
      !
      !  Initialization
      !  Cannot initialize the allocatable arrays yet.
      !
      Met%Bsn         = 'xxx'
      Met%Subbasin    = MissingData_Int
      Met%SDateSeq    = MissingData_Date
      Met%EDateSeq    = MissingData_Date
      Met%NumDays     = 0

      U1 = -1
      
      !
      !  Verify file existence
      !
      INQUIRE(File=TRIM(MetFileName), EXIST=FExist)
      IF (.NOT. FExist) THEN
         WRITE(ErrorMessage, 5001) TRIM(MetFileName);  CALL PassMsg
         GOTO 899
      END IF
      
      !
      !  Open the file
      !
      U1 = GetFreeUnitNumber()
      OPEN(UNIT=U1, FILE=TRIM(MetFileName), STATUS='OLD', ERR=811)
      CALL FileWasOpened(U1)
      
      !
      !  Read the header section (3 lines)
      !
      LineNum = 1
      READ(U1, 1100, ERR=812)      ! skip it

      LineNum = 2
      READ(U1, 1100, ERR=812) Line
      CALL ParseCommaSepLine(Line, Strings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
      IF (NumStr .LT. 2) THEN
         WRITE(ErrorMessage, 5010) LineNum, TRIM(MetFileName);   CALL PassMsg
         GOTO 898
      END IF
      Met%Bsn = GetLowercase(TRIM(ADJUSTL(Strings(1))))
      
      S = TRIM(ADJUSTL(Strings(2)))
      READ(S, *, IOSTAT=IOS) Met%Subbasin
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5011) LineNum, TRIM(MetFileName);   CALL PassMsg
         GOTO 898
      END IF

      LineNum = 3
      READ(U1, 1100, ERR=812)      ! skip it

      !
      !  Read lines 4-end to determine the date extent
      !
      SSeq = DateSeq_MaxValue
      ESeq = DateSeq_MinValue
      LineNum = 4
      READ(U1, 1100, IOSTAT=IOS) Line
      DO WHILE (IOS .EQ. 0)
         CALL ParseCommaSepLine(Line, Strings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899        
         IF (NumStr .LT. 4) THEN
            WRITE(ErrorMessage, 5010) LineNum, TRIM(MetFileName);   CALL PassMsg
            GOTO 898
         END IF
         
         S = TRIM(ADJUSTL(Strings(1)))
         Seq = DateStringYMDToSeq(S)
         IF (Seq .EQ. MissingData_Date) THEN
            WRITE(ErrorMessage, 5012) LineNum, TRIM(MetFileName);   CALL PassMsg
            GOTO 898
         END IF
         IF (Seq .LT. SSeq) SSeq = Seq
         IF (Seq .GT. ESeq) ESeq = Seq
         
         LineNum = LineNum + 1
         READ(U1, 1100, IOSTAT=IOS) Line
      END DO

      !
      !  Allocate the memory for the met data
      !
      IF (ESeq .LT. SSeq) THEN
         WRITE(ErrorMessage, 5015) TRIM(MetFileName);  CALL PassMsg
         GOTO 898
      END IF
      Met%SDateSeq = SSeq
      Met%EDateSeq = ESeq
      Met%NumDays  = ESeq - SSeq + 1
      
      ALLOCATE(Met%Values(3, Met%NumDays),  STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5025) Met%NumDays, TRIM(MetFileName);   CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Read the data section of the file again, this time saving values
      !
      REWIND(U1)
      DO I = 1, 3
         LineNum = I
         READ(U1, 1100, ERR=812) 
      END DO
      LineNum = 3

      DO D = 1, Met%NumDays
         LineNum = LineNum + 1
         READ(U1, 1100, ERR=812) Line
         CALL ParseCommaSepLine(Line, Strings, NumStr);  IF (ErrorLevel .NE. 0) GOTO 899
         IF (NumStr .NE. 4) THEN
            WRITE(ErrorMessage, 5010) LineNum, TRIM(MetFileName);   CALL PassMsg
            GOTO 898
         END IF
         S = TRIM(ADJUSTL(Strings(1)))
         Seq = DateStringYMDToSeq(S)
         IF (Seq .EQ. MissingData_Date) THEN
            WRITE(ErrorMessage, 5012) LineNum, TRIM(MetFileName);   CALL PassMsg
            GOTO 898
         END IF
         K = Seq - Met%SDateSeq + 1
         DO I = 2, 4
            READ(Strings(I), *, IOSTAT=J) SVal
            IF (J .NE. 0) THEN
               WRITE(ErrorMessage, 5013) LineNum, TRIM(MetFileName);   CALL PassMsg
               GOTO 898
            END IF
            Met%Values(I-1, K) = SVal
         END DO
      END DO
      
      CLOSE(U1)
      CALL FileWasClosed(U1)
      RETURN

      !
      !  Error handling
      !
  811 ErrorMessage = 'Error opening file ' // TRIM(MetFileName);    CALL PassMsg
      GOTO 898
  812 ErrorMessage = 'Error reading file ' // TRIM(MetFileName);    CALL PassMsg
      GOTO 898
      
  898 ErrorLevel = 1
  899 ErrorMessage = '[traceback] ReadMeteorologyFile_LBRM...'; CALL PassMsg
      Met%Bsn        = 'xxx'
      Met%Subbasin   = MissingData_Int
      Met%SDateSeq   = MissingData_Date
      Met%EDateSeq   = MissingData_Date
      Met%NumDays = 0
      IF (ALLOCATED(Met%Values)) DEALLOCATE(Met%Values, STAT=IOS)
      IF (U1 .GT. 0) THEN
         IF (FileIsOpen(U1)) THEN
            CLOSE(U1)
            CALL FileWasClosed(U1)
         END IF
      END IF

      
 1100 FORMAT(A)
      
 5001 FORMAT('The specified meteorology file (', A, ') does not exist.')
 5010 FORMAT('Error parsing line number ', I0, ' of meteorology file (', A, ')')
 5011 FORMAT('Error parsing subbasin number on line number ', I0, ' of meteorology file (', A, ')')
 5012 FORMAT('Error parsing date on line number ', I0, ' of meteorology file (', A, ')')
 5013 FORMAT('Error parsing data values on line number ', I0, ' of meteorology file (', A, ')')
 5015 FORMAT('Error with date range for meteorology file (', A, ')')
 5025 FORMAT('Error allocating memory for ', I0, ' days of meteorology from file (', A, ')')
      
      END SUBROUTINE ReadMeteorologyFile_LBRM
      
!--------------------------------------------------------------------------------
      !--------------------------------------------------------------------------
      !
      !--------------------------------------------------------------------------
      SUBROUTINE WriteOutputFile_LBRM(OutputFilename, Output)
      IMPLICIT NONE
      CHARACTER(LEN=*),                  INTENT(IN)    :: OutputFileName
      TYPE (TLbrmOutputData), INTENT(INOUT) :: Output
      
      INTEGER :: I, Dy, Mn, Yr, Seq, U1
      REAL :: Runoff, Uszm, Lszm, Gzm, Ss, Snw
      CHARACTER(LEN=200) :: Line
      
      !
      !     Open the file for output and start writing the header lines
      !
      U1 = GetFreeUnitNumber(); IF (ErrorLevel .NE. 0) GOTO 899
      OPEN(UNIT=U1, FILE=TRIM(OutputFilename), STATUS='REPLACE', ERR=811)
      CALL FileWasOpened(U1); IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(U1, 4101, ERR=813) 
      WRITE(U1, 4102, ERR=813) Output%Bsn
      WRITE(U1, 4103, ERR=813) Output%Subbasin
      WRITE(U1, 4104, ERR=813) Output%Area
      CALL SequenceDate(Dy, Mn, Yr, Output%SDateSeq); IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(U1, 4105, ERR=813) Yr, Mn, Dy
      CALL SequenceDate(Dy, Mn, Yr, Output%EDateSeq); IF (ErrorLevel .NE. 0) GOTO 899
      WRITE(U1, 4106, ERR=813) Yr, Mn, Dy
      WRITE(U1, 4107, ERR=813)
      
      !
      !  Construct and write header line 8 (data types)
      !
      Line = 'date,'
      Line = TRIM(Line) // TRIM(GlerlDataTypeString10(GDT_Runoff))              // ','
      Line = TRIM(Line) // TRIM(GlerlDataTypeString10(GDT_UpperSoilMoisture))   // ','
      Line = TRIM(Line) // TRIM(GlerlDataTypeString10(GDT_LowerSoilMoisture))   // ','
      Line = TRIM(Line) // TRIM(GlerlDataTypeString10(GDT_GroundWaterMoisture)) // ','
      Line = TRIM(Line) // TRIM(GlerlDataTypeString10(GDT_SurfaceZoneMoisture)) // ','
      Line = TRIM(Line) // TRIM(GlerlDataTypeString10(GDT_SnowWater))
      WRITE(U1, 4108, ERR=813) TRIM(Line)

      !
      !  Header line 9 (data units)
      !
      WRITE(U1, 4109, ERR=813) (TRIM(GlerlDataUnitString10(GDU_Millimeters)), I=1,6)
      
      !
      DO Seq = Output%SDateSeq, Output%EDateSeq
         CALL SequenceDate(Dy, Mn, Yr, Seq); IF (ErrorLevel .NE. 0) GOTO 899
         I = Seq - Output%SDateSeq + 1
         
         !
         !  The runoff and storages are provided in cubic meters, but converted
         !  to mm over the subbasin.
         !  The original LBRM produced output in centimeters over the area,
         !  rounded to the nearest tenth of a mm.  When these values were
         !  plotted, the resulting output had a very "stepped" look due to the
         !  inherent rounding when doing the volume->depth conversion. 
         !  I very strongly wanted to fix that in this version.
         !
         !  My original thought had been to output cubic meters with this version, 
         !  but after seeing the resulting output file I had a change of heart.
         !  When using the standard F0.2 output format, the file was quite "ugly".
         !  So my solution is just to switch to millimeters instead of centimeters.
         !  The output file is being rounded to 2 digits after the decimal, which
         !  will result in 10 times the resolution. That should help fix the 
         !  step-function look, at least to a decent degree.
         !
         !   (Tim Hunter, March 2017)
         !
         Runoff = Output%Values(1,I) / Output%Area * 1000.0     ! m3 -> mm
         Uszm   = Output%Values(2,I) / Output%Area * 1000.0     ! m3 -> mm
         Lszm   = Output%Values(3,I) / Output%Area * 1000.0     ! m3 -> mm
         Gzm    = Output%Values(4,I) / Output%Area * 1000.0     ! m3 -> mm
         Ss     = Output%Values(5,I) / Output%Area * 1000.0     ! m3 -> mm
         Snw    = Output%Values(6,I) / Output%Area * 1000.0     ! m3 -> mm
         
         WRITE(U1, 4110, ERR=813) Yr, Mn, Dy, Runoff, Uszm, Lszm, Gzm, Ss, Snw
      END DO
      CLOSE (U1)
      CALL FileWasClosed(U1);  IF (ErrorLevel .NE. 0) GOTO 899
      RETURN

      !
      !  Error handling
      !
  811 ErrorMessage = 'LBRM: Error opening file '//TRIM(OutputFilename)
      CALL PassMsg; GOTO 898
  813 ErrorMessage = 'LBRM: Error writing file '//TRIM(OutputFilename)
      CALL PassMsg; GOTO 898

  898 ErrorLevel = 1  ! Return with an error indication.
  899 ErrorMessage = '[traceback] : WriteOutputFile_LBRM...'
      CALL PassMsg
      RETURN

 4101 FORMAT('Output from LBRM.')
 4102 FORMAT('Lake:, ', A3)
 4103 FORMAT('Subbasin:, ', I0)
 4104 FORMAT('Area:, ', E13.5E2)
 4105 FORMAT('Starts(YMD):, ', I4.4, 2('-',I2.2))
 4106 FORMAT('Ends(YMD):,   ', I4.4, 2('-',I2.2))
 4107 FORMAT('Runoff and storages expressed in millimeters over the subbasin.')
 4108 FORMAT(A)
 4109 FORMAT('YYYY-MM-DD', 6(', ', A))
 4110 FORMAT(I4.4, 2('-', I2.2), 6(',', F9.2))
 
 
      END SUBROUTINE WriteOutputFile_LBRM
      
END MODULE LBRM_IO



!===========================================================================
!===========================================================================
!===========================================================================
MODULE LBRM_Main
      USE LBRM_Global
      USE LBRM_Compute
      USE LBRM_IO

CONTAINS

      SUBROUTINE RunTheLBRM(CfgFileName)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: CfgFileName
      
      INTEGER :: IOS, Mode, NumDays
      REAL    :: ComputedCons

      TYPE (Comprehensive_Lbrm_Data) :: LbrmData
      
      !
      !  Call the subroutine to read the config file in order to get the
      !  names of the parameter, boundary, met and output files.
      !  These file names get assigned into global variables declared in
      !  the LBRM_Global module.
      !
      CALL ReadConfigFile_LBRM(CfgFileName); IF (ErrorLevel .NE. 0) GOTO 899

      !      
      !  Read the 3 input files. 
      !  All of the info needed to run the LBRM is contained in those files,
      !  so we really don't need to do much here except be sure that worked.
      !
      CALL ReadParameterFile_LBRM  (LBRM_ParmFileName, LbrmData%Parm);  IF (ErrorLevel .NE. 0) GOTO 899
      CALL ReadBoundaryFile_LBRM   (LBRM_BndcFileName, LbrmData%Bndc);  IF (ErrorLevel .NE. 0) GOTO 899
      CALL ReadMeteorologyFile_LBRM(LBRM_MetFileName,  LbrmData%Met);   IF (ErrorLevel .NE. 0) GOTO 899
      
      !
      !  Allocate memory for storing the daily time-series of results.
      !  These are module global arrays declared in LBRM_Global
      !    DlyOutput  = daily runoff and storages, in cubic meters
      !                (1,:) = total runoff out of the watershed
      !                (2,:) = upper soil zone moisture
      !                (3,:) = lower soil zone moisture
      !                (4,:) = ground zone moistuire
      !                (5,:) = surface zone moisture
      !                (6,:) = snow pack moisture (snow water equivalent)
      !
      NumDays = LbrmData%Parm%EDateSeq - LbrmData%Parm%SDateSeq + 1
      ALLOCATE(LbrmData%Output%Values(6,NumDays), STAT=IOS)
      IF (IOS .NE. 0) THEN
         WRITE(ErrorMessage, 5001) NumDays;       CALL PassMsg
         GOTO 898
      END IF
      
      !
      !  Calculate things for the entire period of interest
      !
      !     Mode = 0,  standard simulation mode
      !     Mode = 1,  calibration mode (calculate ComputedCons "on the fly")
      !
      Mode = 0
      CALL LBRM_Engine(Mode, ComputedCons, LbrmData);   IF (ErrorLevel .NE. 0) GOTO 899

      !
      !
      !
      CALL WriteOutputFile_LBRM(LBRM_OutputFileName, LbrmData%Output);   IF (ErrorLevel .NE. 0) GOTO 899

      GOTO 999

      !
      !  Error handling
      !
  898 ErrorLevel = 1  ! Return with an error indication.
  899 ErrorMessage = '[traceback] : LBRM_Main...';     CALL PassMsg
  
      !
      !  Final cleanup
      !
  999 CALL ClearLbrmData(LbrmData)
  
      RETURN

 5001 FORMAT('Error allocating memory for storage of LBRM outputs. NumDays=', I0)
      
      END SUBROUTINE RunTheLBRM

END MODULE LBRM_Main

