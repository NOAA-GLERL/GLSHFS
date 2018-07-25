!------------------------------------------
!  This file contains variable items that need to be accessible 
!  throughout GLSHFS. 
!
!  Tim Hunter, 22 Aug2016
!------------------------------------------
MODULE GLSHFS_Global
      USE ErrorProcess
      USE GLSHFS_Util
      USE GL_Constants
      IMPLICIT NONE

      !
      !  Define some control parameters that also get used for sizing arrays, etc
      !
      INTEGER, PARAMETER :: MaxScenarioCount = 300
      INTEGER, PARAMETER :: GLShfsNumBasins = 9
      INTEGER, PARAMETER :: MaxNumberOfStations = 2500
      INTEGER, PARAMETER :: MaxForecastLengthInYears  = 15
      INTEGER, PARAMETER :: MaxForecastLengthInMonths = MaxForecastLengthInYears * 12
      INTEGER, PARAMETER :: MaxForecastLengthInDays   = MaxForecastLengthInYears * 366
      
      !
      !  Define a character string variable to hold the name of a "temp" directory.
      !  We will use it this way:
      !   1) At the start of a GLSHFS run, we will define a value
      !   2) We will immediately create the directory (if needed).
      !   3) At any point during the run of GLSHSF, if we need a temporary storage
      !      folder (e.g. a place to put the target file for output from a "dir/b" 
      !      or "ls -1" operation to create a file list) we will use this variable.
      !   4) Whenever we create a file here, we delete it when done.
      !   5) We do NOT indiscriminately clear out the contents of the directory with
      !      something like "rm *.*"
      !
      CHARACTER(LEN=200) :: GlshfsTempDir
      
      !
      !  Define structured type for configuration info
      !
      TYPE GLSHFS_ConfigData
         CHARACTER(LEN=80)  :: ConfigName
         CHARACTER(LEN=150) :: BaseDir, PrgDir, StnDir

         !
         !  Operations to perform, if set to TRUE:
         !
         !  1) Add station data to the stndata_<datatype>_<lake>.csv files
         !  2) Compute new subbasin values from the data in stndata_<datatype>_<lake>.csv
         !  3) Run the models for the historical period
         !  4) Run models forecast mode, using settings specified in other items
         !
         LOGICAL :: AddStationData  
         LOGICAL :: BuildSubbasinMet
         LOGICAL :: UpdateHistorical
         LOGICAL :: DoForecasts

         !
         !  Some control over what forecast output files are generated.
         !  Reducing the number of files can speed up execution if these files
         !  are of no interest to the user.
         !
         LOGICAL :: MakeSubbasinSummaries
         
         !
         !  Clean up temporary forecast files?
         !  A bunch of files are created for each scenario & subbasin in order to
         !  run the LBRM and LLTM for each scenario. The results are summarized in 
         !  the summary files, making most/all of these individual temporary files 
         !  redundant or extraneous.  By setting this to TRUE, we can eliminate 
         !  thousands of files from the output directory.
         !
         LOGICAL :: ClearTemporaryForecastFiles
         
         !
         !  Maximum range for using a station for a particular lake basin (kilometers).
         !  Stations within this buffer zone around a basin will be used when
         !  computing the lumped subbasin meteorology.
         !
         REAL  :: BasinBoundaryBuffer
         
         !
         !  User-specified forecast stuff.
         !
         !  If ForecastStartDate = 8888-08-08 then it is a flag value indicating
         !  that the forecast should start at the end of the available historical
         !  data.
         !  If ForecastStartDate = 9999-12-31 (MissingData_Date) then the user did 
         !  not specify a valid value.  This is ok if we are just processing station 
         !  data or updating for the historical data period, but it will cause an error
         !  if we try to do a forecast.
         !
         !  ForecastName is a name for the entire forecast set, and should be specified
         !  in all cases (using historical data or user-supplied files).  A directory 
         !  with this name will be used (created if needed) to store all of the output
         !  files. In the case where the user is supplying input files, those input
         !  files will also be required to be in this directory. Suggest that it be 
         !  kept pretty short (i.e. no more than 10-12 characters), but can be up to 50.
         !
         !  ForecastSource is the indicator for the source of outlook meteorology.
         !
         !  A value of 1 ("extractfromhist" in the file) means that the outlook met will
         !  be extracted from the observed historical datasets in GLSHFS. 
         !  These files will be named otlkdata_<bsn><sub>_<syr>.csv (e.g. otlkdata_sup01_1948.csv)
         !    <bsn> = 3-character lake name
         !    <sub> = 2-digit subbasin number (00..numsubbasins)
         !    <syr> = 4-digit year that is the start year for the source of extracted data
         !
         !  A value of 2 ("usersupplied" in the file) means that the user will need to 
         !  supply a complete set of input files stored in the ForecastName directory.
         !  These files will be named otlkdata_<bsn><sub>_<scenario>.csv (e.g. otlkdata_sup01_sc1.csv)
         !    <bsn> = 3-character lake name
         !    <sub> = 2-digit subbasin number (00..numsubbasins)
         !    <scenario> = some text string that uniquely identifies the scenario
         !
         !  ScenarioNames is a list of one or more comma-separated scenario names.  
         !  If the user has chosen "extractfromhist" as the forecast met source, then 
         !  he has 2 options:
         !    a) Specify "all" for the single entry here. The scenario names will be
         !       automatically built based on the available data in the historical
         !       met data files.
         !    b) Specify a list of years (e.g. 1950, 1951, 1952, 1980, 1987).  These will
         !       be the scenario names, and only files for those years will be extracted.
         !       Note that if the user specifies an invalid year (e.g. 1937 but the files
         !       don't start until 1948), then that entry will simply be ignored.
         !
         !  If the user has chosen "usersupplied" as the forecast met source, then he
         !  must explicitly name each scenario, and GLSHFS is going to assume that all
         !  required data files are being supplied. If any of them are missing, it will
         !  cause GLSHFS to abort when it reaches the point where the missing file is
         !  used.  Each scenario name must be a 6-character (or less) string and the
         !  valid character set is all alphabetic/numeric characters plus dash and underscore.
         !  Imbedded whitespace (blanks) within a scenario name is NOT acceptable. 
         !  Due to the naming restrictions, double quotes should not be supplied around 
         !  each name. As an example, if the scenario names are:
         !     Cld-Wt
         !     CldDry
         !     Warm-W
         !     Hot_D
         !     C_Wet
         !  then the configuration line could be something like:
         !     ScenarioNames = Cld-Wt, CldDry, Warm-W, Hot_D, C_Wet
         !  or (for example)
         !     ScenarioNames=Cld-Wt,CldDry,Warm-W,   Hot_D,C_Wet
         !  
         !         
         INTEGER :: ForecastStartSeq           ! forecast start date
         INTEGER :: ForecastLen                ! months
         CHARACTER(LEN=50) :: ForecastName     ! forecast "set" id
         INTEGER :: ForecastMetSource          ! 1 = extract from historical data files
                                               ! 2 = use files supplied by user, externally
                                               ! 3 = use CMIP-derived data files supplied by user
         CHARACTER(LEN=150) :: UserMetLocation ! directory containing user-supplied met files
         CHARACTER(LEN=6), DIMENSION(:), ALLOCATABLE :: ScenarioNames
         
         !
         !  LBRM options
         !
         CHARACTER(LEN=10) :: LbrmMethodET           ! currently "1982" or "2016"
         
         !
         !  LLTM options
         !
         !  LltmMethodRadiation____ specifies how to compute the Incident and Net 
         !    Longwave radiation values for the historical data period and for the
         !    forecast period.
         !    1 = Read cloudcover and compute both Incident and NetLW.
         !        This is the historical method that allows us to use the data
         !        most readily available at station locations.
         !    2 = Read Incident and Longwave radiation directly. The Longwave radiation
         !        is interpreted as INCOMING longwave. Net longwave will be computed
         !        by LLTM.
         !        This is the option that we need to choose when using the
         !        output from CMIP models. The NetLW radiation that comes out of
         !        those models apparently does not properly account for the lake, 
         !        and the net value that is calculated and output is significantly
         !        wrong.  Seasonality is reversed, etc.
         !    3 = Read Incident and Longwave directly. Treat the longwave radiation
         !        as a correct computation of the net value and use both radiation 
         !        values as direct inputs.
         !        This will probably be used only when we are using output from
         !        some sort of coupled atmosphere-water model that correctly 
         !        computes the net LW value.
         !
         !  LltmApplyDataCorrections specifies whether we need to apply overwater
         !    corrections to the 
         !
         INTEGER :: LltmMethodRadiationHist        !  1 = cloudcover;  2 = incoming longwave;  3 = direct values
         INTEGER :: LltmMethodRadiationFcst        !  1 = cloudcover;  2 = incoming longwave;  3 = direct values
         LOGICAL :: LltmApplyDataCorrectionsHist
         LOGICAL :: LltmApplyDataCorrectionsFcst
         
         !
         !  What string will be used to denote missing data values in output files?
         !  Suggested values are:
         !      "NA"           works well with R scripts
         !      "#N/A"         works well with Excel
         !      "-9.999999e9"  or similar nonsense value that acts like a flag
         !      ""             blank might work well for some folks
         !  
         !  When READING files, though, this setting is essentially ignored.  If the string
         !  canot be interpreted as a valid number, it will just be interpreted as a
         !  missing data value.  We will need to be cautious if the files use a large 
         !  negative value to denote missing data. If it is not *REALLY* large, some of the
         !  GLSHFS programs might intepret it as a valid value.
         !
         !  The GLSHFS-defined missing data string will be assigned to the PARAMETER value
         !  FixedMissingValueString specification in GLSHFS_Global.
         !
         CHARACTER(LEN=50)  :: UserMissingValueString
         
      END TYPE GLSHFS_ConfigData
      
      !
      !  Define various global arrays that are related to lake basin naming.
      !  These will be used throughout GLSHFS to maintain consistent names.
      !
      !  Arrays with names of the lake basins the way they are treated by the
      !  Coordinated Great Lakes Regulation and Routing Model that gets used for 
      !  computation of levels/flows.  These arrays are prefixed by "Lev".
      !
      !  Note that CGLRRM uses 'mhu' where I am using 'mhg'.
      !
      CHARACTER (LEN=2), PARAMETER :: LevLkCode2(5) =               &
                         (/'sp', 'mh', 'sc', 'er', 'on'/)

      CHARACTER (LEN=3), PARAMETER :: LevLkCode3(5) =               &
                         (/'sup', 'mhu', 'stc', 'eri', 'ont'/)

      CHARACTER(LEN=10), PARAMETER :: LevLakeName10(5) =            &
            (/'Superior  ', 'Mich-Huron', 'St. Clair ',             &
              'Erie      ', 'Ontario   '/)

      !
      !  Now declare the other variables that will be global
      !
      TYPE (GLSHFS_ConfigData) :: GLSHFS_Config        ! config info for the run
      
      !
      !  This variable defines the string that will be used in output files to
      !  denote a missing data value. I am currently setting it to the value that
      !  Excel uses by default. My understanding is that the standard read procedures
      !  in R can use whatever you specify, and I am assuming other software packages
      !  will be able to do something similar.    
      !  The goal is to define it once (here) and then use it in all of the file
      !  I/O procedures. That way, if someone needs to build a version of this
      !  software package that uses something else (due to what they are doing
      !  with post-processing of files, or any other reason), it can be easily
      !  changed in ALL files, just by changing it here and recompiling.
      !
      !  (Tim Hunter - 14 Oct 2016)
      !
      CHARACTER(LEN=4), PARAMETER :: FixedMissingValueString = 'NA'
   
      !
      !  We sometimes want a numeric "subbasin" number to use when designating
      !  the overland, overlake, or overbasin areas. Overlake has always been
      !  defined as subbasin 0, by convention. But there is no established
      !  convention for overland or overbasin. I have sometimes used 98 and 99
      !  for that, and it works ok. So I am defining those 2 here.
      !
      INTEGER, PARAMETER :: SubNum_OverLand  = 98
      INTEGER, PARAMETER :: SubNum_OverBasin = 99
      
      !
      !  When adding new station data to the persistent data files
      !  (stndata_*.csv and subdata_*.csv) I want some means for tracking the
      !  earliest new data that was added. That way, if those files contain 
      !  data for 50 years, but I only added station data for the most recent
      !  10 days, I don't have to recompute subbasin meteorology for the entire
      !  50 years.  This variable will be set by the procedure that adds the
      !  new station data and then read by the process that computes subbasin
      !  meteorology. 
      !  This INTEGER value is a date sequence number.
      !
      INTEGER :: EarliestNewStationData

      !
      !  Time step constants used whenever we need to specify a timestep interval.
      !  The actual value assigned to each constant is irrelevant as long as they 
      !  are unique within the "set".  I chose this range for no good reason.
      !  Purely arbitrary.
      !
      !  One question was, "Should I allow multiple equivalent definitions?"
      !    e.g.   GTS_Annual = 1
      !           GTS_Yearly = 1
      !  I thought about it, but decided no.
      !
      INTEGER, PARAMETER :: GTS_Annual     =  4201
      INTEGER, PARAMETER :: GTS_Monthly    =  4202
      INTEGER, PARAMETER :: GTS_QtrMonthly =  4203
      INTEGER, PARAMETER :: GTS_Weekly     =  4204
      INTEGER, PARAMETER :: GTS_Daily      =  4205

   
CONTAINS
     
!------------------------------------------------------------------
!    Given a 3-character lake name, retrieve the lake number for that 
!    lake basin as corresponding to the CGLRRM basin naming arrays.
!------------------------------------------------------------------
      INTEGER FUNCTION LevLakeNumberFromName3(Bsn)
      IMPLICIT  NONE
      CHARACTER (LEN=*), INTENT(IN) :: Bsn

      INTEGER :: I
      CHARACTER (LEN=3)  :: S3

      LevLakeNumberFromName3 = -1       ! default bad value

      S3 = 'xxx'
      IF (LEN(Bsn) .GE. 3) S3 = ADJUSTL(Bsn(1:3))
      IF (S3 .EQ. 'xxx') RETURN

      CALL LowerCase(S3)
      DO I = 1, GlShfsNumBasins
         IF (S3 .EQ. LevLkCode3(I)) LevLakeNumberFromName3 = I
      END DO
      RETURN
     
      END FUNCTION LevLakeNumberFromName3

!------------------------------------------------------------------
!     A clean-ish way to let a batch file know that something went wrong in
!     one of the Fortran programs is to create a flag file.  The batch file
!     can simply test for the existence of the flag file at any point.
!     if it exists, something went wrong.
!
!     This routine just creates a simple text file named 'error.int'
!------------------------------------------------------------------
      SUBROUTINE MakeErrorInt
      IMPLICIT  NONE
      INTEGER :: U
      U = 2222
      OPEN(UNIT=U, FILE='error.int', STATUS='REPLACE')
      WRITE(U, *) 'This file was created by GlShfs to flag an error.'
      CLOSE(U)
      END SUBROUTINE MakeErrorInt


END MODULE GLSHFS_Global
