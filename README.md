## Great Lakes Seasonal Hydrology Forecasting System

The Great Lakes Seasonal Hydrology Forecasting System (GLSHFS or GLSHyFS) is a modeling framework that uses daily
meteorology input data along with hydrologic models to produce seasonal (multi-month) forecasts of water supply and
other hydrological variables for the Great Lakes.


 ---
## Installing GLSHFS


Installation is a simple process of creating and filling the seven lake basin directories, station data directory (if desired),
and the glshfs program executable (source code optional). Currently, there are just five files required for each lake basin
(six if you want the option to use both versions of LBRM). The content of these files will be described later in this
manual, but the quick list is:
basininfo.txt
xxxbytcd.map
lbrm_boundaryconditions_xxx.txt
lltm_parameters_glshfs_xxx.csv
lbrm_parameters_1982_xxx.txt (if you will ever run the LBRM using the old 1982 ET method)
lbrm_parameters_2016_xxx.txt (if you will ever run the LBRM using the new 2016 ET method)
where xxx=sup, mic, hur, geo, stc, eri, ont.
Numerous additional files will be created in each lake basin directory as GLSHyFS is run, but for initial setup you only
need those five (or six).

---
## Running GLSHyFS
The process for running GLSHyFS consists of building a valid configuration file, supplying met station data (as needed),
and then running the GLSHyFS executable. There are four main operations in GLSHyFS, and a single run may include any
or all of them. These operations are:

1. Adding new met station data to the stndata\_\*.csv files.
2. Building subbasin and overlake averages from new station data for the historical period defined by all station
data that has ever been added via step one.
3. Running the LBRM and LLTM for the historical period up to the end of the data created in step two.
4. Running forecasts for a user-defined time period starting on any day during the historical period or on the day
immediately after the end of the historical period.

Input files
Configuration file:
The main configuration file can be named anything, and can be located anywhere. It does not have to be located
within the GLSHFS directory structure. This file contains information on file locations, forecast settings, etc. The
format of each entry is keyword=value, and comment lines are denoted by a # character. The keywords (left side of
equal sign) are not case-sensitive. The values may be, though. Directory specifications are case-sensitive in a linux
environment, but not in a Windows environment. White space on either side of the = character is allowed, but not
required. Specific entries are:

**AddStationData:**
User-specified flag to process the station data in StnDir. When this flag is set to Yes/True, the met_*.csv
files will be read and checked for obvious errors. Then, if the station is within the specified distance of
each lake basin (see BasinBuffer entry) the data will be added to the stndata_*.csv files for that lake.
Valid values are T, F, True, False, Yes, No.

**BuildSubbasinMet:** User-specified flag to process the station data in stndata_\*.csv, adding that data to the subbasin data
sets for all lakes. Valid values are T, F, True, False, Yes, No (not case-sensitive).

**UpdateHistorical:**
Logical flag indicating if LBRM and LLTM should be run on the updated subbasin/lakewide data sets.
Valid values are T, F, True, False, Yes, No (not case-sensitive).

**RunForecasts:**
Logical flag indicating if forecasts should be run. If True/Yes, then the forecasts settings must be
present, and will be used to control that forecast. Valid values are T, F, True, False, Yes, No (not casesensitive).

**MakeSummaryFiles:**
Logical flag indicating if forecast summary files should be created. Valid values are T, F, True, False, Yes,
No (not case-sensitive).

**ClearTempFiles:**
Logical flag indicating if forecast temporary files should be deleted to clean up directory after building
summary files. If MakeSummaryFiles is set to No/False, then don’t set this to Yes/True, or you will be
left without a useful forecast.

**BaseDir:**
The GLSHFS base directory for everything. This directory contains all of the lake subdirectories, program
subdirectory, etc. This must be a full directory path specification; not a relative path. For example:
/mnt/users/hunter/glshfs
(linux)
C:\work\glshfs
(Windows)
StnDir:
The GLSHFS directory that contains any individual station data files (met_*.csv) to be added to the
various GLSHFS datasets. This entry is only required if AddStationData = TRUE.

**BasinBuffer:**
This entry specifies a distance in kilometers (floating point number or integer). When adding station data
to the aggregated station data for each lake, stations that fall within this distance from the basin
boundary will be used. If the station is outside this buffer distance, it will NOT be added to that lake’s
station data files. Please note that changing this setting from one run to the next will ONLY affect what is
done during the current run. It will not re-sort previously added stations, nor will it have any effect on
subbasin values that were already computed. For example, if a prior run of GLSHFS was made with this
set to 100 km, then all stations within 100 km (or less) of the basin boundary would have been added to
the lake’s stndata_*.csv files and that data will be used when computing the subbasin average values.
Changing this entry to 30 for the current run will not remove the stations that are 31-100 km away from
the basin. It will only limit the new additions. Those stations that were previously added will still be
actively used for calculating the subbasin average data values, for the entire period.

**LbrmMethodET:**
Which method should the LBRM use for calculating ET? To use the historical method (a.k.a. the
temperature-adjustment method) that has been used since approximately 1982, set this value to 1982.
To use the new method developed in 2016 (a.k.a the ??? method), set this value to 2016. You must also
ensure that each lake has the predefined LBRM parameter sets in file lbrm_parameters_????_xxx.txt;
where ???? = 1982 or 2016, and xxx=sup, mic, hur, etc. These files are similar, but the 2016 version also
includes long-term air daily temperature averages (i.e. long-term value for each day of the year).
**LltmMethodRadiationHist:**
Which method of calculating the incident shortwave and net longwave radiation should be used for the
historical period (i.e. the period for which we have station data)? Valid options are 1, 2, or 3.
Option 1 = use cloud cover as a proxy to compute the radiation values. This is the method that was
originally encoded into the LLTM starting in 1987. In 2017, it is still the most appropriate choice for
most cases, since we do not have a robust set of radiation data at the station locations. Cloud cover *is*
reported by a fair number of stations.
Option 2 = Use Incident and Longwave radiation directly. The Longwave radiation value read from the
subbasin file is interpreted as INCOMING longwave. NET longwave will be computed by LLTM. This is the
option that we need to choose when using the output from CMIP models. The NetLW radiation that
comes out of those models apparently does not properly account for the lake, and the net value that is
calculated and output is significantly wrong. Seasonality is reversed, etc. But Lisi Pei was able to provide
this downward/incoming value and it is considered useful.
Option 3 = Use Incident and Longwave directly. Treat the longwave radiation as a correct computation
of the net value and use both radiation values as direct inputs. This will probably be used only when we
are using output from some sort of coupled atmosphere-water model that correctly computes the net
LW value, and is provided as a “future-proofing” option.

**LltmMethodRadiationFcst:**
Which method of calculating the incident shortwave and net longwave radiation should be used for the
forecast period? Valid options are the same as for LltmMethodHistorical (1, 2, or 3).

**LltmApplyDataCorrectionsHist:**
Do we apply over-water corrections (as defined in the LLTM code) to the airtemp, dewpoint and
windspeed values during the historical period? These corrections are intended to give a better estimate
of overwater conditions when our estimates for airtemp, dewpoint and windspeed are from land-based
stations. Cloud cover is never adjusted. Valid values are T, F, True, False, Yes, No.

**LltmApplyDataCorrectionsFcst:**
Do we apply over-water corrections (as defined in the LLTM code) to the airtemp, dewpoint and
windspeed values during the forecast period? These corrections are intended to give a better estimate
of overwater conditions when our estimates for airtemp, dewpoint and windspeed are from land-based
stations. Cloud cover is never adjusted. Valid values are T, F, True, False, Yes, No.

**ForecastName:**
Specify a name for the forecast. This entry is needed only if RunForecasts is Yes/True. The value
specified here will be used as the name of a new directory in the BaseDir directory. All of the forecast
files will be created in that new directory.

**ForecastStart:**
The desired forecast start date in YYYY-MM-DD format or the string “EndOfHistData” (no quotation
characters and case-insensitive). If this is set to EndOfHistData, then the forecast start date will be
dynamically calculated as the day after the end of historical data.

**ForecastLength:**
The desired forecast length, specified in full months. Note that if the forecast start date is anything
beyond the first day of the month, then GLSHFS rounds up and will do a complete next month. For
example, if the forecast start date is specified as 2017-07-01 (July 1, 2017) and the forecast length is
specified as 4 months, then the forecast period will be 2017-07-01 through 2017-10-31. If, however, the
forecast start date is 2017-07-02 and the length is 4, then the 4 full months takes us to November 1.
GLSHFS will round this up, and the forecast period will be 2017-07-02 through 2017-11-30.

**ForecastMetSource:**

From where do we get the forecast meteorology sequences? Valid options are ExtractFromHist and
UserSupplied. If the value is extractfromhist, then meteorology sequences will be extracted from the
historical subbasin data files, starting in each possible year. The sequences will start on the forecast start
day (e.g. July 2) and extend as far as needed to fulfill the forecast length required. If the value is
usersupplied, then the user needs to supply all of the meteorology sequences (a full set of files for all
subbasins, for each sequence named in scenarionames).

**ScenarioNames:**
Either the string “All” or a comma-separated list of scenario (ensemble member) names. If
ForecastMetSource = “extractfromhist” and this field is All, then all possible years will be extracted from
the historical period. If ForecastMetSource = “extractfromhist” and this is a comma-separated list, then
this should be a list of years (1950,1951,1952,etc) specifying the start year of each scenario.
If ForecastMetSource = “usersupplied”, then these strings are used to construct the files names for the
files used in each ensemble member, and All is not a valid option.

**MissingValueString:**
This is the string that will be written to output files when the data value is missing. This allows the user
to tailor output files for use with their specific data analysis tools. For example, Excel users may wish to
use #NA. R users may wish to use NA. Other users may wish to use a large negative number, e.g. -9.9e9.
When reading files as input, GLSHFS interprets any text string that cannot be converted to a numeric
value as a missing value. E.g. #NA, NA, Missing, NUL, and other strings would all be interpreted the same
way – as missing. If you use a numeric string you will need to be careful about range.

**Basininfo.txt:**
This file contains summary information about each lake basin.


---


Directory Organization:
GLSHFS generally assumes that all files are located within a single directory tree. The main (base) directory can be
located anywhere, though I caution against putting it too deep in the filesystem directory hierarchy. If the file path name
ends up being too long (>100 characters or so) then it could cause problems due to file names exceeding the space
allocated for them throughout the GLSHFS Fortran code.
The main directory contains one subdirectory for each lake basin, and they are named with the standard 3-letter lake
basin names (sup, mic, hur, geo, stc, eri, ont). In all of the Fortran source code, GLSHFS uses and assumes lowercase
names for all directories. This is irrelevant for use on Windows computers, because the Windows file-system is caseinsensitive. Linux, however, is case sensitive on both directory and file names.
The location for placement of any station data files that the user will be adding is flexible. While I do recommend that it
be a subdirectory of the base directory, it is not required. I chose to allow flexibility here in order to accommodate
procedures that create the station data files in some working folder outside the GLSHyFS directory. Rather than require
all of those files to be copied over to the GLSHyFS hierarchy, they can be manipulated in place. However, please note
that the station files are overwritten during the error-checking phase of adding new station data. Thus, I do recommend
that you make a working copy of them as a subdirectory of the GLSHyFS base directory. This will prevent changes to your
original station data files. If you have no objection to having the original files being overwritten, then you can just access
them where they are. The location is specified in the configuration file.
The GLSHFS software is compiled to a single executable with no extra libraries (beyond standard compiler-required and
installed run-time libraries), so the executable can be placed anywhere on the user’s filesystem.

File Names:
When working with files, it is important to always remember that Windows file names are not case-sensitive, but other
filesystem’s (e.g. linux) file-naming is. When I wrote the Fortran code of GLSHyFS, I chose to always assumes that file
names are entirely lowercase. This is consistent with how I usually see things being done in the linux “world”. If you are
working on a Windows PC, then you are not bound by that requirement, and you can freely mix the case of file names as
much as you want. Be aware, though, that all of the output files created by GLSHyFS will be fully lowercase. And within
the code, file names are always converted to full lowercase for existence tests, etc. Thus, it is possible that you may
name a file with mixed case (in Windows), and then find that GLSHyFS renames it to full lowercase. This will be true for
station data files during the error-checking process because GLSHyFS creates a new version of each file as it checks for
range errors, etc. It may occur elsewhere. It should be of no real consequence, however. However, when working in a
linux environment, it is an absolute requirement that you always use lowercase file names. When working in a casesensitive environment (like linux) GLSHyFS will fail to find files if you have used any uppercase letters in their name.
