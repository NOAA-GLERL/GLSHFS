@echo off
REM --------------------------------------------------
REM  Batch file for USACE to use for updating the met data for GLSHyFS
REM 
REM  Things to edit are...
REM  1) MD = the station met directory for GLSHyFS. This is the "stn"
REM     directory that is a subdirectory of the main GLSHyFS install.
REM  2) FL = name of the file that has all of the stations to be processed.
REM     As long as it is in htis directory, you do not need path info.
REM  3) SY = start year for downloading new station data
REM  4) EY = end year for downloading new station data
REM
REM --------------------------------------------------

set MD=g:\glshfs\test2\stn
set FL=FTP_List.txt
set SY=2016
set EY=2017

@echo Running process to create station files...
python process_all_ish.py %FL% %SY% %EY%
IF ERRORLEVEL 1 GOTO ERR

@echo Copying station files to the station file directory...
copy /y  met_*.csv %MD%
IF ERRORLEVEL 1 GOTO ERR

@echo Cleaning up this directory...
del /q met_*.csv
GOTO DONE

:ERR
@echo  Error forced the process to abort.
@echo  Station data file set was not updated properly.

:DONE