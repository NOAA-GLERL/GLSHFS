** UPDATE 2025 **:
Ideally you won't need this area b/c the run directory from github has all the needed subdirs and param files that are here.  
(You would only need this area if you wanted to start a clean slate with new data.)


This folder is set up for creating new subbasin meteorology files using GLSHFS.


The basic strategy is to set this up like a new GLSHFS install, with a folder for each lake and a folder for the station data.  The lake folders will have the bare minimum files needed (basininfo.txt and xxxbytcd.map). 

We then run GLSHFS using a config file that says to only build the subbasin meteorology, no model runs.  Once that completes, we are done, and will have the full set of subbasin met files.

