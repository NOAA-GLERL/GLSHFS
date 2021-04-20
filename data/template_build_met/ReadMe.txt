This folder is set up for creating new subbasin meteorology files using GLSHFS.

The basic strategy is to set this up like a new GLSHFS install, with a folder for each lake and a folder for the station data.  The lake folders will have the bare minimum files needed (basininfo.txt and xxxbytcd.map). 

We then run GLSHFS using a config file that says to only build the subbasin meteorology, no model runs.  Once that completes, we are done, and will have the full set of subbasin met files.

