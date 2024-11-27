library(sf) # spatial package needed for reading in shapefiles (to get sub-basin areas)
library(docstring)  # needed for help pages to work (i found that you MUST install "pkgload", for this to work

base_dir <- '/mnt/projects/ipemf/glshfs/GLSHFS/run/'
arm_dir <- '/mnt/projects/ipemf/glshfs/validate/ARM_out/'
shp_fn <- '/mnt/projects/ipemf/glshfs/gllbrm-boundaries/shp/lbrm_subbasin_outlines.shp'


vars_lltm <- c('Evap','WtrTemp','IceTemp','IceArea','IceDepth','ReflctdR','LatentR',
			   'SensbleR','AdvectnR','IncidntR','NetLngWR','StrdHeat','AirTemp','DewPt',
			   'WindSpd','CloudCvr','AdjATemp','AdjDewPt','AdjWSpd','AdjCld')

vars_lbrm <-  c('Runoff','UprSoilMst','LwrSoilMst','GroundWatr','SurfaceSto','SnowWaterE')
vars_met  <-  c('AirTempMin','AirTempMax','Precip','AirTemp','Dewpoint','Windspeed','CloudCover')


read_lltm <- function(lk, varname=NULL, dir=base_dir){
	#' Read LLTM output
	#' 
	#' Reads in GLSHFS/LLTM output for a given lake
	#' @param lk the string corresponding to a major basin (sup, mic, hur, geo, eri, stc, eri, ont)
	#' @param varname optional string which allows grabbing a specific variable (see `lltm_vars`). 
	#' 		default value (NULL) will grab all variables
	#' @param dir the main GLSHFS output directory (with sub-dirs for "sup", "mih", etc.) This is defined by default in glshfs_utils.R but you may wish to change it
	#' @return data.frame with dates as first column and following columns populated by the variables
	#' @examples
	#' sup <- read_lltm('sup')       # read in all vars Superior 
	#' eri <- read_lltm('eri', varname='Evap')   # read in Erie evaporation
	#' stc <- read_lltm('eri', dir='/some/other/place/')   # read in St Clair from non std location
	fin <- sprintf('%s/%s/lltm_output_%s.csv', base_dir, lk, lk)
	dat <- read.table(fin, sep=',', skip=6, strip.white=T)[,-22]
	hdr <- scan(fin, what='character', skip=4, nlines=1, sep=',', strip=T)
	hdr[1] <- 'dts'
	names(dat) <- hdr
	dat$dts <- as.Date(dat$dts)
	if(!is.null(varname)) dat <- dat[,c('dts',varname)]
	return(dat)
}


read_lbrm <- function(lk, subs, varname='Runoff', dir=base_dir){
	#' Read LBRM output
	#' 
	#' Reads in *summary files* (GLSHFS/LBRM input and output) for a given lake and sub-basin(s).  
	#' @param lk the string corresponding to a major basin (sup, mic, hur, geo, eri, stc, eri, ont)
	#' @param subs an integer or vector of which sub-basins to read in e.g. subs=4, subs=1:10, subs=1:nsubs$sup
	#' @param varname A string which corresponds to the output in the header of summary files, see vars_lbrm and vars_met
	#' @param dir the main GLSHFS output directory (with sub-dirs for "sup", "mih", etc.) This is defined by default in glshfs_utils.R but you may wish to change it
	#' @return data.frame with dates as first column and following columns populated by the selected sub-basins (named sub01, sub02, etc.)
	#' @examples
	#' sup_lbrm_subbas01 <- read_lbrm('sup', subs=1)       # read in superior sub-basin 01
	#' sup_lbrm_df <- read_lbrm('sup', subs=1:22)          # read in all 22 superior sub-basins 
	#' mih_lbrm_df <- read_lbrm('mih', subs=1:nsubs$mih)   # read in all mih; use the nsubs list object
	#' eri_lbrm_df  <- read_lbrm('eri', subs=1:nsubs$eri, dir='/some/diff/path/')   # read from non-std path


	i <- subs[1]
	nskip <- 9
	# preallocate dataframe
	#fin <- sprintf('%s/%s/lbrm_output_%s%02i.csv', dir, lk, lk, i)
	fin <- sprintf('%s/%s/summary_%s%02i.csv', dir, lk, lk, i)
	dat <- read.table(fin, sep=',', skip=nskip, strip.white=T, colClasses=c('Date', rep('numeric', 13)))
	lbrm_out <- data.frame(dts=dat[,1]) 
	for (i in subs){
		subname <- sprintf('bas%02i', i)
		cat(sprintf('processing GLSHFS %s: %s %s \r', varname, lk, subname))
		fin <- sprintf('%s/%s/summary_%s%02i.csv', dir, lk, lk, i)
		if(!file.exists(fin)) stop('file missing for ', subname)
		dat <- read.table(fin, sep=',', skip=nskip, strip.white=T, colClasses=c('Date', rep('numeric', 13)))
		hdr <- scan(fin, what='character', skip=nskip-2, nlines=1, sep=',', strip=T, quiet=T)
		names(dat) <- hdr
		dat <- dat[,c('Date',varname)]
		names(dat) <- c('dts',subname)
		lbrm_out <- merge(lbrm_out, dat, by='dts', all.x=T)
	}
	cat('\ndone\n')
	return(lbrm_out)
}

#read_arm <- function(lk, subs, t0=NULL, tf=NULL){
read_arm <- function(lk, subs, t0='1900-01-01', tf=Sys.Date(), perc_thresh=10, dir=arm_dir){
	#' Read LBRM output
	#' 
	#' Reads in GLSHFS/LBRM output for a given lake and sub-basin(s). A date range ought to be set by the user since
	#'  	not all ARM output files will have the same date range but a single data.frame is returned (the default is to use a very
	#' 		large time window (1900 thru present day) and will result in a lot of NAs)
	#' @param lk the string corresponding to a major basin (sup, mic, hur, geo, eri, stc, eri, ont)
	#' @param subs an integer or vector of which sub-basins to read in e.g. subs=4, subs=1:10, subs=1:nsubs$sup
	#' @param t0 either a Date object or a character string of the form 'YYYY-MM-DD' defining the START date of the resulting data.frame.  
	#' @param tf either a Date object or a character string of the form 'YYYY-MM-DD' defining the END date of the resulting data.frame
	#' @param perc_thresh minimum threshold to use for the percentage of a basin that is gaged (default 10%)
	#' @param dir is where the ARM files live (*.flw) This is defined by default in glshfs_utils.R but you may wish to change it
	#' @return data.frame with dates as first column and following columns populated by the selected sub-basins (named sub01, sub02, etc.)
	#' 		for sub-basins that don't have valid data within the time-window (or for which perc_thresh is not exceeded)
	#' @examples
	#' sup_arm_subbas01 <- read_arm('sup', subs=1)       # read in superior sub-basin 01
	#' sup_arm_df <- read_arm('sup', subs=1:22, t0='1950-01-01') # read in all 22 superior sub-basins for 1950 thru present
	#' mih_arm_df <- read_arm('mih', subs=1:nsubs$mih)   # read in all mih; use the nsubs list object
	#' eri_arm_df  <- read_arm('eri', subs=1:nsubs$eri, dir='/some/diff/path/')   # read from non-std path
	#' ont_arm_03  <- read_arm('ont', subs=1:nsubs$ont, perc_thresh=0)  # read in all ontario arm data regardless of gage percentage
	arm_out <- data.frame(dts=seq(as.Date(t0), as.Date(tf), by='days')) # preallocate dataframe
	for (i in subs){
		subname <- sprintf('bas%02i', i)
		cat(sprintf('processing ARM: %s %s \r', lk, subname))
		fin <- sprintf('%s/%s%02i.flw', arm_dir, lk, i)
		if(!file.exists(fin)) stop('file missing')

		# process file date information (sigh)
		date_stamp <- paste(scan(fin, skip=2, nlines=2, 'character', sep=NULL, quiet=T),collapse='')
		date_stamp <- gsub('\\D','', date_stamp)
		#print(date_stamp)
		file_t0 <- as.Date(substr(date_stamp,1,8), '%Y%m%d')
		file_tf <- as.Date(substr(date_stamp,9,16), '%Y%m%d')
		if(any(is.na(file_t0), is.na(file_tf))){ 
			warning(sprintf('file DNE for: %s', subname))
			arm_out[,subname] <- NA
			next
		}
		file_dts <- seq(from=file_t0, to=file_tf, by='day')
		dat <- read.table(fin, skip=6, head=F, row.names=NULL, col.names=c('flow','perc_gage','n_gages'))

		# check data within desired range
		sel_dts <- file_dts >= as.Date(t0) & file_dts <= as.Date(tf)
		if(!any(sel_dts)){ 
			warning(sprintf('no data within %s - %s for : %s\n', t0, tf, subname))
			arm_out[,subname] <- NA
			next
		}
		# check gaged percentage
		if(mean(dat[sel_dts, 'perc_gage']) < perc_thresh){ 
			warning(sprintf('threshold below %i for: %s\n', perc_thresh, subname))
			arm_out[,subname] <- NA
			next
		}

		dat <- data.frame(dts=file_dts, flow=dat$flow)
		names(dat) <- c('dts', subname) 

		arm_out <- merge(arm_out, dat, by='dts', all.x=T)
	#	print(head(arm_out))

	}
	return(arm_out)
}


shp <- st_read(shp_fn, quiet=T)
shp <- shp[order(shp$subbasin),]

# define all the sub-basins from shp files
A_sup <- shp[shp$basin=='SUP', c('subbasin','area_km2'), drop=T]
A_mic <- shp[shp$basin=='MIC', c('subbasin','area_km2'), drop=T]
A_hur <- shp[shp$basin=='HU', c('subbasin','area_km2'), drop=T]
A_geo <- shp[shp$basin=='GEO', c('subbasin','area_km2'), drop=T]
A_stc <- shp[shp$basin=='STC', c('subbasin','area_km2'), drop=T]
A_eri <- shp[shp$basin=='ER', c('subbasin','area_km2'), drop=T]
A_ont <- shp[shp$basin=='ON', c('subbasin','area_km2'), drop=T]

nsubs <- list(sup=nrow(A_sup), mic=nrow(A_mic), hur=nrow(A_hur), geo=nrow(A_geo), stc=nrow(A_stc), eri=nrow(A_eri), ont=nrow(A_ont))
nsubs <- lapply(nsubs, '-', 1)


convert_mm_to_cms <- function(lbrm_dat, lkname){
	#' convert runoff from mm to CMS
	#' 
	#' converts runoff in an LBRM data frame from millimeter over the basin (per day) to cubic meters per second
	#'  mm/(bas_area*day) ===>   bas_area * 1 day/86,400 secs * 1e6m2/1km2 * 1m/1e3mm
	#' @param lbrm_dat the data.frame (from `read_lbrm`) to be converted
	#' @param lkname a string corresponding to the major basin (defines area used) = {'sup','mic','hur','geo','stc','eri','ont'}
	#' @return a data.frame very much the same as the lbrm_dat but with streamflow values converted
	sub_areas <- get(sprintf('A_%s',lkname))
	out <- lbrm_dat
	out <- out[,-1]*sub_areas[-1,'area_km2']*1e6/86400/1e3
	return(out)
}


# define the areas for lakes (from GLLBRM_boundaries shps)
A_hur_lk <- A_hur[1,'area_km2']
A_geo_lk <- A_geo[1,'area_km2']
A_hurgeo_lk <- A_hur_lk+A_geo_lk  #km2
A_mic_lk <- A_mic[1,'area_km2']
A_mih_lk <- A_mic_lk+A_hurgeo_lk  #km2


bias <- function(obs, sim, na.rm){ 
	mean(obs-sim, na.rm=na.rm) 
}
nse <- function(obs, sim, na.rm){
    #1 -     SSE        / variance of obs * N
    1- sum((obs-sim)^2, na.rm=na.rm)/sum((obs-mean(obs, na.rm=na.rm))^2, na.rm=na.rm)
}



# print what just got loaded...
cat('====================== LOADED GLSHFS UTILITIES ==================================\n')

cat(sprintf('the following directories have been set (over-ride if desired):
\n\t base_dir = %s \t (GLSHFS output) \n\t arm_dir = %s \t   (ARM output) \n\t shp_fn = %s \t  (find on github: gllbrm-boundaries) \n\n\n',
		base_dir, arm_dir, shp_fn))


cat('\t fxns loaded: \n\t\t - read_lltm() \n\t\t - read_lbrm()
\t \t - read_arm() \n\t\t - convert_mm_to_cms()\n')

cat('\n\n\t varables loaded: \n\t\t - shp (sf class, subbasin outlines)
\t \t - nsubs (list, number of sub-basins) \n \t \t - A_sup, A_mic, etc. (subbasin areas dataframes)
\t \t - vars_lbrm (lbrm variable names) \n \t \t - vars_lltm (lltm varables) \n \t \t - vars_met (met varables)')

cat('\n\n =================================================================================\n\n\n\n')
