#!/usr/bin/Rscript
# portmanteau of aggregate + update
# aggregates GLSHFS input/output to major basins for: lake, land and "basin" (both)
# outputs
#	 -tabular monthly files: (YEAR x 12)
#		-land precip
#		-lake precip
#		-basin precip
# 		-evaporation (LLTM output)
#	-daily met forcing: (date x 7 variables)
#		-land
#		-lake
#		-basin 

capture.output(source('/mnt/projects/ipemf/glshfs/GLSHFS/util/glshfs_utils.R'), file='/dev/null')

out_dir <- '/mnt/projects/ipemf/glshfs/web_files/'
unit_line <- c('YYYY-MM-DD','Celsius','Celsius','Millimeter','Celsius','Celsius','Meters/Sec','Percent')

# because we have to read the overLAKE data (which doesn't exist for the LBRM "summary" files),
# we can't use read_lbrm from `glshfs_utils.R`.  instead i use read_subdata define below
# I think this is actually better/more efficient than the read_lbrm since it uses a 3-D array and returns 
#  - all dts (dimension 1)
#  - all vars (dimension 2)
#  - all sub-basins (dimension 3)
# TODO: revisit glshfs_utils and see if you can implement this same method and return 3D arrays

read_subdata <- function(lk, i){ # read a single subdata file (used by read_all_subs)
		fin <- sprintf('%s/%s/subdata_%s%02i.csv', base_dir, lk, lk, i)
		if(!file.exists(fin)) stop('file missing for ', subname)
		#print(fin)
		nskip<-9 # lines to skip
		dat <- read.table(fin, sep=',', skip=nskip, strip.white=T, colClasses=c('Date', rep('numeric',7)))
        hdr <- scan(fin, what='character', skip=nskip-2, nlines=1, sep=',', strip=T, quiet=T)
		names(dat) <- hdr
		return(dat)
}


read_all_subs <- function(lk){ 
	cat(sprintf('\r%s...', lk))
	dat <- read_subdata(lk, 0)
	dts <- dat$Date
	nsub <- nsubs[[lk]]
	var_dim <- 7
	dts_dim <- length(dts)
	subnames <- sprintf('bas%02i', 0:nsub)
	out <- array(NA, dim=c(dts_dim, var_dim, nsub+1), dimnames=list(dts=NULL, var=vars_met, sub=subnames))
	out[,,1] <- as.matrix(dat[,-1]) # omit dates for storign in array
	for (i in 1:nsubs[[lk]]){
	   	out[,,i+1] <- as.matrix(read_subdata(lk, i)[,-1])
	}
	return(out)
}


write_month_tab <- function(daily_dat, varname, fout){
	yrs <- as.numeric(unique(format(dts,'%Y')))
	mon_vec <- aggregate(daily_dat[,varname], by=list(Date=format(dts, '%Y-%m')), sum)[,2]
	out <- t(matrix(NA, length(yrs), 12, dimnames=list(yrs, month.abb)))
	out[1:length(mon_vec)] <- mon_vec
	out <- t(out)
	write.table(format(out, digits=3, nsmall=3, quote=F), quote=F,file=fout, sep=', ')
}

agg_LLB <- function(lk_dat, lk, A){ # aggregate lake land basin from read_all_subs output and write out
	cat(sprintf('\raggregating and writing out basn, land and lake files for %s...', lk))
	# calculate weighted averages for BASin, LaND, and LAKe
	bas <- apply(lk_dat * A$area_km2, c(1,2), sum, na.rm=F)/sum(A$area_km2)
	lnd <- apply(lk_dat[,,'bas00'] * A[1,'area_km2'], c(1,2), sum, na.rm=F)/sum(A[1,'area_km2'])
	lak <- apply(lk_dat[,,-1] * A[-1,'area_km2'], c(1,2), sum, na.rm=F)/sum(A[-1,'area_km2'])

	# merge with univeral dates and round for writing out
	bas <- cbind(Date=dts, as.data.frame(round(bas, digits=2)))
	lnd <- cbind(Date=dts, as.data.frame(round(lnd, digits=2)))
	lak <- cbind(Date=dts, as.data.frame(round(lak, digits=2)))

	# write daily values out for all met vars
	write.table(format(bas,digits=2, nsmall=2), file=sprintf('%s/daily/subdata_%s_basn.csv', out_dir, lk), sep=',\t', row.names=F, quote=F)
	write.table(format(lnd,digits=2, nsmall=2), file=sprintf('%s/daily/subdata_%s_land.csv', out_dir, lk), sep=',\t', row.names=F, quote=F)
	write.table(format(lak,digits=2, nsmall=2), file=sprintf('%s/daily/subdata_%s_lake.csv', out_dir, lk), sep=',\t', row.names=F, quote=F)


	write_month_tab(bas, 'Precip', sprintf('%s/prc_%s_basn.csv', out_dir, lk))
	write_month_tab(lnd, 'Precip', sprintf('%s/prc_%s_land.csv', out_dir, lk))
	write_month_tab(lak, 'Precip', sprintf('%s/prc_%s_lake.csv', out_dir, lk))
}

# read all the data for all the sub-basins
# returns arrays of dim = [dts, vars, sub-basins]
# these can be indexed like sup[,,'bas05'] or mic[,'AirTempMax',4], etc.
dts <<- read_subdata('sup',0)$Date # universal date vector

#cat(sprintf('reading in data for '))
sup <- read_all_subs('sup')
mic <- read_all_subs('mic')
hur <- read_all_subs('hur')
geo <- read_all_subs('geo')
stc <- read_all_subs('stc')
eri <- read_all_subs('eri')
ont <- read_all_subs('ont')
cat(sprintf('\n done'))

# do the aggregation and write out
agg_LLB(sup, 'sup', A_sup)
agg_LLB(mic, 'mic', A_mic)
agg_LLB(hur, 'hur', A_hur)
agg_LLB(geo, 'geo', A_geo)
agg_LLB(stc, 'stc', A_stc)
agg_LLB(eri, 'eri', A_eri)
agg_LLB(ont, 'ont', A_ont)
cat(sprintf('\n'))


# now process the evaporation and write out to monthly files too
e_sup <- read_lltm('sup', varname='Evap')
e_mic <- read_lltm('mic', varname='Evap')
e_hur <- read_lltm('hur', varname='Evap')
e_geo <- read_lltm('geo', varname='Evap')
e_stc <- read_lltm('stc', varname='Evap')
e_eri <- read_lltm('eri', varname='Evap')
e_ont <- read_lltm('ont', varname='Evap')

write_month_tab(e_sup, 'Evap', sprintf('%s/evaporation_%s.csv', out_dir, 'sup'))
write_month_tab(e_mic, 'Evap', sprintf('%s/evaporation_%s.csv', out_dir, 'mic'))
write_month_tab(e_hur, 'Evap', sprintf('%s/evaporation_%s.csv', out_dir, 'hur'))
write_month_tab(e_geo, 'Evap', sprintf('%s/evaporation_%s.csv', out_dir, 'geo'))
write_month_tab(e_stc, 'Evap', sprintf('%s/evaporation_%s.csv', out_dir, 'stc'))
write_month_tab(e_eri, 'Evap', sprintf('%s/evaporation_%s.csv', out_dir, 'eri'))
write_month_tab(e_ont, 'Evap', sprintf('%s/evaporation_%s.csv', out_dir, 'ont'))

