#!/usr/bin/Rscript
# based on Tims code lump_flw.f90 derive a flow/area from gaged basins and then apply it over the entire major basin area

# mathematically flow_per_area = sum of gauged flow DIVIDED by sum of gauged area 
# total flow is then flow_per_area * total basin area
# flow and gauge information is aggregated (mean) to monthly values from daily

# source utils which provides functions and variables (like areas)
source('/mnt/projects/ipemf/glshfs/GLSHFS/util/glshfs_utils.R')
#capture.output(source('/mnt/projects/ipemf/glshfs/GLSHFS/util/glshfs_utils.R'), file='/dev/null') # optionally hide output from source utils
# ARM output is expected to live in `arm_dir' as defined in glshfs_utils.R  or you can manually define it below
arm_dir <- '/mnt/projects/ipemf/glshfs/validate/ARM_out/'

# TODO: skip SUP19

process_arm <- function(lk, subbasin_areas, plt=T){ 
	#cat(sprintf('\r%s...', lk))
	nsub <- nsubs[[lk]]

	n_gauges <- read_arm(lk, subs=1:nsub, varname='n_gages', perc_thresh=0) 
	flow <- read_arm(lk, subs=1:nsub, varname='flow', perc_thresh=0)
	dts <- flow$dts   # save dates for next steps
	flow <- flow[,-1] # omit dates for calculations
	n_gauges <- n_gauges[,-1] # omit dates for calculations
	is_gauged <- !(is.na(n_gauges)) & n_gauges > 0 # logical: subbasins that are gauged or not
	areas <- subbasin_areas[-1,2] # omit the first column (bas num) and first row (lake area)

	flow_per_area <- matrix(NA, length(dts), 1)
	for (d in 1:length(dts)) flow_per_area[d] <- sum(flow[d,], na.rm=T) / sum(areas[!is.na(n_gauges[d,])])

	total_flow <- flow_per_area*sum(areas)
   	total_gauges <- apply(n_gauges[,-1], 1, sum, na.rm=T)
	gauged_area <- apply(is_gauged, 1, function(x) sum(x*areas, na.rm=T))/sum(areas)
	daily <- cbind(total_flow, gauged_area, total_gauges)
	monthly <- aggregate(daily, by=list(format(dts, '%Y-%m')), mean)
	names(monthly) <- c('YYYY-MM', 'flow_cms', 'fraction_gauged', 'total_gauges')
	yearmon <- as.Date(sprintf('%s-01',c(monthly[,1])))

	write.table(format(monthly, digits=2), quote=F, file=sprintf('%s/runoff_%s_arm.csv', out_dir, lk), row.names=F, sep=',\t')

	if(plt){
		png(sprintf('%s/%s_arm.png', out_dir, lk), w=1200, h=800)
		plot(x=yearmon, y=monthly$fraction_gauged, col='lightgrey', 'h', yaxt='n', xlab=NA, ylab=NA, main=lk)
		axis(4, col='lightgrey', col.axis='lightgrey')
		par(new=T)
		plot(x=yearmon, y=monthly$flow_cms, 'l', col='blue', yaxt='n',, xlab=NA, ylab=NA)
		axis(2, col='blue', col.axis='blue')
		mtext(side=2, 'flow (cms)', col='blue', line=3)
		mtext(side=4, 'fraction gauged', col='lightgrey')
		dev.off()
	}

}

# process each basin (leave plotting on)
process_arm('sup', A_sup)
process_arm('mic', A_mic)
process_arm('hur', A_hur)
process_arm('geo', A_geo)
process_arm('stc', A_stc)
process_arm('eri', A_eri)
process_arm('ont', A_ont)


#process_arm('sup', A_sup, plt=F) # don't plot
