#!/usr/bin/Rscript
capture.output(source('/mnt/projects/ipemf/glshfs/GLSHFS/util/glshfs_utils.R'), file='/dev/null')

fout <- '../2014_onward_land_aggregated.txt'

combine_subs <- function(lk, varlist, area_df){
	for (v in varlist){
		subvar <- read_lbrm(lk, subs=1:nsubs[[lk]], varname=v)
		if(!exists('out')) out=data.frame(dts=subvar$dts)
		#  area-weighted mean:     scale each column by area (Hadamard product); sum; divide by total area
		out[v] <- apply(subvar[,-1]*area_df[-1, 'area_km2'], 1, sum, na.rm=T)/sum(area_df[-1,'area_km2'])
	}
	return(out)
}


#sup_met <- combine_subs('sup', vars_met, A_sup)
#mic_met <- combine_subs('mic', vars_met, A_mic)
#hur_met <- combine_subs('hur', vars_met, A_hur)
#geo_met <- combine_subs('geo', vars_met, A_geo)
#stc_met <- combine_subs('stc', vars_met, A_stc)
#eri_met <- combine_subs('eri', vars_met, A_eri)
#ont_met <- combine_subs('ont', vars_met, A_ont)
#ont_met <- ont_met[-nrow(ont_met),] # for some reason ontario has 1 extra date row?  ¯\_(ツ)_/¯

total_land <- sum(shp[shp$subbasin>0,'area_km2',drop=T])


# sum each var scaled by the total area of a given lake and divide by sum of all land area
# notation below:
# xxx_met[,-1]          ====> selects all the non-date columns
# A_xxx[-1, 'area_km2'] ====> is the sum of all the land in a given basin (omits subbasin==0 which is lake)

gl_met <- (
	sup_met[,-1]*sum(A_sup[-1,'area_km2']) + 
	mic_met[,-1]*sum(A_mic[-1,'area_km2']) + 
	hur_met[,-1]*sum(A_hur[-1,'area_km2']) + 
	geo_met[,-1]*sum(A_geo[-1,'area_km2']) + 
	stc_met[,-1]*sum(A_stc[-1,'area_km2']) + 
	eri_met[,-1]*sum(A_eri[-1,'area_km2']) + 
	ont_met[,-1]*sum(A_ont[-1,'area_km2'])
	) / total_land

gl_met <- cbind(dts=sup_met$dts, round(gl_met, digits=2))

write.table(gl_met, file=fout, sep='\t', row.names=F, quote=F)

