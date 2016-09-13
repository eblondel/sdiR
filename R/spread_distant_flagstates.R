#
# Scripts to experiment SPREAD-like algorithm for catch statistics of
# distant countries, based on % of catches by coastal countries, by 
# FAO major area / year
# 
# @author eblondel
# @date 13/09/2016
#

#working directory & environment
#-------------------------------------------------------------------------
setwd("D:/Mes documents/Documents/DEV/R/sdi/20160913_SPREAD")
options(stringsAsFactors = FALSE)

#package requirements
#-------------------------------------------------------------------------

#inputs
#-------------------------------------------------------------------------
#read catch statistics (normalized form)
stats <- read.table("CaptureView.csv", h=T,sep=",")

#read intersects
intersects <- read.table("EEZ_FA_LMErcl.txt", h=T,sep="\t")

#business functions
#-------------------------------------------------------------------------

#build a intersecting table with probabilities by coastal flagstates
#computation is done taking into account the following dimensions:
#- FIC_SYS_CATCH_AREA (FAO major area)
#- YR_ITEM (year)

computeWprobByDims <- function(intersects, stats, area, year){
	
	#extract stats for coastal flagstates only
	area.intersects <- intersects[intersects$F_AREA == area,]
	flagstates <- unique(area.intersects[,"ISO_3digit"])
	out.stats <- stats[stats$FIC_SYS_CATCH_AREA == area
						& stats$YR_ITEM == year
						& stats$ISO_3_CODE %in% flagstates,]
	
	#compute wprob
	out.stats$wprob <- out.stats$SumOfQUANTITY / sum(out.stats$SumOfQUANTITY)
	out.stats <- out.stats[,c("FIC_SYS_CATCH_AREA", "ISO_3_CODE", "YR_ITEM", "wprob")]
	
	#merge intersects with wprob
	out.wprobs <- merge(x = intersects, y = out.stats,
						by.x = c("F_AREA", "ISO_3digit"), by.y = c("FIC_SYS_CATCH_AREA", "ISO_3_CODE"), all.y = TRUE)
	out.wprobs <- out.wprobs[order(as.factor(out.wprobs$EcoTypeNew)),]
	out.wprobs <- out.wprobs[!duplicated(out.wprobs[,c("F_AREA", "ISO_3digit", "wprob")], fromLast = TRUE),]
	return(out.wprobs)
	
}

computeWprob <- function(intersects, stats){	
	dims <- unique(stats[,c("FIC_SYS_CATCH_AREA","YR_ITEM")])
	wprobs <- do.call("rbind", lapply(1:nrow(dims),
						function(i){
							out <- computeWprobByDims(intersects, stats, area = dims[i,"FIC_SYS_CATCH_AREA"], dims[i,"YR_ITEM"])
							return(out)
						}))
	return(wprobs)
}

#execution & outputs
#-------------------------------------------------------------------------

#step 1: compute table of probabilities based on coastal flagstate catch %
#compute probabilities
wprobs <- computeWprob(intersects, stats)

#validation of probabilities
aggregate(wprobs$wprob, by = as.list(wprobs[c("F_AREA","YR_ITEM")]), FUN = "sum")


