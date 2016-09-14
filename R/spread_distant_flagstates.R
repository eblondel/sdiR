#
# Scripts to experiment SPREAD-like algorithm for catch statistics of
# distant countries, based on % of catches by coastal countries, by 
# FAO major area / year
# 
# @author eblondel
# @date 13/09/2016
#
#==========================================================================

#working directory & environment
#==========================================================================
setwd("D:/Mes documents/Documents/DEV/R/sdi/20160913_SPREAD")
options(stringsAsFactors = FALSE)

#package requirements
#==========================================================================
if(!require(RFigisGeo)){
	require(devtools)
	install_github("openfigis/RFigisGeo")
	require(RFigisGeo)
}

if(!require(spread)){
	require(devtools)
	install_github("eblondel/spread")
	require(spread)
}

#inputs
#==========================================================================
#read catch statistics (normalized form)
stats <- read.table("CaptureView.csv", h=T,sep=",")

#read intersects
intersects <- read.table("EEZ_FA_LMErcl.txt", h=T,sep="\t")

#business functions
#==========================================================================

#build a intersecting table with probabilities by coastal flagstates
#computation is done taking into account the following dimensions:
#- FIC_SYS_CATCH_AREA (FAO major area)
#- YR_ITEM (year)

# @title computeWprobByDims
# @description
# unit function to compute by area/year the probability model which will give
# probability values by target area (represented by a combination of FAO area,
# LME (reclassified) and EEZ (reclassified as ISO3 code)
# @param intersects an object of class "data.frame" giving the intersects
# @param stats an object of class "data.frame" giving the catch statistics
# @param area an object of class "integer" or "character" (FAO major area)
# @param area an object of class "integer" or "character" (FAO major area)
# @return an object of class "data.frame" giving the probability model to use for reallocation
computeWprobByDims <- function(intersects, stats, area, year){
	
	#extract stats for coastal flagstates only
	area.intersects <- intersects[intersects$F_AREA == area,]
	flagstates <- unique(area.intersects[,"ISO_3digit"])
	out.stats <- stats[stats$FIC_SYS_CATCH_AREA == area
						& stats$YR_ITEM == year
						& stats$ISO_3_CODE %in% flagstates
						& stats$SumOfQUANTITY > 0,]
	
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

# @title computeWprob
# @description
# function to compute the probability model which will give probability values by
# target area (represented by a combination of FAO area, LME (reclassified) and 
# EEZ (reclassified as ISO3 code)
# @param intersects an object of class "data.frame" giving the intersects
# @param stats an object of class "data.frame" giving the catch statistics
# @return an object of class "data.frame" giving the probability model to use for reallocation
computeWprob <- function(intersects, stats){	
	dims <- unique(stats[,c("FIC_SYS_CATCH_AREA","YR_ITEM")])
	wprobs <- do.call("rbind", lapply(1:nrow(dims),
						function(i){
							out <- computeWprobByDims(
										intersects, stats,
										area = dims[i,"FIC_SYS_CATCH_AREA"],
										year = dims[i,"YR_ITEM"])
							return(out)
						}))
	return(wprobs)
}

# @title getForeignCatches
# @description
# function to get catches done by foreign flagstates in coastal areas
# foreign flagstates are obtained by difference of coastal flagstates
# (obtained by intersect between EEZ/FAO area)
# @param intersects an object of class "data.frame" giving the intersects
# @param stats an object of class "data.frame" giving the catch statistics
# @return an object of class "data.frame" giving the foreign catch statistics
getForeignCatches <- function(intersects, stats){	
	out <- do.call("rbind",
		lapply(unique(intersects$F_AREA),
			function(area){
				#extract catch statistics of distant flagstates
				area.intersects <- intersects[intersects$F_AREA == area,]
				flagstates <- unique(area.intersects[,"ISO_3digit"])
				out.stats <- stats[stats$FIC_SYS_CATCH_AREA == area
									& !(stats$ISO_3_CODE %in% flagstates)
									& stats$SumOfQUANTITY > 0,]
				return(out.stats)
			}))		
	return(out)
}


#execution & outputs
#==========================================================================

processStartingTime <- Sys.time()
cat(paste0("Started at: ", as.character(processStartingTime),"\n"))

#step 1: compute table of probabilities based on coastal flagstate catch %
#-------
cat("Building probability model for reallocation...\n")
#compute probabilities
wprobs <- computeWprob(intersects, stats)

#validation of probabilities
aggregate(wprobs$wprob, by = as.list(wprobs[c("F_AREA","YR_ITEM")]), FUN = "sum")

#step 2: extract catches for distant countries
#-------
cat("Grabing foreign catches...\n")
foreign_catches <- getForeignCatches(intersects, stats)

#step 3: reallocate based on probability model computed in step 1
#(the function "reallocate" is moved to 'spread' package)
#-------
cat("Reallocating statistical data...\n")
result <- spread::reallocate(
		x = foreign_catches,
		y = wprobs,
		area.x = "FIC_SYS_CATCH_AREA",
		area.y = "F_AREA",
		by.x = "YR_ITEM",
		by.y = "YR_ITEM",
		data = "SumOfQUANTITY",
		warea = NULL,
		wprob = "wprob",
		aggregates = NULL
)

#csv output
write.table(result, "spread_output.csv", row.names = FALSE, col.names = TRUE, sep=",", dec=".")

processEndingTime <- Sys.time()
cat(paste0("Started at: ", as.character(processEndingTime),"\n"))
cat(paste0("Computation completed in ", as.character(round(as.numeric(processEndingTime - processStartingTime),3))," seconds!\n"))

#test case to validate stats
#French catches in 1998 in FAO major area 34
testcase <- result[ result$ISO_3_CODE == "FRA"
			& result$FIC_SYS_CATCH_AREA == 34
			& result$YR_ITEM == 1998,]
cat(paste0("Initial catch value to reallocate = ", unique(testcase$SumOfQUANTITY),"\n"))
cat(paste0("Sum of reallocated values = ", sum(testcase$spread),"\n"))
