
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
#set your working dir here with setwd()
#setwd("D:/Mes documents/Documents/DEV/R/sdi/20160913_SPREAD")
options(stringsAsFactors = FALSE)

#package requirements
#==========================================================================
if(!require(spread)){
	require(devtools)
	install_github("eblondel/spread")
	require(spread)
}

#inputs
#==========================================================================
#read catch statistics (normalized form)
#the file "CaptureView-modified.csv" is a result of a first data reallocation
#exercise provided by the script 'spread_old_flagstates.R'
stats <- read.table("CaptureView-modified.csv", h=T,sep=",")
stats <- stats[stats$SumOfQUANTITY > 0,]

#read intersects
intersects <- read.table("EEZ_FA_LMErcl.txt", h=T,sep=",")

#set roundingDecimals set to NULL if you don't want to round at all
#roundingDecimals <- NULL
roundingDecimals <- 0

#business functions
#==========================================================================

#build a intersecting table with probabilities by coastal flagstates
#computation is done taking into account the following dimensions:
#- FIC_SYS_CATCH_AREA (FAO major area)
#- YR_ITEM (year)

# @title computeWprobByDims
# @description
# unit function to compute by area/year/flagstate the probability model which will give
# probability values by target area (represented by a combination of FAO area,
# LME (reclassified) and EEZ (reclassified as ISO3 code). This function will distinguish
# among DISTANT vs. COASTAL flagstates
# @param intersects an object of class "data.frame" giving the intersects
# @param stats an object of class "data.frame" giving the catch statistics
# @param area an object of class "integer" or "character" (FAO major area)
# @param year an object of class "integer" or "character" (year)
# @param flagstate an object of class "character" (ISO3 code of flagstate)
# @param includeDistantCoastalWaters an object of class "logical". Default is FALSE. If TRUE, then in case of EEZs
# on which the distant flagstate has sovereignty, a probably will be assigned corresponding to the % of target intersect
# surface on the total intersect surface in the area.
# @param useCoastalWaterSurfaces object of class "logical". Default is FALSE. If TRUE, in case of coastal flagstates,
# the surface will be used as probability to reallocate values among multiple waters.
# @return an object of class "data.frame" giving the probability model to use for reallocation
computeWprobByDims <- function(intersects, stats, area, year, flagstate,
								includeDistantCoastalWaters = FALSE,
								useCoastalWaterSurfaces = FALSE){

	isDistant <- TRUE
	out.wprobs <- NULL

	#extract stats for coastal flagstates only
	area.intersects <- intersects[intersects$F_AREA == area,]
	
	#determine if flagstate is considered as DISTANT or COASTAL in this area
	fsIdx <- area.intersects$ISO_3digit == flagstate
	fs <- area.intersects[fsIdx,]
	if(nrow(fs) > 0){
		isDistant <- !any(fs$Country == fs$Sovereign)
	}
	
	if(isDistant){
		#Case of distant flagstates
		#--------------------------
		coastal <- area.intersects
		if(nrow(fs) > 0) coastal <- area.intersects[!fsIdx,]
		flagstates <- unique(coastal[,"ISO_3digit"])
		
		#probabilities without considering potential EEZ on which the flagstate has sovereignty
		out.stats <- stats[stats$FIC_SYS_CATCH_AREA == area
					& stats$YR_ITEM == year
					& stats$ISO_3_CODE %in% flagstates,]			
		
		if(nrow(out.stats) > 0){	
			#compute wprob
			out.stats$flagstate <- flagstate
			out.stats$wprob <- out.stats$SumOfQUANTITY / sum(out.stats$SumOfQUANTITY)
			out.stats <- out.stats[,c("FIC_SYS_CATCH_AREA", "ISO_3_CODE", "YR_ITEM", "flagstate", "wprob")]
			
			#merge intersects with wprob
			out.wprobs <- merge(x = intersects, y = out.stats,
						by.x = c("F_AREA", "ISO_3digit"), by.y = c("FIC_SYS_CATCH_AREA", "ISO_3_CODE"), all.y = TRUE)
			out.wprobs <- out.wprobs[order(as.factor(out.wprobs$EcoTypeNew)),]
			out.wprobs <- out.wprobs[!duplicated(out.wprobs[,c("F_AREA", "ISO_3digit", "wprob")], fromLast = TRUE),]
		}
		
		if(nrow(fs) > 0){
			if(includeDistantCoastalWaters){
				#compute the prob to associate to EEZ on which the distant flagstate has sovereignty
				fsprob <- 1 - sum(out.wprobs$Surface) / sum(area.intersects$Surface)
				out.wprobs <- rbind(
					cbind(fs, YR_ITEM = year, flagstate = flagstate, wprob = fsprob),
					out.wprobs
				)
			}
		}
	
	}else{
		#Case of coastal flagstates
		#--------------------------
		coastal.probs <- switch(as.character(useCoastalWaterSurfaces),
			"TRUE" = fs$Surface / sum(fs$Surface),
			"FALSE" = rep(1/nrow(fs), nrow(fs))
		)
		out.wprobs <- cbind(fs, YR_ITEM = year, flagstate = flagstate, wprob = coastal.probs)
	}
	
	cnames <- c("F_AREA", "ISO_3digit", "EEZ", "Country", "Sovereign", "FID_LME_re", 
				"LME_NUMBER", "LME_NAME", "Classified", "SSTNPPCODE", "EcoTypeNew", 
				"SSTNPPCODN", "LonCentr", "ISO3_FSA", "Surface", "YR_ITEM", "flagstate", "wprob")
	out.wprobs <- out.wprobs[, cnames]
	return(out.wprobs)
}

# @title computeWprob
# @description
# function to compute the probability model which will give probability values by
# target area (represented by a combination of FAO area, LME (reclassified) and 
# EEZ (reclassified as ISO3 code)
# @param intersects an object of class "data.frame" giving the intersects
# @param stats an object of class "data.frame" giving the catch statistics
# @param includeDistantCoastalWaters an object of class "logical". Default is FALSE. If TRUE, then in case of EEZs
# on which the distant flagstate has sovereignty, a probably will be assigned corresponding to the % of target intersect
# surface on the total intersect surface in the area.
# @param useCoastalWaterSurfaces object of class "logical". Default is FALSE. If TRUE, in case of coastal flagstates,
# the surface will be used as probability to reallocate values among multiple waters.
# @param verbose if logs have to be provided. Default is FALSE.
# @return an object of class "data.frame" giving the probability model to use for reallocation
computeWprob <- function(intersects, stats,
						 includeDistantCoastalWaters = FALSE,
						 useCoastalWaterSurfaces = FALSE,
						 verbose = FALSE){
						 
	dims <- unique(stats[,c("FIC_SYS_CATCH_AREA","YR_ITEM", "ISO_3_CODE")])
	wprobs.list <- lapply(1:nrow(dims),
						function(i){
							trgArea <- dims[i,"FIC_SYS_CATCH_AREA"]
							trgYear <- dims[i,"YR_ITEM"]
							trgFlagstate <- dims[i, "ISO_3_CODE"]
							cat(sprintf("Computing wprobs for Area %s / year %s / flagstate '%s' \n", trgArea, trgYear, trgFlagstate))
							out <- computeWprobByDims(
										intersects, stats,
										area = trgArea,
										year = trgYear,
										flagstate = trgFlagstate,
										includeDistantCoastalWaters = includeDistantCoastalWaters,
										useCoastalWaterSurfaces = useCoastalWaterSurfaces)
							return(out)
						})
	wprobs.list <- wprobs.list[!sapply(wprobs.list, is.null)]
	wprobs <- do.call("rbind", wprobs.list)
	return(wprobs)
}

#execution & outputs
#==========================================================================

processStartingTime <- Sys.time()
cat(paste0("Started at: ", as.character(processStartingTime),"\n"))

#step 1: compute table of probabilities based on coastal flagstate catch %
#-------
cat("Building probability model for reallocation...\n")
#compute probabilities
wprobs <- computeWprob(intersects, stats, includeDistantCoastalWaters = TRUE, useCoastalWaterSurfaces = TRUE)

#validation of probabilities
#aggregate(wprobs$wprob, by = as.list(wprobs[c("F_AREA","YR_ITEM")]), FUN = "sum")


#step 2: reallocate based on probability model computed in step 1
#(the function "reallocate" is moved to 'spread' package)
#-------
cat("Reallocating statistical data...\n")
result <- spread::reallocate(
		x = stats,
		y = wprobs,
		area.x = "FIC_SYS_CATCH_AREA",
		area.y = "F_AREA",
		by.x = c("YR_ITEM", "ISO_3_CODE"),
		by.y = c("YR_ITEM", "flagstate"),
		data = "SumOfQUANTITY",
		warea = NULL,
		wprob = "wprob",
		aggregates = NULL
)
if(!is.null(roundingDecimals)) result$spread <- round(result$spread, roundingDecimals)

#csv output
write.table(result, "spread_output.csv", row.names = FALSE, col.names = TRUE, sep=",", dec=".")

processEndingTime <- Sys.time()
cat(paste0("Started at: ", as.character(processEndingTime),"\n"))
cat(paste0("Computation completed in ", as.character(round(as.numeric(as.integer(processEndingTime) - as.integer(processStartingTime)),3))," seconds!\n"))

#test case to validate stats
#French catches in 1998 in FAO major area 34
#note that in case of 'rounding' you may have slightly different values
testcase <- result[ result$ISO_3_CODE == "FRA"
			& result$FIC_SYS_CATCH_AREA == 34
			& result$YR_ITEM == 1998,]
cat(paste0("Initial catch value to reallocate = ", unique(testcase$SumOfQUANTITY),"\n"))
cat(paste0("Sum of reallocated values = ", sum(testcase$spread),"\n"))
