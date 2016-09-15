#
# Scripts to experiment SPREAD-like algorithm for catch statistics of
# former USSR by FAO major area / year. Based on a given period (post
# 1988), % of catches for flagstates that result from the end of USSR
# are computed, and then formerUSSR catches splitted accordingly.
# 
# @author eblondel
# @date 14/09/2016
#
#==========================================================================

#working directory & environment
#==========================================================================
#set your working dir here using setwd()
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
stats <- read.table("CaptureView.csv", h=T,sep=",")
stats <- stats[stats$SumOfQUANTITY > 0,]


#params for splitting catches from old flagstaes
#The "flagParams" is a simple object composed by a list of flagstate definitions.
#	Each 'definition' defines the 'old' flagstate (ISO3 code) and the 'new'
#	flagstates (resulting from the splitting)
# Note: in R the simplest way to define an 'object' with some properties is
# to use the "list" primitive.
flagParams = list(
	list(
		old = "SUN",
		new = c("EST", "LVA", "LTU", "UKR", "GEO", "RUS")
	)
	#,list(
	#	old = "TEST",
	#	new = c("TEST1", "TEST2")
	#)
)

#set roundingDecimals set to NULL if you don't want to round at all
#roundingDecimals <- NULL
roundingDecimals <- 0

#business functions
#==========================================================================

# @title reallocateOldFlagstateCatches
# @description Splits catch statistics from on an old flagstate (which country is now splitted)
# using the contributions of catches of the new flagstates resulting from the country split..
# @param stats an object of class "data.frame" giving the catch statistics
# @param flagstates an object of class "list" giving the old flagstate and list of new flagstates
# (with ISO codes) in the following form list(old = "TEST", new = c("TEST1", "TEST2"))
# @param data.period object of class "integer" giving the start/end years of the range targeted for the
# data reallocation.
# @param wprob.period object of class "integer" giving the start/end years of the range chosen to compute
# the relative contributions of catch of each new flagstate. Default value is c(1988,1992)
# @return an object of class "data.frame" giving reallocated statistics for the data.period selected
# and the new flagstates defined in the 'flagstates' argument
#
reallocateOldFlagstateCatches <- function(stats, flagstates, data.period = c(1950,1987), wprob.period = c(1988,1992)){
	
	#get data for new flagstates and the selected period
	ref.stats <- stats[stats$ISO_3_CODE %in% flagstates$new
					 & stats$YR_ITEM >= wprob.period[1]
					 & stats$YR_ITEM <= wprob.period[2],]
	ref.stats <- aggregate(ref.stats$SumOfQUANTITY, by = as.list(ref.stats[,c("FIC_SYS_CATCH_AREA","ISO_3_CODE")]), FUN = "sum")
	
	#calculate contribs
	ref.contribs <- do.call("rbind",
		lapply(unique(ref.stats$FIC_SYS_CATCH_AREA),
			function(i){
				out <- ref.stats[ref.stats$FIC_SYS_CATCH_AREA == i,]
				out$wprob <- out$x / sum(out$x)
				return(out)
			}
		)
	)
	ref.contribs <- ref.contribs [order(as.factor(ref.contribs$FIC_SYS_CATCH_AREA)),] #ordering (just a convenience for checking result...)
	
	#check validity of contribs
	#aggregate(ref.contribs$wprob, by = as.list(ref.contribs["FIC_SYS_CATCH_AREA"]), FUN = "sum")
	
	#calculate new stats
	src.idx <- which(stats$ISO_3_CODE == flagstates$old
					 & stats$YR_ITEM >= data.period[1]
					 & stats$YR_ITEM <= data.period[2])
	src.stats <- stats[src.idx,]
	spread.stats <- spread::reallocate(
		x = src.stats,
		y = ref.contribs,
		area.x = "FIC_SYS_CATCH_AREA",
		area.y = "FIC_SYS_CATCH_AREA",
		by.x = NULL,
		by.y = NULL,
		data = "SumOfQUANTITY",
		warea = NULL,
		wprob = "wprob",
		aggregates = NULL
	)
	#adapt structure for merging with other stats
	spread.stats$ISO_3_CODE <- spread.stats$ISO_3_CODE.y
	spread.stats$SumOfQUANTITY <- spread.stats$spread
	if(!is.null(roundingDecimals)) spread.stats$SumOfQUANTITY <- round(spread.stats$SumOfQUANTITY, roundingDecimals)
	spread.stats <- spread.stats[,colnames(stats)]

	#remove old stats
	new.stats <- rbind(stats[-src.idx,], spread.stats)
	return(new.stats)
}

# @title reallocateOldFlagstateCatchesAll
# @description Applies the above 'reallocateOldFlagstateCatches' function for all flagstate definitions
# @param params an object of class "list" giving the list of flagstate definitions. Each flagstate definition
# is itself a list object giving the old flagstate and list of new flagstates. An example of params object would be
#	list(
#		list(old = "TEST1", new = c("TEST1a", "TEST1b")),
#		list(old = "TEST2", new = c("TEST2a", "TEST2b"))
#	)
# @param data.period object of class "integer" giving the start/end years of the range targeted for the
# data reallocation.
# @param wprob.period object of class "integer" giving the start/end years of the range chosen to compute
# the relative contributions of catch of each new flagstate. Default value is c(1988,1992)
# @return an object of class "data.frame" giving reallocated statistics for the data.period selected
# and the new flagstates defined in the 'params' argument
#
reallocateOldFlagstateCatchesAll <- function(stats, params, data.period = c(1950,1987), wprob.period = c(1988,1992)){
	new.stats <- stats
	invisible(
		lapply(params,
			function(flagstates){
				new.stats <<- reallocateOldFlagstateCatches(new.stats, flagstates, data.period = data.period, wprob.period = wprob.period)
			}
		)
	)
	return(new.stats)
}


#execution & outputs
#==========================================================================

#starting
processStartingTime <- Sys.time()
cat(paste0("Started at: ", as.character(processStartingTime),"\n"))

result <- reallocateOldFlagstateCatches(stats, flagParams[[1]], data.period = c(1950,1987), wprob.period = c(1988,1992))

#the next line is to trigger instead in case you have more than one 'flagstates' definition (e.g. adding Yugoslavia, etc)
#result <- reallocateOldFlagstateCatchesAll(stats, flagParams, data.period = c(1950,1987), wprob.period = c(1988,1992))

#csv output
write.table(result, "CaptureView-modified.csv", row.names = FALSE, col.names = TRUE, sep=",", dec=".")

#ending
processEndingTime <- Sys.time()
cat(paste0("Started at: ", as.character(processEndingTime),"\n"))
cat(paste0("Computation completed in ", as.character(round(as.numeric(processEndingTime - processStartingTime),3))," seconds!\n"))

#testcases
#---------
#check that we don't have anymore catches for "SUN"
#normally we should get only those few records reported during transition of USSR to splitted countries (ie from 1988)
result[result$ISO_3_CODE == "SUN",]

#test case to validate stats
#USSR catches in 1985 in FAO major area 34
#note that in case of 'rounding' you may have slightly different values
src <- stats[stats$FIC_SYS_CATCH_AREA == 34 & stats$YR_ITEM == 1985 & stats$ISO_3_CODE == flagParams[[1]]$old,]
trg <- result[result$FIC_SYS_CATCH_AREA == 34 & result$YR_ITEM == 1985 & result$ISO_3_CODE %in% flagParams[[1]]$new,]
cat(paste0("Initial catch value to reallocate = ", src$SumOfQUANTITY,"\n"))
cat(paste0("Sum of reallocated values = ", sum(trg$SumOfQUANTITY),"\n"))
