#
# Scripts to experiment SPREAD-like algorithm to estimate effort (initially
# available by flagstate/year) to the target area defined by FAO area / EEZ
# / reclassified LME, for which catches where previously reallocated.
# 
# @author eblondel
# @date 16/09/2016
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
#read effort table
effort <- read.table("Effort_by_Country_1950-2012.csv", h=T, sep=",")
effort_long <- reshape(effort,idvar=c("Country", "ISO_3_CODE"), varying = list(3:ncol(effort)), direction="long", v.names = "effort")
effort_long$time <- as.integer(sapply(1:nrow(effort_long), function(i){unlist(strsplit(colnames(effort)[2+effort_long[i,"time"]],"X"))[[2]]}))

#read intersects
catches <- read.table("spread_output.csv", h=T, sep=",")
catches <- catches[,-(16:20)] #remove previous spread computation fields
catches$wprob <- catches$spread
catches$spread <- NULL

#set roundingDecimals set to NULL if you don't want to round at all
#roundingDecimals <- NULL
roundingDecimals <- 0

#business functions
#==========================================================================


#execution & outputs
#==========================================================================

processStartingTime <- Sys.time()
cat(paste0("Started at: ", as.character(processStartingTime),"\n"))

cat("Reallocating statistical data...\n")
result <- spread::reallocate(
		x = effort_long,
		y = catches,
		area.x = "ISO_3_CODE",
		area.y = "ISO_3_CODE",
		by.x = "time",
		by.y = "YR_ITEM",
		data = "effort",
		warea = NULL,
		wprob = "wprob",
		aggregates = NULL
)
if(!is.null(roundingDecimals)) result$spread <- round(result$spread, roundingDecimals)

#csv output
write.table(result, "spread_output_effort.csv", row.names = FALSE, col.names = TRUE, sep=",", dec=".")

processEndingTime <- Sys.time()
cat(paste0("Started at: ", as.character(processEndingTime),"\n"))
cat(paste0("Computation completed in ", as.character(round(as.numeric(as.integer(processEndingTime) - as.integer(processStartingTime)),3))," seconds!\n"))

#test case to validate stats
#note that in case of 'rounding' you may have slightly different values
testcase <- result[ result$ISO_3_CODE == "ESP" & result$time == 1980,]
cat(paste0("Initial catch value to reallocate = ", unique(testcase$effort),"\n"))
cat(paste0("Sum of reallocated values = ", sum(testcase$spread),"\n"))
