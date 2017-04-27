#
# Scripts to process the results of zonal statistics pre-computed
# with ArcPy Python scripts
# 
# @author eblondel
# @date 12/10/2015
#


# setwd("") Uncomment to setup the working directory; remember to escape "\" in the windows path with "\\"
#function to normalize the SST computation results for a given file
normalizeZonalStatistics <- function(file, variable, extractAreaCodes = FALSE, chunks = 0, suffix = NULL){

	#extract record from name
	filename <- unlist(strsplit(file,"/"))
	filename <- filename[length(filename)]
	record <- substr(filename, as.numeric(regexpr("[0-9]+",filename)), nchar(filename)-4)
    if(chunks>0) record <- unlist(strsplit(record,"_"))[1]
    if(!is.null(suffix)) record <- unlist(strsplit(record, suffix))[1]
	
	#process time dimension	
	year <- as.numeric(substr(record, 1, 4))
	isLeapYear <- (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0))
	month <- as.numeric(substr(record, 5, nchar(record)))
	if(isLeapYear & !(month %in% c(1,32))) month = month - 1
	month <- switch(as.character(month), "1" = 1, "32" = 2, "60" = 3, "91" = 4,
							 "121" = 5, "152" = 6, "182" = 7, "213" = 8,
			    				 "244" = 9, "274" = 10, "305" = 11, "335" = 12)
	#read data
	df <- read.csv(file)
	
    #prepare df.new
    df.new <- data.frame(VARIABLE = rep(variable, nrow(df)), stringsAsFactors = FALSE) 
	
    #extract area codes
    areaCodeNb <- 0
    if(extractAreaCodes){
        #(NOTE: a new code combination is used called SSTNPPCODN in place of the orginal SSTNPPCODE)
        areas <- as.data.frame(
            do.call("rbind",strsplit(as.character(df[,"SSTNPPCODN"]),"_")),
            stringsAsFactors = FALSE
        )
        colnames(areas) <- c("EEZ", "LME", "FSA")
        df.new <- cbind(df.new, areas)
         areaCodeNb <- 3
    }
	
    statColIdx <- if(extractAreaCodes) 6:11 else 5:10
	df.new <- cbind(
		df.new,
		ZONE_CODE = df[,ifelse(extractAreaCodes,3,2)],
		YEAR = rep(year,nrow(df)),
		MONTH = rep(month,nrow(df)),
		df[,statColIdx],
		stringsAsFactors = FALSE
    )
	df.names <- colnames(df.new)
		
	#normalize
	statistics <- c("MIN", "MAX", "RANGE", "MEAN" , "STD", "SUM") 
	output <- reshape(df.new, idvar = df.names[1:(4+areaCodeNb)], varying = statistics,  times = statistics,
				timevar = "STATISTIC", v.names = "VALUE", direction = "long")
	row.names(output) <- 1:nrow(output)
	return(output)

}

#global function to normalize and merge all SST computation results
fetchZonalStatistics <- function(dir = getwd(), variable, extractAreaCodes = FALSE, chunks = 0, suffix = NULL){
	
	#variable
	#modified to account for different data using other NPP models
	if(!(variable %in% c("CBPM", "EPPL", "SST", "NPP", "CHL"))) stop("Invalid variable (expected 'CBPM', 'EPPL', 'SST' or 'NPP' or 'CHL')")
	
	#list files
    suffixPattern <- ""
    if(chunks > 0) suffixPattern <- paste0("_[",1,"-",chunks,"]")
    if(!is.null(suffix)) suffixPattern <- suffix
	files <- list.files(path = dir, full.names = TRUE, ignore.case = TRUE,pattern = paste0("[a-z]", variable, "[a-z][0-9]+", suffixPattern, ".csv"))
	
	#process results
	out <- do.call("rbind",lapply(files, normalizeZonalStatistics, variable, extractAreaCodes, chunks, suffix))	
	return(out)
}

#global function to merge results for several variables
#modified to account for different data using other NPP models
fetchZonalStatisticsAll <- function(dir = getwd(), variables = c("CBPM", "EPPL", "SST", "NPP", "CHL"), extractAreaCodes = FALSE,
                                    chunks = 0, suffix = NULL){
	out <- do.call("rbind", lapply(
		variables,
		function(x) { fetchZonalStatistics(dir, x, extractAreaCodes, chunks, suffix)}))
	out <- cbind(RowID = 1:nrow(out), out)
	return(out)
}

#run the process
#use extractAreaCodes = TRUE if there is area code concatenation field such as 'SSTNPPCODN' in the source files, to extract as columns
result <- fetchZonalStatisticsAll(extractAreaCodes = FALSE)
baseName <- sprintf("ZonalStatistics_%s_",paste(unique(result$VARIABLE), collapse="-"))
outputFile<- paste0(baseName, format(Sys.time(),"%Y%m%d%H%M%S"),".csv")
write.csv(result, file = outputFile, row.names = FALSE)
