#
# Scripts to process the results of zonal statistics pre-computed
# with ArcPy Python scripts
# 
# @author eblondel
# @date 12/10/2015
#

#function to normalize the SST computation results for a given file
normalizeZonalStatistics <- function(file, variable){

	#extract record from name
	filename <- unlist(strsplit(file,"/"))
	filename <- filename[length(filename)]
	record <- substr(filename, as.numeric(regexpr("[0-9]+",filename)), nchar(filename)-4)
	
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
	
	#extract area codes
	areas <- as.data.frame(
		do.call("rbind",strsplit(as.character(df[,"SSTNPPCODE"]),"_")),
		stringsAsFactors = FALSE
	)

	colnames(areas) <- c("EEZ", "LME", "FSA")
	df <- cbind(
		VARIABLE = rep(variable, nrow(df)),
		areas,
		ZONE_CODE = df[,3],
		YEAR = rep(year,nrow(df)),
		MONTH = rep(month,nrow(df)),
		df[,4:11],
		stringsAsFactors = FALSE)
	df.names <- colnames(df)
		
	#normalize
	output <- reshape(df, idvar = df.names[1:7], varying = df.names[8:15],  times = df.names[8:15],
				timevar = "STATISTIC", v.names = "VALUE", direction = "long")
	row.names(output) <- 1:nrow(output)
	return(output)
}

#global function to normalize and merge all SST computation results
fetchZonalStatistics <- function(dir = getwd(), variable){
	
	#variable
	if(!(variable %in% c("SST", "NPP"))) stop("Invalid variable (expected 'SST' or 'NPP')")
	
	#list files
	files <- list.files(path = dir, full.names = TRUE, ignore.case = TRUE,
				  pattern = paste0("[a-z]", variable, "[a-z][0-9]+.csv"))
	
	#process results
	out <- do.call("rbind",lapply(files, normalizeZonalStatistics, variable))	
	return(out)
}

#global function to merge results for several variables
fetchZonalStatisticsAll <- function(dir = getwd(), variables = c("SST", "NPP")){
	
	out <- do.call("rbind", lapply(
		variables,
		function(x) { fetchZonalStatistics(dir, x)}))
	
}


#run the process
result <- fetchZonalStatisticsAll()
write.csv(result, file = "ZonalStatistics.csv")
