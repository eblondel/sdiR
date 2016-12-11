#packages
require(RFirmsGeo)
require(RFigisGeo)
require(RCurl)
require(XML)
require(rgeos)
require(maptools)

.default_options <- options()
options(encoding = "UTF-8")
options(stringsAsFactors = FALSE)

#working directory
origin.path <- "D:/Mes documents/Documents/DEV/R/firms/results" #TODO to be defined for execution with FI
setwd(origin.path)
path <- paste(origin.path, format(Sys.time(), "%Y%m%d"), sep="/")
if(!dir.exists(path)) dir.create(path)
if(dir.exists(path)) setwd(path) else stop("Target directory doesn't exist")

#variables

#variables
firmsHost <- "http://www.fao.org"
firmsDomains <- c("resource", "fishery")
runParallel <- TRUE
runCores <- 8

system.time(
  for(firmsDomain in firmsDomains) {

    refs <- fetchFactsheetReferences(firmsHost, firmsDomain, verbose = TRUE)
    items <- refs$factsheet
    
    #running the data processing
    #---------------------------
    #with exporting partial results (polygon and point)
    #final result gives directly the point dataset
    outputName <- paste(firmsDomain, "all_points", sep = "_")
    system.time(
      capture.output(
        result <- invisible(buildSpatialDataset(
                    host = firmsHost, domain = firmsDomain,
                    cleanGeom = TRUE, cleanStrategy = "BUFFER",
                    unionStrategy = "bbox",
                    runParallel = runParallel, runCores = runCores,
                    exportPartialResults = FALSE, exportPath = getwd(), verbose = TRUE
                  )),
        file = paste0(firmsDomain, ".log")
      )
    )

    #export results
    #to ESRI Shapefile
    exportFeatures(result, file.path = path, file.name = outputName)
    zip(zipfile = paste0(outputName, ".zip"), files = list.files(pattern = outputName))

    #to geojson (for preview)
    file <- paste0(outputName, ".geojson")
    writeOGR(result, file, outputName, driver='GeoJSON', check_exists = FALSE)

    #check missing factsheets
    missingItems <- items[!(items %in% unique(result@data$FIGIS_ID))]
    if(length(missingItems) > 0){
      missingItems <- as.integer(missingItems)
      missingItems <- missingItems[order(missingItems)]
      print(sprintf("Missing '%s' items: [%s]", firmsDomain, paste(missingItems, collapse=",")))
    }

  }
)
options(.default_options)
