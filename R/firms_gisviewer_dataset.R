#packages
#--------
#core packages
require(RCurl)
require(XML)
require(rgeos)
require(maptools)
#FIRMS GIS production functionalities
if(!require(RFirmsGeo)){
  require(devtools)
  install_github("openfigis/RFirmsGeo")
  require(RFirmsGeo)
}
#Common FIGIS functionalities
if(!require(RFigisGeo)){
  require(devtools)
  install_github("openfigis/RFigisGeo")
  require(RFigisGeo)
}
#R interface to GeoServer REST API
if(!require(geosapi)){
  require(devtools)
  install_github("eblondel/geosapi")
  require(geosapi)
}


.default_options <- options()
options(encoding="UTF-8")
options(stringsAsFactors = FALSE)

#working directory
origin.path <- "/home/emmanuel.blondel/firms"
setwd(origin.path)
path <- paste(origin.path, format(Sys.time(), "%Y%m%d"), sep="/")
if(!dir.exists(path)) dir.create(path)
if(dir.exists(path)) setwd(path) else stop("Target directory doesn't exist")

#variables
firmsHost <- "http://www.fao.org"
gsURL <- paste0(firmsHost, "/figis/geoserver")
gsUser <- "user"
gsPwd <- "pwd"
firmsDomains <- c("resource", "fishery")
runParallel <- TRUE
runCores <- 8

system.time(
  for(firmsDomain in firmsDomains) {
    
    #items
    refs <- fetchFactsheetReferences(firmsHost, firmsDomain, verbose = TRUE)
    items = refs$factsheet
      
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
    
    #try to apply long dash (alt+0150) everywhere
    #TODO investigate issue of Geoserver with DBF UTF-8 charset
    #result$TITLE <- gsub("-", "–", result$TITLE, fixed = TRUE)
    result$TITLE <- gsub("–", "-", result$TITLE, fixed = TRUE)
    
    #export results
    #to ESRI Shapefile
    Sys.getlocale("LC_CTYPE")
    getCPLConfigOption("SHAPE_ENCODING")
    encoding <- "UTF-8"
    setCPLConfigOption("SHAPE_ENCODING", encoding)
    writeOGR(result, ".", outputName, driver="ESRI Shapefile", overwrite_layer=T)
    
    writeEncFile <- function(extension, encoding){
      encFile <- file(paste("test_encoding", extension, sep="."))
      writeLines(encoding, encFile, sep="")
      unlink(encFile)
    }
    writeEncFile("cst", encoding)
    writeEncFile("cpg", encoding)
    
    #to zip (for upload)
    zipfilename <- paste0(outputName, ".zip")
    zip(zipfile = zipfilename, files = list.files(pattern = outputName))
  
    #to geojson (for preview)
    file <- paste0(outputName, ".geojson")
    writeOGR(result, file, outputName, driver='GeoJSON', check_exists = FALSE)
    setCPLConfigOption("SHAPE_ENCODING", NULL) #reset encoding shape option
        
    #publish to Geoserver
    gsMan <- GSDatastoreManager$new(url = gsUrl, user = gsUser, pwd = gsPwd)
    gsMan$uploadShapefile(workspace = "firms", datastore = "firms_shapefiles", endpoint = "file",
                          configure = "none", update = "overwrite", zipfilename, "UTF-8")
    
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
