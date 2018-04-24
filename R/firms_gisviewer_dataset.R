#packages
#--------
#core packages
require(RCurl)
require(XML)
require(rgeos)
require(maptools)

#OGC OWS client
if(!require(ows4R)){
  require(devtools)
  install_github("eblondel/ows4R")
  require(ows4R)
}

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
#Facilities to generate OGC/ISO 19115:2003/19139 metadata
if(!require(geometa)){
  require(devtools)
  install_github("eblondel/geometa")
  require(geometa)
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
gsProdUrl <- paste0(firmsHost, "/figis/geoserver")
gsTestUrl <- ""
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
    setCPLConfigOption("SHAPE_ENCODING", NULL)
    writeOGR(result, ".", outputName, driver="ESRI Shapefile", overwrite_layer=T)
    writeEncFile <- function(extension, encoding = "UTF-8"){
      encFile <- file(paste(outputName, extension, sep="."))
      writeLines(encoding, encFile, sep="")
      unlink(encFile)
    }
    writeEncFile("cst")
    writeEncFile("cpg")
    
    #to zip (for upload)
    zipfilename <- paste0(outputName, ".zip")
    zip(zipfile = zipfilename, files = list.files(pattern = outputName))
  
    #to geojson (for preview)
    file <- paste0(outputName, ".geojson")
    writeOGR(result, file, outputName, driver='GeoJSON', check_exists = FALSE)
        
    #publish to Geoserver
    endpoints <- gsProdUrl
    if(gsTestUrl != "") endpoints <- c(gsTestUrl, gsProdUrl)
    for(endpoint in endpoints){
      gsMan <- GSManager$new(url = endpoint, user = gsUser, pwd = gsPwd, logger = "INFO")
      gsMan$uploadShapefile(ws = "firms", ds = "firms_shapefiles", endpoint = "file",
                            configure = "none", update = "overwrite", zipfilename, "UTF-8")
    }
    
    #create & publish metadata
    resultMeta <- buildSpatialMetadata(result)
    #TODO publication (insert/update)
    
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
