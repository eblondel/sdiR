#
# Utils to generate RFB maps
#	@author eblondel
#	@date 15/09/2015
#

#required packages
#------------------
require("httr")
require("XML")

#functions
#---------

#function to get the rfb acronym
getRfbAcronym <- function(rfbItem){
	name <- xmlGetAttr(rfbItem, "name")
	return(name)
}

#function to get the rfb title
getRfbTitle <- function(rfbItem, uppercase = FALSE){
	descriptor <- getNodeSet(xmlDoc(rfbItem), "//descriptor")[[1]]
	rfbTtitle <- xmlGetAttr(descriptor, "title")
	if(uppercase){
		rfbTitle <- toupper(rfbTtitle)
	}
	return(rfbTitle)
}

#function to get Rfb acronyms
getRfbAcronyms <- function(rfbItems){
	out <- sapply(rfbItems, getRfbAcronym)
	return(out)
}

#function to get the rfb layer style to apply
getRfbStyle <- function(rfbItem){
	style <- xmlGetAttr(rfbItem, "style")
	return(style)
}

#function to get country members
getRfbCountryMembers <- function(rfbItem){
	countryListXML <- getNodeSet(xmlDoc(rfbItem), "//country")
	countries <- sapply(countryListXML, xmlValue)
	return(countries)
}

#simple function to build a wms request
getRfbMap <- function(rfbItem, figisapps = TRUE, uppercase = FALSE,
					  dpi = 90, width=684, height=330, imgFormat = "img/png"){
	
	#baseUrl
	host <- ifelse(figisapps, "http://figisapps.fao.org","http://www.fao.org")
	baseUrl <- paste0(host, "/figis/geoserver/wms?service=WMS&version=1.1.0&request=GetMap")
	
	#layers
	layers <- c("fifao:limit_200nm", "fifao:RFB_COMP", "fifao:FAO_MAJOR", "fifao:UN_CONTINENT2", "fifao:MarineAreas", "fifao:country_bounds")
	layerString <- paste(layers, collapse=",")	

	#styles
	rfbStyle <- getRfbStyle(rfbItem)
	styles <- c("",rfbStyle, "","","", "")
	styleString <- paste(styles, collapse=",")

	#filters (it may help when the online moniker is not yet updated)
	acronym <- getRfbAcronym(rfbItem)
	rfbFilter <- paste0("RFB = '", acronym, "'")
	countries <- getRfbCountryMembers(rfbItem)
	if("NOR" %in% countries) countries <- c(countries, "SJM")
	if("DNK" %in% countries) countries <- c(countries, "GRL")
	# additional exceptions
	if("WECAFC" %in% acronym) countries <- c(countries, "GUF")
	if("ICCAT" %in% acronym) countries <- c(countries, "GUF")
	
	countryFilter <- paste0("ISO_3 IN (", paste(paste0("'", countries, "'"), collapse=","), ")")
	filters <- c("INCLUDE",rfbFilter, "INCLUDE", "INCLUDE", "INCLUDE", countryFilter)
	filterString <- paste(filters, collapse=";")
	
	#wms map
	map <- paste0(baseUrl, "&layers=", layerString, "&styles=", styleString, "&cql_filter=", filterString)
	map <- paste0(map, "&bbox=-180,-90,180,90&width=",width,"&height=",height,"&srs=EPSG:4326&format=", imgFormat)
	
	#add wms dpi & decoration
	myLayout <- "rfbmap"
	if(acronym %in% c("NAFO", "NEAFC")){
		myLayout <- "rfbmap_RA"
	}
	map <- paste0(map, "&format_options=dpi:",dpi,";layout:", myLayout)
	map <- paste0(map, "&env=rfbname:", getRfbTitle(rfbItem, uppercase))
	
	return(map)
}

getRfbMaps <- function(figisapps = TRUE, uppercase = FALSE, dpi = 90, width = 684, height=330, imgFormat = "img/png"){
	
	#get the figismap data
	req <- GET("http://www.fao.org/figis/moniker/figismapdata")
	rfbXML <- content(req)
	rfbListXML <- getNodeSet(rfbXML, "//rfb")
	rfbs <- getRfbAcronyms(rfbListXML)
	
	out <- data.frame( RFB = rfbs,
					   MAP = sapply(rfbListXML, getRfbMap, figisapps, uppercase, dpi, width, height, imgFormat),
					   stringsAsFactors = FALSE)
	return(out)
}

#function if you want to download the maps
downloadRfbMaps <- function(maps, destfolder){
	for(i in 1:nrow(maps)){
		suppressWarnings(download.file(maps[i,"MAP"], paste0(destfolder, "/", maps[i,"RFB"],".png"), mode="wb", quiet = FALSE))
	}
}

#Execute the code
#----------------

#my list of RFBs
myRfbs <- c("CECAF", "ICCAT", "ICES", "NASCO", "NEAFC", "NAFO", "SEAFO", "WECAFC")

#generate the wms map requests
maps <- getRfbMaps(figisapps = TRUE, uppercase = TRUE, dpi = 150, width = 2280, height=1100, imgFormat = "image/png; mode=8bit")
myMaps <- maps[maps$RFB %in% myRfbs,]

#in case you want to download these maps to a target folder
myfolder <- "D:/Mes documents/Documents/DEV/R/fabio/20150915_RFB_Maps"
downloadRfbMaps(myMaps, myfolder)

