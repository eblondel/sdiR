#
# Easy script to get species distribution mappings including:
# - identifiers: 3-alpha code, FigisId
# - layer name, title
# - urls: metadata URL (html), factsheet URL (html), FLOD URL
#
# @author eblondel
# @date 2015/10/28
#

library(httr)
library(XML)

req = GET("http://www.fao.org/figis/geoserver/species/ows?service=WMS&version=1.3.0&request=GetCapabilities")
xml = content(req)
layers <- getNodeSet(xml, "//ns:Layer[@queryable='1']", c(ns = "http://www.opengis.net/wms"))

#get name
getName <- function(x){
	name = xmlValue(getNodeSet(xmlDoc(x),"//ns:Name",c(ns = "http://www.opengis.net/wms"))[[1]])
	return(name)
}

#get title
getTitle <- function(x){
	title <- xmlValue(getNodeSet(xmlDoc(x), "//ns:Title", c(ns = "http://www.opengis.net/wms"))[[1]])
	return(title)
}

#get factsheet link
getFactsheet <- function(x){
	keywords <- sapply(getNodeSet(xmlDoc(x),"//ns:Keyword",c(ns = "http://www.opengis.net/wms")), xmlValue)
	figisKeyword <- keywords[regexpr("FIGIS",keywords)>0]
	figisId <- unlist(strsplit(figisKeyword," "))[1]
	fsUrl <- sprintf("http://www.fao.org/fishery/species/%s", figisId)
	return(fsUrl)
}

getMetadataUrl <- function(x){
	metadata <- getNodeSet(xmlDoc(x), "//ns:MetadataURL[ns:Format ='text/html']", c(ns = "http://www.opengis.net/wms"))[[1]]
	mdUrl <- xmlGetAttr(getNodeSet(xmlDoc(metadata),"//ns:OnlineResource", c(ns = "http://www.opengis.net/wms"))[[1]],"xlink:href")
	return(mdUrl)
}

getFLODUrl <- function(x){
  flodUrl <- xmlValue(getNodeSet(xmlDoc(x),"//ns:Identifier[@authority='FLOD']", c(ns = "http://www.opengis.net/wms"))[[1]])
  return(flodUrl)
}

layerNames <- sapply(layers, getName)
speciesCodes <- sapply(strsplit(layerNames,"SPECIES_DIST_"), function(x){x[2]})
factsheetUrls <- sapply(layers, getFactsheet)
figisIds <- sapply(strsplit(factsheetUrls, "http://www.fao.org/fishery/species/"), function(x){x[2]})
layerTitles <- sapply(layers, getTitle)
speciesNames <- sapply(strsplit(layerTitles, "of "), function(x){x[2]})

#output
mapping <- data.frame(
	ALPHACODE = speciesCodes,
	FIGISID = figisIds,
	NAME = speciesNames,
	LAYER = layerNames,
	TITLE = layerTitles,
	METADATA_URL = sapply(layers, getMetadataUrl),
	FACTSHEET_URL = factsheetUrls,
	FLOD_URL = sapply(layers, getFLODUrl),
	stringsAsFactors = FALSE
)
write.table(mapping, "mapping.csv", sep=",", row.names = FALSE)
