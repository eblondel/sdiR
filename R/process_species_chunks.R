#
# Easy script to build chunks of species code lists.
# The script is primarily used to produce chunks in order to publish/maintain
# the collection of GIS species distribution data/metadata by chunks, given
# the low availability of FAO Geonetwork updateMetadata web-service endpoint
#
# @author eblondel
# @date 2016/06/28
#


#basic function to build chunks of species codes
buildChunks <- function(array, n = 10, toXML = TRUE){
	chunks <- split(array, ceiling(seq_along(array)/20))
	if(toXML){
		chunks <- lapply(chunks,function(chunk.list){
			return(do.call("paste",lapply(chunk.list, function(x){paste("<string>",x,"</string>")})),collapse="")
		})
	}
	return(chunks)
}

if(FALSE){
	require(XML)
	speciesXML <- xmlParse("http://www.fao.org/figis/geoserver/factsheets/js/specieslist.xml")
	species <- sapply(getNodeSet(speciesXML, "//item"), xmlGetAttr,"a3c")
	species <- species[order(species)]
 	buildChunks(species, n=100, toXML = TRUE)
}
