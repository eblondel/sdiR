#
# Scripts to manage the fishing areas GIS dataset
# This includes the production of:
# - normalized dataset FAO_AREAS (not erased)
# - normalized dataset and erased by continent FAO_AREAS_ERASE
# 
# The derivate products handle the status of eac area, whether it is
# officially endorsed by CWP, or still at draft stage
#
# @author eblondel
# @date 2015/10/27
#


#get fishery statistical area levels
getFisheryStatAreas <- function(){
	fisheryStatAreas <- data.frame(
		propertyName = c("F_AREA", "F_SUBAREA", "F_DIVISION", "F_SUBDIVIS", "F_SUBUNIT"),
		levelName = c("MAJOR", "SUBAREA", "DIVISION", "SUBDIVISION", "SUBUNIT"),
		stringsAsFactors = FALSE
	)
	return(fisheryStatAreas)
}

#dissolve feature
dissolveFeature <- function(area, features, cleanGeom = TRUE){
	
	#proceed to the build
	areaCode = NULL
	subarea = NULL
	div = NULL
	subdiv = NULL
	
	#process the geometry
	out.sp = NULL
	if(cleanGeom){
		features <- clgeo_Clean(features)
	}
	out.sp <- gUnaryUnion(features)
	if(cleanGeom){
		out.sp <- clgeo_Clean(out.sp)
	}
	
	#area status (officially endorsed vs. draft)
	areaStatus <- 1
	areaCode <- unique(features@data[,area$propertyName])
	if(!is.na(areaCode)){
		if(class(areaCode) == "factor"){
			areaCode <- as.character(areaCode)
			if(substr(areaCode,1,1) == "_"){
				areaStatus <- 0
				areaCode <- substr(areaCode,2,nchar(areaCode))
			}
		}		
	}
	
	#handle attributes
	out.df <- switch(area$levelName,
		"MAJOR" = data.frame(
			"F_LEVEL" = "MAJOR",
			"F_CODE" = areaCode,
			"F_STATUS" = areaStatus,
			"OCEAN" = unique(features@data[,"OCEAN"]),
			"SUBOCEAN" = unique(features@data[,"SUBOCEAN"]),
			"F_AREA" = areaCode,
			"F_SUBAREA" = NA,
			"F_DIVISION" = NA,
			"F_SUBDIVIS" = NA,
			"F_SUBUNIT" = NA,
			stringsAsFactors = FALSE
		),
		"SUBAREA" = data.frame(
			"F_LEVEL" = "SUBAREA",
			"F_CODE" = areaCode,
			"F_STATUS" = areaStatus,
			"OCEAN" = unique(features@data[,"OCEAN"]),
			"SUBOCEAN" = unique(features@data[,"SUBOCEAN"]),
			"F_AREA" = unique(features@data[,"F_AREA"]),
			"F_SUBAREA" = areaCode,
			"F_DIVISION" = NA,
			"F_SUBDIVIS" = NA,
			"F_SUBUNIT" = NA,
			stringsAsFactors = FALSE
		),
		"DIVISION" = data.frame(
			"F_LEVEL" = "DIVISION",
			"F_CODE" = areaCode,
			"F_STATUS" = areaStatus,
			"OCEAN" = unique(features@data[,"OCEAN"]),
			"SUBOCEAN" = unique(features@data[,"SUBOCEAN"]),
			"F_AREA" = unique(features@data[,"F_AREA"]),
			"F_SUBAREA" = unique(features@data[,"F_SUBAREA"]),
			"F_DIVISION" = areaCode,
			"F_SUBDIVIS" = NA,
			"F_SUBUNIT" = NA,
			stringsAsFactors = FALSE
		),
		"SUBDIVISION" = data.frame(
			"F_LEVEL" = "SUBDIVISION",
			"F_CODE" = areaCode,
			"F_STATUS" = areaStatus,
			"OCEAN" = unique(features@data[,"OCEAN"]),
			"SUBOCEAN" = unique(features@data[,"SUBOCEAN"]),
			"F_AREA" = unique(features@data[,"F_AREA"]),
			"F_SUBAREA" = unique(features@data[,"F_SUBAREA"]),
			"F_DIVISION" = unique(features@data[,"F_DIVISION"]),
			"F_SUBDIVIS" = areaCode,
			"F_SUBUNIT" = NA,
			stringsAsFactors = FALSE
		),
		"SUBUNIT" = data.frame(
			"F_LEVEL" = "SUBUNIT",
			"F_CODE" = areaCode,
			"F_STATUS" = areaStatus,
			"OCEAN" = unique(features@data[,"OCEAN"]),
			"SUBOCEAN" = unique(features@data[,"SUBOCEAN"]),
			"F_AREA" = unique(features@data[,"F_AREA"]),
			"F_SUBAREA" = unique(features@data[,"F_SUBAREA"]),
			"F_DIVISION" = unique(features@data[,"F_DIVISION"]),
			"F_SUBDIVIS" = unique(features@data[,"F_SUBDIVIS"]),
			"F_SUBUNIT" = areaCode,
			stringsAsFactors = FALSE
		)
	)
	row.names(out.df) <- as.character(areaCode)
	
	out = NULL
	if(!is.null(out.sp)){
		out.sp <- spChFIDs(out.sp, as.character(areaCode))
		out <- SpatialPolygonsDataFrame(Sr = out.sp, data = out.df, match.ID = TRUE)		
	}
	
	return(out)
}


#area-based function to create new (dissolved) area
dissolveByFisheryArea <- function(area, features, cleanGeom = TRUE){

	#get unique list of codes
	areaCodes <- unique(features@data[,as(area$propertyName,"character")])
	areaCodes <- areaCodes[!is.na(areaCodes)]
	
	#sub-collection
	sp.list <- lapply(areaCodes,
					  function(x){
						subcol <- features[!is.na(features@data[,as(area$propertyName,"character")]) &
										   features@data[,as(area$propertyName,"character")] == x,]
						out <- dissolveFeature(area, subcol, cleanGeom)
						return(out)
					  })
	sp.list <- sp.list[!sapply(sp.list, is.null)]
	out.sp <- do.call("rbind",sp.list)
}

#main function to manage fishery stat areas
manageFisheryStatAreas <- function(features, cleanGeom = TRUE){

	areas <- getFisheryStatAreas()
	sp.list <- lapply(1:nrow(areas),
								function(x){
									area <- areas[x,]
									out <- dissolveByFisheryArea(area, features, cleanGeom)
									return(out)
								})
	sp.list <- sp.list[!sapply(sp.list, is.null)]
	out.sp <- do.call("rbind", sp.list)
	return(out.sp)
}

#main function to erase fishery stat areas
eraseFisheryStatAreas <- function(features, eraser, cleanGeom = TRUE){
	
	if(cleanGeom){
		features <- clgeo_Clean(features)
		eraser <- clgeo_Clean(eraser)
	}
	out.sp <- gDifference(features, eraser, byid = TRUE)
	if(cleanGeom){
		out.sp <- clgeo_Clean(out.sp)
	}
	
	#wrap output as SpatialPolygonsDataFrame object
	out <- NULL
	if(!is.null(out.sp)){
		out.sp <- spChFIDs(out.sp, row.names(features@data))
		areaCRS <- CRS("+proj=eck4 +lon_0=Central Meridian +x_0=False Easting +y_0=False Northing")
		out.df <- cbind(
		  features@data,
		  SURFACE = gArea(spTransform(out.sp, areaCRS)),
		  stringsAsFactors = FALSE)
		row.names(out.df) <- row.names(features@data)
		out <- SpatialPolygonsDataFrame(Sr = out.sp, data = out.df, match.ID = TRUE)
	}
	return(out)
}


#routine
if(TRUE){

	path = "your_path"
	setwd(path)
	data <- readWFS("http://figisapps.fao.org/figis/geoserver.dv.2/fifao/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=fifao:FAO_AREAS_MASTER")
	
	#compute and export 'FAO_AREAS'
	result <- manageFisheryStatAreas(data)
	exportFeatures(result, file.path = path, file.name = "FAO_AREAS")
	
	#compute and export 'FAO_AREAS_ERASE'
	continent <- readWFS("http://figisapps.fao.org/figis/geoserver.dv.2/fifao/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=fifao:UN_CONTINENT2")
	result_erased <- eraseFisheryStatAreas(result, continent)
	exportFeatures(result_erased, file.path = path, file.name = "FAO_AREAS_ERASE")
	
}