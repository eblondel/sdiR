#
# Easy script to derivate highseas from EEZs
#
# @note in preparation
# @author eblondel
# @date 2016/01/26
#

require(RFigisGeo)
require(cleangeo)

#read and clean EEZ data
sp1 <- readWFS("http://geo.vliz.be/geoserver/MarineRegions/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=MarineRegions:eez_land")
sp2 <- clgeo_Clean(sp1)

#build a world extent spatial polygon
p <- SpatialPolygons(
	list(Polygons(
		list(Polygon(data.frame(
		x=c(-180,-180,180,180,-180),
		y=c(-90,90,90,-90,-90)))
		), 
		ID = "1")
	))
proj4string(p) <- proj4string(sp2)

#difference
hs <- gDifference(p, sp2)
hsp <- SpatialPolygonsDataFrame(hs, data = data.frame(mrgid = "0", name = "no eez (high seas)", category = "HIGHSEA", stringsAsFactors = FALSE))

#eez data
eez <- readWFS("http://geo.vliz.be/geoserver/MarineRegions/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=MarineRegions:eez")
eez@data <- cbind(eez@data[,c("mrgid","eez")], category = "EEZ")
names(eez@data) <- c("mrgid", "name", "category")

#output
out <- spRbind(hsp, eez)
out <- clgeo_Clean(out)

#export
exportFeatures(out, file.path = "your path", file.name = "EEZ_HIGHSEAS")
