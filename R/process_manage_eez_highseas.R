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
