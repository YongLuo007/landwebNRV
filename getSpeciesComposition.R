rm(list=ls())

workPath <- "M:/data/kNN"
dir(workPath, pattern = ".tif")
canadaMap <- shapefile("~/GIS DataBase/Canada/Canada.shp")
abie_bal <- raster(file.path(workPath, "Abie_Bal.tif"))

abie_bal <- raster(file.path(workPath, "Abie_Bal.tif"))

crs(abie_bal) <- crs("+proj=longlat +ellps=GRS80 +no_defs")
plot(abie_bal)

canadaMap <- spTransform(canadaMap, crs("+proj=longlat +ellps=GRS80 +no_defs"))

dd <- raster("~/LandWeb/studyareaecoregion.tif")
crs(dd) <- crs("+proj=longlat +ellps=GRS80 +no_defs")
