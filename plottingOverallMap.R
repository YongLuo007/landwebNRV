rm(list = ls())
ecoregionMap <- raster("~/LandWeb/ecoregionMap.tif")


initialCommunitiesMap <- raster("~/LandWeb/initialCommunities.tif")
projection(initialCommunitiesMap) <- projection(ecoregionMap)

# NApixels <- Which(is.na(initialCommunitiesMap), cells = TRUE, na.rm = FALSE)
# ecoregionMap[NApixels] <- NA
# writeRaster(initialCommunitiesMap,
#             file.path("~/LandWeb/initialCommunities1.tif"),
#             overwrite = TRUE)

canadaMap <- shapefile("~/GIS DataBase/Canada/Canada.shp")
canadaMap <- spTransform(canadaMap, crs(ecoregionMap))

studyarea <- readRDS("C:/Users/yonluo/Documents/LandWeb/landwebsimplePoly.rds") 
studyarea <- spTransform(studyarea, crs(ecoregionMap))

ecoregionMapAll <- rgdal::readOGR("M:/data/Ecozones/ecozones.shp",
                               layer = "ecozones")
ecoregionMapAll <- spTransform(ecoregionMapAll, crs(ecoregionMap))

ecodistrictMapAll <- rgdal::readOGR("M:/data/ecoFramework/Ecodistricts/ecodistricts.shp",
                                  layer = "ecodistricts")
ecodistrictMapAll <- spTransform(ecodistrictMapAll, crs(ecoregionMap))
ecodistrictMapSmallPoly <- crop(ecodistrictMapAll, newExt)

library(SpaDES)
dev(4)
clearPlot()
plot(canadaMap, border = "grey")
plot(ecoregionMapAll, border = "green", add=T)
plot(studyarea, border = "red", add = T)

dev(5)
Plot(ecoregionMap, initialCommunitiesMap)

