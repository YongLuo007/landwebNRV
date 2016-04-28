rm(list = ls())
load("~/GitHub/landwebNRV/pureStand.RData")
pureStandHeader <- allPlotHeaderDataLongLat[MeasureID %in% pureStand$MeasureID,]

# pureStandbyEcoregion <- plotsByEcoregion(plotLocation = pureStandHeader, studyAreaEcoregionMap = studyareaecoregion)

canadaMap <- shapefile("~/GIS DataBase/Canada/Canada.shp")
studyarea <- readRDS("C:/Users/yonluo/Documents/LandWeb/landwebsimplePoly.rds") 
ecoregionMap <- rgdal::readOGR("M:/data/Ecozones/ecozones.shp",
                               layer = "ecozones")
studyarea <- spTransform(studyarea, crs(ecoregionMap))

studyareawithecoregion <- intersect(ecoregionMap, studyarea)

plotLocation <- SpatialPointsDataFrame(pureStandHeader[,.(Longitude, Latitude)],
                                       pureStandHeader[,.(MeasureID)],
                                       match.ID = TRUE,
                                       proj4string = crs(ecoregionMap))
plotLocationwithecoregion <- intersect(plotLocation, studyareawithecoregion)
canadaMap <- spTransform(canadaMap, crs(ecoregionMap))
library(SpaDES)
dev(4)
clearPlot()
plot(canadaMap, border = "grey")
plot(ecoregionMap, border = "green", add=T)
plot(studyarea, border = "red", add = T)
plot(plotLocation, add = T)
plot(plotLocationwithecoregion, col = "red", add = T)
