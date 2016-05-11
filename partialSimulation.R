rm(list=ls())
setwd("C:/Users/yonluo/Documents/GitHub/landwebNRV")
# these codes to divide study area into several ecozones
# and for each ecozone, ecoregion is defined by ecoDistrict
library(dplyr); library(data.table); library(raster); library(sp); library(SpaDES)
studyarea <- readRDS("C:/Users/yonluo/Documents/LandWeb/landwebsimplePoly.rds") 

ecozones <- shapefile("M:/data/Ecozones/ecozones.shp")
studyarea <- spTransform(studyarea, crs(ecozones))
studyareaEcozone <- raster::intersect(ecozones, studyarea)
ecozones <- sort(unique(studyareaEcozone@data$ZONE_NAME))
ecoDistricts <- shapefile("M:/data/ecoFramework/Ecodistricts/ecodistricts.shp")
activeStatus <- data.table(active = "yes",
                           ecoregion = 1:1031)
# allData <- list()
# i <- 1
specieslayers <- readRDS("C:/Users/yonluo/Documents/GitHub/landwebNRV/speciesLayersStack.rds")
speciesnames <- names(specieslayers)
for(ecozone in ecozones[2:2]){
  studyarea_sub <- studyareaEcozone[studyareaEcozone@data$ZONE_NAME == ecozone,]
  studyarea_sub <- SpatialPolygons(studyarea_sub@polygons, proj4string = studyarea_sub@proj4string)
  source('~/GitHub/landwebNRV/landwebNRV/R/initialCommunityMapProducer_kNN.R')
  initialCommFiles <- initialCommunityMapProducer_kNN(speciesLayers = specieslayers, 
                                                      speciesNames = speciesnames,
                                                      studyArea = studyarea_sub)

  source('~/GitHub/landwebNRV/landwebNRV/R/ecoregionMapProducer.R')
  ecoregionFiles <- ecoregionMapProducer(studyAreaRaster = initialCommFiles$initialCommunityMap,
                                         ecoregionMapFull = ecoDistricts,
                                         ecoregionName = "ECODISTRIC",
                                         ecoregionActiveStatus = activeStatus,
                                         studyArea = studyarea_sub)
  lcc2005 <- raster("M:/data/LandCoverOfCanada2005_V1_4/LCC2005_V1_4a.tif")
  activeStatusTable <- data.table(active = c(rep("yes", 15), rep("no", 25)),
                                  mapcode = 1:40)  # this is based on description
  source('~/GitHub/landwebNRV/landwebNRV/R/nonactiveEcoFromRaster.R')
  alldataAdded<- nonactiveEcoFromRaster(nonactiveRaster = lcc2005,
                                        activeStatus = activeStatusTable,
                                        ecoregionMap = ecoregionFiles$ecoregionMap,
                                        ecoregion = ecoregionFiles$ecoregion,
                                        initialCommunityMap = initialCommFiles$initialCommunityMap,
                                        initialCommunity = initialCommFiles$initialCommunity)
  # allData[[i]] <- alldataAdded
  # i <- i+1
}


names(allData) <- ecozones

