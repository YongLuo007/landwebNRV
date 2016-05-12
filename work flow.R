rm(list=ls())
# all the workflow
# random draw a polygon as a study area
library(dplyr); library(data.table); library(raster); library(sp); library(SpaDES)
canadamap <- shapefile("~/GIS DataBase/Canada/Canada.shp")
dev(4)
clearPlot()
Plot(canadamap)
severalrandompoints <- clickCoordinates(10)
studyarea <- SpatialPolygons(list(Polygons(list(Polygon(severalrandompoints$coords)), ID = 1)),
                             proj4string = crs(canadamap))

# prepare ecoregion map and initial community map in study area
ecoregionfull <- shapefile("M:/data/ecoFramework/Ecodistricts/ecodistricts.shp")
ecoregionstatus <- data.table(active = "yes",
                              ecoregion = 1:1031)
specieslayersfull <- readRDS("C:/Users/yonluo/Documents/GitHub/landwebNRV/speciesLayersStack.rds")
speciesnames <- names(specieslayersfull)

source('~/GitHub/landwebNRV/landwebNRV/R/initialCommunityMapProducer_kNN.R')
initialCommFiles <- initialCommunityMapProducer_kNN(speciesLayers = specieslayersfull, 
                                                    speciesPresence = 50,
                                                    studyArea = studyarea)

source('~/GitHub/landwebNRV/landwebNRV/R/ecoregionMapProducer.R')
ecoregionFiles <- ecoregionMapProducer(studyAreaRaster = initialCommFiles$initialCommunityMap,
                                       ecoregionMapFull = ecoregionfull,
                                       ecoregionName = "ECODISTRIC",
                                       ecoregionActiveStatus = ecoregionstatus,
                                       studyArea = studyarea)


lcc2005 <- raster("M:/data/LandCoverOfCanada2005_V1_4/LCC2005_V1_4a.tif")
activeStatusTable <- data.table(active = c(rep("yes", 15), rep("no", 25)),
                                mapcode = 1:40)  # this is based on description
source('~/GitHub/landwebNRV/landwebNRV/R/nonactiveEcoFromRaster.R')
simulationMaps <- nonactiveEcoFromRaster(nonactiveRaster = lcc2005,
                                         activeStatus = activeStatusTable,
                                         ecoregionMap = ecoregionFiles$ecoregionMap,
                                         ecoregion = ecoregionFiles$ecoregion,
                                         initialCommunityMap = initialCommFiles$initialCommunityMap,
                                         initialCommunity = initialCommFiles$initialCommunity)
biomassmap <- raster("M:/data/kNN/Original/NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif")
source('~/GitHub/landwebNRV/landwebNRV/R/biomassAttr_IntepFromkNN.R')
speciesEcoregionTable <- biomassAttr_IntepFromkNN(speciesLayers = specieslayersfull,
                                                  biomassLayer = biomassmap,
                                                  percentageCutPoint = 50,
                                                  intepolateMethod = "IDW",
                                                  ecoregionMap = simulationMaps$ecoregionMap)


# get the biomass attributs from kNN maps 
biomassmap <- raster("M:/data/kNN/Original/NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif")
samap <- raster("M:/data/kNN/Original/NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif")
source('~/GitHub/landwebNRV/landwebNRV/R/biomassAttributes_kNN.R')
speciesEcoregionTable <- biomassAttributes_kNN(speciesLayers = specieslayersfull,
                                               biomassLayer = biomassmap,
                                               SALayer = samap,
                                               ecoregionMap = simulationMaps$ecoregionMap)


# get the biomass attributs from ground plots


























