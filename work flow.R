rm(list=ls())
# all the workflow
# random draw a polygon as a study area
library(dplyr); library(data.table); library(raster); library(sp); library(SpaDES)
canadamap <- shapefile("~/GIS DataBase/Canada/Canada.shp")
dev(4)
clearPlot()
Plot(canadamap)
dataPath <- "~/GitHub/LandWeb/landWebParent/data"
ecoZone <- raster::shapefile(file.path(dataPath, "ecozones.shp"))


severalrandompoints <- clickCoordinates(10)
studyarea <- SpatialPolygons(list(Polygons(list(Polygon(severalrandompoints$coords)), ID = 1)),
                             proj4string = crs(canadamap))

# prepare ecoregion map and initial community map in study area
ecoregionfull <- shapefile("M:/data/ecoFramework/Ecodistricts/ecodistricts.shp")
ecoregionstatus <- data.table(active = "yes",
                              ecoregion = 1:1031)
specieslayersfull <- readRDS("C:/Users/yonluo/Documents/GitHub/landwebNRV/speciesLayersStack.rds")
Pseu_Men <- raster("M:/data/kNN/Original/NFI_MODIS250m_kNN_Species_Pseu_Men_v0.tif")
specieslayersfull <- stack(specieslayersfull, Pseu_Men)
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
# source('~/GitHub/landwebNRV/landwebNRV/R/biomassAttr_IntepFromkNN.R')
# speciesEcoregionTable <- biomassAttr_IntepFromkNN(speciesLayers = specieslayersfull,
#                                                   biomassLayer = biomassmap,
#                                                   percentageCutPoint = 50,
#                                                   intepolateMethod = "IDW",
#                                                   ecoregionMap = simulationMaps$ecoregionMap)


# get the biomass attributs from kNN maps 
biomassmap <- raster("M:/data/kNN/Original/NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif")
samap <- raster("M:/data/kNN/Original/NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif")
source('~/GitHub/landwebNRV/landwebNRV/R/biomassAttributes_kNN.R')
speciesEcoregionTable <- biomassAttributes_kNN(speciesLayers = specieslayersfull,
                                               biomassLayer = biomassmap,
                                               SALayer = samap,
                                               ecoregionMap = simulationMaps$ecoregionMap)


# get the biomass attributs from ground plots

source('~/GitHub/landwebNRV/landwebNRV/R/speciesRelativeAbundance_kNN.R')
speciesSEP <- speciesRelativeAbundance_kNN(ecoregionMap = simulationMaps$ecoregionMap,
                                                speciesLayers = specieslayersfull)


septable <- speciesSEP$speciesAbundanceTable
sepmaps <- speciesSEP$speciesAbundanceMaps
names(septable) <- c("ecoregion", "species", "SEP")


speciesEcoregionTable <- left_join(speciesEcoregionTable, septable, by = c("ecoregion", "species")) %>%
  data.table

speciesEcoregionTable[SEP==0, ':='(maxBiomass = 0, maxANPP = 0)]

NON_NAdata <- speciesEcoregionTable[!is.na(maxBiomass),]

NAdata <- speciesEcoregionTable[is.na(maxBiomass),]

# replace NA values with ecoregion zone value
source('~/GitHub/landwebNRV/landwebNRV/R/biomassAttributes_kNN_biggerEcoAddition.R')

dd <- biomassAttributes_kNN_biggerEcoAddition(speciesLayers = specieslayersfull,
                                              biomassLayer = biomassmap,
                                              SALayer = samap,
                                              ecoregionMap = simulationMaps$ecoregionMap,
                                              biggerEcoMap = shapefile("M:/data/ecoFramework/Ecoregions/ecoregions.shp"),
                                              NAData = NAdata)

names(NON_NAdata)
biggerEcoMap<- shapefile("M:/data/ecoFramework/Ecoregions/ecoregions.shp")
NON_NAdata <- rbind(NON_NAdata, dd$addData[!is.na(maxBiomass), .(ecoregion, species, maxBiomass, maxANPP, SEP)])

NAdata <- dd$addData[is.na(maxBiomass),.(ecoregion, species, maxBiomass, maxANPP, SEP)]

biggerEcoMap1<- shapefile("M:/data/ecoFramework/Ecozones/ecozones.shp")

names(biggerEcoMap1@data)[grep("ECOZONE",names(biggerEcoMap1@data))] <- "ECOREGION"
source('~/GitHub/landwebNRV/landwebNRV/R/biomassAttributes_kNN_biggerEcoAddition.R')
ecozoneValues <- biomassAttributes_kNN_biggerEcoAddition(speciesLayers = specieslayersfull,
                                              biomassLayer = biomassmap,
                                              SALayer = samap,
                                              ecoregionMap = simulationMaps$ecoregionMap,
                                              biggerEcoMap = biggerEcoMap1,
                                              NAData = NAdata)



