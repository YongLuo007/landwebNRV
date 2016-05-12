rm(list=ls())
library(dplyr); library(data.table); library(raster); library(sp); library(SpaDES)
regions <- c("BC", "BSW", "BP", "MC", "WestON")
maxBiomassOutput <- data.table(district = character(),
                               species = character(),
                               maxBiomass = numeric())
maxANPPOutput <- data.table(district = character(),
                            species = character(),
                            maxANPP = numeric())
SEPOutput <- data.table(district = character(),
                        species = character(),
                        SEP = numeric())

for(region in regions){
  load(file.path("~/GitHub/PicusToLandisIIBiomassSuccession/processedOutputs",
                 paste("growthParam_", region, ".RData", sep = "")))
  maxBiomassTable <- growthParam$Baseline$Baseline$maxBiomass
  maxBiomassTable <- data.frame(t(maxBiomassTable))
  speciesNames <- colnames(maxBiomassTable)
  maxBiomassTable$district <- substr(row.names(maxBiomassTable),
                                     1, 
                                     nchar(row.names(maxBiomassTable))-1)
  
  maxBiomassTable <- data.table(maxBiomassTable)
  maxBiomassTable[maxBiomassTable==0] <- NA
  maxBiomassTableLong <- data.table::melt(maxBiomassTable, id.vars = "district",
                                          variable.name = "species",
                                          value.name = "maxBiomass")
  maxBiomassTable <- maxBiomassTableLong[,.(maxBiomass = mean(maxBiomass, na.rm = TRUE)),
                                         by = c("district", "species")]
  maxBiomassTable <- maxBiomassTable[maxBiomass != "NaN",]
  maxBiomassOutput <- rbind(maxBiomassOutput, maxBiomassTable)
  
  
  maxANPPTable <- growthParam$Baseline$Baseline$maxANPP
  maxANPPTable <- data.frame(t(maxANPPTable))
  speciesNames <- colnames(maxANPPTable)
  maxANPPTable$district <- substr(row.names(maxANPPTable),
                                  1, 
                                  nchar(row.names(maxANPPTable))-1)
  
  maxANPPTable <- data.table(maxANPPTable)
  maxANPPTable[maxANPPTable==0] <- NA
  maxANPPTableLong <- data.table::melt(maxANPPTable, id.vars = "district",
                                       variable.name = "species",
                                       value.name = "maxANPP")
  maxANPPTable <- maxANPPTableLong[,.(maxANPP = mean(maxANPP, na.rm = TRUE)),
                                   by = c("district", "species")]
  maxANPPTable <- maxANPPTable[maxANPP != "NaN",]
  maxANPPOutput <- rbind(maxANPPOutput, maxANPPTable)
  
  load(file.path("~/GitHub/PicusToLandisIIBiomassSuccession/processedOutputs",
                 paste("sep_", region, ".RData", sep = "")))
  SEPTable <- pEst$Baseline$Baseline
  
  
  SEPTable <- data.frame(t(SEPTable))
  speciesNames <- colnames(SEPTable)
  SEPTable$district <- substr(row.names(SEPTable),
                              1, 
                              nchar(row.names(SEPTable))-1)
  
  SEPTable <- data.table(SEPTable)
  SEPTable[SEPTable==0] <- NA
  SEPTableLong <- data.table::melt(SEPTable, id.vars = "district",
                                   variable.name = "species",
                                   value.name = "SEP")
  SEPTable <- SEPTableLong[,.(SEP = mean(SEP, na.rm = TRUE)),
                           by = c("district", "species")]
  SEPTable <- SEPTable[SEP != "NaN",]
  SEPOutput <- rbind(SEPOutput, SEPTable)
}
maxBiomassOutput[,':='(district = as.numeric(district),
                       species = as.character(species),
                       maxBiomass = as.numeric(maxBiomass))]
maxANPPOutput[,':='(district = as.numeric(district),
                    species = as.character(species),
                    maxANPP = as.numeric(maxANPP))]
SEPOutput[,':='(district = as.numeric(district),
                species = as.character(species),
                SEP = as.numeric(SEP))]


# 
# canadamap <- shapefile("~/GIS DataBase/Canada/Canada.shp")
# dev(4)
# clearPlot()
# Plot(canadamap)
# severalrandompoints <- clickCoordinates(10)
# studyarea <- SpatialPolygons(list(Polygons(list(Polygon(severalrandompoints$coords)), ID = 1)),
#                              proj4string = crs(canadamap))

# studyarealarge <- canadamap[canadamap$NAME %in% c("Saskatchewan", "Alberta", "British Columbia", "Manitoba", "Ontario", "Northwest Territories"),]
lcc2005 <- raster("M:/data/LandCoverOfCanada2005_V1_4/LCC2005_V1_4a.tif")
lcc2005 <- raster(lcc2005)
studyarea<- readRDS("~/GitHub/landwebNRV/largePolygons.rds")
studyarealargePoly <- spTransform(studyarea, crs(lcc2005))


ecoregionfull <- shapefile("M:/data/ecoFramework/Ecodistricts/ecodistricts.shp")
ecoregionfull <- spTransform(ecoregionfull, crs(lcc2005))
ecoregionInLargeArea <- intersect(ecoregionfull, studyarealargePoly)
studyarealargeRaster <- crop(lcc2005, ecoregionInLargeArea)
# for maxBiomass map
maxBiomassMaps <- stack()
specieaNames <- unique(maxBiomassOutput$species)
for(individualSpecies in specieaNames){
  studyarealargeRasterBySpecies <- setValues(studyarealargeRaster, 0)
  ecoregions <- unique(maxBiomassOutput$district)
  ecoregions <- ecoregionInLargeArea$ECODISTRIC[ecoregionInLargeArea$ECODISTRIC %in% ecoregions]
  for(ecoregion in ecoregions){
    assignedB <- maxBiomassOutput[species == individualSpecies &
                                    district == ecoregion,]$maxBiomass
    if(length(assignedB)==1){
      ecoregionIndividualMap <- ecoregionInLargeArea[ecoregionInLargeArea$ECODISTRIC == ecoregion,]
      studyarealargeRasterBySpeciesAdd <- setValues(studyarealargeRaster, maxBiomassOutput[species == individualSpecies &
                                                                                             district == ecoregion,]$maxBiomass)
      studyarealargeRasterBySpeciesAdd <- suppressWarnings(mask(studyarealargeRasterBySpeciesAdd, ecoregionIndividualMap))
      studyarealargeRasterBySpeciesAdd[is.na(studyarealargeRasterBySpeciesAdd)] <- 0
      studyarealargeRasterBySpecies <- studyarealargeRasterBySpecies + studyarealargeRasterBySpeciesAdd
    }
  }
  xy <- data.frame(xyFromCell(studyarealargeRasterBySpecies, 1:ncell(studyarealargeRasterBySpecies)))
  xy$v <- getValues(studyarealargeRasterBySpecies)
  xy <- xy[which(!is.na(xy$v)),]
  mg <- gstat::gstat(id = "v", formula = v~1,
                     locations = ~x+y, data = xy)
  maxBiomassRaster <- raster(studyarealargeRasterBySpecies)
  maxBiomassRaster <- interpolate(maxBiomassRaster, mg)
  
  
  names(maxBiomassRaster) <- individualSpecies
  maxBiomassMaps <- stack(maxBiomassMaps, maxBiomassRaster)
}

save.image("~/LandWeb/maxBiomassYanInterpolated.RData")



rm(speciesNames, inidividualSpecies, studyarealargeRasterBySpecies,
   ecoregion, assignedB, ecoregionIndividualMap, studyarealargeRasterBySpeciesAdd, 
   maxBiomassRaster)


# for maxANPP
maxANPPMaps <- stack()
specieaNames <- unique(maxANPPOutput$species)
for(individualSpecies in specieaNames){
  studyarealargeRasterBySpecies <- setValues(studyarealargeRaster, 0)
  ecoregions <- unique(maxBiomassOutput$district)
  ecoregions <- ecoregionInLargeArea$ECODISTRIC[ecoregionInLargeArea$ECODISTRIC %in% ecoregions]
  for(ecoregion in ecoregions){
    assignedB <- maxANPPOutput[species == individualSpecies &
                                 district == ecoregion,]$maxANPP
    if(length(assignedB)==1){
      ecoregionIndividualMap <- ecoregionInLargeArea[ecoregionInLargeArea$ECODISTRIC == ecoregion,]
      studyarealargeRasterBySpeciesAdd <- setValues(studyarealargeRaster, maxANPPOutput[species == individualSpecies &
                                                                                          district == ecoregion,]$maxANPP)
      studyarealargeRasterBySpeciesAdd <- suppressWarnings(mask(studyarealargeRasterBySpeciesAdd, ecoregionIndividualMap))
      studyarealargeRasterBySpeciesAdd[is.na(studyarealargeRasterBySpeciesAdd)] <- 0
      studyarealargeRasterBySpecies <- studyarealargeRasterBySpecies + studyarealargeRasterBySpeciesAdd
    }
  }
  xy <- data.frame(xyFromCell(studyarealargeRasterBySpecies, 1:ncell(studyarealargeRasterBySpecies)))
  xy$v <- getValues(studyarealargeRasterBySpecies)
  xy <- xy[which(!is.na(xy$v)),]
  mg <- gstat::gstat(id = "v", formula = v~1,
                     locations = ~x+y, data = xy)
  maxANPPRaster <- raster(studyarealargeRasterBySpecies)
  maxANPPRaster <- interpolate(maxANPPRaster, mg)
  
  
  names(maxANPPRaster) <- individualSpecies
  maxANPPMaps <- stack(maxANPPMaps, maxANPPRaster)
}

save.image("~/LandWeb/maxANPPYanInterpolated.RData")



rm(speciesNames, inidividualSpecies, studyarealargeRasterBySpecies,
   ecoregion, assignedB, ecoregionIndividualMap, studyarealargeRasterBySpeciesAdd, 
   maxANPPRaster)


# for SEP
SEPMaps <- stack()
specieaNames <- unique(SEPOutput$species)
for(individualSpecies in specieaNames){
  studyarealargeRasterBySpecies <- setValues(studyarealargeRaster, 0)
  ecoregions <- unique(maxBiomassOutput$district)
  ecoregions <- ecoregionInLargeArea$ECODISTRIC[ecoregionInLargeArea$ECODISTRIC %in% ecoregions]
  for(ecoregion in ecoregions){
    assignedB <- SEPOutput[species == individualSpecies &
                             district == ecoregion,]$SEP
    if(length(assignedB)==1){
      ecoregionIndividualMap <- ecoregionInLargeArea[ecoregionInLargeArea$ECODISTRIC == ecoregion,]
      studyarealargeRasterBySpeciesAdd <- setValues(studyarealargeRaster, SEPOutput[species == individualSpecies &
                                                                                      district == ecoregion,]$SEP)
      studyarealargeRasterBySpeciesAdd <- suppressWarnings(mask(studyarealargeRasterBySpeciesAdd, ecoregionIndividualMap))
      studyarealargeRasterBySpeciesAdd[is.na(studyarealargeRasterBySpeciesAdd)] <- 0
      studyarealargeRasterBySpecies <- studyarealargeRasterBySpecies + studyarealargeRasterBySpeciesAdd
    }
  }
  xy <- data.frame(xyFromCell(studyarealargeRasterBySpecies, 1:ncell(studyarealargeRasterBySpecies)))
  xy$v <- getValues(studyarealargeRasterBySpecies)
  xy <- xy[which(!is.na(xy$v)),]
  mg <- gstat::gstat(id = "v", formula = v~1,
                     locations = ~x+y, data = xy)
  SEPRaster <- raster(studyarealargeRasterBySpecies)
  SEPRaster <- interpolate(SEPRaster, mg)
  
  
  names(SEPRaster) <- individualSpecies
  SEPMaps <- stack(SEPMaps, SEPRaster)
}

save.image("~/LandWeb/SEPYanInterpolated.RData")



rm(speciesNames, inidividualSpecies, studyarealargeRasterBySpecies,
   ecoregion, assignedB, ecoregionIndividualMap, studyarealargeRasterBySpeciesAdd, 
   SEPRaster)





