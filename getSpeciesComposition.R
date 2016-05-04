rm(list=ls())
workPath <- "~/LandWeb/KNN_Original"
biomassMap <- raster(file.path(workPath, "NFI_MODIS250m_kNN_Structure_Biomass_TotalLiveAboveGround_v0.tif"))
ageMap <- raster(file.path(workPath, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif"))

studyarea <- readRDS("C:/Users/yonluo/Documents/LandWeb/landwebsimplePoly.rds") 
ecoregionMap <- rgdal::readOGR("M:/data/Ecozones/ecozones.shp",
                               layer = "ecozones")
studyarea <- spTransform(studyarea, crs(ecoregionMap))

studyareawithecoregionMap <- intersect(ecoregionMap, studyarea)
ecoregions <- unique(studyareawithecoregionMap$ECOZONE)
studyareawithecoregionMap <- spTransform(studyareawithecoregionMap, crs(biomassMap))

for (ecoregion in ecoregions){
  individualecoregionMap <- studyareawithecoregionMap[studyareawithecoregionMap$ECOZONE == ecoregion, ]
  speciesNames <- c("Abie_Bal", "Abie_Las", "Betu_Pap", "Pice_Gla", "Pice_Mar",
                    "Pinu_Ban", "Pinu_Con", "Pinu_Str", "Popu_Tre")
  biomassMap_sub <- crop(biomassMap, extent(individualecoregionMap))
  biomassMap_sub <- mask(biomassMap_sub, individualecoregionMap)
  
  ageMap_sub <- crop(ageMap, extent(individualecoregionMap))
  ageMap_sub <- mask(ageMap_sub, individualecoregionMap)
  raster::writeRaster(biomassMap_sub, 
                      file.path("~/LandWeb/speciesMapByEcoregion",
                                paste("ecoregion",ecoregion, "_Biomass.tif", sep = "")),
                      overwrite=TRUE)
  raster::writeRaster(ageMap_sub, 
                      file.path("~/LandWeb/speciesMapByEcoregion",
                                paste("ecoregion",ecoregion, "_SA.tif", sep = "")),
                      overwrite=TRUE)
  for(species in speciesNames){
    speciesMap <- raster(file.path(workPath,
                                   paste("NFI_MODIS250m_kNN_Species_", species,
                                         "_V0.tif", sep = "")))
    projection(speciesMap) <- crs(studyareawithecoregionMap)
    speciesMap <- crop(speciesMap, extent(individualecoregionMap))
    speciesMap <- mask(speciesMap, individualecoregionMap)
    
    raster::writeRaster(speciesMap, 
                        file.path("~/LandWeb/speciesMapByEcoregion",
                                  paste("ecoregion",ecoregion, "_",species, ".tif", sep = "")),
                        overwrite=TRUE)
  }
}
