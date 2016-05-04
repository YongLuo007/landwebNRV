# to generate initial community map based on species

rm(list=ls())
workPath <- "~/LandWeb/KNN_Original"
speciesNames <- c("Abie_Bal", "Abie_Las", "Betu_Pap", "Pice_Gla", "Pice_Mar",
                  "Pinu_Ban", "Pinu_Con", "Pinu_Str", "Popu_Tre")
studyarea <- readRDS("C:/Users/yonluo/Documents/LandWeb/landwebsimplePoly.rds")
for(species in speciesNames){
  speciesMap <- raster(file.path(workPath, paste("NFI_MODIS250m_kNN_Species_", species,
                                                 "_v0.tif", sep = "")))
  projection(speciesMap) <- crs(studyarea)
  speciesMap <- crop(speciesMap, extent(studyarea))
  speciesMap <- mask(speciesMap, studyarea)
  raster::writeRaster(speciesMap, 
                      file.path("~/LandWeb/speciesMap",
                                paste(species, ".tif", sep = "")),
                      overwrite=TRUE)
}
