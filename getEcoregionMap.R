# get ecoregionMap
rm(list = ls())
workPath <- "C:/Users/yonluo/Documents/LandWeb/speciesMapByEcoregion"
ecoregions <- c(3,  4,  5,  6,  9, 12, 14, 15)
ecoregion <- ecoregions[1]
ecoregionRaster <- raster(file.path(workPath,
                                    paste("ecoregion", ecoregion,
                                          "_Abie_Bal.tif",
                                          sep = "")))
ecoregionPoly <- shapefile(file.path(workPath,
                                     paste("ecoregion", ecoregion,
                                           ".shp", sep = "")))
crs(ecoregionPoly) <- projection(ecoregionRaster)
ecoregionRaster <- setValues(ecoregionRaster, ecoregion)
ecoregionMap <- mask(ecoregionRaster, ecoregionPoly)


for(ecoregion in ecoregions[2:8]){
  ecoregionRaster <- raster(file.path(workPath,
                                      paste("ecoregion", ecoregion,
                                            "_Abie_Bal.tif",
                                            sep = "")))
  ecoregionPoly <- shapefile(file.path(workPath,
                                       paste("ecoregion", ecoregion,
                                             ".shp", sep = "")))
  crs(ecoregionPoly) <- projection(ecoregionRaster)
  ecoregionRaster <- setValues(ecoregionRaster, ecoregion)
  ecoregionRaster_sub <- mask(ecoregionRaster, ecoregionPoly)
  ecoregionMap <- merge(ecoregionMap, ecoregionRaster_sub)
}

raster::writeRaster(ecoregionMap, 
                    file.path("~/LandWeb/ecoregionMap.tif"),
                    overwrite=TRUE)



