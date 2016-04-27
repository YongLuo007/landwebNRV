rm(list=ls())
source('~/GitHub/landwebNRV/landwebNRV/R/ecoregionClassification.R')

studyarea <- readRDS("C:/Users/yonluo/Documents/LandWeb/landwebsimplePoly.rds") 
ecoregionMap <- rgdal::readOGR("M:/data/Ecozones/ecozones.shp",
                               layer = "ecozones")

# this is the first function
dd <- ecoregionClassification(studyAreaMap = studyarea,
                              ecoregionMap = ecoregionMap,
                              cellSize = 100)
if(file.exists("~/GitHub/landwebNRV/studyareaecoregion.tif")){
  file.remove("~/GitHub/landwebNRV/studyareaecoregion.tif")
}
raster::writeRaster(dd$studyareaecoregion, "~/GitHub/landwebNRV/studyareaecoregion.tif",
                    overwrite=TRUE)
w <- rgdal::readGDAL("~/GitHub/landwebNRV/studyareaecoregion.tif")
rgdal::writeGDAL(w, "~/GitHub/landwebNRV/studyareaecoregion.tif",
                 drivername = "GTiff", type = "Int32", mvFlag = 0)


studyareaecoregion <- raster("~/GitHub/landwebNRV/studyareaecoregion.tif")

source('~/GitHub/landwebNRV/landwebNRV/R/plotsByEcoregion.R')
load("~/GitHub/landwebNRV/pureStand.RData")
pureStandHeader <- allPlotHeaderData[MeasureID %in% pureStand$MeasureID,]

pureStandbyEcoregion <- plotsByEcoregion(plotLocation = pureStandHeader, studyAreaEcoregionMap = studyareaecoregion)


