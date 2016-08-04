rm(list = ls())
workingPath <- "~/LandWeb/BorealPlain"
summaryContents <- c("seralStage", "vegType")
ecoDisFull <- shapefile("M:/data/ecoFramework/Ecodistricts/ecodistricts.shp")
summaryMaps <- readRDS(file.path(workingPath, "seralStageMapStackRep1.rds"))
ecoDisFull <- spTransform(ecoDisFull, crs(summaryMaps))
ecoDis <- crop(ecoDisFull, summaryMaps[[1]])
rm(summaryMaps)
source('~/GitHub/landwebNRV/landwebNRV/R/postSimuSummary.R')
# please note that ecoregion could be any geo area such as FMA
output <- data.table(content = character(), ecoregion = numeric(),
                     replicate = numeric(), year = numeric(), type = numeric(),
                     NofCell = numeric(), totalCell = numeric(), 
                     areaHa = numeric(), areaPercentage = numeric())
ecoregions <- sort(unique(ecoDis$ECOREGION))
for(summaryContent in summaryContents){
  for(ecoregion in ecoregions){
    focalRegion <- ecoDis[ecoDis$ECOREGION == ecoregion, ]
    focalRegion <- SpatialPolygons(focalRegion@polygons, proj4string = crs(focalRegion))
    for(i in 1:10){ # for replication
      summaryMaps <- readRDS(file.path(workingPath,
                                       paste(summaryContent,
                                             "MapStackRep", i, ".rds", sep = "")))
      summaryTable <- postSimuSummary(focalArea = focalRegion,
                                     summaryMaps = summaryMaps)
      summaryTable[,':='(content = summaryContent,
                         ecoregion = ecoregion,
                         replicate = i)]
      summaryTable <- summaryTable[,.(content, ecoregion, replicate, year, type, NofCell,
                                      totalCell, areaHa, areaPercentage)]
      output <- rbind(output, summaryTable)
      
    }
    
  }
}

