# summary patch based on size bigger than 5000ha, 4000ha, and 3000ha
rm(list=ls())
workingPatch <- "M:/data/LandWeb/BorealPlain simulation/outputs"
source('~/GitHub/landwebNRV/landwebNRV/R/postSimuSummary_Patch.R')
ecoDisFull <- shapefile("M:/data/ecoFramework/Ecodistricts/ecodistricts.shp")
summaryMaps <- readRDS(file.path(workingPatch, "seralStageMapStackRep1.rds"))
ecoDisFull <- spTransform(ecoDisFull,crs(summaryMaps))
ecoDis <- crop(ecoDisFull, summaryMaps)
regions <- c(139, 149)
contents <- c("seralStage", "vegType")
allfreqTable <- data.table(content = character(),
                        ecoregion = numeric(),
                        rep = numeric(),
                        patchClasses = character(),
                        year = numeric(),
                        landType = numeric(),
                        NofPatch = numeric())
for(content1 in contents){
  for(region in regions){
    summaryArea <- ecoDis[ecoDis$ECOREGION == region,]
    replicates <- 1:10
    for(rp in replicates){
      summayMapss <- readRDS(file.path(workingPatch,
                                       paste(content1, "MapStackRep", rp, ".rds", sep = "")))
      summaryPatch <-  postSimuSummary_Patch(focalArea = summaryArea,
                                       summaryMaps = summayMapss,
                                       patchClass = c(1000, 3000, 5000))
      freqTable <- summaryPatch$freqTable[,':='(content = content1,
                                                ecoregion = region,
                                                rep = rp) ]
      freqTable <- freqTable[,.(content, ecoregion, rep, patchClasses,
                                year, landType, NofPatch)]
      allfreqTable <- rbind(allfreqTable, freqTable)
      saveRDS(summaryPatch$clumppedMaps,
              file.path(workingPatch,
                        paste("patches_", content1, "_region_",
                              region, "_rep_", rp, ".rds", sep = "")))
    }
  }
}

write.csv(allfreqTable, file.path(workingPatch, "Patchsummary.csv"), row.names = FALSE)

