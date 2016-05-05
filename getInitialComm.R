rm(list=ls())
library(data.table)
library(raster)
speciesNames <- c("Abie_Bal", "Abie_Las", "Betu_Pap", "Pice_Gla", "Pice_Mar",
                  "Pinu_Ban", "Pinu_Con", "Pinu_Str", "Popu_Tre")
speciesComMap <- raster(file.path("~/LandWeb/speciesMap",
                                  paste(speciesNames[1], ".tif", sep = "")))
speciesComMap <- as.logical(speciesComMap)
speciesComMap[Which(is.na(speciesComMap), cells = TRUE, na.rm = FALSE)] <- 0
k <- 1
for(species in speciesNames[2:length(speciesNames)]){
  speciesMap <- raster(file.path("~/LandWeb/speciesMap",
                                 paste(species, ".tif", sep = "")))
  speciesMap <- as.logical(speciesMap)
  speciesMap[Which(is.na(speciesMap), cells = TRUE, na.rm = FALSE)] <- 0
  speciesComMap <- speciesMap*(10^k)+speciesComMap
  k <- k+1
}
# set the non-forested area as NA
speciesComMap[Which(speciesComMap==0, cells = TRUE, na.rm = FALSE)] <- NA

raster::writeRaster(speciesComMap, 
                    file.path("~/LandWeb/initialCommunities.tif"),
                    overwrite=TRUE)

initialCommunities <- data.table(mapCode=sort(unique(getValues(speciesComMap))))
initialCommunities[,mapCodeStr:=as.character(mapCode)]


initialCommunities[, NofStr:=nchar(mapCodeStr)]
for(i in 1:8){
  initialCommunities[NofStr==i, mapCodeFull:=paste(paste(rep("0",(length(speciesNames)-i)),
                                                         collapse = ""),
                                                   mapCodeStr,
                                                   sep = "")]
}
initialCommunities[NofStr==length(speciesNames), mapCodeFull:=mapCodeStr]


output <- data.table(mapCode = numeric(), speciesPresence = character(),
                     species = character())
for(i in 1:nrow(initialCommunities)){
  outputAdd <- data.table(mapCode = initialCommunities$mapCode[i],
                          speciesPresence = substring(initialCommunities$mapCodeFull[i],
                                                      seq(1, length(speciesNames), 1),
                                                      seq(1, length(speciesNames), 1)),
                          species = speciesNames[length(speciesNames):1])
  
  output <- rbind(output, outputAdd)
}

initialCommunities <- output[speciesPresence!="0",]
initialCommunities[,newMapCode:=as.numeric(as.factor(mapCode))]


mapcodeconnection <- unique(initialCommunities[,.(mapCode, newMapCode)], by = "mapCode")

ncell(speciesComMap)

indexTable <- data.table(pixelIndex=1:ncell(speciesComMap),
                         mapCode=getValues(speciesComMap))
indexTable <- indexTable[!is.na(mapCode),]
indexTable <- setkey(indexTable, mapCode)[setkey(mapcodeconnection, mapCode),
                                          nomatch = 0]
speciesComMap[indexTable$pixelIndex] <- indexTable$newMapCode

initialCommunities[, ':='(mapCode = newMapCode, newMapCode = NULL, speciesPresence = NULL)]
write.csv(initialCommunities, "initialCommunities.csv", row.names = FALSE)


activeRegion <- raster("~/LandWeb/activeRegion_studyarea.tif")
speciesComMap[Which(activeRegion==0, cells = TRUE)] <- NA

raster::writeRaster(speciesComMap, 
                    file.path("~/LandWeb/initialCommunities.tif"),
                    overwrite=TRUE)



