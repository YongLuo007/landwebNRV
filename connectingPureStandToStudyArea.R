
rm(list=ls())

source('~/GitHub/landwebNRV/landwebNRV/R/plotsByEcoregion.R')
load("~/GitHub/landwebNRV/pureStand.RData")
pureStandHeader <- allPlotHeaderData[MeasureID %in% pureStand$MeasureID,]

# pureStandbyEcoregion <- plotsByEcoregion(plotLocation = pureStandHeader, studyAreaEcoregionMap = studyareaecoregion)


studyarea <- readRDS("C:/Users/yonluo/Documents/LandWeb/landwebsimplePoly.rds") 
ecoregionMap <- rgdal::readOGR("M:/data/Ecozones/ecozones.shp",
                               layer = "ecozones")
studyarea <- spTransform(studyarea, crs(ecoregionMap))

studyareawithecoregion <- intersect(ecoregionMap, studyarea)

plotsbyEcoregion <- plotsByEcoregion(plotLocation = pureStandHeader,
                                     studyAreaEcoregionMap = studyareawithecoregion)
pureStandInEcoregion <- setkey(pureStand, MeasureID)[setkey(plotsbyEcoregion, MeasureID),
                                                     nomatch = 0]
setnames(pureStandInEcoregion, "ECOZONE", "Ecoregion")
ecoregionSpecies <- unique(pureStandInEcoregion[,.(Ecoregion, Species)],
                           by = c("Ecoregion", "Species")) %>%
  setkey(.,Ecoregion, Species)
# instead of using POM and quantile regresssion analyses, I used 90% quantile for the
# maximum Biomass
for(i in 1:NROW(ecoregionSpecies)){
  print(ecoregionSpecies[i,])
  Biomass <- pureStandInEcoregion[Ecoregion == ecoregionSpecies[i,]$Ecoregion &
                                    Species == ecoregionSpecies[i,]$Species,]$Biomass
  maxb <- quantile(Biomass/10, probs = 0.8)
  ecoregionSpecies[i, maxB:=maxb]
}
specieses <- unique(ecoregionSpecies$Species)
for(species in specieses){
  print(ecoregionSpecies[Species == species,])
}


BBYSAfigure <- ggplot(data = pureStandInEcoregion[Species == "jack pine"],
                      aes(x = SA, y = Biomass, colour = Ecoregion))+
  geom_point()





