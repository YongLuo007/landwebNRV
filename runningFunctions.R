

studyAreaMapPath <- "C:/Users/yonluo/Documents/LandWeb"
studyAreaMap <- "landwebsimplePoly.rds"
ecoregionMapPath <- "M:/data/Ecozones"
ecoregionMap <- "ecozones.shp"
studyAreaMapPath <- "C:/Users/yonluo/Documents/LandWeb"
studyAreaMap <- "landwebsimplePoly.rds"
ecoregionMapPath <- "M:/data/Ecozones"
ecoregionMap <- "ecozones.shp"
studyarea <- readRDS(file.path(studyAreaMapPath, studyAreaMap)) 
ecoregionMap <- rgdal::readOGR(file.path(ecoregionMapPath, ecoregionMap),
                               layer = "ecozones")

# this is the firest function
dd <- ecoregionClassification(studyAreaMap = studyarea,
                              ecoregionMap = ecoregionMap,
                              cellSize = 1000)

raster::writeRaster(dd,
                    file.path(studyAreaMapPath, "studyareaecoregion.tif"),
                    overwrite=TRUE)

w <- rgdal::readGDAL(file.path(studyAreaMapPath, "studyareaecoregion.tif"))
rgdal::writeGDAL(w, file.path(studyAreaMapPath, "studyareaecoregion.tif"),
                 drivername = "GTiff", type = "Int32", mvFlag = 0)


studyAreaEcoregionMap <- "studyareaecoregion.tif"
dd <- raster(file.path(studyAreaMapPath, studyAreaEcoregionMap))

plotLocation <- data.table(expand.grid(longitude = seq(-120, -90, length = 20),
                                       latitude = seq(52, 60, length = 20)))
plotLocation[,PlotID:=1:nrow(plotLocation)]

ff <- plotsByEcoregion(plotLocation = plotLocation, studyAreaEcoregionMap = dd)

filePath <- "C:/Users/Yong Luo/Documents/PSPs/Data/Data/AB/ESRDMaturePSPs2013"

filePathMature <- "C:/Users/Yong Luo/Documents/PSPs/maturetest"
filePathJuvenile <- "C:/Users/Yong Luo/Documents/PSPs/juveniletest"


testmature <- obtainTreeDataAB(filePath = filePathMature, pspType = "Mature")
testjuvenile <- obtainTreeDataAB(filePath = filePathJuvenile, pspType = "Juvenile")


setwd("C:/Users/predecol/Documents/PSPs/BC")
plotheader <- read.csv("Plotsummary.csv",header=TRUE,stringsAsFactor=FALSE) %>%
  data.table
setnames(plotheader,c("SAMP_ID","utm_zone","utm_easting","utm_northing","sampletype"),
         c("Plotnumber","Zone","Easting","Northing","Datatype"))
plot_withlocation  <- plotheader[,.(Plotnumber, Zone, Easting, Northing)]
utmdata <- unique(plot_withlocation, by = c("Plotnumber"))



coordRefTo <- "+proj=longlat +ellps=GRS80 +no_defs"



dd <- UTMtoLongLat(UTMTable = utmdata, 
                   coordRefTo = coordRefTo)


rm(list=ls())
filePath <- "M:/data/kNN/Original"
speciesNames <- c("Abie_Bal", "Abie_Las", "Betu_Pap", "Pice_Gla", "Pice_Mar",
                  "Pinu_Ban", "Pinu_Con", "Pinu_Str", "Popu_Tre")
speciesLayers <- stack()

for(species in speciesNames){
  speciesLayer <- raster(file.path(filePath, paste("NFI_MODIS250m_kNN_Species_",
                                                   species,"_v0.tif",
                                                   sep = "")))
  speciesLayers <- stack(speciesLayers, speciesLayer)
}
names(speciesLayers) <- speciesNames
saveRDS(speciesLayers, "speciesLayersStack.rds")



rm(list=ls())
speciesLayers <- readRDS("speciesLayersStack.rds")
studyArea <- readRDS("randomStudyArea.rds")


# dd <- initialCommunityMapkNN(speciesLayers, speciesNames, studyArea)
dd <- initialCommunityMapProducer_kNN(speciesLayers = speciesLayers, studyArea = studyArea)




rm(list = ls())
a <- readRDS("initialCommMap.rds")
b <- shapefile("M:/data/Ecozones/ecozones.shp")
c <- "ZONE_NAME"
d <- data.table(active = "yes",
                                    ecoregion = c("Taiga Cordillera", "Boreal PLain", "Taiga Shield",
                                                  "Boreal Cordillera", 
                                                   "Boreal Shield", "Hudson Plain", 
                                                  "Montane Cordillera",  "MixedWood Plain"))
e <- readRDS("randomStudyArea.rds")

ecoregionMap <- ecoregionMapProducer(studyAreaRaster = a,
                                     ecoregionMapFull = b,
                                     ecoregionName = c,
                                     ecoregionActiveStatus = d,
                                     studyArea = e)
rm(list=ls())
a <- readRDS("nonactiveEcoPoly.rds")
b <- raster("ecoregionMap.tif")
c <- read.csv("ecoregion.csv", header=T, stringsAsFactor = F) %>%
  data.table
d <- readRDS("initialCommMap.rds")
e <- readRDS("initialComm.rds")
source('~/GitHub/landwebNRV/landwebNRV/R/nonactiveEcoFromPolys.R')
removeNonEco <- nonactiveEcoFromPolys(nonactivePolys = a,
                                      ecoregionMap = b,
                                      ecoregion = c,
                                      initialCommunityMap = d,
                                      initialCommunity = e)


rm(list = ls())
a <- raster("M:/data/LandCoverOfCanada2005_V1_4/LCC2005_V1_4a.tif")
b <- raster("ecoregionMap.tif")
c <- read.csv("ecoregion.csv", header=T, stringsAsFactor = F) %>%
  data.table
d <- readRDS("initialCommMap.rds")
e <- readRDS("initialComm.rds")
f <- data.table(active = c(rep("yes", 16), rep("no", 24)),
                mapcode = 1:40)

source('~/GitHub/landwebNRV/landwebNRV/R/nonactiveEcoFromRaster.R')
removeNonEco <- nonactiveEcoFromRaster(nonactiveRaster = a,
                                       activeStatus = f,
                                       ecoregionMap = b,
                                       ecoregion = c,
                                       initialCommunityMap = d,
                                       initialCommunity = e)




