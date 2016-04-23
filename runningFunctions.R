

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
treedataraw <- read.csv("C:/Users/yonluo/Documents/LandWeb/Data/AB/ABMatureTreeData.csv",
                        header = TRUE,
                        stringsAsFactor = FALSE) %>%
  data.table
headdataraw <- read.csv("C:/Users/yonluo/Documents/LandWeb/Data/AB/plotLocation.csv",
                        header = TRUE,
                        stringsAsFactor = FALSE) %>%
  data.table

source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_ABMature.R')
ls()
output <- dataPurification_ABMature(treeDataRaw = treedataraw, headDataRaw = headdataraw)



rm(list=ls())
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_BCMature.R')
setwd("C:/Users/yonluo/Documents/LandWeb/Data/BC")
load("BC_PSP.RData")

dd <- dataPurification_BC(treeDataRaw = treedata, headDataRaw = plotheader)

rm(list=ls())
setwd("~/LandWeb/Data/SK")
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_SKPSP.R')
load("SKPSP.RData")
SADataRaw <- plotheader1
plotHeadRaw <- plotheader3
measureHeadRaw <- plotheader2
treeDataRaw <- treedata

dd <- dataPurification_SKPSP(SADataRaw = SADataRaw, plotHeadRaw = plotHeadRaw,
                             measureHeadRaw = measureHeadRaw, treeDataRaw= treeDataRaw)


rm(list=ls())
setwd("~/LandWeb/Data/SK")
load("SKTSP_PAPP.RData")
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_SKTSP.R')

PPoutput <- dataPurification_SKTSP(sampleTreeRaw = PPsampletree,
                             plotHeadRaw = PPplotheader,
                             treeDataRaw = PPtreedata)

PAoutput <- dataPurification_SKTSP(sampleTreeRaw = PAsampletree,
                                   plotHeadRaw = PAplotheader,
                                   treeDataRaw = PAtreedata)

rm(list=ls())
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_SKTSP_Mistic.R')
load("SK_TSP_Mistic.RData")

Mistikoutput <- dataPurification_SKTSP_Mistic(compiledPlotData = plotheader,
                                          compiledTreeData = treedata)







