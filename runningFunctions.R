

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
output <- dataPurification_ABMature(treeDataRaw = treedataraw, plotHeaderDataRaw = headdataraw)



rm(list=ls())
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_BCPSP.R')
setwd("C:/Users/yonluo/Documents/LandWeb/Data/BC")
load("BC_PSP.RData")
dd <- dataPurification_BCPSP(treeDataRaw = treedata, plotHeaderDataRaw = plotheader)
treeData <- dd$treeData

source('~/GitHub/landwebNRV/landwebNRV/R/standardizeSpeciesName.R')

treeData1 <- standardizeSpeciesName(speciesTable = treeData, forestInventorySource = "BCPSP")

source('~/GitHub/landwebNRV/landwebNRV/R/biomassCalculation.R')
treeData1[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]

Biomass <- biomassCalculation(species = treeData1$newSpeciesName, DBH = treeData1$DBH)


rm(list=ls())
setwd("~/LandWeb/Data/SK")
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_SKPSP.R')
load("SKPSP.RData")
SADataRaw <- plotheader1
plotHeadRaw <- plotheader3
measureHeadRaw <- plotheader2
treeDataRaw <- treedata

dd <- dataPurification_SKPSP(SADataRaw = SADataRaw, plotHeaderRaw = plotHeadRaw,
                             measureHeaderRaw = measureHeadRaw, treeDataRaw= treeDataRaw)


rm(list=ls())
setwd("~/LandWeb/Data/SK")
load("SKTSP_PAPP.RData")
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_SKTSP_PPPA.R')

PPoutput <- dataPurification_SKTSP_PPPA(sampleTreeRaw = PPsampletree,
                             plotHeaderRaw = PPplotheader,
                             treeDataRaw = PPtreedata)

PAoutput <- dataPurification_SKTSP_PPPA(sampleTreeRaw = PAsampletree,
                                   plotHeaderRaw = PAplotheader,
                                   treeDataRaw = PAtreedata)

rm(list=ls())
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_SKTSP_Mistic.R')
load("SK_TSP_Mistic.RData")

Mistikoutput <- dataPurification_SKTSP_Mistic(compiledPlotData = plotheader,
                                          compiledTreeData = treedata)

rm(list=ls())
setwd("~/PSPs/Data/Data/MB")
setwd("C:/Users/yonluo/Documents/PSPs/MB/PSPcsv")
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_MBPSP.R')
MBPSPDataRaw <- read.csv("MBPSP_2012_file1.csv",
                         header = TRUE,
                         stringsAsFactors = FALSE) %>%
  data.table
MBdata <- dataPurification_MBPSP(MBPSPDataRaw = MBPSPDataRaw)



rm(list=ls())
setwd("C:/Users/Yong Luo/Documents/PSPs/Data/Data/MB")
MBTSPDataRaw <- read.csv("MB_TSP_Highrock.csv",
                         header = TRUE,
                         stringsAsFactors = FALSE) %>%
  data.table
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_MBTSP.R')

dd <- dataPurification_MBTSP(MBTSPDataRaw)


MBTSPDataRaw <- read.csv("MB_TSP_FML1.csv",
                         header = TRUE,
                         stringsAsFactors = FALSE) %>%
  data.table
dd <- dataPurification_MBTSP(MBTSPDataRaw)


rm(list=ls())
setwd("~/PSPs/NRCAN/NFI_PSP")
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_NFIPSP.R')
lgptreeRaw <- read.csv("all_gp_ltp_tree.csv", header= TRUE, stringsAsFactor=F) %>%
  data.table
lgpHeaderRaw <- read.csv("all_gp_ltp_header.csv", header= TRUE, stringsAsFactor=F) %>%
  data.table
approxLocation <- read.csv("all_gp_climate_approx_loc.csv", header= TRUE, stringsAsFactor=F) %>%
  data.table

NFIoutput <- dataPurification_NFIPSP(lgptreeRaw = lgptree, 
                                     lgpHeaderRaw = lgpHeader,
                                     approxLocation = approxLocations)

