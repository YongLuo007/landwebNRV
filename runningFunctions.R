

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












