studyAreaMapPath <- "C:/Users/yonluo/Documents/LandWeb"
studyAreaMap <- "landwebsimplePoly.rds"
ecoregionMapPath <- "M:/data/Ecozones"
ecoregionMap <- "ecozones.shp"

dd <- ecoregionClassification(studyAreaMap = studyAreaMap,
                              studyAreaMapPath = studyAreaMapPath,
                              ecoregionMap = ecoregionMap,
                              ecoregionMapPath = ecoregionMapPath,
                              cellSize = 1000)