# to produce figure one
rm(list=ls())
library(raster); library(SpaDES); library(rgdal);library(grid);library(ggplot2)
library(maptools); library(dplyr)
CanadaMap <- shapefile("M:/data/CanadaMap/Canada.shp")
initialCommunityMap <- raster("M:/data/LandWeb/BorealPlain simulation/inputs/initialCommunitiesMap.tif")
ecoregionMap <- raster("M:/data/LandWeb/BorealPlain simulation/inputs/landtypes_BP.tif")
CanadaMap <- spTransform(CanadaMap, crs(ecoregionMap))
studyArea <- crop(CanadaMap, ecoregionMap)

CanadaMap <- fortify(CanadaMap, region = "NAME") 
CanadaMap <- data.table(CanadaMap)
CanadaMap1 <- CanadaMap[long<=1.5e+06, ] %>%
  data.frame

figure1 <- ggplot(CanadaMap, aes(x = long, y = lat, col = "grey")) +
  geom_rect(aes(xmin = xmin(studyArea), xmax = xmax(studyArea),
                ymin = ymin(studyArea), ymax = ymax(studyArea)),
            fill = "white", col = "red")+
  geom_path(aes(group = group))+
  scale_color_manual(values = "grey", label = "border")+
  geom_rect(aes(xmin = 0.8e+06, xmax = 3e+06,
            ymin = 8.5e+06, ymax = 1.09e+07), fill = "white", col = "white")+
  geom_rect(aes(xmin = 0.8e+06, xmax = 3e+06,
                ymin = 6e+06, ymax = 8.4e+06), fill = "white", col = "white")+
  annotation_raster(as.raster(ecoregionMap), xmin = 0.8e+06, xmax = 3e+06,
                    ymin = 8.5e+06, ymax = 1.05e+07)+
  annotation_raster(as.raster(initialCommunityMap), xmin = 0.8e+06, xmax = 3e+06,
                    ymin = 6e+06, ymax = 8e+06)+
  annotate("text", x = 1.9e+06, y = 1.075e+7, label = "Ecoregions (N=163)", size = 5)+
  annotate("text", x = 1.9e+06, y = 8.25e+6, label = "Initial Communities (N=442)", size = 5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
