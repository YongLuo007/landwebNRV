# to produce figure 2
rm(list=ls())
library(data.table);library(dplyr);library(ggplot2)
for(i in 1:10){
  biomasstable <- read.csv(paste("biomassOutput", i, ".csv", sep = ""),
                           header = TRUE,
                           stringsAsFactors = FALSE) %>%
    data.table
  biomasstable[,chain:=i]
  biomasstable <- biomasstable[,.(Year, totalB, chain)]
  if(i == 1){
    output <- biomasstable
  } else {
    output <- rbind(output, biomasstable)
  }

}
output[, chain:=as.factor(chain)]

ecoDisFull <- shapefile("M:/data/ecoFramework/Ecodistricts/ecodistricts.shp")
seralMapExample <- readRDS("M:/data/LandWeb/BorealPlain simulation/outputs/seralStageMapStackRep1.rds")
ecoDisFull <- spTransform(ecoDisFull, crs(seralMapExample[[1]]))
ecoDisStudy <- crop(ecoDisFull, seralMapExample[[1]])

# for ecoregion 139
sub1Title <- grobTree(textGrob("Region 1", x = 0.3, y = 0.95, hjust = 0,
                               gp = gpar(col = "black", fontsize = 15)))
# add example
ecoDisStudyRegion1 <- ecoDisStudy[ecoDisStudy$ECOREGION==139,]
ecoDisStudyRegion2 <- ecoDisStudy[ecoDisStudy$ECOREGION==149,]
tempMap <- setValues(seralMapExample[[1]], 1)
MapRegion1 <- suppressWarnings(mask(tempMap, ecoDisStudyRegion1))
MapRegion1[Which(is.na(MapRegion1), cells = TRUE)] <- 0
MapRegion2 <- suppressWarnings(mask(tempMap, ecoDisStudyRegion2))
MapRegion2[Which(is.na(MapRegion2), cells = TRUE)] <- 0

figure2 <- ggplot(data = output, aes(x = Year, y = totalB, col = chain))+
  geom_rect(aes(xmin = 800, xmax = 2000, ymin = 6000, ymax = 7200),
            fill = "white", col = "red")+
  annotate("text", x = 1400, y = 7150, label = "Summary Regions", size = 6)+
  annotation_raster(as.raster(MapRegion1), xmin = 850, xmax = 1375,
                    ymin = 6050, ymax = 7000)+
  annotation_raster(as.raster(MapRegion2), xmin = 1425, xmax = 1950,
                    ymin = 6050, ymax = 7000)+
  geom_rect(aes(xmin = 1400, xmax = 2000, ymin = 7500, ymax = 8000),
            fill = "white", col = "red")+
  annotate("text", x = 1700, y = 7650, label = "Summary Period", size = 6)+
  geom_line()+
  scale_x_continuous("Year", limits = c(-0, 2000), breaks = seq(0, 2000, by = 400))+
  scale_y_continuous(name = expression(paste("Aboveground biomass (g . ", m^-2, ")",
                                             sep = "")),
                     limits = c(6000, 9000), breaks = seq(6000, 9000, by = 500))+
  theme_bw()+
  guides(colour = guide_legend(title = "Simulations", title.position = "top"))+

  # labs(colour = "Simulation")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size = 1),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.title.align = 0.5,
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.key.width = unit(60, "points"),
        legend.key.height = unit(35, "points"),
        legend.position = c(0.7, 0.8),
        legend.direction = "horizontal",
        legend.text = element_text(size = 15))
