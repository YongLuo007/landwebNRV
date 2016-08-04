# this is to produce figure 2 and figure 3
rm(list=ls())
library(data.table); library(ggplot2);library(grid)
library(gtable); library(gridExtra);library(raster)
thedata <- read.csv("M:/data/LandWeb/BorealPlain simulation/outputs/summarizedSeralStageAndVegType.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
thedata <- data.table(thedata)
thedata <- thedata[,.(minArea = min(areaHa, na.rm = TRUE), 
                      maxArea = max(areaHa, na.rm = TRUE),
                      minAreaPercentage = min(areaPercentage*100, na.rm = TRUE),
                      maxAreaPercentage = max(areaPercentage*100, na.rm = TRUE)),
                   by = c("content", "ecoregion", "type")]
thedata[, temptype:=type]
thedata[temptype == 1, type:=4]
thedata[temptype == 2, type:=3]
thedata[temptype == 3, type:=2]
thedata[temptype == 4, type:=1]
thedata[, type:=as.character(type)]

# for figure 2 seral stage
seralStageData <- thedata[content == "seralStage",]
vegTypeData <- thedata[content == "vegType",]
vegTypeDataExp <- data.table(expand.grid(type = as.character(1:4), 
                                         ecoregion = c(139, 149)))
vegTypeData <- dplyr::left_join(vegTypeDataExp, vegTypeData, by = c("type", "ecoregion"))
vegTypeData[is.na(vegTypeData)] <- 0

ecoDisFull <- shapefile("M:/data/ecoFramework/Ecodistricts/ecodistricts.shp")
seralMapExample <- readRDS("M:/data/LandWeb/BorealPlain simulation/outputs/seralStageMapStackRep1.rds")
vegTypeMapExample <- readRDS("M:/data/LandWeb/BorealPlain simulation/outputs/vegTypeMapStackRep1.rds")
ecoDisFull <- spTransform(ecoDisFull, crs(seralMapExample[[1]]))
ecoDisStudy <- crop(ecoDisFull, seralMapExample[[1]])

# for ecoregion 139
sub1Title <- grobTree(textGrob("Region 1", x = 0.3, y = 0.95, hjust = 0,
                               gp = gpar(col = "black", fontsize = 15)))
# add example
ecoDisStudyRegion1 <- ecoDisStudy[ecoDisStudy$ECOREGION==139,]
ecoDisStudyRegion2 <- ecoDisStudy[ecoDisStudy$ECOREGION==149,]
seralMapExampleRegion1 <- suppressWarnings(mask(seralMapExample[[1]],ecoDisStudyRegion1))
seralMapExampleRegion2 <- suppressWarnings(mask(seralMapExample[[1]],ecoDisStudyRegion2))
vegTypeMapExampleRegion1 <- suppressWarnings(mask(vegTypeMapExample[[1]],ecoDisStudyRegion1))
vegTypeMapExampleRegion2 <- suppressWarnings(mask(vegTypeMapExample[[1]],ecoDisStudyRegion2))

sub1Title <- grobTree(textGrob("Region 1", x = 0.3, y = 0.95, hjust = 0,
                               gp = gpar(col = "black", fontsize = 15)))
figure3_1 <- ggplot(data = seralStageData[ecoregion == 139,],
                    aes(fill = as.factor(seralStageData[ecoregion == 139,]$type))) + 
  annotation_custom(sub1Title)+
  annotation_raster(as.raster(seralMapExampleRegion1), xmin = 0.7, xmax = 3.7,
                    ymin = 25, ymax = 55)+
  geom_rect(aes(x = type,
                xmin = as.numeric(type)-0.2, 
                xmax = as.numeric(type)+0.2,
                ymin = minAreaPercentage,
                ymax = maxAreaPercentage)) +
  scale_fill_manual(name = "Seral Stage", values = c("forestgreen", "olivedrab3",
                                                     "goldenrod3", "grey"),
                    labels = c("Overold (>100)", "Old (80-100)",
                               "Mature (40-80)", "Young (< 40)"),
                    guide = guide_legend(reverse = TRUE))+
  scale_y_continuous("", 
                     breaks = c(seq(0, 20, by = 5), seq(80, 100, by = 5)),
                     labels = c(seq(0, 20, by = 5), seq(80, 100, by = 5)))+

  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size = 1),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.length = unit(5, "points"),
        axis.ticks.y = element_blank(),
        legend.position = c(0.7, 0.6),
        legend.title = element_text(size = 15),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.margin = unit(50, "points"),
        legend.key.width = unit(20, "points"),
        legend.key.height = unit(20, "points"),
        legend.text = element_text(size = 12))+
  coord_flip()


sub2Title <- grobTree(textGrob("Region 2", x = 0.3, y = 0.95, hjust = 0,
                               gp = gpar(col = "black", fontsize = 15)))
figure3_2 <- ggplot(data = seralStageData[ecoregion == 149,],
                    aes(fill = as.factor(seralStageData[ecoregion == 149,]$type))) + 
  annotation_custom(sub2Title)+
  annotation_raster(as.raster(seralMapExampleRegion2), xmin = 0.7, xmax = 3.7,
                    ymin = 25, ymax = 55)+
  geom_rect(aes(x = type,
                xmin = as.numeric(type)-0.2, 
                xmax = as.numeric(type)+0.2,
                ymin = minAreaPercentage,
                ymax = maxAreaPercentage)) +
  scale_fill_manual(name = "Seral Stage", values = c("forestgreen", "olivedrab3",
                                                     "goldenrod3", "grey"),
                    labels = c("Overold (>100)", "Old (80-100)",
                               "Mature (40-80)", "Young (< 40)"),
                    guide = guide_legend(reverse = TRUE))+
  scale_y_continuous("Percentage in forested landscape (%)", 
                     breaks = c(seq(0, 20, by = 5), seq(80, 100, by = 5)),
                     labels = c(seq(0, 20, by = 5), seq(80, 100, by = 5)))+
  
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size = 1),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.ticks.length = unit(5, "points"),
        axis.ticks.y = element_blank(),
        legend.position = c(0.7, 0.6),
        legend.title = element_text(size = 15),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.margin = unit(50, "points"),
        legend.key.width = unit(20, "points"),
        legend.key.height = unit(20, "points"),
        legend.text = element_text(size = 12))+
  coord_flip()


figure3_1 <- ggplotGrob(figure3_1)
figure3_2 <- ggplotGrob(figure3_2)
figure3_1$widths <- figure3_2$widths
figure3_1$heights <- figure3_2$heights

grid.arrange(figure3_1, figure3_2)




sub1Title <- grobTree(textGrob("Region 1", x = 0.1, y = 0.95, hjust = 0,
                               gp = gpar(col = "black", fontsize = 15)))
# for figure 4 forest vegetation types
figure4_1 <- ggplot(data = vegTypeData[ecoregion == 139,],
                    aes(fill = as.factor(vegTypeData[ecoregion == 139,]$type))) + 
  annotation_custom(sub1Title)+
  annotation_raster(as.raster(vegTypeMapExampleRegion1), xmin = 0.5, xmax = 4,
                    ymin = 0, ymax = 15)+
  geom_rect(aes(x = type,
                xmin = as.numeric(type)-0.2, 
                xmax = as.numeric(type)+0.2,
                ymin = minAreaPercentage,
                ymax = maxAreaPercentage)) +
  scale_fill_manual(name = "Vegetation Type", values = c("white", "yellow3",
                                                     "grey70", "white"),
                    labels = c(expression(paste("Mixed (", B[none], " > 50%)", sep = "")),
                               expression(paste("Spruce Leading (",
                                                B[spruce], " > 50%)", sep = "")),
                               expression(paste("Aspen Leading (",
                                                B[aspen], " > 50%)", sep = "")),
                               expression(paste("Pine Leading (",
                                                B[pine], " > 50%)", sep = ""))),
                    guide = guide_legend(reverse = TRUE))+
  scale_y_continuous("", limits = c(0, 60),
                     breaks = c(seq(40, 60, by = 5)),
                     labels = c(seq(40, 60, by = 5)))+
  
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size = 1),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.length = unit(5, "points"),
        axis.ticks.y = element_blank(),
        legend.position = c(0.45, 0.6),
        legend.title = element_text(size = 15),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.margin = unit(50, "points"),
        legend.key.width = unit(20, "points"),
        legend.key.height = unit(20, "points"),
        legend.text = element_text(size = 12),
        legend.text.align = 0)+
  coord_flip()

sub2Title <- grobTree(textGrob("Region 2", x = 0.1, y = 0.95, hjust = 0,
                               gp = gpar(col = "black", fontsize = 15)))
figure4_2 <- ggplot(data = vegTypeData[ecoregion == 149,],
                    aes(fill = as.factor(vegTypeData[ecoregion == 149,]$type))) + 
  annotation_custom(sub2Title)+
  annotation_raster(as.raster(vegTypeMapExampleRegion2), xmin = 0.5, xmax = 4,
                    ymin = 0, ymax = 15)+
  geom_rect(aes(x = type,
                xmin = as.numeric(type)-0.2, 
                xmax = as.numeric(type)+0.2,
                ymin = minAreaPercentage,
                ymax = maxAreaPercentage)) +
  scale_fill_manual(name = "Vegetation Type", values = c("white", "forestgreen",
                                                        "grey70", "white"),
                    labels = c(expression(paste("Mixed (", B[none], " > 50%)", sep = "")),
                               expression(paste("Spruce Leading (",
                                                B[spruce], " > 50%)", sep = "")),
                               expression(paste("Aspen Leading (",
                                                B[aspen], " > 50%)", sep = "")),
                               expression(paste("Pine Leading (",
                                                B[pine], " > 50%)", sep = ""))),
                    guide = guide_legend(reverse = TRUE))+
  scale_y_continuous("Percentage in forested landscape (%)", limits = c(0, 60),
                     breaks = c(seq(40, 60, by = 5)),
                     labels = c(seq(40, 60, by = 5)))+
  
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size = 1),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.ticks.length = unit(5, "points"),
        axis.ticks.y = element_blank(),
        legend.position = c(0.45, 0.6),
        legend.title = element_text(size = 15),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.margin = unit(50, "points"),
        legend.key.width = unit(20, "points"),
        legend.key.height = unit(20, "points"),
        legend.text = element_text(size = 12),
        legend.text.align = 0)+
  coord_flip()


figure4_1 <- ggplotGrob(figure4_1)
figure4_2 <- ggplotGrob(figure4_2)
figure4_1$widths <- figure4_2$widths
figure4_1$heights <- figure4_2$heights
grid.arrange(figure4_1, figure4_2)
