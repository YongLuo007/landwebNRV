# this is to produce figure 2 and figure 3
library(data.table); library(ggplot2);library(grid)
library(gtable)
thedata <- read.csv("summarizedSeralStageAndVegType.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
thedata <- data.table(thedata)
thedata <- thedata[,.(minArea = min(areaHa), 
                      maxArea = max(areaHa),
                      minAreaPercentage = min(areaPercentage),
                      maxAreaPercentage = max(areaPercentage)),
                      by = c("content", "ecoregion", "type")]
thedata[, type := as.character(type)]

# for figure 2 seral stage
seralStageData <- thedata[content == "seralStage",]
seralStageData[type == 1, seralStage := "Young"]
seralStageData[type == 2, seralStage := "Mature"]
seralStageData[type == 3, seralStage := "Old"]
seralStageData[type == 4, seralStage := "Overold"]

# for ecoregion 139
sub1Title <- grobTree(textGrob("Region 1", x = 0.05, y = 0.95, hjust = 0,
                               gp = gpar(col = "black", fontsize = 20)))

figure2_1 <- ggplot(data = seralStageData[ecoregion == 139,],
                    aes(fill = "red")) + 
  annotation_custom(sub1Title)+
  geom_rect(aes(x = type,
                xmin = as.numeric(type)-0.2, 
                xmax = as.numeric(type)+0.2,
                ymin = as.numeric(minAreaPercentage),
                ymax = as.numeric(maxAreaPercentage))) +
   scale_x_discrete("", breaks = as.character(c(1:4)),
                    labels = c("Young", "Mature", "Old", "Overold"))+
  scale_y_continuous("", 
                   breaks = c(seq(0, 0.2, by = 0.05), seq(0.8, 1, by = 0.05)),
                     labels = c(seq(0, 0.2, by = 0.05), seq(0.8, 1, by = 0.05)))+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")+
  coord_flip()





