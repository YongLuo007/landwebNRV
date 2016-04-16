rm(list=ls())
setwd("~/PSPs/NT/data/wesbogy dataset")
wesbogyPlot <- read.csv("WESBOGY_PLOT.csv", header=T, stringsAsFactor = F) %>%
  data.table

wesbogyTree <- read.csv("WESBOGY_TREE.csv", header=T, stringsAsFactor = F) %>%
  data.table


rm(list=ls())
setwd("~/PSPs/SK/SKPSP")
treedata <- read.csv("SKPSPtrees.csv",header=T, stringsAsFactor = F) %>%
  data.table


plotheader1 <- read.csv("age_samples.csv",header=T, stringsAsFactor = F) %>%
  data.table


plotheader2 <- read.csv("measurement_header.csv",header=T, 
                        stringsAsFactor = F) %>%
  data.table


plotheader3 <- read.csv("plot_header.csv",header=T, stringsAsFactor = F) %>%
  data.table


rm(list=ls())
setwd("~/PSPs/SK/SKTSP")
PAtreedata <- read.csv("PA_Tree_Measurement.csv",header=T, stringsAsFactor = F) %>%
  data.table


PAplotheader <- read.csv("PA_Plot_Locations_csrs98.csv",header=T, stringsAsFactor = F) %>%
  data.table

PAsampletree <- read.csv("PA_Sample_Trees.csv",header=T, stringsAsFactor = F) %>%
  data.table

PPtreedata <- read.csv("PP_Tree_Measurement.csv",header=T, stringsAsFactor = F) %>%
  data.table


PPplotheader <- read.csv("PP_Plot_Locations_csrs98.csv",header=T, stringsAsFactor = F) %>%
  data.table

PPsampletree <- read.csv("PP_Sample_Trees.csv",header=T, stringsAsFactor = F) %>%
  data.table


rm(list=ls())
setwd("~/PSPs/SK/SKTSP/Mistic")

treedata <- read.csv("COMPILEDTREEDATA.csv",header=T, stringsAsFactor = F) %>%
  data.table


plotheader <- read.csv("COMPILEDPLOTDATA.csv",header=T, stringsAsFactor = F) %>%
  data.table


rm(list=ls())
setwd("~/PSPs/NRCAN/NFI_PSP")
largeTreedata <- read.csv("all_gp_ltp_tree.csv",header=T, stringsAsFactor = F) %>%
  data.table


largeTreeAgedata <- read.csv("all_gp_ltp_tree_age.csv",header=T, stringsAsFactor = F) %>%
  data.table


largeTreePlotHeader <- read.csv("all_gp_ltp_header.csv",header=T, stringsAsFactor = F) %>%
  data.table


plotOrigin <- read.csv("all_gp_origin.csv",header=T, stringsAsFactor = F) %>%
  data.table

smallTreedata <- read.csv("all_gp_stp_tree.csv",header=T, stringsAsFactor = F) %>%
  data.table


smallTreePlotHeader <- read.csv("all_gp_stp_header.csv",header=T, stringsAsFactor = F) %>%
  data.table


