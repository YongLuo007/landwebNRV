rm(list=ls())
# workPath <- "H:/LandWeb/Data/AB"
library(data.table)
library(dplyr)
library(rgdal)
library(sp)
library(raster)
library(fpCompare)
workPath <- "C:/Users/yonluo/Documents/LandWeb/Data/AB"
treedataraw <- read.csv(file.path(workPath, "ABMatureTreeData.csv"),
                        header = TRUE,
                        stringsAsFactor = FALSE) %>%
  data.table
headdataraw <- read.csv(file.path(workPath, "plotLocation.csv"),
                        header = TRUE,
                        stringsAsFactor = FALSE) %>%
  data.table
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_ABMature.R')

ABPSPoutput <- dataPurification_ABMature(treeDataRaw = treedataraw, plotHeaderDataRaw = headdataraw)

rm(treedataraw, dataPurification_ABMature, headdataraw, workPath)


source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_BCPSP.R')
load("C:/Users/yonluo/Documents/LandWeb/Data/BC/BC_PSP.RData")
BCPSPoutput <- dataPurification_BCPSP(treeDataRaw = treedata, plotHeaderDataRaw = plotheader)
rm(treedata, plotheader, dataPurification_BCPSP)


source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_SKPSP.R')
load("~/LandWeb/Data/SK/SKPSP.RData")
SKPSPoutput <- dataPurification_SKPSP(SADataRaw = plotheader1, plotHeaderRaw = plotheader3,
                             measureHeaderRaw = plotheader2, treeDataRaw= treedata)
rm(dataPurification_SKPSP, plotheader1, plotheader2, plotheader3, treedata)



load("~/LandWeb/Data/SK/SKTSP_PAPP.RData")
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_SKTSP_PPPA.R')
sharedCol <- names(PPsampletree)[names(PPsampletree) %in% names(PAsampletree)]
sampletree <- rbind(PPsampletree[, sharedCol, with = FALSE], 
                    PAsampletree[, sharedCol, with = FALSE])
rm(sharedCol)
sharedCol <- names(PPplotheader)[names(PPplotheader) %in% names(PAplotheader)]
plotheader <- rbind(PPplotheader[, sharedCol, with = FALSE], 
                    PAplotheader[, sharedCol, with = FALSE])

rm(sharedCol)
sharedCol <- names(PPtreedata)[names(PPtreedata) %in% names(PAtreedata)]
treedata <- rbind(PPtreedata[, sharedCol, with = FALSE], 
                  PAtreedata[, sharedCol, with = FALSE])

SKTSP_PPPA_output <- dataPurification_SKTSP_PPPA(sampleTreeRaw = sampletree,
                                        plotHeaderRaw = plotheader,
                                        treeDataRaw = treedata)
rm(dataPurification_SKTSP_PPPA, sharedCol, PAplotheader, PPplotheader,
   PAsampletree, PPsampletree, PAtreedata, PPtreedata, sampletree, plotheader,
   treedata)



source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_SKTSP_Mistic.R')
load("~/LandWeb/Data/SK/SK_TSP_Mistic.RData")

SKTSP_Mistik_output <- dataPurification_SKTSP_Mistic(compiledPlotData = plotheader,
                                              compiledTreeData = treedata)

rm(dataPurification_SKTSP_Mistic, plotheader, treedata)



workPath <- "C:/Users/yonluo/Documents/PSPs/MB/PSPcsv"
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_MBPSP.R')
for (i in 1:5){
  MBPSPDataRawadd <- read.csv(file.path(workPath,
                                        paste("MBPSP_2012_file", i, ".csv",
                                              sep = "")),
                           header = TRUE,
                           stringsAsFactors = FALSE) %>%
    data.table
  if(i == 1){
    MBPSPDataRaw <- MBPSPDataRawadd
  } else {
    MBPSPDataRaw <- rbind(MBPSPDataRaw, MBPSPDataRawadd)
  }
  
}
rm(i, MBPSPDataRawadd, workPath)
MBPSPoutput <- dataPurification_MBPSP(MBPSPDataRaw = MBPSPDataRaw)
rm(MBPSPDataRaw, dataPurification_MBPSP)


workPath <- "C:/Users/yonluo/Documents/PSPs/MB/TSPcsv"
files <- dir(workPath, pattern = ".csv")
MBTSPDataRaw <- read.csv(file.path(workPath, "MB_TSP_FML1.csv"),
                         header = TRUE,
                         stringsAsFactor = FALSE) %>%
  data.table
names(MBTSPDataRaw)[12] <- "F_H"

MBTSPDataRawAdd <- read.csv(file.path(workPath, "MB_TSP_FMU11.csv"),
                         header = TRUE,
                         stringsAsFactor = FALSE) %>%
  data.table
MBTSPDataRaw <- rbind(MBTSPDataRaw, MBTSPDataRawAdd)
rm(MBTSPDataRawAdd)

MBTSPDataRawAdd <- read.csv(file.path(workPath, "MB_TSP_FMU12.csv"),
                            header = TRUE,
                            stringsAsFactor = FALSE) %>%
  data.table
MBTSPDataRaw <- rbind(MBTSPDataRaw, MBTSPDataRawAdd)
rm(MBTSPDataRawAdd)

MBTSPDataRawAdd <- read.csv(file.path(workPath, "MB_TSP_FMUs13_14.csv"),
                            header = TRUE,
                            stringsAsFactor = FALSE) %>%
  data.table
names(MBTSPDataRawAdd)[12] <- "F_H"
set(MBTSPDataRawAdd, ,c("COND2", "COND3"), NULL)
setnames(MBTSPDataRawAdd, "COND1", "COND")
MBTSPDataRaw <- rbind(MBTSPDataRaw, MBTSPDataRawAdd)
rm(MBTSPDataRawAdd)

MBTSPDataRawAdd <- read.csv(file.path(workPath, "MB_TSP_Highrock.csv"),
                            header = TRUE,
                            stringsAsFactor = FALSE) %>%
  data.table
MBTSPDataRaw <- rbind(MBTSPDataRaw, MBTSPDataRawAdd)
rm(MBTSPDataRawAdd)

MBTSPDataRawAdd <- read.csv(file.path(workPath, "MB_TSP_Pineland.csv"),
                            header = TRUE,
                            stringsAsFactor = FALSE) %>%
  data.table
setnames(MBTSPDataRawAdd, c("TRANSECT", "FULL_HALF"), c("TRANS", "F_H"))
MBTSPDataRaw <- rbind(MBTSPDataRaw, MBTSPDataRawAdd)
rm(MBTSPDataRawAdd)

MBTSPDataRawAdd <- read.csv(file.path(workPath, "MB_TSP_Saskriver_1.csv"),
                            header = TRUE,
                            stringsAsFactor = FALSE) %>%
  data.table
names(MBTSPDataRawAdd)[12] <- "F_H"
MBTSPDataRaw <- rbind(MBTSPDataRaw, MBTSPDataRawAdd)
rm(MBTSPDataRawAdd)

MBTSPDataRawAdd <- read.csv(file.path(workPath, "MB_TSP_Saskriver_2.csv"),
                            header = TRUE,
                            stringsAsFactor = FALSE) %>%
  data.table
names(MBTSPDataRawAdd)[12] <- "F_H"
MBTSPDataRaw <- rbind(MBTSPDataRaw, MBTSPDataRawAdd)
rm(MBTSPDataRawAdd)

source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_MBTSP.R')

MBTSPoutput <- dataPurification_MBTSP(MBTSPDataRaw)


rm(MBTSPDataRaw, workPath)
workPath <- "~/PSPs/NRCAN/NFI_PSP"
source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_NFIPSP.R')
lgptreeRaw <- read.csv(file.path(workPath, "all_gp_ltp_tree.csv"),
                       header = TRUE,
                       stringsAsFactor = FALSE) %>%
  data.table
lgpHeaderRaw <- read.csv(file.path(workPath, "all_gp_ltp_header.csv"),
                         header = TRUE,
                         stringsAsFactor = FALSE) %>%
  data.table
approxLocation <- read.csv(file.path(workPath, "all_gp_climate_approx_loc.csv"),
                           header = TRUE, 
                           stringsAsFactor = FALSE) %>%
  data.table

NFIPSPoutput <- dataPurification_NFIPSP(lgptreeRaw = lgptreeRaw, 
                                     lgpHeaderRaw = lgpHeaderRaw,
                                     approxLocation = approxLocation)
rm(workPath, lgptreeRaw, lgpHeaderRaw, approxLocation, dataPurification_NFIPSP)


workPath <- "~/PSPs/NT/data"
plotHeaderRaw <- read.csv(file.path(workPath, "PLOT.csv"),
                          header = TRUE,
                          stringsAsFactor = FALSE) %>%
  data.table
additionalLocationInfor <- read.csv(file.path(workPath, "AdditionalLocations.csv"),
                                    header = TRUE,
                                    stringsAsFactor = FALSE) %>%
  data.table
treeDataRaw <- read.csv(file.path(workPath, "TREE.csv"),
                        header = TRUE,
                        stringsAsFactor = FALSE) %>%
  data.table
primarySAInfor <- read.csv(file.path(workPath, "SAMPLE_TREE.csv"),
                           header = TRUE,
                           stringsAsFactor = FALSE) %>%
  data.table
secondSAInfor <- read.csv(file.path(workPath, "SA_infor.csv"),
                          header = TRUE,
                          stringsAsFactor = FALSE) %>%
  data.table

source('~/GitHub/landwebNRV/landwebNRV/R/dataPurification_NWTTSP.R')

NWTTSPoutput <- dataPurification_NWTTSP(plotHeaderRaw,
                                        primarySAInfor,
                                        secondSAInfor,
                                        additionalLocationInfor,
                                        treeDataRaw)

