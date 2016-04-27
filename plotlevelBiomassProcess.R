rm(list=ls())
library(data.table)
library(dplyr)
library(rgdal)
library(sp)
library(raster)
workPath <- "~/GitHub/landwebNRV"
load(file.path(workPath, "alldata_purified.RData"))
source('~/GitHub/landwebNRV/landwebNRV/R/standardizeSpeciesName.R')
source('~/GitHub/landwebNRV/landwebNRV/R/biomassCalculation.R')
source('~/GitHub/landwebNRV/landwebNRV/R/speciesDominanceIdentification.R')
source('~/GitHub/landwebNRV/landwebNRV/R/UTMtoLongLat.R')
# gethering plot header data
allPlotHeaderData <- ABPSPoutput$plotHeaderData
allPlotHeaderData[,Northing:=as.numeric(Northing)]
allPlotHeaderData <- rbind(allPlotHeaderData, BCPSPoutput$plotHeaderData)
allPlotHeaderData <- rbind(allPlotHeaderData, MBPSPoutput$plotHeaderData)
allPlotHeaderData <- rbind(allPlotHeaderData, MBTSPoutput$plotHeaderData)
allPlotHeaderData <- rbind(allPlotHeaderData, NFIPSPoutput$plotHeaderData)
allPlotHeaderData <- rbind(allPlotHeaderData, NWTTSPoutput$plotHeaderData)
allPlotHeaderData <- rbind(allPlotHeaderData, SKPSPoutput$plotHeaderData)
allPlotHeaderData <- rbind(allPlotHeaderData, SKTSP_Mistik_output$plotHeaderData[
  , ':='(Easting = Easting*10000, Northing = Northing*10000)])
allPlotHeaderData <- rbind(allPlotHeaderData, SKTSP_PPPA_output$plotHeaderData)
allPlotHeaderDataLongLat <- allPlotHeaderData[!is.na(Longitude) & Longitude!=0, ]
print(range(allPlotHeaderDataLongLat$Longitude))
print(range(allPlotHeaderDataLongLat$Latitude))
allPlotHeaderDataNoLongLat <- allPlotHeaderData[!(MeasureID %in% allPlotHeaderDataLongLat$MeasureID),]
print(range(allPlotHeaderDataNoLongLat$Zone))
print(range(allPlotHeaderDataNoLongLat$Easting))
print(range(allPlotHeaderDataNoLongLat$Northing))
allPlotHeaderDataNoLongLat <- UTMtoLongLat(UTMTable = allPlotHeaderDataNoLongLat)$Transformed

allPlotHeaderDataLongLat <- rbind(allPlotHeaderDataLongLat, allPlotHeaderDataNoLongLat)


# working on AB PSP
ABPSPtreeData <- ABPSPoutput$treeData
ABPSPtreeData <- standardizeSpeciesName(ABPSPtreeData, forestInventorySource = "ABPSP")
# nrow(ABPSPtreeData[newSpeciesName == "unknown",]) # 0
ABPSPtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]
# unique(ABPSPtreeData[Biomass==0,]$newSpeciesName)
# limber pine whitebark pine
# for these two species, using softwood equation
ABPSPtreeData[newSpeciesName == "limber pine" | newSpeciesName == "whitebark pine",
              newSpeciesName:="softwood"]

ABPSPtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]
# unique(ABPSPtreeData[Biomass==0,]$newSpeciesName)
dominantSpecies <- speciesDominanceIdentification(ABPSPtreeData)


# working on AB PSP
BCPSPtreeData <- BCPSPoutput$treeData
BCPSPtreeData <- standardizeSpeciesName(BCPSPtreeData, forestInventorySource = "BCPSP")
# nrow(ABPSPtreeData[newSpeciesName == "unknown",]) # 0
BCPSPtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]
# unique(BCPSPtreeData[Biomass==0,]$newSpeciesName)
# [1] "willow"           "mountain alder"  
# [3] "fir"              "mountain hemlock"
# [5] "scoulers willow" 
# 
ABPSPtreeData[newSpeciesName == "limber pine" | newSpeciesName == "whitebark pine",
              newSpeciesName:="softwood"]

ABPSPtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]



SKPSPtreeData <- SKPSPoutput$treeData
SKPSPtreeData <- standardizeSpeciesName(SKPSPtreeData, forestInventorySource = "SKPSP")
nrow(SKPSPtreeData[newSpeciesName == "unknown",])
# 4750

unique(SKPSPtreeData[newSpeciesName == "unknown",]$Species)
# "XX" ""

SKPSPtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]

unique(SKPSPtreeData[Biomass==0,]$newSpeciesName)
range(SKPSPtreeData$DBH) # NA
SKPSPtreeData <- SKPSPtreeData[!is.na(DBH) & DBH != 0,] # need to fix this bug

SKPSPtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]

unique(SKPSPtreeData[Biomass==0,]$newSpeciesName)
# [1] "unknown"        "manitoba maple"


SKTSP_MistiktreeData <- SKTSP_Mistik_output$treeData
SKTSP_MistiktreeData <- standardizeSpeciesName(SKTSP_MistiktreeData, forestInventorySource = "SKTSP")

nrow(SKTSP_MistiktreeData[newSpeciesName == "unknown",])
# 162
unique(SKTSP_MistiktreeData[newSpeciesName == "unknown",]$Species)
# [1] "DC" ""   "DD"

SKTSP_MistiktreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]
unique(SKTSP_MistiktreeData[Biomass==0,]$newSpeciesName)
# unknown


SKTSP_PPPAtreeData <- SKTSP_PPPA_output$treeData
SKTSP_PPPAtreeData <- standardizeSpeciesName(SKTSP_PPPAtreeData, forestInventorySource = "SKTSP")

nrow(SKTSP_PPPAtreeData[newSpeciesName == "unknown",])
# 0

SKTSP_PPPAtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]
unique(SKTSP_PPPAtreeData[Biomass==0,]$newSpeciesName)
# [1] "manitoba maple" "green ash"  

NFIPSPtreeData <- NFIPSPoutput$treeData
NFIPSPtreeData <- standardizeSpeciesName(NFIPSPtreeData, forestInventorySource = "NFIPSP")

nrow(NFIPSPtreeData[newSpeciesName == "unknown",])
# 6
unique(NFIPSPtreeData[newSpeciesName == "unknown",]$Genus) # UNKN SPP

NFIPSPtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]
unique(NFIPSPtreeData[Biomass==0,]$newSpeciesName)
# [1] "scouler willow" "birch"         
# [3] "willow"         "alder"         
# [5] "unknown"        "poplar"        
# [7] "bur oak"


MBPSPtreeData <- MBPSPoutput$treeData
MBPSPtreeData <- standardizeSpeciesName(MBPSPtreeData, forestInventorySource = "MBPSP")

nrow(MBPSPtreeData[newSpeciesName == "unknown",])
# 25945

unique(MBPSPtreeData[newSpeciesName == "unknown",]$Species)
# [1] ""   "WE"

MBPSPtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]
unique(MBPSPtreeData[Biomass==0,]$newSpeciesName)
# [1] "unknown"        "back ash"      
# [3] "manitoba maple" "white oak"


MBTSPtreeData <- MBTSPoutput$treeData
MBTSPtreeData <- standardizeSpeciesName(MBTSPtreeData, forestInventorySource = "MBTSP")

nrow(MBTSPtreeData[newSpeciesName == "unknown",])
# 1733

unique(MBTSPtreeData[newSpeciesName == "unknown",]$Species) # need to fix this bug
# [1] "DC"  ""    "DD"  "ta"  "ba"  "ws"  "bs" 
# [8] "tl"  "bf"  "AE"  "GA"  " BS" "BW"  "jp" 
# [15] "wb"

MBTSPtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]
unique(MBTSPtreeData[Biomass==0,]$newSpeciesName)
# [1] "unknown"        "back ash"      
# [3] "manitoba maple" "white oak"

NWTTSPtreeData <- NWTTSPoutput$treeData
NWTTSPtreeData <- standardizeSpeciesName(NWTTSPtreeData, forestInventorySource = "NWTTSP")

nrow(NWTTSPtreeData[newSpeciesName == "unknown",])
# 0

NWTTSPtreeData <- NWTTSPtreeData[!is.na(DBH) & DBH != 0,] # need to fix this bug

NWTTSPtreeData[, Biomass:=biomassCalculation(species = newSpeciesName, DBH = DBH)$biomass]
unique(NWTTSPtreeData[Biomass==0,]$newSpeciesName) # willow
range(NWTTSPtreeData$DBH) # NA NA


