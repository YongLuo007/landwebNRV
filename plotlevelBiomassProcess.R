rm(list=ls())
workPath <- "C:/Users/yonluo/Documents/GitHub/landwebNRV"
load(file.path(workPath, "alldata_purified.RData"))
source('~/GitHub/landwebNRV/landwebNRV/R/standardizeSpeciesName.R')
source('~/GitHub/landwebNRV/landwebNRV/R/biomassCalculation.R')
source('~/GitHub/landwebNRV/landwebNRV/R/speciesDominanceIdentification.R')
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
ABPSP_dominantSpecies <- speciesDominanceIdentification[]

