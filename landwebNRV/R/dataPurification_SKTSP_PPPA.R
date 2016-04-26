################################################################################
#' purify SK TSP data of Pasquia Porcupine and Prince Albert.
#' 
#' 
#' @param sampleTreeRaw  data table, is age_samples in the MS access file
#' 
#' 
#' @param plotHeaderRaw data.table, is plot_header in MS access file
#' 
#'        
#' @param treeDataRaw data.table, is trees in MS access file
#'
#' @return  two data tables, the first one head data, which contains the location and SA info.
#'                           the second one is purified tree data, which contains inividual tree infor.
#'                           for the tree data, all trees are alive.
#'                      
#'
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname dataPurification_SKTSP_PPPA
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_SKTSP_PPPA", function(sampleTreeRaw, plotHeaderRaw,
                                              treeDataRaw) {
  standardGeneric("dataPurification_SKTSP_PPPA")
})
#' @export
#' @rdname dataPurification_SKTSP_PPPA
setMethod(
  "dataPurification_SKTSP_PPPA",
  signature = c(sampleTreeRaw = "data.table", 
                plotHeaderRaw = "data.table",
                treeDataRaw = "data.table"),
  definition = function(sampleTreeRaw, plotHeaderRaw,
                        treeDataRaw) {
    rm(list=ls())
    setwd("~/LandWeb/Data/SK")
    load("SKTSP_PAPP.RData")
    sampleTreeRaw <- PPsampletree
    plotHeaderRaw <- PPplotheader
    treeDataRaw <- PPtreedata
    
    options(scipen = 999) # avoid scientific notation
    # calculate SA from sample tree data for each plot
    # get dominant and codominant trees
    sampleTreeRaw <- sampleTreeRaw[CROWN_CL == 1 | CROWN_CL == 2,]
    sampleTreeRaw <- sampleTreeRaw[!is.na(TOT_AGE),][, SA:=0]
    plotids <- unique(sampleTreeRaw$PLOT_NUM)
    headData_SA <- sampleTreeRaw[0,][,MeasureID:=NA]
    k <- 1
    for(plotid in plotids){
      tempSA <- as.integer(mean(sampleTreeRaw[PLOT_NUM %==% plotid, ]$TOT_AGE))
      headData_SAoutput <- sampleTreeRaw[PLOT_NUM %==% plotid,][, SA:=tempSA]
      headData_SAoutput <- headData_SAoutput[1,][, MeasureID:=paste("SKTSP_PPPA_", k, sep = "")]
      k <- k+1
      headData_SA <- rbind(headData_SA, headData_SAoutput)
      rm(tempSA, headData_SAoutput)
    }
    # to avoid floating point problem
    headData_SA <- headData_SA[,.(MeasureID, PLOT_NUM, SA)]
    rm(k, plotid, plotids)
    

    plotHeadRawtemp <- plotHeaderRaw[!is.na(M_AREA),.(PLOT_NUM, GPS_ZONE, PLOT,
                                                        GPS_EASTING, GPS_NORTHING,
                                                        CR_YEAR, M_AREA)][
      !is.na(GPS_ZONE),][
      !is.na(GPS_EASTING),][
        !is.na(GPS_NORTHING),]
    
    plotids <- unique(headData_SA$PLOT_NUM)
    headData <- plotHeadRawtemp[0, ][, ':='(PLOT = NULL, SA = 0, MeasureID = 0)]
    for(plotid in plotids){
      subheadData_PSLocaAdd <- plotHeadRawtemp[PLOT_NUM %==% plotid,]
      if(nrow(subheadData_PSLocaAdd)>0){
        subheadData_PSLocaAdd <- unique(subheadData_PSLocaAdd, by = "PLOT")
        subheadData_PSLocaAdd <- subheadData_PSLocaAdd[,':='(GPS_ZONE = mean(GPS_ZONE),
                                                             GPS_EASTING = mean(GPS_EASTING),
                                                             GPS_NORTHING = mean(GPS_NORTHING),
                                                             M_AREA = sum(M_AREA))]
        subheadData_PSLocaAdd <- subheadData_PSLocaAdd[1,][, ':='(PLOT = NULL,
                                                                  SA = headData_SA[PLOT_NUM %==% plotid,]$SA,
                                                                  MeasureID = headData_SA[PLOT_NUM %==% plotid,]$MeasureID)]
        headData <- rbind(headData, subheadData_PSLocaAdd)
      }
      rm(subheadData_PSLocaAdd)
    }
    rm(plotid, plotids)
    setnames(headData, c("GPS_ZONE", "GPS_EASTING", "GPS_NORTHING", "CR_YEAR", "M_AREA"),
             c("Zone", "Easting", "Northing", "MeasureYear", "PlotSize"))
    
    
    # for tree data
    treeDataRawtemp <- treeDataRaw[,.(PLOT_NUM, TREE_NO, SPECIES, DBH, TOT_HT,
                                  CONCODE1, CONCODE2, CONCODE3, CONCODE4, CONCODE5, CONCODE6)]
    treeDataRawtemp <- treeDataRawtemp[CONCODE1 != 25 | is.na(CONCODE1),]
    treeDataRawtemp <- treeDataRawtemp[CONCODE2 != 25 | is.na(CONCODE2),]
    treeDataRawtemp <- treeDataRawtemp[CONCODE3 != 25 | is.na(CONCODE3),]
    treeDataRawtemp <- treeDataRawtemp[CONCODE4 != 25 | is.na(CONCODE4),]
    treeDataRawtemp <- treeDataRawtemp[CONCODE5 != 25 | is.na(CONCODE5),]
    treeDataRawtemp <- treeDataRawtemp[CONCODE6 != 25 | is.na(CONCODE6),]
    set(treeDataRawtemp, ,c("CONCODE1", "CONCODE2", "CONCODE3", "CONCODE4", "CONCODE5", "CONCODE6"),
        NULL)
    plotids <- unique(headData$PLOT_NUM)
    treeData <- treeDataRawtemp[0,][,MeasureID:=0]
    for(plotid in plotids){
      treeDataAdded <- treeDataRawtemp[PLOT_NUM %==% plotid, ]
      if(nrow(treeDataAdded)>0){
        treeDataAdded <- treeDataAdded[ , MeasureID:=headData[PLOT_NUM %==% plotid,]$MeasureID]
        treeData <- rbind(treeData, treeDataAdded)
      }
      rm(treeDataAdded)
    }
    setnames(treeData, c("TREE_NO", "SPECIES",  "TOT_HT"),
             c("TreeNumber", "Species", "Height"))
    headData <- headData[MeasureID %in% unique(treeData$MeasureID),]
    headData <- headData[,.(MeasureID, OrigPlotID1 = PLOT_NUM, MeasureYear, Longitude = NA,
                            Latitude = NA, Zone, Easting, Northing, PlotSize, baseYear = MeasureYear,
                            baseSA = SA)]
    treeData <- setkey(treeData, MeasureID)[setkey(headData[,.(MeasureID, MeasureYear)], MeasureID),
                                            nomatch = 0]
    treeData <- treeData[,.(MeasureID, OrigPlotID1 = PLOT_NUM, OrigPlotID2 = NA, MeasureYear,
                            TreeNumber, Species,  DBH, Height)]
    return(list(plotHeaderData = headData, treeData = treeData))
  })
