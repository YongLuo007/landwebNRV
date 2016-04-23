################################################################################
#' purify SK TSP data of Pasquia Porcupine and Prince Albert.
#' 
#' 
#' @param sampleTreeRaw  data table, is age_samples in the MS access file
#' 
#' 
#' @param plotHeadRaw data.table, is plot_header in MS access file
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
#' @rdname dataPurification_SKTSP
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_SKTSP", function(sampleTreeRaw, plotHeadRaw,
                                              treeDataRaw) {
  standardGeneric("dataPurification_SKTSP")
})
#' @export
#' @rdname dataPurification_SKTSP
setMethod(
  "dataPurification_SKTSP",
  signature = c(sampleTreeRaw = "data.table", 
                plotHeadRaw = "data.table",
                treeDataRaw = "data.table"),
  definition = function(sampleTreeRaw, plotHeadRaw,
                        treeDataRaw) {
    options(scipen = 999) # avoid scientific notation
    # calculate SA from sample tree data for each plot
    # get dominant and codominant trees
    sampleTreeRaw <- sampleTreeRaw[CROWN_CL == 1 | CROWN_CL == 2,]
    sampleTreeRaw <- sampleTreeRaw[!is.na(TOT_AGE),][, SA:=0]
    plotids <- unique(sampleTreeRaw$PLOT_NUM)
    headData_SA <- sampleTreeRaw[0,][,PlotID:=0]
    k <- 1
    for(plotid in plotids){
      tempSA <- as.integer(mean(sampleTreeRaw[PLOT_NUM %==% plotid, ]$TOT_AGE))
      headData_SAoutput <- sampleTreeRaw[PLOT_NUM %==% plotid,][, SA:=tempSA]
      headData_SAoutput <- headData_SAoutput[1,][, PlotID:=k]
      k <- k+1
      headData_SA <- rbind(headData_SA, headData_SAoutput)
      rm(tempSA, headData_SAoutput)
    }
    # to avoid floating point problem
    headData_SA <- headData_SA[,.(PlotID, PLOT_NUM, SA)]
    rm(k, plotid, plotids)
    

    plotHeadRawtemp <- plotHeadRaw[!is.na(M_AREA),.(PLOT_NUM, GPS_ZONE, PLOT,
                                                        GPS_EASTING, GPS_NORTHING,
                                                        CR_YEAR, M_AREA)][
      !is.na(GPS_ZONE),][
      !is.na(GPS_EASTING),][
        !is.na(GPS_NORTHING),]
    
    plotids <- unique(headData_SA$PLOT_NUM)
    headData <- plotHeadRawtemp[0, ][, ':='(PLOT = NULL, SA = 0, PlotID = 0)]
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
                                                                  PlotID = headData_SA[PLOT_NUM %==% plotid,]$PlotID)]
        headData <- rbind(headData, subheadData_PSLocaAdd)
      }
      rm(subheadData_PSLocaAdd)
    }
    rm(plotid, plotids)
    setnames(headData, c("GPS_ZONE", "GPS_EASTING", "GPS_NORTHING", "CR_YEAR", "M_AREA"),
             c("Zone", "Easting", "Northing", "Year", "Plotsize"))
    
    
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
    treeData <- treeDataRawtemp[0,][,PlotID:=0]
    for(plotid in plotids){
      treeDataAdded <- treeDataRawtemp[PLOT_NUM %==% plotid, ]
      if(nrow(treeDataAdded)>0){
        treeDataAdded <- treeDataAdded[ , PlotID:=headData[PLOT_NUM %==% plotid,]$PlotID]
        treeData <- rbind(treeData, treeDataAdded)
      }
      rm(treeDataAdded)
    }
    setnames(treeData, c("TREE_NO", "SPECIES",  "TOT_HT"),
             c("Treenumber", "Species", "Height"))
    
    headData <- headData[PlotID %in% unique(treeData$PlotID),]
    return(list(treeData = treeData, headData = headData))
  })
