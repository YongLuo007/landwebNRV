################################################################################
#' purify MB TSP data
#' 
#' 
#' @param MBTSPDataRaw  data table, which is the raw tree data from DAT file, recordType == 2
#'                  treeData can be obtained using obtainTreeDataAB function
#'
#'        
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
#' @rdname dataPurification_MBTSP
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_MBTSP", function(MBTSPDataRaw) {
  standardGeneric("dataPurification_MBTSP")
})
#' @export
#' @rdname dataPurification_MBTSP
setMethod(
  "dataPurification_MBTSP",
  signature = c(MBTSPDataRaw = "data.table"),
  definition = function(MBTSPDataRaw) {
    # get SA information using dominant and codominant trees
    MBTSPDataRaw <- MBTSPDataRaw[,Nofplot:=length(unique(PLOTNO)), by = c("FMU", "TILE", "POLY")]
    
    MBTSPDataRaw <- MBTSPDataRaw[Nofplot == 3 & (!is.na(FMU) & !is.na(TILE) & !is.na(POLY)),] # based on manual, 1 and 2 should be removed
    tempheader <- unique(MBTSPDataRaw[,.(FMU, TILE, POLY)], by = c("FMU", "TILE", "POLY"))
    tempheader[, MeasureID:=paste("MBTSP_", row.names(tempheader), sep = "")]
    MBTSPDataRaw <- setkey(MBTSPDataRaw, FMU, TILE, POLY)[setkey(tempheader, FMU, TILE, POLY),
                                                          nomatch = 0]
    MBTSPDataRaw[,OrigPlotID1:=paste(FMU, "_", TILE, "_", POLY, sep = "")]
        
    headData <- MBTSPDataRaw[!is.na(AGE_BH) & (CC == "D" | CC == "C"),]
    headData <- headData[, baseSA:=as.integer(mean(AGE_BH))+8, by = MeasureID]
    headData[, PlotSize:=0.02*Nofplot]
    
    headData <- headData[,.(MeasureID, OrigPlotID1, YEAR, EASTING, NORTHING, baseSA, PlotSize)]
    headData <- headData[!is.na(EASTING) & !is.na(NORTHING),]
    headData <- unique(headData, by = "MeasureID")
    setnames(headData, c("YEAR", "EASTING", "NORTHING"),
             c("MeasureYear", "Easting", "Northing"))
    
    treeData <- MBTSPDataRaw[MeasureID %in% unique(headData$MeasureID),][
      ,.(MeasureID, OrigPlotID1, TREENO, SPP, DBH, HT, COND)]
    treeData <- treeData[!is.na(COND) | COND != 10,][,COND:=NULL]
    treeData <- treeData[SPP != "DD" & SPP != "DC" & SPP != "DU",] # species code for DD is dead deciduous standing
                                                 # species code for DC is dead coniferous standing
                                                 # species code for DU is dead unknown standing
    setnames(treeData, c("TREENO", "SPP", "HT"),
             c("TreeNumber", "Species", "Height"))

    headData <- headData[,.(MeasureID, OrigPlotID1, MeasureYear, Longitude = NA,
                            Latitude = NA, Zone = 14, Easting, Northing, PlotSize,
                            baseYear = MeasureYear, baseSA)]
    treeData <- setkey(headData[,.(MeasureID, MeasureYear)], MeasureID)[setkey(treeData, MeasureID),
                                                                        nomatch = 0]
    treeData <- treeData[,.(MeasureID, OrigPlotID1, OrigPlotID2 = NA, MeasureYear,
                            TreeNumber, Species,  DBH, Height)]
    return(list(plotHeaderData = headData,
                treeData = treeData))
  })
