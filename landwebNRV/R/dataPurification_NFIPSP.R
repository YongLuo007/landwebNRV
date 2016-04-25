################################################################################
#' purify NFI PSP data
#' 
#' 
#' @param lgtreeRaw  data table, which is the raw tree data from DAT file, recordType == 2
#'                  treeData can be obtained using obtainTreeDataAB function
#'                  
#'                  
#' @param lgtreeHeadRaw  data table, which is the raw tree data from DAT file, recordType == 2
#'                  treeData can be obtained using obtainTreeDataAB function
#'
#' @param standOrigin  data table, which is the raw tree data from DAT file, recordType == 2
#'                  treeData can be obtained using obtainTreeDataAB function
#'                  
#' @param lgtreeRaw  data table, which is the raw tree data from DAT file, recordType == 2
#'                  treeData can be obtained using obtainTreeDataAB function
#'                  
#' @param lgtreeAge  data table, which is the raw tree data from DAT file, recordType == 2
#'                  treeData can be obtained using obtainTreeDataAB function

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
#' @rdname dataPurification_NFIPSP
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_NFIPSP", function(MBTSPDataRaw) {
  standardGeneric("dataPurification_NFIPSP")
})
#' @export
#' @rdname dataPurification_NFIPSP
setMethod(
  "dataPurification_NFIPSP",
  signature = c(MBTSPDataRaw = "data.table"),
  definition = function(MBTSPDataRaw) {
    # get SA information using dominant and codominant trees
    MBTSPDataRaw <- MBTSPDataRaw[,Nofplot:=length(unique(PLOTNO)), by = TILE]
    
    headData <- MBTSPDataRaw[!is.na(AGE_BH) & (CC == "D" | CC == "C"),]
    headData <- headData[, SA:=as.integer(mean(AGE_BH))+8, by = TILE]
    headData[, Plotsize:=0.02*Nofplot]
    headData <- headData[,.(TILE, YEAR, EASTING, NORTHING, SA, Plotsize)]
    headData <- headData[!is.na(EASTING) & !is.na(NORTHING),]
    headData <- unique(headData, by = "TILE")
    setnames(headData, c("YEAR", "EASTING", "NORTHING"),
             c("Year", "Easting", "Northing"))
    
    treeData <- MBTSPDataRaw[TILE %in% unique(headData$TILE),][
      ,.(TILE, TREENO, SPP, DBH, HT, COND)]
    treeData <- treeData[!is.na(COND) | COND != 10,][,COND:=NULL]
    setnames(treeData, c("TILE", "TREENO", "SPP", "HT"),
             c("PlotID", "Treenumber", "Species", "Height"))
    setnames(headData, "TILE", "PlotID")
    return(list(headData = headData,
                treeData = treeData))
  })
