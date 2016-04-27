################################################################################
#' purify MB PSP data
#' 
#' 
#' @param MBPSPDataRaw  data table, which is the raw tree data from DAT file, recordType == 2
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
#' @rdname dataPurification_MBPSP
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_MBPSP", function(MBPSPDataRaw) {
  standardGeneric("dataPurification_MBPSP")
})
#' @export
#' @rdname dataPurification_MBPSP
setMethod(
  "dataPurification_MBPSP",
  signature = c(MBPSPDataRaw = "data.table"),
  definition = function(MBPSPDataRaw) {
    dbh <- grep("DBH",   names(MBPSPDataRaw))
    height <- grep("HGT",   names(MBPSPDataRaw))
    status <- grep("STATUS",   names(MBPSPDataRaw))
    dominance <- grep("CLASS",   names(MBPSPDataRaw))
    age <- grep("AGE",   names(MBPSPDataRaw))
    measyear <- grep("YEARMEA",   names(MBPSPDataRaw))
    plotsize <- grep("PLOTSIZE",   names(MBPSPDataRaw))
    mincut <- min(dbh, height, status, dominance, age, measyear, plotsize)
    treeData <- MBPSPDataRaw[0,1:24, with = FALSE]
    names(treeData)[18:24] <- c("DBH", "Height", "Status",
                                "Class", "Age", "MeasureYear",
                                "PlotSize")
    for(i in 1:length(dbh)){
      treeDataAdd <- MBPSPDataRaw[,c(1:17, dbh[i], height[i], status[i],
                                     dominance[i], age[i], measyear[i], plotsize[i]), with = FALSE]
      names(treeDataAdd)[18:24] <- c("DBH", "Height", "Status",
                                     "Class", "Age", "MeasureYear",
                                     "PlotSize")
      treeData <- rbind(treeData, treeDataAdd)
      rm(treeDataAdd)
    }
    treeData <- treeData[!is.na(DBH),]
    headData <- treeData[!is.na(Age) & (Class == 1 | Class == 2),]
    headData[,baseYear:=min(MeasureYear), by = PLOTID]
    headData[,baseSA:= as.integer(mean(Age-(MeasureYear-baseYear))), by = PLOTID]
    headData <- unique(headData, by  = "PLOTID")
    headData <- headData[STANDTYPE == "NAT",][,.(PLOTID, EASTING, NORTHING, 
                                                 PlotSize, baseYear, baseSA)]
    setnames(headData, c("EASTING", "NORTHING"),
             c("Easting", "Northing"))
    treeData <- treeData[PLOTID %in% headData$PLOTID,]
    treeData <- treeData[Status == 0 | Status == 1 | Status == 2,][
      ,.(PLOTID, TREENO, SPECIES, DBH, Height, MeasureYear)]
    setnames(treeData, c("TREENO", "SPECIES"), c("TreeNumber", "Species"))
    
    measureidtable <- unique(treeData[,.(PLOTID, MeasureYear)], by = c("PLOTID", "MeasureYear"))
    measureidtable[, MeasureID:=paste("MBPSP_", row.names(measureidtable), sep = "")]
    measureidtable <- measureidtable[,.(MeasureID, PLOTID, MeasureYear)]
    headData <- setkey(measureidtable, PLOTID)[setkey(headData, PLOTID), nomatch = 0]
    treeData <- setkey(measureidtable, PLOTID, MeasureYear)[setkey(treeData, PLOTID, MeasureYear),
                                                            nomatch = 0]
    treeData <- treeData[,.(MeasureID, OrigPlotID1 = PLOTID, OrigPlotID2 = NA, MeasureYear,
                            TreeNumber, Species,  DBH, Height)]
    headData <- headData[,.(MeasureID, OrigPlotID1 = PLOTID, MeasureYear, Longitude = NA,
                            Latitude = NA, Zone = 14, Easting, Northing, PlotSize = PlotSize/10000,
                            baseYear, baseSA)]
    
    return(list(plotHeaderData = headData,
                treeData = treeData))
  })
