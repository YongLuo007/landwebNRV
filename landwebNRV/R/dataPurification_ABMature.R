################################################################################
#' purify Alberta mature PSP data that derived from original DAT files
#' 
#' 
#' @param treeDataRaw  data table, which is the raw tree data from DAT file, recordType == 2
#'                  treeData can be obtained using obtainTreeDataAB function
#'
#' 
#' @param headDataRaw data.table, which is PSPDATA2015B, provided by AB
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
#' @rdname dataPurification_ABMature
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_ABMature", function(treeDataRaw, headDataRaw) {
  standardGeneric("dataPurification_ABMature")
})
#' @export
#' @rdname dataPurification_ABMature
setMethod(
  "dataPurification_ABMature",
  signature = c(treeDataRaw = "data.table", 
                headDataRaw = "data.table"),
  definition = function(treeDataRaw, headDataRaw) {
    
    # generate head data for each plot, unmanaged, SA available, location available
    headData1 <- treeDataRaw[Treenumber == 0 & (!is.na(DBHage) | !is.na(Stumpage)),]
    SADiff <- as.integer(mean(headData1[!is.na(DBHage) & !is.na(Stumpage)]$Stumpage-
                                headData1[!is.na(DBHage) & !is.na(Stumpage)]$DBHage))
    headData1 <- headData1[!is.na(DBHage) & is.na(Stumpage), Stumpage:=DBHage+SADiff][
      ,.(Groupnumber, Plotnumber, MeasureYear, Stumpage)]
    headData1[,firstMeasureYear:=min(MeasureYear), by = c("Groupnumber")][
      ,treeAge:=Stumpage-MeasureYear+firstMeasureYear]
    headData1 <- headData1[,.(baseYear = mean(firstMeasureYear), baseSA = round(mean(treeAge))), 
                           by = c("Groupnumber")]
    headData1[,Groupnumber:=as.character(Groupnumber)]
    
    setnames(headDataRaw, c("PLOT..", "TYPE", "PLOTS", "DEC...LONG", "DEC...LAT",
                          "Plot.Size.m2", "Stand.origin", "Managed."),
             c("Groupnumber", "Type", "NofSubplot", "Longitude", "Latitude",
               "PlotSize", "StandOrigin", "Managed"))
    # select PSPs
    headDataRaw <- headDataRaw[,.(Groupnumber, Type, NofSubplot, Longitude, Latitude,Easting,
                              Northing, Elevation,
                              PlotSize, StandOrigin, Managed)]
    # select the plots with locations
    headDataRaw <- headDataRaw[(Longitude !=0 & Latitude != 0) | (Northing != 0 & Easting != 0),]
    # select plots unmanaged
    headDataRaw <- headDataRaw[Managed == "No",]
    
    # joining the SA information
    headData <- setkey(headDataRaw, Groupnumber)[setkey(headData1, Groupnumber), nomatch  = 0][
      ,':='(Type = NULL, NofSubplot = NULL, StandOrigin = NULL, Managed = NULL)]
    headData <- unique(headData, by = "Groupnumber")
    
    treeData <- treeDataRaw[Treenumber != 0,][
      ,.(Groupnumber, Plotnumber, MeasureYear, Treenumber, 
         Species, DBH, Height, Conditioncode1, Conditioncode2, Conditioncode3, Treeplotsize)]
    # remove DBH is not available
    treeData <- treeData[!is.na(DBH) & DBH != 0,]
    
    treeData <- treeData[Groupnumber %in% unique(as.numeric(headData$Groupnumber))]
    tempPlotID <- unique(treeData[,.(Groupnumber, Plotnumber)], by = c("Groupnumber", "Plotnumber"))
    tempPlotID[,PlotID := as.numeric(row.names(tempPlotID))]
    setkey(tempPlotID, Groupnumber, Plotnumber)
    treeData <- setkey(treeData, Groupnumber, Plotnumber)[tempPlotID, nomatch = 0]
    

      
    # treeData condition check
    treeDataLiving <- treeData[Conditioncode1 != 25 & Conditioncode1 != 61 &
                                 Conditioncode1 != 79 & Conditioncode1 != 80,]
    
    treeDataLiving <- treeDataLiving[is.na(Conditioncode2) |
                                       (Conditioncode2 != 25 & Conditioncode2 != 61 &
                                          Conditioncode2 != 79 & Conditioncode2 != 80),]
    treeDataLiving <- treeDataLiving[is.na(Conditioncode3) |
                                       (Conditioncode3 != 25 & Conditioncode3 != 61 &
                                          Conditioncode2 != 79 & Conditioncode2 != 80),]
    treeDataLiving[,':='(Conditioncode1 = NULL, Conditioncode2 = NULL, Conditioncode3 = NULL)]
    
    # check the plot size
    treeDataLiving[,plotsizetime:=as.numeric(length(unique(Treeplotsize))), by = c("PlotID")]
    
    if(nrow(treeDataLiving[plotsizetime == 2,])>0){
      plotids <- unique(treeDataLiving[plotsizetime == 2,]$PlotID)
      for(plotid in plotids){
        groupnumber <- as.character(unique(treeDataLiving[PlotID == plotid,]$Groupnumber))
        plotsize <- as.numeric(headData[Groupnumber == groupnumber,]$PlotSize) # obtain plot size from headData
        treeDataLiving[PlotID == plotid, Treeplotsize := plotsize]
      }
    }
    setnames(treeDataLiving, "Treeplotsize", "Plotsize")
    plotiddata <- setkey(unique(treeDataLiving[,.(PlotID, Groupnumber, Plotsize)], by = "PlotID"), Groupnumber)
    headData[,':='(Groupnumber = as.numeric(Groupnumber),
                   PlotSize = NULL)]
    headData <- plotiddata[setkey(headData, Groupnumber), nomatch  = 0]
    return(list(headData = headData[,':='(Longitude = -(Longitude), PlotSize=NULL)],
                treeData = treeDataLiving[, ':='(DBH = DBH/10, plotsizetime = NULL, Plotsize = NULL)]))
  })
