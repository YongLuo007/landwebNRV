################################################################################
#' purify Alberta mature PSP data that derived from original DAT files
#' 
#' 
#' @param treeData  data table, which is the tree data from DAT file, recordType == 2
#'
#' 
#' @param headerData,  data table. recordType == 1           
#'        
#'
#' @return  a data tables, the first one contains successfully standardized species.
#'          the newSpeciesName is the standardized name, unknown means the species in 
#'          the original species table can not be found according to manual
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
setGeneric("dataPurification_ABMature", function(treeData, headerData) {
  standardGeneric("dataPurification_ABMature")
})
#' @export
#' @rdname standardizeSpeciesName
setMethod(
  "dataPurification_ABMature",
  signature = c(speciesTable = "data.table", 
                forestInventorySource = "character"),
  definition = function(speciesTable, forestInventorySource) {
    rm(list=ls())
    pathw <- "C:/Users/yonluo/Documents/LandWeb/Data/AB"
    treeData <- read.csv(file.path(pathw, "ABMatureTreeData.csv"),
                         header=T,
                         stringsAsFactor = F) %>%
      data.table
    
    locations <- read.csv(file.path(pathw, "plotLocation.csv"),
                         header=T,
                         stringsAsFactor = F) %>%
      data.table
    treedata <- treeData[Treenumber != 0,]
    
    SAInfor <- treeData[Treenumber == 0 & (!is.na(DBHage) | !is.na(Stumpage)),]
    SADiff <- as.integer(mean(SAInfor[!is.na(DBHage) & !is.na(Stumpage)]$Stumpage-
                                SAInfor[!is.na(DBHage) & !is.na(Stumpage)]$DBHage))
    SAInfor <- SAInfor[!is.na(DBHage) & is.na(Stumpage), Stumpage:=DBHage+SADiff][
      ,.(Groupnumber, Plotnumber, MeasureYear, Stumpage)]
    SAInfor[,firstMeasureYear:=min(MeasureYear), by = c("Groupnumber")][
      ,treeAge:=Stumpage-MeasureYear+firstMeasureYear]
    SAInfor <- SAInfor[,.(Year = mean(firstMeasureYear), SA = round(mean(treeAge))), 
                       by = c("Groupnumber")]
    
    
    
    
  })