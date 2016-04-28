################################################################################
#' purify SK TSP data from Mistic.
#' 
#' 
#' @param compiledPlotData  data table, 
#' 
#' 
#' @param compiledTreeData data.table,
#' 
#'        
#' @return  two data tables, the first one head data, which contains the location and SA info.
#'                           the second one is purified tree data, which contains inividual tree infor.
#'                           for the tree data, all trees are alive.
#' 
#' @importFrom data.table setnames setkey ':=' unique set                                         
#'
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname dataPurification_SKTSP_Mistic
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("dataPurification_SKTSP_Mistic", function(compiledPlotData, 
                                                     compiledTreeData) {
  standardGeneric("dataPurification_SKTSP_Mistic")
})
#' @export
#' @rdname dataPurification_SKTSP_Mistic
setMethod(
  "dataPurification_SKTSP_Mistic",
  signature = c(compiledPlotData = "data.table", 
                compiledTreeData = "data.table"),
  definition = function(compiledPlotData, 
                        compiledTreeData) {
    options(scipen = 999) # avoid scientific notation
    headData <- compiledPlotData[,.(ID_FOR, CRZ_ZONE, CRZNORTH, CRZ_EAST, 
                                            PLOTNUM, YEAR, PSIZE, P_AGECLS)]
    
    headData <- unique(headData, by = c("ID_FOR", "PLOTNUM"))
    headData[,PlotSize := sum(PSIZE), by = ID_FOR]
    headData <- unique(headData, by = "ID_FOR")[
      , ':='(PLOTNUM = NULL, PSIZE = NULL)]
    headData[,MeasureID:=paste("SKTSP_Mistik_", row.names(headData), sep = "")]
    setnames(headData, c("CRZNORTH", "CRZ_EAST", "CRZ_ZONE",
             "YEAR", "P_AGECLS"),
             c("Northing", "Easting", "Zone", "MeasureYear", "SA"))

    treeData <- compiledTreeData[,.(ID_FOR, TREENO, SPECIES, DBH,
                                               HEIGHT, CONDCOD1,
                                               CONDCOD2, CONDCOD3)]
    treeData <- treeData[ID_FOR %in% unique(headData$ID_FOR), ]
    # remove dead trees
    treeData <- treeData[CONDCOD1 != "DE",]
    treeData <- treeData[CONDCOD2 != "DE",]
    treeData <- treeData[CONDCOD3 != "DE",]
    set(treeData, ,c("CONDCOD1", "CONDCOD2", "CONDCOD3"),
        NULL)
    treeData <- setkey(headData[,.(MeasureID, ID_FOR, MeasureYear)], ID_FOR)[setkey(treeData, ID_FOR),
                                                                nomatch = 0]

    
    treeData <- treeData[,.(MeasureID, OrigPlotID1 = ID_FOR, OrigPlotID2 = NA, MeasureYear,
                            TreeNumber = TREENO, Species = SPECIES,  DBH, Height = HEIGHT)]
    
    headData <- headData[,.(MeasureID, OrigPlotID1 = ID_FOR, MeasureYear, Longitude = NA,
                            Latitude = NA, Zone, Easting = Easting*10000,
                            Northing = Northing*10000, PlotSize,
                            baseYear = MeasureYear, baseSA = SA)]
    
    return(list(plotHeaderData = headData, treeData = treeData))
  })
