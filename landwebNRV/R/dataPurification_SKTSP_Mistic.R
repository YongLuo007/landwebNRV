################################################################################
#' purify SK TSP data from Mistic.
#' 
#' 
#' @param compiledPlotData  data table, is age_samples in the MS access file
#' 
#' 
#' @param compiledTreeData data.table, is plot_header in MS access file
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
    headData[,Plotsize := sum(PSIZE), by = ID_FOR]
    headData <- unique(headData, by = "ID_FOR")[
      , ':='(PLOTNUM = NULL, PSIZE = NULL)]
    headData[,PlotID:=as.numeric(row.names(headData))]
    setnames(headData, c("CRZNORTH", "CRZ_EAST", "CRZ_ZONE",
             "YEAR", "P_AGECLS"),
             c("Northing", "Easting", "Zone", "Year", "SA"))
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
    setnames(treeData, c("TREENO", "SPECIES", "HEIGHT"),
             c("Treenumber", "Species", "Height"))
    
    return(list(treeData = treeData, headData = headData))
  })
