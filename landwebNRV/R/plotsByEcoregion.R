################################################################################
#' this function is to classify ecoregion in to study region 
#' based on ecoregion map and study area map
#' 
#' @param studyAreaMap  Character string. The name of study area map.
#'
#' @param studyAreaMapPath  Character string. The path direct to the study area map.
#'        Default is the current working directory.
#' 
#' @param ecoregionMap,  Character string. The name of ecoregion map.
#'        
#' 
#' @param ecoregionMapPath  Character string. The path direct to the ecoregion map.
#'        Default is the current working directory.
#' 
#' @param cellSize  Numeric. Determine the cell size for the generated map
#'        Default is 200 meters
#'
#' @return 
#'
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname plotsByEcoregion
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("plotsByEcoregion", function(plotLocation, studyAreaMapPath,
                                        ecoregionMap, ecoregionMapPath,
                                        cellSize) {
  standardGeneric("ecoregionClassification")
})

setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "character", studyAreaMapPath = "character",
                ecoregionMap = "character", ecoregionMapPath = "character",
                cellSize = "numeric"),
  definition = function(studyAreaMap, studyAreaMapPath,
                        ecoregionMap, ecoregionMapPath,
                        cellSize) {
    
  })


