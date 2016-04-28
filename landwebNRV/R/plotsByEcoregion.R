################################################################################
#' this function is to classify plot into ecoregion in study region 
#' 
#' 
#' @param plotLocation  data.table. It must have three columns: PlotID, longitude and latitude.
#'
#' @param plotID character. Specify the plot identity in the plotLocation table
#' 
#' @param studyAreaEcoregionMap,  RasterLayer. It has ecoregion information in the study area.
#'        
#' @param ecoregionID, character. Specify the ecoregion identity in the studyAreaEcoregionMap.
#' 
#' @return a data table object. It has the PlotID for each ecoregion in the study area.
#' 
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom raster intersect
#' @importFrom data.table data.table
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
setGeneric("plotsByEcoregion", function(plotLocation,
                                        plotID,
                                        studyAreaEcoregionMap,
                                        ecoregionID) {
  standardGeneric("plotsByEcoregion")
})
setMethod(
  "plotsByEcoregion",
  signature = c(plotLocation = "data.table", 
                plotID = "character",
                studyAreaEcoregionMap = "SpatialPolygonsDataFrame",
                ecoregionID = "character"),
  definition = function(plotLocation, plotID, 
                        studyAreaEcoregionMap, ecoregionID) {
    plotLocation <- SpatialPointsDataFrame(plotLocation[,.(Longitude, Latitude)],
                                           plotLocation[, plotID, with = FALSE],
                                           match.ID = TRUE,
                                           proj4string = crs(studyAreaEcoregionMap))
    plotNumberEcoregionMap <- intersect(plotLocation, studyAreaEcoregionMap)
    plotNumberEcoregionTable <- data.table(plotNumberEcoregionMap@data)[
      , c(plotID, ecoregionID), with = F]
    return(plotNumberEcoregionTable)
  })


#' @export
#' @rdname plotsByEcoregion
setMethod(
  "plotsByEcoregion",
  signature = c(plotLocation = "data.table", 
                plotID = "missing",
                studyAreaEcoregionMap = "SpatialPolygonsDataFrame",
                ecoregionID = "character"),
  definition = function(plotLocation,
                        studyAreaEcoregionMap,
                        ecoregionID) {
    plotsByEcoregion(plotLocation = plotLocation, 
                     plotID = "MeasureID",
                     studyAreaEcoregionMap = studyAreaEcoregionMap,
                     ecoregionID = ecoregionID)
  })    

#' @export
#' @rdname plotsByEcoregion
setMethod(
  "plotsByEcoregion",
  signature = c(plotLocation = "data.table", 
                plotID = "character",
                studyAreaEcoregionMap = "SpatialPolygonsDataFrame",
                ecoregionID = "missing"),
  definition = function(plotLocation,
                        plotID,
                        studyAreaEcoregionMap) {
    plotsByEcoregion(plotLocation = plotLocation, 
                     plotID = plotID,
                     studyAreaEcoregionMap = studyAreaEcoregionMap,
                     ecoregionID = "ECOZONE")
  })   


#' @export
#' @rdname plotsByEcoregion
setMethod(
  "plotsByEcoregion",
  signature = c(plotLocation = "data.table", 
                plotID = "missing",
                studyAreaEcoregionMap = "SpatialPolygonsDataFrame",
                ecoregionID = "missing"),
  definition = function(plotLocation,
                        studyAreaEcoregionMap) {
    plotsByEcoregion(plotLocation = plotLocation, 
                     plotID = "MeasureID",
                     studyAreaEcoregionMap = studyAreaEcoregionMap,
                     ecoregionID = "ECOZONE")
  })   
