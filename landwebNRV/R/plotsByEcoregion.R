################################################################################
#' this function is to classify plot into ecoregion in study region 
#' 
#' 
#' @param plotLocation  data.table. It must have three columns: PlotID, longitude and latitude.
#'
#' 
#' @param studyAreaEcoregionMap,  RasterLayer. It has ecoregion information in the study area.
#'        
#' 
#' 
#' @return a data table object. It has the PlotID for each ecoregion in the study area.
#' 
#' @importFrom sp coordinates proj4string
#' @importFrom raster crop raster extent crs rasterize getValues
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
                                        studyAreaEcoregionMap) {
  standardGeneric("plotsByEcoregion")
})
setMethod(
  "plotsByEcoregion",
  signature = c(plotLocation = "data.table", studyAreaEcoregionMap = "RasterLayer"),
  definition = function(plotLocation, studyAreaEcoregionMap) {
    coordinates(plotLocation) <- c("longitude", "latitude")
    proj4string(plotLocation) <- proj4string(studyAreaEcoregionMap)
    plotInStudyArea <- raster::crop(plotLocation, extent(studyAreaEcoregionMap))
    r <- raster(ncol=ncol(studyAreaEcoregionMap),
                nrow=nrow(studyAreaEcoregionMap))# based on south size )
    extent(r) <- extent(studyAreaEcoregionMap)
    crs(r) <- crs(studyAreaEcoregionMap)
    plotInStudyArea <- rasterize(plotInStudyArea, r, "PlotID")
    plotNumberEcoregionTable <- data.table(PlotID = getValues(plotInStudyArea),
                                           Ecoregion = getValues(studyAreaEcoregionMap))
    plotNumberEcoregionTable <- plotNumberEcoregionTable[!is.na(PlotID) & !is.na(Ecoregion),]
    return(plotNumberEcoregionTable)
  })


