################################################################################
#' this function is to summarize the seralStage and vegetation type for a given polygon
#' after simulations 
#' 
#' 
#' @param focalArea  SpatialPolygons. Specify the area these two indices should derive from.
#'
#' @param seralStageMap RasterLayer. The simulation output of seral stage map in LANDWEB 
#' 
#' @param vegTypeMap,  RasterLayer. The simulation output of vegetation type map in LANDWEB 
#'        
#' 
#' @return a data table object. It has the PlotID for each ecoregion in the study area.
#' 
#' @importFrom sp spTransform
#' @importFrom raster raster crop mask
#' @importFrom data.table data.table ':='
#' 
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname postSimuSummarize
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("postSimuSummarize", function(focalArea,
                                         seralStageMap,
                                         vegTypeMap) {
  standardGeneric("postSimuSummarize")
})
setMethod(
  "postSimuSummarize",
  signature = c(focalArea = "SpatialPolygons",
                seralStageMap = "RasterLayer",
                vegTypeMap = "RasterLayer"),
  definition = function(focalArea,
                        seralStageMap,
                        vegTypeMap) {
    focalArea <- spTransform(focalArea, crs(seralStageMap))
    extractTable <- data.table(seralStage = extract(seralStageMap, focalArea)[[1]],
                               vegType = extract(vegTypeMap, focalArea)[[1]])
    seralStageArea <- extractTable[,.(NofPixel=length(vegType)), by = seralStage][
      !is.na(seralStage),]
    seralStageArea[, ':='(area_ha = NofPixel*(res(seralStageMap)[1]^2)/10000,
                          areaPercentage = NofPixel/sum(NofPixel))]
    vegTypeArea <- extractTable[,.(NofPixel=length(seralStage)), by = vegType][
      !is.na(vegType),]
    vegTypeArea[, ':='(area_ha = NofPixel*(res(vegTypeMap)[1]^2)/10000,
                       areaPercentage = NofPixel/sum(NofPixel))]
    return(list(seralStageSummary = seralStageArea,
                vegTypeSummary = vegTypeArea))
  })
