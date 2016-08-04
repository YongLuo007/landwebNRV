################################################################################
#' this function is to summarize the seralStage and vegetation type for a given polygon
#' after simulations 
#' 
#' 
#' @param focalArea  SpatialPolygons. Specify the area these two indices should derive from.
#'  
#'
#' @param summaryMaps RasterStack. This is the time seral stack from simulation output in LANDWEB.
#'                    Please note that the names of the stacked maps must be 'Year100' 
#' 
#' @param summaryPeriod,  Numeric. Providing period of the summary.
#'                        If missing, all the maps will be summarized.
#' 
#' @param patchClass, Numeric. Providing the minimum patch size should be summarized.
#'                      Default is 5000 ha.
#' 
#' @return one data table objects. It has the PlotID for each ecoregion in the study area.
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
#' @rdname postSimuSummary_Patch
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("postSimuSummary_Patch", function(focalArea,
                                             summaryMaps,
                                             summaryPeriod,
                                             patchClass) {
  standardGeneric("postSimuSummary_Patch")
})
setMethod(
  "postSimuSummary_Patch",
  signature = c(focalArea = "SpatialPolygons",
                summaryMaps = "RasterStack",
                summaryPeriod = "numeric",
                patchClass = "numeric"),
  definition = function(focalArea,
                        summaryMaps,
                        summaryPeriod,
                        patchClass) {
    focalArea <- spTransform(focalArea, crs(summaryMaps))
    subsetNames <- names(summaryMaps)[names(summaryMaps) <= paste("Year", max(summaryPeriod), sep = "") 
                                      & names(summaryMaps) >= paste("Year", min(summaryPeriod), sep = "")]
    summaryMaps_sub <- subset(summaryMaps, subsetNames)
    summaryMaps_sub <- crop(summaryMaps_sub, focalArea)
    summaryMaps_sub <- suppressWarnings(mask(summaryMaps_sub, focalArea))
    years <- as.numeric(sapply(strsplit(names(summaryMaps_sub), "Year", fixed = TRUE), function(x)(x[2])))
    # for each layer
    if(min(patchClass) != 0){
      patchClass <- c(0, patchClass)
    }
    patchClass <- c(patchClass, ncell(summaryMaps_sub)*(res(summaryMaps_sub)[1]^2)/10000)
    clumpMaps <- stack()
    output <- data.table(patchClasses = character(),
                         year = numeric(),
                         landType = numeric(),
                         NofPatch = numeric())
    for (indiYear in years){
      yearLayer1 <- subset(summaryMaps_sub, paste("Year", indiYear, sep = ""))
      types <- sort(unique(getValues(yearLayer1)))
      # for each type
      for(type in types){
        yearLayer <- yearLayer1
        yearLayer[Which(!is.na(yearLayer) & yearLayer != type, cells = TRUE)] <- NA
        clumpedLayer <- clump(yearLayer)
        names(clumpedLayer) <- paste("Year", indiYear, "_Type", type, sep = "")
        clumpMaps <- stack(clumpMaps, clumpedLayer)
        freqTable <- data.table(freq(clumpedLayer))[!is.na(value),]
        freqTable <- freqTable[,.(frequency = length(value)), by = count][
          , area:=count*(res(clumpedLayer)[1]^2)/10000]
        freqTable[, patchClasses := as.character(cut(freqTable$area, patchClass, right = FALSE))]
        outputAdd <- freqTable[,.(year = indiYear,
                                  landType = type,
                                  NofPatch = sum(frequency)),
                               by = patchClasses]
        output <- rbind(output, outputAdd)
      }
    }
    return(list(freqTable = output, clumppedMaps = clumpMaps))
  })



#' @export
#' @rdname postSimuSummary_Patch
setMethod(
  "postSimuSummary_Patch",
  signature = c(focalArea = "SpatialPolygons",
                summaryMaps = "RasterStack",
                summaryPeriod = "missing",
                patchClass = "numeric"),
  definition = function(focalArea, summaryMaps, patchClass) {
    postSimuSummary_Patch(focalArea = focalArea, 
                          summaryMaps = summaryMaps,
                          summaryPeriod = range(as.numeric(sapply(strsplit(names(summaryMaps), "Year", fixed = TRUE),
                                                                  function(x)(x[2])))),
                          patchClass = patchClass)
  })    

#' @export
#' @rdname postSimuSummary_Patch
setMethod(
  "postSimuSummary_Patch",
  signature = c(focalArea = "SpatialPolygons",
                summaryMaps = "RasterStack",
                summaryPeriod = "numeric",
                patchClass = "missing"),
  definition = function(focalArea, summaryMaps, summaryPeriod) {
    postSimuSummary_Patch(focalArea = focalArea, 
                          summaryMaps = summaryMaps,
                          summaryPeriod = summaryPeriod,
                          patchClass = 5000)
  })    



#' @export
#' @rdname postSimuSummary_Patch
setMethod(
  "postSimuSummary_Patch",
  signature = c(focalArea = "SpatialPolygons",
                summaryMaps = "RasterStack",
                summaryPeriod = "missing",
                patchClass = "missing"),
  definition = function(focalArea, summaryMaps) {
    postSimuSummary_Patch(focalArea = focalArea, 
                          summaryMaps = summaryMaps,
                          summaryPeriod = range(as.numeric(sapply(strsplit(names(summaryMaps), "Year", fixed = TRUE),
                                                                  function(x)(x[2])))),
                          patchClass = 5000)
  }) 


