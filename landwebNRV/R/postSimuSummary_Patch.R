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
#' @param minPatchSize, Numeric. Providing the minimum patch size should be summarized.
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
                                             minPatchSize) {
  standardGeneric("postSimuSummary_Patch")
})
setMethod(
  "postSimuSummary_Patch",
  signature = c(focalArea = "SpatialPolygons",
                summaryMaps = "RasterStack",
                summaryPeriod = "numeric",
                minPatchSize = "numeric"),
  definition = function(focalArea,
                        summaryMaps,
                        summaryPeriod,
                        minPatchSize) {
    ecoDisFull <- shapefile("M:/data/ecoFramework/Ecodistricts/ecodistricts.shp")
    summaryMaps <- readRDS("M:/data/LandWeb/BorealPlain simulation/outputs/seralStageMapStackRep1.rds")
    ecoDisFull <- spTransform(ecoDisFull,crs(summaryMaps))
    ecoDis <- crop(ecoDisFull, summaryMaps)
    
    summarizedRegion <- sort(unique(ecoDis$ECOREGION)) # could be FMA or area owned by companies.
    focalArea <- ecoDis[ecoDis$ECOREGION == summarizedRegion[2],]
    summaryPeriod <- c(1400, 1500)
    focalArea <- spTransform(focalArea, crs(summaryMaps))
    summaryMaps_sub <- crop(summaryMaps, focalArea)
    summaryMaps_sub <- suppressWarnings(mask(summaryMaps_sub, focalArea))
    subsetNames <- names(summaryMaps)[names(summaryMaps) <= paste("Year", max(summaryPeriod), sep = "") 
                                      & names(summaryMaps) >= paste("Year", min(summaryPeriod), sep = "")]
    summaryMaps_sub <- subset(summaryMaps_sub, subsetNames)
    years <- as.numeric(sapply(strsplit(names(summaryMaps_sub), "Year", fixed = TRUE), function(x)(x[2])))
    # for each layer
    output <- data.table(year = numeric(),
                         landType = numeric(),
                         NofPatch = numeric())
    for (indiYear in years){
      yearLayer1 <- subset(summaryMaps_sub, paste("Year", indiYear, sep = ""))
      types <- sort(unique(getValues(yearLayer1)))
      # for each type
      for(type in types){
        yearLayer <- yearLayer1
        yearLayer[Which(!is.na(yearLayer) & yearLayer != type, cells = TRUE)] <- 0
        clumpedLayer <- clump(yearLayer)
        
        freqTable <- data.table(freq(clumpedLayer))[!is.na(value),]
        freqTable <- freqTable[,.(frequency = length(value)), by = count][
          , area:=count*(res(clumpedLayer)[1]^2)/10000][area >= minPatchSize,]
        outputAdd <- data.table(year = indiYear,
                                landType = type,
                                NofPatch = sum(freqTable$frequency))
        output <- rbind(output, outputAdd)
      }
      
    }
    
    
    
    
    return(output)
  })



#' @export
#' @rdname postSimuSummary_Patch
setMethod(
  "postSimuSummary_Patch",
  signature = c(focalArea = "SpatialPolygons",
                summaryMaps = "RasterStack",
                summaryPeriod = "missing"),
  definition = function(focalArea, summaryMaps) {
    postSimuSummary_Patch(focalArea = focalArea, 
                          summaryMaps = summaryMaps,
                          summaryPeriod = range(as.numeric(sapply(strsplit(names(summaryMaps), "Year", fixed = TRUE),
                                                                  function(x)(x[2])))))
  })    






