################################################################################
#' this function is to classify plot into ecoregion in study region 
#' 
#' 
#' @param plotHeader  data.table. It must have three columns: PlotID, longitude and latitude.
#'
#' @param plotID character. Specify the plot identity in the plotLocation table
#' 
#' @param pureStandBiomass data.table. It must be connected to plot header by plotID
#' 
#' @param ecoregionMap,  RasterLayer. It has ecoregion information in the study area.
#' 
#'        
#' 
#' @return a data table object. It has the PlotID for each ecoregion in the study area.
#' 
#' @importFrom sp SpatialPointsDataFrame spTransform
#' @importFrom raster crop
#' @importFrom data.table data.table setkey ':='
#' 
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname biomassAttributes_Groundplots
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("biomassAttributes_Groundplots", function(plotHeader,
                                                     plotID,
                                                     pureStandBiomass,
                                                     ecoregionMap) {
  standardGeneric("biomassAttributes_Groundplots")
})
setMethod(
  "biomassAttributes_Groundplots",
  signature = c(plotHeader = "data.table",
                plotID = "character",
                pureStandBiomass = "data.table",
                ecoregionMap = "RasterLayer"),
  definition = function(plotHeader,
                        plotID,
                        pureStandBiomass,
                        ecoregionMap) {
    plotLocation <- SpatialPointsDataFrame(plotHeader[,.(Longitude, Latitude)],
                                           plotHeader[, plotID, with = FALSE],
                                           match.ID = TRUE,
                                           proj4string = crs("+init=epsg:4326"))
    plotLocation <- spTransform(plotLocation, crs(ecoregionMap))
    plotNumberEcoregionMap <- crop(plotLocation, ecoregionMap)
    
    plotNumberEcoregionTable <- data.table(plotID = plotNumberEcoregionMap@data[,1],
                                           ecoregion = extract(ecoregionMap, plotNumberEcoregionMap))[!is.na(ecoregion),]
    
    setnames(pureStandBiomass, plotID, "plotID")
    pureStandInStudyArea <- setkey(pureStandBiomass, plotID)[setkey(plotNumberEcoregionTable, plotID),
                                                             nomatch = 0]
    biomassAttributes <- pureStandInStudyArea[,.(maxBiomass = quantile(Biomass, 0.8)),
                                              by = c("ecoregion", "Species")]
    biomassAttributes[,maxANPP:=maxBiomass/30]
    return(biomassAttributes)
  })


#' @export
#' @rdname biomassAttributes_Groundplots
setMethod(
  "biomassAttributes_Groundplots",
  signature = c(plotHeader = "data.table",
                plotID = "missing",
                pureStandBiomass = "data.table",
                ecoregionMap = "RasterLayer"),
  definition = function(plotHeader,
                        pureStandBiomass,
                        ecoregionMap) {
    biomassAttributes_Groundplots(plotHeader = plotHeader, 
                     plotID = "MeasureID",
                     pureStandBiomass = pureStandBiomass,
                     ecoregionMap = ecoregionMap)
  })    
