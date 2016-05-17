################################################################################
#' this function is to summarize the seralStage and vegetation type for a given polygon
#' after simulations 
#' 
#' 
#' @param focalArea  SpatialPolygons. Specify the area these two indices should derive from.
#'  
#'
#' @param summaryContent RasterStack. This is the time seral stack from simulation output in LANDWEB.
#'                    Please note that the names of the stacked maps must be 'Year100' 
#' 
#' @param summaryPeriod,  Numeric. Providing period of the summary.
#'                        If missing, all the maps will be summarized.
#' 
#'        
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
#' @rdname postSimuSummary
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("postSimuSummary", function(focalArea,
                                       summaryMaps,
                                       summaryPeriod) {
  standardGeneric("postSimuSummary")
})
setMethod(
  "postSimuSummary",
  signature = c(focalArea = "SpatialPolygons",
                summaryMaps = "RasterStack",
                summaryPeriod = "numeric"),
  definition = function(focalArea,
                        summaryMaps,
                        summaryPeriod) {
    focalArea <- spTransform(focalArea, crs(summaryMaps))
    subsetNames <- names(summaryMaps)[names(summaryMaps) <= paste("Year", max(summaryPeriod), sep = "") 
                                      & names(summaryMaps) >= paste("Year", min(summaryPeriod), sep = "")]
    summaryMaps_sub <- subset(summaryMaps, subsetNames)
    extractTable <- data.table(suppressWarnings(extract(summaryMaps_sub, focalArea)[[1]]))
    years <- as.numeric(sapply(strsplit(colnames(extractTable), "Year", fixed = TRUE), function(x)(x[2])))
    extractTable[,tempcol:=extractTable[,1, with = F]]
    extractTable <- extractTable[!is.na(tempcol),]
    extractTable[, totalCell:=length(tempcol)]
    allTypes <- sort(unique(extractTable$tempcol))
    extractTable[,tempcol:=NULL]
    output <- data.table(year = numeric(),
                         type = numeric(),
                         NofCell = numeric(),
                         totalCell = numeric(),
                         areaHa = numeric(),
                         areaPercentage = numeric())
    if(length(allTypes) > 0){
      for(indiYear in years){
        extractTable[,type := extractTable[, paste("Year", indiYear, sep = ""), with = F]]
        outputAdd <- data.table(year = indiYear,
                                type = allTypes)
        outputAdd2 <- extractTable[,.(NofCell = length(totalCell), totalCell = mean(totalCell)), by = type]
        outputAdd2[,':='(areaHa = NofCell*(res(summaryMaps)[1]^2)/10000,
                         areaPercentage = NofCell/totalCell)]
        outputAdd <- dplyr::left_join(outputAdd, outputAdd2, by = "type") %>%
          data.table 
        outputAdd <- outputAdd[,.(year, type, NofCell, totalCell, areaHa, areaPercentage)]
        output <- rbind(output, outputAdd)
      }
    }
    return(output)
  })



#' @export
#' @rdname postSimuSummary
setMethod(
  "postSimuSummary",
  signature = c(focalArea = "SpatialPolygons",
                summaryMaps = "RasterStack",
                summaryPeriod = "missing"),
  definition = function(focalArea, summaryMaps) {
    postSimuSummary(focalArea = focalArea, 
                    summaryMaps = summaryMaps,
                    summaryPeriod = range(as.numeric(sapply(strsplit(names(summaryMaps), "Year", fixed = TRUE),
                                                            function(x)(x[2])))))
  })    






