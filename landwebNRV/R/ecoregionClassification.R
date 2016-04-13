################################################################################
#' this function is to classify ecoregion in to study region 
#' based on ecoregion map and study area map
#' 
#' @param studyAreaMap  SpatialPolygons. This is study area map.
#'
#' 
#' @param ecoregionMap,  SpatialPolygonsDataFrame. It contains ecoregion information.
#'        
#' 
#' @param cellSize  Numeric. Determine the cell size for the generated map
#'        Default is 200 meters
#'
#' @return Two objects: studyareaecoregion is RasterLayer 
#'                      and attributesTable is data table
#'
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname ecoregionClassification
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("ecoregionClassification", function(studyAreaMap, ecoregionMap, cellSize) {
  standardGeneric("ecoregionClassification")
})

#' @export
#' @rdname ecoregionClassification
setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "SpatialPolygons", 
                ecoregionMap = "SpatialPolygonsDataFrame",
                cellSize = "numeric"),
  definition = function(studyAreaMap, 
                        ecoregionMap,
                        cellSize) {
    studyAreaMap <- sp::spTransform(studyAreaMap, crs(ecoregionMap))
    studyareaecoregion <- raster::crop(ecoregionMap, extent(studyAreaMap))
    r <- raster(ncol=round(1000*earthDist(long1=extent(studyareaecoregion)[1],
                                          lat1=extent(studyareaecoregion)[3],
                                          long2=extent(studyareaecoregion)[1],
                                          lat2=extent(studyareaecoregion)[4]))/cellSize,
                nrow=round(1000*earthDist(long1=extent(studyareaecoregion)[1],
                                          lat1=extent(studyareaecoregion)[3],
                                          long2=extent(studyareaecoregion)[2],
                                          lat2=extent(studyareaecoregion)[3]))/cellSize)# based on south size )
    extent(r) <- extent(studyareaecoregion)
    crs(r) <- crs(studyareaecoregion)
    studyarea1 <- rasterize(studyAreaMap, r, 1)
    studyareaecoregion <- rasterize(studyareaecoregion, r, "ECOZONE")
    studyareaecoregion <- raster::mask(studyareaecoregion, studyAreaMap)
    attributesTable <- data.table::data.table(ecoregionMap@data)
    attributesTable <- attributesTable[ECOZONE %in% unique(getValues(studyareaecoregion)),]
    return(list(studyareaecoregion = studyareaecoregion,
                attributesTable = attributesTable))
  })

#' @export
#' @rdname ecoregionClassification
setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "SpatialPolygons",
                ecoregionMap = "SpatialPolygonsDataFrame",
                cellSize = "missing"),
  definition = function(studyAreaMap, ecoregionMap) {
    ecoregionClassification(studyAreaMap = studyAreaMap, 
                            ecoregionMap = ecoregionMap, 
                            cellSize = 200)
  })    

# classify ecoregion based ecoregion map in the study region



earthDist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}






