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
#' @rdname ecoregionClassification
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("ecoregionClassification", function(studyAreaMap, studyAreaMapPath,
                                               ecoregionMap, ecoregionMapPath,
                                               cellSize) {
  standardGeneric("ecoregionClassification")
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "character", studyAreaMapPath = "character",
                ecoregionMap = "character", ecoregionMapPath = "character",
                cellSize = "numeric"),
  definition = function(studyAreaMap, studyAreaMapPath,
                        ecoregionMap, ecoregionMapPath,
                        cellSize) {
    studyarea <- readRDS(file.path(studyAreaMapPath, studyAreaMap)) 
    ecoregionMap <- rgdal::readOGR(file.path(ecoregionMapPath, ecoregionMap),
                                   layer = "ecozones")
    studyarea <- spTransform(studyarea, crs(ecoregionMap))
    studyareaecoregion <- crop(ecoregionMap, extent(studyarea))
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
    studyarea1 <- rasterize(studyarea, r, 1)
    studyareaecoregion <- rasterize(studyareaecoregion, r, "ECOZONE")
    studyareaecoregion <- mask(studyareaecoregion, studyarea)
    return(studyareaecoregion)
  })

#' @export
#' @rdname ecoregionClassification
setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "character", studyAreaMapPath = "missing",
                ecoregionMap = "character", ecoregionMapPath = "character",
                cellSize = "numeric"),
  definition = function(studyAreaMap, ecoregionMap,
                        ecoregionMapPath, cellSize) {
    ecoregionClassification(studyAreaMap = studyAreaMap, studyAreaMapPath = ".",
                            ecoregionMap = ecoregionMap, ecoregionMapPath = ecoregionMapPath,
                            cellSize = cellSize)
  })    

#' @export
#' @rdname ecoregionClassification
setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "character", studyAreaMapPath = "character",
                ecoregionMap = "character", ecoregionMapPath = "missing",
                cellSize = "numeric"),
  definition = function(studyAreaMap, studyAreaMapPath,
                        ecoregionMap, cellSize) {
    ecoregionClassification(studyAreaMap = studyAreaMap, studyAreaMapPath = studyAreaMapPath,
                            ecoregionMap = ecoregionMap, ecoregionMapPath = ".",
                            cellSize = cellSize)
  })   

#' @export
#' @rdname ecoregionClassification
setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "character", studyAreaMapPath = "character",
                ecoregionMap = "character", ecoregionMapPath = "character",
                cellSize = "missing"),
  definition = function(studyAreaMap, studyAreaMapPath,
                        ecoregionMap, ecoregionMapPath) {
    ecoregionClassification(studyAreaMap = studyAreaMap, studyAreaMapPath = studyAreaMapPath,
                            ecoregionMap = ecoregionMap, ecoregionMapPath = ecoregionMapPath,
                            cellSize = 200)
  }) 

#' @export
#' @rdname ecoregionClassification
setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "character", studyAreaMapPath = "missing",
                ecoregionMap = "character", ecoregionMapPath = "missing",
                cellSize = "numeric"),
  definition = function(studyAreaMap, ecoregionMap, cellSize) {
    ecoregionClassification(studyAreaMap = studyAreaMap, studyAreaMapPath = ".",
                            ecoregionMap = ecoregionMap, ecoregionMapPath = ".",
                            cellSize = cellSize)
  }) 

#' @export
#' @rdname ecoregionClassification
setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "character", studyAreaMapPath = "missing",
                ecoregionMap = "character", ecoregionMapPath = "character",
                cellSize = "missing"),
  definition = function(studyAreaMap, ecoregionMap, ecoregionMapPath) {
    ecoregionClassification(studyAreaMap = studyAreaMap, studyAreaMapPath = ".",
                            ecoregionMap = ecoregionMap, ecoregionMapPath = ecoregionMapPath,
                            cellSize = 200)
  }) 

#' @export
#' @rdname ecoregionClassification
setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "character", studyAreaMapPath = "character",
                ecoregionMap = "character", ecoregionMapPath = "missing",
                cellSize = "missing"),
  definition = function(studyAreaMap, studyAreaMapPath, ecoregionMap) {
    ecoregionClassification(studyAreaMap = studyAreaMap, studyAreaMapPath = studyAreaMapPath,
                            ecoregionMap = ecoregionMap, ecoregionMapPath = ".",
                            cellSize = 200)
  }) 

#' @export
#' @rdname ecoregionClassification
setMethod(
  "ecoregionClassification",
  signature = c(studyAreaMap = "character", studyAreaMapPath = "missing",
                ecoregionMap = "character", ecoregionMapPath = "missing",
                cellSize = "missing"),
  definition = function(studyAreaMap, ecoregionMap) {
    ecoregionClassification(studyAreaMap = studyAreaMap, studyAreaMapPath = ".",
                            ecoregionMap = ecoregionMap, ecoregionMapPath = ".",
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






