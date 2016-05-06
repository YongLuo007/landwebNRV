################################################################################
#' this function is to add non-active ecoregion from RasterLayer to initial community map and
#' ecoregion map and also updates the ecoregion and initial community lookup tables
#' 
#' @param nonactiveRaster  RasterLayer. This is a raster layer that contains nonactive ecoregion
#'                         for example water bodies and cities, this map must be same or bigger than
#'                         ecoregion map or initial community map and have same resolution.
#'                        
#' @param activeStatus data.table. This is lookup table for the nonactiveRaster that must contain
#'                     active status for map code, if a map code's satus is missing, nonactive will
#'                     be assigned. Please also note that this table must have two columns of active
#'                     and mapcode.
#'                        
#' @param ecoregionMap RasterLayer. Ecoreion map in study area. 
#' 
#' @param ecoregion data.table. ecoregion lookup table that connect mapcode to ecoregion
#'
#' @param initialCommunityMap  RasterLayer. Ininital community map in study area.
#' 
#' @param initialCommunity data table. It contains map code corresponding to initial community map
#' 
#'
#' @return two raster layers of ecoregion map and initial community map in study area
#'         two updated data tables ecoregion and initial community
#' 
#' @importFrom data.table data.table ':='
#' @importFrom raster crop Which setValues getValues projection
#' @importFrom sp spTransform
#'
#' @note no note
#'
#' @seealso no
#'
#' @export
#' @docType methods
#' @rdname nonactiveEcoFromRaster
#'
#' @author Yong Luo
#'
setGeneric("nonactiveEcoFromRaster", function(nonactiveRaster,
                                              activeStatus,
                                              ecoregionMap,
                                              ecoregion,
                                              initialCommunityMap,
                                              initialCommunity) {
  standardGeneric("nonactiveEcoFromRaster")
})

#' @export
#' @rdname nonactiveEcoFromRaster
setMethod(
  "nonactiveEcoFromRaster",
  signature = c(nonactiveRaster = "RasterLayer",
                activeStatus = "data.table",
                ecoregionMap = "RasterLayer",
                ecoregion = "data.table",
                initialCommunityMap = "RasterLayer",
                initialCommunity = "data.table"),
  definition = function(nonactiveRaster,
                        activeStatus,
                        ecoregionMap,
                        ecoregion,
                        initialCommunityMap,
                        initialCommunity) {
    
    browser()
    projection(nonactiveRaster) <- projection(ecoregionMap)
    nonactiveRasterSmall <- crop(nonactiveRaster, ecoregionMap)
    nonecomapcode <- activeStatus[active=="no",]$mapcode
    nonactiveRasterSmall[Which(nonactiveRasterSmall %in% nonecomapcode, cells = TRUE)] <- NA
    initialCommunityMap[Which(is.na(nonactiveRasterSmall), cells = TRUE)] <- NA
    ecoregionMap[Which(is.na(nonactiveRasterSmall), cells = TRUE)] <- NA
    initialCommunity <- initialCommunity[mapcode %in% sort(unique(getValues(initialCommunityMap))),]
    ecoregion <- ecoregion[mapcode %in% sort(unique(getValues(ecoregionMap))),]
    return(list(ecoregionMap = ecoregionMap,
                ecoregion = ecoregion,
                initialCommunityMap = initialCommunityMap,
                initialCommunity = initialCommunity))
  })