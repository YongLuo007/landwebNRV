################################################################################
#' this function is to add non-active ecoregion from SpatialPolygons to initial community map and
#' ecoregion map and also updates the ecoregion and initial community lookup tables
#' 
#' @param nonactivePolys  SpatialPolygons. This is a spatial polygons that specify nonactive ecoregion
#'                        for example water bodies and cities
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
#' @importFrom raster mask Which setValues getValues
#' @importFrom sp spTransform
#'
#' @note no note
#'
#' @seealso no
#'
#' @export
#' @docType methods
#' @rdname nonactiveEcoFromPolys
#'
#' @author Yong Luo
#'
setGeneric("nonactiveEcoFromPolys", function(nonactivePolys,
                                             ecoregionMap,
                                             ecoregion,
                                             initialCommunityMap,
                                             initialCommunity) {
  standardGeneric("nonactiveEcoFromPolys")
})

#' @export
#' @rdname nonactiveEcoFromPolys
setMethod(
  "nonactiveEcoFromPolys",
  signature = c(nonactivePolys = "SpatialPolygons",
                ecoregionMap = "RasterLayer",
                ecoregion = "data.table",
                initialCommunityMap = "RasterLayer",
                initialCommunity = "data.table"),
  definition = function(nonactivePolys,
                        ecoregionMap,
                        ecoregion,
                        initialCommunityMap,
                        initialCommunity) {
    nonactivePolys <- spTransform(nonactivePolys, crs(ecoregionMap))
    temprasterlayer <- setValues(ecoregionMap, 1)
    nonactiveLayer <- mask(temprasterlayer, nonactivePolys)
    initialCommunityMap[Which(nonactiveLayer==1, cells = TRUE)] <- NA
    ecoregionMap[Which(nonactiveLayer==1, cells = TRUE)] <- NA
    initialCommunity <- initialCommunity[mapcode %in% sort(unique(getValues(initialCommunityMap))),]
    ecoregion <- ecoregion[mapcode %in% sort(unique(getValues(ecoregionMap))),]
    return(list(ecoregionMap = ecoregionMap,
                ecoregion = ecoregion,
                initialCommunityMap = initialCommunityMap,
                initialCommunity = initialCommunity))
  })