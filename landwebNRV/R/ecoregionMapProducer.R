################################################################################
#' this function is to generate ecoregion map for landis input for a given raster layer
#' in order to be consistent with map of initial communities, it is be highly recommended to 
#' use initial community as a study area raster layer.
#' 
#' @param studyAreaRaster  RasterLayer. This is a rasterlayer of study area. Initial community map
#'                         is preferred. Other rasterlayer that have same dimension, resolution,
#'                         extent and coordinate reference as initial community map also works.
#' 
#' @param ecoregionMapFull SpatialPolygonsDataFrame. This is the full ecoregion map the study area
#'                         ecoregion map derive from. 
#'
#' @param ecoregionName  character. Specify the ecoregion name in ecoregion data frame.
#' 
#' @param ecoregionActiveStatus data table. It contains two columns, i.e., active and ecoregion.
#'                              active column gives a status of ecoregion such as yes or no.
#'                              ecoregion gives the name of ecoregion. If a ecoregion's active
#'                              status is not specified, this ecoregion will be treated as inactive.
#' 
#' @param studyArea  SpatialPolygons. Specify study area.
#' 
#' 
#'
#' @return a raster layer of ecoregion in study area
#'         a data table that has three columns, i.e., active, mapcode and ecoregion
#' 
#' @importFrom data.table data.table ':='
#' @importFrom raster intersect crop mask Which setValues
#' @importFrom sp spTransform
#' @importFrom dplyr left_join '%>%' 
#'
#' @note no note
#'
#' @seealso no
#'
#' @export
#' @docType methods
#' @rdname ecoregionMapProducer
#'
#' @author Yong Luo
#'
setGeneric("ecoregionMapProducer", function(studyAreaRaster,
                                            ecoregionMapFull,
                                            ecoregionName,
                                            ecoregionActiveStatus,
                                            studyArea) {
  standardGeneric("ecoregionMapProducer")
})

#' @export
#' @rdname ecoregionMapProducer
setMethod(
  "ecoregionMapProducer",
  signature = c(studyAreaRaster = "RasterLayer",
                ecoregionMapFull = "SpatialPolygonsDataFrame",
                ecoregionName = "character",
                ecoregionActiveStatus = "data.table",
                studyArea = "SpatialPolygons"),
  definition = function(studyAreaRaster,
                        ecoregionMapFull,
                        ecoregionName,
                        ecoregionActiveStatus,
                        studyArea) {
    # change the coordinate reference for all spatialpolygons
    ecoregionMapFull <- spTransform(ecoregionMapFull, crs(studyAreaRaster))
    studyArea <- spTransform(studyArea, crs(studyAreaRaster))
    ecoregionMapInStudy <- raster::intersect(ecoregionMapFull, studyArea)
    ecoregions <- ecoregionMapInStudy@data[,ecoregionName]
    ecoregionTable <- data.table(mapcode = numeric(),
                                 ecoregion = character())
    mapcode <- 1
    for(ecoregion in ecoregions){
      ecoregionTable <- rbind(ecoregionTable, 
                              data.table(mapcode = mapcode,
                                         ecoregion = ecoregion)) 
      singleecoMapPoly <- ecoregionMapInStudy[ecoregionMapInStudy@data[,ecoregionName]==ecoregion,]
      studyAreaRaster <- setValues(studyAreaRaster, mapcode)
      singleecoMapRaster <- crop(studyAreaRaster, singleecoMapPoly)
      singleecoMapRaster <- mask(singleecoMapRaster, singleecoMapPoly)
      if(mapcode == 1){
        ecoregionMap <- singleecoMapRaster
      } else {
        ecoregionMap <- merge(ecoregionMap, singleecoMapRaster)
      }
      mapcode <- mapcode + 1
    }
    ecoregionTable <- dplyr::left_join(ecoregionTable,
                                       ecoregionActiveStatus,
                                       by = "ecoregion") %>%
      data.table
    ecoregionTable[is.na(active), active:="no"]
    ecoregionTable <- ecoregionTable[,.(active, mapcode, ecoregion)]
    return(list(ecoregionMap = ecoregionMap,
                ecoregion = ecoregionTable))
  })