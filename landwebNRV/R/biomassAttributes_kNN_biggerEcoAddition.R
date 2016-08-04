################################################################################
#' this function is to generate maxBiomass and maxANPP for NA ecodistrict by replacing them 
#' with values that derived from ecoregion area.
#' 
#' 
#' @param speciesLayers  RasterStack. It contains all the species raster layers of kNN.
#' 
#' @param biomassLayer RasterLayer. This is biomass map of kNN.
#' 
#' @param SALayer RasterLayer. This is stand age map of kNN.
#' 
#' @param ecoregionMap RasterLayer. This is the ecoregion map in the study area
#' 
#' @param NAData data.table
#' 
#' @param biggerEcoMap SpatialPolygonsDataFrame
#' 
#'
#' @return a data table that has three columns, i.e., active, mapcode and ecoregion
#' 
#' @importFrom data.table data.table ':='
#' @importFrom raster projection crop mask getValues
#' @importFrom dplyr left_join '%>%' 
#'
#' @note no note
#'
#' @seealso no
#'
#' @export
#' @docType methods
#' @rdname biomassAttributes_kNN_biggerEcoAddition
#'
#' @author Yong Luo
#'
setGeneric("biomassAttributes_kNN_biggerEcoAddition",
           function(speciesLayers,
                    biomassLayer,
                    SALayer,
                    ecoregionMap,
                    biggerEcoMap,
                    NAData) {
             standardGeneric("biomassAttributes_kNN_biggerEcoAddition")
           })

#' @export
#' @rdname biomassAttributes_kNN_biggerEcoAddition
setMethod(
  "biomassAttributes_kNN_biggerEcoAddition",
  signature = c(speciesLayers = "RasterStack",
                biomassLayer = "RasterLayer",
                SALayer = "RasterLayer",
                ecoregionMap = "RasterLayer",
                biggerEcoMap = "SpatialPolygonsDataFrame",
                NAData = "data.table"),
  definition = function(speciesLayers,
                        biomassLayer,
                        SALayer,
                        ecoregionMap,
                        biggerEcoMap,
                        NAData) {
    browser()
    
    if(projection(speciesLayers)!=projection(ecoregionMap)){
      ecoregionMap <- projectRaster(ecoregionMap, crs = crs(speciesLayers))
      ecoregionMap[] <- round(getValues(ecoregionMap))
    }
    if(projection(speciesLayers)!=projection(biomassLayer)){
      biomassLayer <- projectRaster(biomassLayer, crs = crs(speciesLayers))
    }
    if(projection(speciesLayers)!=projection(SALayer)){
      SALayer <- projectRaster(SALayer, crs = crs(speciesLayers))
    }
    if(projection(biggerEcoMap) != projection(speciesLayers)){
      biggerEcoMap <- sp::spTransform(biggerEcoMap, CRSobj = crs(speciesLayers))
    }
    subEcoregion <- ecoregionMap
    subEcoregion[!(getValues(subEcoregion) %in% unique(NAData$ecoregion))] <- NA
    subbiggerEcoMap <- raster::crop(biggerEcoMap, subEcoregion)
    browser()
    subbiggerEcoLevel <- unique(subbiggerEcoMap@data$ECOREGION)
    subbigEcoMap <- biggerEcoMap[biggerEcoMap@data$ECOREGION %in% subbiggerEcoLevel,]
    subbiggerEcoMap_Raster <- crop(biomassmap, subbigEcoMap)
    
    subbiggerEcoMap_Raster <- setValues(ecoregionMap, NA)
    for(indiEcoregion in subbiggerEcoLevel){
      indiSubBiggerEcoMap <- biggerEcoMap[biggerEcoMap@data$ECOREGION == indiEcoregion,]
      indiEcoMapRaster <- setValues(subbiggerEcoMap_Raster, indiEcoregion) 
      indiEcoMapRaster <- crop(indiEcoMapRaster, indiSubBiggerEcoMap)
      indiEcoMapRaster <- suppressWarnings(mask(indiEcoMapRaster, indiSubBiggerEcoMap))
      if(indiEcoregion == subbiggerEcoLevel[1]){
        biggerEcoMapRaster <- indiEcoMapRaster
      } else {
        biggerEcoMapRaster <- merge(biggerEcoMapRaster, indiEcoMapRaster)
      }
    }
    biggerEcoMapRaster <- suppressWarnings(mask(biggerEcoMapRaster, subEcoregion))
    ecodistrictEcoregionTable <- data.table(ecoregion = getValues(subEcoregion),
                                            biggerEcoregion = getValues(biggerEcoMapRaster))[!is.na(ecoregion),]
    #check whether one district has more than one ecoregion, which is not correct
    ecodistrictEcoregionTable[,totLength:=length(biggerEcoregion), by = ecoregion]
    ecodistrictEcoregionTable[,ecoLength:=length(totLength), by = c("biggerEcoregion", "ecoregion")]
    ecodistrictEcoregionTable[, percentage:=ecoLength/totLength]
    ecodistrictEcoregionTable[, maxPercent:=max(percentage), by = ecoregion]
    ecodistrictEcoregionTable <- ecodistrictEcoregionTable[percentage == maxPercent, .(biggerEcoregion, ecoregion)] %>%
      unique(., by = c("biggerEcoregion", "ecoregion"))

    ecoregionBiomass <- biomassAttributes_kNN(speciesLayers = specieslayersfull,
                          biomassLayer = biomassmap,
                          SALayer = samap,
                          ecoregionMap = biggerEcoMapRaster)
    setnames(ecoregionBiomass, "ecoregion", "biggerEcoregion")
    NAData <- setkey(NAData, ecoregion)[setkey(ecodistrictEcoregionTable, ecoregion), nomatch = 0]
    NAData <- dplyr::left_join(NAData[,.(biggerEcoregion, ecoregion, species, SEP)], ecoregionBiomass,
                               by = c("biggerEcoregion", "species")) %>%
      data.table
    return(list(addData = NAData, biggerEcoMapRaster = biggerEcoMapRaster))
  })