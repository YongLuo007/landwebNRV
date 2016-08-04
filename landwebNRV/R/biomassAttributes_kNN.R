################################################################################
#' this function is to generate the species ecoregion table for landis input.
#' the maximum biomass for pure stand 
#' 
#' @param speciesLayers  RasterStack. It contains all the species raster layers of kNN.
#' 
#' @param biomassLayer RasterLayer. This is biomass map of kNN.
#' 
#' @param SALayer RasterLayer. This is stand age map of kNN.
#'
#' 
#' @param ecoregionMap RasterLayer. This is the ecoregion map in the study area
#'
#' @return a raster layer of ecoregion in study area
#'         a data table that has three columns, i.e., active, mapcode and ecoregion
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
#' @rdname biomassAttributes_kNN
#'
#' @author Yong Luo
#'
setGeneric("biomassAttributes_kNN", function(speciesLayers,
                                      biomassLayer,
                                      SALayer,
                                      ecoregionMap) {
  standardGeneric("biomassAttributes_kNN")
})

#' @export
#' @rdname biomassAttributes_kNN
setMethod(
  "biomassAttributes_kNN",
  signature = c(speciesLayers = "RasterStack",
                biomassLayer = "RasterLayer",
                SALayer = "RasterLayer",
                ecoregionMap = "RasterLayer"),
  definition = function(speciesLayers,
                        biomassLayer,
                        SALayer,
                        ecoregionMap) {
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
    
    speciesinstudyarea <- crop(speciesLayers, ecoregionMap)
    speciesinstudyarea <- suppressWarnings(mask(speciesinstudyarea, ecoregionMap))
    speciesTable <- data.table(getValues(speciesinstudyarea))
    
    biomassinstudyarea <- crop(biomassLayer, ecoregionMap)
    biomassinstudyarea <- suppressWarnings(mask(biomassinstudyarea, ecoregionMap))
    speciesTable[, biomass:=getValues(biomassinstudyarea)]
    

    
    SAinstudyarea <- crop(SALayer, ecoregionMap)
    SAinstudyarea <- suppressWarnings(mask(SAinstudyarea, ecoregionMap))
    
    speciesTable[,':='(SA = getValues(SAinstudyarea), ecoregion = getValues(ecoregionMap))]
    speciesTable <- speciesTable[!is.na(ecoregion),]
    speciesTable <- melt.data.table(speciesTable, measure.vars = names(speciesinstudyarea),
                                     variable.name = "species", value.name = "percentage")
    speciesTable[, speciesBiomass:=biomass*percentage*0.01]
    speciesTable1 <- speciesTable[percentage>=50,]
    speciesTable1 <- speciesTable1[,.(maxBiomass = 100*quantile(speciesBiomass, 0.8, na.rm = TRUE)),
                                   by = c("ecoregion", "species")]
    speciesTable1[, maxANPP:=maxBiomass/30]
    output <- unique(speciesTable[,.(ecoregion, species)], by = c("ecoregion", "species"))
    output <- dplyr::left_join(output, speciesTable1, by = c("ecoregion", "species")) %>%
      data.table
    return(speciesBiomass = output)
  })