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
    biomassinstudyarea <- crop(biomassLayer, ecoregionMap)
    biomassinstudyarea <- suppressWarnings(mask(biomassinstudyarea, ecoregionMap))
    speciesTable <- data.table(biomass=getValues(biomassinstudyarea))
    SAinstudyarea <- crop(SALayer, ecoregionMap)
    SAinstudyarea <- suppressWarnings(mask(SAinstudyarea, ecoregionMap))
    
    speciesTable[,':='(SA = getValues(SAinstudyarea), ecoregion = getValues(ecoregionMap))]
    outputPartial <- data.table(ecoregion = numeric(), species = character(),
                         maxBiomass = numeric(), maxANPP = numeric())
    speciess <- names(speciesLayers)
    for(species in speciess){
      indispeciesraster <- subset(speciesinstudyarea, species)
      speciesTable[, percentage:=getValues(indispeciesraster)]
      speciesTable_narmed <- speciesTable[!is.na(ecoregion),]
      speciesTable_narmed[, speciesBiomass:=biomass*percentage*0.01]
      speciesTable_narmed <- speciesTable_narmed[percentage>=50,]
      speciesTable_narmed[,species:=species]
      speciesTable_narmed <- speciesTable_narmed[,.(maxBiomass = 100*quantile(speciesBiomass, 0.8, na.rm = TRUE)),
                                     by = c("ecoregion", "species")]
      speciesTable_narmed[, maxANPP:=maxBiomass/30]
      outputPartial <- rbind(outputPartial, speciesTable_narmed)
    }
    output <- data.table(expand.grid(ecoregion = as.numeric(unique(getValues(ecoregionMap))),
                                     species = speciess))[!is.na(ecoregion),][,species:=as.character(species)]
    output <- dplyr::left_join(output, outputPartial, by = c("ecoregion", "species")) %>%
      data.table
    return(speciesBiomass = output)
  })