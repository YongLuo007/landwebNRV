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
    projection(speciesLayers) <- projection(ecoregionMap)
    speciesinstudyarea <- crop(speciesLayers, ecoregionMap)
    speciesinstudyarea <- suppressWarnings(mask(speciesinstudyarea, ecoregionMap))
    speciesTable <- data.table(getValues(speciesinstudyarea))
    
    projection(biomassLayer) <- projection(ecoregionMap)
    biomassinstudyarea <- crop(biomassLayer, ecoregionMap)
    biomassinstudyarea <- suppressWarnings(mask(biomassinstudyarea, ecoregionMap))
    speciesTable[,biomass:=getValues(biomassinstudyarea)]
    
    projection(SALayer) <- projection(ecoregionMap)
    SAinstudyarea <- crop(SALayer, ecoregionMap)
    SAinstudyarea <- suppressWarnings(mask(SAinstudyarea, ecoregionMap))
    speciesTable[,SA:=getValues(SAinstudyarea)]
    
    speciesTable[,ecoregion:=getValues(ecoregionMap)]
    speciesTable <- speciesTable[!is.na(ecoregion),]
    output <- data.table(ecoregion = numeric(),
                         species = character(),
                         maxBiomass = numeric(),
                         maxANPP = numeric())
    for(species in names(speciesLayers)){
      speciestable_sub <- speciesTable[,c(species, "biomass", "SA", "ecoregion"), with = FALSE]
      setnames(speciestable_sub, species, "precentage")
      speciestable_sub[,species:=species]
      speciestable_sub <- speciestable_sub[precentage>50,]# species dominant stands
      outputAdd <- data.table(ecoregion = sort(unique(speciesTable$ecoregion)),
                                               species = species)
      if(nrow(speciestable_sub)>0){
        speciestable_sub <- speciestable_sub[,.(maxBiomass=100*quantile(biomass, 0.8, na.rm = TRUE)),
                                             by = c("ecoregion", "species")]
        speciestable_sub[,maxANPP:=maxBiomass/30]# using this equation for now, 
                                                  # will improve later
        outputAdd <- left_join(outputAdd, speciestable_sub, by = c("ecoregion", "species")) %>%
          data.table
      } else {
        outputAdd[,':='(maxBiomass = NA, maxANPP = NA)]
      }
      output <- rbind(output, outputAdd)
    }
    return(speciesBiomass = output)
  })