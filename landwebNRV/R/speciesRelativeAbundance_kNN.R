################################################################################
#' this function is to generate species relative abundance for each ecoregion to replace SEP based on kNN maps
#' 
#' 
#' @param speciesLayers  RasterStack. It contains all the species raster layers of kNN.
#'
#' 
#' 
#' @param ecoregionMap  SpatialPolygons. Specify ecoregion in study area.
#'
#' @return a data table that has two columns, i.e., mapcode and species
#' 
#' @importFrom data.table data.table ':=' unique
#' @importFrom raster stack subset crop mask Which getValues
#' @importFrom sp spTransform 
#'
#' @note no note
#'
#' @seealso no
#'
#' @export
#' @docType methods
#' @rdname speciesRelativeAbundance_kNN
#'
#' @author Yong Luo
#'
setGeneric("speciesRelativeAbundance_kNN", function(speciesLayers,
                                                       ecoregionMap) {
  standardGeneric("speciesRelativeAbundance_kNN")
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "speciesRelativeAbundance_kNN",
  signature = c(speciesLayers = "RasterStack", 
                ecoregionMap = "RasterLayer"),
  definition = function(speciesLayers,
                        ecoregionMap) {
    if(as.character(crs(speciesLayers)) != as.character(crs(ecoregionMap))){
      ecoregionMap <- projectRaster(ecoregionMap, crs = projection(speciesLayers))
    } 
    ecoregionMap[] <- round(getValues(ecoregionMap))
    speciesLayerEcoregion <- crop(speciesLayers, ecoregionMap)
    speciesLayerEcoregion <- suppressWarnings(mask(speciesLayerEcoregion, ecoregionMap))
    speciesTableEcoregion <- cbind(data.table(ecoregion = getValues(ecoregionMap)), 
                                       data.table(getValues(speciesLayerEcoregion)))
    speciesTableEcoregion <- speciesTableEcoregion[complete.cases(speciesTableEcoregion)]
    speciesTableEcoregion <- melt.data.table(speciesTableEcoregion,
                                                 measure.vars = names(speciesTableEcoregion)[-1],
                                                 variable.name = "species") 
    speciesTableEcoregion[value < 10, presence := 0]
    speciesTableEcoregion[value >= 10, presence := 1]
    speciesTableEcoregion <- speciesTableEcoregion[,.(relativeAbundance = sum(presence)/length(value)),
                                                           by = c("ecoregion", "species")]
    speciesLevels <- unique(speciesTableEcoregion$species)
    abundanceMapStack <- stack()
    names(ecoregionMap) <- "ecoregion"
    for(i in 1:length(speciesLevels)){
      speciesTableEcoregionBySpecies <- speciesTableEcoregion[species == speciesLevels[i],][
        , species:=NULL]
      abundanceMap <- rasterizeReduced(speciesTableEcoregionBySpecies, ecoregionMap, "relativeAbundance")
      names(abundanceMap) <- as.character(speciesLevels[i])
      abundanceMapStack <- stack(abundanceMapStack, abundanceMap)
    }
    return(list(speciesAbundanceTable = speciesTableEcoregion,
                speciesAbundanceMaps = abundanceMapStack))
  })