################################################################################
#' this function is to generate species ecoregion table for landis input from a interpolated
#' maximum biomass map from pure stand (defined by percentageCutpoint) using kNN map.
#' 
#' @param speciesLayers  RasterStack. It contains all the species raster layers of kNN.
#' 
#' @param biomassLayer RasterLayer. This is biomass map of kNN.
#' 
#' @param percentageCutPoint numeric. This is the cut point for dominance of a species.
#' 
#' @param intepolateMethod character. specify the interplating function.
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
#' @rdname biomassAttr_IntepFromkNN
#'
#' @author Yong Luo
#'
setGeneric("biomassAttr_IntepFromkNN", function(speciesLayers,
                                             biomassLayer,
                                             percentageCutPoint,
                                             intepolateMethod,
                                             ecoregionMap) {
  standardGeneric("biomassAttr_IntepFromkNN")
})

#' @export
#' @rdname biomassAttr_IntepFromkNN
setMethod(
  "biomassAttr_IntepFromkNN",
  signature = c(speciesLayers = "RasterStack",
                biomassLayer = "RasterLayer",
                percentageCutPoint = "numeric",
                intepolateMethod = "character",
                ecoregionMap = "RasterLayer"),
  definition = function(speciesLayers,
                        biomassLayer,
                        percentageCutPoint,
                        intepolateMethod,
                        ecoregionMap) {
    
    projection(speciesLayers) <- projection(ecoregionMap)
    speciesinstudyarea <- crop(speciesLayers, ecoregionMap)
    speciesinstudyarea <- suppressWarnings(mask(speciesinstudyarea, ecoregionMap))
    speciesTable <- data.table(getValues(speciesinstudyarea))
    speciesTable[,pixelindex:=1:ncell(speciesinstudyarea)]
    
    projection(biomassLayer) <- projection(ecoregionMap)
    biomassinstudyarea <- crop(biomassLayer, ecoregionMap)
    biomassinstudyarea <- suppressWarnings(mask(biomassinstudyarea, ecoregionMap))
    speciesTable[,biomass:=getValues(biomassinstudyarea)]
    
    speciesTable[,ecoregion:=getValues(ecoregionMap)]
    speciesTable <- speciesTable[!is.na(ecoregion),]
    intepolateMethod <- "IDW"
    maxBiomassStack <- stack()
    for(species in names(speciesLayers)){
      speciestable_sub <- speciesTable[,c("pixelindex",species, "biomass", "ecoregion"), with = FALSE]
      setnames(speciestable_sub, species, "precentage")
      speciestable_sub <- speciestable_sub[precentage > percentageCutPoint,]# species dominant stands
      emptyRaster <- raster(biomassinstudyarea)
      if(nrow(speciestable_sub)>0){
        speciestable_subshort <- speciestable_sub[,.(maxBiomass=100*quantile(biomass, 0.8, na.rm = TRUE)),
                                                  by = c("ecoregion")]
        speciestable_sub <- setkey(speciestable_sub, ecoregion)[setkey(speciestable_subshort, ecoregion),
                                                                nomatch = 0]
        
        emptyRaster[speciestable_sub$pixelindex] <- speciestable_sub$maxBiomass
        xy <- data.frame(xyFromCell(emptyRaster, 1:ncell(emptyRaster)))
        xy$v <- getValues(emptyRaster)
        xy <- xy[which(!is.na(xy$v)),]

        if(intepolateMethod == "TPS"){ # TPS is thin plate spline model
          tps <- fields::Tps(xy[,1:2], xy[,3])
          maxBiomassRaster <- raster(emptyRaster)
          maxBiomassRaster <- interpolate(maxBiomassRaster, tps)
        } else if(intepolateMethod == "IDW"){ # IDW is inverse distance weighted
          mg <- gstat::gstat(id = "v", formula = v~1,
                             locations = ~x+y, data = xy)
          maxBiomassRaster <- raster(emptyRaster)
          maxBiomassRaster <- interpolate(maxBiomassRaster, mg)
        } else if(intepolateMethod == "OK"){ #  ordinary kriging
          coordinates(xy) <- ~x+y
          projection(xy) <- projection(emptyRaster)
          v <- gstat::variogram(log(v)~1, xy)
          m <- fit.variogram(v, vgm(1, "Sph", 300, 1))
          GOK <- gstat(NULL, "log.v", log(v)~1, xy, model=m)
          maxBiomassRaster <- raster(emptyRaster)
          maxBiomassRaster <- interpolate(maxBiomassRaster, GOK)
        } else if(intepolateMethod == "UK"){ # UK is universal kriging
          coordinates(xy) <- ~x+y
          projection(xy) <- projection(emptyRaster)
          v <- variogram(log(v)~1, xy)
          m <- fit.variogram(v, vgm(1, "Sph", 300, 1))
          GOK <- gstat(NULL, "log.v", log(v)~1, xy, model=m)
          maxBiomassRaster <- raster(emptyRaster)
          maxBiomassRaster <- interpolate(maxBiomassRaster, GOK)
          
        } # can define more interpolate methods here
        names(maxBiomassRaster) <- species
        maxBiomassRaster <- mask(maxBiomassRaster, ecoregionMap)
        maxBiomassStack <- stack(maxBiomassStack,maxBiomassRaster)
      } 
    }
    
    names(ecoregionMap) <- "ecoregion"
    maxBiomassStack <- stack(maxBiomassStack, ecoregionMap)
    biomassTable <- data.table(getValues(maxBiomassStack))[!is.na(ecoregion),]
    biomassTable <- data.table::melt(biomassTable,
                                     id.vars = "ecoregion",
                                     variable.name = "species",
                                     value.name = "maxBiomass")
    biomassTable <- biomassTable[,.(maxBiomass=mean(maxBiomass)),
                                 by = c("ecoregion", "species")]
    biomassTable[,maxANPP:=maxBiomass/30]
    return(biomassTable)
  })