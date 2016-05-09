################################################################################
#' this function is to generate initial community map for landis input based on kNN maps
#' 
#' 
#' @param speciesLayers  RasterStack. It contains all the species raster layers of kNN.
#'
#' @param speciesNames  character. It contains a vect of species names. 
#'                      species name must be in names(speciesLayers)
#' 
#' @param studyArea  SpatialPolygons. Specify study area.
#' 
#' 
#'
#' @return a raster layer of initial community
#'         a data table that has two columns, i.e., mapcode and species
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
#' @rdname initialCommunityMapProducer_kNN
#'
#' @author Yong Luo
#'
setGeneric("initialCommunityMapProducer_kNN", function(speciesLayers,
                                                       speciesNames,
                                                       studyArea) {
  standardGeneric("initialCommunityMapProducer_kNN")
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "initialCommunityMapProducer_kNN",
  signature = c(speciesLayers = "RasterStack", 
                speciesNames = "character", 
                studyArea = "SpatialPolygons"),
  definition = function(speciesLayers,
                        speciesNames,
                        studyArea) {
    
    studyArea <- spTransform(studyArea, crs(speciesLayers))
    specieslayerBySpecies <- subset(speciesLayers, speciesNames)
    specieslayerBySpeciesInStudyArea <- crop(specieslayerBySpecies,
                                             studyArea)
    specieslayerBySpeciesInStudyArea <- mask(specieslayerBySpeciesInStudyArea,
                                             studyArea)
    
    speciesComMap <- subset(specieslayerBySpeciesInStudyArea, speciesNames[1])
    speciesComMap <- as.logical(speciesComMap)
    speciesComMap[Which(is.na(speciesComMap), cells = TRUE, na.rm = FALSE)] <- 0
    k <- 1
    for(species in speciesNames[2:length(speciesNames)]){
      speciesMap <- subset(specieslayerBySpeciesInStudyArea, species)
      speciesMap <- as.logical(speciesMap)
      speciesMap[Which(is.na(speciesMap), cells = TRUE, na.rm = FALSE)] <- 0
      speciesComMap <- speciesMap*(10^k)+speciesComMap
      k <- k+1
    }
    # set the non-forested area as NA
    speciesComMap[Which(speciesComMap==0, cells = TRUE, na.rm = FALSE)] <- NA
    initialCommunities <- data.table(mapcode=sort(unique(getValues(speciesComMap))))
    initialCommunities[,mapCodeStr:=as.character(mapcode)]
    initialCommunities[, NofStr:=nchar(mapCodeStr)]
    for(i in 1:(length(speciesNames)-1)){
      initialCommunities[NofStr==i, mapCodeFull:=paste(paste(rep("0",(length(speciesNames)-i)),
                                                             collapse = ""),
                                                       mapCodeStr,
                                                       sep = "")]
    }
    initialCommunities[NofStr==length(speciesNames), mapCodeFull:=mapCodeStr]
    output <- data.table(mapcode = numeric(), speciesPresence = character(),
                         species = character())
    for(i in 1:nrow(initialCommunities)){
      outputAdd <- data.table(mapcode = initialCommunities$mapcode[i],
                              speciesPresence = substring(initialCommunities$mapCodeFull[i],
                                                          seq(1, length(speciesNames), 1),
                                                          seq(1, length(speciesNames), 1)),
                              species = speciesNames[length(speciesNames):1])
      
      output <- rbind(output, outputAdd)
    }
    initialCommunities <- output[speciesPresence!="0",]
    initialCommunities[,newMapCode:=as.numeric(as.factor(mapcode))]
    mapcodeconnection <- unique(initialCommunities[,.(mapcode, newMapCode)], by = "mapcode")
    indexTable <- data.table(pixelIndex=1:ncell(speciesComMap),
                             mapcode=getValues(speciesComMap))
    indexTable <- indexTable[!is.na(mapcode),]
    indexTable <- setkey(indexTable, mapcode)[setkey(mapcodeconnection, mapcode),
                                              nomatch = 0]
    speciesComMap[indexTable$pixelIndex] <- indexTable$newMapCode
    initialCommunities[, ':='(mapcode = newMapCode, newMapCode = NULL, speciesPresence = NULL)]
    return(list(initialCommunityMap = speciesComMap,
                initialCommunity = initialCommunities))
  })


#' @export
#' @rdname initialCommunityMapProducer_kNN
setMethod(
  "initialCommunityMapProducer_kNN",
  signature = c(speciesLayers = "RasterStack", 
                speciesNames = "missing", 
                studyArea = "SpatialPolygons"),
  definition = function(speciesLayers, studyArea) {
    initialCommunityMapProducer_kNN(speciesLayers = speciesLayers,
                                    speciesNames = names(speciesLayers),
                                    studyArea = studyArea)
  })
