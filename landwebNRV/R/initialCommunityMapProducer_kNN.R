################################################################################
#' this function is to generate initial community map for landis input based on kNN maps
#' 
#' 
#' @param speciesLayers  RasterStack. It contains all the species raster layers of kNN.
#'
#' @param speciesPresence  numeric. Define a presence cut point for a species in a pixel
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
                                                       speciesPresence,
                                                       studyArea) {
  standardGeneric("initialCommunityMapProducer_kNN")
})

#' @export
#' @rdname moduleCoverage
setMethod(
  "initialCommunityMapProducer_kNN",
  signature = c(speciesLayers = "RasterStack", 
                speciesPresence = "numeric", 
                studyArea = "SpatialPolygons"),
  definition = function(speciesLayers,
                        speciesPresence,
                        studyArea) {
    studyArea <- spTransform(studyArea, crs(speciesLayers))
    specieslayerInStudyArea <- crop(speciesLayers,
                                             studyArea)
    specieslayerInStudyArea <- suppressWarnings(mask(specieslayerInStudyArea,
                                             studyArea))
    speciesNames <- names(specieslayerInStudyArea)[which(maxValue(specieslayerInStudyArea)>=speciesPresence)]
    specieslayerBySpecies <- subset(specieslayerInStudyArea, speciesNames[1])
    specieslayerBySpecies[Which(is.na(specieslayerBySpecies) & specieslayerBySpecies<=5,
                                cells = TRUE)] <- 0 # 5% or less presence removed
    speciesComMap <- as.logical(specieslayerBySpecies)
    rm(specieslayerBySpecies)
    k <- 1
    for(species in speciesNames[2:length(speciesNames)]){
      specieslayerBySpecies <- subset(specieslayerInStudyArea, species)
      specieslayerBySpecies[Which(is.na(specieslayerBySpecies) & specieslayerBySpecies <=5,
                                   cells = TRUE)] <- 0
      speciesMap <- as.logical(specieslayerBySpecies)
      speciesComMap <- speciesMap*(10^k)+speciesComMap
      k <- k+1
      rm(specieslayerBySpecies, speciesMap)
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
                speciesPresence = "missing", 
                studyArea = "SpatialPolygons"),
  definition = function(speciesLayers, studyArea) {
    initialCommunityMapProducer_kNN(speciesLayers = speciesLayers,
                                    speciesPresence = 0.5,
                                    studyArea = studyArea)
  })
