################################################################################
#' UTM to LongLat translator
#' 
#' 
#' @param UTMTable  data table. It must have three columns of Zone, Easting and Northing
#'
#' 
#' @param coordRefTo,  Character string. Define the spatial coordination reference that you wish to transform.
#'        Default is "+proj=longlat" without further specification             
#'                  
#'        
#'
#' @return  two data tables, the first one contains the fransformed longitude and latitude.
#'          The second one was the failed to transform due to missing information of UTM coordinates, ie., Zone, 
#'          Easting and Northing. 
#'                      
#'
#' @note no note
#'
#' @seealso no
#'
#' @include 
#' @export
#' @docType methods
#' @rdname UTMtoLongLat
#'
#' @author Yong Luo
#'
#' @examples
#' \dontrun{
#' 
#' }
setGeneric("UTMtoLongLat", function(UTMTable, coordRefTo) {
  standardGeneric("UTMtoLongLat")
})
#' @export
#' @rdname UTMtoLongLat
setMethod(
  "UTMtoLongLat",
  signature = c(UTMTable = "data.table", 
                coordRefTo = "character"),
  definition = function(UTMTable, coordRefTo) {
    output <- UTMTable[1,]
    output[,':='(Longitude = 0, Latitude = 0)]
    fullUTMInfor <- UTMTable[!is.na(Zone) & !is.na(Easting) & !is.na(Northing),]
    missingUTMInfor <- UTMTable[is.na(Zone) | is.na(Easting) | is.na(Northing),]
    utmZones <- unique(fullUTMInfor$Zone)
    for(utmZone in utmZones){
      crsUTMstring <- CRS(paste("+proj=utm +zone=", utmZone, sep=""))
      utmcoor <- SpatialPoints(cbind(fullUTMInfor[Zone == utmZone, ]$Easting,
                                     fullUTMInfor[Zone == utmZone, ]$Northing),
                             proj4string = crsUTMstring)
      longlatcoor <- spTransform(utmcoor, CRS(coordRefTo))
      transformed <- data.table(attributes(longlatcoor)$coords)
      names(transformed) <- c("Longitude", "Latitude")
      outputAdded <- cbind(fullUTMInfor[Zone == utmZone, ], transformed)
      output <- rbindlist(list(output, outputAdded))
    }
    output <- output[-1, ]
    return(list(Transformed  = output, UnTransformed  = missingUTMInfor))
  })


#' @export
#' @rdname UTMtoLongLat
setMethod(
  "UTMtoLongLat",
  signature = c(UTMTable = "data.table", 
                coordRefTo = "missing"),
  definition = function(UTMTable) {
    ecoregionClassification(UTMTable = UTMTable, 
                            coordRefTo = "+proj=longlat")
  })    

