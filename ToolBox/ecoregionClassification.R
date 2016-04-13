# classify ecoregion based ecoregion map in the study region

# cellSize in meter
ecoregionClassification <- function(studyAreaMap, studyAreaMapPath = ".",
                                    ecoregionMap, ecoregionMapPath = ".",
                                    cellSize = 200){
  studyarea <- readRDS(file.path(studyAreaMapPath, studyAreaMap)) 
  ecoregionMap <- readOGR(file.path(ecoregionMapPath, ecoregionMap),
                          layer = "ecozones")
  studyarea <- spTransform(studyarea, crs(ecoregionMap))
  studyareaecoregion <- crop(ecoregionMap, extent(studyarea))
  r <- raster(ncol=round(1000*earthDist(long1=extent(studyareaecoregion)[1],
                                     lat1=extent(studyareaecoregion)[3],
                                     long2=extent(studyareaecoregion)[1],
                                     lat2=extent(studyareaecoregion)[4]))/cellSize,
              nrow=round(1000*earthDist(long1=extent(studyareaecoregion)[1],
                                     lat1=extent(studyareaecoregion)[3],
                                     long2=extent(studyareaecoregion)[2],
                                     lat2=extent(studyareaecoregion)[3]))/cellSize)# based on south size )
  extent(r) <- extent(studyareaecoregion)
  crs(r) <- crs(studyareaecoregion)
  studyarea1 <- rasterize(studyarea, r, 1)
  studyareaecoregion <- rasterize(studyareaecoregion, r, "ECOZONE")
  studyareaecoregion <- mask(studyareaecoregion, studyarea)
  return(studyareaecoregion)
}
earthDist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}








