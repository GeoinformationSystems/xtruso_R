#' Calculate Zonal Overlap for raster on input polygons
#'
#' This function calculates the zonal overlap for given raster cells for each input polygon.
#' It adds an attribute "raster_fun" to the polygons, which can be used to calculate the weigted average
#' of a raster within the respective polygon. However, to work properly, each raster that is later used
#' to execute the function must follow the structure of the raster input of this function.
#' The character string "raster_fun" has the following structure: "raster[%cellID%] * %cellWeight% + ..."
#'
#' @param path path to the RADOLAN SF binary input file, is set, the timestamp is ignored
#' @param timestamp used, if path is not set; must be of type POSIXct or keyword "latest", which will return the latest available timestamp
#' @return list($timestamp = timestamp of the image, $raster = RADOLAN SF raster object)
#' @export
ZonalOverlap <- function(raster, polygons) {

  if(class(polygons) != "SpatialPolygonsDataFrame")
    stop("function requires polygons instanceof 'SpatialPolygonsDataFrame'")

  #parallel execution with foreach
  if (requireNamespace("doSNOW", quietly=TRUE)) {

    #init parallel environment
    cl <- snow::makeCluster(as.integer(Sys.getenv("NUMBER_OF_PROCESSORS")) - 1)
    doSNOW::registerDoSNOW(cl)
    clusterExport(cl, "raster", envir=environment())

    #calculate raster overlap
    polygons@data["raster_fun"] <- foreach(i=1:nrow(polygons), .combine=rbind) %dopar% {
      #get ovelap weight for each overlapping cell
      z <- raster::extract(raster, polygons[i,], small=T, weights=T, cellnumbers=T, normalizeWeights=T)
      #append cell information to dataframe
      return(paste("raster[", paste(z[[1]][,"cell"],z[[1]][,"weight"], sep ="] * ", collapse=" + raster["), sep=""))
    }

    #stop cluster
    stopCluster(cl)

  #sequential execution in a for loop
  } else {

    #calculate raster overlap
    for(i in 1:nrow(polygons)) {
      #get ovelap weight for each overlapping cell
      z <- raster::extract(raster, polygons[i,], small=T, weights=T, cellnumbers=T, normalizeWeights=T)
      #append cell information to dataframe
      polygons[i, "raster_fun"] <- (paste("raster[", paste(z[[1]][,"cell"],z[[1]][,"weight"], sep ="] * ", collapse=" + raster["), sep=""))
    }

  }

  #return polygons with added "raster_fun" attribute
  return(polygons)

}
