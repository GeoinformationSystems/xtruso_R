#' Calculate Zonal Overlap for raster on input polygons
#'
#' This function calculates the zonal overlap for given raster cells for each input polygon.
#' It adds an attribute to the polygons, which can be used to calculate the weigted average
#' of a raster within the respective polygon. However, to work properly, each raster that is later used
#' to execute the function must follow the structure of the raster input of this function.
#' The character string "raster_fun" has the following structure: "raster[%cellID%] * %cellWeight% + ..."
#'
#' @param raster raster dataset
#' @param polygons polygons overlayed on input raster
#' @param fun.column column in polygons@data, where overlap function is stored
#' @param raster.name name of the raster within the function string
#' @return polygons with added column for raster average computation
#' @export
ZonalOverlap <- function(raster,
                         polygons,
                         fun.colunm = "raster.avg",
                         raster.name = "raster",
                         parallel = TRUE) {

  if(missing(raster))
    stop("Need to specify input raster.")

  if(missing(polygons))
    stop("Need to specify input polygons.")

  if(class(polygons) != "SpatialPolygonsDataFrame")
    stop("function requires polygons instanceof 'SpatialPolygonsDataFrame'")

  #transfor polygons, if CRS do not match (do not transform raster to preserve raster cell values)
  if(sp::proj4string(raster) != sp::proj4string(polygons))
    polygons <- sp::spTransform(polygons, sp::proj4string(raster))

  #parallel execution with foreach
  if (parallel && "doSNOW" %in% installed.packages()[, "Package"]) {

    require(doSNOW, quietly = TRUE)

    #init parallel environment
    cl <- snow::makeCluster(as.integer(Sys.getenv("NUMBER_OF_PROCESSORS")) - 1)
    doSNOW::registerDoSNOW(cl)

    #calculate raster overlap
    polygons@data[fun.colunm] <- foreach::foreach(i=1:nrow(polygons), .combine=rbind, .export=("raster")) %dopar% {
      #get ovelap weight for each overlapping cell
      z <- raster::extract(raster, polygons[i,], small=T, weights=T, cellnumbers=T, normalizeWeights=T)
      #append cell information to dataframe
      return(ZonalOverlap.avg(raster.name, z[[1]][,"cell"], z[[1]][,"weight"]))
    }

    #stop cluster
    snow::stopCluster(cl)

  #sequential execution in a for loop
  } else {

    #calculate raster overlap
    for(i in 1:nrow(polygons)) {
      #get ovelap weight for each overlapping cell
      z <- raster::extract(raster, polygons[i,], small=T, weights=T, cellnumbers=T, normalizeWeights=T)
      #append cell information to dataframe
      polygons[i, fun.colunm] <- ZonalOverlap.avg(raster.name, z[[1]][,"cell"], z[[1]][,"weight"])
    }

  }

  #return polygons with added "raster_fun" attribute
  return(polygons)

}


#' Get function to calculate the average of raster values within a polygon
#'
#' @param raster.name name of the raster within the function string
#' @param cell array of cell ids
#' @param weight array of cell weights corresponding to input cells
#' @return function string for calculating the average based on input cells and weights
#' @export
ZonalOverlap.avg <- function(raster.name, cell, weight) {
  return(paste(raster.name,"[", paste(cell,"] * ",weight, sep ="", collapse=paste(" + ",raster.name,"[", sep="")), sep=""))
}
