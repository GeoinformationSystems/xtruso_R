#'
#' get coordinate in 900x900 RADOLAN CRS from xy-index
#' @param radolan.x x index
#' @param radolan.y y index
#' @return RADOLAN coordinate for index
#'
Utility.getRADOLANCoordinate <- function(index.col, 
                                         index.row) {

  #get x and y min
  extent.minx <- radolan.configuration.extent900@xmin
  extent.maxy <- radolan.configuration.extent900@ymax

  #get target coordinate
  coord.x <- extent.minx + index.col - 0.5
  coord.y <- extent.maxy - index.row + 0.5

  #return sp coordinate
  return(sp::SpatialPoints(cbind(coord.x, coord.y), proj4string=xtruso::radolan.configuration.crs))

}


#'
#' get coordinate index in 900x900 RADOLAN CRS from coordinate
#' @param coord.x x coordinate (RADOLAN projection)
#' @param coord.y y coordinate (RADOLAN projection)
#' @param threshold cell border threshold, if index is within this threshold distance from ell border, both cell indices are returned
#' @return RADOLAN index for x and y
#'
Utility.getRADOLANIndexFromCoord <- function(coord.x, 
                                             coord.y,
                                             threshold = 0.05) {

  #get x and y min
  extent.minx <- radolan.configuration.extent900@xmin
  extent.maxy <- radolan.configuration.extent900@ymax

  #get target index position
  index.col <- coord.x - extent.minx + 0.5
  index.row <- extent.maxy - coord.y + 0.5

  #if index is below raster cell border threshold, add both indices to result
  index.col <- if(((index.col + .5) %% 1) < 0.05 || ((index.col + .5) %% 1) > 0.95) c(floor(index.col), ceiling(index.col)) else round(index.col)
  index.row <- if(((index.row + .5) %% 1) < 0.05 || ((index.row + .5) %% 1) > 0.95) c(floor(index.row), ceiling(index.row)) else round(index.row)

  #return index
  return(list(col=index.col, row=index.row))

}


#'
#' get coordinate index in 900x900 RADOLAN CRS from coordinate
#' @param cell cell number
#' @return RADOLAN index for x and y
#'
Utility.getRADOLANIndexFromCell <- function(cell) {

  #get target index position
  index.col <- cell %% 900
  index.row <- ceiling(cell / 900)

  return(list(col=index.col, row=index.row))

}


#'
#' get indices and weights for 900x900 RADOLAN raster for a given polygon
#' @param polygon input polygon
#' @param raster for which the overlay is computed
#' @return RADOLAN bounding box indices and weights for x and y covering the polygon
#'
Utility.getRADOLANIndicesForPolygon <- function(polygon,
                                                raster = xtruso::xtruso.radolan.sample) {
  
  
  #get zonal overlay
  overlap <- ZonalOverlap.getWeightedOverlap(raster, polygon, polygons.id="GKZ", parallel=F)
  
  #get x and y indices
  index.col <- c(min(overlap$col) : max(overlap$col))
  index.row <- c(min(overlap$row) : max(overlap$row))
  
  #init weight matrix for polygon based on overlap
  weights = array(0, dim=c(length(index.row), length(index.col)))
  colnames(weights) <- index.col
  rownames(weights) <- index.row
  for(i in 1:nrow(overlap)){
    weights[as.character(overlap[i, "row"]), as.character(overlap[i, "col"])] <- overlap[i, "weight"]
  }
  
  #return x, y and weights as a list
  return(list(col=index.col, row=index.row, weights=weights))
  
}


#'
#' get value from list that is closest to provided value
#' @param list list of values
#' @param value input search value
#' @param direction direction to search for closest value; default is "both", other valid values are "geq" (greater equal) and "leq" ("lower equal)
#' @return closest value from list
#'
Utility.getClosest <- function(list,
                               value,
                               direction = "both") {
  
  if(!direction %in% c("both", "leq", "geq"))
    stop("Invalid direction.")
  
  #compute lowest value for both directions (minimal absolute difference)
  if(direction == "both")
    return(which(abs(list - as.double(value)) == min(abs(list - as.double(value)))))
  
  #compute lower equal value
  else if(direction == "leq"){
    sublist <- list[list - as.double(value) < 0]
    ifelse(length(sublist) == 0, return(list[1]), return(max(sublist)))
  }
    
  #compute greater equal value
  else if(direction == "geq"){
    sublist <- list[list - as.double(value) > 0]
    ifelse(length(sublist) == 0, return(list[length(list)]), return(min(sublist)))
  }

}
