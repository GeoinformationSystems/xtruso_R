#'
#' get coordinate in 900x900 RADOLAN CRS from xy-index
#' @param radolan.x x index
#' @param radolan.y y index
#' @return RADOLAN coordinate for index
#'
Utility.getRADOLANCoordinate <- function(index.x, index.y) {

  #get x and y min
  extent.minx <- radolan.configuration.extent900@xmin
  extent.miny <- radolan.configuration.extent900@ymin

  #get target coordinate
  coord.x <- extent.minx + index.x
  coord.y <- extent.miny + index.y

  #return sp coordinate
  return(sp::SpatialPoints(cbind(coord.x, coord.y), proj4string=xtruso::radolan.configuration.crs))

}


#'
#' get coordinate index in 900x900 RADOLAN CRS from coordinate
#' @param coord.x x coordinate (RADOLAN projection)
#' @param coord.y y coordinate (RADOLAN projection)
#' @return RADOLAN index for x and y
#'
Utility.getRADOLANIndexFromCoord <- function(coord.x, coord.y) {

  #get x and y min
  extent.minx <- radolan.configuration.extent900@xmin
  extent.miny <- radolan.configuration.extent900@ymin

  #get target index position
  index.x <- coord.x - extent.minx
  index.y <- coord.y - extent.miny

  #get target index
  index.x <- if(((index.x + .5) %% 1) < 0.05 || ((index.x + .5) %% 1) > 0.95) c(floor(index.x), ceiling(index.x)) else round(index.x)
  index.y <- if(((index.y + .5) %% 1) < 0.05 || ((index.y + .5) %% 1) > 0.95) c(floor(index.y), ceiling(index.y)) else round(index.y)

  #return index
  return(list(x=index.x, y=index.y))

}


#'
#' get coordinate index in 900x900 RADOLAN CRS from coordinate
#' @param cell cell number
#' @return RADOLAN index for x and y
#'
Utility.getRADOLANIndexFromCell <- function(cell) {

  #get target index position
  index.x <- cell %% 900
  index.y <- ceiling(cell / 900)

  return(data.frame(x=index.x, y=index.y))

}
