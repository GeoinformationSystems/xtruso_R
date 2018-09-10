#' 
#' Calculate Zonal Overlap for input polygons overlaying an input raster
#'
#' @param raster input raster
#' @param polygon input polygon(s) to be overlayed
#' @param polygon.id field used to identify the polygons
#' @param parallel flag: use parallel computation with doParallel
#' @return dataframe with overlap information (id, name, cell, x, y, weight)
#' @export
x.zonal.overlap <- function(raster,
                            polygon,
                            polygon.id = names(polygon@data)[1],
                            parallel = TRUE) {

  if(missing(raster))
    stop("Need to specify input raster.")

  if(missing(polygon))
    stop("Need to specify input polygon(s).")

  if(class(polygon) != "SpatialPolygonsDataFrame")
    stop("function requires polygons being instanceof 'SpatialPolygonsDataFrame'")

  #transfor polygons, if CRS do not match (do not transform raster to preserve raster cell values)
  if(sp::proj4string(raster) != sp::proj4string(polygon))
    polygon <- sp::spTransform(polygon, sp::proj4string(raster))

  #parallel execution with foreach
  if (parallel && "doParallel" %in% installed.packages()[, "Package"]) {

    require(doParallel, quietly = TRUE)

    #init parallel environment
    cl <- makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)

    #extract values for each polygon
    df.overlap <- foreach::foreach(i=1:nrow(polygon), .combine=rbind) %dopar% {
      return(x.zonal.overlap.extract(i, raster, polygon[i,], polygon.id))
    }

    #stop cluster
    parallel::stopCluster(cl)

    #sequential execution in a for loop
  } else {

    #extract values for each polygon
    df.overlap <- data.frame()
    for(i in 1:nrow(polygon)) {
      df.overlap <- rbind(df.overlap, x.zonal.overlap.extract(i, raster, polygon[i,], polygon.id), stringsAsFactors = F)
    }

  }
  
  #set names
  names(df.overlap) <- c("index", "id", "cell", "x", "y", "size", "weight", "weight_norm")

  #return dataframe with overlap
  return(df.overlap)

}


#' 
#' Extract values from raster based on polygon overlay
#'
#' @param raster input raster
#' @param polygon input polygon to be overlayed
#' @param polygon.id field used to identify the polygons
#' @return dataframe with overlap information (id, name, cell, x, y, weight)
x.zonal.overlap.extract <- function(index, raster, polygon, polygon.id) {
  
  #get polygon id
  id <- as.character(polygon[[polygon.id]])
  
  #get ovelap weight for each overlapping cell
  z <- as.data.frame(raster::extract(raster, polygon, small=T, weights=T, cellnumbers=T, normalizeWeights=F))
  
  #get actual area of cells in map units
  area <- raster::area(raster::projectRaster(raster, crs=CRS("+proj=longlat +datum=WGS84")))
  z$area <- area[z$cell]
  
  #calculate and normalized weights
  weight_norm = z$weight / sum(z$weight)
  
  return(data.frame(index, id, cell=z$cell, raster::xyFromCell(raster, z$cell), area=z$area, z$weight, weight_norm))
  
}


#' Get zonal statistics for provided raster based on computed Zonal Overlap
#'
#' @param raster input raster
#' @param df.overlap overlap dataframe
#' @return zonal statistics dataframe
#' @export
x.zonal.stats <- function(raster,
                          df.overlap) {

  if(missing(raster))
    stop("Need to specify input RADOLAN raster")

  if(missing(df.overlap) || !all(c("index","id") %in% names(df.overlap)))
    stop("Need to specify valid input dataframe")

  #get unique indices
  indices <- unique(df.overlap$index)

  #init statistics dataframe
  df.stats <- data.frame()

  #compute statistics for each polygon in df.overlap
  for(index in indices) {
  
    #get subset for index
    overlap <- df.overlap[df.overlap$index == index, ]
    
    #init statistics values
    min <- 999
    max <- -999
    mean <- 0
    
    #calculate statistics
    for(j in 1:nrow(overlap)) {
      
      #get value of current cell
      v <- raster[overlap[j, "cell"]]
      
      #set min, max
      min <- min(min, v)
      max <- max(max, v)
      
      #set mean based on corresponding weight
      mean <- mean + overlap[j, "weight"] * v
      
    }
    
    #calculate sum
    sum <- mean * sum(overlap[j, "size"])
    
    #append statistics for current index
    df.stats <- rbind(df.stats, list(index, as.character(overlap$id[1]), min, max, mean, sum), stringsAsFactors = F)
  
  }

  #set names
  names(df.stats) <- c("index", "id", "min", "max", "mean", "sum")
  
  return(df.stats)

}
