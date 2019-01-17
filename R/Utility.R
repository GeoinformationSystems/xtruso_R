#'
#' get value from list that is closest to provided value
#' 
#' @param list list of values
#' @param value input search value(s)
#' @param direction direction to search for closest value(s); default is "both", other valid values are "geq" (greater equal) and "leq" ("lower equal)
#' @return closest value from list
#'
x.utility.closest <- function(list,
                              value,
                              direction = "both") {
  
  if(!direction %in% c("both", "leq", "geq"))
    stop("Invalid direction.")
  
  #apply closer search to list of values and return
  return(sapply(value, function(x){
    
    #return value, if in list
    if(value %in% list) return(value)
  
    #compute lowest value for both directions (minimal absolute difference)
    if(direction == "both")
      return(which(abs(list - as.double(x)) == min(abs(list - as.double(x)))))
    
    #compute lower equal value, if there is no matching value return last item of list
    else if(direction == "leq"){
      sublist <- list[list - as.double(x) < 0]
      ifelse(length(sublist) == 0, return(list[1]), return(max(sublist)))
    }
      
    #compute greater equal value, if there is no matching value return last item of list
    else if(direction == "geq"){
      sublist <- list[list - as.double(x) > 0]
      ifelse(length(sublist) == 0, return(list[length(list)]), return(min(sublist)))
    }
    
  }))

}


#'
#' create time intervals within a given timespan
#' 
#' @param t.start start timestamp
#' @param t.end end timestamp
#' @param interval length of the target intervals
#' @param interval.uom unit of interval measurement
#' @param interval.cut flag: cut last interval to match t.end
#' @return set of intervals
#'
x.utility.time.intervals <- function(t.start,
                                     t.end, 
                                     interval, 
                                     interval.uom, 
                                     interval.cut = TRUE) {
  
  df.intervals <- data.frame()
  t.tmp <- t.start
  
  while(t.tmp < t.end){
    
    #reset t.start to previous t.tmp
    t.start <- t.tmp
    
    #increase time by interval
    t.tmp <- t.tmp + as.difftime(interval, units = interval.uom)
    
    #check, if interval.cut = TRUE and tmp larger than end time
    if(interval.cut && t.tmp > t.end)
      t.tmp = t.end
    
    #append interval to dataframe
    df.intervals <- rbind(df.intervals, data.frame(t.start=t.start, t.end=t.tmp))
    
  }
  
  return(df.intervals)
  
}


#'
#' disaggregate input raster with proxy rasters
#' 
#' @param raster input raster
#' @param stack proxy rasters
#' @param weights list of weights for stacked raster layers in disaggregation
#' @return disaggregated raster stack with length = length(stack)
#'
x.utility.raster.disaggregate <- function(raster,
                                          stack,
                                          weights = 1) {
  
  if(missing(raster))
    stop("Need to specify input raster.")
  
  if(missing(stack))
    stop("Need to specify input stack.")
  
  if(extent(stack) != extent(raster))
    stop("Extent for input raster and stack must be equal.")
  
  #multiply each stack layer with corresponding weight
  stack <- stack * weights
  
  #normalize stack
  stack.sum <- sum(stack)
  for(i in 1:nlayers(stack)){
    #divide by sum
    stack[[i]] <- stack[[i]] / stack.sum
    #set NaN to 0 (stack sum was 0))
    stack[[i]][is.nan(stack[[i]])] <- 0
  }
  
  #init and populate final stack for disaggregated values
  stack.result <- stack()
  for(i in 1:nlayers(stack)){
    stack.result <- raster::addLayer(stack.result, raster * stack[[i]])
  }
  
  #add average raster value for cells, which have a value in raster, but no corresponding weights in stack
  diff <- raster - sum(stack.result)
  for(i in 1:nlayers(stack.result)){
    stack.result[[i]] <- stack.result[[i]] + diff / nlayers(stack.result) 
  }
  
  return(stack.result)
  
}


#'
#' read GeoJSON from string
#' 
#' @param s.geojson GeoJSON input string
#' @param proj projection parameter
#' @return sp geometry object
#'
x.utility.parse.geojson <- function(s.geojson,
                                    proj = "+init=epsg:3857") {
  
  if(typeof(s.geojson) != 'character')
    stop("input must be a character string")
  
  #create temp file
  temp <- tempfile(fileext=".json")
  
  #write extent to file
  writeLines(extent, temp)
  
  tryCatch({
    
    #read JSON file
    sp.geojson <- rgdal::readOGR(temp, p4s=as.character(proj))
    
  }, error = function(e){
    
    sp.geojson = NULL
    warning("Could not parse GeoJSON character string")
    
  })
  
  #remove temp file
  file.remove(temp)
  
  return(sp.geojson)

}


#'
#' compute the great circle distance between two geographic coordinates (lat/lon) in km
#' @param lon1 longitude of 1st point
#' @param lat1 latitude of 1st point
#' @param lon2 longitude of 2st point
#' @param lat2 latitude of 2st point
#' @param R radius of the sphere, default = 6371 (mean radius of the earth)
#' @param is.degree flag: provided coords are in degree, not radians
#' @return great circle distance in km
#' @export
#' 
x.utility.gcd <- function(lon1, 
                          lat1, 
                          lon2, 
                          lat2, 
                          R=6371, 
                          is.degree=T) {
  
  #convert to radians
  if(is.degree){
    lon1 <- lon1 * pi / 180
    lat1 <- lat1 * pi / 180
    lon2 <- lon2 * pi / 180
    lat2 <- lat2 * pi / 180
  }
  
  return(acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(lon2-lon1)) * R)
}


#'
#' get z-coodinate for feature (mean for 2D features)
#' @param dem input elevation raster
#' @param feature input feature (sp class)
#' @return altitude
#' @export
#' 
x.utility.zCoord <- function(dem, 
                             feature) {
  
  return(as.numeric(raster::extract(dem, feature, weights=TRUE, fun=mean)))
  
}


