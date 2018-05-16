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


