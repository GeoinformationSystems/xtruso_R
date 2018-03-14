#' Read timeseries from RADOLAN NetCDF file
#'
#' @param ncdf.folder folder with NetCDF files (files must follow naming convention radolan%%radolan.type%%-%%year%%)
#' @param radolan.type RADOLAN type
#' @param t.start start date
#' @param t.end end date
#' @param t.format time format, required if t.start and/or t.end are not given as POSIXct
#' @param t.zone time zone, required if t.format is applied
#' @param extent extent or polygon for which statistics are calculated; if -1, statistics are calculated for the whole extent
#' @return timeseries for requested RADOLAN product, coordinate and timeframe
#' @export
#' 
x.app.radolan.timeseries <- function(ncdf.folder,
                                     radolan.type = "RW",
                                     t.start,
                                     t.end,
                                     t.format = "%Y-%m-%d %H:%M:%S",
                                     t.zone = "UTC",
                                     extent = -1) {
  
  if(missing(ncdf.folder))
    stop("Need to specify NetCDF folder.")
  
  if(missing(radolan.type))
    stop("Need to specify RADOLAN type.")
  
  if(missing(t.start))
    stop("Need to specify start timestamp.")
  
  if(missing(t.end))
    stop("Need to specify end timestamp.")
  
  #set start/end time as POSIXct
  if(!"POSIXct" %in% class(t.start))
    t.start <- as.POSIXct(t.start, format=t.format, tz=t.zone)
  if(!"POSIXct" %in% class(t.end))
    t.end <- as.POSIXct(t.end, format=t.format, tz=t.zone)
  
  #get target year(s)
  years <- format(t.start, "%Y") : format(t.end, "%Y")
  
  #set timestamp to numeric (as stored in NetCDF)
  timestamp <- as.double(c(t.start, t.end))
  
  #set NetCDF file(s)
  ncdf.files = list()
  for(year in years){
    ncdf.files[as.character(year)] <- paste0(ncdf.folder, "/radolan", radolan.type, "-", year, ".nc")
  }
  
  #init timeseries dataframe
  timeseries <- data.frame()
  
  for(year in years){
    
    #determine, if full year needs to be requested
    full.year <- if(year == min(years) || year == max(years)) FALSE else TRUE
    
    #open NetCDF file
    ncdf <- x.ncdf.open(ncdf.files[[as.character(year)]])
    
    #get timeseries from NetCDF
    timeseries <- rbind(timeseries, x.ncdf.subset(ncdf, extent=extent, timestamp=timestamp, statistics=T))
    
    #close NetCDF file
    x.ncdf.close(ncdf)
    
  }
  
  #return final dataframe, calculates mean for each timestamp
  return(timeseries)
  
}


#' Read single raster from RADOLAN NetCDF file
#'
#' @param ncdf.folder folder with NetCDF files (files must follow naming convention radolan%%radolan.type%%-%%year%%)
#' @param radolan.type RADOLAN type
#' @param timestamp timestamp to be requested
#' @param t.format time format, required if t.start and/or t.end are not given as POSIXct
#' @param t.zone time zone, required if t.format is applied
#' @return RADOLAN image for requested timestamp or stack of 2 images, if timestamp lies in between provision timestamps
#' @export
#' 
x.app.radolan.raster <-  function(ncdf.folder,
                                  radolan.type,
                                  timestamp,
                                  t.format = "%Y-%m-%d %H:%M:%S",
                                  t.zone = "UTC") {
  
  if(missing(ncdf.folder))
    stop("Need to specify NetCDF folder.")
  
  if(missing(radolan.type))
    stop("Need to specify RADOLAN type.")
  
  if(missing(timestamp))
    stop("Need to specify timestamp.")
  
  #set timestamp as POSIXct
  if(!"POSIXct" %in% class(timestamp))
    timestamp <- as.POSIXct(timestamp, format=t.format, tz=t.zone)
  
  #get target year
  year <- format(timestamp, "%Y")
  
  #set file
  ncdf.file <- paste0(ncdf.folder, "/radolan", radolan.type, "-", year, ".nc")
  
  #set timestamp to numeric (as stored in NetCDF)
  timestamp <- as.double(timestamp)
  
  #open NetCDF file
  ncdf <- x.ncdf.open(ncdf.file)
  
  #get RADOLAN image
  raster <- x.ncdf.subset(ncdf, extent=-1, timestamp=timestamp, as.raster=T) 
  
  #close NetCDF file
  x.ncdf.close(ncdf)
  
  return(raster)
  
}


#' Get upstream catchments
#'
#' @param catchments input catchments
#' @param c.graph input catchment graph
#' @param c.id start catchment identifier
#' @param c.x x coordinate of start location, used to identify c.id if not set
#' @param c.y y coordinate of start location, used to identify c.id if not set
#' @param geometry flag: get catchment geometry; if false, a list of identifiers is returned
#' @param dissolve flag: dissolve geometry; only required if geometry == T
#' @return list of upstream catchment identifiers or catchment geometries
#' @export
#' 
x.app.catchment.upstream <- function(catchments = xtruso::xtruso.catchments,
                                     c.graph = xtruso::xtruso.catchments.graph,
                                     c.id,
                                     c.x = NA,
                                     c.y = NA,
                                     geometry = F,
                                     dissolve = F,
                                     as.json = F) {
  
  #set catchment id
  if(missing(c.id)){
    if(missing(c.x) || missing(c.y)) stop("Must specify catchment id or location with c.x and c.y")
    
    #select catchment (assumes same CRS) and set c.id
    c.id <- sp::over(SpatialPoints(cbind(c.x, c.y), proj4string=crs(catchments)), catchments)$GKZNR
    if(is.na(c.id)) stop("No catchment found for provided location")
  }
  
  #get upstream graph
  upstream <- x.graph.neighborhood.in(c.graph, as.numeric(c.id))
  
  #get list of upstream catchments
  upstream.ids <- as.numeric(igraph::V(upstream)$name)
  
  #return, if geometry == F
  if(!geometry)
    return(upstream.ids)
  
  #get source catchment
  catchment <- catchments[catchments@data$GKZNR == c.id, ]
  
  #get geometry for identifiers
  upstream.selection <- catchments[catchments@data$GKZNR %in% upstream.ids, ]
  area <- sum(upstream.selection@data$Area_sqkm)
  
  #dissolve
  if(dissolve){
    
    #aggregate polygons
    upstream.selection <- stats::aggregate(upstream.selection, FUN=function(x){return(NA)})
    
    #reset data
    upstream.selection@data <- data.frame(
      id = catchment@data$GKZNR,
      initial_GKZ = catchment@data$GKZ,
      area_sqkm = area)
    
  }
  
  #convert to json
  if(as.json){
    
    #create temp file
    temp <- tempfile()
    
    #write GeoJSON
    writeOGR(upstream.selection, temp, layer="geojson", driver="GeoJSON")
    
    #read JSON file
    upstream.selection <- paste(readLines(temp), collapse=" ")
    
    #remove temp file
    file.remove(temp)
    
  }
  
  return(upstream.selection)
  
}


#' Get RADOLAN timeseries for upstream catchment
#'
#' @param c.id start catchment identifier
#' @return RADOLAN RW timeseries for upstream catchment
#' @export
#' 
x.app.catchment.radolan <- function(c.id,
                                    radolan.type = "RW",
                                    t.start = "2006-01-01 00:00:00",
                                    t.end = "2017-12-31 23:59:00",
                                    inf.rm = T) {
  
  #get catchment area
  area <- x.app.catchment.upstream(c.id=c.id, geometry=T, dissolve=T)
  
  #get timeseries
  ts <- x.app.radolan.timeseries(ncdf.folder, radolan.type=radolan.type, t.start=t.start, t.end=t.end, extent=area)
  
  #mark and if requested remove lines with min = Inf (== no values in RADOLAN image)
  ts.inf <- ts[is.infinite(ts$min), ]
  if(nrow(ts.inf) > 0) warning(paste0("For ", nrow(ts.inf), " timestamps there are no values available."))
  if(inf.rm) ts <- ts[is.finite(ts$min), ]
  
  return(ts)
  
}
