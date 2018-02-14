#' Read single timeseries from RADOLAN
#'
#' @param radolan.type RADOLAN type
#' @param time.start start date (ISO8601 time string)
#' @param time.end end date (ISO8601 time string)
#' @param time.format time format
#' @param time.zone time zone
#' @param ncdf.folder folder with NetCDF files
#' @param coord.x x coordinate
#' @param coord.y y coordinate
#' @export
Application.getTimeSeriesForCoord <- function(radolan.type,
                                              time.start,
                                              time.end,
                                              time.format = "%Y-%m-%d %H:%M:%S",
                                              time.zone = "UTC",
                                              ncdf.folder = "/ncdf",
                                              coord.x,
                                              coord.y) {
  
  #get coord index
  coord.index <- Utility.getRADOLANIndexFromCoord(coord.x, coord.y)
  
  return(Application.getTimeSeriesForBounds(radolan.type, time.start, time.end, time.format, time.zone, ncdf.folder, coord.indices=coord.index))
  
}


#' Read single timeseries from RADOLAN
#'
#' @param radolan.type RADOLAN type
#' @param time.start start date (ISO8601 time string)
#' @param time.end end date (ISO8601 time string)
#' @param time.format time format
#' @param time.zone time zone
#' @param ncdf.folder folder with NetCDF files
#' @param coord.indices coordinate bounds as list of x,y indices (list(c(x1,x2,...), c(y1,y2,...)))
#' @param fun named functions list to be applied to requested value subset, can be used to define weight matrix for requested subset (same size as subset defined by coord.indices)
#' @return timeseries for requested RADOLAN product, coordinate and timeframe
#' @export
Application.getTimeSeriesForBounds <- function(radolan.type,
                                               time.start,
                                               time.end,
                                               time.format = "%Y-%m-%d %H:%M:%S",
                                               time.zone = "UTC",
                                               ncdf.folder = "/ncdf",
                                               coord.indices,
                                               fun = list(mean=mean)) {
  
  #get start/end time index
  time.start <- as.POSIXct(time.start, format=time.format, tz=time.zone)
  time.end <- as.POSIXct(time.end, format=time.format, tz=time.zone)
  
  #get target year(s)
  years <- format(time.start, "%Y") : format(time.end, "%Y")
  
  #set NetCDF file(s)
  ncdf.files = list()
  for(year in years){
    ncdf.files[as.character(year)] <- paste0(ncdf.folder, "/radolan", radolan.type, "-", year, ".nc")
  }
  
  #read timeseries for each year
  timeseries <- data.frame(timestamp=integer(), value=numeric())
  
  for(year in years){
    
    #determine, if full year needs to be requested
    full.year <- ifelse(year == min(years) || year == max(years), F, T)
    
    #open NetCDF file
    ncdf <- Radolan2Ncdf.openFile(ncdf.files[[as.character(year)]])
    
    #get timeseries for selected year
    timeseries <- rbind(timeseries, Application.getTimeSeriesFromNetCDF(ncdf, full.year=full.year, time.start=time.start, time.end=time.end, coord.indices=coord.indices, fun=fun))
    
    #close NetCDF file
    Radolan2Ncdf.closeFile(ncdf)
    
  }
  
  #return final dataframe, calculates mean for each timestamp
  return(timeseries)
  
}


#' Read timeseries stats from NetCDF
#'
#' @param ncdf NetCDF file pointer
#' @param full.year flag: read full year; if true, start and end date are ignored
#' @param time.start start date
#' @param time.end end date
#' @param coord.indices coordinate bounds as list of x,y indices (list(c(x1,x2,...), c(y1,y2,...)))
#' @param fun named functions list to be applied to requested value subset, can be used to define weight matrix for requested subset (same size as subset defined by coord.indices)
Application.getTimeSeriesFromNetCDF <- function(ncdf,
                                                full.year = F,
                                                time.start,
                                                time.end,
                                                coord.indices,
                                                fun){
  
  #determine start and end indices
  timestamps <- ncdf$dim$t$vals
  if(full.year){
    t.index.start <- 1
  }
  else {
    t.index.start <- which(timestamps == Utility.getClosest(timestamps, time.start, direction="leq"))
    t.index.end <- which(timestamps == Utility.getClosest(timestamps, time.end, direction="geq"))
    timestamps <- timestamps[t.index.start : t.index.end]
  }
  
  #request NetCDF subset
  subset <- Radolan2Ncdf.requestSubset(ncdf, start=c(min(coord.indices$col), min(coord.indices$row), t.index.start), count=c(length(coord.indices$col), length(coord.indices$row), ifelse(full.year, -1, length(timestamps))), tIndex = T)
  
  #init final dataframe with timestamps
  subset.df <- data.frame(timestamp=timestamps)
  
  #calculate aggregate values, based on provided functions
  for(f in names(fun)){
    subset.df[f] <- apply(subset, 3, fun[[f]])
  }
  
  return(subset.df)
  
}


#' Read timeseries stats from RADOLAN for single polygon 
#'
#' @param radolan.type RADOLAN type
#' @param time.start start date (ISO8601 time string)
#' @param time.end end date (ISO8601 time string)
#' @param time.format time format
#' @param time.zone time zone
#' @param polygon polygon for zonal stats
#' @export
Application.getTimeSeriesForPolygon <- function(radolan.type,
                                                time.start,
                                                time.end,
                                                time.format = "%Y-%m-%d %H:%M:%S",
                                                time.zone = "UTC",
                                                ncdf.folder = "/ncdf",
                                                polygon) {
  
  #get indices and corresponding weights for polygon
  coord.indices <- Utility.getRADOLANIndicesForPolygon(polygon)
  
  #get area of polygon, requires transformation to get geodesic area
  polygon.area <- raster::area(sp::spTransform(polygon, CRS('+init=EPSG:4326'))) / 1000000
  
  #init weight function to extract mean precipitation
  fun <- list(
    mean = function(x, weights=coord.indices$weights) { return(sum(x * weights)) },
    max = function(x, weights=coord.indices$weights) { return(max(x[weights > 0])) },
    min = function(x, weights=coord.indices$weights) { return(min(x[weights > 0])) },
    sum = function(x, weights=coord.indices$weights, area=polygon.area) { return(sum(x * weights) * area) }
  )
  
  #get time series
  timeseries <- Application.getTimeSeriesForBounds(radolan.type=radolan.type, time.start=time.start, time.end=time.end, time.format=time.format, time.zone=time.zone, ncdf.folder=ncdf.folder, coord.indices=coord.indices, fun=fun)
  return(timeseries)
  
}


#' Get upstream catchments
#'
#' @param catchments input catchments
#' @param graph input graph
#' @param name catchment identifier
#' @param geometry flag: get catchment geometry; of false, a list of identifiers is returned
#' @param dissolve flag: dissolve geometry; only required if geometry == T
#' @return list of upstream catchment identifiers or catchment geometries
#' @export
Application.getUpstreamCatchments <- function(catchments = xtruso::xtruso.catchments,
                                              graph = xtruso::xtruso.catchments.graph,
                                              name,
                                              geometry = F,
                                              dissolve = F) {
  
  #get upstream graph
  upstream <- CatchmentGraph.getUpstream(graph, name)
  
  #get list of upstream catchments
  upstream.ids <- igraph::V(upstream)$name
  
  #return, if geometry == F
  if(!geometry)
    return(upstream.ids)
  
  #get source catchment
  catchment <- catchments[catchments@data$GKZNR == name, ]
  
  #get geometry for identifiers
  upstream.selection <- catchments[catchments@data$GKZNR %in% upstream.ids, ]
  
  #return geometries, if dissolve == F
  if(!dissolve)
    return(upstream.selection)
  
  #aggregate polygons
  upstream.selection <- stats::aggregate(upstream.selection, FUN=function(x){return(NA)})
  
  #set attributes from initial catchment
  upstream.selection@data <- catchment@data
  
  return(upstream.selection)
  
}


# radolan.type <- "RW"
# time.start <- "2016-01-01 12:00:00"
# time.end <- "2016-04-01 00:00:00"
# time.format = "%Y-%m-%d %H:%M:%S"
# time.zone = "UTC"
# coord.x <- 114
# coord.y <- -4000


# path and file name, set dname
# for(y in 2010:2014){
# 
#   message(paste0("--- NetCDF y = ", y, " ---"))
# 
#   ncdf.filepath <- paste0("D:/Geodaten/RADOLAN/NetCDF/radolanRW",y,".nc")
#   radolan.folder <- paste0("D:/Geodaten/RADOLAN/DWD_Downloads/RW/",y)
#   radolan.type <- "RW"
# 
#   time <- Sys.time()
#   Radolan2Ncdf(ncdf.filepath, radolan.folder, radolan.type, compression = 3)
# 
#   message(paste0("RUNTIME: ", difftime(Sys.time(), time, units = "mins")))
# 
# }
# 
# 
# test <- Radolan2Ncdf.openFile(ncdf.filepath)
# raster <- Radolan2Ncdf.requestImage(test, timestamp=1452977400)
# 
# for(i in 1:5){
#   time <- Sys.time()
#   subset <- Radolan2Ncdf.requestSubset(test, start=c(500,500,1452977400), count=c(i,i,-1))
#   message(difftime(Sys.time(), time, units = "secs"))
# }
# 
# 
# test <- Radolan2Ncdf.openFile("D:/Geodaten/RADOLAN/NetCDF/radolanRW2016.nc")
# 
# time <- Sys.time()
# subset <- Radolan2Ncdf.requestSubset(test, start=c(500,500,1), count=c(1,1,-1))
# runtime1 <- Sys.time() - time
# 
# time <- Sys.time()
# raster <- Radolan2Ncdf.requestImage(test, timestamp=1452973800)
# runtime2 <- Sys.time() - time
# 
# Radolan2Ncdf.closeFile(test)
# 
# 
# 
#rw <- ReadRadolan("https://opendata.dwd.de/weather/radar/radolan/rw", "RW")
#sf <- ReadRadolan("https://opendata.dwd.de/weather/radar/radolan/sf", "SF")
#rx <- ReadRadolan("https://opendata.dwd.de/weather/radar/composit/rx", "RX")
