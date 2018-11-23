#' Read timeseries from RADOLAN NetCDF file
#'
#' @param ncdf.folder folder with NetCDF files (files must follow naming convention radolan%%radolan.type%%-%%year%%)
#' @param radolan.type RADOLAN type
#' @param t.start start date
#' @param t.end end date
#' @param t.format time format, required if t.start and/or t.end are not given as POSIXct
#' @param t.zone time zone, required if t.format is applied
#' @param extent extent or polygon for which statistics are calculated; if -1, statistics are calculated for the whole extent
#' @param statistics flag: request statistics for extent
#' @param proj projection, if extent is given as json string
#' @return timeseries for requested RADOLAN product, coordinate and timeframe
#' @export
#' 
x.app.radolan.timeseries <- function(ncdf.folder = "/ncdf",
                                     radolan.type = "RW",
                                     t.start,
                                     t.end,
                                     t.format = "%Y-%m-%d %H:%M:%S",
                                     t.zone = "UTC",
                                     extent = -1,
                                     statistics = T,
                                     proj = "+init=epsg:3857") {
  
  if(missing(t.start))
    stop("Need to specify start timestamp.")
  
  if(missing(t.end))
    stop("Need to specify end timestamp.")
  
  #try to parse GeoJSON
  if(typeof(extent) == 'character'){
    
    #create temp file
    temp <- tempfile(fileext=".json")
    
    #write extent to file
    writeLines(extent, temp)
    
    #read JSON file
    extent <- rgdal::readOGR(temp, p4s=as.character(proj))
    
    #remove temp file
    file.remove(temp)
    
  }
  
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
  
  #flag: first year in iteration
  first <- T
  
  #result object
  timeseries <- NULL
  
  for(year in years){
    
    #next, if file does not exist
    if(!file.exists(ncdf.files[[as.character(year)]]))
      next
    
    #determine, if full year needs to be requested
    full.year <- if(year == min(years) || year == max(years)) FALSE else TRUE
    
    #open NetCDF file
    ncdf <- x.ncdf.open(ncdf.files[[as.character(year)]])
    
    #get timeseries from NetCDF
    subset <- x.ncdf.subset(ncdf, extent=extent, timestamp=timestamp, statistics=statistics)
    
    #get timeseries from NetCDF
    if(first){
      #initialize timeseries
      timeseries <- subset
      first <- F
    } else {
      #combine timeseries
      if(statistics) timeseries <- rbind(timeseries, subset)
      else timeseries <- abind(timeseries, subset, along=3) 
    }
    
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
x.app.radolan.raster <- function(ncdf.folder = "/ncdf",
                                 radolan.type = "RW",
                                 timestamp,
                                 extent = -1,
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
  raster <- x.ncdf.subset(ncdf, extent=extent, timestamp=timestamp, as.raster=T) 
  
  #close NetCDF file
  x.ncdf.close(ncdf)
  
  return(raster)
  
}


#' Read map image from RADOLAN NetCDF file based on WMS parameters
#'
#' @param ncdf.folder folder with NetCDF files (files must follow naming convention radolan%%radolan.type%%-%%year%%)
#' @param t.format time format, required if t.start and/or t.end are not given as POSIXct
#' @param t.zone time zone, required if t.format is applied
#' @param p.layer RADOLAN layer
#' @param p.timestamp RADOLAN timestamp
#' @param p.width
#' @param p.height
#' @param p.bbox
#' @param p.srs
#' @param p.format
#' @param p.transparent
#' @return RADOLAN image for requested timestamp
#' @export
#' 
x.app.radolan.getMap <- function(ncdf.folder = "/ncdf",
                                 t.format = "%Y-%m-%d %H:%M:%S",
                                 t.zone = "UTC",
                                 p.layer = "RW",
                                 p.timestamp = "latest",
                                 p.width,
                                 p.height,
                                 p.bbox,
                                 p.srs,
                                 p.format = "image/png",
                                 p.transparent = "true") {
  
  tryCatch ({
    
    #validate p.layer
    if(!(p.layer) %in% c("RW"))
      stop(paste0("Layer not supported: ", p.layer))
    
    #validate format
    format <- gsub("%2F", "/", p.format)
    if(!(format) %in% c("image/png"))
      stop(paste0("Layer not supported: ", p.format))
    
    #get timestamp
    timestamp <- if(p.timestamp == "latest") 
      max(x.app.radolan.timestamps(ncdf.folder, p.layer, format(Sys.time(), "%Y"))) else 
        as.POSIXct(p.timestamp, format=t.format, tz=t.zone)
    
    #validate timestamp
    if(!(format(timestamp , "%Y") %in% 2006:2018) || format(timestamp , "%M") != 50)
      stop(paste0("Invalid timestamp: ", timestamp))
    
    #get and validate width and height
    width <- as.numeric(p.width)
    height <- as.numeric(p.height)
    
    #get and validate bounding box (minx,miny,maxx,maxy)
    bbox <- as.double(unlist(strsplit(gsub("%2C", ",", p.bbox), ",")))
    
    #get and validate crs
    crs <- crs(paste0("+init=", gsub("%3A", ":", p.srs)))
    
    #set extent
    extent <- extent(bbox[1], bbox[3], bbox[2], bbox[4])
    extent.proj <- as(extent, 'SpatialPolygons')
    sp::proj4string(extent.proj) <- crs
    extent.proj <- extent(sp::spTransform(extent.proj, xtruso::radolan.configuration.crs)) + 50
    
    #get and validate transparency
    transparent <- as.logical(p.transparent)
    
    #get raster from NetCDF
    raster <- xtruso::x.app.radolan.raster(ncdf.folder=ncdf.folder, extent=extent.proj, radolan.type=p.layer, timestamp=timestamp)
    
    #reproject raster
    if(proj4string(raster) != crs@projargs)
      raster <- raster::projectRaster(raster, crs=crs)
    
    #set 0 to NA to enable transparency
    raster[raster == 0] <- NA
    
    #resample with target extent
    raster.target <- raster(nrow=height, ncol=width, crs=crs, ext=extent)
    raster.target <- raster::resample(raster, raster.target, method='ngb')
    
    #write PNG
    col.map <- xtruso::radolan.configuration[[p.layer]]$col.map
    raster.frame <- as(raster.target, 'SpatialPixelsDataFrame')
    raster.frame$colors <- as.numeric(raster::cut(raster.frame$layer, breaks=c(0,col.map$limit)))
    
    filename = "map.png"
    rgdal::writeGDAL(raster.frame[, 'colors'], filename, drivername=toupper(gsub("image/", "", format)), type="Byte", mvFlag=0, colorTables=list(col.map$col))
    
    return(data.frame("timestamp" = timestamp, "file" = filename))
  
  }, error = function(e) {
    return(paste0("Error: ", e))
  })
  
}


#' 
#' get all supported timestamps from NetCDF
#'
#' @param ncdf.folder folder with NetCDF files
#' @param radolan.folder folder with RADOLAN images
#' @param year requested year
#' @export
#' 
x.app.radolan.timestamps <- function(ncdf.folder = "/ncdf",
                                     radolan.type = "RW",
                                     year = 2006:format(Sys.time(), "%Y")) {
  
  #get NetCDF file(s)
  ncdf.files <- paste0(ncdf.folder, "/radolanRW-", year, ".nc")
  
  #get all timestamps from NetCDF
  timestamps <- c()
  for(file in ncdf.files) {
    
    #next, if file does not exist
    if(!file.exists(file))
      next
    
    #extract timestamps from NetCDF file
    ncdf <- x.ncdf.open(file)
    timestamps <- c(timestamps, ncdf$dim$t$vals)
    x.ncdf.close(ncdf)
  }
  
  return(as.POSIXct(timestamps, origin="1970-01-01", tz="UTC"))
  
}


#' 
#' Update NetCDF file
#'
#' @param ncdf.folder folder with NetCDF files
#' @param radolan.folder folder with RADOLAN images
#' @export
#' 
x.app.radolan.rw.update <- function(ncdf.folder = "/ncdf",
                                    radolan.folder = NA) {
  
  #set ncdf file for current year
  ncdf.file <- paste0(ncdf.folder, "/radolanRW-", format(Sys.time(), "%Y"), ".nc")
  
  #run update
  x.radolan.ncdf.update(ncdf.file, xtruso::radolan.configuration$RW, radolan.folder)
  
}


#' Read forecast timeseries from COSMO NetCDF file
#'
#' @param ncdf.folder folder with NetCDF files (files must follow naming convention cosmoD2-%%parameter%%-%%year%%.nc)
#' @param radolan.type RADOLAN type
#' @param timestamp timestamp for which the forecast will be requested
#' @param t.zone time zone, required if t.format is applied
#' @param catchment.id 
#' @param extent extent or polygon for which statistics are calculated; if -1, statistics are calculated for the whole extent
#' @return timeseries for requested RADOLAN product, coordinate and timeframe
#' @export
#' 
x.app.cosmo.forecast <- function(ncdf.folder = "/ncdf",
                                 cosmo.configuration,
                                 timestamp = "latest",
                                 t.format = "%Y-%m-%d %H:%M:%S",
                                 t.zone = "UTC",
                                 extent = -1,
                                 proj = "+init=epsg:3857") {
  
  if(missing(cosmo.configuration))
    stop("Need to specify a COSMO configuration.")
  
  #try to parse GeoJSON
  if(typeof(extent) == 'character') {
    extent <- x.utility.parse.geojson(extent, proj)
    if(is.null(extent)) stop("Provided extent is invalid.")
  }
  
  if(!"POSIXt" %in% class(timestamp) && timestamp != "latest")
    timestamp <- as.POSIXct(timestamp, format=t.format, tz=t.zone)
 
  #get target year
  year <- if("POSIXt" %in% class(timestamp)) format(timestamp, "%Y") else format(Sys.time(), "%Y")
  
  #set file
  ncdf.file <- paste0(ncdf.folder, "/cosmoD2-", cosmo.configuration$parameter, "-", year, ".nc")
  if(!file.exists(ncdf.file)) stop (paste0("COSMO data for ", year, " is not available."))
  
  #open NetCDF file
  ncdf <- x.ncdf.open(ncdf.file)
  
  #request forecast
  cosmo.subset <- x.cosmode.ncdf.forecast(ncdf, extent=extent, timestamp=timestamp, statistics=T)
  
  #close NetCDF file
  x.ncdf.close(ncdf)

  return(cosmo.subset)
  
}


#' Read single raster from COSMO-D2 NetCDF file
#'
#' @param ncdf.folder folder with NetCDF files (files must follow naming convention cosmoD2-%%parameter%%-%%year%%.nc)
#' @param parameter COSMO parameter
#' @param timestamp timestamp to be requested
#' @param t.format time format, required if timestamp is not given as POSIXct
#' @param t.zone time zone, required if t.format is applied
#' @param forecast forecast step(s)
#' @return RADOLAN image for requested timestamp or stack of 2 images, if timestamp lies in between provision timestamps
#' @export
#' 
x.app.cosmo.raster <- function(ncdf.folder,
                               parameter,
                               timestamp,
                               t.format = "%Y-%m-%d %H:%M:%S",
                               t.zone = "UTC",
                               forecast = -1) {
  
  if(missing(ncdf.folder))
    stop("Need to specify NetCDF folder.")
  
  if(missing(parameter))
    stop("Need to specify RADOLAN type.")
  
  if(missing(timestamp))
    stop("Need to specify timestamp.")
  
  if(missing(forecast))
    stop("Need to specify forecast")
  
  #set timestamp as POSIXct
  if(!"POSIXt" %in% class(timestamp))
    timestamp <- as.POSIXct(timestamp, format=t.format, tz=t.zone)
  
  #get target year
  year <- format(timestamp, "%Y")
  
  #set file
  ncdf.file <- paste0(ncdf.folder, "/cosmoD2-", parameter, "-", format(Sys.time(), "%Y"), ".nc")
  
  #set timestamp to numeric (as stored in NetCDF)
  timestamp <- as.double(timestamp)
  
  #open NetCDF file
  ncdf <- x.ncdf.open(ncdf.file)
  
  #get COSMO image
  raster <- x.ncdf.subset(ncdf, extent=-1, timestamp=timestamp, forecast=forecast, as.raster=T) 
  
  #close NetCDF file
  x.ncdf.close(ncdf)
  
  return(raster)
  
}


#' 
#' Update Cosmo D2 file
#'
#' @param ncdf.folder folder with NetCDF files
#' @param radolan.configuration RADOLAN configuration
#' @param timestamp timestamp from which to update
#' @param radolan.folder folder with RADOLAN images
#' @export
#' 
x.app.cosmo.update <- function(ncdf.folder, 
                               cosmo.configuration, 
                               append = F) {
  
  #set ncdf file for current year
  ncdf.file <- paste0(ncdf.folder, "/cosmoD2-", cosmo.configuration$parameter, "-", format(Sys.time(), "%Y"), ".nc")
  
  #run update
  x.cosmode.ncdf.update(ncdf.file, cosmo.configuration, append)
  
}


#' 
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
                                     station.id,
                                     geometry = F,
                                     dissolve = F,
                                     as.json = F) {
  
  #set catchment id
  if(missing(c.id))
    c.id <- x.app.catchment.id(catchments, station.id=station.id, c.x = c.x, c.y = c.y)
  
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
    rgdal::writeOGR(upstream.selection, temp, layer="geojson", driver="GeoJSON")
    
    #read JSON file
    upstream.selection <- paste(readLines(temp), collapse=" ")
    
    #remove temp file
    file.remove(temp)
    
  }
  
  return(upstream.selection)
  
}


#'
#' Get catchment identifier by station id or coordinate
#'
#' @param catchments input catchments
#' @param station.catchments mapping between station and catchment ids
#' @param station.id station id
#' @param c.x x coordinate of start location, only used if station.id is missing
#' @param c.y y coordinate of start location, only used if station.id is missing
#' @return catchment identifier
#' @export
#' 
x.app.catchment.id <- function(catchments = xtruso::xtruso.catchments,
                               station.catchments = xtruso::xtruso.stations.catchment,
                               station.id,
                               c.x = NA,
                               c.y = NA) {
  
  #select catchment by coordinates (assumes same CRS) and set c.id
  if(missing(station.id))
    c.id <- sp::over(SpatialPoints(cbind(c.x, c.y), proj4string=crs(catchments)), catchments)$GKZNR
  else
    c.id <- station.catchments[station.catchments$PEG_MSTNR==station.id, "MIN_GKZNR"]
  
  if(is.na(c.id)) stop("No catchment found for provided station or location")
  return(c.id)
  
}


#' Get RADOLAN timeseries for upstream catchment
#'
#' @param ncdf.folder NetCDF folder
#' @param c.id start catchment identifier
#' @param t.start first timestamp
#' @param t.end last timestamp
#' @param inf.rm flag: remove undefined measurements
#' @return RADOLAN RW timeseries for upstream catchment
#' @export
#' 
x.app.catchment.radolan <- function(ncdf.folder = "/ncdf",
                                    c.id,
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


#' Get COSMO forecast for upstream catchment
#'
#' @param ncdf.folder NetCDF folder
#' @param c.id start catchment identifier
#' @param timestamp timestamp
#' @param inf.rm flag: remove undefined measurements
#' @return OSMO timeseries for upstream catchment
#' @export
#' 
x.app.catchment.cosmo <- function(ncdf.folder = "/ncdf",
                                  c.id,
                                  timestamp = "latest",
                                  inf.rm = T) {
  
  #get catchment area
  area <- x.app.catchment.upstream(c.id=c.id, geometry=T, dissolve=T)
  
  #get timeseries
  ts <- x.app.cosmo.forecast(ncdf.folder, xtruso::cosmo.configuration$tot_prec, timestamp=timestamp, extent=area)
  
  #mark and if requested remove lines with min = Inf (== no values in COSMO image)
  ts.inf <- ts[is.infinite(ts$min), ]
  if(nrow(ts.inf) > 0) warning(paste0("For ", nrow(ts.inf), " timestamps there are no values available."))
  if(inf.rm) ts <- ts[is.finite(ts$min), ]
  
  return(ts)
  
}


#' Get discharge timeseries for selecte station id
#'
#' @param s.id station identifier
#' @param t.start first timestamp
#' @param t.end last timestamp
#' @param t.format timestamp format string
#' @param t.zone time zone
#' @param ows flag: retrieve data from OpenSensorWeb, instead of HWIMS
#' @param hwims.configuration HWIMS configuration settings
#' @param hwims.authentication valid HWIMS authentication (list(user="username", pass="password"))
#' @return discharge timeseries for selected station
#' @export
#' 
x.app.station.dc <- function(s.id,
                             t.start = "2006-01-01 00:00:00",
                             t.end = Sys.time(),
                             t.format = "%Y-%m-%d %H:%M:%S",
                             t.zone = "UTC",
                             osw = F,
                             osw.configuration = xtruso::osw.configuration$HWIMS_DC_15min,
                             hwims.configuration = xtruso::hwims.configuration$Q_15min,
                             hwims.authentication) {
  
  # check, if station exists
  if(!s.id %in% xtruso::xtruso.stations.catchment$PEG_MSTNR)
    stop(paste0("Station ", s.id, " is not available"))
  
  # set timestamps as POSIXct
  if(!"POSIXct" %in% class(t.start))
    t.start <- as.POSIXct(t.start, format=t.format, tz=t.zone)
  if(!"POSIXct" %in% class(t.end))
    t.end <- as.POSIXct(t.end, format=t.format, tz=t.zone)
  
  #get measurements
  if(osw) df.measurements <- x.osw.get(osw.configuration, s.id, t.start, t.end)
  else df.measurements <- x.hwims.get(hwims.configuration, s.id, t.start, t.end, hwims.authentication)
  
  return(df.measurements)
 
  
}
