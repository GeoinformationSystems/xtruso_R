#' Retrieve timeseries data from OpenSensorWeb
#'
#' @param s.id station identifier
#' @param t.start start date (as.POSIXct)
#' @param t.end end date (as.POSIXct)
#' @return dataframe with measurements
#' @export
x.osw.get <- function(osw.url, 
                      osw.network, 
                      osw.device, 
                      osw.sensor,
                      t.start = NA, 
                      t.end = NA){
  
  #build request
  request <- x.osw.request(osw.url, osw.network, osw.device, osw.sensor, t.start, t.end)
    
  #request service
  response <- httr::GET(request, httr::accept("text/csv"))
  
  if(httr::http_status(response)$category != "Success")
    stop(paste0("HTTR returned: ", httr::http_status(response)$message))
  
  #parse CSV
  df.measurements <- httr::content(response, type="text/csv", encoding="UTF-8", col_types=readr::cols(begin = readr::col_datetime(format = "%Y-%m-%dT%H:%MZ"), v = readr::col_double()))
    
  if(nrow(df.measurements) == 0)
    warning(paste0("No measurements returned for ", request))
  
  return(df.measurements)
  
}


#' Build OpenSensorWeb request based on device and sensor code
#'
#' @param osw.configuration OSW configuration
#' @param deviceCode OSW device code
#' @param t.start start date (as.POSIXct)
#' @param t.end end date (as.POSIXct)
#' @return dataframe with measurements
#' 
x.osw.request <- function(osw.url, 
                          osw.network, 
                          osw.device, 
                          osw.sensor,
                          t.start = NA, 
                          t.end = NA){
  
  #build OSW request
  request <- paste0(osw.url, "/networks/", osw.network, "/devices/", osw.device, "/sensors/", osw.sensor)
  
  if(!is.na(t.start) && !is.na(t.end))
    request <- paste0(request,
                      "/measurements/raw",
                      "?start=", strftime(t.start , "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
                      "&end=", strftime(t.end , "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
                      "&includeLatest=true")
  
  return(request)
  
}

#' Get all stations from OSW matching the requested criteria
#'
#' @param url OSW url
#' @param networkCode network code
#' @param phenomenon phenomenon
#' @param extent requested extent (xmin,ymin,xmax,ymax)
#' @return stations
#' @export
#' 
x.osw.stations <- function(osw.url,
                           osw.network, 
                           phenomenon, 
                           extent) {
  
  if(missing(osw.url))
    stop("Must specify OSW url to request")
  
  if(missing(osw.network) && missing(phenomenon))
    stop("Must specify network code or phenomenon for OSW request")
  
  #set terms
  networksQuery = ''
  phenomenonQuery = ''
  if(!missing(osw.network)) networksQuery <- paste0('"networks":["', paste0(osw.network, collapse="\",\""),'"],')
  if(!missing(phenomenon)) phenomenonQuery <- paste0('"topic_phenomenons" : [{"topic": "Air and Atmosphere", "phenomenons":["', paste0(phenomenon, collapse="\",\""),'"]}],')
  
  #build request
  request <- paste0('{',
    '"aggregation" : "NONE",',
    '"fields":["device_code","network_code","phenomenon","sensor_code","geometry", "uom"],',
      '"filters" : {',
            phenomenonQuery,
            networksQuery,
            '"geo_bbox":{',
                '"top_left" : {"lon":', extent[1], ', "lat":', extent[4], '},',
                '"bottom_right" : {"lon":', extent[3], ', "lat":', extent[2], '}',
            '}',
      '}',
    '}')
  
  #request stations
  response <- httr::POST(osw.url, httr::accept("application/json"), httr::content_type("application/json"), body=request)
  content <- httr::content(response)
  hits <- content
  
  #parse response
  osw.stations <- do.call(rbind, lapply(1:length(hits), function(x){
    source <- hits[[x]]
    
    sp::SpatialPointsDataFrame(coords = source$geometry,
                               data = data.frame(
                                 sensorCode = source$sensor_code, 
                                 networkCode = source$network_code, 
                                 deviceCode = source$device_code, 
                                 phenomenon = source$phenomenon, 
                                 uom = source$uom,
                                 stringsAsFactors = F),
                               proj4string = sp::CRS("+init=epsg:4326"))
  }))
  
  return(osw.stations)
  
}


#' Get closest station matching the requested criteria
#'
#' @param osw.stations stations data frame
#' @param osw.url OSW Url, required, if max.t != NA
#' @param phenomenon phenomenon
#' @param geomety geomety for which the closest station will be determined (sp class)
#' @param max.radius maximum search radius for a station to be listed (in km)
#' @param max.num maximum number of stations to be returned (may return more stations, if distance of max.num equals max.num + x)
#' @param max.t maximum timestamp allowed (Posix)
#' @param c.height catchment height (mean)
#' @param max.deltaH maximum height difference between station and catchment
#' @return stations list
#' @export
#' 
x.osw.closest <- function(osw.stations,
                          osw.url,
                          phenomenon, 
                          geometry,
                          max.radius = 50,
                          max.num = 10,
                          t.start, 
                          t.end,
                          c.height,
                          max.deltaH = NA) {
  
  #transform stations and geometry to lat/lon
  if(sp::proj4string(osw.stations) != sp::CRS("+init=epsg:4326")@projargs)
    stations <- sp::spTransform(osw.stations, sp::CRS("+init=epsg:4326"))
  if(sp::proj4string(geometry) != sp::CRS("+init=epsg:4326")@projargs)
    geometry <- sp::spTransform(geometry, sp::CRS("+init=epsg:4326"))

  # subset stations by phenomenon
  osw.stations <- osw.stations[osw.stations$phenomenon == phenomenon, ]
  
  # compute distances of stations towards centroid of the geometry
  poi <- rgeos::gCentroid(geometry)
  osw.stations$dist <- x.utility.gcd(osw.stations@coords[, "lon"], osw.stations@coords[, "lat"], poi@coords[, "x"], poi@coords[, "y"])
  
  # select stations within max.radius
  osw.stations <- osw.stations[osw.stations$dist <= max.radius, ]
  if(length(osw.stations) == 0) return(osw.stations)
  
  # select stations by deltaH
  if(!is.na(max.deltaH)) {
    osw.stations$height <- x.utility.zCoord(xtruso::xtruso.dem.sn, osw.stations)
    osw.stations <- osw.stations[abs(osw.stations$height - c.height) <= max.deltaH, ]
    if(length(osw.stations) == 0) return(osw.stations)
  }
  
  # select stations with suitable timestamps
  osw.stations$keep = T
  for(i in 1:nrow(osw.stations)){
    stats <- x.osw.stats(osw.url, osw.stations[i, ]$networkCode, osw.stations[i, ]$deviceCode, osw.stations[i, ]$sensorCode)
    if(strptime(stats$sensor_stats$max_time, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC") < t.start)
      osw.stations@data[i, ]$keep = F
  }
  osw.stations <- osw.stations[osw.stations$keep == T, ]
  
  # select max.num stations
  if(nrow(osw.stations) > max.num)
    osw.stations <- osw.stations[which(osw.stations$dist %in% head(sort(osw.stations$dist), n = max.num)), ]
  
  return(osw.stations)
   
}


#' Get statistics for stations from OSW
#'
#' @param osw.url OSW API url
#' @param osw.network OSW network name
#' @param osw.device OSW device name
#' @param osw.sensor OSW sensor name
#' @return station statisitics as list
#' @export
#' 
x.osw.stats <- function(osw.url, 
                        osw.network, 
                        osw.device, 
                        osw.sensor){
  
  #build request
  request <- x.osw.request(osw.url, osw.network, osw.device, osw.sensor)
  
  #request service
  response <- httr::GET(request, httr::accept("application/json"))
  
  if(httr::http_status(response)$category != "Success")
    stop(paste0("HTTR returned: ", httr::http_status(response)$message))
  
  #parse CSV
  df.stats <- httr::content(response, type="application/json", encoding="UTF-8")
  
  return(df.stats)
  
}
