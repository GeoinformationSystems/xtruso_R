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
    warning(paste0("No measurements returned for ", osw.sensor))
  
  return(df.measurements)
  
}


#' Build OpenSensorWeb measurement request based on device and sensor code
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
  request <- paste0(osw.url, "/networks/", osw.network, "/devices/", osw.device, "/sensors/", osw.sensor, "/measurements/raw")
  
  if(!is.na(t.start) && !is.na(t.end))
    request <- paste0(request, 
                      "?start=", strftime(t.start , "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
                      "&end=", strftime(t.end , "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
  
  return(request)
  
}

#' Get all stations from OSW matching the requested criteria
#'
#' @param networkCode network code
#' @param phenomenon phenomenon
#' @param extent requested extent (lat/lon)
#' @return stations list
#' @export
#' 
x.osw.stations <- function(url,
                           networkCode, 
                           phenomenon, 
                           extent) {
  
  #set terms
  terms = ''
  if(!missing(networkCode)) terms <- paste0('{"terms": {"networkCode":["', networkCode,'"]}}')
  if(!missing(phenomenon)) terms <- paste0(terms, '{"terms": {"phenomenon":["', phenomenon,'"]}}')
  
  #build request
  request <- paste0('{',
    '"size" : 10000,',
    '"query" : { "bool" : { "must" : { "match_all" : {} },',
      '"filter" : [',
          '{ "bool": { "must" : [', terms, '] } },',
          '[{',
            '"geo_bounding_box":{',
              '"geometry" : {',
                '"top_left" : {"lon":', extent[1], ', "lat":', extent[4], '},',
                '"bottom_right" : {"lon":', extent[3], ', "lat":', extent[2], '}',
               '}',
            '}',
          '}]',
      ']',
    '}}}')
  
  #request stations
  response <- httr::POST(url, httr::accept("application/json"), httr::content_type("application/json"), body=request)
  content <- httr::content(response)
  hits <- content$hits$hits
  
  #parse response
  stations <- do.call(rbind, lapply(1:length(hits), function(x){
    source <- hits[[x]][["_source"]]
    sp::SpatialPointsDataFrame(coords = source$geometry,
                               data = data.frame(sensorCode = source$sensorCode, networkCode = source$networkCode, deviceCode = source$deviceCode, phenomenon = source$phenomenon, uom = source$uom, stringsAsFactors = F),
                               proj4string = sp::CRS("+init=epsg:4326"))
  }))
  
}
