#' Retrieve timeseries data from OpenSensorWeb
#'
#' @param s.id station identifier
#' @param t.start start date (as.POSIXct)
#' @param t.end end date (as.POSIXct)
#' @return dataframe with measurements
#' @export
x.osw.get <- function(osw.configuration,
                      s.id, 
                      t.start = NA, 
                      t.end = NA){
  
  #build request
  request <- x.osw.request(osw.configuration, s.id, t.start, t.end)
    
  #request service
  response <- httr::GET(request, httr::accept("text/csv"))
  
  if(httr::http_status(response)$category != "Success")
    stop(paste0("HTTR returned: ", httr::http_status(response)$message))
  
  #parse CSV
  df.measurements <- httr::content(response, type="text/csv", encoding="UTF-8", col_types=readr::cols(begin = readr::col_datetime(format = "%Y-%m-%dT%H:%MZ"), v = readr::col_double()))
    
  if(nrow(df.measurements) == 0)
    warning(paste0("No measurements returned for ", s.id))
  
  return(df.measurements)
  
}


#' Build OpenSensorWeb measurement request
#'
#' @param osw.configuration OSW configuration
#' @param deviceCode OSW device code
#' @param t.start start date (as.POSIXct)
#' @param t.end end date (as.POSIXct)
#' @return dataframe with measurements
#' 
x.osw.request <- function(osw.configuration,
                          deviceCode,
                          t.start = NA, 
                          t.end = NA){
  
  #build OSW request
  request <- paste0(osw.configuration$url,
                "/networks/", osw.configuration$network,
                "/devices/", deviceCode,
                "/sensors/", gsub("%%deviceCode%%", deviceCode, osw.configuration$sensorCode),
                "/measurements/raw")
  
  if(!is.na(t.start) && !is.na(t.end))
    request <- paste0(request, 
                      "?start=", strftime(t.start , "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
                      "&end=", strftime(t.end , "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
  
  return(request)
  
}

