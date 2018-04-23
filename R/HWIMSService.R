#' Create graph from input dataframe using a specified connection field
#'
#' @param hwims.configuration HWIMS WSDL configuration
#' @param station station identifier
#' @param t.start start date
#' @param t.end end date
#' @param hwims.authentication HWIMS BASIC HTML authentication (list("user" = ..., "pass" = ...))
#' @return dataframe with measurements
#' @export
x.hwims.get <- function(hwims.configuration, 
                        station, 
                        t.start, 
                        t.end, 
                        hwims.authentication){
  
  #subset, if interval is too large
  if(difftime(t.end, t.start, units=hwims.configuration[["interval.uom"]]) > hwims.configuration[["interval.max"]]) {
    df.time = x.utility.time.intervals(t.start, t.end, hwims.configuration[["interval.max"]], hwims.configuration[["interval.uom"]])
  } else {
    df.time <- data.frame(t.start=t.start, t.end=t.end)
  }
  
  #create measurement dataframe
  df.measurements <- data.frame()
  
  for(i in 1:nrow(df.time)){
  
    request <- x.hwims.envelope(hwims.configuration, station, df.time[i, "t.start"], df.time[i, "t.end"])
    
    #request service
    response <- httr::POST(url=hwims.configuration[["url"]], body = request, httr::authenticate(user=hwims.authentication$user, password=hwims.authentication$pass))
    
    #parse XML content
    content <- httr::content(response)
    
    #parse response
    parent <- xml2::xml_find_first(content, ".//spur")
    measurements <- xml2::xml_children(parent)
    
    #populate measurement dataframe
    df.measurements <- rbind(df.measurements, do.call("rbind", lapply(measurements, function(row){
      
      if(xml2::xml_name(row) == "wert") {
        
        #get timestamp and ignore measurement, if already written
        timestamp <- as.POSIXct(xml2::xml_text(xml2::xml_find_first(row, "zeitstempel")), format="%Y-%m-%dT%H:%M:%S")
        if(!timestamp %in% df.measurements$timestamp){
          value <- as.numeric(xml2::xml_text(xml2::xml_find_first(row, "wert")))
          status <- xml2::xml_text(xml2::xml_find_first(row, "status"))
          return(data.frame(time=timestamp, timestamp=as.double(timestamp), value=value, status=status))
        }
        
      }
      
    })))
    
    if(nrow(df.measurements) == 0)
      warning(paste("no measurements returned for", station, df.time$t.start[i], df.time$t.end[i], sep=" : "))
    
  }
  
  return(df.measurements)
  
}


#' Build HWIMS service request
#'
#' @param hwims.configuration HWIMS WSDL configuration
#' @param station station identifier
#' @param t.start start date
#' @param t.end end date
#' @return requestdataframe with measurements
x.hwims.envelope <- function(hwims.configuration, 
                            station, 
                            t.start, 
                            t.end){
  
  #build SOAP request envelope
  return(paste0("<x:Envelope xmlns:x=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:spu=\"http://spurwerte.webservice.hwims.t_systems_mms.com/\">
                    <x:Header/>
                    <x:Body>
                    <spu:liefereWerteZuSpur2>
                    <spurIdentifikator>
                    <messstationKennziffer>", station, "</messstationKennziffer>
                    <messstationTyp>", hwims.configuration[["type"]], "</messstationTyp>
                    <physikalischeGroesse>", hwims.configuration[["var"]], "</physikalischeGroesse>
                    <spurTyp>", hwims.configuration[["spur"]], "</spurTyp>
                    </spurIdentifikator>
                    <startZeitpunkt>", strftime(t.start , "%Y-%m-%dT%H:%M:%S", tz="CET"), "</startZeitpunkt>
                    <endeZeitpunkt>", strftime(t.end , "%Y-%m-%dT%H:%M:%S", tz="CET"), "</endeZeitpunkt>
                    <statistischeZeitangaben>", hwims.configuration[["stat"]], "</statistischeZeitangaben>
                    </spu:liefereWerteZuSpur2>
                    </x:Body>
                    </x:Envelope>"))
  
}

