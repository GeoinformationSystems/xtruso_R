#' request Octave API
#'
#' @param octave.url Octave API endpoint
#' @param octave.process proces id
#' @param params named list of proess parameters
#' @export
#' 
x.octave.execute <- function(octave.url, 
                             octave.process,
                             octave.params) {
  
  if(missing(octave.url) || missing(octave.process))
    stop("Octave URL and process id must be specified.")
    
  if(typeof(octave.params) != "list")
    stop("Process parameters must be provided as named list")
  
  #build url
  url <- paste0(octave.url, "/", octave.process)
  
  #POST
  response <- httr::POST(url, body=octave.params)
  
  #parse response
  content <- httr::content(response)
  return(content)
  
}


#' request Octave flood forecast based on Neuronal Network
#'
#' @param octave.url Octave API endpoint
#' @param octave.process proces id
#' @param gauge requested station id
#' @param leadtime forecasted timeframe (in hours)
#' @param len.discharge length of discharge measurements (in hours)
#' @param len.precipitation length of precipitation measurements (in days)
#' @export
#'
x.octave.flood_nn <- function(octave.url = "http://172.22.1.142/octave",
                              octave.process = "flood_nn",
                              ncdf.folder = "/ncdf",
                              gauge,
                              leadtime = 6,
                              len.discharge = 6,
                              len.precipitation = 14,
                              tz = "UTC") {
  
  #init list
  octave.params <- list()
  octave.params[["gauge"]] <- gauge
  octave.params[["leadtime"]] <- leadtime
  
  #get current hour (fcpoint)
  fcpoint <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:00:00"))
  octave.params[["fcpoint"]] <- as.character(fcpoint, tz=tz)
  
  #get discharge
  osw.discharge <- x.osw.get(xtruso::osw.configuration$HWIMS_DC_15min, gauge, t.start = fcpoint - (3600 * len.discharge - 1) , t.end=fcpoint)
  
  #calculate hourly discharge mean
  osw.discharge$begin <- apply(osw.discharge[, "begin"], 1, function(t){
    h <- trunc.POSIXt(t, "hours")
    if(h == t) return(t) else return(format(h + 3600, "%Y-%m-%d %H:%M:%S"))
  })
  osw.discharge <- aggregate(. ~ begin, osw.discharge, mean)
  
  #set discharge param
  octave.params[["Q"]] <- paste0(osw.discharge$v, collapse=",")
  octave.params[["Qdate"]] <- paste0(as.character(osw.discharge$begin, tz=tz), collapse=",")
  
  #get catchment corresponding to station
  c.id <- x.app.catchment.id(station.id = gauge)
  
  #get recent precipitation
  ts.radolan <- x.app.catchment.radolan(ncdf.folder=ncdf.folder, c.id=c.id, radolan.type="RW", t.start = fcpoint - (86400 * len.precipitation), t.end=fcpoint)
  ts.radolan$timestamp <- as.numeric(levels(ts.radolan$timestamp))
  
  #add 10min to RADOLAN timestamp to support forecasting
  #TODO interpolate
  ts.radolan$timestamp <- ts.radolan$timestamp + 600
  
  #get forecasted precipitation
  ts.cosmo <- x.app.catchment.cosmo(ncdf.folder=ncdf.folder, c.id=c.id)
  ts.cosmo$timestamp <- as.numeric(levels(ts.cosmo$timestamp)) + (as.numeric(levels(ts.cosmo$forecast)) * 3600)
  ts.cosmo <- ts.cosmo[-which(ts.cosmo$timestamp %in% ts.radolan$timestamp), ]
  
  #combine precipitation table
  ts.prec <- rbind(ts.radolan[, c("timestamp", "mean")], ts.cosmo[, c("timestamp", "mean")])
  ts.prec$timestamp <- as.POSIXct(ts.prec$timestamp, origin="1970-01-01")
  
  octave.params[["P"]] <- paste0(ts.prec$mean, collapse=",")
  octave.params[["Pdate"]] <- paste0(as.character(ts.prec$timestamp, tz=tz), collapse=",")
  
  #execute
  forecast <- x.octave.execute(octave.url, octave.process, octave.params)
  
  #assemble dataframe
  output <- unlist(strsplit(gsub("\n", ",", forecast$output), ","))
  df.output <- data.frame(timestamp=as.POSIXct(output[seq(leadtime+1, leadtime*2)], tz=tz), value=as.numeric(output[seq(1, leadtime)]))
  return(df.output[!is.na(df.output$value), ])
  
}
