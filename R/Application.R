#' Read single timeseries from RADOLAN
#'
#' @param radolan.type RADOLAN type
#' @param time.start start date (ISO8601 time string)
#' @param time.end end date (ISO8601 time string)
#' @param time.format time format
#' @param time.zone time zone
#' @param coord.x x coordinate
#' @param coord.y y coordinate
#' @export
Application.getTimeSeriesForCoord <- function(radolan.type,
                                              time.start,
                                              time.end,
                                              time.format = "%Y-%m-%d %H:%M:%S",
                                              time.zone = "UTC",
                                              coord.x,
                                              coord.y) {
  
  #get coord index
  coord.index <- Utility.getRADOLANIndexFromCoord(coord.x, coord.y)
  
  return(Application.getTimeSeriesForIndex(radolan.type, time.start, time.end, time.format, time.zone, coord.index))
  
}


#' Read single timeseries from RADOLAN
#'
#' @param radolan.type RADOLAN type
#' @param time.start start date (ISO8601 time string)
#' @param time.end end date (ISO8601 time string)
#' @param time.format time format
#' @param time.zone time zone
#' @param coord.index coordinate index (x, y)
Application.getTimeSeriesForIndex <- function(radolan.type,
                                              time.start,
                                              time.end,
                                              time.format = "%Y-%m-%d %H:%M:%S",
                                              time.zone = "UTC",
                                              coord.index) {
  
  #get start/end time index
  time.start <- as.POSIXct(time.start, format=time.format, tz=time.zone)
  time.end <- as.POSIXct(time.end, format=time.format, tz=time.zone)
  
  #get target year(s)
  years <- format(time.start, "%Y") : format(time.end, "%Y")
  
  #set NetCDF file(s)
  ncdf.files = list()
  for(year in years){
    ncdf.files[as.character(year)] <- paste0("/ncdf/radolan", radolan.type, year, ".nc")
  }
  
  #read timeseries from single file
  if(length(years) == 1){
    
    #open NetCDF file
    ncdf <- Radolan2Ncdf.openFile(ncdf.files[[as.character(year)]])
    
    #determine start and end indices
    timestamps <- ncdf$dim$t$vals
    start.index <- which(abs(timestamps - as.double(time.start)) == min(abs(timestamps - as.double(time.start))))
    end.index <- which(abs(timestamps - as.double(time.end)) == min(abs(timestamps - as.double(time.end)))) 
    
    #request subset
    subset <- Radolan2Ncdf.requestSubset(ncdf, start=c(min(coord.index$x), min(coord.index$y), timestamps[start.index]), count=c(length(coord.index$x), length(coord.index$y), end.index - start.index + 1))
    
    #close NetCDF file
    Radolan2Ncdf.closeFile(ncdf)
  }
  
  ### TODO implement subsetting across multiple NetCDF files ###
  else
    stop("selection across multiple years is not yet supported")
  
  
  #return final dataframe, calculates mean for each timestamp
  return(data.frame(t = c(timestamps[start.index:end.index]), v = apply(subset, 2, mean)))
  
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
