#' Read RADOLAN SF dataset
#'
#' This function reads a RADOLAN SF (24h avg in mm/h) binary raster as provided by the DWD
#'
#' @param path path to the RADOLAN SF binary input file, is set, the timestamp is ignored
#' @param timestamp used, if path is not set; must be of type POSIXct or keyword "latest", which will return the latest available timestamp
#' @return list($timestamp = timestamp of the image, $raster = RADOLAN SF raster object)
#' @export
radolan_readBinarySF <- function(path = NULL,
                                 timestamp="latest",
                                 baseUrl="https://opendata.dwd.de/weather/radar/radolan/sf/",
                                 nrows=900,
                                 ncols=900,
                                 extent=extent_radolan) {

  #get RADOLAN SF from path
  if(!is.null(path))
    sf <- list("timestamp"=timestamp, "raster"=radolan_readBinary(path, nrows, ncols, extent))
  #get RADOLAN SF based on timestamp
  else if(timestamp == "latest")
    sf <- f_readSFRaster_latest(baseUrl, nrows, ncols, extent)
  else
    sf <- list("timestamp"=timestamp, "raster"=f_readSFRaster(baseUrl, timestamp, nrows, ncols, extent))


  #remove flagged data by setting them to NA
  sf$raster[sf$raster > 4095] <- NA
  #divide by 10, to get mm/24h
  sf$raster <- sf$raster / 10
  #return timestamp and raster
  return(sf)

}

#read RADOLAN SF raster from destination url, use lastest timestamp
f_readSFRaster_latest <- function(baseUrl, nrows, ncols, extent) {

  #get current time
  now <- as.POSIXlt(Sys.time(), "UTC")

  #get latest timestamp with %H = 50 (default for RADOLAN SF data)
  latest <- strptime(paste(format(now, "%y%m%d%H"), "50", sep=""), "%y%m%d%H%M", "UTC")

  #subtract one hour, if latest is future timestamp
  if(latest > now)
    latest <- latest - 3600

  #try to fetch RADOLAN data from DWD
  radolan_sf <- NULL
  while(is.null(radolan_sf)){
    radolan_sf <- f_readSFRaster(baseUrl, latest, nrows, ncols, extent)
    if(is.null(radolan_sf))
      latest <- latest - 3600
  }

  #return timestamp and raster
  return(list("timestamp" = latest, "raster" = radolan_sf))

}

#read RADOLAN SF raster from destination url and timestamp
f_readSFRaster <- function(baseUrl, timestamp, nrows, ncols, extent) {

  #get path
  sfTime <- format(timestamp, "%y%m%d%H%M")
  url <- paste(baseUrl, "raa01-sf_10000-", sfTime, "-dwd---bin", sep="")

  #check if url exists
  if(http_error(url))
    return(NULL)

  #download radolan sf file
  tmpFile <- paste("sf_", sfTime, ".temp", sep="")
  download.file(url, tmpFile, mode="wb")

  #read sf file
  raster <- radolan_readBinary(tmpFile, nrows, ncols, extent)

  #delete temp file
  unlink(tmpFile)
  return(raster)

}
