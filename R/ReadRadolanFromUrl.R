#' Read RADOLAN binary image
#'
#' This function reads a RADOLAN binary image as provided by the DWD
#'
#' @param url.root root path, where RADOLAN images are stored
#' @param radolan.type RADOLAN image type
#' @param timestamp requested timestamp for the RADOLAN image
#' @param previous if a timestamp is not available, check previous timestamps according to the respective RADOLAN interval
#' @param previous.break number of previous timestamps to be checked
#' @param rm.flagged remove flagged pixels from RADOLAN image
#' @return list($timestamp = timestamp of the image, $raster = RADOLAN raster object)
#' @export
ReadRadolanFromUrl <- function(url.root,
                               radolan.type,
                               timestamp = "latest",
                               previous = FALSE,
                               previous.break = 5,
                               rm.flagged = TRUE,
                               fx.prediction = 120) {

  if(missing(url.root))
    stop("Need to specify path to RADOLAN root Url.")

  if(missing(radolan.type))
    stop("Need to specify type of RADOLAN product.")

  if(!(radolan.type %in% names(radolan.configuration)))
    stop(paste("RADOLAN type", type, "is not supported.", sep=" "))

  if(radolan.type == "FX" && !(fx.prediction %in% seq(0,120,5)))
    stop("RADOLAN FX prediction must be within seq(from=0, to=120, by=5).")

  #get RADOLAN configuration
  configuration <- radolan.configuration[[radolan.type]]

  #set timestamp
  if(timestamp == "latest")
    timestamp <- eval(parse(text = configuration$time.latest))

  #read file
  radolan.tuple <- ReadRadolanFromUrl.getTuple(url.root, timestamp, configuration, previous, previous.break, rm.flagged, fx.prediction)

  #return timestamp and raster
  return(radolan.tuple)

}


#' Read RADOLAN binary image
#'
#' This function reads a RADOLAN binary image based on a specified RADOLAN configuration
#'
#' @param url.root root path, where RADOLAN images are stored
#' @param timestamp requested timestamp for the RADOLAN image
#' @param configuration RADOLAN configuration
#' @param previous if a timestamp is not available, check previous timestamps according to the respective RADOLAN interval
#' @param previous.break number of previous timestamps to be checked
#' @param rm.flagged remove flagged pixels from RADOLAN image
#' @return list($timestamp = timestamp of the image, $raster = RADOLAN raster object)
ReadRadolanFromUrl.getTuple <- function(url.root,
                                        timestamp,
                                        configuration,
                                        previous,
                                        previous.break,
                                        rm.flagged,
                                        fx.prediction) {

  radolan.raster <- NULL
  previous.step = 0

  while(is.null(radolan.raster) && previous.step <= previous.break){

    #get URL for RADOLAN raster
    radolan.url <- ReadRadolanFromUrl.getURL(url.root, timestamp, configuration, fx.prediction)

    #try to read RADOLAN raster from Url
    radolan.raster <- ReadRadolanFromUrl.getRaster(radolan.url, configuration$type, rm.flagged)
    if(is.null(radolan.raster)){
      timestamp <- timestamp - configuration$time.interval
      previous.step <- previous.step + 1
    }
  }

  #return timestamp and raster
  return(list("timestamp" = timestamp, "raster" = radolan.raster))

}


#' Get Url of RADOLAN file based on a specified timestamp
#'
#' This function determines a proper URL to access a RADOLAN image with a specified timestamp
#'
#' @param url.root root path, where RADOLAN images are stored
#' @param timestamp requested timestamp for the RADOLAN image
#' @param configuration RADOLAN configuration
#' @return proper RADOLAN URL
ReadRadolanFromUrl.getURL <- function(url.root, timestamp, configuration, fx.prediction){

  #get file name
  file.name <- gsub("%%time%%", format(timestamp, configuration$time.format), configuration$file.pattern)

  #set prediction for FX product
  if(configuration$type == "FX"){
    if(fx.prediction < 100)
      fx.prediction <- paste("0", fx.prediction, sep="")
    file.name <- gsub("%%prediction%%", fx.prediction, file.name)
  }

  #return url
  return(paste(url.root, file.name, sep="/"))

}


#' Read RADOLAN image from URL
#'
#' This function reads a RADOLNa binary file from URL
#'
#' @param radolan.url URL of RADOLAN image
#' @param radolan.type RADOLAN image type
#' @param rm.flagged remove flagged pixels from RADOLAN image
#' @return RADOLAN raster
ReadRadolanFromUrl.getRaster <- function(radolan.url, radolan.type, rm.flagged) {

  #check if url exists
  if(httr::http_error(radolan.url))
    return(NULL)

  #download radolan file
  tmp.path <- paste("radolan_", sample(1:10000, 1), ".temp", sep="")
  download.file(radolan.url, tmp.path, mode="wb")

  #read sf file
  radolan.raster <- ReadRadolanBinary(tmp.path, radolan.type, rm.flagged)

  #delete temp file
  unlink(tmp.path)

  #return
  return(radolan.raster)

}


