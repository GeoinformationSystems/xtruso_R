#' Read RADOLAN SF dataset
#'
#' This function reads a RADOLAN SF (24h avg in mm/h) binary raster as provided by the DWD
#'
#' @param path path to the RADOLAN SF binary input file, is set, the timestamp is ignored
#' @param timestamp used, if path is not set; must be of type POSIXct or keyword "latest", which will return the latest available timestamp
#' @return list($timestamp = timestamp of the image, $raster = RADOLAN SF raster object)
#' @export
ReadRadolanFromUrl <- function(url.root,
                               radolan.type,
                               timestamp = "latest",
                               previous = FALSE,
                               previous.break = 5,
                               rm.flagged = TRUE) {

  if(missing(url.root))
    stop("Need to specify path to RADOLAN root Url.")

  if(missing(radolan.type))
    stop("Need to specify type of RADOLAN product.")

  if(!(radolan.type %in% names(radolan.configuration)))
    stop(paste("RADOLAN type", type, "is not supported.", sep=" "))

  #get RADOLAN configuration
  configuration <- radolan.configuration[[radolan.type]]

  #set timestamp
  if(timestamp == "latest")
    timestamp <- eval(parse(text = configuration$time.latest))

  #read file
  radolan.tuple <- ReadRadolanFromUrl.getTuple(url.root, timestamp, configuration, previous, previous.break, rm.flagged)

  #return timestamp and raster
  return(radolan.tuple)

}


#read RADOLAN raster and return final timestamp and raster
ReadRadolanFromUrl.getTuple <- function(url.root, timestamp, configuration, previous, previous.break, rm.flagged) {

  radolan.raster <- NULL
  previous.step = 0

  while(is.null(radolan.raster) && previous.step <= previous.break){

    #get URL for RADOLAN raster
    radolan.url <- ReadRadolanFromUrl.getURL(url.root, timestamp, configuration)

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


#get Url of RADOLAN file based on timestamp
ReadRadolanFromUrl.getURL <- function(url.root, timestamp, configuration){

  #get file name
  file.name <- gsub("%%time%%", format(timestamp, configuration$time.format), configuration$file.pattern)

  #return url
  return(paste(url.root, file.name, sep="/"))

}


# Read RADOLAN raster from url
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


