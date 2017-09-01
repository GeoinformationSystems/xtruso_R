#' Read RADOLAN binary file from specified folder accordimg to the official DWD file pattern
#'
#' @param radolan.root root path, where RADOLAN images are stored (folder or URL)
#' @param radolan.type RADOLAN image type
#' @param timestamp requested timestamp for the RADOLAN image
#' @param previous if a timestamp is not available, check previous timestamps according to the specified RADOLAN interval
#' @param previous.break number of previous timestamps to be checked, if previous = TRUE
#' @param rm.flagged remove flagged pixels from RADOLAN raster (set to NA)
#' @param fx.prediction prediction interval for RADOLAN FX product (0:120, step=5)
#' @return requested RADOLAN raster
#' @export
ReadRadolan <- function(radolan.root,
                        radolan.type,
                        timestamp = "latest",
                        previous = FALSE,
                        previous.break = 5,
                        rm.flagged = TRUE,
                        fx.prediction = 120) {

  if(missing(radolan.root))
    stop("Need to specify path to RADOLAN file root folder or URL.")

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
  radolan.raster <- ReadRadolan.getRaster(radolan.root, timestamp, configuration, previous, previous.break, rm.flagged, fx.prediction)

  #return
  return(radolan.raster)

}


#' Get RADOLAN raster that matches the defined timestamp
#'
#' @param radolan.root root path, where RADOLAN images are stored
#' @param timestamp requested timestamp for the RADOLAN image
#' @param configuration RADOLAN configuration
#' @param previous if a timestamp is not available, check previous timestamps according to the respective RADOLAN interval
#' @param previous.break number of previous timestamps to be checked
#' @param rm.flagged remove flagged pixels from RADOLAN image
#' @return list($timestamp = timestamp of the image, $raster = RADOLAN raster object)
ReadRadolan.getRaster <- function(radolan.root,
                                  timestamp,
                                  configuration,
                                  previous,
                                  previous.break,
                                  rm.flagged,
                                  fx.prediction) {

  radolan.raster <- NULL
  previous.step = 0

  while(is.null(radolan.raster) && previous.step <= previous.break){

    #get path for RADOLAN raster
    radolan.path <- ReadRadolan.getPath(radolan.root, timestamp, configuration, fx.prediction)

    #try to read RADOLAN raster from root
    radolan.raster <- ReadRadolan.parseBinary(radolan.path, configuration$type, rm.flagged)
    if(is.null(radolan.raster)){
      timestamp <- timestamp - configuration$time.interval
      previous.step <- previous.step + 1
    }
  }

  if(is.null(radolan.raster))
    stop("Could not read a requested RADOLAN raster.")

  #return raster
  radolan.raster@title <- as.character(timestamp)
  return(radolan.raster)

}


#' Get proper path to RADOLAN file based on the type and specified timestamp
#'
#' @param radolan.root root path, where RADOLAN images are stored
#' @param timestamp requested timestamp for the RADOLAN image
#' @param configuration RADOLAN configuration
#' @return proper RADOLAN URL
ReadRadolan.getPath <- function(radolan.root,
                                timestamp,
                                configuration,
                                fx.prediction){

  #get file name
  file.name <- gsub("%%time%%", format(timestamp, configuration$time.format), configuration$file.pattern)

  #set prediction for FX product
  if(configuration$type == "FX"){
    if(fx.prediction < 100)
      fx.prediction <- paste("0", fx.prediction, sep="")
    file.name <- gsub("%%prediction%%", fx.prediction, file.name)
  }

  #return url
  return(paste(radolan.root, file.name, sep="/"))

}


#' Try to read RADOLAN raster from path, returns NULL if file does not exist
#'
#' @param radolan.path path to RADOLAN image
#' @param radolan.type RADOLAN type
#' @param rm.flagged remove flagged pixels from RADOLAN image
#' @return RADOLAN raster
ReadRadolan.parseBinary <- function(radolan.path,
                                    radolan.type,
                                    rm.flagged) {

  download <- FALSE

  #download
  if(any(startsWith(radolan.path, c("http://", "https://", "ftp://")))){
    tryCatch({
      tmp.path <- paste(tempdir(), basename(radolan.path), sep="/")
      download.file(radolan.path, tmp.path, mode="wb")
      radolan.path <- tmp.path
      download <- TRUE
    }, error = function(e){
      return(NULL)
    })
  }

  else if(!file.exists(radolan.path))
    return(NULL)

  #read sf file
  radolan.raster <- ReadRadolanBinary(radolan.path, radolan.type, rm.flagged)

  #delete downloaded file
  if(download)
    unlink(radolan.path)

  #return
  return(radolan.raster)

}

#' Parse RADOLAN binary file
#'
#' @param file.path path to the RADOLAN binary input file
#' @param radolan.type RADOLAN type according to DWD classification (see radolan.configuration for supported types)
#' @param rm.flagged remove flagged pixels from RADOLAN image
#' @return RADOLAN raster object
#' @export
ReadRadolanBinary <- function(radolan.path,
                              radolan.type,
                              rm.flagged = TRUE) {

  if(missing(radolan.path))
    stop("Need to specify path to RADOLAN binary.")

  if(missing(radolan.type))
    stop("Need to specify type of RADOLAN product.")

  if(!(radolan.type %in% names(radolan.configuration)))
    stop(paste("RADOLAN type", type, "is not supported.", sep=" "))

  #get RADOLAN configuration
  configuration <- radolan.configuration[[radolan.type]]

  #define end of header position ("03")
  header.end <- regexpr("\003", readLines(radolan.path, 1, skipNul = TRUE, warn = FALSE))

  #open binary stream
  radolan.stream <- file(radolan.path, "rb")

  #read header
  tmp <- readBin(radolan.stream, "raw", n = header.end, endian = "little")

  #read integer data (note: includes flags described in RADOLAN spec)
  radolan.data <- readBin(radolan.stream, integer(), n = configuration$nrow * configuration$ncol, size = configuration$bits, endian = "little", signed=FALSE)

  #close connection
  close(radolan.stream)

  #create raster with projection
  radolan.raster <- raster::raster(t(matrix(radolan.data, ncol = configuration$ncol, nrow = configuration$nrow)))

  #flip raster direction
  radolan.raster <- flip(radolan.raster, "y")

  #set extent and projection
  extent(radolan.raster) <- configuration$extent
  projection(radolan.raster) <- configuration$proj

  #remove flagged values
  if(rm.flagged)
    radolan.raster[radolan.raster > configuration$max.value] <- NA

  #convert values, if precision != 1
  if(configuration$precision != 1)
    radolan.raster <- radolan.raster * configuration$precision

  #convert RVP6 values to dBZ, remove negative dBZ
  if(configuration$convert.to.dBZ) {
    radolan.raster <- radolan.raster / 2 - 32.5
    radolan.raster[radolan.raster < 0] <- NA
  }

  #return
  return(radolan.raster)

}
