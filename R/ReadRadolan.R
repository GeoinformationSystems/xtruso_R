#' Read RADOLAN binary file from specified folder accordimg to the official DWD file pattern
#'
#' @param radolan.root root path, where RADOLAN images are stored (folder or URL)
#' @param radolan.type RADOLAN product type
#' @param timestamp requested timestamp for the RADOLAN image
#' @param previous if a timestamp is not available, check previous timestamps according to the specified RADOLAN interval
#' @param previous.break number of previous timestamps to be checked, if previous = TRUE
#' @param rm.flagged remove flagged pixels from RADOLAN raster (set to NA)
#' @param fx.prediction prediction intervals for RADOLAN FX product (0:120, step=5)
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
    stop("Need to specify path to RADOLAN root folder or URL.")

  if(missing(radolan.type))
    stop("Need to specify a RADOLAN product type.")

  #get RADOLAN configuration
  configuration <- ReadRadolan.getConfiguration(radolan.type)

  if(radolan.type == "FX" && !all(fx.prediction %in% seq(0,120,5)))
    stop("RADOLAN FX prediction must be within seq(from=0, to=120, by=5).")

  #set timestamp
  if(timestamp == "latest")
    timestamp <- eval(parse(text = configuration$time.latest))

  #read file
  radolan.raster <- ReadRadolan.getRaster(radolan.root, timestamp, configuration, previous, previous.break, rm.flagged, fx.prediction)

  #return
  return(radolan.raster)

}


#'
#' Check, if RADOLAN product type is supported
#' @param radolan.type RADOLAN product type
#' @return true, if product tye is supported
ReadRadolan.isSupported <- function(radolan.type){

  if(missing(radolan.type))
    stop("Need to specify a RADOLAN product type.")

  return(radolan.type %in% names(xtruso::radolan.configuration))

}


#'
#' Get RADOLAN product configuration
#' @param radolan.type RADOLAN product type
#' @return true, if product tye is supported
ReadRadolan.getConfiguration <- function(radolan.type){

  if(!ReadRadolan.isSupported(radolan.type))
    stop(paste("RADOLAN type", radolan.type, "is not supported.", sep=" "))

  return(xtruso::radolan.configuration[[radolan.type]])

}


#' Get RADOLAN raster that matches the defined timestamp
#'
#' @param radolan.root root path, where RADOLAN images are stored
#' @param timestamp requested timestamp for the RADOLAN image
#' @param configuration RADOLAN configuration
#' @param previous if a timestamp is not available, check previous timestamps according to the respective RADOLAN interval
#' @param previous.break number of previous timestamps to be checked
#' @param rm.flagged remove flagged pixels from RADOLAN image
#' @param fx.prediction prediction intervals for RADOLAN FX product (0:120, step=5)
#' @return RADOLAN raster object
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
    radolan.path <- ReadRadolan.getPath(radolan.root, timestamp, configuration, fx.prediction[1])

    #try to read RADOLAN raster from root
    radolan.raster <- ReadRadolanBinary(radolan.path, configuration$type, rm.flagged)
    if(is.null(radolan.raster)){
      timestamp <- timestamp - configuration$time.interval
      previous.step <- previous.step + 1
    }
  }

  if(is.null(radolan.raster))
    stop("Could not read requested RADOLAN raster.")

  #set title
  radolan.raster@title <- as.character(timestamp)

  #return nonFX
  if(configuration$type != "FX"){
    return(radolan.raster)
  }

  #set FX title
  radolan.raster@title <- paste(radolan.raster@title, fx.prediction[1], sep=" - ")

  if(length(fx.prediction) == 1)
    return(radolan.raster)

  #init stack
  radolan.stack <- stack(radolan.raster)

  #read full stack
  for(i in fx.prediction[fx.prediction != fx.prediction[1]]){
    radolan.path <- ReadRadolan.getPath(radolan.root, timestamp, configuration, i)
    radolan.raster <- ReadRadolanBinary(radolan.path, configuration$type, rm.flagged)
    if(!(is.null(radolan.raster))){
      radolan.raster@title <- paste(as.character(timestamp), i, sep=" - ")
      radolan.stack <- addLayer(radolan.stack, radolan.raster)
    }
  }

  #return stack
  return(radolan.stack)

}


#' Get proper path to RADOLAN file based on the type and specified timestamp
#'
#' @param radolan.root root path, where RADOLAN images are stored
#' @param timestamp requested timestamp for the RADOLAN image
#' @param configuration RADOLAN configuration
#' @param fx.prediction prediction intervals for RADOLAN FX product (0:120, step=5)
#' @return proper RADOLAN path
ReadRadolan.getPath <- function(radolan.root,
                                timestamp,
                                configuration,
                                fx.prediction){

  #get file name
  file.name <- gsub("%%time%%", format(timestamp, configuration$time.format), configuration$file.pattern)

  #set prediction for FX product
  if(configuration$type == "FX"){
    if(fx.prediction < 10) {
      fx.prediction <- paste("00", fx.prediction, sep="")
    } else if(fx.prediction < 100) {
      fx.prediction <- paste("0", fx.prediction, sep="")
    }
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
#' @export
ReadRadolanBinary <- function(radolan.path,
                              radolan.type,
                              rm.flagged = TRUE) {

  if(missing(radolan.path))
    stop("Need to specify path to RADOLAN binary.")

  if(missing(radolan.type))
    stop("Need to specify type of RADOLAN product.")

  #get RADOLAN configuration
  configuration <- ReadRadolan.getConfiguration(radolan.type)

  download <- FALSE

  #download
  if(any(startsWith(radolan.path, c("http://", "https://", "ftp://")))){
    tryCatch({
      tmp.path <- paste(tempdir(), basename(radolan.path), sep="/")
      utils::download.file(radolan.path, tmp.path, mode="wb")
      radolan.path <- tmp.path
      download <- TRUE
    }, error = function(e){
      #do nothing
    })
  }

  if(!file.exists(radolan.path))
    return(NULL)

  #unzip
  if(any(base::endsWith(radolan.path, c(".bz2", ".gz")))) {
    if (requireNamespace("R.utils", quietly=TRUE)) {
      if(isGzipped(radolan.path))
        R.utils::gunzip(radolan.path, overwrite=TRUE)
      if(isBzipped(radolan.path))
        R.utils::bunzip2(radolan.path, overwrite=TRUE)
      radolan.path <- gsub(".bz2|.gz", "", radolan.path)
    }
    else
      stop("Need to install R.utils to decompress .bz2 or .gz.")
  }

  #read file
  radolan.raster <- ReadRadolanBinary.read(radolan.path, configuration, rm.flagged)

  #set timestamp and product type as attributes
  attr(radolan.raster, "timestamp") <- ReadRadolanBinary.getTimestamp(radolan.path, configuration)
  attr(radolan.raster, "type") <- radolan.type

  #delete downloaded file
  if(download)
    unlink(radolan.path)

  #return
  return(radolan.raster)

}


#' get timestamp from RADOLAN path
#'
#' @param radolan.path path to the RADOLAN input file
#' @param radolan.type RADOLAN type according to DWD classification (see radolan.configuration for supported types)
#' @return RADOLAN timestamp
ReadRadolanBinary.getTimestamp <- function(radolan.path, configuration) {

  #get filename from path
  file.name <- basename(radolan.path)

  #extract timestamp string
  timestamp.surroundings <- unlist(strsplit(configuration$file.pattern, "%%time%%"))
  timestamp <- gsub(paste(timestamp.surroundings[1], timestamp.surroundings[2], sep="|"), "", file.name)

  #get timestamp
  return(strptime(timestamp, configuration$time.format, "UTC"))

}

#' Parse RADOLAN binary file
#' adapted from http://moc.online.uni-marburg.de/doku.php?id=courses:bsc:project-thesis-geoei:lecture-notes:bdh:pt-ge-ln-bdh-01-9000
#'
#' @param radolan.path path to the RADOLAN binary input file
#' @param radolan.type RADOLAN type according to DWD classification (see radolan.configuration for supported types)
#' @param rm.flagged remove flagged pixels from RADOLAN image
#' @return RADOLAN raster object
ReadRadolanBinary.read <- function(radolan.path,
                                   configuration,
                                   rm.flagged = TRUE) {

  if(missing(radolan.path))
    stop("Need to specify path to RADOLAN binary.")

  if(missing(configuration))
    stop("Need to specify RADOLAN product configuration.")

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
  radolan.raster <- raster::flip(radolan.raster, "y")

  #set extent and projection
  raster::extent(radolan.raster) <- configuration$extent
  raster::projection(radolan.raster) <- configuration$proj

  #remove flagged values
  if(rm.flagged)
    radolan.raster[radolan.raster > configuration$max.value] <- NA

  #convert values, if precision != 1
  if(configuration$precision != 1)
    radolan.raster <- radolan.raster * configuration$precision

  #convert RVP6 values to dBZ, remove negative dBZ
  if(configuration$convert.to.dBZ) {
    radolan.raster <- radolan.raster / 2 - 32.5
    radolan.raster[radolan.raster < 0] <- 0
  }

  #return
  return(radolan.raster)

}
