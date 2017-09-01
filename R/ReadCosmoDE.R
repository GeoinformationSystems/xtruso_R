#' Read COSMO DE grib file
#'
#' This function reads a COSMA DE file as provided by the DWD
#'
#' @param cosmo.root root path (folder or URL) to COSMO DE files (.grib2 or .bz2)
#' @param cosmo.parameter COSMO DE parameter
#' @param timestamp requested timestamp for the COSMO DE model output
#' @param previous if a timestamp is not available, check previous timestamps according to the interval of COSMO DE model runs
#' @param previous.break number of previous timestamps to be checked, if previous = TRUE
#' @param prediction COSMO DE prediction in hours (0 - 27)
#' @return COSMO DE raster
#' @export
ReadCosmoDE <- function(cosmo.root,
                        cosmo.parameter,
                        timestamp = "latest",
                        previous = FALSE,
                        previous.break = 5,
                        prediction = 27) {

  if(missing(cosmo.root))
    stop("Need to specify path to COSMO DE root folder or URL.")

  if(missing(cosmo.parameter))
    stop("Need to specify COSMO DE parameter.")

  if(!(cosmo.parameter %in% names(cosmo.configuration)))
    stop(paste("COSMO DE parameter", cosmo.parameter, "is not supported.", sep=" "))

  if(!(prediction %in% 0:27))
    stop("COSMO DE prediction must be within interval c(0:27).")

  #get COSMO DE configuration
  configuration <- cosmo.configuration[[cosmo.parameter]]

  #set timestamp
  if(timestamp == "latest")
    timestamp <- eval(parse(text = configuration$time.latest))

  #read file
  cosmo.raster <- ReadCosmoDE.getRaster(cosmo.root, timestamp, configuration, previous, previous.break, prediction)

  #return
  return(cosmo.raster)

}

#' Get COSMO DE raster that matches the defined timestamp
#'
#' @param radolan.root root path, where COSMO DE images are stored
#' @param timestamp requested timestamp for the COSMO DE raster
#' @param configuration COSMO DE configuration
#' @param previous if a timestamp is not available, check previous timestamps according to the respective COSMO DE interval
#' @param previous.break number of previous timestamps to be checked
#' @param prediction COSMO DE prediction in hours (0 - 27)
#' @return list($timestamp = timestamp of the image, $raster = RADOLAN raster object)
ReadCosmoDE.getRaster <- function(cosmo.root,
                                 timestamp,
                                 configuration,
                                 previous,
                                 previous.break,
                                 prediction) {

  cosmo.raster <- NULL
  previous.step = 0

  while(is.null(cosmo.raster) && previous.step <= previous.break){

    #get path for COSMO DE raster
    cosmo.path <- ReadCosmoDE.getPath(cosmo.root, timestamp, configuration, prediction)

    #try to read COSMO DE raster from root
    cosmo.raster <- ReadCosmoDEGrib(cosmo.path)
    if(is.null(cosmo.raster)){
      timestamp <- timestamp - configuration$time.interval
      previous.step <- previous.step + 1
    }
  }

  if(is.null(cosmo.raster))
    stop("Could not read a requested COSMO DE raster.")

  #return raster
  cosmo.raster@title <- paste(configuration$parameter, timestamp, prediction, sep=" - ")
  return(cosmo.raster)

}


#' Get proper path to COSMO DE file based on the type and specified timestamp
#'
#' @param cosmo.root root path, where COSMO DE images are stored
#' @param timestamp requested timestamp for the COSMO DE image
#' @param configuration COSMO DE configuration
#' @return proper COSMO DE path
ReadCosmoDE.getPath <- function(cosmo.root,
                                timestamp,
                                configuration,
                                prediction){

  #get file name
  file.name <- gsub("%%time%%", format(timestamp, configuration$time.format), configuration$file.pattern)

  #set prediction
  if(prediction < 10)
    prediction <- paste("00", prediction, sep="")
  else
    prediction <- paste("0", prediction, sep="")
  file.name <- gsub("%%prediction%%", prediction, file.name)

  #return path
  return(paste(cosmo.root, file.name, sep="/"))

}


#' Try to read COSMO DE raster from path, returns NULL if file does not exist
#'
#' @param cosmo.path path to COSMO DE file
#' @return COSMO DE raster
#' @export
ReadCosmoDEGrib <- function(cosmo.path){

  download <- FALSE

  #download
  if(any(startsWith(cosmo.path, c("http://", "https://", "ftp://")))){
    tryCatch({
      tmp.path <- paste(tempdir(), basename(cosmo.path), sep="/")
      download.file(cosmo.path, tmp.path, mode="wb")
      cosmo.path <- tmp.path
      download <- TRUE
    }, error = function(e){
      #do nothing
    })
  }

  if(!(file.exists(cosmo.path)))
    return(NULL)

  #unzip
  if(endsWith(cosmo.path, ".bz2")) {
    if (requireNamespace("R.utils", quietly=TRUE)){
      bunzip2(cosmo.path, overwrite=TRUE)
      cosmo.path <- gsub(".bz2", "", cosmo.path)
    }
    else
      stop("Need to install R.utils in order to read from .bz2.")
  }

  #read file
  cosmo.raster <- raster::raster(cosmo.path)
  cosmo.raster <- raster::readAll(cosmo.raster)

  #delete downloaded file
  if(download)
    unlink(cosmo.path)

  #return
  return(cosmo.raster)

}
