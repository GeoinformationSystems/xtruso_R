#' Read COSMO DE grib file
#'
#' This function reads a COSMO DE file as provided by the DWD
#'
#' @param cosmo.root root path (folder or URL) to COSMO DE files (.grib2 or .bz2)
#' @param cosmo.parameter COSMO DE parameter
#' @param timestamp requested timestamp for the COSMO DE model output
#' @param previous if a timestamp is not available, check previous timestamps according to the interval of COSMO DE model runs
#' @param previous.break number of previous timestamps to be checked, if previous = TRUE
#' @param prediction single or vector of COSMO DE prediction intervals, in hours (0 - 45)
#' @return COSMO DE raster
#' @export
x.cosmode.get <- function(cosmo.root,
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

  if(!all(prediction %in% 0:45))
    stop("COSMO DE prediction must be within interval 0:45.")

  #get COSMO DE configuration
  configuration <- cosmo.configuration[[cosmo.parameter]]

  #set timestamp
  if(timestamp == "latest")
    timestamp <- eval(parse(text = configuration$time.latest))

  if(timestamp$hour != 3 && any(prediction %in% 28:45))
    stop("COSMO DE prediction for hour != 3 must be within interval 0:27.")

  cosmo.raster <- NULL
  previous.step = 0
  
  while(is.null(cosmo.raster) && previous.step <= previous.break){
    
    #get path for COSMO DE raster
    cosmo.path <- x.cosmode.path(cosmo.root, timestamp, configuration, prediction[1])
    
    #try to read COSMO DE raster from root
    cosmo.raster <- ReadCosmoDEGrib(cosmo.path)
    if(is.null(cosmo.raster)){
      timestamp <- as.POSIXlt(timestamp - configuration$time.interval)
      previous.step <- previous.step + 1
    }
  }
  
  if(is.null(cosmo.raster))
    stop("Could not read a requested COSMO DE raster.")
  
  #return single raster, if length(prediction) == 1
  if(length(prediction) == 1){
    cosmo.raster@title <- paste(configuration$parameter, timestamp, prediction, sep=" - ")
    return(cosmo.raster)
  }
  
  #init stack
  cosmo.raster@title <- paste(configuration$parameter, timestamp, prediction[1], sep=" - ")
  cosmo.stack <- stack(cosmo.raster)
  
  #read full stack
  for(i in prediction[prediction != prediction[1]]){
    cosmo.path <- x.cosmode.path(cosmo.root, timestamp, configuration, i)
    cosmo.raster <- ReadCosmoDEGrib(cosmo.path)
    if(!(is.null(cosmo.raster))){
      cosmo.raster@title <- paste(configuration$parameter, timestamp, i, sep=" - ")
      cosmo.stack <- addLayer(cosmo.stack, cosmo.raster)
    }
  }
  
  #return stack
  return(cosmo.stack)

}


#' Get proper path to COSMO DE file based on the type and specified timestamp
#'
#' @param cosmo.root root path, where COSMO DE images are stored
#' @param timestamp requested timestamp for the COSMO DE image
#' @param configuration COSMO DE configuration
#' @return proper COSMO DE path
x.cosmode.path <- function(cosmo.root,
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
x.cosmode.read <- function(cosmo.path){

  download <- FALSE

  #download
  if(any(startsWith(cosmo.path, c("http://", "https://", "ftp://")))){
    tryCatch({
      tmp.path <- paste(tempdir(), basename(cosmo.path), sep="/")
      utils::download.file(cosmo.path, tmp.path, mode="wb")
      cosmo.path <- tmp.path
      download <- TRUE
    }, error = function(e){
      #do nothing
    })
  }

  if(!(file.exists(cosmo.path)))
    return(NULL)

  #unzip
  if(any(base::endsWith(cosmo.path, c(".bz2", ".gz")))) {
    if (requireNamespace("R.utils", quietly=TRUE)) {
      if(isGzipped(cosmo.path))
        R.utils::gunzip(cosmo.path, overwrite=TRUE)
      if(isBzipped(cosmo.path))
        R.utils::bunzip2(cosmo.path, overwrite=TRUE)
      cosmo.path <- gsub(".bz2|.gz", "", cosmo.path)
    }
    else
      stop("Need to install R.utils to decompress .bz2 or .gz.")
  }

  #read file
  cosmo.raster <- raster::raster(cosmo.path)

  if(download){
    #read to memory
    cosmo.raster <- raster::readAll(cosmo.raster)
    #delete file
    unlink(cosmo.path)
  }

  return(cosmo.raster)

}
