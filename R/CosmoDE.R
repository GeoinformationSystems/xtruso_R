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
                          prediction = 27) {

  if(missing(cosmo.root))
    stop("Need to specify path to COSMO DE root folder or URL.")

  if(missing(cosmo.parameter))
    stop("Need to specify COSMO DE parameter.")

  if(!(cosmo.parameter %in% names(xtruso::cosmo.configuration)))
    stop(paste("COSMO DE parameter", cosmo.parameter, "is not supported.", sep=" "))

  if(!all(prediction %in% 0:45))
    stop("COSMO DE prediction must be within interval 0:45.")

  #get COSMO DE configuration
  configuration <- xtruso::cosmo.configuration[[cosmo.parameter]]

  #set run
  if(!"POSIXt" %in% class(timestamp) && timestamp == "latest")
    timestamp <- eval(parse(text = configuration$time.latest))

  if(format(timestamp, "%H") != "03" && any(prediction %in% 28:45))
    stop("COSMO DE prediction for run != 03 must be within interval 0:27.")
    
  #get path for COSMO DE raster
  cosmo.path <- x.cosmode.path(cosmo.root, timestamp, configuration, max(prediction))
  
  #try to read COSMO DE raster from root
  cosmo.raster <- x.cosmode.read(cosmo.path)
  
  if(is.null(cosmo.raster))
    stop("Requested COSMO DE raster is not available.")
  
  #return single raster, if length(prediction) == 1
  if(length(prediction) == 1){
    cosmo.raster@title <- paste(configuration$parameter, timestamp, prediction, sep=" - ")
    return(cosmo.raster)
  }
  
  #init stack
  cosmo.stack <- raster::stack()
  
  #read full stack
  for(i in prediction[prediction != max(prediction)]){
    cosmo.path <- x.cosmode.path(cosmo.root, timestamp, configuration, i)
    raster <- x.cosmode.read(cosmo.path)
    if(!(is.null(raster))){
      raster@title <- paste(configuration$parameter, timestamp, i, sep=" - ")
      cosmo.stack <- addLayer(cosmo.stack, raster)
    }
  }
  
  #add max forecast
  cosmo.raster@title <- paste(configuration$parameter, timestamp, max(prediction), sep=" - ")
  cosmo.stack <- addLayer(cosmo.stack, cosmo.raster)
  
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
                           prediction) {

  #get file name
  file.name <- gsub("%%time%%", format(timestamp, configuration$time.format), configuration$file.pattern)

  #set prediction
  if(prediction < 10) {
    prediction <- paste("00", prediction, sep="")
  } else prediction <- paste("0", prediction, sep="")
  file.name <- gsub("%%prediction%%", prediction, file.name)

  #return path
  return(paste(cosmo.root, format(timestamp, "%H"), configuration$parameter, file.name, sep="/"))

}


#' Try to read COSMO DE raster from path, returns NULL if file does not exist
#'
#' @param cosmo.path path to COSMO DE file
#' @return COSMO DE raster
#' @export
x.cosmode.read <- function(cosmo.path) {

  download <- FALSE
  tmp.dir <- tempdir()
  cosmo.file <- NULL

  #download
  if(any(startsWith(cosmo.path, c("http://", "https://", "ftp://")))){
    tryCatch({
      tmp.path <- paste(tmp.dir, basename(cosmo.path), sep="/")
      utils::download.file(cosmo.path, tmp.path, mode="wb")
      cosmo.file <- tmp.path
      download <- TRUE
    }, error = function(e){
      #do nothing
    })
  }

  if(is.null(cosmo.file) || !(file.exists(cosmo.file)))
    return(NULL)
  
  #unzip
  if(any(base::endsWith(cosmo.file, c(".bz2", ".gz")))) {
    if (requireNamespace("R.utils", quietly=TRUE)) {
      if(R.utils::isGzipped(cosmo.file))
        R.utils::gunzip(cosmo.file, overwrite=TRUE)
      if(R.utils::isBzipped(cosmo.file))
        R.utils::bunzip2(cosmo.file, overwrite=TRUE)
      cosmo.file <- gsub(".bz2|.gz", "", cosmo.file)
    }
    else
      stop("Need to install R.utils to decompress .bz2 or .gz.")
  }
  
  #read file
  cosmo.raster <- raster::raster(cosmo.file)
  
  if(download){
    #read to memory
    cosmo.raster <- raster::readAll(cosmo.raster)
    #delete file
    unlink(cosmo.file)
  }

  return(cosmo.raster)

}


#'
#' create NetCDF file for COSMO images
#' @param ncdf.file NetCDF file path
#' @param cosmo.configuration configuration for COSMO files
#' @param chunksizes target chunksizes
#' @param compression file compression
#' @param year creates limited time dimension for the specified year
#' @return NetCDF file pointer
#' @export
x.cosmode.ncdf.create <- function(ncdf.file,
                                  cosmo.configuration,
                                  chunksizes = NA,
                                  compression = NA,
                                  year = NA) {
  
  if(missing(ncdf.file))
    stop("Need to specify a NetCDF file.")
  
  if(missing(cosmo.configuration))
    stop("Need to specify a COSMO configuration.")
  
  #create x dimension based on COSMO extent (subtract .5 to get center of cell)
  ncdf.x <- x.ncdf.dim("x","degree", seq(cosmo.configuration$extent@xmin + cosmo.configuration$res.x / 2, cosmo.configuration$extent@xmax - cosmo.configuration$res.x / 2, cosmo.configuration$res.x))
  
  #get y dimension (subtract .5 to get center of cell)
  ncdf.y <- x.ncdf.dim("y","degree", seq(cosmo.configuration$extent@ymin + cosmo.configuration$res.y / 2, cosmo.configuration$extent@ymax - cosmo.configuration$res.y / 2, cosmo.configuration$res.y))
  
  #get time dimension
  if(is.na(year)) {
    ncdf.t <- x.ncdf.dim("t", "seconds since 1970-01-01 00:00", as.integer(), unlimited=TRUE)
  } else {
    ncdf.t <- x.ncdf.dim("t", "seconds since 1970-01-01 00:00", as.double(x.cosmode.timestamps(year)$timestamp))
  }
  
  #get forecast dimension
  ncdf.f <- x.ncdf.dim("f", "forecast in hours", 0:45)
  
  #get variable description
  ncdf.v <- x.ncdf.var.create(cosmo.configuration$phenomenon, cosmo.configuration$uom, list(ncdf.x, ncdf.y, ncdf.t, ncdf.f), chunksizes=chunksizes, compression=compression)
  
  #create NetCDF file
  ncdf <- x.ncdf.create(ncdf.file, ncdf.v)
  
  #write default attributes
  x.ncdf.attribute.write(ncdf, "x", 'standard_name', 'xCoordinate')
  x.ncdf.attribute.write(ncdf, "y", 'standard_name', 'yCordinate')
  x.ncdf.attribute.write(ncdf, "t", 'standard_name', 'time')
  x.ncdf.attribute.write(ncdf, "f", 'standard_name', 'forecast')
  
  x.ncdf.attribute.write(ncdf, "x", "axis", "X")
  x.ncdf.attribute.write(ncdf, "y", "axis", "Y")
  x.ncdf.attribute.write(ncdf, "t", "axis", "T")
  x.ncdf.attribute.write(ncdf, "f", "axis", "F")
  
  x.ncdf.attribute.write(ncdf, 0, "proj4_params", as.character(cosmo.configuration$proj))
  
  return(ncdf)
  
}


#'
#' update NetCDF file with COSMO images
#' @param ncdf.file COSMO NetCDF file path
#' @param cosmo.configuration cosmo configuration
#' @param append only append datastamps older than existing ones
#' @export
#' 
x.cosmode.ncdf.update <- function(ncdf.file,
                                  cosmo.configuration,
                                  append = F) {
  
  if(missing(ncdf.file))
    stop("Need to specify a target NetCDF file.")
  
  #open NetCDF file or create new file, if it does not yet exist
  if(!file.exists(ncdf.file)) {
    
    ncdf <- x.cosmode.ncdf.create(ncdf.file, cosmo.configuration, chunksizes = c(20,20,1,46), compression = 3)
    
  } else ncdf <- x.ncdf.open(ncdf.file, write = T)
  
  #get all timestamp from NetCDF, returns NULL, if empty
  t.all <- if(ncdf$dim$t$len > 0) as.POSIXct(ncdf$dim$t$vals, origin="1970-01-01", tz="UTC") else NULL
  
  #get all timestamps for the requested year
  t.df <- x.cosmode.timestamps(ifelse(is.null(t.all), as.numeric(format(Sys.time(), "%Y")), as.numeric(format(max(t.all), "%Y"))))
  
  #filter timestamps to be updated
  t.df <- t.df[!(t.df$timestamp %in% t.all) & t.df$timestamp <= Sys.time(), ]
  
  #filter latest timestamps, if append == T
  if(append == TRUE && !is.null(t.all))
    t.df <- t.df[t.df$timestamp > max(t.all), ]
  
  if(nrow(t.df) == 0){
    
    message("no timestamps to update")
    
    #close file
    x.ncdf.close(ncdf)
    
    return()
  }
  
  for(i in 1:nrow(t.df)) {
    
    #set row
    row <- t.df[i, ]
    cosmo.raster <- NULL
    
    tryCatch({
      
      timestamp = row[["timestamp"]]
      
      #read raster stack
      cosmo.stack <- x.cosmode.get(cosmo.configuration$dwd.root, cosmo.configuration$parameter, timestamp = timestamp, prediction = if(format(timestamp, "%H") == "03") 0:45 else 0:27)
      
      #subtract predecessors, if parameter is stored as total sum
      if(cosmo.configuration$is.sum) {
        
        #subtract predecessor
        for(j in nlayers(cosmo.stack):2) {
          cosmo.stack[[j]] <- cosmo.stack[[j]] - cosmo.stack[[j-1]]
        }
        
      }
      
      #write to NetCDF
      if(!is.null(cosmo.stack) && raster::nlayers(cosmo.stack) == if(format(timestamp, "%H") == "03") 46 else 28) {
        
        x.ncdf.write(ncdf, cosmo.configuration$phenomenon, raster=cosmo.stack, t.value=as.double(row[["timestamp"]]), t.index = row[["index"]])
        
      } else warning(paste0("COSMO raster for ", row[["timestamp"]], " is NULL or incomplete and was not written to NetCDF"))
      
    }, error = function(e){
      #do nothing
    })
    
    
  }
  
  #close file
  x.ncdf.close(ncdf)
  
}


#'
#' get COSMO forecast
#' @param ncdf NetCDF file object
#' @param extent requested COSMO extent
#' @param timestmap requested COSMO timestamp
#' @param statistics flag: request COSMO statistics
#' @return COSMO forecast
#' @export
#' 
x.cosmode.ncdf.forecast <- function(ncdf,
                                    extent = -1,
                                    timestamp = "latest",
                                    t.format = "%Y-%m-%d %H:%M:%S",
                                    t.zone = "UTC",
                                    statistics = F) {
  #get timestamps
  t.all <- ncdf$dim$t$vals
  if(is.null(t.all) || length(t.all) == 0 || !any(t.all > 0))
    stop("No valid timestamps found in NetCDF file.")
  
  #check for non-Posixt type
  if(!"POSIXt" %in% class(timestamp))
    timestamp <- if(timestamp == "latest") max(t.all) else as.double(as.POSIXct(timestamp, format=t.format, tz=t.zone))

  #select latest COSMO timestamp
  timestamp <- x.utility.closest(t.all, timestamp, "leq")
  if(timestamp < 0)
    stop("No corresponding timestamp found in NetCDF file.")
  
  #get forecast
  forecast <- if(format(timestamp, "%H") == "03") 0:45 else 0:27
  cosmo.subset <- x.ncdf.subset(ncdf, extent=extent, timestamp=as.double(timestamp), forecast=forecast, statistics=statistics, as.raster=!statistics)
  
  return(cosmo.subset)
  
}


#'
#' get COSMO timestamps with index
#' @param year year
#' @return timestamps with associated index
#' @export
#' 
x.cosmode.timestamps <- function(year) {
  
  #set min and max timestamp
  t.min <- as.POSIXct(paste0(year, "-01-01"), tz="UTC")
  t.max <- as.POSIXct(paste0(year, "-12-31 21:00:00"), tz="UTC")
  
  #set number of steps
  t.steps <- ceiling((as.double(t.max) - as.double(t.min)) / 10800) + 1
  
  #init dataframe
  df <- data.frame(timestamp = seq(t.min, t.max, by=10800), index = 1:t.steps)
  
  return(df)
  
}


#'
#' get COSMO timestamp for index
#' @param index timestamp index (index = 1 == t.first)
#' @param year year
#' @return timestamp associated with index
#' @export
#' 
x.cosmode.timestamp4index <- function(index,
                                      year) {
  
  #get first timestamp of target year
  t.first <-  as.POSIXct(paste0(year, "-01-01"), tz="UTC")
  
  #get target timestamp
  return(t.first + (index - 1) * 10800)
  
}


#'
#' get COSMO index for timestamp
#' @param timestamp timestamp
#' @param year year
#' @return index associated with timestamp
#' @export
#' 
x.cosmode.index4timestamp <- function(timestamp,
                                      year) {
  
  #get first timestamp of target year
  t.first <-  as.POSIXct(paste0(year, "-01-01"), tz="UTC")
  
  #get offset
  offset <- as.double(timestamp) - as.double(t.first)
  
  #get target timestamp
  return(ifelse(offset %% 10800 == 0, offset / 10800 , NA))
  
}
