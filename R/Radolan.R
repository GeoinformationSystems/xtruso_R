#' Read RADOLAN binary file from specified folder accordimg to the official DWD file pattern
#'
#' @param radolan.root root path, where RADOLAN images are stored (folder or URL)
#' @param radolan.type RADOLAN product type
#' @param timestamp requested timestamp for the RADOLAN image
#' @param previous flag: if a timestamp is not available, check previous timestamps according to the specified RADOLAN interval
#' @param previous.break number of previous timestamps to be checked, if previous = TRUE
#' @param rm.flagged remove flagged pixels from RADOLAN raster (set to NA)
#' @return requested RADOLAN raster
#' @export
x.radolan.get <- function(radolan.root,
                          radolan.type,
                          zipped = F,
                          timestamp = "latest",
                          previous = FALSE,
                          previous.break = 5,
                          rm.flagged = TRUE) {

  if(missing(radolan.root))
    stop("Need to specify path to RADOLAN root folder or URL.")

  if(missing(radolan.type))
    stop("Need to specify a RADOLAN product type.")

  #get RADOLAN configuration
  radolan.configuration<- x.radolan.configuration(radolan.type)

  #set timestamp
  if(!"POSIXt" %in% class(timestamp) && timestamp == "latest")
    timestamp <- eval(parse(text = radolan.configuration$time.latest))

  radolan.raster <- NULL
  previous.step = 0
  
  while(is.null(radolan.raster) && previous.step <= previous.break){
    
    #get path for RADOLAN raster
    radolan.path <- x.radolan.path(radolan.root, timestamp, radolan.configuration, zipped)
    
    #try to get RADOLAN raster from current path
    radolan.raster <- x.radolan.read(radolan.path, radolan.configuration, rm.flagged=rm.flagged)
    
    #break anyway, if previous = F
    if(!previous) break
    
    #check for previous timestamps
    if(is.null(radolan.raster)){
      timestamp <- timestamp - radolan.configuration$time.interval
      previous.step <- previous.step + 1
    }
    
  }
  
  if(is.null(radolan.raster))
    warning("Could not read requested RADOLAN raster.")
  
  return(radolan.raster)

}


#'
#' Get RADOLAN product configuration
#' @param radolan.type RADOLAN product type
#' @return true, if product tye is supported
x.radolan.configuration <- function(radolan.type){
  
  if(missing(radolan.type))
    stop("Need to specify a RADOLAN product type.")

  if(!radolan.type %in% names(xtruso::radolan.configuration))
    stop(paste("RADOLAN type", radolan.type, "is not supported.", sep=" "))

  return(xtruso::radolan.configuration[[radolan.type]])

}


#' Get proper path to RADOLAN file based on the type and specified timestamp
#'
#' @param radolan.root root path, where RADOLAN images are stored
#' @param timestamp requested timestamp for the RADOLAN image
#' @param radolan.configurationRADOLAN configuration
#' @param fx.prediction prediction intervals for RADOLAN FX product (0:120, step=5)
#' @return proper RADOLAN path
x.radolan.path <- function(radolan.root,
                           timestamp,
                           radolan.configuration,
                           zipped) {

  #get file name
  file.name <- if(zipped) {
    gsub("%%time%%", format(timestamp, radolan.configuration$time.format), radolan.configuration$zip.pattern)
  } else gsub("%%time%%", format(timestamp, radolan.configuration$time.format), radolan.configuration$file.pattern)

  #return url
  return(paste(radolan.root, file.name, sep="/"))

}


#' Read RADOLAN raster(s) from url, folder or file(s)
#'
#' @param radolan.files RADOLAN raster url(s), folder or file(s)
#' @param radolan.configuration RADOLAN configuration
#' @param file.pattern file pattern to search for, if radolan.path is a folder
#' @param rm.flagged flag: remove flagged pixels from RADOLAN image (set NA)
#' @return RADOLAN raster
#' @export
x.radolan.read <- function(radolan.files,
                           radolan.configuration,
                           file.pattern = radolan.configuration$file.pattern,
                           rm.flagged = TRUE) {

  if(missing(radolan.files))
    stop("Need to specify RADOLAN file(s)")

  if(missing(radolan.configuration))
    stop("Need to specify RADOLAN product configuration.")
  
  #set radolan file(s) for folder
  radolan.files <- if(!any(startsWith(radolan.files, c("http://", "https://", "ftp://"))) && dir.exists(radolan.files)) {
    list.files(radolan.files, pattern=gsub("%%time%%", "(.*)", file.pattern), full.names=TRUE)
  } else radolan.files
  
  if(length(radolan.files) == 0)
    stop("There are no files matching the requested RADOLAN product.")
  
  radolan.stack <- raster::stack()
  tmp.dir <- tempdir()

  for(radolan.file in radolan.files){
  
    download <- FALSE
  
    #download
    if(any(startsWith(radolan.file, c("http://", "https://", "ftp://")))){
      tryCatch({
        tmp.path <- paste(tmp.dir, basename(radolan.file), sep="/")
        utils::download.file(radolan.file, tmp.path, mode="wb")
        radolan.file <- tmp.path
        download <- TRUE
      }, error = function(e){
        return(NULL)
      })
    }
  
    if(!file.exists(radolan.file))
      return(NULL)
  
    #unzip
    if(any(endsWith(radolan.file, c(".bz2", ".gz")))) {
      
      #unpack and get list of files
      tmp.dest <- paste(tmp.dir, gsub("\\.", "", basename(radolan.file)), sep="/")
      utils::untar(radolan.file, exdir=tmp.dest)
      
      #get unpacked files
      tmp.files <- list.files(tmp.dest, pattern=gsub("%%.*%%", "(.*)", radolan.configuration$file.pattern), full.names=TRUE)
      
      #run x.radolan.read with unpacked files
      tmp.stack <- x.radolan.read(tmp.files, radolan.configuration, radolan.configuration$file.pattern, rm.flagged)
      radolan.stack <- stack(radolan.stack, tmp.stack)
      
      #clean
      unlink(tmp.dest, recursive = TRUE)
      
    } else {
  
      #read file
      radolan.raster <- x.radolan.parse(radolan.file, radolan.configuration, rm.flagged)
    
      #set timestamp and product type as attributes
      attr(radolan.raster, "timestamp") <- x.radolan.pattern.value(radolan.file, radolan.configuration)
      attr(radolan.raster, "type") <- radolan.configuration$type
      
      #set prediction for FX
      if(radolan.configuration$type == "FX")
        attr(radolan.raster, "prediction") <- x.radolan.pattern.value(radolan.file, radolan.configuration, property = "%%prediction%%")
      
      #add raster to stack
      radolan.stack <- raster::addLayer(radolan.stack, radolan.raster)
      
    }
    
    #clean
    if(download)
      unlink(radolan.file)
    
  }

  #return
  if(raster::nlayers(radolan.stack) == 1) return(radolan.stack[[1]])
  else return(radolan.stack)

}


#' get value from RADOLAN file name pattern
#'
#' @param radolan.file path to the RADOLAN input file
#' @param radolan.type RADOLAN type according to DWD classification (see radolan.configuration for supported types)
#' @param property to search for
#' @return RADOLAN timestamp
x.radolan.pattern.value <- function(radolan.file, 
                                    radolan.configuration, 
                                    property = "%%time%%") {

  #get filename from path
  file.name <- basename(radolan.file)

  #extract surrounding strings strings for property
  surroundings <- unlist(strsplit(radolan.configuration$file.pattern, property))
  
  #replace additional escaped properties with (.*)
  surroundings <- gsub("%%.*%%", "[A-Za-z0-9]*", surroundings)
  
  #replace surroundings
  value <- gsub(paste(surroundings[1], surroundings[2], sep="|"), "", file.name)

  return(if(property == "%%time%%") strptime(value, radolan.configuration$time.format, "UTC") else value)

}


#' Parse RADOLAN binary file
#' adapted from http://moc.online.uni-marburg.de/doku.php?id=courses:bsc:project-thesis-geoei:lecture-notes:bdh:pt-ge-ln-bdh-01-9000
#'
#' @param radolan.path path to the RADOLAN binary input file
#' @param radolan.type RADOLAN type according to DWD classification (see radolan.configuration for supported types)
#' @param rm.flagged remove flagged pixels from RADOLAN image
#' @return RADOLAN raster object
x.radolan.parse <- function(radolan.path,
                            radolan.configuration,
                            rm.flagged = TRUE) {

  if(missing(radolan.path))
    stop("Need to specify path to RADOLAN binary.")

  if(missing(radolan.configuration))
    stop("Need to specify RADOLAN product configuration.")

  #define end of header position ("03")
  header.end <- regexpr("\003", readLines(radolan.path, 1, skipNul = TRUE, warn = FALSE))

  #open binary stream
  radolan.stream <- file(radolan.path, "rb")

  #read header
  tmp <- readBin(radolan.stream, "raw", n = header.end, endian = "little")

  #read integer data (note: includes flags described in RADOLAN spec)
  radolan.data <- readBin(radolan.stream, integer(), n = radolan.configuration$nrow * radolan.configuration$ncol, size = radolan.configuration$bits, endian = "little", signed=FALSE)

  #close connection
  close(radolan.stream)

  #create raster with projection
  radolan.raster <- raster::raster(t(matrix(radolan.data, ncol = radolan.configuration$ncol, nrow = radolan.configuration$nrow)))

  #flip raster direction
  radolan.raster <- raster::flip(radolan.raster, "y")

  #set extent and projection
  raster::extent(radolan.raster) <- radolan.configuration$extent
  raster::projection(radolan.raster) <- radolan.configuration$proj
  
  #handle Bit 13 (secondary data source for 2-bit data)
  if(radolan.configuration$bits == 2)
    radolan.raster[radolan.raster >= 4096 & radolan.raster < 8192] <- radolan.raster[radolan.raster >= 4096 & radolan.raster < 8192] - 4096

  #remove flagged values
  if(rm.flagged)
    radolan.raster[radolan.raster > radolan.configuration$max.value] <- NA

  #convert values, if precision != 1
  if(radolan.configuration$precision != 1)
    radolan.raster <- radolan.raster * radolan.configuration$precision

  #convert RVP6 values to dBZ, remove negative dBZ
  if(radolan.configuration$convert.to.dBZ) {
    radolan.raster <- radolan.raster / 2 - 32.5
    radolan.raster[radolan.raster < 0] <- 0
  }

  #return
  return(radolan.raster)

}


#'
#' get coordinate in 900x900 RADOLAN CRS from col-row-index or cell number
#' @param index.col col index; ignored, if cell number is set
#' @param index.row row index; ignored, if cell number is set
#' @param cell cell number
#' @return RADOLAN coordinate for index
#' @export
x.radolan.coordinate <- function(index.col, 
                                 index.row,
                                 cell = NA) {
  
  #get row and col for cell
  if(!is.na(cell)){
    
    index.col <- cell %% 900
    index.row <- ceiling(cell / 900)
    
  }
  
  #get target coordinate
  coord.x <- xtruso::radolan.configuration.extent900@xmin + index.col - 0.5
  coord.y <- xtruso::radolan.configuration.extent900@ymax - index.row + 0.5
  
  #return sp coordinate
  return(sp::SpatialPoints(cbind(coord.x, coord.y), proj4string=xtruso::radolan.configuration.crs))
  
}


#'
#' get coordinate index in 900x900 RADOLAN CRS from coordinate or cell number
#' @param coord.x x coordinate (RADOLAN projection); ignored, if cell number is set
#' @param coord.y y coordinate (RADOLAN projection); ignored, if cell number is set
#' @param threshold cell border threshold, if index is within this threshold distance from cell border, both cell indices are returned; ignored, if cell number is set
#' @param cell cell number
#' @return RADOLAN index for x and y
#' @export
x.radolan.index <- function(coord.x, 
                            coord.y,
                            coord.threshold = 0.05,
                            cell = NA) {
  
  if(!is.na(cell)){
    
    #get row and col for cell
    index.col <- cell %% 900
    index.row <- ceiling(cell / 900)
    
  } else {
    
    #get row and col from coordinates
    index.col <- coord.x - xtruso::radolan.configuration.extent900@xmin + 0.5
    index.row <- xtruso::radolan.configuration.extent900@ymax - coord.y + 0.5
    
    #if index position is below raster cell border threshold, add both indices to result
    index.col <- if(((index.col + .5) %% 1) < 0.05 || ((index.col + .5) %% 1) > 0.95) c(floor(index.col), ceiling(index.col)) else round(index.col)
    index.row <- if(((index.row + .5) %% 1) < 0.05 || ((index.row + .5) %% 1) > 0.95) c(floor(index.row), ceiling(index.row)) else round(index.row)
    
  }
  
  #return index list
  return(list(col=index.col, row=index.row))
  
}


#'
#' get indices and weights over 900x900 RADOLAN raster for a given polygon
#' @param polygon input polygon
#' @param raster for which the overlay is computed
#' @return RADOLAN bounding box indices and weights for x and y covering the polygon
#'
x.radolan.overlay <- function(polygon,
                              raster = xtruso::xtruso.radolan.sample) {
  
  #get zonal overlay
  overlap <- x.zonal.overlap(raster, polygon, polygon.id="GKZ", parallel=F)
  
  #get col and row indices spanning the polygon
  index.col <- c(min(overlap$col) : max(overlap$col))
  index.row <- c(min(overlap$row) : max(overlap$row))
  
  #init weight matrix for polygon based on overlap
  weights = array(0, dim=c(length(index.row), length(index.col)))
  colnames(weights) <- index.col
  rownames(weights) <- index.row
  
  #set weights
  for(i in 1:nrow(overlap)){
    weights[as.character(overlap[i, "row"]), as.character(overlap[i, "col"])] <- overlap[i, "weight"]
  }
  
  #return col, row and weights in a list
  return(list(col=index.col, row=index.row, weights=weights))
  
}


#' 
#' Disaggregate RADOLAN RW raster by RX reflectivity rasters
#'
#' @param path.rw path to RW product to be disaggregated
#' @param path.rx path to RX product for disaggregation
#' @param path.out path to write disaggregated TIF output files; if NA, no files are written
#' @param parallel flag: enable parallel computation with foreach
#' @return disaggregated RW product, raster stack with 12 layers for each RX raster
#' @export
x.radolan.disaggregate <- function(path.rw,
                                   path.rx,
                                   strategy = "weighted",
                                   path.out = getwd(),
                                   parallel = TRUE) {
  
  if(missing(path.rw))
    stop("Need to specify path to RADOLAN RW files.")
  
  if(missing(path.rx))
    stop("Need to specify path to RADOLAN RX files.")
  
  if(!strategy %in% c("weighted"))
    stop("Need to specify valid disaggregation stategy.")
 
  #sequence for RX files (values subtracted from RW timestamp)
  seq.rx <- seq(0, 55, by=5)
  
  #sequence of temporary file names and time difference from RW
  names.rx <- sprintf("file.rx.%d", seq.rx)
  timediff.rx <- sprintf("timediff.rx.%d", seq.rx)
  
  #get list of all RW files
  files.rw <- list.files(path.rw, pattern=xtruso::radolan.configuration$RW$file.pattern, full.names=T)
  
  #get timestamps for each RW file
  timestamps.rw <- unlist(lapply(files.rw, function(file){
    as.character(x.radolan.pattern.value(file, radolan.configuration.rw))
  }))
  
  #build dataframe with RW timestamp and corresponding file path
  df.rw <- data.frame("timestamp.rw" = timestamps.rw, "file.rw" = files.rw, stringsAsFactors = F)
  
  #get RX timestamps and path associated with each RW timestamp
  for(i in seq.rx){
    
    #set RX timestamps
    t.index <- paste("timediff.rx", i, sep=".")
    t.path <- paste("file.rx", i, sep=".")
    df.rw[t.index] <- format(as.POSIXct(df.rw[,"timestamp.rw"], tz="UTC") - (i * 60), format="%y%m%d%H%M")
    
    #set path to corresponding RX file
    df.rw[t.path] <- apply(df.rw, 1, function(row) {
      paste0(path.rx, "/raa01-rx_10000-", row[t.index], "-dwd---bin")
    })
    
    #check, if RX files exists, set NA, if file is not present
    df.rw[file.exists(df.rw[, t.path]) == FALSE, t.path] <- NA
  }
  
  #determine number of missing RX files for each RW file
  df.rw["missing"] <- apply(df.rw[, names.rx], 1, function(row) {
    
    #determine NA in file list (indicates missing file)
    sum(is.na(row))
   
  })
  
  #check for output path
  if(!file.exists(path.out) || !file.info(path.out)$isdir)
    dir.create(file.path(path.out), showWarnings = FALSE)
   
  #parallel disaggregate RW image using weights from RX images
  if (parallel && "doParallel" %in% installed.packages()[, "Package"]) {
    
    require(doParallel, quietly = TRUE)
    
    #init parallel environment
    cl <- makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)
    
    #run disaggregation for each row
    foreach::foreach(i = 1:nrow(df.rw), .export=c("df.rw", "names.rx", "strategy", "path.out"), .packages=c("raster")) %dopar% {
      
      #do nothing, if there is a missing RX file
      #TODO implement reasonable means for interpolation
      if(df.rw[i, "missing"] > 0) {
        warning(paste0("No disaggregation for t:", df.rw[i, "timestamp.rw"], ", due to ", df.rw[i, "missing"], " missing RX value(s)"))
        
      } else x.radolan.disaggregate.row(df.rw[i, ], names.rx, strategy, path.out)

    }
    
    #stop cluster
    parallel::stopCluster(cl)
    
  #sequential disaggregation for each row
  } else {
    
    apply(df.rw, 1, function(row){
      
      if(row["missing"] > 0) {
        warning(paste0("No disaggregation for t:", row["timestamp.rw"], ", due to ", row["missing"], " missing RX value(s)"))
        
      } else x.radolan.disaggregate.row(row, names.rx, strategy, path.out)
      
    })
    
  }
  
}


#' 
#' Write disaggregated RW files using specified RX file stack
#'
#' @param row row specifying corresponding RW and RX files
#' @param names.rx columns idnetifying the RX files
#' @param strategy disaggregation strategy
#' @param path.out path to write disaggregated TIF output files
x.radolan.disaggregate.row <- function(row,
                                       names.rx,
                                       strategy,
                                       path.out) {
  
  #read RADOLAN RW
  raster.rw <- x.radolan.read(row[, "file.rw"], xtruso::radolan.configuration$RW)

  #read RX stack
  stack.rx <- x.radolan.read(as.character(row[, names.rx]), xtruso::radolan.configuration$RX)
  
  #disaggregate based on reflectivity values in RX stack
  if(strategy == "weighted"){
    stack.rw <- x.utility.raster.disaggregate(raster.rw, stack.rx)
  } else {
    warning(paste0("Invalid disaggregation strategy: ", strategy))
    return(NULL)
  }
  
  #write RW stack to output folder
  for(i in 1:nlayers(stack.rw)){
    
    #set output name
    name.out <- paste0("RW_RX_", strftime(attr(stack.rx[[i]], "timestamp"), format="%y%m%d%H%M"), ".tif")
    
    #write
    raster::writeRaster(stack.rw[[i]], filename=paste(path.out, name.out, sep="/"), format="GTiff", overwrite=TRUE, options=c("COMPRESS=DEFLATE"))
    
  }
    
}


#'
#' create NetCDF file for RADOLAN images
#' @param ncdf.file NetCDF file path
#' @param radolan.configuration configuration for RADOLAN files
#' @param radolan.folder RADOLAN files to write
#' @param chunksizes target chunksizes
#' @param compression file compression
#' @param close flag: close NetCDF file after creation
#' @return NetCDF file pointer
#' @export
x.radolan.ncdf.create <- function(ncdf.file,
                                  radolan.configuration,
                                  radolan.folder = NA,
                                  chunksizes = NA,
                                  compression = NA,
                                  close = T) {
  
  if(missing(ncdf.file))
    stop("Need to specify a NetCDF file.")
  
  if(missing(radolan.configuration))
    stop("Need to specify a RADOLAN configuration.")
  
  #create x dimension based on RADOLAN extent (subtract .5 to get center of cell)
  ncdf.x <- x.ncdf.dim("x","m", seq(radolan.configuration$extent@xmin + .5, radolan.configuration$extent@xmax - .5, 1))
  
  #get y dimension (subtract .5 to get center of cell)
  ncdf.y <- x.ncdf.dim("y","m", seq(radolan.configuration$extent@ymin + .5, radolan.configuration$extent@ymax - .5, 1))
  
  #get unlimited time dimension
  ncdf.t <- x.ncdf.dim("t", "seconds since 1970-01-01 00:00", as.integer(), unlimited=TRUE)
  
  #get variable description
  ncdf.v <- x.ncdf.var.create(radolan.configuration$phenomenon, radolan.configuration$uom, list(ncdf.x, ncdf.y, ncdf.t), chunksizes=chunksizes, compression=compression)
  
  #create NetCDF file
  ncdf <- x.ncdf.create(ncdf.file, ncdf.v)
  
  #write default attributes
  x.ncdf.attribute.write(ncdf, "x", 'standard_name', 'xCoordinate')
  x.ncdf.attribute.write(ncdf, "y", 'standard_name', 'yCordinate')
  x.ncdf.attribute.write(ncdf, "t", 'standard_name', 'time')
  
  x.ncdf.attribute.write(ncdf, "x", "axis", "X")
  x.ncdf.attribute.write(ncdf, "y", "axis", "Y")
  x.ncdf.attribute.write(ncdf, "t", "axis", "T")
  
  x.ncdf.attribute.write(ncdf, 0, "proj4_params", as.character(radolan.configuration$proj))
  x.ncdf.attribute.write(ncdf, 0, "RADOLAN_product", radolan.configuration$type)
  
  #write rasters from folder
  if(!is.na(radolan.folder)){
    
    #get all files from folder
    radolan.files <- list.files(radolan.folder, pattern=gsub("%%time%%", "(.*)", radolan.configuration$file.pattern), full.names=TRUE)
    
    #write raster files to NetCDF
    for(i in 1:length(radolan.files)){

      #read raster
      radolan.raster <- x.radolan.read(radolan.files[i], radolan.configuration)
      
      #write to NetCDF
      if(!is.null(radolan.raster)) {
        x.ncdf.write(ncdf, ncdf.v, raster=radolan.raster, t.value=as.double(attr(radolan.raster, "timestamp")), t.index=i)
      } else 
        warning(paste("File",radolan.file,"is NULL, was not written to NetCDF", sep=" "))
    }
    
  }
  
  #return file pointer
  if(close)
    x.ncdf.close(ncdf)
  
  return(ncdf)
  
}


#'
#' update NetCDF file with RADOLAN images
#' @param ncdf.file NetCDF file path
#' @param radolan.folder folder to search for RADOLAN images; if NA, images are retrived online
#' @export
#' 
x.radolan.ncdf.update <- function(ncdf.file,
                                  radolan.configuration,
                                  radolan.folder = NA) {
  
  if(missing(ncdf.file))
    stop("Need to specify a target NetCDF file.")
  
  #open NetCDF file or create new file, if it does not yet exist
  if(!file.exists(ncdf.file)) {
    ncdf <- x.radolan.ncdf.create(ncdf.file, radolan.configuration, chunksizes = c(20,20,100), compression = 3, close = F)
  } else ncdf <- x.ncdf.open(ncdf.file, write = T)
  
  #get all timestamp from NetCDF, beginning of current year, if empty
  t.all <- if(ncdf$dim$t$len > 0) as.POSIXct(ncdf$dim$t$vals, origin="1970-01-01", tz="UTC") else strptime(paste0(format(Sys.time(), "%Y"), "-01-01"), format="%Y-%m-%d", tz="UTC")
  
  #get all timestamps for the requested year
  t.df <- x.radolan.timestamps(radolan.configuration, as.numeric(format(max(t.all), "%Y")))
  
  #filter timestamps to be updated
  t.df <- t.df[!(t.df$timestamp %in% t.all) & t.df$timestamp <= Sys.time(), ]
  
  if(nrow(t.df) == 0){
    
    message("no timestamps to update")
    
    #close file
    x.ncdf.close(ncdf)
    
    return()
  }
    
  
  for(i in 1:nrow(t.df)) {
    
    #set row
    row <- t.df[i, ]
    radolan.raster <- NULL
    
    #get RADOLAN raster from folder (if specified) or online
    if(!is.na(radolan.folder))
      radolan.raster <- x.radolan.get(radolan.folder, radolan.configuration$type, timestamp = row[["timestamp"]])
    
    #if null, try to get RADOLAN raster online
    if(is.null(radolan.raster))
      radolan.raster <- x.radolan.get(radolan.configuration$dwd.root, radolan.configuration$type, timestamp = row[["timestamp"]], zipped = FALSE)
    
    #write to NetCDF
    if(!is.null(radolan.raster)) {
      x.ncdf.write(ncdf, radolan.configuration$phenomenon, raster=radolan.raster, t.value = as.double(row[["timestamp"]]), t.index = row[["index"]])
    } else 
      warning(paste0("RADOLAN raster for ", row[["timestamp"]], " is NULL and was not written to NetCDF"))
  }
  
  #close file
  x.ncdf.close(ncdf)
  
}


#'
#' get RADOLAN timestamps with index
#' @param radolan.configuration RADOLAN configuration
#' @param year year
#' @return timestamps with associated index
#' @export
#' 
x.radolan.timestamps <- function(radolan.configuration,
                                 year) {
  
  #set min and max timestamp
  t.min <- strptime(gsub("%%year%%", year, radolan.configuration$time.firstOfYear), format="%Y-%m-%d %H:%M:%SZ", tz="UTC")
  t.max <- strptime(paste0(year + 1, "-01-01"), format="%Y-%m-%d", tz="UTC") - 1
  
  #set number of steps
  t.steps <- ceiling((as.double(t.max) - as.double(t.min)) / radolan.configuration$time.interval)
  
  #init dataframe
  df <- data.frame(timestamp = seq(t.min, t.max, by=radolan.configuration$time.interval), index = 1:t.steps)
  
  return(df)
  
}


#'
#' get RADOLAN timestamp for index
#' @param radolan.configuration RADOLAN configuration
#' @param index timestamp index (index = 1 == t.first)
#' @param year year
#' @return timestamp associated with index
#' @export
#' 
x.radolan.timestamp4index <- function(radolan.configuration,
                                      index,
                                      year) {
  
  #get first timestamp of target year
  t.first <- strptime(gsub("%%year%%", year, radolan.configuration$time.firstOfYear), format="%Y-%m-%d %H:%M:%SZ")
  
  #get target timestamp
  return(t.first + (index - 1) * radolan.configuration$time.interval)
  
}


#'
#' get RADOLAN index for timestamp
#' @param radolan.configuration RADOLAN configuration
#' @param timestamp timestamp
#' @param year year
#' @return index associated with timestamp
#' @export
#' 
x.radolan.index4timestamp <- function(radolan.configuration,
                                      timestamp,
                                      year) {
  
  #get first timestamp of target year
  t.first <- strptime(gsub("%%year%%", year, radolan.configuration$time.firstOfYear), format="%Y-%m-%d %H:%M:%SZ")
  
  #get offset
  offset <- as.double(timestamp) - as.double(t.first)
  
  #get target timestamp
  return(ifelse(offset %% radolan.configuration$time.interval == 0, offset / radolan.configuration$time.interval , NA))
  
}


#'
#' interpolate RADOLAN values to match full hour timestamps
#' @param radolan.matrix RADOLAN value matrix (time slices with dimnames representing x,y,t)
#' @param timestamps new timestamps for interpolation
#' @return interpolated RADOLAN matrix
#' @export
#' 
x.radolan.interpolate <- function(radolan.matrix, 
                                  timestamps = as.double(dimnames(radolan.matrix)[[3]]) + 600) {
  
  #get old timestamps from matrix dimnaes
  timestamps.orig <- as.double(dimnames(radolan.matrix)[[3]])
  
  #interpolate RADOLAN values
  matrix.new <- apply(radolan.matrix, 1:2, function(x){
    
    #define interpolation function for each row/col
    f <- approxfun(timestamps.orig, x)
    
    #interpolate
    return(f(timestamps))
    
  })
  
  #reorder and set new timestamps
  matrix.new <- aperm(matrix.new, c(2,3,1))
  dimnames(matrix.new)[[3]] <- timestamps
  
  return(matrix.new)
  
}