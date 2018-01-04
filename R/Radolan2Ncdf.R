#' Upload Radolan images to specified scidb instance
#'
#' @param scidb.conn scidb connection
#' @param scidb.array scidb target array
#' @param radolan.folder folder with RADOLAN binary files
#' @param radolan.type radolan type
#' @param sample sample size (e.g. for debugging)
#' @return list with runtime information for raster creation and upload
#' @export
Radolan2Ncdf <- function(ncdf.filepath,
                         radolan.folder,
                         radolan.type,
                         subset = NA,
                         compression = NA) {

  if(missing(ncdf.filepath))
    stop("Need to specify a NetCDF file path.")

  if(missing(radolan.folder))
    stop("Need to specify a folder with RADOLAN binary files.")

  if(missing(radolan.type))
    stop("Need to specify a RADOLAN product type.")

  #get RADOLAN files from folder, check for file pattern
  radolan.configuration <- ReadRadolan.getConfiguration(radolan.type)
  radolan.files <- list.files(radolan.folder, pattern=gsub("%%time%%", "(.*)", radolan.configuration$file.pattern), full.names=TRUE)

  if(length(radolan.files) == 0)
    stop("There are no files matching the requested RADOLAN product.")

  #subsample input, if requested
  if(!is.na(subset)) radolan.files <- radolan.files[1:min(subset,length(radolan.files))]

  #init NetCDF variable for RADOLAN
  ncdf.v <- Radolan2Ncdf.createVar(ncdf.phen=radolan.configuration$phenomenon, ncdf.phen.uom=radolan.configuration$uom, compression)

  #init NetCDF file
  ncdf.file <- Radolan2Ncdf.createFile(ncdf.filepath, ncdf.v)
  Radolan2Ncdf.writeDefaultAtt(ncdf.file, radolan.type)

  #query scidb
  tryCatch({

    #iterate files, write NetCDF
    for(i in 1:length(radolan.files)){

      #read raster
      radolan.raster <- ReadRadolanBinary(radolan.files[i], radolan.type)

      #write to NetCDF
      if(!is.null(radolan.raster))
        Radolan2Ncdf.writeRaster(ncdf.file, ncdf.v, radolan.raster, time.index=i)

      else
        message(paste("File",radolan.file,"is NULL, was not written to NetCDF", sep=" "))
    }

  }, error = function(err) {

    message(err,"\n")

  }, finally = {

    #close file
    Radolan2Ncdf.closeFile(ncdf.file)

  })

  return(ncdf.file)

}



#'
#' write default RADOLAN attributes to NetCDF file
#' @param ncdf.file NetCDF file
#' @param ncdf.v NetCDF variable
#' @param radolan.raster RADOLAN raster
#' @param time.index index for raster timestamp within NetCDF file
#' @param extent target extent of RADOLAN raster
Radolan2Ncdf.writeRaster <- function(ncdf.file,
                                     ncdf.v,
                                     radolan.raster,
                                     time.index = 1) {

  if(missing(ncdf.file))
    stop("Need to specify a NetCDF file.")

  if(missing(radolan.raster))
    stop("Need to specify a RADOLAN raster.")

  #get timestamp
  timestamp <- as.double(attr(radolan.raster, "timestamp"))

  #get raster matrix
  matrix <- raster::as.matrix(radolan.raster)

  #add timestamp as variable
  ncdf4::ncvar_put(ncdf.file, "t", timestamp, start=time.index, count=1)

  #add radolan matrix for selected timestamp
  ncdf4::ncvar_put(ncdf.file, ncdf.v, matrix, start=c(1,1,time.index), count=c(-1,-1,1))

}


#'
#' create NetCDF file for given variable
#' @param ncdf.filepath NetCDF file path
#' @param ncdf.v NetCDF variable
#' @export
Radolan2Ncdf.createFile <- function(ncdf.filepath,
                                    ncdf.v) {

  if(!"ncdf4" %in% installed.packages()[, "Package"])
    stop("Package ncdf4 is not installed.")

  if(missing(ncdf.filepath))
    stop("Need to specify a NetCDF file path.")

  if(missing(ncdf.v))
    stop("Need to specify a NetCDF variable.")

  return(ncdf4::nc_create(ncdf.filepath, ncdf.v, force_v4=TRUE))

}


#'
#' close default RADOLAN attributes to NetCDF file
#' @param ncdf.file NetCDF file
Radolan2Ncdf.openFile <- function(ncdf.filepath) {

  if(missing(ncdf.filepath))
    stop("Need to specify a NetCDF filepath.")

  if(!file.exists(ncdf.filepath))
    stop("Need to specify an existing NetCDF filepath.")

  return(nc_open(ncdf.filepath))

}


#'
#' close default RADOLAN attributes to NetCDF file
#' @param ncdf.file NetCDF file
Radolan2Ncdf.closeFile <- function(ncdf.file) {

  if(missing(ncdf.file))
    stop("Need to specify a NetCDF file.")

  ncdf4::nc_close(ncdf.file)

}


#'
#' write default RADOLAN attributes to NetCDF file
#' @param ncdf.file NetCDF file
Radolan2Ncdf.writeDefaultAtt <- function(ncdf.file,
                                         radolan.type) {

  if(missing(ncdf.file))
    stop("Need to specify a NetCDF file.")

  if(missing(radolan.type))
    stop("Need to specify a radolan product type.")

  #set dimension description
  ncdf4::ncatt_put(ncdf.file, "x", 'standard_name', 'xCoordinate')
  ncdf4::ncatt_put(ncdf.file, "y", 'standard_name', 'YCordinate')
  ncdf4::ncatt_put(ncdf.file, "t", 'standard_name', 'Timestamp')

  #set axis information
  ncdf4::ncatt_put(ncdf.file, "x", "axis", "X")
  ncdf4::ncatt_put(ncdf.file, "y", "axis", "Y")
  ncdf4::ncatt_put(ncdf.file, "t", "axis", "T")

  #set projection
  ncdf4::ncatt_put(ncdf.file, 0, "proj4_params", as.character(xtruso::radolan.configuration.crs))

  #set radolan info
  ncdf4::ncatt_put(ncdf.file, 0, "product_type", radolan.type)

}



#'
#' create NetCDF Variable for RADOLAN data (extent 900*900*t)
#' @param ncdf.phen phenomenon
#' @param ncdf.phen.uom phenomenon uom
#' @param compression ncdf4 compression
#' @return NetCDF variable for RADOLAN products
Radolan2Ncdf.createVar <- function(ncdf.phen,
                                   ncdf.phen.uom,
                                   compression = NA) {

  if(missing(ncdf.phen))
    stop("Need to specify a phenomenon.")

  if(missing(ncdf.phen.uom))
    stop("Need to specify a unit of measurement for ncdf.phen.")

  if(!is.na(compression) && !(compression %in% 1:9))
    stop("Need to specify a compression between 1 and 9, or NA.")

  #get x dimension (subtract .5 to get center of cell)
  ncdf.x <- ncdf4::ncdim_def("x","m", seq(xtruso::radolan.configuration.extent900@xmin + .5, xtruso::radolan.configuration.extent900@xmax - .5, 1))

  #get y dimension (subtract .5 to get center of cell)
  ncdf.y <- ncdf4::ncdim_def("y","m", seq(xtruso::radolan.configuration.extent900@ymin + .5, xtruso::radolan.configuration.extent900@ymax - .5, 1))

  #get unlimited time dimension
  ncdf.t <- ncdf4::ncdim_def("t", "seconds since 1970-01-01 00:00", as.integer(), unlim=TRUE)

  #create variable declaration
  ncdf.v <- ncdf4::ncvar_def(ncdf.phen, ncdf.phen.uom, list(ncdf.x, ncdf.y, ncdf.t), NA, prec="float", compression = compression)

  return(ncdf.v)

}


#'
#' request a RADOLAN raster or raster stack from NetCDF file
#'
#' @param ncdf.file NetCDF file
#' @return available timestamps in the provided NetCDF file (as.double.POSIXlt)
#' @export
Radolan2Ncdf.getTimestamps <- function(ncdf.file,
                                       ncdf.phen){

  if(missing(ncdf.file))
    stop("Need to specify a NetCDF file.")

  return(ncdf.file$dim$t$vals)

}


#'
#' request a RADOLAN raster or raster stack from NetCDF file
#'
#' @param ncdf.file NetCDF file
#' @param timestamp single timestamp or list of timestamps to be requested
#' @param ncdf.phen phenomenon
#' @return RADOLAN raster or raster stack
#' @export
Radolan2Ncdf.requestImage <- function(ncdf.file,
                                      timestamp,
                                      ncdf.phen){

  if(missing(ncdf.file))
    stop("Need to specify a NetCDF file.")

  if(missing(ncdf.phen))
    ncdf.phen <- ncdf.file$var[[1]]$name

  if(missing(timestamp))
    stop("Need to specify a timestamp.")

  radolan.stack <- raster::stack()
  for(t in timestamp) {

    #get t dimension index
    t.index <- match(t, ncdf.file$dim$t$vals)

    if(!is.na(t.index)){
      #get raster from NetCDF file
      radolan.raster <- raster::raster(ncdf4::ncvar_get(ncdf.file, ncdf.phen, start=c(1,1,t.index), count=c(-1,-1,1)))

      #set RADOLAN extent and CRS
      radolan.raster <- raster::setExtent(radolan.raster, xtruso::radolan.configuration.extent900)
      raster::projection(radolan.raster) <- xtruso::radolan.configuration.crs

      #set timestamp and product type as attributes
      attr(radolan.raster, "timestamp") <- as.POSIXct(timestamp, origin="1970-01-01", tz="UTC")
      attr(radolan.raster, "type") <- ncdf4::ncatt_get(ncdf.file, 0, "product_type")$value

      radolan.stack <- raster::addLayer(radolan.stack, radolan.raster)
    }
  }

  if(raster::nlayers(radolan.stack) == 1) return(radolan.stack[[1]])
  else return(radolan.stack)

}


#'
#' request a RADOLAN subset from NetCDF file
#'
#' @param ncdf.file NetCDF file
#' @param start start indices (x, y, t)
#' @param count dim length to be requested  (x, y, t); -1 indicates whole dimension
#' @param ncdf.phen phenomenon
#' @param xyIndex flag: start coordinates for x and y are index coordinates; if false RADOLAN coordinates are assumed
#' @param tIndex flag: start timestamp is index; if false double timestamp in seconds since 1970-01-01 is assumed
#' @return RADOLAN raster or raster stack
#' @export
Radolan2Ncdf.requestSubset <- function(ncdf.file,
                                       start,
                                       count,
                                       ncdf.phen,
                                       xyIndex=TRUE,
                                       tIndex=FALSE){

  if(missing(ncdf.file))
    stop("Need to specify a NetCDF file.")

  if(missing(ncdf.phen))
    ncdf.phen <- ncdf.file$var[[1]]$name

  if(missing(start) || length(start) != 3)
    stop("Need to specify a valid start coordinate (x, y, t).")

  if(missing(count) || length(count) != 3)
    stop("Need to specify a valid count (x, y, t).")

  #get xy start coordinate
  if(xyIndex == FALSE)
    start[1:2] <- Utility.getRADOLANindex(start[1], start[2])

  #get timestamp
  if(tIndex == FALSE)
    start[3] <- match(start[3], ncdf.file$dim$t$vals)

  #get subset from NetCDF file
  radolan.subset <- ncdf4::ncvar_get(ncdf.file, ncdf.phen, start=start, count=count)

  return(radolan.subset)

}
