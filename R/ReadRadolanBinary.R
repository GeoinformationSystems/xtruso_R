#' Read RADOLAN binary raster
#'
#' This function reads a RADOLAN binary file as provided by the DWD
#'
#' @param file.path path to the RADOLAN binary input file
#' @param radolan.type RADOLAN type according to DWD classification (see radolan.configuration for supported types)
#' @param rm.flagged remove flagged pixels from RADOLAN image
#' @return RADOLAN raster object
#' @export
ReadRadolanBinary <- function(file.path,
                              radolan.type,
                              rm.flagged = TRUE) {

  if(missing(file.path))
    stop("Need to specify path to RADOLAN binary.")

  if(missing(radolan.type))
    stop("Need to specify type of RADOLAN product.")

  if(!(radolan.type %in% names(radolan.configuration)))
    stop(paste("RADOLAN type", type, "is not supported.", sep=" "))

  #get RADOLAN configuration
  configuration <- radolan.configuration[[radolan.type]]

  #read raster file
  radolan.raster <- ReadRadolanBinary.read(file.path, configuration)

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


#' Read RADOLAN binary raster
#'
#' This function reads a RADOLAN binary file as provided by the DWD
#' derived from http://moc.online.uni-marburg.de/doku.php?id=courses:bsc:project-thesis-geoei:lecture-notes:bdh:pt-ge-ln-bdh-01-9000
#'
#' @param file.path path to the RADOLAN binary input file
#' @param configuration RADOLAN configuration
#' @return RADOLAN raster object
ReadRadolanBinary.read <- function(file.path,
                                   configuration) {

  #define end of header position ("03")
  header.end <- regexpr("\003", readLines(file.path, 1, skipNul = TRUE, warn = FALSE))

  #open binary stream
  radolan.stream <- file(file.path, "rb")

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

  #return raster
  return(radolan.raster)

}
