#' Read a RADOLAN binary raster
#'
#' This function reads a RADOLAN binary raster as provided by the DWD
#'
#' @param path path to the RADOLAN binary input file
#' @param nrows number of rows in the binary input file
#' @param ncols number of columns in the binary input file
#' @param extent spatial extent
#' @return RADOLAN raster object
#' @export
radolan_readBinary <- function(path, nrows, ncols, extent) {
  #define end of header position ("03")
  header_end <- regexpr("\003", readLines(path, 1, skipNul = TRUE))
  #open binary stream
  rb_stream <- file(path, "rb")
  #read header
  skip_temp <- readBin(rb_stream, "raw", n = header_end, endian = "little")
  #read integer data (note: includes flags described in RADOLAN spec)
  rb_data <- readBin(rb_stream, integer(), n = nrows*ncols*2, size = 2, endian = "little")
  #close connection
  close(rb_stream)
  #create raster with projection
  rb <- raster(t(matrix(rb_data, ncol = ncols, nrow = nrows)))
  #flip raster direction
  rb <- flip(rb, "y")
  #set extent and projection
  extent(rb) <- extent
  projection(rb) <- proj_radolan
  return(rb)
}
