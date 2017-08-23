#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param path path to the RADOLAN SF binary input file
#' @return RADOLAN raster object
#' @export
radolan_readBinarySF <- function(path) {
  #read binary format with default settings for RADOLAN SF data
  radolan_sf <- radolan_readBinary(path, 900, 900, extent_radolan)
  #remove flagged data by setting them to NA
  radolan_sf[radolan_sf > 4095] <- NA
  #set negative values to NA
  radolan_sf[radolan_sf < 0] <- NA
  #divide by 10, to get mm/24h
  radolan_sf <- radolan_sf / 10
  return(radolan_sf)
}
