#' Disaggregate RADOLAN RW product by RX Reflectivity values
#'
#' @param path.rw path to RW product to be disaggregated
#' @param path.rx path to RX product for disaggregation
#' @param missing.rx number of missing RX files allowed for disaggregation
#' @param missing.rx.seq number of missing RX files in sequence allowed for disaggregation
#' @param parallel flag: enable parallel computation with foreach
#' @return disaggregated RW products, one raster stack with 12 layers for each RW image
#' @export
DisaggregateRadolanRW <- function(path.rw,
                                  path.rx,
                                  path.rw.disaggregated,
                                  missing.rx = 1,
                                  missing.rx.seq = 1,
                                  interpolation.rx = "linear",
                                  parallel = TRUE) {

  #sequence for RX files (values subtracted from RW timestamp)
  seq.rx <- seq(0, 55, by=5)
  names.rx <- sprintf("file.rx.%d", seq.rx)
  times.rx <- sprintf("timestamp.rx.%d", seq.rx)

  if(missing(path.rw))
    stop("Need to specify path to RW products.")

  if(missing(path.rx))
    stop("Need to specify path to RX products.")

  if(missing.rx > 1)
    stop("More than one missing RX value per RW is currently not supported.")

  if(missing.rx.seq > 1)
    stop("More than one missing RX value in sequence per RW is currently not supported.")

  if(!(interpolation.rx %in% c("linear")))
    stop(paste("RX interpolation", interpolation.rx, "is not supported.", sep=" "))

  #get list of all RW files
  fileList.rw <- base::list.files(path.rw, full.names=T)

  #get timestamps for each RW file
  timestamps.rw <- unlist(lapply(fileList.rw, function(path){
    DisaggregateRadolanRW.getTimestamp(path, FALSE)
  }))

  #build dataframe from timestamp and RW path
  df.rw <- data.frame("timestamp.rw" = timestamps.rw, "file.rw" = fileList.rw, stringsAsFactors = F)

  #get RX timestamps and path associated with each RW timestamp
  for(i in seq.rx){
    #set RX timestamps
    t.index <- paste("timestamp.rx", i, sep=".")
    t.path <- paste("file.rx", i, sep=".")
    df.rw[t.index] <- format(as.POSIXct(df.rw[,"timestamp.rw"], format="%y%m%d%H%M", tz="UTC") - (i * 60), format="%y%m%d%H%M")
    #set path to corresponding RX file
    df.rw[t.path] <- apply(df.rw, 1, function(row) {
      paste0(path.rx, "/raa01-rx_10000-", row[t.index], "-dwd---bin")
    })
    #check, if RX files exists, set NA, if file is not present
    df.rw[base::file.exists(df.rw[, t.path]) == FALSE, t.path] <- NA
  }

  #assess possibility for disaggregation based on missing.rx and missing.rx.seq; interpolate RX data, if required
  df.rw["disaggregate"] <- apply(df.rw[, names.rx], 1, function(row) {
    #determine NA in file list (indicates missing file)
    missing <- is.na(row)
    #return TRUE if no file is missing
    if(!any(missing))
      return(TRUE)
    #check for total missing files
    if(sum(missing) > missing.rx)
      return(FALSE)
    #check sequence of na = TRUE with run length encoding
    missing.rle <- rle(c(missing))
    if(tapply(missing.rle$length, missing.rle$values, max)["TRUE"] > missing.rx.seq)
      return(FALSE)
    #return true, is neither missing.rx nor missing.rx.seq are exceeded
    return(TRUE)
  })

  #check for output path
  if(!base::file.exists(path.rw.disaggregated))
    dir.create(file.path(path.rw.disaggregated), showWarnings = FALSE)

  #parallel disaggregate RW image using weights from RX images
  if (parallel && "doSNOW" %in% installed.packages()[, "Package"]) {

    require(doSNOW, quietly = TRUE)

    #init parallel environment
    cl <- snow::makeCluster(as.integer(Sys.getenv("NUMBER_OF_PROCESSORS")) - 1)
    doSNOW::registerDoSNOW(cl)

    #run disaggregation for each row
    foreach::foreach(i = 1:nrow(df.rw), .export=c("path.rx", "times.rx", "names.rx", "seq.rx", "df.rw", "path.rw.disaggregated"), .packages=c("raster")) %dopar% {
      if(df.rw[i, "disaggregate"] == TRUE)
        DisaggregateRadolanRW.disaggregate(path.rx, setNames(as.character(df.rw[i, ]), names(df.rw)), path.rw.disaggregated, times.rx, names.rx, seq.rx)
    }

    #stop cluster
    snow::stopCluster(cl)

  #sequential disaggregate RW image using weights from RX images
  } else {

    apply(df.rw[df.rw$disaggregate == TRUE, ], 1, function(row){
      DisaggregateRadolanRW.disaggregate(row, path.rw.disaggregated)
    })

  }

}

#' Interpolate RX image by computing mean bewteen previous and next image
#'
#' @param path.rx path to RX products
#' @param timestamp.rx timestamp to be interpolated
#' @return interpolated RADOLAN raster object
#' @export
DisaggregateRadolanRW.interpolate.linear <- function(path.rx,
                                                     timestamp.rx) {

  #set timestamp
  if(typeof(timestamp.rx) == "character")
    timestamp.rx <- as.POSIXct(timestamp.rx, format="%y%m%d%H%M", tz="UTC")

  #check, if file does not exist (should ideally be checked before calling the function)
  file.rx <- paste0(path.rx, "/raa01-rx_10000-", format(timestamp.rx,  format="%y%m%d%H%M"), "-dwd---bin")
  if(base::file.exists(file.rx))
    return(ReadRadolanBinary(file.rx, "RX"))

  #get previous and next RX file
  file.rx.previous <- paste0(path.rx, "/raa01-rx_10000-", format(timestamp.rx - 300,  format="%y%m%d%H%M"), "-dwd---bin")
  file.rx.next <- paste0(path.rx, "/raa01-rx_10000-", format(timestamp.rx + 300,  format="%y%m%d%H%M"), "-dwd---bin")
  #return NA, if either previous or next RX image is missing
  if(!base::file.exists(file.rx.previous) || !base::file.exists(file.rx.next))
    return(NA)

  #read images
  raster.rx.previous <- ReadRadolanBinary(file.rx.previous, "RX")
  raster.rx.next <- ReadRadolanBinary(file.rx.next, "RX")

  #compute linear interpolation between previous and next RX image (mean)
  raster.rx <- mean(raster.rx.previous, raster.rx.next)
  return(raster.rx)

}

#' read stack of RADOLAN RX products
#'
#' @param row.rx RX files with timestamp and path
#' @param interpolate flag: interpolate 5min steps
#' @return RADOLAN raster stack
DisaggregateRadolanRW.readRXStack <- function(path.rx, row, seq.rx, interpolate=TRUE){

  #init stack
  raster.rx.stack <- stack()
  #get 5min layers
  for(i in seq.rx){
    file.rx <- row[paste("file.rx", i, sep=".")]
    if(is.na(file.rx)){
      #interpolate
      if(interpolate){
        raster.rx <- DisaggregateRadolanRW.interpolate.linear(path.rx, row[paste("timestamp.rx", i, sep=".")])
        if(is.na(raster.rx)){
          warning(paste("Could not interpolate RX with timestamp",row[paste("timestamp.rx", i, sep=".")]))
          return(NA)
        }
      }
      else {
        warning(paste("Interpolation disabled for RX timestamp",row[paste("timestamp.rx", i, sep=".")]))
        return(NA)
      }
    }
    else
      raster.rx <- ReadRadolanBinary(file.rx, "RX")
    #add to stack
    names(raster.rx) <- paste0("RX_", DisaggregateRadolanRW.getTimestamp(file.rx, FALSE))
    raster.rx.stack <- addLayer(raster.rx.stack, raster.rx)
  }
  return(raster.rx.stack)

}

#' get timestamp from RADOLAN file name
#'
#' @param file.name name of the file
#' @param as.POSIXct return time object
#' @return timestamp
DisaggregateRadolanRW.getTimestamp <- function(file.name, as.POSIXct=T) {

  time.chr <- unlist(strsplit(basename(file.name),"-"))[3]
  if(as.POSIXct)
    return(as.POSIXct(time.chr, format="%y%m%d%H%M", tz="UTC"))
  else
    return(time.chr)

}

#' disaggregation process, writes output to folder
#'
#' @param rw.row name of the file
DisaggregateRadolanRW.disaggregate <- function(path.rx, row, path.rw.disaggregated, times.rx, names.rx, seq.rx) {

    #read RW product
    raster.rw <- ReadRadolanBinary(row["file.rw"], "RW")
    #read RX products
    raster.rx.stack <- DisaggregateRadolanRW.readRXStack(path.rx, row[c(times.rx,names.rx)], seq.rx)
    if(length(raster.rx.stack) == 1)
      return()
    #get weights for each RX layer in stack
    raster.rx.stack.sum <- sum(raster.rx.stack)
    raster.rx.names <- names(raster.rx.stack)
    for(i in 1:nlayers(raster.rx.stack)){
      #get weights by division with stack sum
      raster.rx.stack[[i]] <- raster.rx.stack[[i]] / raster.rx.stack.sum
      #set NaN to 0 (where stack sum is 0)
      raster.rx.stack[[i]][is.nan(raster.rx.stack[[i]])] <- 0
    }
    names(raster.rx.stack) <- raster.rx.names
    #init RW stack and populate with weited RW raster
    raster.rw.stack <- stack()
    for(i in 1:nlayers(raster.rx.stack)){
      raster.rw.stack <- raster::addLayer(raster.rw.stack, raster.rw * raster.rx.stack[[i]])
    }
    names(raster.rw.stack) <- paste0("RW_", raster.rx.names)
    #write RW stack to output folder
    for(i in 1:nlayers(raster.rw.stack)){
      raster::writeRaster(raster.rw.stack[[i]], filename=paste0(path.rw.disaggregated, "/", names(raster.rw.stack[[i]]), ".tif"), format="GTiff", overwrite=TRUE, options=c("COMPRESS=DEFLATE"))
    }

}
