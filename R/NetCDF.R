#'
#' rechunk a NetCDF file
#' @param ncdf.source source NetCDF file
#' @param ncdf.target target NetCDF file
#' @param chunksizes target chunk size
#' @param compression target compression
#' @export
#' 
x.ncdf.rechunk <- function(ncdf.source,
                           ncdf.target,
                           chunksizes,
                           compression = NA) {
  
  if(missing(ncdf.source) || !file.exists(ncdf.source))
    stop("Need to specify a valid NetCDF file.")
  
  if(missing(chunksizes) || length(chunksizes) != 3)
    stop("Need to specify valid target chunksize (length 3).")
  
  #get source file
  ncdf.in <- x.ncdf.open(ncdf.source)
  
  #get source variable declaration
  ncdf.v <- ncdf.in$var[[1]]
  
  #reset chunksize and compression
  ncdf.v$chunksizes = chunksizes
  ncdf.v$compression = compression
  
  #create output file
  ncdf.out <- x.ncdf.create(ncdf.target, ncdf.v)
  
  #get source attributes
  attributes <- list()
  attributes[["0"]] <- ncdf4::ncatt_get(ncdf.in, 0)
  for(dim in ncdf.in$dim) {
    attributes[[dim$name]] <- ncdf4::ncatt_get(ncdf.in, dim$name)
  }
  
  #write target attributes
  for(var.id in names(attributes)){
    for(att in names(attributes[[var.id]])) {
      #write attributes to target
      x.ncdf.attribute.write(ncdf.out, if(var.id == "0") 0 else var.id, att, attributes[[var.id]][[att]])
    }
  }
  
  #transfer rasters
  tryCatch({
    
    #iterate files, write NetCDF
    for(i in 1:ncdf.in$dim$t$len){
      
      #get raster for current index
      raster <- raster::raster(ncdf4::ncvar_get(ncdf.in, start=c(1,1,i), count=c(-1,-1,1)))
      
      #get timestamp for current index
      t.value <- ncdf.in$dim$t$vals[i]
      
      #write to target NetCDF
      if(!is.null(raster))
        x.ncdf.write <- function(ncdf.out, ncdf.v, raster, t.value, t.index=i)
          x.ncdf.write(ncdf.out, ncdf.v, raster, t.index=i)
      else
        message(paste0("File for t:", t," is NULL and was not written to NetCDF"))
      
    }
    
  }, error = function(err) { message(err,"\n")
  }, finally = {
    #close file
    x.ncdf.close(ncdf.in)
    x.ncdf.close(ncdf.out)
  })
  
}


#'
#' write raster to NetCDF file
#' @param ncdf.file NetCDF file pointer
#' @param ncdf.v NetCDF variable
#' @param raster raster to write
#' @param t.value timestamp(s) for t dimension
#' @param t.index index for timestamp within NetCDF t dimension
#' @param xy flag: axis order is xy (transposed from row-col order)
#' @export
#' 
x.ncdf.write <- function(ncdf,
                         ncdf.v,
                         raster,
                         t.value,
                         t.index = 1) {
  
  if(missing(ncdf))
    stop("Need to specify a NetCDF file.")
  
  if(missing(raster))
    stop("Need to specify a raster or raster stack to write.")
  
  #get raster array, flip and transpose to move bottom-left to top-left corner (rotation 90° clockwise)
  matrix <- raster::as.matrix(raster::t(raster::flip(raster, "y")))
  
  #add timestamp as variable
  ncdf4::ncvar_put(ncdf, "t", t.value, start=t.index, count=1)
  
  #add radolan matrix for selected timestamp
  ncdf4::ncvar_put(ncdf, ncdf.v, matrix, start=c(1,1,t.index), count=c(-1,-1,1))
  
}


#'
#' create NetCDF file for given NetCDF variable
#' @param ncdf.file NetCDF file path
#' @param ncdf.v NetCDF variable definition
#' @param NetCDF file pointer
#' @export
#' 
x.ncdf.create <- function(ncdf.file,
                          ncdf.v) {
  
  if(!"ncdf4" %in% installed.packages()[, "Package"])
    stop("Package ncdf4 is not installed.")
  
  if(missing(ncdf.file))
    stop("Need to specify a NetCDF file path.")
  
  if(missing(ncdf.v))
    stop("Need to specify a NetCDF variable.")
  
  return(ncdf4::nc_create(ncdf.file, ncdf.v, force_v4=TRUE))
  
}


#'
#' open NetCDF file
#' @param ncdf.file NetCDF file
#' @param write flag: NetCDF file is writable
#' @export
#' 
x.ncdf.open <- function(ncdf.file, write=F) {
  
  if(!"ncdf4" %in% installed.packages()[, "Package"])
    stop("Package ncdf4 is not installed.")
  
  if(missing(ncdf.file))
    stop("Need to specify a NetCDF file.")
  
  if(class(ncdf.file) == "ncdf4")
    ncdf.file <- ncdf.file$filename
  
  if(!file.exists(ncdf.file))
    stop("Need to specify an existing NetCDF file.")
  
  return(ncdf4::nc_open(ncdf.file, write))
  
}


#'
#' close NetCDF file
#' @param ncdf.file NetCDF file
#' @export
#' 
x.ncdf.close <- function(ncdf) {
  
  if(missing(ncdf))
    stop("Need to specify a NetCDF file pointer.")
  
  ncdf4::nc_close(ncdf)
  
}


#'
#' Write attribute to NetCDF file
#' @param ncdf NetCDF file pointer
#' @param var.id variable id(s) to write attributes (0 = global)
#' @param name attribute name(s)
#' @param value attribute value(s)
#' @export
#' 
x.ncdf.attribute.write <- function(ncdf,
                                   var.id = 0,
                                   name,
                                   value) {
  
  if(missing(ncdf))
    stop("Need to specify a NetCDF file.")
  
  #write attribute to ncdf
  ncdf4::ncatt_put(ncdf, var.id, name, value)
  
}


#'
#' create NetCDF dimension
#' @param name dimension name
#' @param uom dimension mmeasurement unit
#' @param values dimension values
#' @param unlimited flag: unlimited dimension
#' @param dimvar flag: create dimensional variable 
#' @return NetCDF variable for RADOLAN products
#' @export
#' 
x.ncdf.dim <- function(name,
                       uom,
                       values,
                       unlimited = FALSE,
                       dimvar = TRUE) {
  
  ncdf.dim <- ncdf4::ncdim_def(name, uom, values, unlimited, dimvar)
  return(ncdf.dim)
  
}



#'
#' create NetCDF Variable with x, y, time and value
#' @param v.phen value phenomenon
#' @param v.uom value phenomenon uom
#' @param x.dim ncdf4 dimension definition for x
#' @param y.dim ncdf4 dimension definition for y
#' @param t.dim ncdf4 dimension definition for t
#' @param chunksizes NetCDF chunksize (vector of length 3 for x,y,t)
#' @param compression NetCDF compression level (NA, 1:9)
#' @return NetCDF variable for RADOLAN products
#' @export
#' 
x.ncdf.var.xytv <- function(v.phen,
                            v.uom,
                            ncdf.x,
                            ncdf.y,
                            ncdf.t,
                            chunksizes = NA,
                            compression = NA) {
  
  if(missing(v.phen))
    stop("Need to specify a phenomenon.")
  
  if(missing(v.uom))
    stop("Need to specify a unit of measurement for v.")
  
  if(!is.na(compression) && !(compression %in% 1:9))
    stop("Need to specify a compression between 1 and 9, or NA.")
  
  #create variable declaration
  ncdf.v <- ncdf4::ncvar_def(v.phen, v.uom, list(ncdf.x, ncdf.y, ncdf.t), NA, chunksizes=chunksizes, compression=compression)
  
  return(ncdf.v)
  
}


#'
#' request a subset from NetCDF file(s) with x,y,t,v variable
#'
#' @param ncdf.files list of NetCDF files to be requetsed for subset
#' @param extent spatial extent or polygon for requesting the subset; if missing, the whole spatial extent is requested
#' @param timestamp timestamp or interval with 2 timestmaps for t.start and t.end; if missing, the whole temporal extent is requested
#' @param ncdf.phen phenomenon to be requested fron NetCDF
#' @param statistics flag: calculate statistics along temporal dimension
#' @param as.raster flag: return raster or stack; ignored, if statistics = T
#' @return NetCDF value subset
#' @export
#' 
x.ncdf.subset <- function(ncdf,
                          extent = -1,
                          timestamp = -1,
                          ncdf.phen = ncdf$var[[1]]$name,
                          statistics = F,
                          as.raster = F) {
  
  if(missing(ncdf))
    stop("Need to specify valid NetCDF file pointer.")
  
  if(isS4(extent) && !any(c("Extent", "SpatialPolygons", "SpatialPolygonsDataFrame") %in% class(extent)))
    stop("Need to specify valid extent object (Entent, Polygon) or -1.")
  
  #return subset array, if no statistics are requested
  if(!statistics)
    return(x.ncdf.subset.xytv(ncdf, extent, timestamp, as.raster=as.raster))
  
  #return statistics for polygon mask
  if(any(c("SpatialPolygons", "SpatialPolygonsDataFrame") %in% class(extent)))
    return(x.ncdf.subset.mask(ncdf, extent, timestamp))
  
  #return statistics for specified extent
  return(x.ncdf.subset.extent(ncdf, extent, timestamp))
  
}


#'
#' request a subset from NetCDF file with x,y,t,v variable
#'
#' @param ncdf NetCDF file pointer
#' @param extent extent, -1 returns whole extent
#' @param timestamp timestamp(s) to be requested, -1 returns all timestamps
#' @param extent.indices flag: extent coordinates are provided as NetCDF indices
#' @param extent.all flag: request whole spatial dimension
#' @param t.index flag: t is provided as index; if false, timestamp is selected from NetCDF file
#' @param t.all flag: request whole temporal dimension
#' @param ncdf.phen phenomenon to be requested
#' @param as.raster flag: return raster or raster stack instead of matrix
#' @return NetCDF value subset
#' 
x.ncdf.subset.xytv <- function(ncdf,
                               extent = -1,
                               timestamp = -1,
                               extent.indices = F,
                               t.index = F,
                               ncdf.phen = ncdf$var[[1]]$name,
                               as.raster = F) {
  
  #init start and count
  start <- c(1, 1, 1)
  count <- c(-1, -1, -1)
  
  #reset start and count for extent, if -1 (== not an S4 object)
  if(isS4(extent)){
    
    #get extent (xmin,xmax,ymin,ymax)
    if(!extent.indices)
      extent <- x.ncdf.extent(ncdf, extent)
    
    #set start and count
    start[c(1,2)] <- extent[c(1,3)]
    count[c(1,2)] <- c(extent[2] - extent[1] + 1, extent[4] - extent[3] + 1)
  
  } else {
    
    #get max extent
    extent <- x.ncdf.extent(ncdf, -1)
    
  } 
  
  #reset start and count for timestamp, if not -1
  if(timestamp[1] != -1){
  
    #get index for timestamp(s)
    if(!t.index)
      timestamp <- x.ncdf.timestamps(ncdf, timestamp, t.range=T, t.closest=T)
    
    #set start and count for timestamp
    start[3] <- min(timestamp)
    count[3] <- length(timestamp)
  
  }
  
  #get subset from NetCDF file
  subset <- ncdf4::ncvar_get(ncdf, ncdf.phen, start=start, count=count, collapse_degen=FALSE)
  
  #first transpose then flip subset (horizontally) to get top-left from bottom-left corner
  subset <- aperm(subset, c(2,1,3))
  subset <- apply(subset, c(2,3), rev)
  
  #set dimension names (xy coordinates and timestamps)
  dimnames(subset)[[2]] <- as.list(ncdf$dim$x$vals[extent[1]:extent[2]])
  dimnames(subset)[[1]] <- as.list(ncdf$dim$y$vals[extent[4]:extent[3]])
  dimnames(subset)[[3]] <- as.list(x.ncdf.timestamps(ncdf, timestamp, inverse=T))
  
  #return matrix, if as.raster = F
  if(!as.raster)
    return(subset)
  
  #get real extent, timestamp and projection; prevents repeated calls to ncdf within for loop
  timestamp <- x.ncdf.timestamps(ncdf, timestamp, inverse = T)
  extent <- x.ncdf.extent(ncdf, extent, inverse = T)
  proj <- ncdf4::ncatt_get(ncdf, 0)[["proj4_params"]]
  if(!is.null(proj)) proj <- sp::CRS(proj)
  
  #init stack and iterate over third dimension (t)
  stack <- raster::stack()
  for(t in 1:length(timestamp)) {
    
    #convert to raster
    raster <- raster::raster(subset[,,t])
    
    #set proper spatial extent
    raster <- raster::setExtent(raster, extent)
    
    #check for projection from global proj4_params attribute
    if(!is.null(proj)) raster::projection(raster) <- proj
    
    #set timestamp
    attr(raster, "timestamp") <- as.POSIXct(timestamp[t], origin="1970-01-01", tz="UTC")
    
    #add raster to stack
    stack <- raster::addLayer(stack, raster)
  }
  
  if(raster::nlayers(stack) == 1) return(stack[[1]])
  else return(stack)
  
}


#' 
#' Calculate statistics for NetCDF timeseries based on given spatial extent
#'
#' @param ncdf NetCDF file pointer
#' @param extent target extent (xmin,xmax,ymin,ymax) or polygon mask
#' @param timestamp timestamp(s) to be requested
#' @param extent.indices flag: extent is provided as NetCDF indices
#' @param extent.all flag: request wholespatial extent
#' @param t.index flag: t.start and t.end are provided as NetCDF indices
#' @param t.all flag: request whole temporal extent, t.start and t.end are ignored
#' @param ncdf.phen target NetCDF phenomenon
#' @param fun named functions to be applied on requested value subset, applied along temporal dimension
#' @return NetCDF subset matrix or dataframe with function results, if fun != NA
#' 
x.ncdf.subset.extent <- function(ncdf,
                                 extent = -1,
                                 timestamp = -1,
                                 extent.indices = F,
                                 t.index = F,
                                 ncdf.phen = ncdf$var[[1]]$name,
                                 fun = list(mean=mean, median=median, min=min, max=max, sum=sum, sd=sd)) {
  
  #request NetCDF subset
  subset <- x.ncdf.subset.xytv(ncdf, extent, timestamp, extent.indices, t.index, ncdf.phen, as.raster=F)
  
  #init dataframe with timestamps
  subset.df <- data.frame(timestamp=dimnames(subset)[[3]])
  
  #calculate statistics along temporal dimension based on provided functions
  for(f in names(fun)){
    subset.df[f] <- apply(subset, 3, fun[[f]], na.rm=T)
  }
  return(subset.df)
  
}


#' 
#' Calculate statistics for NetCDF timeseries based on polygon mask
#'
#' @param ncdf NetCDF file pointer
#' @param polygon polygon mask
#' @param timestamp timestamp(s) to be requested
#' @param t.index flag: t.start and t.end are provided as NetCDF indices
#' @param t.all flag: request whole temporal extent
#' @param ncdf.phen target NetCDF phenomenon
#' @param fun named functions to be applied on requested value subset, applied along temporal dimension
#' @return NetCDF subset matrix or dataframe with function results, if fun != NA
#' 
x.ncdf.subset.mask <- function(ncdf,
                               polygon,
                               timestamp = -1,
                               t.index = F,
                               ncdf.phen = ncdf$var[[1]]$name) {
  
  #get projection from NetCDF
  proj <- ncdf4::ncatt_get(ncdf, 0)[["proj4_params"]]
  if(!is.null(proj)) proj <- sp::CRS(proj) else stop("NetCDF must contain a valid projection parameter 'proj4_params'.")
  
  #reproject polygon to NetCDF projection
  polygon <- sp::spTransform(polygon, proj)
  
  #get extent from polygon
  extent <- raster::extent(polygon)
  extent.ncdf <- x.ncdf.extent(ncdf, extent)
  extent <- x.ncdf.extent(ncdf, extent.ncdf, inverse=T)
  
  #request NetCDF subset
  subset <- x.ncdf.subset.xytv(ncdf, extent.ncdf, timestamp, extent.indices=T, t.index=t.index, ncdf.phen=ncdf.phen, as.raster=F)
  
  #init raster for zonal overlap computation
  raster <- raster::raster(subset[,,1])
  raster <- raster::setExtent(raster, extent)
  raster::projection(raster) <- proj
  
  #determine zonal overlap for polygon and raster
  df.overlap <- x.zonal.overlap(raster, polygon, parallel=F)
  
  #initialize weights, x,y order to match cell number iteration in raster::extract
  weights = array(0, dim=dim(raster)[c(2,1)])
  
  #set weights based on overlay
  for(i in 1:nrow(df.overlap)){
    weights[df.overlap[i, "cell"]] <- df.overlap[i, "weight"]
  }
  
  #transpose weights to get row,col order
  weights <- t(weights)
  
  #get total area of polygon in map units
  area <- sum(df.overlap$size)
  
  #init functions based on polygon mask
  fun <- list(
    mean = function(x, weights) { return(sum(x * weights, na.rm=T)) },
    max = function(x, weights) { return(suppressWarnings(max(x[weights > 0], na.rm=T))) },
    min = function(x, weights) { return(suppressWarnings(min(x[weights > 0], na.rm=T))) }
  )
  
  #init dataframe with timestamps
  subset.df <- data.frame(timestamp=dimnames(subset)[[3]])
  
  #calculate statistics along temporal dimension based on provided functions
  for(f in names(fun)){
    subset.df[f] <- apply(subset, 3, fun[[f]], weights=weights)
  }
  
  #calculate sum by multiplying mean with area
  subset.df["sum"] <- subset.df$mean * area
  
  return(subset.df)
  
}


#' 
#' Request NetCDF coordinate indices for given spatial extent or object
#'
#' @param ncdf NetCDF file pointer
#' @param extent spatial extent or object, -1 return whole extent
#' @param inverse flag: determine coordinate extent based on indices
#' @return requested extent
#' 
x.ncdf.extent <- function(ncdf,
                          extent = -1,
                          inverse = F) {
  
  #get coordinates from NetCDF
  x <- ncdf$dim$x$vals
  y <- ncdf$dim$y$vals
  
  #get cell size (assumes homogeneous cell size)
  x.cell <- x[2] - x[1]
  y.cell <- y[2] - y[1]
  
  #get coordinate extent for indices
  if(inverse) {
    
    #set max extent, if extent = -1
    if(!isS4(extent))
      extent <- extent(1, length(x), 1, length(y))
    
    #determine target coordinates, add half cell size to move from cell center to corner
    xmin <- x[extent[1]] - x.cell/2
    xmax <- x[extent[2]] + x.cell/2
    ymin <- y[extent[3]] - y.cell/2
    ymax <- y[extent[4]] + y.cell/2
    
    return(raster::extent(xmin, xmax, ymin, ymax))
    
  }
  
  #set max extent, if extent = -1
  if(!isS4(extent))
    extent <- extent(min(x), max(x), min(y), max(y))
  
  #get extent
  if(!class(extent) %in% c("Extent"))
    extent <- raster::extent(extent)
  
  #get min and max x and y, looks for closest cell center within extent
  xmin <- which(x == x.utility.closest(x, extent@xmin - x.cell/2, direction="geq"))
  xmax <- which(x == x.utility.closest(x, extent@xmax + x.cell/2, direction="leq"))
  ymin <- which(y == x.utility.closest(y, extent@ymin - y.cell/2, direction="geq"))
  ymax <- which(y == x.utility.closest(y, extent@ymax + y.cell/2, direction="leq"))
  
  #return coordinate indices
  return(raster::extent(xmin, xmax, ymin, ymax))
  
}



#'
#' Request NetCDF index/indices for given timestamp(s)
#'
#' @param ncdf NetCDF file pointer
#' @param timestamp timestamp(s) to search for, -1 returns all timestamps
#' @param t.index flag: timestamp(s) represent index or indices in the NetCDF file
#' @param range flag: request a range of timestamps from min(t) to max(t)
#' @param closest flag: select closest values, if timestamps are not available; direction is both, except if range = T, which will look for leq(min(t)) and geq(max(t))
#' @param inverse flag: get timestamp(s) from index/indices
#' @return dataframe with timestamp(s) and corresponding index or indices in NetCDF file
#' 
x.ncdf.timestamps <- function(ncdf, 
                              timestamp = -1,
                              t.range = F,
                              t.closest = F,
                              inverse = F) {
  
  if(missing(ncdf))
    stop("Need to specify a NetCDF file.")
  
  if(missing(timestamp))
    stop("Need to specify timestamp.")
  
  #get all timestamps
  timestamps <- ncdf$dim$t$vals
  
  #return all timestamps, if timestamp = -1
  if(timestamp[1] == -1)
    return(if(inverse) timestamps else c(1:length(timestamps)))
  
  #get timestamps from indices
  if(inverse) {
    
    #return range of timestamps
    if(length(timestamp) > 0 && t.range)
      return(timestamps[min(timestamp) : max(timestamp)])
    
    #return timestamps matchin gthe input indices
    return(timestamps[timestamp])
    
  }
  
  #t.index = F; check for exact matches
  if(!t.closest) {
    
    #get exact matches
    t.index <- match(timestamp, timestamps)
    
    if(any(is.na(t.index)))
      warning("Some timestamps are not present in NetCDF.")
    
    #check for range
    if(t.range) {
      
      range <- min(t.index, na.rm=T) : max(t.index, na.rm=T)
      return(range)
      
    } else return(t.index)

  }
    
  #closest = T; check for range
  if(t.range){
    
    #get closest timestamp lower or equal to t.start or first item, if no value is leq
    t.min <- which(timestamps == x.utility.closest(timestamps, min(timestamp), direction="leq"))
    
    #get closest timestamp greater or equal to t.start or last item, if no value is geq
    t.max <- which(timestamps == x.utility.closest(timestamps, max(timestamp), direction="geq"))
    
    range <- t.min : t.max    
    return(range)
    
  } else {
    
    #get closest
    closest <- x.utility.closest(timestamps, timestamp)
    return(closest)
    
  }
}
