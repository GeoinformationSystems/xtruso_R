#' Calculate Zonal Overlap for raster based on input polygons
#'
#' @param raster.name raster dataset
#' @param polygons polygons overlayed on input raster
#' @param polygons.id if set, this field is used to identify the polygons, otherwise the index in polygons is used
#' @param parallel flag: use parallel computation
#' @return dataframe with overlap information (id, name, cell, x, y, weight)
#' @export
ZonalOverlap.getWeightedOverlap <- function(raster,
                                            polygons,
                                            polygons.id = NA,
                                            parallel = TRUE) {

  if(missing(raster))
    stop("Need to specify input raster.")

  if(missing(polygons))
    stop("Need to specify input polygon(s).")

  if(class(polygons) != "SpatialPolygonsDataFrame")
    stop("function requires polygons being instanceof 'SpatialPolygonsDataFrame'")

  #transfor polygons, if CRS do not match (do not transform raster to preserve raster cell values)
  if(sp::proj4string(raster) != sp::proj4string(polygons))
    polygons <- sp::spTransform(polygons, sp::proj4string(raster))

  #create target dataframe
  df.overlap <- data.frame(index=integer(),
                           name=character(),
                           cell=integer(),
                           x=integer(),
                           y=integer(),
                           weight=numeric())

  #parallel execution with foreach
  if (parallel && "doParallel" %in% installed.packages()[, "Package"]) {

    require(doParallel, quietly = TRUE)

    #init parallel environment
    cl <- makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)

    #calculate raster overlap
    df.overlap.tmp <- foreach::foreach(i=1:nrow(polygons), .combine=rbind) %dopar% {

      #get polygon id
      polygon.id <- if(is.na(polygons.id)) i else as.character(polygons[i,][[polygons.id]])

      #get ovelap weight for each overlapping cell
      z <- raster::extract(raster, polygons[i,], small=T, weights=T, cellnumbers=T, normalizeWeights=T)

      #append cell information to dataframe
      cell <- z[[1]][,"cell"]
      return(data.frame(i, polygon.id, cell, Utility.getRADOLANIndexFromCell(cell), z[[1]][,"weight"]))

    }

    df.overlap <- rbind(df.overlap, setNames(df.overlap.tmp, names(df.overlap)))

    #stop cluster
    parallel::stopCluster(cl)

    #sequential execution in a for loop
  } else {

    #calculate raster overlap

    for(i in 1:nrow(polygons)) {

      #get polygon id
      polygon.id <- if(is.na(polygons.id)) i else as.character(polygons[i,][[polygons.id]])

      #get ovelap weight for each overlapping cell
      z <- raster::extract(raster, polygons[i,], small=T, weights=T, cellnumbers=T, normalizeWeights=T)

      #append cell information to dataframe
      cell <- z[[1]][,"cell"]
      df.overlap <- rbind(df.overlap, setNames(data.frame(polygon.id, cell, Utility.getRADOLANIndexFromCell(cell), z[[1]][,"weight"]), names(df.overlap)))
    }

  }

  #return dataframe with overlap
  return(df.overlap)

}


#' Get Zonal Overlap from previously calculated overlap dataframe
#'
#' @param df.overlap overlap dataframe
#' @param index polygon index or indices
#' @param name polygon name(s); if set, the index is ignored
#' @return dataframe with overlap information (id, name, cell, x, y, weight)
#' @export
ZonalOverlap.getPolygonOverlap <- function(df.overlap,
                                           index,
                                           name = NA) {

  if(missing(df.overlap) || !all(c("index","name") %in% names(overlap)))
    stop("Need to specify valid input dataframe")

  if(missing(index) && is.na(name))
    stop("Need to specify index or name of target polygon(s)")

  #check for names
  if(!is.na(name))
    return(df.overlap[df.overlap$name %in% name, ])

  #check for index
  return(df.overlap[df.overlap$index %in% index, ])

}


#' Get mean value for provided raster based on Zonal Overlap
#'
#' @param raster input raster or raster stack
#' @param df.overlap overlap dataframe
#' @param statisitcs statistics to compute
#' @param parallel flag: use parallel computation
#' @return
#' @export
ZonalOverlap.geStatistics <- function(radolan.raster,
                                      df.overlap,
                                      parallel = TRUE) {

  if(missing(radolan.raster))
    stop("Need to specify input RADOLAN raster or raster stack")

  if(missing(df.overlap) || !all(c("index","name") %in% names(df.overlap)))
    stop("Need to specify valid input dataframe")

  #get unique indices
  indices <- unique(df.overlap$index)

  #init target dataframe
  df.stats <- data.frame(index = integer(),
                         name = character(),
                         timestamp = as.POSIXct(character()),
                         stat = character(),
                         value = numeric())

  #parallel execution with foreach
  if (parallel && "doParallel" %in% installed.packages()[, "Package"]) {

    require(doParallel, quietly = TRUE)

    #init parallel environment
    cl <- makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)

    #evaluation function
    for(i in 1:nlayers(radolan.raster)) {

      #calculate and append stats for current raster
      raster.i <- radolan.raster[[i]]
      df.overlap.tmp <- foreach::foreach(j=1:nrow(df.overlap), .combine=rbind) %dopar% {

      }
      df.stats <- rbind(df.stats, setNames(df.stats.tmp, names(df.stats)))

    }

    #stop cluster
    parallel::stopCluster(cl)

    #sequential execution with apply
  } else {

    #evaluation function
    for(i in 1:nlayers(radolan.raster)) {
      for(index in indices) {

        #get subset for index
        overlap <- ZonalOverlap.getPolygonOverlap(df.overlap, index)

        #calculate and append stats for current raster
        raster.i <- radolan.raster[[i]]
        min <- 999
        max <- -999
        mean <- 0
        for(j in 1:nrow(overlap)) {
          v <- raster.i[overlap[j, "cell"]]
          min <- min(min, v)
          max <- max(max, v)
          mean <- mean + overlap[j, "weight"] * v
        }

        df.stats <- rbind(df.stats, setNames(data.frame(index, overlap$name[1], as.POSIXct(attr(raster.i, "timestamp")), "min", min), names(df.stats)))
        df.stats <- rbind(df.stats, setNames(data.frame(index, overlap$name[1], as.POSIXct(attr(raster.i, "timestamp")), "max", max), names(df.stats)))
        df.stats <- rbind(df.stats, setNames(data.frame(index, overlap$name[1], as.POSIXct(attr(raster.i, "timestamp")), "mean", mean), names(df.stats)))

      }
    }

  }

  return(df.stats)

}
