#' Get BROOK90 model environment
#'
#' @param envir start environment
#' @param params list with individual BROOK90 parameters to be redefined
#' 
x.brook90.getEnvironment <- function(envir = NA, 
                                     params = list()) {
  
  # create new environment for BROOK90
  if(is.na(envir)) {
    envir = new.env()
    for(n in ls(xtruso::brook90.environment, all.names=TRUE)) assign(n, get(n, xtruso::brook90.environment), envir)
    source("./data-raw/brook90.utility.R", local=envir)
  }
  
  # redefine environment parameters
  for(param in names(params)){
    if(param %in% names(envir)) envir[[param]] <- params[[param]] else warning(paste("Unknown parameter:", param, "(is not set)"))
  }
  
  return(envir)
}


#' Execute BROOK90 model
#'
#' @param envir BROOK90 model environment
#' @param df.meteoFile meteorological data required for BROOK90
#' @param df.precFile precipitation data required for BROOK90
#' 
x.brook90.run <- function(envir,
                          df.meteoFile, 
                          df.precFile) {
  
  # set measurement input
  envir$MData <- matrix(df.meteoFile)
  envir$MhhData <- matrix(df.precFile)
  
  # execute model within provided model environment
  envir$execute()

}


#' Sample plot for an executed BROOK90 model
#'
#' @param envir BROOK90 model environment (post-run)
#' @param filename plot image file (if NA, the main device is used)
#' 
x.brook90.plot <- function(envir,
                         filename = NA) {
  
  if(!(all(c("swatt","precc","evpp") %in% names(envir))))
    stop("Environment does not contain swatt, precc and/or evpp")
  
  if(!(is.na(path))) png(filename=path, width=1200, height=1200)
  
  plot(1:envir$NDAYS, envir$swatt[1:envir$NDAYS], ylim=c(0,max(envir$swatt)), col="red", type="l", lwd=3, xlab="Tage [d]", ylab="Werte [mm/d]")
  points(1:envir$NDAYS, envir$precc, col="darkblue", lwd=3)
  lines(1:envir$NDAYS, envir$evpp, col="green", lwd=3)
  
  if(!(is.na(path))) dev.off()
  
}


#' Execute BROOK90 model based on catchment parameters
#'
#' @param c.param catchment parameter set
#' @param df.meteoFile meteorological measurement input
#' @param df.precFile precipitation input
#' @param parallel logical: enable parallel execution with foreach
#' @param ts.results list of result parameters from BROOK90 model
#' @param weighted.avg logical: return only weighted average for single parameter combinations based on their area
#' 
x.brook90.run.catchment <- function(c.param, 
                                    df.meteoFile,
                                    df.precFile,
                                    parallel = FALSE,
                                    ts.results = c("swatt"),
                                    weighted.avg = TRUE) {
  
  # init catchment parameters
  params = list()
  params$LAT <- c.param$latitude * pi / 180 # lat in radians
  params$DTIMAX <- 0.5 # 2 iterations per day (value significantly impacts runtime)
  params$ESLOPE <- c.param$slope_mean * pi / 180 # slope in radians
  params$ESLOPED <- c.param$slope_mean # slope in degree
  #params$SUBDAYDATA <- TRUE
  #params$NPINT <- 24
  
  # init soil moisture data frame
  soilmoist <- data.frame("date"=as.Date(paste(df.meteoFile$year, df.meteoFile$month, df.meteoFile$day, sep="-")))
  
  # iterate catchment parameter set and compute single soil moisture values for each combination
  if (parallel && "doParallel" %in% installed.packages()[, "Package"]) {
    
    require(doParallel, quietly = TRUE)
    
    #init parallel environment
    cl <- makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)
    
    #extract values for each polygon
    soilmoist.tmp <- foreach::foreach(i=1:nrow(c.param$characteristics), .combine=cbind, .packages="xtruso") %dopar% {
      
      # execute model
      df <- data.frame(x = x.brook90.run.catchment.sub(params=params, 
                                                       clc=c.param$characteristics[i, ]$CLC_Class,
                                                       bk50=c.param$characteristics[i, ]$BK50_Legende,
                                                       flow="topdown",
                                                       df.meteoFile=df.meteoFile, 
                                                       df.precFile=df.precFile,
                                                       ts.results=ts.results))
      names(df) <- paste0(ts.results,i)
      
      # return soil moisture ts
      return(df)
    }
    
    #stop cluster
    parallel::stopCluster(cl)
    soilmoist <- cbind(soilmoist, soilmoist.tmp)
    
    #sequential execution in a for loop
  } else {
    
    #extract values for each polygon
    for(i in 1:nrow(c.param$characteristics)) {
      
      # execute model
      ts <- x.brook90.run.catchment.sub(params=params, 
                                        clc=c.param$characteristics[i, ]$CLC_Class,
                                        bk50=c.param$characteristics[i, ]$BK50_Legende,
                                        flow="topdown",
                                        df.meteoFile=df.meteoFile, 
                                        df.precFile=df.precFile,
                                        ts.results=ts.results)
      
      # append result for timestamp
      soilmoist[paste0(ts.results,i)] <- ts
      
    }
  }
  
  # get weighted average based on total area
  weights <- c.param$characteristics$area_sqkm / sum(c.param$characteristics$area_sqkm)
  for(ts.result in ts.results){
    soilmoist[paste0(ts.result,"_avg")] <- apply(soilmoist[grep(ts.result, names(soilmoist))], 1, FUN = function(x) { sum(x * weights) })
  }
  
  if(weighted.avg) return(soilmoist[, c("date", paste0(ts.results,"_avg"))]) else return(soilmoist)
}


#' subroutine for x.brook90.run.catchment - computes single soil moisture for single combination of landcover, soil and flow
#' 
#' @param params list with individual BROOK90 parameters to be redefined
#' @param c.param catchment characteristics
#' @param flow flow type
#' @param df.meteoFile meteorological input 
#' @param df.precFile precipitation input
#' @param ts.results list of result parameters from BROOK90 model
#' @return soil moisture timeseries
#' 
x.brook90.run.catchment.sub <- function(params=params,
                                        clc,
                                        bk50,
                                        flow,
                                        df.meteoFile,
                                        df.precFile,
                                        ts.results = c("swatt")) {
  
  # init model environment
  params <- x.brook90.param.landcover(params=params, lcover=clc)
  params <- x.brook90.param.soil(params=params, soiltype=bk50)
  params <- x.brook90.param.flow(params=params, flow=flow)
  envir <- x.brook90.getEnvironment(params=params)
  
  # run BROOK90 model
  x.brook90.run(envir, df.meteoFile, df.precFile)
  
  # return soil moisture ts
  return(mget(ts.results, envir))
  
}


#' Sample plot of a BROOK90 model result
#'
#' @param envir BROOK90 model environment
x.brook90.plot <- function(envir){
  
  plot(1:envir$NDAYS, envir$swatt[1:envir$NDAYS], col="red", type="l", lwd=3, xlab="Tage [d]", ylab="Werte [mm/d]") #,xlim=c(IDAY,NDAYS+1))
  
  PrecBound <- 10 #mm
  #points(IDAY,PTRAN,col="black",pch="?")
  points(which(envir$precc %in% envir$precc[envir$precc > PrecBound]), envir$precc[envir$precc > PrecBound], col="blue", pch="*")
  #line(IDAY,EVAPD,col="green",pch="*")
  #	points(IDAY,TA,col="red",pch="?")
  #points(IDAY,ATR[1],col="red",pch="?")
  #points(IDAY,GER[1],col="green",pch="?")
  #points(IDAY,PTR[1],col="black",pch="?")
  lines(1:envir$NDAYS, envir$evpp, col="blue", lwd=3)
  lines(1:envir$NDAYS, envir$mesfld, col="darkgreen", lwd=2)
  lines(1:envir$NDAYS, envir$floww, col="darkgreen", lwd=3)
  #	points(IDAY,RNET,col="brown",pch="?")
  
}


#' Get catchment parameters required for BROOK90 run
#' 
#' @param catchment input catchment
#' @return data.frame with parameters
#' 
x.brook90.params <- function(catchment) {
  
  params <- list()
  
  # get catchment parameters on soil and land cover
  df.stats <- xtruso::xtruso.catchments.stat.b90
  df.stats <- df.stats[df.stats$GKZ == catchment$GKZ, ]
  params[["characteristics"]] <- df.stats
  
  # check for stats
  if(nrow(df.stats) == 0)
    stop(paste0("No catchment stats available for GKZ ", catchment$GKZ))
  
  # check area sum to warn for non-fully-covered catchment
  if(abs(catchment$Area_sqkm - sum(df.stats$area_sqkm)) > 0.1)
    warning(paste("catchment area for", catchment$GKZ, "is not fully covered"))
  
  # get lcover with "Others"
  params[["clc_other"]] <- df.stats$CLC_Class %in% c("Others")
    
  # get soil with no info
  params[["soil_noinfo"]] <- !(df.stats$BK50_Legende %in% xtruso::xtruso.catchments.soil.b90$Legende)
  
  # get height stats
  dgm.stats <- xtruso::xtruso.catchments.stat.dgm
  params[["height_mean"]] <- dgm.stats[dgm.stats$GKZ == catchment$GKZ, "MEAN"] / 100
  params[["height_min"]] <- dgm.stats[dgm.stats$GKZ == catchment$GKZ, "MIN"] / 100
  params[["height_max"]] <- dgm.stats[dgm.stats$GKZ == catchment$GKZ, "MAX"] / 100
  params[["slope_mean"]] <- dgm.stats[dgm.stats$GKZ == catchment$GKZ, "MEAN.SLOPE"]
  
  # get latitude
  params[["latitude"]] <- mean(catchment@bbox["y",])
  
  return(params)
  
}


#' Get daily measurements required for BROOK90 run
#' 
#' @param catchment input catchment
#' @param c.height height of the catchment
#' @param osw.stations OSW stations point dataframe
#' @param osw.phenomenon reuqested phenomenon
#' @param osw.ts OSW timeseries cache
#' @param osw.url OSW API Url
#' @param osw.network OSW network
#' @param t.start start date for measurements
#' @param t.end end date for measurements
#' @param max.radius max search radius in km
#' @param max.num max number of stations
#' @param max.t max timestamp
#' @param max.deltaH maximum height difference between stations
#' @param intermediate logical: provide intermediate information (stations, raw measurements)
#' @return list with stations and corresponding measurements
#' 
x.brook90.measurements <- function(catchment,
                                 c.height,
                                 osw.stations,
                                 osw.phenomenon,
                                 osw.cache,
                                 osw.url,
                                 osw.network,
                                 t.start,
                                 t.end,
                                 max.radius = c(50,200,1000),
                                 max.num = 10,
                                 max.deltaH = NA,
                                 intermediate = F) {
  
  # get phenomenon id
  p.id <- gsub(" ", ".", osw.phenomenon)
  
  #get closest stations for phenomen
  for(r in max.radius) {
    osw.closest <- x.osw.closest(osw.stations, osw.url, osw.phenomenon, catchment, r, max.num, t.start, t.end, c.height, max.deltaH)
    if(length(osw.closest) > 0) break
  }
  if(length(osw.closest) == 0) {
    warning(paste("Could not find stations for", osw.phenomenon, "in", catchment$GKZ))
    return(list(measurements.day.combined = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("date", paste0(p.id, c(".mean",".max",".min"))))))
  }
  
  #set station index
  osw.closest$index <- 1:nrow(osw.closest)
  
  #get measurements
  measurements <- list()
  for(i in osw.closest$index) {
    osw.id <- paste0(osw.closest[i, ]$networkCode, osw.closest[i, ]$deviceCode, osw.closest[i, ]$sensorCode)
    if(!(osw.id %in% names(osw.cache))) osw.cache[[osw.id]] <- x.osw.get(osw.url, osw.closest[i, ]$networkCode, osw.closest[i, ]$deviceCode, osw.closest[i, ]$sensorCode, t.start, t.end)
    m <- osw.cache[[osw.id]]
    if(nrow(m) > 0)
      measurements[[paste0("s.", i)]] <- m
  }
  
  #aggregate by day (min, max, mean)
  measurements.day <- list()
  dist = c()
  for(i in osw.closest$index) {
    index <- paste0("s.", i)
    if(index %in% names(measurements)){
      measurements.day[[index]] <- as.data.frame(as.list(stats::aggregate(measurements[[index]]$v, list(as.Date(measurements[[index]]$begin)), FUN=function(x) c(mean=mean(x, na.rm=T), max=max(x, na.rm=T), min=min(x, na.rm=T)))))
      names(measurements.day[[index]]) <- c("date", paste0("mean.",index), paste0("max.",index), paste0("min.",index))
      dist <- c(dist, osw.closest[i,]$dist)
    }
  }
  
  #combine measurements in a dataframe
  measurements.day.combined <- Reduce(function(df1, df2) merge(df1, df2, by="date", all.x=TRUE, suffixes=c(1,2)), measurements.day)
  
  # set inverse weights
  weights.inv <- 1 / dist ^ 2
  
  # compute weighted mean
  measurements.day.combined[[paste0(p.id,".mean")]] <- apply(measurements.day.combined[, grep("mean.", names(measurements.day.combined)), drop=FALSE], 1, function(x){
    sum(x * weights.inv, na.rm=T) / sum(weights.inv[!is.na(x)])
  })
  
  # add min and max measurements
  measurements.day.combined[[paste0(p.id,".max")]] <- rowMeans(measurements.day.combined[, grep("max.", names(measurements.day.combined)), drop=FALSE], na.rm=T)
  measurements.day.combined[[paste0(p.id,".min")]] <- rowMeans(measurements.day.combined[, grep("min.", names(measurements.day.combined)), drop=FALSE], na.rm=T)
  
  # filter date, mean, min and max
  measurements.day.combined <- measurements.day.combined[, c("date", paste0(p.id, c(".mean",".max",".min")))]
  
  #compile and return measurements
  if(intermediate) return(list(stations = osw.closest,
              measurements = measurements,
              measurements.day = measurements.day,
              measurements.day.combined = measurements.day.combined,
              osw.cache = osw.cache))
  return(measurements.day.combined)
  
}


#' Get land cover parameters
#'
#' @param params list with BROOK90 parameters
#' @param landcover land cover ("Coniferous Forest", "Grass Land", "Cultivated" or "Deciduous Forest")
#' @return updated parameter list
#' 
x.brook90.param.landcover <- function(params = list(),
                                      lcover) {
  
  if(missing(lcover))
    stop("Missing landcover information.")
  
  if(!(lcover %in% c("Coniferous Forest", "Grass Land", "Cultivated", "Deciduous Forest")))
    stop(paste("Unrecognized landcover:", lcover, "(must be \"Coniferous Forest\", \"Grass Land\", \"Cultivated\" or \"Deciduous Forest\")"))
  
  # set land cover parameters
  if(lcover == "Coniferous Forest") {
    params$RELHT <- c(1,1,366,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    params$RELLAI <- c(1,1,366,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    params$ALB <- 0.14
    params$ALBSN <- 0.14
    params$KSNVP <- 0.3
    params$Z0G <- 0.02
    params$MAXHT <- 25 
    params$MAXLAI <- 6 
    params$MXRTLN <- 3100 
    params$MXKPL <- 8 
    params$FXYLEM <- 0.5
    params$GLMAX <- 0.53 
    params$LWIDTH <- 0.004 
    params$CR <- 0.5
    params$ROOTDEN <- c(100,.22,100,.17,100,.13,100,.1,100,.08,100,.06,100,.05,100,.04,100,.03,100,.02,100,.02,100,.01,100,.01,100,.01,100,.01,100,.01,100,.01,100,.01,100,.01,100,0,100,0,100,0,100,0,100,0,100,0)
    
  } else if(lcover == "Grass Land") {
    params$RELHT <- c(1,.1,115,.1,145,1,268,1,298,1,366,.1,0,0,0,0,0,0,0,0)
    params$RELLAI <- c(1,0,115,0,145,1,268,1,298,0,366,0,0,0,0,0,0,0,0,0)
    params$ALB <- 0.2
    params$ALBSN <- 0.5
    params$KSNVP <- 1
    params$Z0G <- 0.01
    params$MAXHT <- .5 
    params$MAXLAI <- 3 
    params$MXRTLN <- 1000 
    params$MXKPL <- 8 
    params$FXYLEM <- 0
    params$GLMAX <- 0.8 
    params$LWIDTH <- 0.01 
    params$CR <- 0.7
    params$ROOTDEN <- c(100,.44,100,.25,100,.14,100,.08,100,.04,100,.02,100,.01,100,.01,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0)
    
  } else if(lcover == "Cultivated") {
    params$RELHT <- c(1,0,100,0,213,1,278,1,308,0,366,0,0,0,0,0,0,0,0,0)
    params$RELLAI <- c(1,0,100,0,213,1,278,1,308,0,366,0,0,0,0,0,0,0,0,0)
    params$ALB <- 0.22
    params$ALBSN <- 0.5
    params$KSNVP <- 1
    params$Z0G <- 0.005
    params$MAXHT <- .3 
    params$MAXLAI <- 3 
    params$MXRTLN <- 110 
    params$MXKPL <- 8 
    params$FXYLEM <- 0
    params$GLMAX <- 1.1 
    params$LWIDTH <- .1 
    params$CR <- 0.7
    params$ROOTDEN <- c(100,.34,100,.22,100,.15,100,.1,100,.07,100,.04,100,.03,100,.02,100,.1,100,.1,100,.1,100,.1,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0)
    
  } else if(lcover == "Deciduous Forest") {
    params$RELHT <- c(1,1,366,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    params$RELLAI <- c(1,0,54,0,84,1,299,1,329,0,366,0,0,0,0,0,0,0,0,0)
    params$ALB <- 0.18
    params$ALBSN <- 0.23
    params$KSNVP <- 0.3
    params$Z0G <- 0.02
    params$MAXHT <- 25 
    params$MAXLAI <- 6 
    params$MXRTLN <- 3000 
    params$MXKPL <- 8 
    params$FXYLEM <- 0.5
    params$GLMAX <- 0.53 
    params$LWIDTH <- 0.1 
    params$CR <- 0.6
    params$ROOTDEN <- c(100,.22,100,.17,100,.13,100,.1,100,.08,100,.06,100,.05,100,.04,100,.03,100,.02,100,.02,100,.01,100,.01,100,.01,100,.01,100,.01,100,.01,100,.01,100,.01,100,0,100,0,100,0,100,0,100,0,100,0)
  }

  return(params)
  
}


#' Get soil parameters
#'
#' @param params list with BROOK90 parameters
#' @param soil soil type information; triple with type ("Cl", "ClLo", "Lo", "LoSa", "Other", "Sa", "SaClLo", "SaLo", "SiClLo" or "SiLo"), thickness (in mm) and stone fraction (0..1)
#' @param nlayer number of modelled layers (<= 25)
#' @return updated parameter list
#' 
x.brook90.param.soil <- function(params = list(),
                                 soiltype,
                                 nLayer = 25) {
 
  if(missing(soiltype))
    stop("Missing soil information.")
  
  if(!(soiltype %in% xtruso::xtruso.catchments.soil.b90$Legende))
    stop(paste("unrecognized soil type:", soiltype, "(must correspond to BK50 legend number 1 to 1150)"))
  
  # get soiltype specification
  soil <- xtruso::xtruso.catchments.soil.b90[xtruso::xtruso.catchments.soil.b90$Legende == soiltype, ]
  
  # set number of layers
  params$NLAYER <- nrow(soil)
  
  # init layer parameters
  params$THICK <- rep(0, nLayer)
  params$STONEF <- rep(0, nLayer)
  params$PSIF <- rep(0, nLayer)
  params$THETAF <- rep(0, nLayer)
  params$THSAT <- rep(0, nLayer)
  params$BEXP <- rep(0, nLayer)
  params$KF <- rep(0, nLayer)
  params$WETINF <- rep(0, nLayer)
  
  # set specified parameters for each layer
  for(layer.nr in soil$layer.Nr){
    params$THICK[layer.nr] <- soil[soil$layer.Nr == layer.nr, "THICK"]
    params$STONEF[layer.nr] <- soil[soil$layer.Nr == layer.nr, "STONEF"]
    params$PSIF[layer.nr] <- soil[soil$layer.Nr == layer.nr, "PSIF"]
    params$THETAF[layer.nr] <- soil[soil$layer.Nr == layer.nr, "THETAF"]
    params$THSAT[layer.nr] <- soil[soil$layer.Nr == layer.nr, "THSAT"]
    params$BEXP[layer.nr] <- soil[soil$layer.Nr == layer.nr, "BEXP"]
    params$KF[layer.nr] <- soil[soil$layer.Nr == layer.nr, "KF"]
    params$WETINF[layer.nr] <- soil[soil$layer.Nr == layer.nr, "WETINF"]
  }
  
  return(params)
   
}


#' Get soil parameters
#'
#' @param params list with BROOK90 parameters
#' @param flow flow type ("topdown" or "uni500")
#' @return updated parameter list
#' 
x.brook90.param.flow <- function(params = list(),
                               flow) {
  
  if(missing(flow))
    stop("Missing flow information.")
  
  if(!(flow %in% c("topdown", "uni500")))
    stop(paste("unrecognized flow:", flow, "(must be \"topdown\" or \"uni500\")"))
  
  # set flow parameters
  if(flow == "topdown") {
    params$IDEPTH <- 0
    params$INFEXP <- 0
    params$IMPERV <- 0
    params$BYPAR <- 0
    params$QDEPTH <- 0
    params$QFPAR <- 0
    params$QFFC <- 0
    params$DRAIN <- 1
    params$GSC <- 0
    params$GSP <- 0
    
  } else if(flow == "uni500") {
    params$IDEPTH <- 500
    params$INFEXP <- 1
    params$IMPERV <- 0
    params$BYPAR <- 0
    params$QDEPTH <- 0
    params$QFPAR <- 0
    params$QFFC <- 0
    params$DRAIN <- 1
    params$GSC <- 0
    params$GSP <- 0
  }
  
  return(params)
  
}


#' compute avg vapor pressure using the Magnus formula
#' @param t_mean mean temperature for the day
#' @param rh_mean mean relative humidity
#' @return average vapur pressure
#' 
x.brook90.vaporPressure <- function(t_mean, 
                                    rh_mean) {
  
  return (611.2 * exp( (17.62 * t_mean) / (t_mean + 243.12) ) * rh_mean * 0.001)
  
}