#' get default BROOK90 model environment
#'
BROOK90.getEnvironment <- function(vars = list()){
  return(xtruso::brook90.environment)
}


#' reset BROOK90 model parameters
#'
#' @param params list with BROOK90 variables to be redefined
BROOK90.resetParams <- function(envir, params = list()) {
  for(param in names(params)){
    envir[[param]] <- params[[param]]
  }
}


#' Execute BROOK90 model
#'
#' @param envir BROOK90 model environment
#' @param df.meteoFile meteorological data required for BROOK90
#' @param df.precFile precipitation data required for BROOK90
BROOK90.run <- function(envir = xtruso::brook90.environment, 
                        df.meteoFile, 
                        df.precFile) {
  
  #set measurement input
  envir$MData <- matrix(meteoFile)
  envir$MhhData <- matrix(precFile)
  #execute model
  envir$execute()
}


#' Sample plot of a BROOK90 model result
#'
#' @param envir BROOK90 model environment
BROOK90.plot <- function(envir){
  
  plot(1:envir$NDAYS, envir$floww[1:envir$NDAYS], col="red", type="l", lwd=3, ylim=c(0.0, 70), xlab="Tage [d]", ylab="Werte [mm/d]") #,xlim=c(IDAY,NDAYS+1))
  
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


#' Sample BROOK90 parameter changeset for the Wernersbach catchment
#'
#' @return BROOK90 parameters to be redefined
BROOK90.getParams.Wernersbach <- function() {
  
  ML <- xtruso::brook90.environment$ML
  
  #Wernersbach changeset
  changeset <- list()
  changeset$ALB <- 0.07 # * albedo with no snow
  changeset$ALBSN <- 0.3 # * albedo with snow on the ground
  BEXP<-rep(0, ML) #* exponent for psi-theta relation
  BEXP[1] <- 5.37433
  BEXP[2] <- 4.03320
  BEXP[3] <- 5.64096
  BEXP[4] <- 6
  BEXP[5] <- 5
  changeset$BEXP <- BEXP
  changeset$BYPAR <- 1 #* 1 to allow BYFL, or 0 to prevent BYFL
  changeset$CR <- 0.5 #* light extinction coefficient for projected LAI + SAI
  changeset$DENSEF <- 0.8580571 #* density or thinning multiplier for MAXLAI,CS,RTLEN,RPLANT, not <.001
  changeset$DRAIN <- 0.626259 #* multiplier of VFLUX(n) for drainage to groundwater
  changeset$DTI <- 0 #time step for iteration interval, d
  changeset$DTIMAX <- 0.5 #* maximum iteration time step, d
  changeset$DTINEW <- 0.0 #second estimate of DTI
  changeset$DURATN <- c(4,4,4,4,4,4,4,4,4,4,4,4) #* average duration of daily precip by month, hr
  changeset$ESLOPED <- 2 #* slope for evapotranspiration and snowmelt, degrees
  changeset$GLMAXC <- 0.494377 # * maximum leaf conductance, cm/s
  changeset$GSP <- 0.085 #* fraction of discharge to seepage
  changeset$GWATIN <- 20 #**** initial groundwater storage below soil layers, mm
  changeset$mnuhalfiter <- FALSE
  changeset$IDEPTH <- 1000 #* depth over which infiltration is distributed
  changeset$IMPERV <- 0.025 #* impervious fraction of area for SRFL
  changeset$INFEXP <- 0.8797636 #* infiltration exponent, 0-all to top to 1-uniform with depth
  changeset$IRVPY <- 0 #evaporation of intercepted rain, mm
  KF <- rep(0, ML) #* hydraulic conductivity at field capacity, mm/d
  KF[1] <- 6.9
  KF[2] <- 2.7
  KF[3] <- 2.9
  KF[4] <- 1
  KF[5] <- 3.5
  changeset$KF <- KF
  changeset$KSNVP <- 0.009734 #* multiplier to fix snow evaporation problem
  changeset$LATD <- 50.5 #**** latitude, degrees
  changeset$LWIDTH <- 0.004 #* leaf width, m
  changeset$MAXLAI <- 7.693270 #* maximum projected leaf area index for the year, m2/m2
  changeset$MELFAC <- 1.728930 #* degree day melt factor for open, MJ m-2 d-1 K-1
  changeset$MXKPL <- 7.03463 #* maximum plant conductivity, (mm/d)/MPa
  changeset$MXRTLN <- 3000.001 #* maximum root length per unit land area, m/m2
  changeset$NLAYER <- 5 #* number of soil layers to be used in model, <= ML
  PSIF <- rep(0, ML) #* matric potential at field capacity, kPa
  PSIF[1] <- -11.818
  PSIF[2] <- -11.516 
  PSIF[3] <- -10.22
  PSIF[4] <- -10
  PSIF[5] <- -10
  changeset$PSIF <- PSIF
  changeset$PSIMIN <- rep(-10, ML) # initial PSIM()
  changeset$QFFC <- 0.00104 #  0.00104  #* quick flow fraction (SRFL or BYFL) at field capacity
  changeset$QFPAR <- 0.834524 #0.834524    #* quick flow parameter (SRFL or BYFL)
  changeset$QDEPTH <- 0.1 #* soil depth for SRFL calculation, 0 to prevent SRFL
  changeset$RELHT <- c(1,1,366,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #* ten pairs of DOY and relative canopy height
  changeset$RELLAI <- c(1,1,366,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #* ten pairs of DOY and relative LAI
  changeset$ROOTDEN <- c(50,1,50,1,50,1,50,1,50,1,50,.3,50,.2,50,.1,50,.1,50,.1,50,0.0,50,0.0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0,50,0) #* 25 pairs of root layer thickness (mm) and relative root density per unit volume
  changeset$RRD <- 0.55
  changeset$RSTEMP <- -1.29978 #* base temperature for snow-rain transition, °C
  changeset$SNOWIN <- 20 #**** initial water equivalent of snow on the ground, mm
  STONEF<-rep(0.00, ML) #* stone volume fraction, unitless
  STONEF[1] <- 0.02
  STONEF[2] <- 0.2
  STONEF[3] <- 0.25
  STONEF[4] <- 0.25
  STONEF[5] <- 0.7
  changeset$STONEF <- STONEF
  changeset$TH <- 40 #* temperature above which stomates are closed, degC
  THETAF<-rep(0, ML) #* volumetric water content at field capacity
  THETAF[1] <- 0.34062341
  THETAF[2] <- 0.39705807  
  THETAF[3] <- 0.24359704
  THETAF[4] <- 0.35
  THETAF[5] <- 0.23
  changeset$THETAF <- THETAF
  THICK<-rep(0, ML) #* layer thicknesses, mm
  THICK[1] <- 50
  THICK[2] <- 250
  THICK[3] <- 300
  THICK[4] <- 300
  THICK[5] <- 100
  changeset$THICK <- THICK
  THSAT <- rep(0, ML) #* theta at saturation, matrix porosity
  THSAT[1] <- 0.6313342
  THSAT[2] <- 0.6189386
  THSAT[3] <- 0.4930716
  THSAT[4] <- 0.680
  THSAT[5] <- 0.600
  changeset$THSAT <- THSAT
  WETINF <- rep(0, ML) #* wetness at dry end of near-saturation range
  WETINF[1] <- 0.92
  WETINF[2] <- 0.92
  WETINF[3] <- 0.92
  WETINF[4] <- 0.92
  WETINF[5] <- 0.92
  changeset$WETINF <- WETINF
  changeset$Z0W <- 2.8 #* weather station roughness parameter, m
  changeset$ZW <- 14 #* weather station measurement height for wind, m"
  
  return(changeset)
}