library(raster)

#create configuration with RADOLAN
radolan.configuration <- list()

radolan.configuration.extent900 <- raster::extent(-523.4622, 376.5378, -4658.645, -3758.645)
radolan.configuration.crs <- CRS("+proj=stere +lat_0=90 +lat_ts=90 +lon_0=10 +k=0.93301270189 +x_0=0 +y_0=0 +a=6370040 +b=6370040 +to_meter=1000 +no_defs")

radolan.configuration.time.format = "%y%m%d%H%M"

#append configurations
radolan.configuration[["SF"]] <- list("type" = "SF",
                                      "ncol" = 900,
                                      "nrow" = 900,
                                      "extent" = radolan.configuration.extent900,
                                      "proj" = radolan.configuration.crs,
                                      "bits" = 2,
                                      "convert.to.dBZ" = FALSE,
                                      "precision" = 0.1,
                                      "max.value" = 4095,
                                      "time.latest" = "
                                          latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                          latest <- strptime(paste(format(latest, \"%y%m%d%H\"), \"50\", sep=\"\"), \"%y%m%d%H%M\", \"UTC\")
                                          if(latest > as.POSIXlt(Sys.time(), \"UTC\"))
                                            latest <- latest - 3600
                                          return(latest)",
                                      "time.interval" = 3600,
                                      "time.format" = radolan.configuration.time.format,
                                      "file.pattern" = "raa01-sf_10000-%%time%%-dwd---bin")

radolan.configuration[["RW"]] <- list("type" = "RW",
                                      "ncol" = 900,
                                      "nrow" = 900,
                                      "extent" = radolan.configuration.extent900,
                                      "proj" = radolan.configuration.crs,
                                      "bits" = 2,
                                      "convert.to.dBZ" = FALSE,
                                      "precision" = 0.1,
                                      "max.value" = 4095,
                                      "time.latest" = "
                                          latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                          latest <- strptime(paste(format(latest, \"%y%m%d%H\"), \"50\", sep=\"\"), \"%y%m%d%H%M\", \"UTC\")
                                          if(latest > as.POSIXlt(Sys.time(), \"UTC\"))
                                          latest <- latest - 3600
                                          return(latest)",
                                      "time.interval" = 3600,
                                      "time.format" = radolan.configuration.time.format,
                                      "file.pattern" = "raa01-rw_10000-%%time%%-dwd---bin")

radolan.configuration[["RX"]] <- list("type" = "RX",
                                      "ncol" = 900,
                                      "nrow" = 900,
                                      "extent" = radolan.configuration.extent900,
                                      "proj" = radolan.configuration.crs,
                                      "bits" = 1,
                                      "convert.to.dBZ" = TRUE,
                                      "precision" = 1,
                                      "max.value" = 248,
                                      "time.latest" = "
                                          latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                          latest$min <- latest$min %/% 5 * 5
                                          latest$sec <- 0
                                          return(latest)",
                                      "time.interval" = 300,
                                      "time.format" = radolan.configuration.time.format,
                                      "file.pattern" = "raa01-rx_10000-%%time%%-dwd---bin")

radolan.configuration[["FX"]] <- list("type" = "FX",
                                      "ncol" = 900,
                                      "nrow" = 900,
                                      "extent" = radolan.configuration.extent900,
                                      "proj" = radolan.configuration.crs,
                                      "bits" = 2,
                                      "convert.to.dBZ" = TRUE,
                                      "precision" = 0.1,
                                      "max.value" = 4095,
                                      "time.latest" = "
                                          latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                          latest$min <- latest$min %/% 5 * 5
                                          latest$sec <- 0
                                          return(latest)",
                                      "time.interval" = 300,
                                      "time.format" = radolan.configuration.time.format,
                                      "file.pattern" = "FX%%time%%_%%prediction%%_MF002")

#save configurations
devtools::use_data(radolan.configuration, overwrite=T)
devtools::use_data(radolan.configuration.extent900, overwrite=T)
devtools::use_data(radolan.configuration.crs, overwrite=T)
