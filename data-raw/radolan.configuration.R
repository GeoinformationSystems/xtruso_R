#create configuration with RADOLAN
radolan.configuration <- list()

#append configurations
radolan.configuration[["SF"]] <- list("type" = "SF",
                                      "ncol" = 900,
                                      "nrow" = 900,
                                      "extent" = raster::extent(-523.4622, 376.5378, -4658.645, -3758.645),
                                      "proj" = proj.radolan,
                                      "bits" = 2,
                                      "convert.to.dBZ" = FALSE,
                                      "precision" = 0.1,
                                      "max.value" = 409.5,
                                      "time.latest" = "
                                          latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                          latest <- strptime(paste(format(latest, \"%y%m%d%H\"), \"50\", sep=\"\"), \"%y%m%d%H%M\", \"UTC\")
                                          if(latest > as.POSIXlt(Sys.time(), \"UTC\"))
                                            latest <- latest - 3600
                                          return(latest)",
                                      "time.interval" = 3600,
                                      "time.format" = "%y%m%d%H%M",
                                      "file.pattern" = "raa01-sf_10000-%%time%%-dwd---bin")

radolan.configuration[["RW"]] <- list("type" = "RW",
                                      "ncol" = 900,
                                      "nrow" = 900,
                                      "extent" = raster::extent(-523.4622, 376.5378, -4658.645, -3758.645),
                                      "proj" = proj.radolan,
                                      "bits" = 2,
                                      "convert.to.dBZ" = FALSE,
                                      "precision" = 0.1,
                                      "max.value" = 409.5,
                                      "time.latest" = "
                                          latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                          latest <- strptime(paste(format(latest, \"%y%m%d%H\"), \"50\", sep=\"\"), \"%y%m%d%H%M\", \"UTC\")
                                          if(latest > as.POSIXlt(Sys.time(), \"UTC\"))
                                          latest <- latest - 3600
                                          return(latest)",
                                      "time.interval" = 3600,
                                      "time.format" = "%y%m%d%H%M",
                                      "file.pattern" = "raa01-rw_10000-%%time%%-dwd---bin")

radolan.configuration[["RX"]] <- list("type" = "RX",
                                      "ncol" = 900,
                                      "nrow" = 900,
                                      "extent" = raster::extent(-523.4622, 376.5378, -4658.645, -3758.645),
                                      "proj" = proj.radolan,
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
                                      "time.format" = "%y%m%d%H%M",
                                      "file.pattern" = "raa01-rx_10000-%%time%%-dwd---bin")
radolan.configuration[["FX"]] <- list("type" = "FX",
                                      "ncol" = 900,
                                      "nrow" = 900,
                                      "extent" = raster::extent(-523.4622, 376.5378, -4658.645, -3758.645),
                                      "proj" = proj.radolan,
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
                                      "time.format" = "%y%m%d%H%M",
                                      "file.pattern" = "FX%%time%%_%%prediction%%_MF002")

#save configurations
devtools::use_data(radolan.configuration, overwrite=T)
