library(raster)

#create configuration with RADOLAN
radolan.configuration <- list()

#append configurations
radolan.configuration[["SF"]] <- list("type" = "SF",
                              "ncol" = 900,
                              "nrow" = 900,
                              "extent" = extent(-523.4622, 376.5378, -4658.645, -3758.645),
                              "proj" = proj_radolan,
                              "precision" = 0.1,
                              "time.latest" = "
                                  latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                  latest <- strptime(paste(format(latest, \"%y%m%d%H\"), \"50\", sep=\"\"), \"%y%m%d%H%M\", \"UTC\")
                                  if(latest > as.POSIXlt(Sys.time(), \"UTC\"))
                                    latest <- latest - 3600
                                  return(latest)",
                              "time.interval" = 3600,
                              "time.format" = "%y%m%d%H%M",
                              "file.pattern" = "raa01-sf_10000-%%time%%-dwd---bin")

#save configurations
devtools::use_data(radolan.configuration)
