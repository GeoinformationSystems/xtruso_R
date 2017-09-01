#create configuration with RADOLAN
cosmo.configuration <- list()

cosmo.configuration.latest <- "latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                         latest$hour <- latest$hour %/% 3 * 3
                                         latest$min <- 0
                                         latest$sec <- 0
                                         return(latest)"
cosmo.configuration.interval <- 10800
cosmo.configuration.tFormat <- "%Y%m%d%H"

#append configurations
cosmo.configuration[["tot_prec"]] <- list("parameter" = "tot_prec",
                                          "file.pattern" = "COSMODE_single_level_elements_TOT_PREC_%%time%%_%%prediction%%.grib2.bz2",
                                          "time.latest" = cosmo.configuration.latest,
                                          "time.interval" = cosmo.configuration.interval,
                                          "time.format" = cosmo.configuration.tFormat
                                          )

#save configurations
devtools::use_data(cosmo.configuration, overwrite=T)
