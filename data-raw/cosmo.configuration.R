#create configuration with RADOLAN
cosmo.configuration <- list()

cosmo.configuration.time.latest <- "latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                         latest$hour <- latest$hour %/% 3 * 3
                                         latest$min <- 0
                                         latest$sec <- 0
                                         return(latest)"
cosmo.configuration.time.interval <- 10800
cosmo.configuration.time.format <- "%Y%m%d%H"

#append configurations
cosmo.configuration[["tot_prec"]] <- list("parameter" = "tot_prec",
                                          "file.pattern" = "COSMODE_single_level_elements_TOT_PREC_%%time%%_%%prediction%%.grib2.bz2",
                                          "time.latest" = cosmo.configuration.time.latest,
                                          "time.interval" = cosmo.configuration.time.interval,
                                          "time.format" = cosmo.configuration.time.format)

#save configurations
devtools::use_data(cosmo.configuration, overwrite=T)
