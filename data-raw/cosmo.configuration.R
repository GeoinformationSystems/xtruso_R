#create configuration with RADOLAN
cosmo.configuration <- list()

cosmo.configuration.time.latest <- "latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                         latest$hour <- latest$hour %/% 3 * 3
                                         latest$min <- 0
                                         latest$sec <- 0
                                         return(latest)"
cosmo.configuration.time.interval <- 10800
cosmo.configuration.time.format <- "%Y%m%d%H"

cosmo.configuration.extent <- raster::extent(-0.01, 17.01, 42.99, 57.01)
cosmo.configuration.res.x <- 0.02
cosmo.configuration.res.y <- 0.02
cosmo.configuration.crs <- CRS("+proj=longlat +a=6371229 +b=6371229 +no_defs")

#append configurations
cosmo.configuration[["tot_prec"]] <- list("parameter" = "tot_prec",
                                          "file.pattern" = "cosmo-d2_germany_regular-lat-lon_single-level_%%time%%_%%prediction%%_TOT_PREC.grib2.bz2",
                                          "phenomenon" = "precipitation",
                                          "uom" = "mm/sqm",
                                          "time.latest" = cosmo.configuration.time.latest,
                                          "time.interval" = cosmo.configuration.time.interval,
                                          "time.format" = cosmo.configuration.time.format,
                                          "extent" = cosmo.configuration.extent,
                                          "proj" = cosmo.configuration.crs,
                                          "res.x" = cosmo.configuration.res.x,
                                          "res.y" = cosmo.configuration.res.y,
                                          "dwd.root" = "https://opendata.dwd.de/weather/nwp/cosmo-d2/grib/")

#save configurations
devtools::use_data(cosmo.configuration, overwrite=T)
devtools::use_data(cosmo.configuration.extent, overwrite=T)
devtools::use_data(cosmo.configuration.crs, overwrite=T)