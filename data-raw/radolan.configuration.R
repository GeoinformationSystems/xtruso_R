library(raster)

#create configuration for RADOLAN
radolan.configuration <- list()

radolan.configuration.extent900 <- raster::extent(-523.4622, 376.5378, -4658.645, -3758.645)
radolan.configuration.extent1100 <- raster::extent(-443.4622, 456.5378, -4758.645, -3658.645)
radolan.configuration.crs <- CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=10 +k=0.93301270189 +x_0=0 +y_0=0 +a=6370040 +b=6370040 +to_meter=1000 +no_defs")

radolan.configuration.time.format = "%y%m%d%H%M"

radolan.configuration.create = function(type,
                                        ncol = 900,
                                        nrow = 900,
                                        extent = radolan.configuration.extent900,
                                        bits,
                                        convert.to.dBZ,
                                        precision,
                                        max.value,
                                        time.firstOfYear,
                                        time.latest,
                                        time.interval,
                                        file.pattern,
                                        zip.pattern = NA,
                                        phenomenon,
                                        uom,
                                        dwd.root,
                                        col.map) {

  return(list("type" = type,
              "ncol" = ncol,
              "nrow" = nrow,
              "extent" = extent,
              "proj" = radolan.configuration.crs,
              "bits" = bits,
              "convert.to.dBZ" = convert.to.dBZ,
              "precision" = precision,
              "max.value" = max.value,
              "time.firstOfYear" = time.firstOfYear,
              "time.latest" = time.latest,
              "time.interval" = time.interval,
              "time.format" = radolan.configuration.time.format,
              "file.pattern" = file.pattern,
              "zip.pattern" = zip.pattern,
              "phenomenon" = phenomenon,
              "uom" = uom,
              "dwd.root" = dwd.root,
              "col.map" = col.map))

}

#append configurations
radolan.configuration[["SF"]] <- radolan.configuration.create(type = "SF",
                                                              bits = 2,
                                                              convert.to.dBZ = FALSE,
                                                              precision = 0.1,
                                                              max.value = 4095,
                                                              time.firstOfYear = "%%year%%-01-01 00:50:00Z",
                                                              time.latest = "
                                                                  latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                                                  latest <- strptime(paste(format(latest, \"%y%m%d%H\"), \"50\", sep=\"\"), \"%y%m%d%H%M\", \"UTC\")
                                                                  if(latest > as.POSIXlt(Sys.time(), \"UTC\")) latest <- latest - 3600
                                                                  return(latest)",
                                                              time.interval = 3600,
                                                              file.pattern = "raa01-sf_10000-%%time%%-dwd---bin",
                                                              phenomenon = "precipitation",
                                                              uom = "mm/24h",
                                                              dwd.root = "https://opendata.dwd.de/weather/radar/radolan/sf/",
                                                              col.map = data.frame(limit=c(), col=c(), stringsAsFactors = F))

radolan.configuration[["RW"]] <- radolan.configuration.create(type = "RW",
                                                              bits = 2,
                                                              convert.to.dBZ = FALSE,
                                                              precision = 0.1,
                                                              max.value = 4095,
                                                              time.firstOfYear = "%%year%%-01-01 00:50:00Z",
                                                              time.latest = "
                                                                  latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                                                  latest <- strptime(paste(format(latest, \"%y%m%d%H\"), \"50\", sep=\"\"), \"%y%m%d%H%M\", \"UTC\")
                                                                  if(latest > as.POSIXlt(Sys.time(), \"UTC\")) latest <- latest - 3600
                                                                  return(latest)",
                                                              time.interval = 3600,
                                                              file.pattern = "raa01-rw_10000-%%time%%-dwd---bin",
                                                              zip.pattern = "raa01-rw_10000-%%time%%-dwd---bin.gz",
                                                              phenomenon = "precipitation",
                                                              uom = "mm/h",
                                                              dwd.root = "https://opendata.dwd.de/weather/radar/radolan/rw/",
                                                              col.map = data.frame(limit=c(0.1,0.2,0.5,1,2,5,10,15,25,40,60,80,100), col=c("#fcffc1","#fbff5c","#dffc26","#a0d626","#45c379","#00d6d8","#11a1d6","#0702fc","#9232b7","#da28c6","#e70d0c","#880e0d","#4f0e0d"), stringsAsFactors = F))


radolan.configuration[["RX"]] <- radolan.configuration.create(type = "RX",
                                                              bits = 1,
                                                              convert.to.dBZ = TRUE,
                                                              precision = 1,
                                                              max.value = 248,
                                                              time.firstOfYear = "%%year%%-01-01 00:00:00Z",
                                                              time.latest = "
                                                                  latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                                                  latest$min <- latest$min %/% 5 * 5
                                                                  latest$sec <- 0
                                                                  return(latest)",
                                                              time.interval = 300,
                                                              file.pattern = "raa01-rx_10000-%%time%%-dwd---bin",
                                                              phenomenon = "reflectivity",
                                                              uom = "dbZ",
                                                              dwd.root = "https://opendata.dwd.de/weather/radar/composit/rx/",
                                                              col.map = data.frame(limit=c(), col=c(), stringsAsFactors = F))


radolan.configuration[["FX"]] <- radolan.configuration.create(type = "FX",
                                                              bits = 2,
                                                              convert.to.dBZ = TRUE,
                                                              precision = 0.1,
                                                              max.value = 4095,
                                                              time.firstOfYear = "%%year%%-01-01 00:00:00Z",
                                                              time.latest = "
                                                                  latest <- as.POSIXlt(Sys.time(), \"UTC\")
                                                                  latest$min <- latest$min %/% 5 * 5
                                                                  latest$sec <- 0
                                                                  return(latest)",
                                                              time.interval = 300,
                                                              file.pattern = "FX%%time%%_%%prediction%%_MF002",
                                                              zip.pattern = "FX%%time%%.tar.bz2",
                                                              phenomenon = "reflectivity",
                                                              uom = "dbZ",
                                                              dwd.root = "https://opendata.dwd.de/weather/radar/composit/fx/",
                                                              col.map = data.frame(limit=c(), col=c(), stringsAsFactors = F))

radolan.configuration[["YW"]] <- radolan.configuration.create(type = "YW",
                                                              ncol = 1100,
                                                              extent = radolan.configuration.extent1100,
                                                              bits = 2,
                                                              convert.to.dBZ = FALSE,
                                                              precision = 0.01,
                                                              max.value = 4095,
                                                              time.firstOfYear = "%%year%%-01-01 00:00:00Z",
                                                              time.latest = "",
                                                              time.interval = 300,
                                                              file.pattern = "raa01-yw2017.002_10000-%%time%%-dwd---bin",
                                                              phenomenon = "precipitation",
                                                              uom = "mm/5min",
                                                              dwd.root = "https://opendata.dwd.de/climate_environment/CDC/grids_germany/5_minutes/radolan/reproc/2017_002/bin/",
                                                              col.map = data.frame(limit=c(), col=c(), stringsAsFactors = F))

#save configurations
usethis::use_data(radolan.configuration, overwrite=T)
usethis::use_data(radolan.configuration.extent900, overwrite=T)
usethis::use_data(radolan.configuration.crs, overwrite=T)
