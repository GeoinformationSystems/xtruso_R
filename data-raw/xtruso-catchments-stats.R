#read DGM statistics
xtruso.catchments.stat.dgm <- read.table("data-raw/xtruso-catchments-stat-dgm25.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
row.names(xtruso.catchments.stat.dgm) <- xtruso.catchments.stat.dgm$GKZ

#compute raster slope and assign to DGM stats
terrain.stats <- read.table("data-raw/xtruso-catchments-stat-slope.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
xtruso.catchments.stat.dgm <- merge(xtruso.catchments.stat.dgm, setNames(terrain.stats[, c("GKZ", "MEAN")], c("GKZ", "MEAN.SLOPE")), by="GKZ")
usethis::use_data(xtruso.catchments.stat.dgm, overwrite=T)

#read BROOK90 catchment statistics
xtruso.catchments.stat.b90 <- read.table("data-raw/xtruso-catchments-stat-brook90.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
usethis::use_data(xtruso.catchments.stat.b90, overwrite=T)

#read and prepare BROOK90 soil parameters
xtruso.catchments.soil.b90 <- read.table("data-raw/xtruso-catchments-soil-brook90.csv", stringsAsFactors = F, header = T)
usethis::use_data(xtruso.catchments.soil.b90, overwrite=T)

#read srtm dem
xtruso.dem.sn <- raster::raster("data-raw/srtm_sn.tif")
xtruso.dem.sn <- raster::readAll(xtruso.dem.sn)
usethis::use_data(xtruso.dem.sn, overwrite=T)