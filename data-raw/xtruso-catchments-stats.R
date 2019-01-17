#read DGM statistics
xtruso.catchments.stat.dgm <- read.table("data-raw/xtruso-catchments-stat-dgm25.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
row.names(xtruso.catchments.stat.dgm) <- xtruso.catchments.stat.dgm$GKZ

#read CLC statistics
xtruso.catchments.stat.clc <- read.table("data-raw/xtruso-catchments-stat-clc.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
xtruso.catchments.stat.clc$GKZ <- row.names(xtruso.catchments.stat.clc)

#read BK50 statistics
xtruso.catchments.stat.bk50 <- read.table("data-raw/xtruso-catchments-stat-bk50.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
xtruso.catchments.stat.bk50$GKZ <- row.names(xtruso.catchments.stat.bk50)

#read BROOK90 statistics
xtruso.catchments.stat.b90 <- read.table("data-raw/xtruso-b90-egz-data.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
usethis::use_data(xtruso.catchments.stat.b90, overwrite=T)

#read srtm dem
xtruso.dem.sn <- raster::raster("data-raw/srtm_sn.tif")

#compute raster slope and assign to DGM stats
terrain.stats <- read.table("data-raw/xtruso-catchments-stat-slope.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
xtruso.catchments.stat.dgm <- merge(xtruso.catchments.stat.dgm, setNames(terrain.stats[, c("GKZ", "MEAN")], c("GKZ", "MEAN.SLOPE")), by="GKZ")

usethis::use_data(xtruso.catchments.stat.dgm, overwrite=T)
usethis::use_data(xtruso.catchments.stat.clc, overwrite=T)
usethis::use_data(xtruso.catchments.stat.bk50, overwrite=T)
usethis::use_data(xtruso.catchments.stat.b90, overwrite=T)
usethis::use_data(xtruso.dem.sn, overwrite=T)