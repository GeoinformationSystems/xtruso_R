#read DGM statistics
xtruso.catchments.stat.dgm <- read.table("data-raw/xtruso-catchments-stat-dgm25.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
row.names(xtruso.catchments.stat.dgm) <- xtruso.catchments.stat.dgm$GKZ
devtools::use_data(xtruso.catchments.stat.dgm, overwrite=T)

#read CLC statistics
xtruso.catchments.stat.clc <- read.table("data-raw/xtruso-catchments-stat-clc.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
xtruso.catchments.stat.clc$GKZ <- row.names(xtruso.catchments.stat.clc)
devtools::use_data(xtruso.catchments.stat.clc, overwrite=T)

#read BK50 statistics
xtruso.catchments.stat.bk50 <- read.table("data-raw/xtruso-catchments-stat-bk50.csv", dec=",", sep=";", stringsAsFactors = F, header = T)
xtruso.catchments.stat.bk50$GKZ <- row.names(xtruso.catchments.stat.bk50)
devtools::use_data(xtruso.catchments.stat.bk50, overwrite=T)
