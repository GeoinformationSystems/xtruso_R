#calculate overlap
overlap <- ZonalOverlap(xtruso.radolan.sample, xtruso.catchments)
xtruso.raster.fun.radolan <- overlap@data[,c("OBJECTID", "GKZ", "FLAECHE_HA", "raster.avg")]

#store data
save(xtruso.raster.fun.radolan, file = "data-xtruso/rasterFun-radolan.Rdata")
