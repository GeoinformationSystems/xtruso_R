#calculate overlap
overlap <- ZonalOverlap(xtruso.cosmo.sample, xtruso.catchments)
xtruso.raster.fun.cosmo <- overlap@data[,c("OBJECTID", "GKZ", "FLAECHE_HA", "raster.avg")]

#store data
save(xtruso.raster.fun.cosmo, file = "data-xtruso/rasterFun-cosmo.Rdata")
