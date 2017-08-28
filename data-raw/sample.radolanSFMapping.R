#calculate overlap
overlap <- ZonalOverlap(sample.radolanSF, sample.catchmentsSN)
#get mapping
sample.radolanSFMapping <- overlap@data[,c("OBJECTID", "GKZ", "FLAECHE_HA", "raster.fun")]
#evaluate function with sampel_radolanSF
sample.radolanSFMapping <- EvalRasterFun(sample.radolanSF, sample.radolanSFMapping, "raster.fun")

#store data
save(sample.radolanSFMapping, file = "data/sample.radolanSFMapping.Rdata")
