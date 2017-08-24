library(raster)
library(xtruso)

#read radolan data
sample_radolanSF <- radolan_readBinary("data-raw/sample_radolanSF")
#read catchment data
load("data/sample_catchments_SN.RData")
#calculate overlap
overlap <- raster_zonalOverlap(sample_radolanSF, sample_catchments_SN)
#get mapping
sample_radolanSFMapping <- overlap@data[,c("OBJECTID", "GKZ", "FLAECHE_HA", "raster_fun")]
#evaluate function with sampel_radolanSF
sample_radolanSFMapping <- raster_evalRasterFun(sample_radolanSF, sample_radolanSFMapping, "raster_fun")

#store data
save(sample_radolanSFMapping, file = "data/sample_radolanSFMapping.Rdata")
