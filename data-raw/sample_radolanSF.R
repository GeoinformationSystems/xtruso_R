library(raster)
library(xtruso)

#read catchment data
sample_radolanSF <- radolan_readBinarySF("data-raw/sample_radolanSF")
#store data
save(sample_radolanSF, file = "data/sample_radolanSF.Rdata")
