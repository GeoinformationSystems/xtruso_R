library(raster)
library(xtruso)

#read radolan data
sample_radolanSF <- radolan_readBinarySF("data-raw/sample_radolanSF")$raster
#store data
save(sample_radolanSF, file = "data/sample_radolanSF.Rdata")
