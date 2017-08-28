library(raster)
library(xtruso)

#read radolan data
sample_radolanSF <- radolan_readBinarySF("data-raw/raa01-sf_10000-1708210650-dwd---bin")$raster
#store data
save(sample_radolanSF, file = "data/sample_radolanSF.Rdata")
