#read radolan data
sample.radolanSF <- ReadRadolanBinary("data-raw/raa01-sf_10000-1708262350-dwd---bin", "SF")
#store data
save(sample.radolanSF, file = "data/sample.radolanSF.Rdata")
