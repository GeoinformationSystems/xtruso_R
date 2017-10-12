#read radolan data
xtruso.radolan.sample <- ReadRadolanBinary("data-xtruso-raw/raa01-sf_10000-1708262350-dwd---bin", "SF")
#store data
save(xtruso.radolan.sample, file = "data-xtruso/radolanSample.Rdata")
