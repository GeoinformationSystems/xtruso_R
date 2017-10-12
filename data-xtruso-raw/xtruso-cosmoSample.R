#read radolan data
xtruso.cosmo.sample <- ReadCosmoDEGrib("data-xtruso-raw/COSMODE_single_level_elements_TOT_PREC_2017083106_027.grib2")
#store data
save(xtruso.cosmo.sample, file = "data-xtruso/cosmoSample.Rdata")
