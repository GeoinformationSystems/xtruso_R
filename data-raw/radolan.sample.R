#read radolan image
xtruso.radolan.sample <- ReadRadolanBinary("data-raw/raa01-rw_10000-1710071450-dwd---bin", "RW")

#store data
devtools::use_data(xtruso.radolan.sample, overwrite=T)
