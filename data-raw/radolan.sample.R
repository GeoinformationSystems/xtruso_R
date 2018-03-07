#read radolan image
xtruso.radolan.sample <- x.radolan.read("data-raw/raa01-rw_10000-1710071450-dwd---bin", xtruso::radolan.configuration$RW)

#store data
devtools::use_data(xtruso.radolan.sample, overwrite=T)
