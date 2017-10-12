#read catchment data
xtruso.catchments <- rgdal::readOGR("data-xtruso-raw/xtruso-catchments.json", "OGRGeoJSON", p4s="+init=epsg:3857")
#store data
save(xtruso.catchments, file = "data-xtruso/catchments.Rdata")
