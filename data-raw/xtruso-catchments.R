#read catchment data
f.xtruso.catchments <- utils::unzip("data-raw/xtruso-catchments.zip", exdir="data-raw")
xtruso.catchments <- rgdal::readOGR("data-raw/xtruso-catchments.json")
usethis::use_data(xtruso.catchments, overwrite=T)
file.remove(f.xtruso.catchments)

#generate catchment graph
xtruso.catchments.graph <- x.graph.init(xtruso.catchments@data, "GKZNR", colname.conn.out="GKZ_NEXT", conn.none = -1)
usethis::use_data(xtruso.catchments.graph, overwrite=T)

#generate mapping for stations
stations <- rgdal::readOGR("data-raw/xtruso-stations.json", "OGRGeoJSON")
xtruso.stations.catchment <- stations@data[,c("PEG_MSTNR","MIN_GKZNR")]
usethis::use_data(xtruso.stations.catchment, overwrite=T)


xtruso.catchments <- xtruso.catchments[-c(1,8:11)]
xtruso.catchments <- sp::spTransform(xtruso.catchments, sp::CRS("+init=epsg:4326"))

writeOGR(xtruso.catchments, "data-raw/xtruso-catchments-2.json", layer="xtruso-catchments", driver="OGRGeoJSON")
