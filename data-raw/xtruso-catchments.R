#read catchment data
xtruso.catchments <- rgdal::readOGR("data-raw/xtruso-catchments.json", "OGRGeoJSON", p4s="+init=epsg:3857")
devtools::use_data(xtruso.catchments, overwrite=T)

#generate catchment graph
xtruso.catchments.graph <- x.graph.init(xtruso.catchments@data, "GKZNR", colname.conn.out="GKZ_NEXT", conn.none = -1)
devtools::use_data(xtruso.catchments.graph, overwrite=T)
