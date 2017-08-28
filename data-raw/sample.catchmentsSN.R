#read catchment data
catchments <- rgdal::readOGR("data-raw/sample.catchmentsSN.json", "OGRGeoJSON", p4s="+init=epsg:3857")
#reproject catchments to RADOLAN stereographic projection
sample.catchmentsSN <- sp::spTransform(catchments, proj_radolan)
#store data
save(sample.catchmentsSN, file = "data/sample.catchmentsSN.Rdata")
