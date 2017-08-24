library(rgdal)

#read catchment data
catchments <- readOGR("data-raw/catchments_SN.json", "OGRGeoJSON", p4s="+init=epsg:3857")
#reproject catchments to RADOLAN stereographic projection
sample_catchmentsSN <- spTransform(catchments, proj_radolan)
#store data
save(sample_catchmentsSN, file = "data/sample_catchmentsSN.Rdata")
