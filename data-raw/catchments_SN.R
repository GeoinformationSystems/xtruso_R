library(rgdal)

#read catchment data
catchments <- readOGR("data-raw/catchments_SN.json", "OGRGeoJSON", p4s="+init=epsg:3857")
#reproject catchments to RADOLAN stereographic projection
catchments <- spTransform(catchments, proj_radolan)
#store data
save(catchments, file = "data/catchments_SN.Rdata")
