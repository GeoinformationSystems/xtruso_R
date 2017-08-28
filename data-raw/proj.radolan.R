#create and save stereographic RADOLAN projection
proj.radolan <- CRS("+proj=stere +lat_0=90 +lat_ts=90 +lon_0=10 +k=0.93301270189 +x_0=0 +y_0=0 +a=6370040 +b=6370040 +to_meter=1000 +no_defs")
devtools::use_data(proj.radolan)
