#create configuration list for WSDL
osw.configuration <- list()

#HWIMS configuration
osw.configuration[["HWIMS_DC_15min"]] <- list("url" = "https://api.opensensorweb.de/v0",
                                              "network" = "HWIMS",
                                              "sensorCode" = "%%deviceCode%%_DC_15MTV")

#save configurations
devtools::use_data(osw.configuration, overwrite=T)
