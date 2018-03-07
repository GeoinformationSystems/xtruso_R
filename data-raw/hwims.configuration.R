#create configuration list for WSDL
hwims.configuration <- list()

#HWIMS configuration
hwims.configuration[["Q_15min"]] <- list("url" = "http://www.umwelt.sachsen.de:80/umwelt/infosysteme/hwims/webservices/spurwerte-ws",
                                         "type" = "Pegel",
                                         "var" = "Q",
                                         "spur" = "Ziel",
                                         "stat" = "false",
                                         "interval.max" = 9,
                                         "interval.uom" = "days")

hwims.configuration[["Q_1h"]] <- list("url" = "http://www.umwelt.sachsen.de:80/umwelt/infosysteme/hwims/webservices/spurwerte-ws",
                                      "type" = "Pegel",
                                      "var" = "Q",
                                      "spur" = "Ziel-TW-1H",
                                      "stat" = "false",
                                      "interval.max" = 9,
                                      "interval.uom" = "days")

hwims.configuration[["Q_1d"]] <- list("url" = "http://www.umwelt.sachsen.de:80/umwelt/infosysteme/hwims/webservices/spurwerte-ws",
                                      "type" = "Pegel",
                                      "var" = "Q",
                                      "spur" = "Ziel-MW-1T",
                                      "stat" = "false",
                                      "interval.max" = 9,
                                      "interval.uom" = "days")

#save configurations
devtools::use_data(hwims.configuration, overwrite=T)
