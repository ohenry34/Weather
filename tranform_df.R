library(tidyverse)
library(stringr)

transform_df <- function(df){
  names(df) <- c("Time",
                     "Latitude",
                     "Longitude",
                     "Aircraft Pressure",
                     "Aircraft Altitude",
                     "MSLP",
                     "Temp",
                     "Dew Point",
                     "Wind Direction and Speed",
                     "Peak Flight-Level Wind (10s)",
                     "Peak Surface Wind (SMFR)",
                     "Rain Rate",
                     "Error Code")
  df %>%
    mutate(
    Time = paste0(substring(Time, 1, 1), ":", 
                  substring(Time, 2, 3), ":", 
                  substring(Time, 4, 5)),
    Latitude = paste0(substring(Latitude, 1, 2), ".", 
                      substring(Latitude, 3, 4), "\u00B0", 
                      substring(Latitude, 5, 5)),
    Longitude = ifelse(as.numeric(substring(Longitude, 1, 5)) < 10000,
           paste0(substring(Longitude, 2, 3), ".", 
                              substring(Longitude, 4, 5), "\u00B0", 
                              substring(Longitude, 6, 6)),
           paste0(substring(Longitude, 1, 3), ".", 
                              substring(Longitude, 4, 5), "\u00B0", 
                              substring(Longitude, 6, 6))),
    `Aircraft Pressure` = ifelse(`Aircraft Pressure` > 1000,
                                 `Aircraft Pressure` / 10,
                                 (`Aircraft Pressure` / 10) + 1000),
    `Aircraft Altitude` = suppressWarnings(ifelse(`Aircraft Altitude` == "/////",
                                 NA,
                                 as.numeric(`Aircraft Altitude`))),
    MSLP = suppressWarnings(ifelse(MSLP == "////",
                                   NA,
                                   ifelse(as.numeric(MSLP) > 1000,
                                          as.numeric(MSLP) / 10,
                                          (as.numeric(MSLP) / 10) + 1000)
                                   )),
    Temp = suppressWarnings(ifelse(Temp == "////",
                                   NA,
                                   ifelse(substring(Temp, 1, 1) == "-",
                                          as.numeric(substring(Temp, 2, 4)) / 10 * -1,
                                          as.numeric(Temp) / 10
                                   ))),
    `Dew Point` = suppressWarnings(ifelse(`Dew Point` == "////",
                                   NA,
                                   ifelse(substring(`Dew Point`, 1, 1) == "-",
                                          as.numeric(substring(`Dew Point`, 2, 4)) / 10 * -1,
                                          as.numeric(`Dew Point`) / 10
                                   ))),
    `Wind Direction` = suppressWarnings(ifelse(`Wind Direction and Speed` == "//////",
                                          NA,
                                          substring(str_pad(`Wind Direction and Speed`, 6, pad = "0"), 1, 3)
                                          )),
    `Wind Speed` = suppressWarnings(ifelse(`Wind Direction and Speed` == "//////",
                                               NA,
                                               substring(str_pad(`Wind Direction and Speed`, 6, pad = "0"), 4, 6)
                                          )),
    `Peak Flight-Level Wind (10s)` = as.numeric(suppressWarnings(ifelse(`Peak Flight-Level Wind (10s)` == "///",
                                                             NA,
                                                             `Peak Flight-Level Wind (10s)`)
                                          )),
    `Peak Surface Wind (SMFR)` = as.numeric(suppressWarnings(ifelse(`Peak Surface Wind (SMFR)` == "///",
                                                NA,
                                                `Peak Surface Wind (SMFR)`)
    )), 
    `Rain Rate` = as.numeric(ifelse(`Rain Rate` == "///", NA, `Rain Rate`))
    )%>%
  select(-9)
}


  
  
  
  