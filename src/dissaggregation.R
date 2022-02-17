library(here)
library(ggplot2)
library(lubridate)
library(reshape2)
library(viridis)
library(tidyr)
library(hydroGOF)
library(stringr)
library(knitr)
library(kableExtra)
library(rnaturalearthdata)
library(rnaturalearth)
library(ggrepel)
library(sp)
library(tibble)
library(verification)
library(purrr)
library(dplyr)
library(ggspatial)
library(sf)
library(tidyverse)

chirps_daily <- readRDS(here("data", "daily data sorted.RDS"))
chirps_daily <- chirps_daily$get_data_frame("Sattelite_data")

chirps_daily <- dplyr::transmute(chirps_daily,
                                 station = station,
                                 date = as.Date(date),
                                 year = lubridate::year(date),
                                 month = lubridate::month(date),
                                 day = lubridate::day(date),
                                 chirps_rain = chirps_rain
                                 )
chirps_monthly <- chirps_daily %>% 
  group_by(station, year, month) %>%
  summarise(t_rain_chirps = sum(chirps_rain, na.rm = TRUE))

alpha_grouped <- filter(alpha_grouped, year < 2007) #the filter is applied because our monthly data only goes up to 2006 by the looks of it. 

# Better to do the NA coding at import
monthly_data <- read.csv(here("data", "156_stations.csv"), na.strings = c("#NUM!", "NA"))

monthly_data <- monthly_data %>%
  pivot_longer(!year & !month, names_to = "station", values_to = "station_rain") 

monthly_data <- filter(monthly_data, year >= 1981) %>%
  group_by(station)

# Using full_join() instead of merge() but same outcome
full_data <- full_join(monthly_data, alpha_grouped, by = c("station", "year", "month"))

full_data <- full_data %>%
 mutate(ratio = ifelse(!is.na(station_rain) & !is.na(t_rain_chirps), station_rain/t_rain_chirps, NA))
#some inf that you will have to deal with above

#tidying up our data for the station chirps values 
alpha <- filter(alpha, year < 2007)

full_data_daily <- left_join(alpha, full_data, by = c("station", "year", "month")) #gives us all the values for chirps data per month and the ratio for that month so that we can multiply it across

# We could possibly think about this case separately as we might want to do something different when station total == 0 and chirps_total != 0
full_data_daily[full_data_daily == Inf] <- 0 #replace all infinities with 0 since for our purposes, inf as a ratio implies station rain is zero so when we multiply across , we are all set

full_data_daily <- full_data_daily %>%
  mutate(scaled_rain = ifelse(!is.na(ratio), chirps_rain * ratio, chirps_rain)) #for those months where the ratio is NA we have to leave the chirps rain as is else we multiply by our ratio

