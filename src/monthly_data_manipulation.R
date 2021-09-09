library(here)
library(ggplot2)
library(lubridate)
library(reshape2)
library(viridis)
library(RColorBrewer)
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
library (ggspatial)
library(sf)
library(tidyverse)

#this section of the code should ensure that the proportion of raindays of chirps and the station are the same 
df1 <- readRDS(here( "data", "daily_data_sorted1.RDS")) 
df1 <- df1$get_data_frame("merge")
df1 <- dplyr::filter(df1, station != "thiotte")
df1$date <- as.Date(df1$date)


df1_grouped <- df1 %>% 
  group_by(station, month) %>%
  summarise(n_rain_dry = 1 - mean (rain > 0.85, na.rm = TRUE))

full_data1 <- full_join(df1, df1_grouped, by = c("station", "month"))

# Good thinking but you don't need to sort the data before, quantile() will do this itself
# full_data1 <- full_data[order(full_data$chirps_rain)] 

full_data2 <- full_data1 %>%
  group_by(station, month) %>%
  mutate(
    chirps_thresh = quantile(full_data1$chirps_rain, n_rain_dry) 
                                           #this looks like it gets the job done 
  )

full_data2$n_rain_dry[is.nan(full_data2$n_rain_dry)]<-NA #NaN values pop up because we have instances where rain is NA so when we do calculations with rain n_rain_dry end up with zero 


full_data2 <- full_data2 %>%
  mutate(
    # sensible to have this as a different name to check it works correctly
    chirps_rain_adj = ifelse(chirps_rain < chirps_thresh, 0, chirps_rain)
    
  ) 

#this section of the code should ensure that the total rainfall for the chirps and station data are the same 

alpha_grouped <- full_data2 %>% 
  group_by(station, year, month) %>%
  mutate(t_rain_chirps = sum(chirps_rain_adj, na.rm = TRUE),
         t_rain_station = sum(rain, na.rm = TRUE),
         ratio = ifelse(!is.na(t_rain_station) & !is.na(t_rain_chirps), t_rain_station/t_rain_chirps, NA)
         )
#NaN and inf pop up so I am going to set those equal to 0 for these purposes
alpha_grouped$ratio[is.nan(alpha_grouped$ratio)] <- NA
alpha_grouped[alpha_grouped == Inf] <- 0

alpha_grouped <- alpha_grouped %>%
  mutate(chirps_rain_adj = ifelse(!is.na(ratio), chirps_rain_adj * ratio, chirps_rain_adj),
         month.abb = month.abb[month])
write.csv(alpha_grouped , here("data", "alpha_grouped.csv"))


g <- ggplot(alpha_grouped, mapping = aes(factor(month), chirps_thresh, colour = station)) +
  
  geom_line()

print(g)
ggsave(here("results", "chirps_thresh_comparisons.png"), width = 12, height = 6)
