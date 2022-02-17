# Setup -------------------------------------------------------------------

library(here)
library(tidyverse)
library(naflex)

source(here("src", "helper_funs.R"))

daily <- readRDS(here::here("data", "daily_station_tidy.RDS"))
sts <- levels(daily$station)
sts <- str_replace(sts, "_", " ")
sts <- str_trim(sts)
sts <- str_to_title(sts)
levels(daily$station) <- sts
daily$station <- fct_drop(daily$station)
daily$station <- fct_reorder(daily$station, daily$latitde, mean, na.rm = TRUE)
daily$station <- fct_rev(daily$station)

# Import and prepare data -------------------------------------------------

# Focus on part of the Hurricane season June to October where CHIRPS rainfall totals are
# good and n rain days are overestimated so can be adjusted for
daily <- daily %>%
  dplyr::filter(month %in% 6:10)
  
monthly <- daily %>%
  dplyr::group_by(station, year, month) %>%
  dplyr::summarise(station_sum = sum(rain %>% na_omit_if(n = 5, consec = 3)),
                   station_n = sum(rain %>% na_omit_if(n = 5, consec = 3) >= 0.85),
                   n_miss = sum(is.na(rain)),
                   chirps_sum = sum(chirps_rain %>% na_omit_if(n = 5, consec = 3)),
                   chirps_n = sum(chirps_rain %>% na_omit_if(n = 5, consec = 3) >= 0.85)
                   )


# Thresholds --------------------------------------------------------------

thresh_station_month <- daily %>%
  mutate(st_wd = rain >= 1) %>%
  filter(!is.na(rain) & !is.na(chirps_rain)) %>%
  group_by(station, month) %>%
  summarise(prob = mean(st_wd), 
            thres = quantile(chirps_rain, 1 - prob))

thresh_month <- daily %>%
  mutate(st_wd = rain >= 1) %>%
  filter(!is.na(rain) & !is.na(chirps_rain)) %>%
  group_by(month) %>%
  summarise(prob = mean(st_wd), 
            thres = quantile(chirps_rain, 1 - prob))

ggplot(thresh_station_month, aes(x = month, y = thres, colour = station)) +
  geom_line() +
  geom_line(aes(x = month, y = thres), data = thresh_month, inherit.aes = FALSE, 
            colour = "black", size = 1) +
  scale_colour_manual(values = c25[1:7])

thresh_station_month <- thresh_station_month %>%
  mutate(thres = ifelse(thres < 1, 1, thres))

thresh_month <- thresh_month %>%
  mutate(thres = ifelse(thres < 1, 1, thres))

# Merge thresholds back to daily data

daily <- full_join(daily, thresh_station_month, by = c("station", "month"))


# Scale parameters --------------------------------------------------------

scale_month_st <- daily %>%
  filter(!is.na(rain) & !is.na(chirps_rain)) %>%
  filter(rain > 1) %>%
  group_by(station, month) %>%
  summarise(s_st = mean(rain)) %>%
  mutate(s_st = s_st - 1)

scale_month_chirps <- daily %>%
  filter(!is.na(rain) & !is.na(chirps_rain)) %>%
  filter(chirps_rain > thres) %>%
  group_by(station, month) %>%
  summarise(s_chirps = mean(chirps_rain),
            thres = dplyr::first(thres)) %>%
  mutate(s_chirps = s_chirps - thres)

scale_month <- full_join(scale_month_st, scale_month_chirps, by = c("station", "month"))
scale_month$s <- scale_month$s_st/scale_month$s_chirps
scale_month$s_st <- NULL
scale_month$s_chirps <- NULL

ggplot(scale_month, aes(x = month, y = s, colour = station)) +
  geom_line() +
  scale_colour_manual(values = c25[1:7])

daily$prob <- NULL
daily$thres <- NULL

daily <- full_join(daily, scale_month, by = c("station", "month"))

daily <- daily %>%
  mutate(st = rain,
         chirps = chirps_rain,
         chirps_loci = ifelse(chirps <= thres, 0, chirps - thres),
         chirps_loci = chirps_loci * s)
daily$chirps_rain <- NULL
daily$rain <- NULL
daily_long <- daily %>%
  pivot_longer(c(st, chirps, chirps_loci), names_to = "source", values_to = "rain", 
               names_ptypes = list(source = factor(levels = c("st", "chirps", "chirps_loci"))))


# Annual summaries --------------------------------------------------------

yearly <- daily_long %>%
  group_by(source, station, year) %>%
  summarise(t_rain = sum(rain %>% na_omit_if(n = 10)),
            n_rain = sum(rain %>% na_omit_if(n = 10) >= 1))

ggplot(yearly, aes(x = year, y = t_rain, colour = source)) +
  geom_line() +
  facet_wrap(vars(station))

yearly_wide <- yearly %>%
  pivot_wider(id_cols = c(source, station, year), names_from = source, values_from = c(t_rain, n_rain))

gof_t_rain <- yearly_wide %>%
  group_by(station) %>%
  summarise(cor_chirps = cor(t_rain_chirps, t_rain_st, use = "na.or.complete"),
            cor_chirps_loci = cor(t_rain_chirps_loci, t_rain_st, use = "na.or.complete"),
            bias_chirps = hydroGOF::me(t_rain_chirps, t_rain_st),
            bias_chirps_loci = hydroGOF::me(t_rain_chirps_loci, t_rain_st),
            rmse_chirps = hydroGOF::rmse(t_rain_chirps, t_rain_st),
            rmse_chirps_loci = hydroGOF::rmse(t_rain_chirps_loci, t_rain_st),
  )

ggplot(yearly, aes(x = year, y = n_rain, colour = source)) +
  geom_line() +
  facet_wrap(vars(station))

gof_n_rain <- yearly_wide %>%
  group_by(station) %>%
  summarise(cor_chirps = cor(n_rain_chirps, n_rain_st, use = "na.or.complete"),
            cor_chirps_loci = cor(n_rain_chirps_loci, n_rain_st, use = "na.or.complete"),
            bias_chirps = hydroGOF::me(n_rain_chirps, n_rain_st),
            bias_chirps_loci = hydroGOF::me(n_rain_chirps_loci, n_rain_st),
            rmse_chirps = hydroGOF::rmse(n_rain_chirps, n_rain_st),
            rmse_chirps_loci = hydroGOF::rmse(n_rain_chirps_loci, n_rain_st),
  )
