# Setup -------------------------------------------------------------------

library(here)
library(tidyverse)

source(here("src", "helper_funs.R"))

daily <- readRDS(here::here("data", "daily_station_tidy.RDS"))
sts <- levels(daily$station)
sts <- str_replace(sts, "_", " ")
sts <- str_trim(sts)
sts <- str_to_title(sts)
levels(daily$station) <- sts
stations <- sts
daily$station <- fct_drop(daily$station)
daily$station <- fct_reorder(daily$station, daily$latitde, mean, na.rm = TRUE)
daily$station <- fct_rev(daily$station)

daily <- daily %>% 
  mutate(rainday1 = factor(ifelse(rain >= 0.85, "w", "d"), levels = c("d", "w")),
         lag1 = dplyr::lag(rainday1),
         lag2 = dplyr::lag(rainday1, 2),
         lag3 = dplyr::lag(rainday1, 3),
         lag4 = dplyr::lag(rainday1, 4),
         lag5 = dplyr::lag(rainday1, 5),
         p1 = lag1,
         p2 = factor(paste0(lag2, lag1), levels = c(paste0("d", levels(p1)), paste0("w", levels(p1)))),
         p3 = factor(paste0(lag3, lag2, lag1), levels = c(paste0("d", levels(p2)), paste0("w", levels(p2)))),
         p4 = factor(paste0(lag4, lag3, lag2, lag1), levels = c(paste0("d", levels(p3)), paste0("w", levels(p3)))),
         p5 = factor(paste0(lag5, lag4, lag3, lag2, lag1), levels = c(paste0("d", levels(p4)), paste0("w", levels(p4)))),
         p2 = na_if(p2, is.na(lag1) | is.na(lag2)),
         p1p5 = forcats::fct_recode(p2, w = "dw", w = "ww"),
         p3 = na_if(p3, is.na(lag1) | is.na(lag2) | is.na(lag3)),
         p2p5 = forcats::fct_recode(p3, w = "ddw", w = "dww", w = "wdw", w = "www"),
         p4 = na_if(p4, is.na(lag1) | is.na(lag2) | is.na(lag3) | is.na(lag4)),
         p3p5 = forcats::fct_recode(p4, w = "dddw", w = "ddww", w = "dwdw", w = "dwww",
                                    w = "wddw", w = "wdww", w = "wwdw", w = "wwww"),
         p5 = na_if(p5, is.na(lag1) | is.na(lag2) | is.na(lag3) | is.na(lag4) | is.na(lag5)),
         p4p5 = forcats::fct_recode(p5, w = "ddddw", w = "dddww", w = "ddwdw", w = "ddwww",
                                    w = "dwddw", w = "dwdww", w = "dwwdw", w = "dwwww",
                                    w = "wdddw", w = "wddww", w = "wdwdw", w = "wdwww",
                                    w = "wwddw", w = "wwdww", w = "wwwdw", w = "wwwww")
  )

daily <- daily %>% 
  filter(!is.na(p1) & !is.na(p2) & !is.na(p3) & !is.na(p4) & !is.na(p5))

summary(daily, maxsum = 40)

fit_mcm <- function(data, fm) {
  glm(as.formula(fm), data = data, family = binomial)
}

daily_nest <- daily %>%
  group_by(station) %>%
  nest()

daily_nest <- daily_nest %>%
  mutate(m_p1 = purrr::map(data, fit_mcm, fm = fm_p1))

daily_nest$m_p1 %>% 
  purrr::set_names(daily_nest$station) %>%
  purrr::map(anova, test = "Chisq")

daily_nest <- daily_nest %>%
  mutate(m_p1_int = purrr::map(data, fit_mcm, fm = fm_p1_int))

daily_nest$m_p1_int %>% 
  purrr::set_names(daily_nest$station) %>%
  purrr::map(anova, test = "Chisq")

daily_nest <- daily_nest %>%
  mutate(m_p2 = purrr::map(data, fit_mcm, fm = fm_p2))

daily_nest$m_p2 %>% 
  purrr::set_names(daily_nest$station) %>%
  purrr::map(anova, test = "Chisq")

daily_nest <- daily_nest %>%
  mutate(m_p2_int = purrr::map(data, fit_mcm, fm = fm_p2_int))

daily_nest$m_p2_int %>% 
  purrr::set_names(daily_nest$station) %>%
  purrr::map(anova, test = "Chisq")

daily_nest <- daily_nest %>%
  mutate(m_p3 = purrr::map(data, fit_mcm, fm = fm_p3))

daily_nest$m_p3 %>% 
  purrr::set_names(daily_nest$station) %>%
  purrr::map(anova, test = "Chisq")


####


# Cap Haitien -------------------------------------------------------------

cap_haiten <- daily %>% 
  filter(station == "Cap Haitien")

fm_p1p5_4h_1hi <- rainday1 ~ 
  p1p5 * (cos(doy * 1 * 2 * pi/366) +
          sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366) +
  cos(doy * 4 * 2 * pi/366) +
  sin(doy * 4 * 2 * pi/366)
m_p1p5_4h_1hi <- glm(as.formula(fm_p1p5_4h_1hi), data = cap_haiten, family = binomial)
anova(m_p1p5_4h_1hi, test = "Chisq")

fm_p1p5_3h_1hi <- rainday1 ~ 
  p1p5 * (cos(doy * 1 * 2 * pi/366) +
          sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)
m_p1p5_3h_1hi <- glm(as.formula(fm_p1p5_3h_1hi), data = cap_haiten, family = binomial)
anova(m_p1p5_3h_1hi, test = "Chisq")

fm_p1p5_3h_2hi <- rainday1 ~ 
  p1p5 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)
m_p1p5_3h_2hi <- glm(as.formula(fm_p1p5_3h_2hi), data = cap_haiten, family = binomial)
anova(m_p1p5_3h_2hi, test = "Chisq")

fm_p1p5_3h_3hi <- rainday1 ~ 
  p1p5 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366))
m_p1p5_3h_3hi <- glm(as.formula(fm_p1p5_3h_3hi), data = cap_haiten, family = binomial)
anova(m_p1p5_3h_3hi, test = "Chisq")

fm_p2_3h_1hi <- rainday1 ~ 
  p1 + p1p5 + p2 * (cos(doy * 1 * 2 * pi/366) +
          sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)
m_p2_3h_1hi <- glm(as.formula(fm_p2_3h_1hi), data = cap_haiten, family = binomial)
anova(m_p2_3h_1hi, test = "Chisq")

fm_p2_4h_1hi <- rainday1 ~ 
  p2 * (cos(doy * 1 * 2 * pi/366) +
          sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366) +
  cos(doy * 4 * 2 * pi/366) +
  sin(doy * 4 * 2 * pi/366)
m_p2_4h_1hi <- glm(as.formula(fm_p2_4h_1hi), data = cap_haiten, family = binomial)
anova(m_p2_4h_1hi, test = "Chisq")

fm_p2p5_3h_1hi <- rainday1 ~ 
  p1p5 + p2p5 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)
m_p2p5_3h_1hi <- glm(as.formula(fm_p2p5_3h_1hi), data = cap_haiten, family = binomial)
anova(m_p2p5_3h_1hi, test = "Chisq")

fm_p2p5_3h_2hi <- rainday1 ~ 
  p1p5 + p2p5 * (cos(doy * 1 * 2 * pi/366) +
                   sin(doy * 1 * 2 * pi/366) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366)) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)
m_p2p5_3h_2hi <- glm(as.formula(fm_p2p5_3h_2hi), data = cap_haiten, family = binomial)
anova(m_p2p5_3h_2hi, test = "Chisq")

fm_p2p5_3h_3hi <- rainday1 ~ 
  p1p5 + p2p5 * (cos(doy * 1 * 2 * pi/366) +
                   sin(doy * 1 * 2 * pi/366) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366))
m_p2p5_3h_3hi <- glm(as.formula(fm_p2p5_3h_3hi), data = cap_haiten, family = binomial)
anova(m_p2p5_3h_3hi, test = "Chisq")

fm_p3p5_3h_1hi <- rainday1 ~ 
  p2p5 + p3p5 * (cos(doy * 1 * 2 * pi/366) +
                   sin(doy * 1 * 2 * pi/366)) +
                   cos(doy * 2 * 2 * pi/366) +
                   sin(doy * 2 * 2 * pi/366) +
                   cos(doy * 3 * 2 * pi/366) +
                   sin(doy * 3 * 2 * pi/366)
m_p3p5_3h_1hi <- glm(as.formula(fm_p3p5_3h_1hi), data = cap_haiten, family = binomial)
anova(m_p3p5_3h_1hi, test = "Chisq")

fm_p3p5_3h_2hi <- rainday1 ~ 
  p2p5 + p3p5 * (cos(doy * 1 * 2 * pi/366) +
                   sin(doy * 1 * 2 * pi/366) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366)) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)
m_p3p5_3h_2hi <- glm(as.formula(fm_p3p5_3h_2hi), data = cap_haiten, family = binomial)
anova(m_p3p5_3h_2hi, test = "Chisq")

fm_p4p5_3h_1hi <- rainday1 ~ 
  p3p5 + p4p5 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)
m_p4p5_3h_1hi <- glm(as.formula(fm_p4p5_3h_1hi), data = cap_haiten, family = binomial)
anova(m_p4p5_3h_1hi, test = "Chisq")

fm_p4p5_3h_2hi <- rainday1 ~ 
  p3p5 + p4p5 * (cos(doy * 1 * 2 * pi/366) +
                   sin(doy * 1 * 2 * pi/366) +
                   cos(doy * 2 * 2 * pi/366) +
                   sin(doy * 2 * 2 * pi/366)) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)
m_p4p5_3h_2hi <- glm(as.formula(fm_p4p5_3h_2hi), data = cap_haiten, family = binomial)
anova(m_p4p5_3h_2hi, test = "Chisq")

AIC(m_p1p5_4h_1hi, m_p1p5_3h_1hi, m_p1p5_3h_2hi, m_p1p5_3h_3hi, m_p2_3h_1hi, m_p2_4h_1hi, m_p2p5_3h_1hi, m_p2p5_3h_2hi, m_p2p5_3h_3hi, m_p3p5_3h_1hi, m_p3p5_3h_2hi, m_p4p5_3h_1hi)
BIC(m_p1p5_4h_1hi, m_p1p5_3h_1hi, m_p1p5_3h_2hi, m_p1p5_3h_3hi, m_p2_3h_1hi, m_p2_4h_1hi, m_p2p5_3h_1hi, m_p2p5_3h_2hi, m_p2p5_3h_3hi, m_p3p5_3h_1hi, m_p3p5_3h_2hi, m_p4p5_3h_1hi)

# Choice: Second-order hybrid (3 levels) with 3 harmonics and interactions on the first harmonic

# Damien ------------------------------------------------------------------

damien <- daily %>% 
  filter(station == "Damien")

fm_p1_3h_1i <- rainday1 ~ 
  p1  * (cos(doy * 1 * 2 * pi/366) +
           sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)

m_p1_3h_1i <- glm(as.formula(fm_p1_3h_1i), data = damien, family = binomial)
anova(m_p1_3h_1i, test = "Chisq")

fm_p1p5_3h_1i <- rainday1 ~ 
  p1 + p1p5 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)

m_p1p5_3h_1i <- glm(as.formula(fm_p1p5_3h_1i), data = damien, family = binomial)
anova(m_p1p5_3h_1i, test = "Chisq")

fm_p1p5_4h_1i <- rainday1 ~ 
  p1p5 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366) +
  cos(doy * 4 * 2 * pi/366) +
  sin(doy * 4 * 2 * pi/366)

m_p1p5_4h_1i <- glm(as.formula(fm_p1p5_4h_1i), data = damien, family = binomial)
anova(m_p1p5_4h_1i, test = "Chisq")

fm_p1p5_4h_2i <- rainday1 ~ 
  p1p5 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366)) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366) +
  cos(doy * 4 * 2 * pi/366) +
  sin(doy * 4 * 2 * pi/366)

m_p1p5_4h_2i <- glm(as.formula(fm_p1p5_4h_2i), data = damien, family = binomial)
anova(m_p1p5_4h_2i, test = "Chisq")

fm_p2_4h_1i <- rainday1 ~ 
  p1p5 + p2 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366) +
  cos(doy * 4 * 2 * pi/366) +
  sin(doy * 4 * 2 * pi/366)

m_p2_4h_1i <- glm(as.formula(fm_p2_4h_1i), data = damien, family = binomial)
anova(m_p2_4h_1i, test = "Chisq")

fm_p2_4h_2i <- rainday1 ~ 
  p1p5 + p2 * (cos(doy * 1 * 2 * pi/366) +
                 sin(doy * 1 * 2 * pi/366) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366)) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366) +
  cos(doy * 4 * 2 * pi/366) +
  sin(doy * 4 * 2 * pi/366)

m_p2_4h_2i <- glm(as.formula(fm_p2_4h_2i), data = damien, family = binomial)
anova(m_p2_4h_2i, test = "Chisq")

fm_p2p5_3h_1i <- rainday1 ~ 
  p1p5 + p2p5 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366)

m_p2p5_3h_1i <- glm(as.formula(fm_p2p5_3h_1i), data = damien, family = binomial)
anova(m_p2p5_3h_1i, test = "Chisq")

fm_p2p5_4h_1i <- rainday1 ~ 
  p1p5 + p2p5 * (cos(doy * 1 * 2 * pi/366) +
          sin(doy * 1 * 2 * pi/366)) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366) +
  cos(doy * 4 * 2 * pi/366) +
  sin(doy * 4 * 2 * pi/366)

m_p2p5_4h_1i <- glm(as.formula(fm_p2p5_4h_1i), data = damien, family = binomial)
anova(m_p2p5_4h_1i, test = "Chisq")

fm_p2p5_4h_2i <- rainday1 ~ 
  p1p5 + p2p5 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
  cos(doy * 2 * 2 * pi/366) +
  sin(doy * 2 * 2 * pi/366)) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366) +
  cos(doy * 4 * 2 * pi/366) +
  sin(doy * 4 * 2 * pi/366)

m_p2p5_4h_2i <- glm(as.formula(fm_p2p5_4h_2i), data = damien, family = binomial)
anova(m_p2p5_4h_2i, test = "Chisq")

fm_p3_4h_1i <- rainday1 ~ 
  p1 + p2 + p3 * (cos(doy * 1 * 2 * pi/366) +
                   sin(doy * 1 * 2 * pi/366)) +
                   cos(doy * 2 * 2 * pi/366) +
                   sin(doy * 2 * 2 * pi/366) +
  cos(doy * 3 * 2 * pi/366) +
  sin(doy * 3 * 2 * pi/366) +
  cos(doy * 4 * 2 * pi/366) +
  sin(doy * 4 * 2 * pi/366)

m_p3_4h_1i <- glm(as.formula(fm_p3_4h_1i), data = damien, family = binomial)
anova(m_p3_4h_1i, test = "Chisq")

AIC(m_p1_3h_1i, m_p1p5_3h_1i, m_p1p5_4h_1i, m_p1p5_4h_2i, m_p2_4h_1i, m_p2_4h_2i, m_p2p5_3h_1i, m_p2p5_4h_1i, m_p2p5_4h_2i, m_p3_4h_1i)
BIC(m_p1_3h_1i, m_p1p5_3h_1i, m_p1p5_4h_1i, m_p1p5_4h_2i, m_p2_4h_1i, m_p2_4h_2i, m_p2p5_3h_1i, m_p2p5_4h_1i, m_p2p5_4h_2i, m_p3_4h_1i)

# Choice: Either: 1) second-order hybrid (w, dd, wd) with 3 harmonics and interactions on the first harmonic
#                 2) second-order hybrid (w, dd, wd) with 4 harmonics and interactions on the first harmonic


# Jacmel ------------------------------------------------------------------

jacmel <- daily %>% 
  filter(station == "Jacmel")

m_p1_2h <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)), 
  data = jacmel, family = binomial)
anova(m_p1_2h, test = "Chisq")

m_p1_3h <- glm(
  rainday1 ~ 
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)),
  data = jacmel, family = binomial)
anova(m_p1_3h, test = "Chisq")

m_p1_2h_2i <- glm(
  rainday1 ~
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)), 
  data = jacmel, family = binomial)
anova(m_p1_2h_2i, test = "Chisq")

m_p1_3h_3i <- glm(
  rainday1 ~ 
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)), 
  data = jacmel, family = binomial)
anova(m_p1_3h_3i, test = "Chisq")

m_p1p5_3h_3i <- glm(
  rainday1 ~
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5,
  data = jacmel, family = binomial)
anova(m_p1p5_3h_3i, test = "Chisq")

m_p2_3h_3i <- glm(
  rainday1 ~
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p2,
  data = jacmel, family = binomial)
anova(m_p2_3h_3i, test = "Chisq")

AIC(m_p1_2h, m_p1_3h, m_p1_2h_2i, m_p1_3h_3i, m_p1p5_3h_3i, m_p2_3h_3i)
BIC(m_p1_2h, m_p1_3h, m_p1_2h_2i, m_p1_3h_3i, m_p1p5_3h_3i, m_p2_3h_3i)

# Choice: first-order with 2 harmonics and no interactions


# Jeremie -----------------------------------------------------------------

jeremie <- daily %>% 
  filter(station == "Jeremie") %>%
  mutate(p1p5dry = forcats::fct_recode(p2, d = "wd", d = "dd"),
         p2p5dry = forcats::fct_recode(p3, d = "dwd", d = "ddd", d = "wwd", d = "wdd"))

m_p1p5_3h_3i <- glm(
  rainday1 ~
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5,
  data = jeremie, family = binomial)
anova(m_p1p5_3h_3i, test = "Chisq")

m_p2_3h_3i <- glm(
  rainday1 ~
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p2,
  data = jeremie, family = binomial)
anova(m_p2_3h_3i, test = "Chisq")

m_p1p5dry_3h_3i <- glm(
  rainday1 ~
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5dry,
  data = jeremie, family = binomial)
anova(m_p1p5dry_3h_3i, test = "Chisq")

m_p1p5dry_2h_2i <- glm(
  rainday1 ~
    p1p5dry * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)),
  data = jeremie, family = binomial)
anova(m_p1p5dry_2h_2i, test = "Chisq")

m_p1p5dry_2h_1i <- glm(
  rainday1 ~
    p1p5dry * (cos(doy * 1 * 2 * pi/366) +
                 sin(doy * 1 * 2 * pi/366)) +
                 cos(doy * 2 * 2 * pi/366) +
                 sin(doy * 2 * 2 * pi/366),
  data = jeremie, family = binomial)
anova(m_p1p5dry_2h_1i, test = "Chisq")

m_p1p5dry_2h <- glm(
  rainday1 ~
    p1p5dry + (cos(doy * 1 * 2 * pi/366) +
                 sin(doy * 1 * 2 * pi/366) +
                 cos(doy * 2 * 2 * pi/366) +
                 sin(doy * 2 * 2 * pi/366) +
                 ),
  data = jeremie, family = binomial)
anova(m_p1p5dry_2h, test = "Chisq")

m_p1p5dry_3h <- glm(
  rainday1 ~
    p1p5dry + (cos(doy * 1 * 2 * pi/366) +
                 sin(doy * 1 * 2 * pi/366) +
                 cos(doy * 2 * 2 * pi/366) +
                 sin(doy * 2 * 2 * pi/366) +
                 cos(doy * 3 * 2 * pi/366) +
                 sin(doy * 3 * 2 * pi/366)),
  data = jeremie, family = binomial)
anova(m_p1p5dry_3h, test = "Chisq")

m_p1p5dry_3h_3i_2 <- glm(
  rainday1 ~
    p1p5dry * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)),
  data = jeremie, family = binomial)
anova(m_p1p5dry_3h_3i_2, test = "Chisq")

m_p2p5dry_2h <- glm(
  rainday1 ~
    p1p5dry + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + p2p5dry, 
  data = jeremie, family = binomial)
anova(m_p2p5dry_2h, test = "Chisq")

AIC(m_p1p5_3h_3i, m_p2_3h_3i, m_p1p5dry_3h_3i, m_p1p5dry_2h_2i, m_p1p5dry_2h_1i, m_p1p5dry_2h, m_p1p5dry_3h, m_p2p5dry_2h)
BIC(m_p1p5_3h_3i, m_p2_3h_3i, m_p1p5dry_3h_3i, m_p1p5dry_2h_2i, m_p1p5dry_2h_1i, m_p1p5dry_2h, m_p1p5dry_3h, m_p2p5dry_2h)

# Choice: hybrid second-order (d, dw, ww) with 2 harmonics and no interactions


# Les Cayes ---------------------------------------------------------------

les_cayes <- daily %>% 
  filter(station == "Les Cayes") %>%
  mutate(p1p5dry = forcats::fct_recode(p2, d = "wd", d = "dd"),
         p2p5dry = forcats::fct_recode(p3, d = "dwd", d = "ddd", d = "wwd", d = "wdd"))

m_p1p5_3h_3i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5 * (cos(doy * 1 * 2 * pi/366) +
              sin(doy * 1 * 2 * pi/366) +
              cos(doy * 2 * 2 * pi/366) +
              sin(doy * 2 * 2 * pi/366) +
              cos(doy * 3 * 2 * pi/366) +
              sin(doy * 3 * 2 * pi/366)),
  data = les_cayes, family = binomial)
anova(m_p1p5_3h_3i, test = "Chisq")

m_p1p5dry_3h_3i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
                      sin(doy * 1 * 2 * pi/366) +
                      cos(doy * 2 * 2 * pi/366) +
                      sin(doy * 2 * 2 * pi/366) +
                      cos(doy * 3 * 2 * pi/366) +
                      sin(doy * 3 * 2 * pi/366)) +
    p1p5dry * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)),
  data = les_cayes, family = binomial)
anova(m_p1p5dry_3h_3i, test = "Chisq")

m_p1p5_3h_1i <- glm(
  rainday1 ~
    p1p5 * (cos(doy * 2 * 2 * pi/366) +
                 sin(doy * 2 * 2 * pi/366)) +
    cos(doy * 1 * 2 * pi/366) +
    sin(doy * 1 * 2 * pi/366) +
    cos(doy * 3 * 2 * pi/366) +
    sin(doy * 3 * 2 * pi/366),
  data = les_cayes, family = binomial)
anova(m_p1p5_3h_1i, test = "Chisq")

m_p1p5dry_3h_1i <- glm(
  rainday1 ~
    p1p5dry * (cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) +
    cos(doy * 1 * 2 * pi/366) +
    sin(doy * 1 * 2 * pi/366) +
    cos(doy * 3 * 2 * pi/366) +
    sin(doy * 3 * 2 * pi/366),
  data = les_cayes, family = binomial)
anova(m_p1p5dry_3h_1i, test = "Chisq")

m_p2_3h_3i <- glm(
  rainday1 ~
    p2 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)),
  data = les_cayes, family = binomial)
anova(m_p2_3h_3i, test = "Chisq")

m_p2_3h_1i <- glm(
  rainday1 ~
    p2 * (cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    (cos(doy * 1 * 2 * pi/366) +
       sin(doy * 1 * 2 * pi/366) +
       cos(doy * 3 * 2 * pi/366) +
       sin(doy * 3 * 2 * pi/366)),
  data = les_cayes, family = binomial)
anova(m_p2_3h_1i, test = "Chisq")

m_p3_3h_1i <- glm(
  rainday1 ~
    p2 * (cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    (cos(doy * 1 * 2 * pi/366) +
       sin(doy * 1 * 2 * pi/366) +
       cos(doy * 3 * 2 * pi/366) +
       sin(doy * 3 * 2 * pi/366)) +
    p3,
  data = les_cayes, family = binomial)
anova(m_p3_3h_1i, test = "Chisq")

m_p2p5_3h_1i <- glm(
  rainday1 ~
    p2 * (cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    (cos(doy * 1 * 2 * pi/366) +
       sin(doy * 1 * 2 * pi/366) +
       cos(doy * 3 * 2 * pi/366) +
       sin(doy * 3 * 2 * pi/366)) + 
    p2p5,
  data = les_cayes, family = binomial)
anova(m_p2p5_3h_1i, test = "Chisq")

AIC(m_p1p5_3h_3i, m_p1p5dry_3h_3i, m_p1p5dry_3h_1i, m_p1p5_3h_1i, m_p2_3h_3i, m_p2_3h_1i, m_p3_3h_1i, m_p2p5_3h_1i) %>% arrange(AIC)
BIC(m_p1p5_3h_3i, m_p1p5dry_3h_3i, m_p1p5dry_3h_1i, m_p1p5_3h_1i, m_p2_3h_3i, m_p2_3h_1i, m_p3_3h_1i, m_p2p5_3h_1i) %>% arrange(BIC)

# Choice either: 1) hybrid second-order (d, dw, ww) with 3 harmonics and interactions on the second harmonic
#                2) hybrid second-order (w, wd, dd) with 3 harmonics and interactions on the second harmonic
#                3) second-order with 3 harmonics and interactions on the second harmonic

# Petitionville -----------------------------------------------------------

petitionville <- daily %>% 
  filter(station == "Petitionville")

m_p1_3h_3i <- glm(
  rainday1 ~
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)),
  data = petitionville, family = binomial)
anova(m_p1_3h_3i, test = "Chisq")

m_p1_2h_2i <- glm(
  rainday1 ~
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)),
  data = petitionville, family = binomial)
anova(m_p1_2h_2i, test = "Chisq")

m_p1p5_2h_2i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    p1p5 * (cos(doy * 1 * 2 * pi/366) +
              sin(doy * 1 * 2 * pi/366) +
              cos(doy * 2 * 2 * pi/366) +
              sin(doy * 2 * 2 * pi/366)),
  data = petitionville, family = binomial)
anova(m_p1p5_2h_2i, test = "Chisq")

m_p1p5_2h_1i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    p1p5 * (cos(doy * 1 * 2 * pi/366) +
              sin(doy * 1 * 2 * pi/366)),
  data = petitionville, family = binomial)
anova(m_p1p5_2h_1i, test = "Chisq")

m_p1p5_2h <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    p1p5,
  data = petitionville, family = binomial)
anova(m_p1p5_2h, test = "Chisq")

m_p2_2h_2i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    p1p5 + p2 * (cos(doy * 1 * 2 * pi/366) +
                   sin(doy * 1 * 2 * pi/366) +
                   cos(doy * 2 * 2 * pi/366) +
                   sin(doy * 2 * 2 * pi/366)),
  data = petitionville, family = binomial)
anova(m_p2_2h_2i, test = "Chisq")

m_p2p5_2h_2i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    p1p5 + p2p5 * (cos(doy * 1 * 2 * pi/366) +
                     sin(doy * 1 * 2 * pi/366) +
                     cos(doy * 2 * 2 * pi/366) +
                     sin(doy * 2 * 2 * pi/366)),
  data = petitionville, family = binomial)
anova(m_p2p5_2h_2i, test = "Chisq")

m_p2p5_2h_1i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    p1p5 + p2p5 * (cos(doy * 1 * 2 * pi/366) +
                     sin(doy * 1 * 2 * pi/366)),
  data = petitionville, family = binomial)
anova(m_p2p5_2h_1i, test = "Chisq")

m_p3p5_2h_2i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    p1p5 + p2p5 + p3p5 * (cos(doy * 1 * 2 * pi/366) +
                     sin(doy * 1 * 2 * pi/366) +
                     cos(doy * 2 * 2 * pi/366) +
                     sin(doy * 2 * 2 * pi/366)),
  data = petitionville, family = binomial)
anova(m_p3p5_2h_2i, test = "Chisq")

m_p3p5_2h_1i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366)) + 
    p1p5 + p2p5 + p3p5 * (cos(doy * 1 * 2 * pi/366) +
                            sin(doy * 1 * 2 * pi/366)),
  data = petitionville, family = binomial)
anova(m_p3p5_2h_1i, test = "Chisq")

AIC(m_p1_3h_3i, m_p1_2h_2i, m_p1p5_2h_2i, m_p1p5_2h_1i, m_p1p5_2h, m_p2_2h_2i, m_p2p5_2h_2i, m_p2p5_2h_1i, m_p3p5_2h_2i, m_p3p5_2h_1i) %>% arrange(AIC)
BIC(m_p1_3h_3i, m_p1_2h_2i, m_p1p5_2h_2i, m_p1p5_2h_1i, m_p1p5_2h, m_p2_2h_2i, m_p2p5_2h_2i, m_p2p5_2h_1i, m_p3p5_2h_2i, m_p3p5_2h_1i) %>% arrange(BIC)

# Choice either: 1) hybrid second-order (w, wd, dd) with 2 harmonics and no interactions
#                2) hybrid second-order (w, wd, dd) with 2 harmonics and interactions on the first harmonic


# Quanaminthe -------------------------------------------------------------

quanaminithe <- daily %>% 
  filter(station == "Quanaminthe")

m_p1_3h_3i <- glm(
  rainday1 ~
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p1_3h_3i, test = "Chisq")

m_p1_3h_1i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) +
    p1 * (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p1_3h_1i, test = "Chisq")

m_p1p5_3h_3i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5 * (cos(doy * 1 * 2 * pi/366) +
              sin(doy * 1 * 2 * pi/366) +
              cos(doy * 2 * 2 * pi/366) +
              sin(doy * 2 * 2 * pi/366) +
              cos(doy * 3 * 2 * pi/366) +
              sin(doy * 3 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p1p5_3h_3i, test = "Chisq")

m_p1p5_3h_1i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5 * (cos(doy * 1 * 2 * pi/366) +
              sin(doy * 1 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p1p5_3h_1i, test = "Chisq")

m_p1p5_4h_1i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366) +
            cos(doy * 4 * 2 * pi/366) +
            sin(doy * 4 * 2 * pi/366)) + 
    p1p5 * (cos(doy * 1 * 2 * pi/366) +
              sin(doy * 1 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p1p5_4h_1i, test = "Chisq")


m_p2_3h_3i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5 + p2 * (cos(doy * 1 * 2 * pi/366) +
                   sin(doy * 1 * 2 * pi/366) +
                   cos(doy * 2 * 2 * pi/366) +
                   sin(doy * 2 * 2 * pi/366) +
                   cos(doy * 3 * 2 * pi/366) +
                   sin(doy * 3 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p2_3h_3i, test = "Chisq")

m_p2_3h_1i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5 + p2 * (cos(doy * 1 * 2 * pi/366) +
                   sin(doy * 1 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p2_3h_1i, test = "Chisq")

m_p2p5_3h_3i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5 + p2p5 * (cos(doy * 1 * 2 * pi/366) +
                     sin(doy * 1 * 2 * pi/366) +
                     cos(doy * 2 * 2 * pi/366) +
                     sin(doy * 2 * 2 * pi/366) +
                     cos(doy * 3 * 2 * pi/366) +
                     sin(doy * 3 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p2p5_3h_3i, test = "Chisq")

m_p2p5_3h_1i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5 + p2p5 * (cos(doy * 1 * 2 * pi/366) +
                     sin(doy * 1 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p2p5_3h_1i, test = "Chisq")

m_p3p5_3h_3i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5 + p2p5 + p3p5 * (cos(doy * 1 * 2 * pi/366) +
                            sin(doy * 1 * 2 * pi/366) +
                            cos(doy * 2 * 2 * pi/366) +
                            sin(doy * 2 * 2 * pi/366) +
                            cos(doy * 3 * 2 * pi/366) +
                            sin(doy * 3 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p3p5_3h_3i, test = "Chisq")

m_p3p5_3h_1i <- glm(
  rainday1 ~
    p1 + (cos(doy * 1 * 2 * pi/366) +
            sin(doy * 1 * 2 * pi/366) +
            cos(doy * 2 * 2 * pi/366) +
            sin(doy * 2 * 2 * pi/366) +
            cos(doy * 3 * 2 * pi/366) +
            sin(doy * 3 * 2 * pi/366)) + 
    p1p5 + p2p5 + p3p5 * (cos(doy * 1 * 2 * pi/366) +
                            sin(doy * 1 * 2 * pi/366)),
  data = quanaminithe, family = binomial)
anova(m_p3p5_3h_1i, test = "Chisq")

AIC(m_p1_3h_3i, m_p1_3h_1i, m_p1p5_4h_1i, m_p1p5_3h_3i, m_p1p5_3h_1i, m_p2_3h_3i, m_p2_3h_1i, m_p2p5_3h_3i, m_p2p5_3h_1i, m_p3p5_3h_3i, m_p3p5_3h_1i) %>% arrange(AIC)
BIC(m_p1_3h_3i, m_p1_3h_1i, m_p1p5_4h_1i, m_p1p5_3h_3i, m_p1p5_3h_1i, m_p2_3h_3i, m_p2_3h_1i, m_p2p5_3h_3i, m_p2p5_3h_1i, m_p3p5_3h_3i, m_p3p5_3h_1i) %>% arrange(BIC)
# 

# Choice either: 1) first-order with 3 harmonics and interactions on the first harmonic
#                2) hybrid second-order (w, wd, dd) with 3 harmonics and interactions on the first harmonic
















for(s in seq_along(stations)) {
  predict_df <- data.frame(station = stations[s], s_doy = 1:366,
                           s_doy_date = as.Date(1:366, origin = as.Date("1999/12/31")))
  dat <- haiti_markov %>%
    filter(station == stations[s])
  
  zero_order_station <- glm(f_zero_order_station, data = dat, family = binomial)
  zero_order_product <- glm(f_zero_order_product, data = dat, family = binomial)
  #print(anova(zero_order_station, test="Chisq"))
  predict_df[["station_fit"]] <- predict(zero_order_station, newdata = predict_df,
                                         type = "response")
  predict_df[["chirps_fit"]] <- predict(zero_order_product, newdata = predict_df,
                                        type = "response")
  
  f_zero_order_product_2thres <- update.formula(f_zero_order_station, pr_rainday2 ~ .)
  f_zero_order_product_3thres <- update.formula(f_zero_order_station, pr_rainday3 ~ .)
  f_zero_order_product_4thres <- update.formula(f_zero_order_station, pr_rainday4 ~ .)
  f_zero_order_product_5thres <- update.formula(f_zero_order_station, pr_rainday5 ~ .)
  fms_thres <- list(f_zero_order_product_2thres, f_zero_order_product_3thres,
                    f_zero_order_product_4thres, f_zero_order_product_5thres)
  for(j in seq_along(fms_thres)) {
    zero_order <- glm(fms_thres[[j]], data = dat, family = binomial)
    predict_df[[paste0("chirps", "_", j + 1, "thres")]] <- predict(zero_order,
                                                                   newdata = predict_df,
                                                                   type = "response")
  }
  
  predict_stack <- predict_df %>% melt(id.vars = c("station", "s_doy", "s_doy_date"), 
                                       variable.name = "product", value.name = "prob")
  
  predict_stack$product <- as.character(predict_stack$product)
  predict_stack_lst[[length(predict_stack_lst) + 1]] <- predict_stack
  # Plot small amounts
  # g <- ggplot(predict_stack, aes(x = s_doy, y = prob, colour = type)) +
  #   geom_line() +
  #   facet_wrap(~product2) +
  #   scale_color_manual(values = c("black", c25[1:7])) +
  #   ggtitle(paste("Chance of rain:", stations[s]))
  # ggsave(here("results", "haiti", paste0("haiti_", "markov_zero", stations[s], ".png")), 
  #        plot = g, width = 12, height = 6)
}
predict_stack_all <- bind_rows(predict_stack_lst)
predict_stack_all$product <- factor(predict_stack_all$product)
predict_stack_all$product <- relevel(predict_stack_all$product, "station_fit")

dat <- predict_stack_all %>% filter(product %in% c("station_fit", "chirps_fit"))
g <- ggplot(dat, aes(x = s_doy_date, y = prob, colour = product)) +
  geom_line(size = 0.8) +
  facet_wrap(~station) +
  scale_color_manual(name = "Source", labels = c("Station", "CHIRPS"), values = c("black", c25[1:4])) +
  scale_x_date(name  ="Date", date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(name = "Probability of a rain day", breaks = seq(0, 1, 0.1), limits = c(0, NA)) +
  ggtitle(paste("Probability of a rain day: Station vs CHIRPS"))
print(g)

dat <- predict_stack_all
g <- ggplot(dat, aes(x = s_doy_date, y = prob, colour = product, size = product)) +
  geom_line() +
  facet_wrap(~station) +
  scale_size_manual(values = c(0.8, rep(0.6, 5))) +
  scale_color_manual(values = c("black", c25[1:5])) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  ggtitle(paste("Chance of rain"))
print(g)