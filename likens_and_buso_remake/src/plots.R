library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(feather)

source('helpers.R')

## load data ####

s <- read_csv('data/HubbardBrook_weekly_stream_chemistry.csv')
p <- read_csv('data/HubbardBrook_weekly_precipitation_chemistry.csv',
              guess_max = 10000) %>%
    select(-all_of(c('barcode', 'fieldCode', 'notes', 'uniqueID', 'duplicate',
                   'sampleType', 'canonical', 'site'))) %>%
    group_by(date, timeEST) %>%
    summarize(across(-waterYr, ~mean(., na.rm = TRUE)),
              waterYr = first(waterYr),
              site = 'all',
              .groups = 'drop')

p0 <- read_csv('data/dailyGagePrecip1956-2024.csv')

q1 <- map_dfr(list.files('data', pattern = 'w[0-9]_.*?2012\\.csv', full.names = TRUE),
              read_csv)
### HERE: FORGET APPROXJOIN. JUST AVERAGE CHEM AND Q BY DAY AND JOIN
q1 <- q1 %>%
    mutate(site = paste0('W', WS)) %>%
    select(site,
q2 <- map_dfr(list.files('data', pattern = 'w[0-9]_.*?_5min\\.csv', full.names = TRUE),
              read_csv)

## plot 1 ####

spcond_s <- calc_vwc_wateryear(s, 'spCond')
spcond_p <- calc_vwc_wateryear(p, 'spCond')

filter(spcond_s, site == 'W6') %>%
    ggplot(aes(x = waterYr, y = spCond)) +
    geom_line() +
    geom_point()

spcond_p %>%
    ggplot(aes(x = waterYr, y = spCond)) +
    geom_line() +
    geom_point()

#uEq
