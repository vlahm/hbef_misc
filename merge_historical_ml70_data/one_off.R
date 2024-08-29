#historical mirror lake outlet (ML70) data acquired from
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-hbr.86.7
#on 2024-08-29

setwd('~/git/hbef/hbef_misc/merge_historical_ml70_data/')
dbname <- 'hbef'

library(tidyverse)
library(RMariaDB)
library(lubridate)

con <- dbConnect(MariaDB(),
                 user = 'root',
                 password = readLines('../../RMySQL.config'),
                 host = 'localhost',
                 dbname = dbname)

#load ML70 data, acquired from link above
ml70 <- read_csv('ml-out.csv', show_col_types = FALSE) %>%
    mutate(across(everything(), ~if_else(.x == -3, NA, .x)),
           date = ymd(paste(yr, mo, dy, sep = '-'))) %>%
    group_by(date) %>%
    summarize(across(everything(),
                     ~mean(.x, na.rm = TRUE)),
              .groups = 'drop') %>%
    mutate(across(-date,
                  ~if_else(is.na(.x), NA_real_, .x))) %>%
    rowwise() %>%
    mutate(site = 'ML70',
           uniqueID = paste(site,
                            gsub('\\-', '', date),
                            'NA',
                            sep = '_')) %>%
    ungroup() %>%
    select(-any_of(c('ws', 'yr', 'mo', 'dy')))

#insert missing columns
d <- DBI::dbReadTable(con, 'historical') %>%
    as_tibble() %>%
    slice(0) %>%
    bind_rows(ml70)

#append to db
dbWriteTable(con, 'historical', d,
             append = TRUE)

dbDisconnect(con)
