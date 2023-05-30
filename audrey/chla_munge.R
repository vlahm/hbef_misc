library(tidyverse)
library(RMariaDB)

pass <- readLines('../../RMySQL.config')

HERE: need to extract rep == 'MT' and save it in separate db table.
also need to find out about missing timeEST and averaging by date

chla <- bind_rows(read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2018.csv'),
                  read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2019.csv'),
                  read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2020.csv')) %>%
    filter(rep %in% c('M', 'T')) %>%
    select(date = DATE, chla = value_mgm2, site = weir, rep) %>%
    group_by(date, site) %>%
    summarize(chla = mean(chla, na.rm = TRUE)) %>%
    ungroup()

con <- dbConnect(MariaDB(),
                 user = 'root',
                 password = pass,
                 host = 'localhost',
                 dbname = 'hbef')

curr <- DBI::dbReadTable(con, 'current') %>%
    as_tibble()

# chla[duplicated(chla) | duplicated(chla, fromLast = T),]
chla_with_time <- curr %>%
    select(date, site, timeEST) %>%
    group_by(date, site) %>%
    filter(first(timeEST == min(timeEST))) %>%
    ungroup() %>%
    distinct() %>%
    right_join(chla, by = c('date', 'site')) %>%
    mutate(matchcol = 1)

curr_ <- curr %>%
    select(-chla) %>%
    mutate(matchcol = 0) %>%
    group_by(date, site) %>%
    mutate(matchcol = ifelse(row_number() == 1, 1, matchcol)) %>%
    ungroup() %>%
    left_join(chla_with_time, by = c('date', 'site', 'timeEST', 'matchcol')) %>%
    select(-matchcol)

dbWriteTable(con, 'current', curr_, overwrite = TRUE, row.names = FALSE)
dbDisconnect(con)
