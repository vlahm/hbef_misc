library(tidyverse)
library(RMariaDB)

options(readr.show_progress = FALSE,
        readr.show_col_types = FALSE)

# setwd('~/git/hbef/hbef_misc/audrey')
setwd('~/hbef_misc/audrey')
pass <- readLines('../../RMySQL.config')

chla <- map_dfr(list.files('datasets_to_integrate/final data',
                           pattern = 'chla_mgm2_[0-9]{4}\\.csv',
                           full.names = TRUE),
                read_csv)

#handle known duplicates
dup_ids1 = c("CH200581", "CH200582", "CH200583", "CH200584", "CH200586", "CH200585", "CH200587")
chla[chla$SampleID %in% dup_ids1, 'DATE'] = as.Date('2020-09-21')
dup_ids2 = c("CH220154")
chla[chla$SampleID %in% dup_ids2, 'DATE'] = as.Date('2022-06-20')

if(any(duplicated(select(chla, DATE, `WEIR-REP`)))){
    ids = chla[duplicated(select(chla, DATE, `WEIR-REP`)), 'SampleID', drop = TRUE]
    # ddup = select(chla, DATE, `WEIR-REP`)
    # dups <- chla[duplicated(ddup) | duplicated(ddup, fromLast = TRUE), ] %>%
    #     arrange(DATE, `WEIR-REP`)

    if(length(ids)) stop('unhandled duplicates detected')
}

warning('currently removing rep == "WM-zero" because there are only 7 such records (2023-06-08)')

chla = chla %>%
    select(date = DATE, chla = value_mgm2, site = weir, rep) %>%
    filter(rep != 'WM-zero') %>%  #only 7 of these atm
    pivot_wider(names_from = rep, values_from = chla,
                names_prefix = 'chla_')

con <- dbConnect(MariaDB(),
                 user = 'root',
                 password = pass,
                 host = 'localhost',
                 dbname = 'hbef')

# #first time only
# dbExecute(con, 'alter table current add column chla_M decimal(8,4);')
# dbExecute(con, 'alter table current add column chla_T decimal(8,4);')
# dbExecute(con, 'alter table current add column chla_MT decimal(8,4);')
# dbExecute(con, 'alter table current add column chla_WM decimal(8,4);')

curr <- DBI::dbReadTable(con, 'current') %>%
    as_tibble()

chla_with_time <- curr %>%
    select(date, site, timeEST) %>%
    group_by(date, site) %>%
    filter(first(timeEST == min(timeEST))) %>%
    ungroup() %>%
    distinct() %>%
    right_join(chla, by = c('date', 'site')) %>%
    mutate(matchcol = 1)

curr_ <- curr %>%
    select(-starts_with('chla')) %>%
    mutate(matchcol = 0) %>%
    group_by(date, site) %>%
    mutate(matchcol = ifelse(row_number() == 1, 1, matchcol)) %>%
    ungroup() %>%
    left_join(chla_with_time, by = c('date', 'site', 'timeEST', 'matchcol')) %>%
    select(-matchcol)

# compare::compare(curr_, curr)

dbExecute(con, 'delete from current;')
dbWriteTable(con, 'current', curr_, append = TRUE, row.names = FALSE)
dbDisconnect(con)

