library(tidyverse)
library(RMariaDB)

pass <- readLines('../../RMySQL.config')

chla <- bind_rows(read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2018.csv'),
                  read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2019.csv'),
                  read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2020.csv'))

#handle known duplicates
if(any(duplicated(select(chla, DATE, `WEIR-REP`)))){
    ids = chla[duplicated(select(chla, DATE, `WEIR-REP`)), 'SampleID', drop = TRUE]
    if(setequal(ids, c("CH200581", "CH200582", "CH200583", "CH200584", "CH200586", "CH200585", "CH200587"))){
        chla[chla$SampleID %in% ids, 'DATE'] = as.Date('2020-09-21')
    } else {
        stop('unhandled duplicates detected')
    }
}

warning('currently removing rep == "WM-zero" because there are only 7 such records (2023-06-08)')

chla = chla %>%
    select(date = DATE, chla = value_mgm2, site = weir, rep) %>%
    filter(rep != 'WM-zero') %>%  #only 7 of these atm
    pivot_wider(names_from = rep, values_from = chla,
                names_prefix = 'chla_')

stop('the database editing portion of this script is disabled to be safe, as it was only intended to be run once')

con <- dbConnect(MariaDB(),
                 user = 'root',
                 password = pass,
                 host = 'localhost',
                 dbname = 'hbef')

# dbExecute(con, 'create table chla (id int(8) auto_increment, site varchar(8), date date, chla double, rep varchar(18), primary key(id));')
# dbWriteTable(con, 'chla', chla, append = TRUE, row.names = FALSE)
# dbDisconnect(con)

dbExecute(con, 'alter table current add column chla_M decimal(8,4);')
dbExecute(con, 'alter table current add column chla_T decimal(8,4);')
dbExecute(con, 'alter table current add column chla_MT decimal(8,4);')
dbExecute(con, 'alter table current add column chla_WM decimal(8,4);')

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

dbExecute(con, 'delete from current;')
dbWriteTable(con, 'current', curr_, append = TRUE, row.names = FALSE)
dbDisconnect(con)
