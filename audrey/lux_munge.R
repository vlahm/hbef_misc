library(tidyverse)
library(RMariaDB)

options(readr.show_progress = FALSE,
        readr.show_col_types = FALSE)

pass <- readLines('../../RMySQL.config')

all_f <- c(list.files('datasets_to_integrate/HOBO data/2018', pattern = '\\.csv$', full.names = TRUE),
           list.files('datasets_to_integrate/HOBO data/2019', pattern = '\\.csv$', full.names = TRUE),
           list.files('datasets_to_integrate/HOBO data/2020', pattern = '\\.csv$', full.names = TRUE),
           list.files('datasets_to_integrate/HOBO data/2021', pattern = '\\.csv$', full.names = TRUE))

light <- tibble()
for(f in all_f){

    coresite <- str_detect(tolower(f), 'bear|paradise', negate = TRUE)

    if(coresite){
        site <- toupper(str_extract(f, '/([wW][0-9]).+$', 1))
        location <- toupper(str_extract(f, '/[wW][0-9]_?(A|B|weir).+$', 1))
    } else {
        site <- str_extract(tolower(f), '/(bear|paradise).+$', 1)
        location <- NA
    }

    light <- read_csv('datasets_to_integrate/HOBO data/2021/W1_weir_2021.csv', skip = 1) %>%
        select(datetime = 'Date Time, GMT-04:00',
               lux = 'Intensity, Lux (LGR S/N: 20203506, SEN S/N: 20203506)') %>%
        mutate(datetime = as.POSIXct(datetime, format = '%m/%d/%y %I:%M:%S %p',
                                     # origin = '1970-01-01', tz = 'Etc/GMT-4'),
                                     # it's actually in GMT-4, but mysql converts to UTC, so this eliminates need to convert twice
                                     origin = '1970-01-01', tz = 'UTC'),
               site = !!site,
               location = !!location) %>%
        bind_rows(light)
}

light <- light %>%
    distinct() %>%
    arrange(site, datetime, location) %>%
    rename(light_lux = lux)

con <- dbConnect(MariaDB(),
                 user = 'root',
                 password = pass,
                 host = 'localhost',
                 dbname = 'hbef')

if('sensor5_light' %in% dbListTables(con)){
    warning('table sensor5_light already exists. doing nothing')
} else {
    dbExecute(con, 'create table sensor5_light (id int(11) auto_increment, site varchar(8), datetime datetime, location varchar(4), light_lux double, primary key(id));')
    dbWriteTable(con, 'sensor5_light', light, append = TRUE, row.names = FALSE)
}

dbDisconnect(con)

