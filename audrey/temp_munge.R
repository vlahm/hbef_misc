library(tidyverse)
library(RMariaDB)

options(readr.show_progress = FALSE,
        readr.show_col_types = FALSE)

pass <- readLines('../../RMySQL.config')

fffs = list.files('datasets_to_integrate/6_Sensor Data/',
                  pattern = '^miniDOT ', full.names = TRUE)

HERE. NOT SURE THIS IS REALLY GETTING EVERYTHING

d = tibble()
for(fff in fffs){

    ffs = list.files(fff, pattern = '^W[0-9]', full.names = TRUE)
    print(fff)
    # ff=ffs[1]

    for(ff in ffs){
        print(ff)

        ff_ = list.files(ff, pattern = '^[0-9]+', full.names = TRUE)
        fs = list.files(ff_, full.names = T, pattern = '\\.txt$')

        for(f in fs){
            d = read_csv(f, skip = 2) %>%
                mutate(datetime = as.POSIXct(`Time (sec)`)) %>%
                select(datetime, temp = `T (deg C)`) %>%
                mutate(site = 'W2') %>%
                bind_rows(d)
        }
    }
}

# distinct(w2)

con <- dbConnect(MariaDB(),
                 user = 'root',
                 password = pass,
                 host = 'localhost',
                 dbname = 'hbef')

dbWriteTable(con, 'sensor5_light', temp, overwrite = TRUE, row.names = FALSE)
dbDisconnect(con)

