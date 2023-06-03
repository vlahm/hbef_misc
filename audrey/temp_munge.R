library(tidyverse)
library(RMariaDB)

options(readr.show_progress = FALSE,
        readr.show_col_types = FALSE)

pass <- readLines('../../RMySQL.config')

fffs = list.files('datasets_to_integrate/6_Sensor Data/',
                  pattern = '^miniDOT ', full.names = TRUE)

HERE. NOT SURE THIS IS REALLY GETTING EVERYTHING

d = tibble()
fff=fffs[1]
for(fff in fffs){

    ffs = list.files(fff, pattern = '^W[0-9]', full.names = TRUE)
    print(fff)
    ff=ffs[3]

    for(ff in ffs){
        print(ff)

        ws_id = str_extract(basename(ff), 'W[0-9]')
        ff_ = list.files(ff, pattern = '^[0-9D]', full.names = TRUE)
        fs = list.files(ff_, full.names = T, pattern = '\\.txt$')

        for(f in fs){

            header = read_lines(f, skip = 2, n_max = 1)
            if(header == "Time (sec)  T (deg C)  DO (mg/l)  Q ()"){
                d_ = as_tibble(read.csv(f, skip = 3, header = FALSE))
                colnames(d_) = c('Time (sec)', 'T (deg C)', 'DO (mg/l)', 'Q ()')
            } else {
                d_ = read_csv(f, skip = 2)
            }

            d = d_ %>%
                mutate(datetime = as.POSIXct(`Time (sec)`)) %>%
                select(datetime, temp = `T (deg C)`) %>%
                mutate(site = ws_id) %>%
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

