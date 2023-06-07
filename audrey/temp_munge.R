library(tidyverse)
library(RMariaDB)

options(readr.show_progress = FALSE,
        readr.show_col_types = FALSE)

#setup and helpers ####

pass <- readLines('../../RMySQL.config')

ws_namemap = c('W1' = 'peter',
               'W2' = 'quill',
               'W3' = 'rocket',
               'W4' = 'polis',
               'W6' = 'leia',
               'W9' = 'luke')

read_templogger = function(d, files){

    accum = tibble()
    for(f in files){

        header = read_lines(f, skip = 2, n_max = 1)
        if(! length(header)) next
        if(header == '') stop()
        if(is.na(header)) stop()
        if(header == "Time (sec)  T (deg C)  DO (mg/l)  Q ()"){
            d_ = as_tibble(read.csv(f, skip = 3, header = FALSE))
            colnames(d_) = c('Time (sec)', 'T (deg C)', 'DO (mg/l)', 'Q ()')
        } else {
            d_ = read_csv(f, skip = 2)
        }

        accum = d_ %>%
            mutate(datetime = as.POSIXct(`Time (sec)`)) %>%
            select(datetime, temp = `T (deg C)`) %>%
            mutate(site = ws_id) %>%
            bind_rows(accum)

    }

    d = bind_rows(d, accum)
    return(d)
}

get_ws_name = function(x){
    ws_id = tolower(basename(x)) %>%
        str_extract(paste(ws_namemap, collapse = '|')) %>%
        match(ws_namemap) %>%
        {ws_namemap[.]} %>%
        names()

    return(ws_id)
}

#munge standard (?) file organization ####

fffs = list.files('datasets_to_integrate/6_Sensor Data/',
                  pattern = '^miniDOT ', full.names = TRUE)

d = tibble()
for(fff in fffs){

    ffs = list.files(fff, pattern = '^[Ww][0-9]', full.names = TRUE)
    print(fff)

    for(ff in ffs){
        print(ff)

        ws_id = toupper(str_extract(basename(ff), '[wW][0-9]'))
        print(paste('WATERSHED', ws_id))
        ff_ = list.files(ff, full.names = TRUE)
        ffdir = Filter(function(x) file_test('-d', x), ff_)

        if(length(ffdir)){
            ffdir = Filter(function(x) grepl('^[0-9D]', basename(x)), ffdir)
            if(length(ffdir) > 1) stop()
            fs = list.files(ffdir, full.names = T, pattern = '\\.txt$')
        } else {
            fs = Filter(function(x) grepl('\\.txt$', x), ff_)
        }

        d = read_templogger(d, fs)
        rm(ws_id)
    }
}

#munge older arrangements ####

ffs2 = list.files('datasets_to_integrate/6_Sensor Data/Minidot/Start Temperature 2021 of minidots 09162021',
                    pattern = '^[0-9S]', full.names = TRUE)

for(ff in ffs2){
    print(ff)

    ws_id = get_ws_name(ff)
    fs = list.files(ff, full.names = TRUE, pattern = '\\.txt$')
    d = read_templogger(d, fs)
    rm(ws_id)
}

ffs3 = list.files('datasets_to_integrate/6_Sensor Data/Minidot/Start Temperature 2021 of minidots 09162021',
                    pattern = '^[0-9S]', full.names = TRUE)

for(ff in ffs3){
    print(ff)

    ws_id = get_ws_name(ff)
    fs = list.files(ff, full.names = TRUE, pattern = '\\.txt$')
    d = read_templogger(d, fs)
    rm(ws_id)
}

# more_files2 = list.files('datasets_to_integrate/6_Sensor Data/minidots 2017 HB/771948 Draco/7392-771948/',
#            pattern = '\\.txt$', full.names = TRUE)
#
# d = read_templogger(d, more_files2)

#final clean and database dump ####

d = distinct(d)
if(any(duplicated(select(d, datetime, site)))) stop('oi!')

d = d %>%
    select(watershedID = site, datetime, S4__TempC = temp) %>%
    arrange(site, datetime)

con <- dbConnect(MariaDB(),
                 user = 'root',
                 password = pass,
                 host = 'localhost',
                 dbname = 'hbef')

# if('sensor6_temp' %in% dbListTables(con)){
#     warning('table sensor6_temp already exists. doing nothing')
# } else {
#     dbExecute(con, 'create table sensor6_temp (id int(11) auto_increment, site varchar(8), datetime datetime, temp_C double, primary key(id));')
#     dbWriteTable(con, 'sensor6_temp', d, append = TRUE, row.names = FALSE)
# }
dbWriteTable(con, 'sensor4', d, append = TRUE, row.names = FALSE)

dbDisconnect(con)

###
dd <- DBI::dbReadTable(con, 'sensor4') %>%
    as_tibble()

HERE: APPENDED TO TABLE SENSOR4, BUT DUPLICATED SOME TIMESTAMPS THAT WAY.
FIND A WAY TO MERGE NEW TEMP DATA WITHOUT MESSING UP THE TABLE
