library(tidyverse)
library(readxl)
library(RMariaDB)

# setup ####
options(readr.show_progress = FALSE,
        readr.show_col_types = FALSE)

setwd('~/hbef_misc/audrey')
pass <- readLines('../../RMySQL.config')

dup = function(x, rearrange = TRUE){

    if(! 'sample_id' %in% colnames(x)) warning('no sample_id column')

    x$dup = 0
    searchcols = select(x, any_of(c('sample_id', 'watershed', 'date', 'side_or_trapnum')))
    x$dup[duplicated(searchcols) | duplicated(searchcols, fromLast = TRUE)] <- 1

    if(rearrange){
        if('sample_id' %in% colnames(x)){
            x = arrange(x, watershed, side_or_trapnum, date, sample_id)
        } else {
            x = arrange(x, watershed, side_or_trapnum, date)
        }
    }

    message(paste(sum(x$dup), 'dupes'))

    return(x)
}

filter_dupes = function(x, rearrange = TRUE){

    searchcols = select(x, any_of(c('sample_id', 'watershed', 'date', 'side_or_trapnum')))
    x = x[duplicated(searchcols) | duplicated(searchcols, fromLast = TRUE), ]

    if(rearrange){
        if('sample_id' %in% colnames(x)){
            x = arrange(x, watershed, side_or_trapnum, date, sample_id)
        } else {
            x = arrange(x, watershed, side_or_trapnum, date)
        }
    }

    return(x)
}

fix_id = function(x){

    trapnum_in_id_col = grepl('^[0-9]{1,2}$', x$sample_id)
    side_ab = grepl('^[AB]$', x$side_or_trapnum)
    replace_ = trapnum_in_id_col & ! side_ab

    # print(x$side_or_trapnum[trapnum_in_id_col])

    x$side_or_trapnum[replace_] = x$sample_id[replace_]
    x$sample_id[replace_] = NA

    return(x)
}

# load stickytrap table from database ####

d = read_csv('sticky_trap/sticky_trap_counts.csv') %>%
    mutate(watershed = as.character(watershed)) %>%
    arrange(watershed, side_or_trapnum, date, sample_id)

# read first (combined) tab of ARCHIVED_Sticky trap data Hubbard Brook.xlsx ####

x_combined = read_xlsx('sticky_trap/ARCHIVED_Sticky trap data Hubbard Brook.xlsx',
               col_types = 'text') %>%
    mutate(caddisfly_small = NA) %>%
    select(side_or_trapnum = 'Trap #',
           watershed = 'Watershed',
           date = 'Date...3',
           dipteran_large = 'Dipteran-Large',
           terrestrial_large = 'Terrestrial- Large',
           caddisfly_large = 'Caddisfly-Large',
           mayfly_large = 'Mayfly-Large',
           stonefly_large = 'Stonefly-Large',
           other_large = 'Other-Large',
           dipteran_small = 'Dipteran- Small',
           terrestrial_small = 'Terrestrial- Small',
           caddisfly_small,
           other_small = 'Other-Small') %>%
    filter(! grepl('^[A-Z]$', date)) %>%
    mutate(watershed = sub('\\.0$', '', watershed),
           date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(ends_with(c('large', 'small')), as.numeric)) %>%
    distinct()

# read individual watershed sheets of ARCHIVED_Sticky trap data Hubbard Brook.xlsx ####

x1 = read_xlsx('sticky_trap/ARCHIVED_Sticky trap data Hubbard Brook.xlsx',
               sheet = 'Watershed 1', skip = 1, col_types = 'text') %>%
    select(sample_id = 'Sample #',
           side_or_trapnum = 'Side',
           watershed = 'WS',
           date = 'Date...3',
           dipteran_large = 'Dipteran...9',
           terrestrial_large = 'Terrestrial',
           caddisfly_large = 'Caddisfly...11',
           mayfly_large = 'Mayfly',
           stonefly_large = 'Stonefly',
           other_large = 'Other...14',
           dipteran_small = 'Dipteran...15',
           terrestrial_small = 'Terr',
           caddisfly_small = 'Caddisfly...17',
           other_small = 'Other...18') %>%
    filter(! grepl('^[A-Z]$', date)) %>%
    mutate(sample_id = sub('\\.0$', '', sample_id),
           watershed = sub('\\.0$', '', watershed),
           date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(ends_with(c('large', 'small')), as.numeric)) %>%
    distinct() %>%
    fix_id()

x2 = read_xlsx('sticky_trap/ARCHIVED_Sticky trap data Hubbard Brook.xlsx',
               sheet = 'Watershed 2', skip = 1, col_types = 'text') %>%
    select(sample_id = 'Sample ID',
           side_or_trapnum = 'Side',
           watershed = 'Watershed',
           date = 'm/d/y',
           dipteran_large = 'Dipteran...6',
           terrestrial_large = 'Terrestrial...7',
           caddisfly_large = 'Caddisfly...8',
           mayfly_large = 'Mayfly',
           stonefly_large = 'Stonefly',
           other_large = 'Other...11',
           dipteran_small = 'Dipteran...12',
           terrestrial_small = 'Terrestrial...13',
           caddisfly_small = 'Caddisfly...14',
           other_small = 'Other...15') %>%
    mutate(sample_id = sub('\\.0$', '', sample_id),
           watershed = sub('\\.0$', '', watershed),
           date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(ends_with(c('large', 'small')), as.numeric)) %>%
    distinct()

x3 = read_xlsx('sticky_trap/ARCHIVED_Sticky trap data Hubbard Brook.xlsx',
               sheet = 'Watershed 3', skip = 1, col_types = 'text') %>%
    select(sample_id = 'Sample ID',
           side_or_trapnum = 'Trap #',
           watershed = 'Watershed',
           date = 'Date...5',
           dipteran_large = 'Dipteran...6',
           terrestrial_large = 'Terrestrial...7',
           caddisfly_large = 'Caddisfly',
           mayfly_large = 'Mayfly',
           stonefly_large = 'Stonefly',
           other_large = 'Other...11',
           dipteran_small = 'Dipteran...12',
           terrestrial_small = 'Terrestrial...13',
           caddisfly_small = '...14',
           other_small = 'Other...15') %>%
    mutate(sample_id = sub('\\.0$', '', sample_id),
           watershed = sub('\\.0$', '', watershed),
           date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(ends_with(c('large', 'small')), as.numeric)) %>%
    distinct()

x4 = read_xlsx('sticky_trap/ARCHIVED_Sticky trap data Hubbard Brook.xlsx',
               sheet = 'Watershed 4', skip = 1, col_types = 'text') %>%
    select(sample_id = 'Sample ID',
           side_or_trapnum = 'Side',
           watershed = 'Watershed',
           date = 'Date...5',
           dipteran_large = 'Dipteran...6',
           terrestrial_large = 'Terrestrial...7',
           caddisfly_large = 'Caddisfly...8',
           mayfly_large = 'Mayfly',
           stonefly_large = 'Stonefly',
           other_large = 'Other...11',
           dipteran_small = 'Dipteran...12',
           terrestrial_small = 'Terrestrial...13',
           caddisfly_small = 'Caddisfly...14',
           other_small = 'Other...15') %>%
    mutate(sample_id = sub('\\.0$', '', sample_id),
           watershed = sub('\\.0$', '', watershed),
           date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(ends_with(c('large', 'small')), as.numeric)) %>%
    distinct() %>%
    fix_id()

x5 = read_xlsx('sticky_trap/ARCHIVED_Sticky trap data Hubbard Brook.xlsx',
               sheet = 'Watershed 5', skip = 1, col_types = 'text') %>%
    select(sample_id = 'Sample ID',
           side_or_trapnum = 'Side',
           watershed = 'Watershed',
           date = 'Date...5',
           dipteran_large = 'Dipteran...6',
           terrestrial_large = 'Terrestrial...7',
           caddisfly_large = 'Caddisfly...8',
           mayfly_large = 'Mayfly',
           stonefly_large = 'Stonefly',
           other_large = 'Other...11',
           dipteran_small = 'Dipteran...12',
           terrestrial_small = 'Terrestrial...13',
           caddisfly_small = 'Caddisfly...14',
           other_small = 'Other...15') %>%
    mutate(sample_id = sub('\\.0$', '', sample_id),
           watershed = sub('\\.0$', '', watershed),
           date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(ends_with(c('large', 'small')), as.numeric)) %>%
    distinct() %>%
    fix_id()

x6 = read_xlsx('sticky_trap/ARCHIVED_Sticky trap data Hubbard Brook.xlsx',
               sheet = 'Watershed 6', skip = 1, col_types = 'text') %>%
    select(sample_id = 'Sample ID #',
           side_or_trapnum = 'Side',
           watershed = 'Watershed',
           date = 'Date...5',
           dipteran_large = 'Dipteran...6',
           terrestrial_large = 'Terrestrial...7',
           caddisfly_large = 'Caddisfly...8',
           mayfly_large = 'Mayfly',
           stonefly_large = 'Stonefly',
           other_large = 'Other...11',
           dipteran_small = 'Dipteran...12',
           terrestrial_small = 'Terrestrial...13',
           caddisfly_small = 'Caddisfly...14',
           other_small = 'Other...15') %>%
    mutate(sample_id = sub('\\.0$', '', sample_id),
           watershed = sub('\\.0$', '', watershed),
           date = sub("^'", '', date),
           date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(ends_with(c('large', 'small')), as.numeric)) %>%
    distinct() %>%
    fix_id() %>%
    filter(! date == as.Date('109-04-01')) %>%
    filter(! date == as.Date('109-03-31'))

x9 = read_xlsx('sticky_trap/ARCHIVED_Sticky trap data Hubbard Brook.xlsx',
               sheet = 'Watershed 9', skip = 1, col_types = 'text') %>%
    select(sample_id = 'Sample ID',
           side_or_trapnum = 'Side',
           watershed = 'Watershed',
           date = 'Date...5',
           dipteran_large = 'Dipteran...6',
           terrestrial_large = 'Terrestrial...7',
           caddisfly_large = 'Caddisfly',
           mayfly_large = 'Mayfly',
           stonefly_large = 'Stonefly',
           other_large = 'Other...11',
           dipteran_small = 'Dipteran...12',
           terrestrial_small = 'Terrestrial...13',
           caddisfly_small = '...14',
           other_small = 'Other...15') %>%
    mutate(sample_id = sub('\\.0$', '', sample_id),
           watershed = sub('\\.0$', '', watershed),
           date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(ends_with(c('large', 'small')), as.numeric)) %>%
    distinct() %>%
    fix_id()

xh = read_xlsx('sticky_trap/ARCHIVED_Sticky trap data Hubbard Brook.xlsx',
               sheet = 'HBK', skip = 1, col_types = 'text') %>%
    select(sample_id = 'Sample #',
           side_or_trapnum = 'Trap #',
           watershed = 'Watershed',
           date = 'Date...5',
           dipteran_large = 'Dipteran...6',
           terrestrial_large = 'Terrestrial...7',
           caddisfly_large = 'Caddisfly...8',
           mayfly_large = 'Mayfly',
           stonefly_large = 'Stonefly',
           other_large = 'Other...11',
           dipteran_small = 'Dipteran...12',
           terrestrial_small = 'Terrestrial...13',
           caddisfly_small = 'Caddisfly...14',
           other_small = 'Other...15') %>%
    mutate(sample_id = sub('\\.0$', '', sample_id),
           watershed = sub('\\.0$', '', watershed),
           date = as.Date(as.numeric(date), origin = '1899-12-30'),
           across(ends_with(c('large', 'small')), as.numeric)) %>%
    distinct() %>%
    fix_id()

x_individual = bind_rows(x1, x2, x3, x4, x5, x6, x9, xh)

# read google sheet ####

goog = read_csv('sticky_trap/HB_WaTER_Stickytrap_mastersheet - Bugs to Upload.csv') %>%
    rename(sample_id = 'Sample ID',
           side_or_trapnum = 'Side/ Trap #') %>%
    rename_with(tolower) %>%
    mutate(date = as.Date(paste(year, month, day), format = '%Y %m %d'),
           watershed = as.character(watershed)) %>%
    select(-month, -day, -year, -`...15`) %>%
    distinct() %>%
    fix_id()


# combine all candidate records; filter definitely bogus records ####

candidates = bind_rows(x_combined, x_individual, goog) %>%
    distinct()

#rm records with no data and no id
check_cols = grep('date|watershed', colnames(candidates), value = TRUE, invert = TRUE)
rm_rows = apply(candidates[check_cols], 1, function(x) all(is.na(x)))
candidates = candidates[! rm_rows, ]

# find records with issues ####

id_normal1 = grepl('^ST[12][90][0-9]{4}$', candidates$sample_id) & grepl('^[AB]$', candidates$side_or_trapnum)
candidates[!id_normal1,]
id_normal2 = grepl('^[0-9]{1,2}$', candidates$side_or_trapnum) & is.na(candidates$sample_id)
wshed_normal = grepl('^[1234569]$', candidates$watershed) | grepl('^HBK$', candidates$watershed)
chill_rows = (id_normal1 | id_normal2) & wshed_normal

candidates[! chill_rows, ] %>%
    arrange(watershed, date, sample_id, side_or_trapnum) %>%
    relocate(sample_id, .before = 'side_or_trapnum') %>%
    write_csv('sticky_trap/out/to_investigate.csv')

#find records that might be new ####

anti_join(candidates[chill_rows, ], d) %>%
    dup() %>%
    arrange(watershed, date, sample_id, side_or_trapnum) %>%
    relocate(sample_id, .before = 'side_or_trapnum') %>%
    write_csv('sticky_trap/out/novel_records.csv')

anti_join(candidates[chill_rows, ], d,
          by = c('sample_id', 'side_or_trapnum', 'watershed', 'date')) %>%
    dup() %>%
    arrange(watershed, date, sample_id, side_or_trapnum) %>%
    relocate(sample_id, .before = 'side_or_trapnum') %>%
    write_csv('sticky_trap/out/novel_record_indices.csv')

#insert new records into database ####

new_records = anti_join(candidates[chill_rows, ], d) %>%
          # by = c('sample_id', 'side_or_trapnum', 'watershed', 'date')) %>%
    dup() %>%
    arrange(watershed, date, sample_id, side_or_trapnum) %>%
    relocate(sample_id, .before = 'side_or_trapnum') %>%
    filter(dup == 1 & if_any(ends_with(c('large', 'small')), ~! is.na(.)) | dup == 0) %>%
    filter(! (side_or_trapnum == 2 & watershed == 1 & date == as.Date('2018-06-18') & is.na(caddisfly_small))) %>%
    filter(! (side_or_trapnum == 3 & watershed == 1 & date == as.Date('2018-10-01') & is.na(caddisfly_small))) %>%
    filter(! (side_or_trapnum == 7 & watershed == 1 & date == as.Date('2018-10-01') & is.na(caddisfly_small))) %>%
    select(-dup)

stickytrap = bind_rows(d, new_records) %>%
    arrange(watershed, date, sample_id, side_or_trapnum)

con <- dbConnect(MariaDB(),
                 user = 'root',
                 password = pass,
                 host = 'localhost',
                 dbname = 'hbef')

dbExecute(con, 'delete from stickytrap;')
dbWriteTable(con, 'stickytrap', stickytrap, append = TRUE, row.names = FALSE)
dbDisconnect(con)
