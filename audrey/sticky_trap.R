library(tidyverse)
library(readxl)

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

    print(x$side_or_trapnum[trapnum_in_id_col])

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
    distinct()

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
    distinct()

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
    distinct()

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
    distinct()

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
    distinct()

x_individual = bind_rows(x1, x2, x3, x4, x5, x6, x9, xh)

# read google sheet ####

goog = read_csv('sticky_trap/HB_WaTER_Stickytrap_mastersheet - Bugs to Upload.csv') %>%
    rename(sample_id = 'Sample ID',
           side_or_trapnum = 'Side/ Trap #') %>%
    rename_with(tolower) %>%
    mutate(date = as.Date(paste(year, month, day), format = '%Y %m %d'),
           watershed = as.character(watershed)) %>%
    select(-month, -day, -year, -`...15`) %>%
    distinct()


# compare combined vs individual watershed sheets ####

nrow(x_combined)
nrow(x_individual)

anti_join(x_combined, x_individual) %>% dup() %>%
    write_csv('sticky_trap/out/rows_in_sheet1_but_not_watershed_sheets.csv')

anti_join(x_individual, x_combined) %>% dup() %>%
    write_csv('sticky_trap/out/rows_in_watershed_sheets_but_not_sheet1.csv')

# compare combined dataset vs database ####

nrow(x_combined)
nrow(d)

#rows present in one set and not the other
x_comb_only = anti_join(x_combined, select(d, -sample_id))
d_only = anti_join(select(d, -sample_id), x_combined)

write_csv(x_comb_only, 'sticky_trap/out/rows_in_sheet1_but_not_database.csv')
write_csv(d_only, 'sticky_trap/out/rows_in_database_but_not_sheet1.csv')

#same, but less strict. ignoring side_or_trapnum
x_comb_only = anti_join(x_combined, select(d, -sample_id),
                        by = c('watershed', 'date'))
d_only = anti_join(select(d, -sample_id), x_combined,
                   by = c('watershed', 'date'))


# compare individual watershed sheets vs database ####

nrow(x_individual)
nrow(d)

#rows present in one set and not the other
x_indiv_only = anti_join(x_individual, d) %>% dup()
d_only = anti_join(d, x_individual,
                   by = c('sample_id', 'side_or_trapnum', 'watershed', 'date'))

write_csv(x_indiv_only, 'sticky_trap/out/rows_in_watershed_sheets_but_not_database.csv')
write_csv(d_only, 'sticky_trap/out/rows_in_database_but_not_watershed_sheets.csv')

#same, but less strict. ignoring side_or_trapnum
x_indiv_only = anti_join(x_individual, d,
                         by = c('sample_id', 'watershed', 'date'))
d_only = anti_join(d, x_individual,
                   by = c('sample_id', 'watershed', 'date'))

#same, but joining on sample_id only
x_indiv_only = anti_join(x_individual, d, by = 'sample_id')
d_only = anti_join(d, x_individual, by = 'sample_id')


# compare google mastersheet vs database ####

nrow(goog)
nrow(d)

goog_only = anti_join(goog, d, by = 'sample_id')
d_only = anti_join(d, goog, by = 'sample_id')

write_csv(d_only, 'sticky_trap/out/rows_in_database_but_not_googlesheet.csv')

# identify duplicates ####

#from excel file tab 1 (no dupes)
searchcols = select(x_combined, watershed, date, side_or_trapnum)
x_combined[duplicated(searchcols) | duplicated(searchcols, fromLast = TRUE), ] %>%
    arrange(watershed, side_or_trapnum, date)
    # write_csv('out/dupes_from_tab1.csv')

#from compiled individual tabs
searchcols = select(x_individual, sample_id, watershed, date, side_or_trapnum)
x_individual[duplicated(searchcols) | duplicated(searchcols, fromLast = TRUE), ] %>%
    arrange(watershed, side_or_trapnum, date, sample_id) %>%
    write_csv('sticky_trap/out/dupes_from_indiv_sheets.csv')

#from google sheet
searchcols = select(goog, sample_id, watershed, date, side_or_trapnum)
goog[duplicated(searchcols) | duplicated(searchcols, fromLast = TRUE), ] %>%
    arrange(watershed, side_or_trapnum, date, sample_id) %>%
    write_csv('sticky_trap/out/dupes_from_gsheet.csv')


