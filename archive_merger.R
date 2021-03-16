library(tidyverse)
library(lubridate)

#TODO: hook this up to database. change old_dates anywhere?

# take 1 (OBSOLETE; starting from excel file with hidden columns) ####

# library(readxl)
# library(openxlsx)

arch = read_xlsx('~/Downloads/HB physical archives stream samples-1.xlsx',
                 skip = 2,
                 col_types = c(watershed = 'text', bin = 'numeric',
                               barcode = 'numeric', `weight g` = 'numeric',
                               `date weighed` = 'text', `time weighed` = 'text',
                               `sample date` = 'date', `time EST` = 'text',

                               #these are hidden columns
                               `bottle type` = 'text', `old date` = 'text',
                               `notes sample` = 'text', condition = 'text',

                               `bottle type` = 'text', `old date` = 'text',
                               `notes sample condition` = 'text'))


# arch = read.xlsx('~/Downloads/HB physical archives stream samples final.xlsx',
#                  startRow = 3,
#                  detectDates = FALSE)

# take 2 (starting from CSV with unneeded columns removed) ####

arch = read_csv('~/Downloads/HB physical archives stream samples.csv',
                skip = 2, col_types = 'cnnnccccccc') %>%
    rename_with(~gsub('\\s+', '_', .))

arch$time_EST[arch$time_EST == -9999] = NA
arch$bottle_type = str_replace(arch$bottle_type,
                               '[nN]algene ?([0-9]+)', 'Nalgene\\1')
arch$bottle_type = str_replace(arch$bottle_type, '([0-9]+)mlNM', 'narrow\\1')

arch = arch %>%
    mutate(watershed = gsub('ws', 'W', watershed),
           sample_date = mdy(sample_date),
           date_weighed = mdy(date_weighed),
           old_date = mdy(old_date),
           time_EST = str_pad(time_EST, 4, 'left', '0'),
           time_EST = paste0(substr(time_EST, 1, 2), ':',
                             substr(time_EST, 3, 4), ':00'))

# arch[!is.na(arch$old_date),] %>% select(watershed, sample_date, time_EST, old_date)
