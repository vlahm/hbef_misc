library(tidyverse)
library(readxl)
library(openxlsx)

#TODO: warn about collapsed columns I-L

#there's a corrupt header row in this excel file, so starting from csv instead
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

#TODO: take care of nalgene500 vs Nalgene500

# arch = read.xlsx('~/Downloads/HB physical archives stream samples final.xlsx',
#                  startRow = 3,
#                  detectDates = FALSE)


arch %>%


arch = as_tibble(arch)
