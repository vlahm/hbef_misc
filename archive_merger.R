library(tidyverse)
# library(readxl)
library(openxlsx)

#TODO: warn about collapsed columns I-L
#warn about cell with a newline in it on row 5665 col D

#there's a corrupt header row in this excel file, so starting from csv instead
arch = read_xlsx('~/Downloads/HB physical archives stream samples final.xlsx',
                 skip = 2,
                 col_types = c(watershed = 'text', bin = 'numeric',
                               barcode = 'numeric', `weight g` = 'numeric',
                               `date weighed` = 'text', `time weighed` = 'text',
                               `sample date` = 'date', `time EST` = 'text',
                               `bottle type` = 'text', `old date` = 'text',
                               `notes sample condition` = 'text'))

arch = read.xlsx('~/Downloads/HB physical archives stream samples final.xlsx',
                 startRow = 3,
                 detectDates = FALSE)


arch %>%


arch = as_tibble(arch)
