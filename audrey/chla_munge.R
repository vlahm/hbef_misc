library(tidyverse)

a = read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2018_2020curve.csv')
b = read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2018.csv')
a = read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2019_2020curve.csv')
b = read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2019.csv')
a = read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2020_2020curve.csv')
b = read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2020.csv')
a = read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2021_2020curve.csv')
b = read_csv('datasets_to_integrate/final data/hbwtr_chla_mgm2_2021.csv')
a
b

compare::compare(a, b, allowAll=T)
compare::compare(b, a, allowAll=T)

# filter(b, SampleID == 'CH190001')
anti_join(a, b, by = 'SampleID')
anti_join(b, a, by = 'SampleID')
