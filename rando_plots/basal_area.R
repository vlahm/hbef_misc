library(tidyverse)
library(readxl)

d <- read_xlsx('~/Downloads/W1W5W6_VegetationSurveyData.xlsx')

d %>%
    filter(! is.na(Species)) %>% #there's one missing species field
    mutate(basal_area = 00007854 * Dbh^2) %>%
    group_by(WS, YEAR, Species) %>%
    summarize(total_basal_area = sum(basal_area),
              mean_basal_area = mean(basal_area),
              .groups = 'drop') %>%
    write_csv('/tmp/basal_area_by_wsyear.csv')
