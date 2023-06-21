library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(readxl)

setwd('~/git/hbef/hbef_misc/temperature_maxima_plots/')

d = read_csv('HBEFdata_All_2023-06-21.csv') %>%
    select(site, date, temp)

d = readxl::read_xlsx('../lake_data_etc/data/ml70chem 1967_2014.xlsx', skip = 1) %>%
    mutate(site = 'ML70',
           date = as.Date(paste0(yr, '-01-01'))) %>%
    select(site, date, temp = `Temp °C`) %>%
    filter(temp > -100) %>%
    bind_rows(d) %>%
    arrange(site, date)

ylims = d %>%
    filter(site %in% c('HBK', 'W6', 'ML70'),
           ! is.na(temp),
           ! year(date) == 2023) %>%
    group_by(year(date)) %>%
    summarize(temp = max(temp, na.rm = TRUE)) %>%
    ungroup() %>%
    pull(temp) %>%
    range(na.rm = TRUE)

plot_temp = function(site, ylim = ylims, write = TRUE){

    years = sort(unique(year(d$date)))
    decades = years[years %% 10 == 0]

    plt = d %>%
        mutate(year = as.integer(year(date))) %>%
        filter(site == !!site,
               ! is.na(temp),
               year != 2023) %>%
        group_by(year) %>%
        summarize(temp = max(temp, na.rm = TRUE)) %>%
        ungroup() %>%
        complete(year = seq(min(.$year), max(.$year), 1)) %>%
        ggplot(aes(x = year, y = temp)) +
        geom_line() +
        labs(x = '', y = expression('Water temperature (' * degree * 'C)')) +
        scale_x_continuous(breaks = decades, labels = decades) +
        ylim(ylim[1], ylim[2]) +
        theme_light()

    if(write){
        ggsave(paste0(site, '.png'), plt, dpi = 300)
    } else {
        print(plt)
    }
}

plot_temp('W6')
plot_temp('HBK')
plot_temp('ML70')

# site = 'ML70'
# plt = d %>%
#     mutate(date = floor_date(date, unit = 'year')) %>%
#     filter(site == !!site,
#            ! is.na(temp),
#            year(date) != 2023) %>%
#     group_by(date) %>%
#     summarize(temp = max(temp, na.rm = TRUE)) %>%
#     ungroup() %>%
#     ggplot(aes(x = date, y = temp)) +
#     geom_line() +
#     labs(x = '', y = expression('Water temperature (' * degree * 'C)')) +
#     scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
#     ylim(ylims[1], ylims[2]) +
#     theme_light()
#
# ggsave(paste0(site, '.png'), plt, dpi = 300)
