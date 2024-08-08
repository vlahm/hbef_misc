#2024-08-07
#latest EDI data versions (retrieved in section 1 below):
#   stream/precip chemistry vsn 11
#   discharge vsn 17
#   precip vsn 22

#open this file via likens_and_buso_remake.Rproj to set paths
#start by collapsing code sections with Alt+o (Linux, Windows) or Cmd+Opt+o (Mac)

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)   #for ggthemes::theme_few
library(RCurl)      #for acquiring data download URLs
library(tidyr)      #for tidyr::complete (row completion)
library(macrosheds) #for unit conversion
library(patchwork)  #for composite plots
library(imputeTS)   #for linear interpolation
library(viridis)    #colorblind-friendly scales

source('src/helpers.R')

#set these to your liking
cutoff_s <- 10 #number of stream samples per water-year to require (NULL to ignore)
cutoff_p <- NULL #same, but for precip samples
site <- 'W6' #W1-9 are available. affects Figs 1-4
bad_codes <- c(955, 969, 970) #remove contaminated/questionable samples
# bad_codes <- c(955, 969, 970, 912, 911, 319) #remove storm/low flow samples too
# bad_codes <- c() #don't remove any samples
chem_maxgap <- 3 #number of consecutive chem samples to interpolate (can be 0)
p_maxgap <- 1 #number of consecutive precip samples to interpolate (can be 0)
q_maxgap <- 1 #number of consecutive discharge samples to interpolate (can be 0)

## 1. setup folders; download data ####

dir.create('data_in', showWarnings = FALSE)
dir.create('data_out', showWarnings = FALSE)
dir.create('figs', showWarnings = FALSE)

chem_urls <- get_edi_url(prodcode = 208, version = 11)
discharge_urls <- get_edi_url(prodcode = 1, version = 17)
precip_urls <- get_edi_url(prodcode = 13, version = 22)
all_urls <- bind_rows(chem_urls, discharge_urls, precip_urls) %>%
    filter(! grepl('Methods|info', filename))

options(timeout = 3600)
for(i in 1:nrow(all_urls)){

    fl <- file.path('data_in', all_urls$filename[i])
    if(file.exists(fl)) next
    cat('downloading', all_urls$filename[i], 'to data_in/ \n')

    res <- try({
        download.file(url = all_urls$url[i],
                      destfile = fl)
    })

    if(inherits(res, 'try-error')){
        suppressWarnings(file.remove(fl))
        cat('Removed partial file', fl, '\n')
    }
}

if(! length(list.files('data_in')) == 21){
    stop('some files failed to download. run the above loop again.')
}

## 2. load and clean data ####

#drop cols that won't be used (simplifies unit conversion)
#pH and pHmetrohm are merged and converted to [H ion]
#ANC960 and ANCMet are merged
unused_vars <- c('pH', 'DIC', 'temp', 'ANC960', 'ANCMet', 'TMAl', 'OMAl',
                 'Al_ICP', 'Al_ferron', 'DOC', 'TDN', 'DON', 'ionError',
                 'ionBalance', 'pHmetrohm', 'SiO2', 'PO4', 'cationCharge',
                 'anionCharge')

drop_cols <- c('timeEST', 'barcode', 'fieldCode', 'notes', 'uniqueID', 'duplicate',
               'sampleType', 'canonical', 'gageHt', 'hydroGraph', 'flowGageHt',
               'precipCatch', unused_vars)

#chemistry
s <- suppressWarnings(read_csv('data_in/HubbardBrook_weekly_stream_chemistry.csv',
              show_col_types = FALSE)) %>%
    filter(! fieldCode %in% bad_codes) %>%
    rowwise() %>%
    mutate(pH = mean(c(pH, pHmetrohm), na.rm = TRUE),
           ANC = mean(c(ANC960, ANCMet), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(H = 10^(-pH) * 1.00784 * 1000) %>% #pH -> mg/L
    select(-any_of(drop_cols)) %>%
    group_by(site, date) %>%
    summarize(across(-waterYr, ~mean(., na.rm = TRUE)),
              waterYr = first(waterYr),
              site = first(site),
              .groups = 'drop') %>%
    arrange(site, date)

p <- read_csv('data_in/HubbardBrook_weekly_precipitation_chemistry.csv',
              guess_max = 10000,
              show_col_types = FALSE) %>%
    rowwise() %>%
    mutate(pH = mean(c(pH, pHmetrohm), na.rm = TRUE),
           ANC = mean(c(ANC960, ANCMet), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(H = 10^(-pH) * 1.00784 * 1000) %>% #pH -> mg/L
    filter(! fieldCode %in% bad_codes) %>%
    select(-any_of(c(drop_cols, 'site'))) %>%
    group_by(date) %>%
    summarize(across(-waterYr, ~mean(., na.rm = TRUE)),
              waterYr = first(waterYr),
              site = 'all',
              .groups = 'drop') %>%
    relocate(site) %>%
    arrange(site, date)

#remove W6 observations with high Ca in 1963
s[s$waterYr == 1963 & s$site == 'W6' & s$Ca > 2, 'Ca'] <- NA
# s[s$waterYr == 1963 & s$site == 'W6', c('SO4', 'NO3', 'SO4_NO3')] %>% print(n=100)

#interpolate gaps in chemistry series
s <- s %>%
    group_by(site) %>%
    mutate(across(-any_of(c('site', 'date', 'waterYr')),
                  ~na_interpolation(., maxgap = chem_maxgap))) %>%
    ungroup()
p <- p %>%
    group_by(site) %>%
    mutate(across(-any_of(c('site', 'date', 'waterYr')),
                  ~na_interpolation(., maxgap = chem_maxgap))) %>%
    ungroup()

#there were four precip values originally. all 0. remove these
p$ANC <- NA

#precip
p0 <- read_csv('data_in/HBEF daily precip.csv',
               show_col_types = FALSE) %>%
    group_by(date = DATE) %>%
    summarize(precip = mean(Precip, na.rm = TRUE),
              site = 'all',
              .groups = 'drop') %>%
    relocate(site) %>%
    arrange(site, date)

#interpolate gaps in precipitation series
p0 <- p0 %>%
    group_by(site) %>%
    mutate(precip = na_interpolation(precip, maxgap = p_maxgap)) %>%
    ungroup()

#discharge
q1 <- map_dfr(list.files('data_in', pattern = 'w[0-9]_.*?2012\\.csv', full.names = TRUE),
              ~read_csv(., show_col_types = FALSE)) %>%
    mutate(site = paste0('W', WS)) %>%
    group_by(site, date = as.Date(DATETIME)) %>%
    summarize(discharge = mean(Discharge_ls, na.rm = TRUE),
              .groups = 'drop')

q2 <- map_dfr(list.files('data_in', pattern = 'w[0-9]_.*?_5min\\.csv', full.names = TRUE),
              ~read_csv(., show_col_types = FALSE)) %>%
    group_by(WS, date = as.Date(DATETIME)) %>%
    summarize(discharge = mean(Discharge_ls, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(site = paste0('W', WS)) %>%
    select(site, date, discharge)

q <- bind_rows(q1, q2) %>%
    arrange(site, date)

#interpolate gaps in precipitation series
q <- q %>%
    group_by(site) %>%
    mutate(discharge = na_interpolation(discharge, maxgap = q_maxgap)) %>%
    ungroup()

#join Q, P to C
s <- left_join(s, q, by = c('site', 'date')) %>%
    relocate(waterYr, discharge, .after = 'date')

p <- left_join(p, p0, by = c('site', 'date')) %>%
    relocate(waterYr, precip, .after = 'date')

#convert solutes to ueq
s <- convert_to_equivalents(s)
p <- convert_to_equivalents(p)

#make composite variables
s$SO4_NO3 <- rowSums(s[, c('SO4', 'NO3')])
p$SO4_NO3 <- rowSums(p[, c('SO4', 'NO3')])
s$base_cat <- rowSums(s[, c('Ca', 'Mg', 'K', 'Na')])
p$base_cat <- rowSums(p[, c('Ca', 'Mg', 'K', 'Na')])

## 3. fig 1 (hysteresis) ####

v1 <- 'SO4_NO3'
v2 <- 'base_cat'

vs1 <- calc_vwc_year(s, v1, sample_cutoff = cutoff_s) %>%
    select(-n, -source)
vs2 <- calc_vwc_year(s, v2, sample_cutoff = cutoff_s) %>%
    select(-n, -source)

fig1d <- vs1 %>%
    inner_join(vs2, by = c('site', 'waterYr')) %>%
    rename(year = waterYr) %>%
    filter(site == !!site) %>%
    arrange(year)

write_csv(fig1d, paste0('data_out/fig1_', site, '.csv'))

fig1d %>%
    ggplot(aes(x = SO4_NO3, y = base_cat, color = year)) +
    geom_abline(intercept = 0, slope = 1, linetype = 'dashed',
                color = 'gray70', linewidth = 0.7) +
    geom_point() +
    scale_color_viridis_c(
        end = 0.85,
        direction = -1,
        option = 'magma',
        name = 'Year',
        breaks = c(1964, seq(1980, 2020, by = 10)),
        limits = c(min(fig1d$year), max(fig1d$year)),
        guide = guide_colorbar(
            title.position = 'top',
            title.hjust = 0.5,
            barwidth = 2,
            barheight = 7,
            frame.colour = 'transparent',
            ticks.colour = 'transparent'
        )
    ) +
    labs(x = expression('Sum of SO'[2] + ' NO'[3] * ' (µeq/L)'),
         y = 'Sum of Base Cations (µeq/L)') +
    theme_few() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.9, 0.2)) +
    scale_y_continuous(limits = c(0, 200),
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 200),
                       expand = c(0, 0))

ggsave(paste0('figs/fig1_', site, '.png'), width = 6, height = 5)


## 4. fig 2 (EC) ####

v_ <- 'spCond'

spcond_s <- calc_vwc_wateryear(s, v_, sample_cutoff = cutoff_s)
spcond_p <- calc_vwc_wateryear(p, v_, sample_cutoff = cutoff_p)

trend_s <- get_trendline(spcond_s, site = site, lims = c(min(spcond_s$waterYr), 2040))
trend_p <- get_trendline(spcond_p, site = 'all', lims = c(min(spcond_p$waterYr), 2040))

fig2d <- spcond_s %>%
    filter(site == !!site) %>%
    bind_rows(spcond_p)

write_csv(fig2d, paste0('data_out/fig2_', site, '.csv'))

fig2d %>%
    ggplot(aes(x = waterYr,
               y = spCond,
               color = source,
               fill = source,
               shape = source)) +
    geom_line() +
    geom_point() +
    geom_line(data = trend_s,
              aes(x = waterYr, y = spCond),
              color = 'red',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_p,
              aes(x = waterYr, y = spCond),
              color = 'red',
              linetype = 'dashed',
              linewidth = 0.3,
              show.legend = FALSE) +
    scale_color_manual(values = c(Precipitation = 'blue3',
                                  Streamwater = 'blue3')) +
    scale_shape_manual(values = c(Precipitation = 21,
                                  Streamwater = 21)) +
    scale_fill_manual(values = c(Precipitation = 'white',
                                 Streamwater = 'blue3')) +
    labs(x = "Water Year",
         y = "Specific Conductance (µS/cm)") +
    guides(color = guide_legend(title = NULL),
           fill = guide_legend(title = NULL),
           shape = guide_legend(title = NULL)) +
    theme_few() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.8, 0.8)) +
    scale_y_continuous(limits = c(0, 40),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1960, 2040, by = 10),
                       limits = c(1960, 2041),
                       # expand = expansion(add = c(5, 0)))
                       expand = c(0, 0))

ggsave(paste0('figs/fig2_', site, '.png'), width = 8, height = 5)


## 5. fig 3 (SO4 + NO3, base cations) ####

v1 <- 'SO4_NO3'
v2 <- 'base_cat'

#panel A

vs1 <- calc_vwc_wateryear(s, v1, sample_cutoff = cutoff_s)
vs2 <- calc_vwc_wateryear(s, v2, sample_cutoff = cutoff_s)

trend_s1 <- get_trendline(vs1, site = site, lims = c(min(vs1$waterYr), 2040))
trend_s2 <- get_trendline(vs2, site = site, lims = c(min(vs1$waterYr), 2040))

vs1 <- convert_to_long(vs1)
vs2 <- convert_to_long(vs2)
trend_s1 <- convert_to_long(trend_s1)
trend_s2 <- convert_to_long(trend_s2)

fig3da <- vs1 %>%
    bind_rows(vs2) %>%
    filter(site == !!site)

write_csv(fig3da, paste0('data_out/fig3A_', site, '.csv'))

panelA <- fig3da %>%
    ggplot(aes(x = waterYr,
               y = val,
               color = var,
               fill = var,
               shape = var)) +
    geom_line() +
    geom_point() +
    geom_line(data = trend_s1,
              aes(x = waterYr, y = val),
              color = 'red',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_s2,
              aes(x = waterYr, y = val),
              color = 'red',
              linetype = 'dashed',
              linewidth = 0.3,
              show.legend = FALSE) +
    scale_color_manual(values = c(SO4_NO3 = 'blue3', base_cat = 'blue3'),
                       labels = c(SO4_NO3 = 'Sum of Sulfate + Nitrate', base_cat = 'Sum of Base Cations')) +
    scale_shape_manual(values = c(SO4_NO3 = 21, base_cat = 21),
                       labels = c(SO4_NO3 = 'Sum of Sulfate + Nitrate', base_cat = 'Sum of Base Cations')) +
    scale_fill_manual(values = c(SO4_NO3 = 'white', base_cat = 'blue3'),
                      labels = c(SO4_NO3 = 'Sum of Sulfate + Nitrate', base_cat = 'Sum of Base Cations')) +
    labs(x = "Water Year",
         y = "Concentration  (µeq/L)") +
    guides(color = guide_legend(title = NULL),
           fill = guide_legend(title = NULL),
           shape = guide_legend(title = NULL)) +
    theme_few() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.7, 0.8)) +
    scale_y_continuous(limits = c(0, 200),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1960, 2070, by = 10),
                       limits = c(1960, 2071),
                       expand = c(0, 0))

#panel B

vp1 <- calc_vwc_wateryear(p, v1, sample_cutoff = cutoff_p)
vp2 <- calc_vwc_wateryear(p, v2, sample_cutoff = cutoff_p)

trend_p1 <- get_trendline(vp1, site = 'all', lims = c(min(vp1$waterYr), 2040))
trend_p2 <- get_trendline(vp2, site = 'all', lims = c(min(vp1$waterYr), 2040))

vp1 <- convert_to_long(vp1)
vp2 <- convert_to_long(vp2)
trend_p1 <- convert_to_long(trend_p1)
trend_p2 <- convert_to_long(trend_p2)

fig3db <- vp1 %>%
    bind_rows(vp2)

write_csv(fig3db, paste0('data_out/fig3B_', site, '.csv'))

panelB <- fig3db %>%
    ggplot(aes(x = waterYr,
               y = val,
               color = var,
               fill = var,
               shape = var)) +
    geom_line() +
    geom_point() +
    geom_line(data = trend_p1,
              aes(x = waterYr, y = val),
              color = 'red',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_p2,
              aes(x = waterYr, y = val),
              color = 'red',
              linetype = 'dashed',
              linewidth = 0.3,
              show.legend = FALSE) +
    scale_color_manual(values = c(SO4_NO3 = 'blue3', base_cat = 'blue3'),
                       labels = c(SO4_NO3 = 'Sum of Sulfate + Nitrate', base_cat = 'Sum of Base Cations')) +
    scale_shape_manual(values = c(SO4_NO3 = 21, base_cat = 21),
                       labels = c(SO4_NO3 = 'Sum of Sulfate + Nitrate', base_cat = 'Sum of Base Cations')) +
    scale_fill_manual(values = c(SO4_NO3 = 'white', base_cat = 'blue3'),
                      labels = c(SO4_NO3 = 'Sum of Sulfate + Nitrate', base_cat = 'Sum of Base Cations')) +
    labs(x = "Water Year",
         y = "Concentration  (µeq/L)") +
    guides(color = guide_legend(title = NULL),
           fill = guide_legend(title = NULL),
           shape = guide_legend(title = NULL)) +
    theme_few() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.7, 0.8)) +
    scale_y_continuous(limits = c(0, 120),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1960, 2070, by = 10),
                       limits = c(1960, 2071),
                       expand = c(0, 0))

panelA + panelB + plot_layout(nrow = 2)#, heights = c(4, 1))

ggsave(paste0('figs/fig3_', site, '.png'), width = 6, height = 8)

## 6. fig 4 (various solutes) ####

l_pos <- c(0.8, 0.8) #legend position

vars <- list(c(Ca = 'Calcium', Na = 'Sodium', Mg = 'Magnesium', K = 'Potassium'),
             c(SO4 = 'Sulfate', Cl = 'Chloride', NO3 = 'Nitrate'),
             c(H = 'Hydrogen Ion', ANC = 'ANC'))

panel_count <- 0
panel_list <- list()
for(vset in vars){

    panel_count <- panel_count + 1
    vs <- vp <- trend_s <- trend_p <- tibble()
    for(v_ in names(vset)){

        vs_ <- calc_vwc_wateryear(s, v_, sample_cutoff = cutoff_s)
        vp_ <- calc_vwc_wateryear(p, v_, sample_cutoff = cutoff_p)

        vs <- bind_rows(vs, convert_to_long(vs_))
        vp <- bind_rows(vp, convert_to_long(vp_))
    }

    fig4d1 <- vs %>%
        filter(site == !!site)

    write_csv(fig4d1, paste0('data_out/fig4A', panel_count, '_', site, '.csv'))

    panel_list[[paste0('A', panel_count)]] <- fig4d1 %>%
        ggplot(aes(x = waterYr,
                   y = val,
                   color = var)) +
        geom_line() +
        geom_point(shape = 20) +
        scale_color_viridis_d(begin = 0.1, end = 0.8, option = 'viridis',
                              labels = vset) +
        geom_smooth(se = FALSE, method = 'lm', fullrange = TRUE,
                    linewidth = 0.7, linetype = 'dotted') +
        labs(x = "Water Year",
             y = "µeq/L") +
        guides(color = guide_legend(title = NULL)) +
        theme_few() +
        theme(legend.position = 'inside',
              legend.position.inside = if(panel_count == 3) c(0.8, 0.2) else l_pos) +
        scale_x_continuous(breaks = seq(1960, 2030, by = 10),
                           limits = c(1960, 2031),
                           expand = c(0, 0))

    if(panel_count == 3){
        panel_list[[paste0('A', panel_count)]] <- panel_list[[paste0('A', panel_count)]] +
            geom_hline(yintercept = 0, linetype = 'dashed',
                       color = 'gray70', linewidth = 0.4)
    }

    write_csv(vp, paste0('data_out/fig4B', panel_count, '_', site, '.csv'))

    panel_list[[paste0('B', panel_count)]] <- vp %>%
        ggplot(aes(x = waterYr,
                   y = val,
                   color = var)) +
        geom_line() +
        geom_point(shape = 20) +
        scale_color_viridis_d(begin = 0.1, end = 0.8, option = 'D', labels = vset,
                              direction = ifelse(panel_count == 3, -1, 1)) +
        geom_smooth(se = FALSE, method = 'lm', fullrange = TRUE,
                    linewidth = 0.7, linetype = 'dotted') +
        labs(x = "Water Year",
             y = "µeq/L") +
        guides(color = guide_legend(title = NULL)) +
        theme_few() +
        theme(legend.position = 'inside',
              legend.position.inside = l_pos) +
        scale_x_continuous(breaks = seq(1960, 2030, by = 10),
                           limits = c(1960, 2031),
                           expand = c(0, 0))
}

panel_list$A1 + panel_list$B1 + panel_list$A2 + panel_list$B2 +
    panel_list$A3 + panel_list$B3 +
    plot_layout(nrow = 3, byrow = TRUE, axes = 'collect')

ggsave(paste0('figs/fig4_', site, '.png'), width = 6, height = 8)

## 7. fig 5 (mainstem annual EC) ####

v_ <- 'spCond'

spcond_s <- calc_mean_year(s, v_, sample_cutoff = cutoff_s)

trend_hbk <- get_trendline(spcond_s, site = 'HBK',
                           lims = c(min(spcond_s$waterYr[spcond_s$site == 'HBK']), 2040))
trend_w3 <- get_trendline(spcond_s, site = 'W3',
                           lims = c(min(spcond_s$waterYr[spcond_s$site == 'W3']), 2040))
trend_w9 <- get_trendline(spcond_s, site = 'W9',
                           lims = c(min(spcond_s$waterYr[spcond_s$site == 'W9']), 2040))

spcond_s <- rename(spcond_s, year = waterYr)
trend_hbk <- rename(trend_hbk, year = waterYr)
trend_w3 <- rename(trend_w3, year = waterYr)
trend_w9 <- rename(trend_w9, year = waterYr)

fig5d <- spcond_s %>%
    filter(site %in% c('HBK', 'W3', 'W9'))

write_csv(fig5d, 'data_out/fig5.csv')

fig5d %>%
    ggplot(aes(x = year,
               y = spCond,
               color = site,
               shape = site)) +
    geom_line() +
    geom_point() +
    geom_line(data = trend_hbk,
              aes(x = year, y = spCond, color = site),
              linetype = 'dashed',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_w3,
              aes(x = year, y = spCond, color = site),
              linetype = 'dashed',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_w9,
              aes(x = year, y = spCond, color = site),
              linetype = 'dashed',
              linewidth = 0.3,
              show.legend = FALSE) +
    scale_color_manual(values = c(HBK = 'blue3', W3 = 'red', W9 = 'black'),
                       labels = c(HBK = 'Hubbard Brook', W3 = 'W3', W9 = 'W9')) +
    scale_shape_manual(values = c(HBK = 20, W3 = 20, W9 = 20),
                       labels = c(HBK = 'Hubbard Brook', W3 = 'W3', W9 = 'W9')) +
    labs(x = "Year",
         y = "Mean Electrical Conductivity (µS/cm)") +
    guides(color = guide_legend(title = NULL),
           fill = guide_legend(title = NULL),
           shape = guide_legend(title = NULL)) +
    theme_few() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.8, 0.8)) +
    scale_y_continuous(limits = c(0, 40),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1960, 2040, by = 10),
                       limits = c(1960, 2041),
                       # expand = expansion(add = c(5, 0)))
                       expand = c(0, 0))

ggsave('figs/fig5.png', width = 8, height = 5)
