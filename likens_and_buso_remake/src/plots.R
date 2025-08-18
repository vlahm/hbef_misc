#2024-08-07
#latest EDI data versions (retrieved in section 1 below):
#   stream/precip chemistry vsn 11
#   discharge vsn 17
#   precip vsn 22

#open this file via likens_and_buso_remake.Rproj to set paths
#start by collapsing code sections with Alt+o (Linux, Windows) or Cmd+Opt+o (Mac)
setwd('~/git/hbef/hbef_misc/likens_and_buso_remake/')

library(segmented)  #for breakpoint analysis
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
site <- 'W6' #W1-9 are available. affects Figs 1-4. appropriate rain gauges are automatically chosen.
bad_codes <- c(955, 969, 970) #remove contaminated/questionable samples
# bad_codes <- c(955, 969, 970, 912, 911, 319) #remove storm/low flow samples too
# bad_codes <- c() #don't remove any samples
chem_maxgap <- Inf #number of consecutive chem samples to interpolate (can be 0)
# p_maxgap <- 1 #number of consecutive precip samples to interpolate (can be 0) #deprecated. using official p
q_maxgap <- 0 #number of consecutive discharge samples to interpolate (can be 0)

## 0. load official hbef stream flux and precip VWC ####

# vwc_official <- read_csv('data_in/official_method/results/ws6_precip_monthly_volwt_conc.csv')
p_official <- read_csv('data_in/official_method/super_official/ws6_precip_WY_volwt_conc.csv',
                       show_col_types = FALSE)
s_official <- read_csv('data_in/ws6_stream_monthly_flux_gHa.csv', show_col_types = FALSE)

## 1. set up folders; download data ####

dir.create('data_in', showWarnings = FALSE)
dir.create('data_out', showWarnings = FALSE)
dir.create('figs', showWarnings = FALSE)
dir.create('figs/breakpoints/precip_1bp', showWarnings = FALSE, recursive = TRUE)
dir.create('figs/breakpoints/stream_1bp', showWarnings = FALSE)
dir.create('figs/breakpoints/precip_multi_bp', showWarnings = FALSE)
dir.create('figs/breakpoints/stream_multi_bp', showWarnings = FALSE)

chem_urls <- get_edi_url(prodcode = 208, version = 13)
discharge_urls <- get_edi_url(prodcode = 1, version = 18)
# precip_urls <- get_edi_url(prodcode = 13, version = 22)
flux_urls <- get_edi_url(prodcode = 8, version = 19)
all_urls <- bind_rows(chem_urls, discharge_urls, flux_urls) %>%
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

# if(length(list.files('data_in')) < 20){
#     stop('some files failed to download. run the above loop again.')
# }

## 2. load and clean data ####

# #establish which precip gauges will be used for N- and S-facing watersheds
# north_facing_gauges <- c('RG19', 'RG23')
# south_facing_gauges <- c('RG8', 'RG22') #RG-41?? W6-open, W1-open?
# # north_facing_gauges <- c('N', paste0('RG', c(12:17, 19:21, 23:25)))
# # south_facing_gauges <- c('S', paste0('RG', c(1:11, 22)))
#
# if(site %in% c('W7', 'W8', 'W9')){
#     gauge_set <- north_facing_gauges
# } else {
#     gauge_set <- south_facing_gauges
# }

#drop cols that won't be used (simplifies unit conversion)
#pH and pHmetrohm are merged and converted to [H ion]
#ANC960 and ANCMet are merged
unused_vars <- c('temp', 'ANC960', 'ANCMet', 'TMAl', 'OMAl',
                 'Al_ICP', 'Al_ferron', 'TDN', 'DON', 'ionError',
                 'ionBalance', 'pHmetrohm', 'SiO2', 'PO4', 'cationCharge',
                 'anionCharge')

drop_cols <- c('timeEST', 'barcode', 'fieldCode', 'notes', 'uniqueID', 'duplicate',
               'sampleType', 'canonical', 'gageHt', 'hydroGraph', 'flowGageHt',
               'precipCatch', unused_vars)

#chemistry
# s <- suppressWarnings(read_csv('data_in/HubbardBrook_weekly_stream_chemistry.csv',
s <- suppressWarnings(read_csv(
    'data_in/HubbardBrook_weekly_stream_chemistry_1963-2024.csv',
    show_col_types = FALSE
)) %>%
    filter(!fieldCode %in% bad_codes) %>%
    rowwise() %>%
    mutate(
        pH = mean(c(pH, pHmetrohm), na.rm = TRUE),
        ANC = mean(c(ANC960, ANCMet), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(H = 10^(-pH) * 1.00784 * 1000) %>% #pH -> mg/L
    select(-any_of(drop_cols)) %>%
    group_by(site, date) %>%
    summarize(
        across(-waterYr, ~ mean(., na.rm = TRUE)),
        waterYr = first(waterYr),
        site = first(site),
        .groups = 'drop'
    ) %>%
    arrange(site, date)

# p <- read_csv('data_in/HubbardBrook_weekly_precipitation_chemistry.csv',
#               guess_max = 10000,
#               show_col_types = FALSE) %>%
#     rowwise() %>%
#     mutate(pH = mean(c(pH, pHmetrohm), na.rm = TRUE),
#            ANC = mean(c(ANC960, ANCMet), na.rm = TRUE)) %>%
#     ungroup() %>%
#     mutate(H = 10^(-pH) * 1.00784 * 1000) %>% #pH -> mg/L
#     filter(! fieldCode %in% bad_codes,
#            site %in% gauge_set) %>%
#     select(-any_of(c(drop_cols, 'site'))) %>%
#     group_by(date) %>%
#     summarize(across(-waterYr, ~mean(., na.rm = TRUE)),
#               waterYr = first(waterYr),
#               site = 'all',
#               .groups = 'drop') %>%
#     relocate(site) %>%
#     arrange(site, date)

#remove W6 observations with high Ca in 1963
s[s$waterYr == 1963 & s$site == 'W6' & s$Ca > 2, 'Ca'] <- NA
# s[s$waterYr == 1963 & s$site == 'W6', c('SO4', 'NO3', 'SO4_NO3')] %>% print(n=100)

#interpolate gaps in chemistry series
s <- s %>%
    group_by(site) %>%
    mutate(across(
        -any_of(c('site', 'date', 'waterYr')),
        ~ na_locf(
            .,
            maxgap = chem_maxgap,
            option = 'nocb',
            na_remaining = 'keep'
        )
    )) %>%
    # ~na_interpolation(., maxgap = chem_maxgap))) %>%
    ungroup()
# p <- p %>%
#     group_by(site) %>%
#     mutate(across(-any_of(c('site', 'date', 'waterYr')),
#                   ~na_locf(., maxgap = chem_maxgap, option = 'nocb'))) %>%
#                   # ~na_interpolation(., maxgap = chem_maxgap))) %>%
#     ungroup()

# #there were four precip ANC values originally. all 0. remove these
# p$ANC <- NA

# #precip
# p0 <- read_csv('data_in/HBEF daily precip.csv',
#                show_col_types = FALSE) %>%
#     filter(rainGage %in% gauge_set) %>%
#     group_by(date = DATE) %>%
#     summarize(precip = mean(Precip, na.rm = TRUE),
#               site = 'all',
#               .groups = 'drop') %>%
#     relocate(site) %>%
#     arrange(site, date)

# #interpolate gaps in precipitation series
# p0 <- p0 %>%
#     group_by(site) %>%
#     mutate(precip = na_interpolation(precip, maxgap = p_maxgap)) %>%
#     ungroup()

#discharge
q1 <- map_dfr(list.files('data_in', pattern = 'w[0-9]_.*?2012\\.csv', full.names = TRUE),
              ~read_csv(., show_col_types = FALSE)) %>%
    mutate(site = paste0('W', WS)) %>%
    group_by(site, date = as.Date(DATETIME)) %>%
    summarize(discharge = mean(Discharge_ls, na.rm = TRUE),
              .groups = 'drop')

# q2 <- map_dfr(list.files('data_in', pattern = 'w[0-9]_.*?_5min\\.csv', full.names = TRUE),
q2 <- map_dfr(
    list.files(
        'data_in',
        pattern = 'w[0-9]_stmflow_5min\\.csv',
        full.names = TRUE
    ),
    ~ read_csv(., show_col_types = FALSE)
) %>%
    group_by(WS, date = as.Date(DATETIME)) %>%
    summarize(
        discharge = mean(Discharge_ls, na.rm = TRUE),
        .groups = 'drop'
    ) %>%
    mutate(site = paste0('W', WS)) %>%
    select(site, date, discharge)

q <- bind_rows(q1, q2) %>%
    arrange(site, date)

#interpolate gaps in discharge series
q <- q %>%
    group_by(site) %>%
    mutate(discharge = na_interpolation(discharge, maxgap = q_maxgap)) %>%
    ungroup()

#join Q, P to C
s <- left_join(s, q, by = c('site', 'date')) %>%
    relocate(waterYr, discharge, .after = 'date')

# p <- left_join(p, p0, by = c('site', 'date')) %>%
#     relocate(waterYr, precip, .after = 'date')

#convert solutes to ueq
s_bak <- s
s <- convert_to_equivalents(select(s, -pH))
s <- bind_cols(s, select(s_bak, pH))
# p <- convert_to_equivalents(p)

#make composite variables
s$SO4_NO3 <- rowSums(s[, c('SO4', 'NO3')])
# p$SO4_NO3 <- rowSums(p[, c('SO4', 'NO3')])
s$base_cat <- rowSums(s[, c('Ca', 'Mg', 'K', 'Na')])
# p$base_cat <- rowSums(p[, c('Ca', 'Mg', 'K', 'Na')])

#process official precip vwc and stream flux
p_official <- p_official %>%
    mutate(site = 'all',
           # waterYr = ifelse(as.numeric(Month) < 6, Year - 1, Year),
           source = 'Precipitation') %>%
    # filter(waterYr != 1963) %>%
    rename_with(~sub('_volwt', '', .)) %>%
    # mutate(date = as.Date(paste0(Year_Month, '-01'))) %>%
    mutate(date = as.Date(paste0(WaterYear, '-06-01'))) %>%
    select(site, date, waterYr = WaterYear, precip = precip_mm, spCond = SpecCond, Ca,
           Mg, K, Na, NH4, SO4, NO3, Cl, Mn, Fe, `F`, H, DOC, pH) %>%
    mutate(across(precip:pH, ~if_else(. < -800, NA_real_, .)))

p_bak <- p_official
p_official <- convert_to_equivalents(select(p_official, -pH))
p_official <- bind_cols(p_official, select(p_bak, pH))
p_official$SO4_NO3 <- rowSums(p_official[, c('SO4', 'NO3')])
p_official$base_cat <- rowSums(p_official[, c('Ca', 'Mg', 'K', 'Na')])

s_official <- s_official %>%
    mutate(site = !!site,
           source = 'Streamwater',
           waterYr = ifelse(as.numeric(Month) < 6, Year - 1, Year)) %>%
    # filter(waterYr != 1963) %>%
    rename_with(~sub('_flux', '', .)) %>%
    mutate(date = as.Date(paste0(Year_Month, '-01'))) %>%
    select(site, date, waterYr, flow_mm, SpecCond_volwt, ANC_volwt, Ca,
           Mg, K, Na, NH4, SO4, NO3, Cl, Mn, Fe, `F`, H, DIC, DOC, pH = pH_volwt) %>%
    mutate(across(SpecCond_volwt:pH, ~if_else(. < -800, NA_real_, .)),
           across(Ca:H, ~. / flow_mm / 10)) %>%
    rename(spCond = SpecCond_volwt, ANC = ANC_volwt)

so_bak <- s_official
s_official <- convert_to_equivalents(select(s_official, -pH))
s_official <- bind_cols(s_official, select(so_bak, pH))
s_official$SO4_NO3 <- rowSums(s_official[, c('SO4', 'NO3')])
s_official$base_cat <- rowSums(s_official[, c('Ca', 'Mg', 'K', 'Na')])

# s_official = s %>%
#     filter(site == !!site) %>%
#     rename(flow_mm = discharge) %>%
#     mutate(waterYr = if_else(month(date) < 6, year(date) - 1, year(date))) %>%
#     group_by(site, waterYr, month = month(date)) %>%
#     summarize(across(flow_mm:base_cat, ~mean(., na.rm = TRUE)),
#               .groups = 'drop') %>%
#     # summarize(Ca = sum(Ca * discharge) / sum(discharge))
#     filter(!(waterYr == 1963 & month < 6)) %>%
#     mutate(date = as.Date(paste0(if_else(month < 6, waterYr + 1, waterYr),
#                                  '-', month, '-01'))) %>%
#     relocate(date, .after = site) %>%
#     select(-month) %>%
#     filter(date >= as.Date('1963-06-01'))


## 3. fig 1 (hysteresis) ####

v1 <- 'SO4_NO3'
v2 <- 'base_cat'

vs1 <- calc_vwc_wateryear(s_official, v1, sample_cutoff = cutoff_s) %>%
    select(-n, -source)
vs2 <- calc_vwc_wateryear(s_official, v2, sample_cutoff = cutoff_s) %>%
    select(-n, -source)

vsfiltA <- vs1 %>%
    left_join(vs2, by = c('site', 'waterYr')) %>%
    filter(waterYr > 1969)
# lm_rngA <- seq(min(vsfiltA$SO4_NO3, na.rm = TRUE), max(vsfiltA$SO4_NO3, na.rm = TRUE))
lm_rngA <- seq(20, 175)
modA <- lm(base_cat ~ SO4_NO3, data = vsfiltA)
predA <- predict(modA, newdata = data.frame(SO4_NO3 = lm_rngA))

trend_sA <- tibble(site = rep(site, length = length(lm_rngA)),
                  SO4_NO3 = lm_rngA,
                  base_cat = unname(predA),
                  source = rep('trend_s', length = length(lm_rngA)))

vsfiltC <- vs1 %>%
    left_join(vs2, by = c('site', 'waterYr')) %>%
    filter(between(waterYr, 1970, 2009))
modC <- lm(base_cat ~ SO4_NO3, data = vsfiltC)
predC <- predict(modC, newdata = data.frame(SO4_NO3 = lm_rngA))

trend_sC <- tibble(site = rep(site, length = length(lm_rngA)),
                  SO4_NO3 = lm_rngA,
                  base_cat = unname(predC),
                  source = rep('trend_s', length = length(lm_rngA)))

vsfiltB <- vs1 %>%
    left_join(vs2, by = c('site', 'waterYr')) %>%
    filter(between(waterYr, 1965, 1969))
lm_rngB <- seq(min(vsfiltB$SO4_NO3, na.rm = TRUE), max(vsfiltB$SO4_NO3, na.rm = TRUE))
modB <- lm(base_cat ~ SO4_NO3, data = vsfiltB)
predB <- predict(modB, newdata = data.frame(SO4_NO3 = lm_rngB))

trend_sB <- tibble(site = rep(site, length = length(lm_rngB)),
                  SO4_NO3 = lm_rngB,
                  base_cat = unname(predB),
                  source = rep('trend_s', length = length(lm_rngB)))

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
    geom_line(data = trend_sA,
              aes(x = SO4_NO3, y = base_cat),
              color = 'black',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_sB,
              aes(x = SO4_NO3, y = base_cat),
              color = 'black',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_sC,
              aes(x = SO4_NO3, y = base_cat),
              color = 'black',
              linewidth = 0.3,
              linetype = 'dashed',
              show.legend = FALSE) +
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
            barwidth = 1.5,
            barheight = 5,
            frame.colour = 'transparent',
            ticks.colour = 'transparent'
        )
    ) +
    labs(x = expression('Sum of SO'[4] + ' NO'[3] * ' (µeq/L)'),
         y = 'Sum of Base Cations (µeq/L)') +
    theme_few() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.85, 0.2),
          plot.margin = margin(t = 10, r = 10, l = 3)) +
    scale_y_continuous(limits = c(0, 200),
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 200),
                       expand = c(0, 0))

ggsave(paste0('figs/fig1_comparison_', site, '.png'), width = 6, height = 5)

## 3b. comparison of slopes ####

n_boot <- 1000
slope_diffs <- numeric(n_boot)

for(i in 1:n_boot){

    dA <- vsfiltA[sample(nrow(vsfiltA), replace = TRUE), ]
    dC <- vsfiltC[sample(nrow(vsfiltC), replace = TRUE), ]

    mA <- lm(base_cat ~ SO4_NO3, data = dA)
    mC <- lm(base_cat ~ SO4_NO3, data = dC)

    slope_diffs[i] <- coef(mC)[2] - coef(mA)[2]
}

ci <- quantile(slope_diffs, probs = c(0.025, 0.975))

hist(slope_diffs, breaks = 30, main = "Bootstrap Distribution of Slope Differences", xlab = "Slope Difference")
abline(v = ci, col = "red", lwd = 2, lty = 2)


## 4. fig 2 (EC) ####

v_ <- 'spCond'

spcond_s <- calc_vwc_wateryear(s_official, v_, sample_cutoff = cutoff_s)
spcond_p <- calc_vwc_wateryear(p_official, v_, sample_cutoff = cutoff_p)

bsln <- min(c(spcond_p$waterYr, spcond_s$waterYr))
trend_sA <- get_trendline(spcond_s, site = site, lims = c(min(spcond_s$waterYr), 2012), filt = TRUE, baseline = bsln)
trend_pA <- get_trendline(spcond_p, site = 'all', lims = c(min(spcond_p$waterYr), 2012), filt = TRUE, baseline = bsln)
trend_sB <- get_trendline(spcond_s, site = site, lims = c(2015, 2022), filt = TRUE, baseline = bsln)
trend_pB <- get_trendline(spcond_p, site = 'all', lims = c(2010, 2022), filt = TRUE, baseline = bsln)
# get_trendline(spcond_s, site = site, lims = c(2010, 2022), filt = TRUE, baseline = bsln)
# get_trendline(spcond_s, site = site, lims = c(1965, 2022), filt = TRUE, baseline = bsln)
# get_trendline(spcond_p, site = 'all', lims = c(1973, 2022), filt = TRUE, baseline = bsln)

fig2d <- spcond_s %>%
    filter(site == !!site) %>%
    bind_rows(spcond_p)

write_csv(fig2d, paste0('data_out/fig2_', site, '.csv'))

fig2d %>%
    mutate(source = if_else(source == 'Streamwater', 'Stream water', source)) %>%
    ggplot(aes(x = waterYr,
               y = spCond,
               color = source,
               fill = source,
               shape = source)) +
    geom_line() +
    geom_point() +
    geom_line(data = trend_sA,
              aes(x = waterYr, y = spCond),
              color = 'red',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_pA,
              aes(x = waterYr, y = spCond),
              color = 'red',
              linetype = 'dashed',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_sB,
              aes(x = waterYr, y = spCond),
              color = 'black',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_pB,
              aes(x = waterYr, y = spCond),
              color = 'black',
              linetype = 'dashed',
              linewidth = 0.3,
              show.legend = FALSE) +
    scale_color_manual(values = c(Precipitation = 'blue3',
                                  `Stream water` = 'blue3')) +
    scale_shape_manual(values = c(Precipitation = 21,
                                  `Stream water` = 21)) +
    scale_fill_manual(values = c(Precipitation = 'white',
                                 `Stream water` = 'blue3')) +
    labs(x = "Water-Year",
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

# vs1 <- calc_vwc_wateryear_v2(s, v1, sample_cutoff = cutoff_s)
# vs2 <- calc_vwc_wateryear_v2(s, v2, sample_cutoff = cutoff_s)
vs1 <- calc_vwc_wateryear(s_official, v1, sample_cutoff = cutoff_s)
vs2 <- calc_vwc_wateryear(s_official, v2, sample_cutoff = cutoff_s)

bsln <- min(c(vs1$waterYr, vs2$waterYr))
trend_s1 <- get_trendline(vs1, site = site, lims = c(min(vs1$waterYr), 2040), baseline = bsln)
trend_s2 <- get_trendline(vs2, site = site, lims = c(min(vs1$waterYr), 2040), baseline = bsln)

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
    labs(x = "Water-Year",
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

# #nocb interpolate precip chem
# p <- imputeTS::na_locf(p, option = 'nocb')
#
# vp1 <- calc_vwc_wateryear(p, v1, sample_cutoff = cutoff_p, month_first = TRUE)
# vp2 <- calc_vwc_wateryear(p, v2, sample_cutoff = cutoff_p, month_first = TRUE)

nit_sul_base <- p_official %>%
    mutate(source = 'Precipitation') %>%
    group_by(site, waterYr, source) %>%
    summarize(SO4_NO3 = mean(SO4_NO3, na.rm = TRUE),
              base_cat = mean(base_cat, na.rm = TRUE),
              site = first(site),
              source = first(source),
              .groups = 'drop',
              n = n()) %>%
    select(site, waterYr, SO4_NO3, base_cat, n, source)
vp1 <- select(nit_sul_base, -base_cat)
vp2 <- select(nit_sul_base, -SO4_NO3)

trend_p1 <- get_trendline(vp1, site = 'all', lims = c(min(vp1$waterYr), 2040), baseline = bsln)
trend_p1poly <- get_trendline(vp1, site = 'all', lims = c(min(vp1$waterYr), 2040), poly = TRUE, baseline = bsln)
trend_p2 <- get_trendline(vp2, site = 'all', lims = c(min(vp1$waterYr), 2040), baseline = bsln)

vp1 <- convert_to_long(vp1)
vp2 <- convert_to_long(vp2)
trend_p1 <- convert_to_long(trend_p1)
trend_p1poly <- convert_to_long(trend_p1poly)
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
    geom_line(data = trend_p1poly,
              aes(x = waterYr, y = val),
              color = 'red',
              linetype = 'dotted',
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
    labs(x = "Water-Year",
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

ggsave(paste0('figs/fig3_comparison_', site, '.png'), width = 6, height = 8)

## 5b. same thing but pH, SO4, NO3 for Amey ####

v1 <- 'NO3'
v2 <- 'SO4'

#panel A

vs1 <- calc_vwc_wateryear(s_official, v1, sample_cutoff = cutoff_s)
vs2 <- calc_vwc_wateryear(s_official, v2, sample_cutoff = cutoff_s)

trend_s1 <- get_trendline(vs1, site = site, lims = c(min(vs1$waterYr), 2040))
trend_s2 <- get_trendline(vs2, site = site, lims = c(min(vs1$waterYr), 2040))

vs1 <- convert_to_long(vs1)
vs2 <- convert_to_long(vs2)
trend_s1 <- convert_to_long(trend_s1)
trend_s2 <- convert_to_long(trend_s2)

panelA <- vs1 %>%
    bind_rows(vs2) %>%
    filter(site == !!site) %>%
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
    scale_color_manual(values = c(NO3 = 'blue3', SO4 = 'blue3'),
                       labels = c(NO3 = 'Nitrate', SO4 = 'Sulfate')) +
    scale_shape_manual(values = c(NO3 = 21, SO4 = 21),
                       labels = c(NO3 = 'Nitrate', SO4 = 'Sulfate')) +
    scale_fill_manual(values = c(NO3 = 'white', SO4 = 'blue3'),
                      labels = c(NO3 = 'Nitrate', SO4 = 'Sulfate')) +
    labs(x = "Water-Year",
         y = "Concentration  (µeq/L)",
         title = 'Stream water') +
    guides(color = guide_legend(title = NULL),
           fill = guide_legend(title = NULL),
           shape = guide_legend(title = NULL)) +
    theme_few() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.9, 0.8)) +
    scale_y_continuous(limits = c(0, 150),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1960, 2050, by = 10),
                       limits = c(1960, 2051),
                       expand = c(0, 0))

#panel B

nit_sul_base <- p_official %>%
    mutate(source = 'Precipitation') %>%
    group_by(site, waterYr, source) %>%
    summarize(NO3 = mean(NO3, na.rm = TRUE),
              SO4 = mean(SO4, na.rm = TRUE),
              site = first(site),
              source = first(source),
              .groups = 'drop',
              n = n()) %>%
    select(site, waterYr, NO3, SO4, n, source)
vp1 <- select(nit_sul_base, -NO3)
vp2 <- select(nit_sul_base, -SO4)

trend_p1 <- get_trendline(vp1, site = 'all', lims = c(min(vp1$waterYr), 2040))
trend_p2 <- get_trendline(vp2, site = 'all', lims = c(min(vp1$waterYr), 2040))

vp1 <- convert_to_long(vp1)
vp2 <- convert_to_long(vp2)
trend_p1 <- convert_to_long(trend_p1)
trend_p2 <- convert_to_long(trend_p2)

panelB <- vp1 %>%
    bind_rows(vp2) %>%
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
    scale_color_manual(values = c(NO3 = 'blue3', SO4 = 'blue3'),
                       labels = c(NO3 = 'Nitrate', SO4 = 'Sulfate')) +
    scale_shape_manual(values = c(NO3 = 21, SO4 = 21),
                       labels = c(NO3 = 'Nitrate', SO4 = 'Sulfate')) +
    scale_fill_manual(values = c(NO3 = 'white', SO4 = 'blue3'),
                      labels = c(NO3 = 'Nitrate', SO4 = 'Sulfate')) +
    labs(x = "Water-Year",
         y = "Concentration  (µeq/L)",
         title = 'Precipitation') +
    guides(color = guide_legend(title = NULL),
           fill = guide_legend(title = NULL),
           shape = guide_legend(title = NULL)) +
    theme_few() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.9, 0.8)) +
    scale_y_continuous(limits = c(0, 150),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1960, 2050, by = 10),
                       limits = c(1960, 2051),
                       expand = c(0, 0))

panelA + panelB + plot_layout(nrow = 2)#, heights = c(4, 1))

ggsave('~/Desktop/no3_so4.png', width = 6, height = 8)

#pH

vs1 <- s_official %>%
    select(site, waterYr, pH) %>%
    filter(! is.na(pH)) %>%
    group_by(site, waterYr) %>%
    summarize(pH = mean(pH, na.rm = TRUE),
              n = n(),
              .groups = 'drop') %>%
    mutate(source = 'Streamwater') %>%
    arrange(waterYr)

trend_s1 <- get_trendline(vs1, site = site, lims = c(min(vs1$waterYr), 2040))

vs1 <- convert_to_long(vs1)
trend_s1 <- convert_to_long(trend_s1)

panelA <- vs1 %>%
    filter(site == !!site) %>%
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
    scale_color_manual(values = c(pH = 'black')) +
    scale_shape_manual(values = c(pH = 21)) +
    scale_fill_manual(values = c(pH = 'white')) +
    labs(x = "Water-Year",
         y = "pH",
         title = 'Stream water') +
    guides(color = guide_legend(title = NULL),
           fill = guide_legend(title = NULL),
           shape = guide_legend(title = NULL)) +
    theme_few() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.1, 0.9)) +
    scale_y_continuous(limits = c(3.8, 6),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1960, 2050, by = 10),
                       limits = c(1960, 2051),
                       expand = c(0, 0))

#panel B

vp1 <- p_official %>%
    mutate(source = 'Precipitation') %>%
    group_by(site, waterYr, source) %>%
    summarize(pH = mean(pH, na.rm = TRUE),
              site = first(site),
              source = first(source),
              .groups = 'drop',
              n = n()) %>%
    select(site, waterYr, pH, n, source)

trend_p1 <- get_trendline(vp1, site = 'all', lims = c(min(vp1$waterYr), 2040))

vp1 <- convert_to_long(vp1)
trend_p1 <- convert_to_long(trend_p1)

panelB <- vp1 %>%
    # bind_rows(vp2) %>%
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
    scale_color_manual(values = c(pH = 'black')) +
    scale_shape_manual(values = c(pH = 21)) +
    scale_fill_manual(values = c(pH = 'white')) +
    labs(x = "Water-Year",
         y = "pH",
         title = 'Precipitation') +
    guides(color = guide_legend(title = NULL),
           fill = guide_legend(title = NULL),
           shape = guide_legend(title = NULL)) +
    theme_few() +
    theme(legend.position = 'inside',
          legend.position.inside = c(0.1, 0.9)) +
    scale_y_continuous(limits = c(3.8, 6),
                       expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1960, 2050, by = 10),
                       limits = c(1960, 2051),
                       expand = c(0, 0))

panelA + panelB + plot_layout(nrow = 2)#, heights = c(4, 1))

ggsave('~/Desktop/pH.png', width = 6, height = 8)

## 6. fig 4 (various solutes) ####

l_pos <- c(0.8, 0.75) #legend position

vars <- list(c(Ca = 'Calcium', Na = 'Sodium', Mg = 'Magnesium', K = 'Potassium'),
             c(SO4 = 'Sulfate', Cl = 'Chloride', NO3 = 'Nitrate'),
             c(H = 'Hydrogen Ion', ANC = 'ANC'),
             c(DIC = 'DIC', DOC = 'DOC'))

panel_count <- 0
panel_list <- list()
tbl_out <- tibble()
bsln <- min(c(s_official$waterYr, p_official$waterYr))
for(vset in vars){

    panel_count <- panel_count + 1
    vs <- vp <- trend_s <- trend_p <- tibble()
    for(v_ in names(vset)){

        if(v_ == 'DOC'){
            vs_ <- calc_mean_wateryear(s_official, v_, sample_cutoff = cutoff_s)
        } else {
            vs_ <- calc_vwc_wateryear(s_official, v_, sample_cutoff = cutoff_s)
        }
        vs <- bind_rows(vs, convert_to_long(vs_))

        if(! v_ %in% c('ANC', 'DIC')){
            if(v_ == 'DOC'){
                vp_ <- calc_mean_wateryear(p_official, v_, sample_cutoff = cutoff_p)
            } else {
                vp_ <- calc_vwc_wateryear(p_official, v_, sample_cutoff = cutoff_p)
            }
            vp <- bind_rows(vp, convert_to_long(vp_))
        }
    }

    fig4d1 <- vs %>%
        filter(site == !!site)

    write_csv(fig4d1, paste0('data_out/fig4A', panel_count, '_', site, '.csv'))

    for(vvv in names(vset)){
        qq <- filter(fig4d1, var == !!vvv) %>% mutate(waterYr = waterYr - bsln)
        smry <- try(summary(lm(val ~ waterYr, data = qq)),
                    silent = TRUE)
        if(! inherits(smry, 'try-error')){
            tbl_row <- tibble(var = vvv,
                              source = 'stream',
                              slope = smry$coefficients[2, 1],
                              intercept = smry$coefficients[1, 1],
                              R2 = smry$adj.r.squared)
            tbl_out <- bind_rows(tbl_out, tbl_row)
        }

        qq2 <- filter(vp, var == !!vvv) %>% mutate(waterYr = waterYr - bsln)
        smry <- try(summary(lm(val ~ waterYr, data = qq2)),
                    silent = TRUE)
        if(! inherits(smry, 'try-error')){
            tbl_row <- tibble(var = vvv,
                              source = 'precip',
                              slope = smry$coefficients[2, 1],
                              intercept = smry$coefficients[1, 1],
                              R2 = smry$adj.r.squared)
            tbl_out <- bind_rows(tbl_out, tbl_row)
        }
    }

    panel_list[[paste0('A', panel_count)]] <- fig4d1 %>%
        ggplot(aes(x = waterYr, y = val, color = var)) +
        geom_line() +
        geom_point(shape = 20) +
        scale_color_viridis_d(
            begin = 0.1,
            end = 0.8,
            option = 'viridis',
            labels = vset
        ) +
        geom_smooth(
            se = FALSE,
            method = 'lm',
            fullrange = TRUE,
            linewidth = 0.7,
            linetype = 'dotted'
        ) +
        labs(x = "Water Year", y = "µeq/L") +
        guides(color = guide_legend(title = NULL)) +
        theme_few() +
        theme(
            legend.position = 'inside',
            legend.position.inside = if (panel_count == 3) {
                c(0.8, 0.2)
            } else if (panel_count == 4) {
                c(0.2, 0.8)
            } else {
                l_pos
            },
            plot.margin = margin(r = 10)
        ) +
        scale_x_continuous(
            breaks = seq(1960, 2030, by = 10),
            limits = c(1960, 2031),
            expand = c(0, 0)
        )

    if(panel_count == 3){
        panel_list[[paste0('A', panel_count)]] <- panel_list[[paste0('A', panel_count)]] +
            geom_hline(yintercept = 0, linetype = 'dashed',
                       color = 'gray70', linewidth = 0.4)
    }

    write_csv(vp, paste0('data_out/fig4B', panel_count, '_', site, '.csv'))

    panel_list[[paste0('B', panel_count)]] <- vp %>%
        ggplot(aes(x = waterYr, y = val, color = var)) +
        geom_line() +
        geom_point(shape = 20) +
        scale_color_viridis_d(
            begin = 0.1,
            end = 0.8,
            option = 'D',
            labels = vset,
            direction = ifelse(panel_count == 3, -1, 1)
        ) +
        geom_smooth(
            se = FALSE,
            method = 'lm',
            fullrange = TRUE,
            linewidth = 0.7,
            linetype = 'dotted'
        ) +
        labs(x = "Water Year", y = "µeq/L") +
        guides(color = guide_legend(title = NULL)) +
        theme_few() +
        theme(
            legend.position = 'inside',
            #   legend.position.inside = l_pos) +
            legend.position.inside = if (panel_count == 4) {
                c(0.2, 0.8)
            } else {
                l_pos
            }
        ) +
        scale_x_continuous(
            breaks = seq(1960, 2030, by = 10),
            limits = c(1960, 2031),
            expand = c(0, 0)
        )
}

panel_list$A1 + panel_list$B1 + panel_list$A2 + panel_list$B2 +
    panel_list$A3 + panel_list$B3 + panel_list$A4 + panel_list$B4 +
    plot_layout(nrow = 4, byrow = TRUE, axes = 'collect')

# tbl_out %>%
#     arrange(source) %>%
#     write_csv('/tmp/zz.csv')

ggsave(paste0('figs/fig4_', site, '.png'), width = 9, height = 12)

## 7. fig 5 (mainstem annual EC) ####

v_ <- 'spCond'

spcond_s <- calc_mean_year(s, v_, sample_cutoff = cutoff_s)

bsln <- min(spcond_s$waterYr)
trend_hbk <- get_trendline(spcond_s, site = 'HBK',
                           lims = c(min(spcond_s$waterYr[spcond_s$site == 'HBK']), 2040),
                           baseline = bsln)
trend_w3 <- get_trendline(spcond_s, site = 'W3',
                          lims = c(min(spcond_s$waterYr[spcond_s$site == 'W3']), 2040),
                          baseline = bsln)
trend_w6 <- get_trendline(spcond_s, site = 'W6',
                          lims = c(min(spcond_s$waterYr[spcond_s$site == 'W6']), 2040),
                          baseline = bsln)
trend_w9 <- get_trendline(spcond_s, site = 'W9',
                          lims = c(min(spcond_s$waterYr[spcond_s$site == 'W9']), 2040),
                          baseline = bsln)

spcond_s <- rename(spcond_s, year = waterYr)
trend_hbk <- rename(trend_hbk, year = waterYr)
trend_w3 <- rename(trend_w3, year = waterYr)
trend_w6 <- rename(trend_w6, year = waterYr)
trend_w9 <- rename(trend_w9, year = waterYr)

fig5d <- spcond_s %>%
    filter(site %in% c('HBK', 'W3', 'W6', 'W9'))

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
    geom_line(data = trend_w6,
              aes(x = year, y = spCond, color = site),
              linetype = 'dashed',
              linewidth = 0.3,
              show.legend = FALSE) +
    geom_line(data = trend_w9,
              aes(x = year, y = spCond, color = site),
              linetype = 'dashed',
              linewidth = 0.3,
              show.legend = FALSE) +
    scale_color_manual(values = c(HBK = 'blue3', W3 = 'red', W6 = 'gray40', W9 = 'black'),
                       labels = c(HBK = 'Hubbard Brook', W3 = 'W3', W6 = 'W6', W9 = 'W9')) +
    scale_shape_manual(values = c(HBK = 20, W3 = 20, W6 = 20, W9 = 20),
                       labels = c(HBK = 'Hubbard Brook', W3 = 'W3', W6 = 'W6',  W9 = 'W9')) +
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

## 8. breakpoint analyses (stream, monthly) [OBSOLETE] ####

vars_included <- setdiff(colnames(s_official), c('site', 'date', 'waterYr', 'flow_mm'))
s_bp_table <- tibble()
s_multi_bp <- list()

for(v_ in vars_included){

    first_non_na <- Position(\(x) ! is.na(x), s_official[[v_]])
    last_non_na <- Position(\(x) ! is.na(x), s_official[[v_]], right = TRUE)

    min_date <- s_official$date[first_non_na]
    max_date <- s_official$date[last_non_na]

    s_seg <- s_official %>%
        slice(first_non_na:nrow(s_official)) %>%
        mutate(date_int = as.numeric(date - !!min_date)) %>%
        select(!!v_, date, date_int)

    lm_mod <- lm(reformulate('date_int', v_), data = s_seg)
    seg1 <- segmented(lm_mod, seg.Z = ~date_int, npsi = 1)
    seg2 <- segmented(lm_mod, seg.Z = ~date_int, npsi = 2)
    seg3 <- segmented(lm_mod, seg.Z = ~date_int, npsi = 3)

    png(paste0('figs/breakpoints/stream_1bp/', v_, '.png'))
    plot(s_seg$date_int, s_seg[[v_]], pch = 16, col = "grey",
         xlab = '', ylab = v_, xaxt = 'n')
    plot(seg1, add = TRUE)
    s_axis <- s_seg %>%
        group_by(floor_date(date, '10 year')) %>%
        slice(1) %>%
        mutate(date = strftime(date, '%e/%-m/%y')) %>%
        ungroup()
    axis(1, s_axis$date_int, s_axis$date, srt = 45)
    # plot(seg2, add = TRUE)
    dev.off()

    breakpoint <- seg1$psi[, "Est."]
    break_date <- min_date + breakpoint
    bp_SE_days <- seg1$psi[, "St.Err"]

    bp_supported_BIC <- diff(BIC(lm_mod, seg1)$BIC) < 0
    multi_bp2 <- diff(BIC(seg1, seg2)$BIC) < 0
    multi_bp3 <- diff(BIC(seg1, seg3)$BIC) < 0
    multi_bp <- multi_bp2 | multi_bp3
    bp_supported_davies <- davies.test(lm_mod, ~date_int, k = 100)$p.value < 0.05
    if(multi_bp){
        n_bp <- c(2, 3)[which.min(BIC(seg1, seg2)$BIC)]
        # warning(n_bp, ' breakpoints plausible for ', v_)
        s_multi_bp[[v_]] <- n_bp
    }
    # bp_supported_davies <- davies.test(seg2, ~date_int)$p.value < 0.05

    intc <- round(seg1$coefficients[1], 3)
    slope1 <- round(seg1$coefficients[2], 3)
    slope2 <- round(seg1$coefficients[2] + seg1$coefficients[3], 3)

    eqn1 <- sprintf('y = %.3f %.3f', intc, slope1)
    eqn2 <- sprintf('y = %.3f %+.3f', intc, slope2)

    bp_row <- tibble(solute = v_,
                     period1 = paste(min_date, '-', break_date),
                     period2 = paste(break_date, '-', max_date),
                     eqn1 = eqn1,
                     eqn2 = eqn2,
                     breakpoint = break_date,
                     bp_SE_days = bp_SE_days,
                     bp_supported_BIC = bp_supported_BIC,
                     bp_supported_davies = bp_supported_davies,
                     multiple_bp_support = multi_bp)

    s_bp_table <- s_bp_table %>%
        bind_rows(bp_row)
}

for(v_ in s_bp_table[s_bp_table$multiple_bp_support, ]$solute){

    first_non_na <- Position(\(x) ! is.na(x), s_official[[v_]])
    last_non_na <- Position(\(x) ! is.na(x), s_official[[v_]], right = TRUE)

    min_date <- s_official$date[first_non_na]
    max_date <- s_official$date[last_non_na]

    s_seg <- s_official %>%
        slice(first_non_na:nrow(s_official)) %>%
        mutate(date_int = as.numeric(date - !!min_date)) %>%
        select(!!v_, date, date_int)

    n_bp <- s_multi_bp[[v_]]
    lm_mod <- lm(reformulate('date_int', v_), data = s_seg)
    seg1 <- segmented(lm_mod, seg.Z = ~date_int, npsi = n_bp)

    png(paste0('figs/breakpoints/stream_multi_bp/', v_, '.png'))
    plot(s_seg$date_int, s_seg[[v_]], pch = 16, col = "grey",
         xlab = '', ylab = v_, xaxt = 'n')
    plot(seg1, add = TRUE)
    s_axis <- s_seg %>%
        group_by(floor_date(date, '10 year')) %>%
        slice(1) %>%
        mutate(date = strftime(date, '%e/%-m/%y')) %>%
        ungroup()
    axis(1, s_axis$date_int, s_axis$date, srt = 45)
    # plot(seg2, add = TRUE)
    dev.off()

    # breakpoint <- seg1$psi[, "Est."]
    # break_date <- min_date + breakpoint
    # bp_SE_days <- seg1$psi[, "St.Err"]
    #
    # intc <- round(seg1$coefficients[1], 3)
    # slope1 <- round(seg1$coefficients[2], 3)
    # slope2 <- round(seg1$coefficients[2] + seg1$coefficients[3], 3)
    #
    # eqn1 <- sprintf('y = %.3f %.3f', intc, slope1)
    # eqn2 <- sprintf('y = %.3f %+.3f', intc, slope2)
    #
    # bp_row <- tibble(solute = v_,
    #                  period1 = paste(min_date, '-', break_date),
    #                  period2 = paste(break_date, '-', max_date),
    #                  eqn1 = eqn1,
    #                  eqn2 = eqn2,
    #                  breakpoint = break_date,
    #                  bp_SE_days = bp_SE_days,
    #                  bp_supported_BIC = bp_supported_BIC,
    #                  bp_supported_davies = bp_supported_davies,
    #                  multiple_bp_support = multi_bp)
    #
    # s_bp_table <- s_bp_table %>%
    #     bind_rows(bp_row)
}

## 8b. breakpoint analyses (stream, wateryear) ####

vars_included <- setdiff(colnames(s_official), c('site', 'date', 'waterYr', 'flow_mm'))
s_bp_table <- tibble()
s_multi_bp <- list()

s_official <- s_official %>%
    group_by(waterYr) %>%
    summarize(across(spCond:base_cat, ~mean(., na.rm = TRUE)),
              .groups = 'drop') %>%
    rename(date = waterYr)

for(v_ in vars_included){

    first_non_na <- Position(\(x) ! is.na(x), s_official[[v_]])
    last_non_na <- Position(\(x) ! is.na(x), s_official[[v_]], right = TRUE)

    min_date <- s_official$date[first_non_na]
    max_date <- s_official$date[last_non_na]

    s_seg <- s_official %>%
        slice(first_non_na:nrow(s_official)) %>%
        mutate(date_int = as.numeric(date - !!min_date)) %>%
        select(!!v_, date, date_int)

    lm_mod <- lm(reformulate('date_int', v_), data = s_seg)
    seg1 <- segmented(lm_mod, seg.Z = ~date_int, npsi = 1)
    seg2 <- segmented(lm_mod, seg.Z = ~date_int, npsi = 2)
    seg3 <- try(segmented(lm_mod, seg.Z = ~date_int, npsi = 3), silent = TRUE)

    png(paste0('figs/breakpoints/stream_1bp/', v_, '.png'))
    plot(s_seg$date_int, s_seg[[v_]], pch = 16, col = "grey",
         xlab = '', ylab = v_, xaxt = 'n')
    plot(seg1, add = TRUE)
    s_axis <- s_seg %>%
        filter(date %% 10 == 0)
    axis(1, s_axis$date_int, s_axis$date)
    # plot(seg2, add = TRUE)
    dev.off()

    breakpoint <- seg1$psi[, "Est."]
    break_date <- round(min_date + breakpoint, 2)
    bp_SE_yrs <- seg1$psi[, "St.Err"]

    bp_supported_BIC <- diff(BIC(lm_mod, seg1)$BIC) < 0
    if(inherits(seg2, 'segmented')){
        multi_bp2 <- diff(BIC(seg1, seg2)$BIC) < 0
    } else {
        multi_bp2 <- FALSE
    }
    if(! inherits(seg3, 'try-error') && inherits(seg3, 'segmented')){
        multi_bp3 <- diff(BIC(seg1, seg3)$BIC) < 0
    } else {
        multi_bp3 <- FALSE
    }
    multi_bp <- multi_bp2 | multi_bp3
    bp_supported_davies <- davies.test(lm_mod, ~date_int, k = 100)$p.value < 0.05
    if(multi_bp){
        n_bp <- c(2, 3)[which.min(BIC(seg2, seg3)$BIC)]
        # warning(n_bp, ' breakpoints plausible for ', v_)
        s_multi_bp[[v_]] <- n_bp
    }
    # bp_supported_davies <- davies.test(seg2, ~date_int)$p.value < 0.05

    intc <- round(seg1$coefficients[1], 3)
    slope1 <- round(seg1$coefficients[2], 3)
    slope2 <- round(seg1$coefficients[3], 3)
    # slope2 <- round(seg1$coefficients[2] + seg1$coefficients[3], 3)

    eqn1 <- sprintf('y = %.3f %+.3fx %+.3f(x-%.3f) + ϵ', intc, slope1, slope2, round(breakpoint, 3))
    # eqn2 <- sprintf('y = %.3f %+.3f', intc, slope2)

    bp_row <- tibble(solute = v_,
                     period1 = paste(min_date, '-', round(break_date, 0)),
                     period2 = paste(round(break_date, 0), '-', max_date),
                     breakpoint = round(break_date, 0),
                     eqn = eqn1,
                     # eqn2 = eqn2,
                     bp_SE_yrs = bp_SE_yrs,
                     bp_supported_BIC = bp_supported_BIC,
                     bp_supported_davies = bp_supported_davies,
                     multiple_bp_support = multi_bp)

    s_bp_table <- s_bp_table %>%
        bind_rows(bp_row)
}

write_csv(s_bp_table, 'data_out/single_breakpoint_table_stream.csv')

for(v_ in s_bp_table[s_bp_table$multiple_bp_support, ]$solute){

    first_non_na <- Position(\(x) ! is.na(x), s_official[[v_]])
    last_non_na <- Position(\(x) ! is.na(x), s_official[[v_]], right = TRUE)

    min_date <- s_official$date[first_non_na]
    max_date <- s_official$date[last_non_na]

    s_seg <- s_official %>%
        slice(first_non_na:nrow(s_official)) %>%
        mutate(date_int = as.numeric(date - !!min_date)) %>%
        select(!!v_, date, date_int)

    n_bp <- s_multi_bp[[v_]]
    lm_mod <- lm(reformulate('date_int', v_), data = s_seg)
    seg1 <- segmented(lm_mod, seg.Z = ~date_int, npsi = n_bp)

    png(paste0('figs/breakpoints/stream_multi_bp/', v_, '.png'))
    plot(s_seg$date_int, s_seg[[v_]], pch = 16, col = "grey",
         xlab = '', ylab = v_, xaxt = 'n')
    plot(seg1, add = TRUE)
    s_axis <- s_seg %>%
        filter(date %% 10 == 0)
    axis(1, s_axis$date_int, s_axis$date)
    # plot(seg2, add = TRUE)
    dev.off()

    # breakpoint <- seg1$psi[, "Est."]
    # break_date <- min_date + breakpoint
    # bp_SE_days <- seg1$psi[, "St.Err"]
    #
    # intc <- round(seg1$coefficients[1], 3)
    # slope1 <- round(seg1$coefficients[2], 3)
    # slope2 <- round(seg1$coefficients[2] + seg1$coefficients[3], 3)
    #
    # eqn1 <- sprintf('y = %.3f %.3f', intc, slope1)
    # eqn2 <- sprintf('y = %.3f %+.3f', intc, slope2)
    #
    # bp_row <- tibble(solute = v_,
    #                  period1 = paste(min_date, '-', break_date),
    #                  period2 = paste(break_date, '-', max_date),
    #                  eqn1 = eqn1,
    #                  eqn2 = eqn2,
    #                  breakpoint = break_date,
    #                  bp_SE_days = bp_SE_days,
    #                  bp_supported_BIC = bp_supported_BIC,
    #                  bp_supported_davies = bp_supported_davies,
    #                  multiple_bp_support = multi_bp)
    #
    # s_bp_table <- s_bp_table %>%
    #     bind_rows(bp_row)
}

## 8c. breakpoint analyses (precip, wateryear) ####

vars_included <- setdiff(colnames(p_official), c('site', 'date', 'waterYr', 'flow_mm', 'precip'))
p_bp_table <- tibble()
p_multi_bp <- list()

p_official <- p_official %>%
    group_by(waterYr) %>%
    summarize(across(spCond:base_cat, ~mean(., na.rm = TRUE)),
              .groups = 'drop') %>%
    rename(date = waterYr)

for(v_ in vars_included){

    first_non_na <- Position(\(x) ! is.na(x), p_official[[v_]])
    last_non_na <- Position(\(x) ! is.na(x), p_official[[v_]], right = TRUE)

    min_date <- p_official$date[first_non_na]
    max_date <- p_official$date[last_non_na]

    s_seg <- p_official %>%
        slice(first_non_na:nrow(p_official)) %>%
        mutate(date_int = as.numeric(date - !!min_date)) %>%
        select(!!v_, date, date_int)

    lm_mod <- lm(reformulate('date_int', v_), data = s_seg)
    seg1 <- try(segmented(lm_mod, seg.Z = ~date_int, npsi = 1), silent = TRUE)
    if(inherits(seg1, 'try-error')) next
    seg2 <- segmented(lm_mod, seg.Z = ~date_int, npsi = 2)
    seg3 <- try(segmented(lm_mod, seg.Z = ~date_int, npsi = 3), silent = TRUE)

    png(paste0('figs/breakpoints/precip_1bp/', v_, '.png'))
    plot(s_seg$date_int, s_seg[[v_]], pch = 16, col = "grey",
         xlab = '', ylab = v_, xaxt = 'n')
    plot(seg1, add = TRUE)
    s_axis <- s_seg %>%
        filter(date %% 10 == 0)
    axis(1, s_axis$date_int, s_axis$date)
    # plot(seg2, add = TRUE)
    dev.off()

    breakpoint <- seg1$psi[, "Est."]
    break_date <- round(min_date + breakpoint, 2)
    bp_SE_yrs <- seg1$psi[, "St.Err"]

    bp_supported_BIC <- diff(BIC(lm_mod, seg1)$BIC) < 0
    if(inherits(seg2, 'segmented')){
        multi_bp2 <- diff(BIC(seg1, seg2)$BIC) < 0
    } else {
        multi_bp2 <- FALSE
    }
    if(! inherits(seg3, 'try-error') && inherits(seg3, 'segmented')){
        multi_bp3 <- diff(BIC(seg1, seg3)$BIC) < 0
    } else {
        multi_bp3 <- FALSE
    }
    multi_bp <- multi_bp2 | multi_bp3
    bp_supported_davies <- davies.test(lm_mod, ~date_int, k = 100)$p.value < 0.05
    if(multi_bp){
        n_bp <- c(2, 3)[which.min(BIC(seg2, seg3)$BIC)]
        # warning(n_bp, ' breakpoints plausible for ', v_)
        p_multi_bp[[v_]] <- n_bp
    }
    # bp_supported_davies <- davies.test(seg2, ~date_int)$p.value < 0.05

    intc <- round(seg1$coefficients[1], 3)
    slope1 <- round(seg1$coefficients[2], 3)
    slope2 <- round(seg1$coefficients[3], 3)
    # slope2 <- round(seg1$coefficients[2] + seg1$coefficients[3], 3)

    eqn1 <- sprintf('y = %.3f %+.3fx %+.3f(x-%.3f) + ϵ', intc, slope1, slope2, round(breakpoint, 3))
    # eqn2 <- sprintf('y = %.3f %+.3f', intc, slope2)

    bp_row <- tibble(solute = v_,
                     period1 = paste(min_date, '-', round(break_date, 0)),
                     period2 = paste(round(break_date, 0), '-', max_date),
                     breakpoint = round(break_date, 0),
                     eqn = eqn1,
                     # eqn2 = eqn2,
                     bp_SE_yrs = bp_SE_yrs,
                     bp_supported_BIC = bp_supported_BIC,
                     bp_supported_davies = bp_supported_davies,
                     multiple_bp_support = multi_bp)

    p_bp_table <- p_bp_table %>%
        bind_rows(bp_row)
}

write_csv(p_bp_table, 'data_out/single_breakpoint_table_precip.csv')

for(v_ in p_bp_table[p_bp_table$multiple_bp_support, ]$solute){

    first_non_na <- Position(\(x) ! is.na(x), p_official[[v_]])
    last_non_na <- Position(\(x) ! is.na(x), p_official[[v_]], right = TRUE)

    min_date <- p_official$date[first_non_na]
    max_date <- p_official$date[last_non_na]

    s_seg <- p_official %>%
        slice(first_non_na:nrow(p_official)) %>%
        mutate(date_int = as.numeric(date - !!min_date)) %>%
        select(!!v_, date, date_int)

    n_bp <- p_multi_bp[[v_]]
    lm_mod <- lm(reformulate('date_int', v_), data = s_seg)
    seg1 <- segmented(lm_mod, seg.Z = ~date_int, npsi = n_bp)

    png(paste0('figs/breakpoints/precip_multi_bp/', v_, '.png'))
    plot(s_seg$date_int, s_seg[[v_]], pch = 16, col = "grey",
         xlab = '', ylab = v_, xaxt = 'n')
    plot(seg1, add = TRUE)
    s_axis <- s_seg %>%
        filter(date %% 10 == 0)
    axis(1, s_axis$date_int, s_axis$date)
    dev.off()
}

# just exporting data ####

#might have to run the commented block above, that begins:
# s_official = s %>% ...

vv = setdiff(colnames(s), c('site', 'date', 'waterYr', 'discharge', 'spCond', 'ANC'))
ll = calc_vwc_wateryear(s_official, 'ANC') %>%
    select(-site, -n, -source)
for(v in vv){
    ll = calc_vwc_wateryear(s_official, v) %>%
        select(-site, -n, -source) %>%
        left_join(ll, by = 'waterYr')
}

write_csv(ll, paste0('/tmp/', site, '_mgL.csv'))
