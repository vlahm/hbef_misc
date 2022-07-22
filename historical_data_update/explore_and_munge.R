library(tidyverse)
library(lubridate)
library(ggplot2)

setwd('~/git/hbef/hbef_misc/historical_data_update/')

d = readxl::read_xlsx('20220713HBEFChem.xlsx', sheet = 2, na = 'NA', guess_max = 25000) %>%
    filter(Site != 'AvgRG1+RG11')
d$Site[d$Site == 'RG-North'] = 'N'
d$Site[d$Site == 'RG-South'] = 'S'
d$Site[d$Site == 'STA/22'] = 'RG22'
d = select(d, -`Al-FerronNotes`, -Project, -starts_with('SubProject'), -ends_with('Likens'))

d0 = read_csv('hbef_historical.csv', na = '\\N', col_names = FALSE, guess_max = 25000)
hist_names = c('refNo','site','date','timeEST','pH','DIC','spCond','temp','ANC960','ANCMet','gageHt','hydroGraph','flowGageHt','precipCatch','fieldCode','notes','uniqueID','waterYr','datetime','Ca','Mg','K','Na','TMAl','OMAl','Al_ICP','NH4','SO4','NO3','Cl','PO4','DOC','TDN','DON','SiO2','Mn','Fe','F','cationCharge','anionCharge','theoryCond','ionError','duplicate','sampleType','ionBalance','canonical')
colnames(d0) = hist_names

#got an additional W7 file from Jeff at some point

w7 = readxl::read_xlsx('W7 1995-2013 update.xlsx', na = 'NA', guess_max = 25000)
w7 = select(w7, -Project, -starts_with('SubProject'), -ends_with('Likens'))
# dw7 = filter(d, Site == 'W7')
# setdiff(paste(w7$SampleDate, w7$SampleTime), paste(dw7$SampleDate, dw7$SampleTime))
# setdiff(paste(dw7$SampleDate, dw7$SampleTime), paste(w7$SampleDate, w7$SampleTime))
d = filter(d, Site != 'W7')
d = bind_rows(d, w7)

# setdiff(colnames(w7), colnames(d))
# setdiff(colnames(d), colnames(w7))

#site names ####
known_sites = c('HBK', 'N', 'RG1', 'RG11', 'RG19', 'RG22', 'RG23', 'S', 'W1', 'W2', 'W3', 'W4', 'W5', 'W6', 'W7', 'W7-Precip', 'W8', 'W9')
d_sites = unique(d$Site)

setdiff(known_sites, d_sites)
setdiff(d_sites, known_sites)
paste(d_sites, collapse = ', ')

#time stuff ####
# t1 = gsub(':', '', substr(d$SampleTime, 12, 19))
# t2 = as.character(d$EST)
# gg = tibble(t1, t2) %>%
#     dplyr::filter(if_any(everything(), ~! is.na(.)))
# sum(is.na(gg$t1))
# sum(is.na(gg$t2))
# table(substr(t1, 5, 6))
#
# t1 = as.numeric(substr(t1, 1, 4))
# t2 = as.numeric(t2)
#
# gg2 = tibble(t1, t2) %>%
#     filter(if_all(everything(), ~! is.na(.)))
# identical(gg2$t1, gg2$t2)
#
# mutate(gg2, diff = t1 - t2) %>% filter(diff != 0)
# mutate(gg2, diff = t1 - t2) %>% filter(diff != 0) %>% pull(t2) %>% table()
# mutate(gg2, diff = t1 - t2) %>% filter(diff != 0, ! grepl('\\.', as.character(t2)))
#
# filter(d, round(as.numeric(as.character(t2)), 3) == 0.333)
#
# filter(d, is.na(SampleTime) & ! is.na(EST)) %>% View()

#site N times are in UTC in d (determined below)
# filter(d, Site == 'N') %>% select(SampleDate, SampleTime, EST) %>% pull(SampleTime) %>% substr(12, 13) %>% as.numeric() %>% max(., na.rm=T)

#okay, so the EST column is bollocks
d = d %>%
    mutate(date = ymd(d$SampleDate),
           timeEST = substr(d$SampleTime, 12, 19)) %>%
    select(-EST, -SampleDate, -SampleTime)

# filter(d0, ! is.na(DOC)) %>% select(site, date, timeEST, DOC) %>% arrange(desc(date))
# filter(d, ! is.na(DOC)) %>% select(Site, SampleDate, SampleTime, EST, DOC) %>% arrange(desc(SampleDate))

#column names ####
d_names = colnames(d)
# setdiff(hist_names, d_names)
# setdiff(d_names, hist_names)
# setdiff(tolower(hist_names), tolower(d_names))
# setdiff(tolower(d_names), tolower(hist_names))
# intersect(tolower(d_names), tolower(hist_names))
# intersect(d_names, hist_names)

var_map = c(ANC = 'ANC960',
            Cation = 'cationCharge',
            Anion = 'anionCharge',
            Flow = 'flowGageHt',
            Hydro = 'hydroGraph',
            SpecCond = 'spCond',
            TheoryCond = 'theoryCond',
            IonBal = 'ionBalance',
            IonErr = 'ionError',
            `Al-ICP` = 'Al_ICP',
            GageHt = 'gageHt',
            # SampleDate = 'date',
            # SampleTime = 'timeEST',
            Temp_C = 'temp',
            Site = 'site',
            PrecipCatch = 'precipCatch')

inds = base::match(names(var_map), colnames(d))
colnames(d)[inds] = unname(var_map)

var_map2 = intersect(d_names, hist_names)
names(var_map2) = var_map2
var_map = append(var_map, as.list(var_map2))

setdiff(tolower(colnames(d0)), tolower(colnames(d)))
setdiff(tolower(colnames(d)), tolower(colnames(d0)))

#some times are entered wrong in one dataset or another. correct them here. ####


# select(to_rebuild, where(~any(! is.na(.))))
# print(qqq, n=100)
d0$timeEST = as.character(d0$timeEST)

d[d$site == 'W1' & d$date == as.Date('1999-10-19') & is.na(d$timeEST), 'timeEST'] = '00:00:00'
d[d$site == 'W1' & d$date == as.Date('1999-10-20') & is.na(d$timeEST), 'timeEST'] = '00:00:00'
d[d$site == 'W1' & d$date == as.Date('2006-10-02'), 'timeEST'] = '14:35:00'
d[d$site == 'W1' & d$date == as.Date('2006-11-20'), 'timeEST'] = '13:55:00'
d[d$site == 'W1' & d$date == as.Date('2007-03-05'), 'timeEST'] = '13:55:00'
d0[d0$site == 'W1' & d0$date == as.Date('2007-05-29'), 'timeEST'] = '10:00:00'
d[d$site == 'W1' & d$date == as.Date('2007-10-11'), 'timeEST'] = NA
d[d$site == 'W1' & d$date == as.Date('2008-11-24'), 'timeEST'] = '14:35:00'
# d[d$site == 'W1' & d$date == as.Date('2013-03-13'), c('date', 'timeEST')]

d[d$site == 'W1' & d$date == as.Date('2013-03-12') & d$timeEST == '00:00:00', 'date'] = as.Date('2013-03-13')
d0[d0$site == 'W1' & d0$date == as.Date('2013-03-12') & is.na(d0$timeEST), 'timeEST'] = '00:00:00'
d0[d0$site == 'W1' & d0$date == as.Date('2013-03-12') & d0$timeEST == '00:00:00', 'date'] = as.Date('2013-03-13')

# d[d$site == 'W1' & d$date == as.Date('2013-03-13') & d$timeEST == '00:00:00', c('date', 'timeEST')]
# d0[d0$site == 'W1' & d0$date == as.Date('2013-03-13'), 'timeEST']

d[d$site == 'W1' & d$date == as.Date('2012-09-18') & d$timeEST == '00:00:00', 'date'] = as.Date('2012-09-19')
d0[d0$site == 'W1' & d0$date == as.Date('2012-09-18') & is.na(d0$timeEST), 'timeEST'] = '00:00:00'
d0[d0$site == 'W1' & d0$date == as.Date('2012-09-18') & d0$timeEST == '00:00:00', 'date'] = as.Date('2012-09-19')

d[d$site == 'W1' & d$date == as.Date('2012-10-29') & d$timeEST == '00:00:00', 'date'] = as.Date('2012-10-30')
d0[d0$site == 'W1' & d0$date == as.Date('2012-10-29') & is.na(d0$timeEST), 'timeEST'] = '00:00:00'
d0[d0$site == 'W1' & d0$date == as.Date('2012-10-29') & d0$timeEST == '00:00:00', 'date'] = as.Date('2012-10-30')

d[d$site == 'W6' & d$date == as.Date('2007-10-11'), 'timeEST'] = NA
d[d$site == 'W1' & d$date == as.Date('2012-05-17'), 'timeEST'] #actual orphan (dupe)
d[d$site == 'W1' & d$date == as.Date('2012-10-28') & d$timeEST == '00:00:00', 'timeEST'] = NA
d[d$site == 'W2' & d$date == as.Date('2011-12-26'), 'timeEST'] #actual orphan (dupe)

d[d$site == 'W3' & d$date == as.Date('1991-07-15'), 'timeEST'] #actual orphan
d[d$site == 'W5' & d$date == as.Date('2009-07-06'), 'timeEST'] #actual orphan
d[d$site == 'W5' & d$date == as.Date('2009-07-13'), 'timeEST'] #actual orphan

d$duplicate = NA_character_
d$duplicate[duplicated(d[, c('site', 'date', 'timeEST')])] = 'Dup'
d = d[! (d$site == 'HBK' & d$date == as.Date('2013-05-13') & ! is.na(d$duplicate)),]
d$duplicate = NULL #will reattach later


# d0[d0$site == 'W1' & d0$date == as.Date('2012-05-17'), 'duplicate']
# d[d$site == 'W1' & d$date == as.Date('2012-05-17'), 'duplicate']
# tt = 'W6'; dd = as.Date('1989-10-18')
# bind_rows(d0[d0$site == tt & d0$date == dd,],
#           to_rebuild[to_rebuild$site == tt & to_rebuild$date == dd, ])
# filter(d0, site == tt, year(date) == 1989) %>% View()
#
# for(i in 20:nrow(to_rebuild)){
#
#     r = to_rebuild[i, ]
#
#     print(paste(r$site, r$date))
#     avail_cols = colnames(select(r, where(~any(! is.na(.)))))
#     comparison_pool = filter(d0, site == r$site, year(date) == year(r$date))
#
#     diffs = rep(NA, nrow(comparison_pool))
#     for(j in seq_len(nrow(comparison_pool))){
#         cmp = comparison_pool[j, ]
#         cmp_cols = bind_rows(r, cmp) %>%
#             select(-refNo, -site, -date, -timeEST, -waterYr, -uniqueID, -notes,
#                    -fieldCode, -duplicate, -sampleType, -canonical, -datetime) %>%
#             select(any_of(avail_cols))
#         cmp_cols[1, ] = t(scale(unlist(cmp_cols[1, ])))
#         cmp_cols[2, ] = t(scale(unlist(cmp_cols[2, ])))
#         diffs[j] = sum(abs(cmp_cols[1, ] - cmp_cols[2, ]))
#     }
#
#     print(t(bind_rows(r, comparison_pool[which.min(diffs), ])))
#     # t(bind_rows(select(r, -refNo, -site, -date, -timeEST, -waterYr, -uniqueID, -notes,
#     #                    -fieldCode, -duplicate, -sampleType, -canonical, -datetime),
#     #             select(comparison_pool[which.min(diffs), ],
#     #                    -refNo, -site, -date, -timeEST, -waterYr, -uniqueID, -notes,
#     #                    -fieldCode, -duplicate, -sampleType, -canonical, -datetime)))
#     catch = readLines(con = stdin(), 1)
#
#     print(filter(qqq, site == r$site, date == r$date), n=100)
#     catch = readLines(con = stdin(), 1)
# }
# select(r, where(~any(! is.na(.))))
# filter(d0, site == r$site, date == r$date) %>% select(duplicate, everything()) %>% print(n=100)
# bind_rows(r, d0) %>% filter(site == r$site, date == r$date) %>% select(timeEST, any_of(avail_cols)) %>% print(n=100)
# bind_rows(r, d0) %>% filter(site == r$site, date == r$date) %>% select(timeEST, everything()) %>% print(n=100) %>% View()
#
ff = filter(d, date %in% as.Date(c('2012-09-17', '2012-09-18', '2012-09-19')), site == 'W1') %>% mutate(dt = ymd_hms(paste(date, timeEST))) %>% arrange(dt)
plot(ff$dt, ff$gageHt)
# ff = filter(d, date %in% as.Date(c('1999-10-18', '1999-10-19', '1999-10-20')), site == 'W1') %>% mutate(dt = ymd_hms(paste(date, timeEST))) %>% arrange(dt)
# plot(ff$dt, ff$gageHt)
#
# bind_rows(r, filter(d0, site=='W3', year(date) == 1991)) %>% View()
# bind_rows(r, filter(d0, site=='W5', is.na(cationCharge), is.na(ionError), is.na(ionBalance), is.na(gageHt))) %>% View()
# bind_rows(r, filter(d0, site=='W6', is.na(cationCharge), is.na(ionError), is.na(ionBalance), is.na(gageHt))) %>% View()


# ANCILLARY earliest dates by variable ####

# earliest_date_comparisons = tibble()
# for(i in seq_along(var_map)){
#     v = unlist(var_map[i], use.names = FALSE)
#     tryCatch({
#         d_earliest = filter(d, ! is.na(!!sym(v))) %>% summarize(mindate = min(date)) %>% pull() %>% as.Date()
#         d0_earliest = filter(d0, ! is.na(!!sym(v))) %>% summarize(mindate = min(date)) %>% pull()
#         earliest_date_comparisons = bind_rows(earliest_date_comparisons,
#                   tibble(variable = v, first_record_portal = d0_earliest,
#                          first_record_file = d_earliest,
#                          earlier = case_when(d_earliest < d0_earliest ~ 'file',
#                                              d_earliest == d0_earliest ~ '-',
#                                              TRUE ~ 'portal')))
#     }, warning = function(w) print('a'))
# }
#
# select(d0, site, date, !!v) %>% filter(! is.na(!!sym(v))) %>% arrange(date)
# select(d0, site, date, !!v) %>% filter(! is.na(!!sym(v))) %>% arrange(date) %>% View()
# select(d, Site, date, !!v) %>% filter(! is.na(!!sym(v))) %>% arrange(date)
# select(d, Site, date, !!v) %>% filter(! is.na(!!sym(v))) %>% arrange(date) %>% View()
#
# write_csv(earliest_date_comparisons, 'earliest_date_comparisons.csv')

# ANCILLARY last dates by variable ####

# last_date_comparisons = tibble()
# for(i in seq_along(var_map)){
#     v = unlist(var_map[i], use.names = FALSE)
#     tryCatch({
#         d_earliest = filter(d, ! is.na(!!sym(v))) %>% summarize(mindate = max(date)) %>% pull() %>% as.Date()
#         d0_earliest = filter(d0, ! is.na(!!sym(v))) %>% summarize(mindate = max(date)) %>% pull()
#         last_date_comparisons = bind_rows(last_date_comparisons,
#                                           tibble(variable = v, first_record_portal = d0_earliest,
#                                                  first_record_file = d_earliest,
#                                                  later = case_when(d_earliest < d0_earliest ~ 'portal',
#                                                                    d_earliest == d0_earliest ~ '-',
#                                                                    TRUE ~ 'file')))
#     }, warning = function(w) print('a'))
# }

#correct dates in future, remove Al_ICP from d0 ####

err_date_inds = d0$date > Sys.Date()
err_dates = d0$date[err_date_inds]
fixed_dates = as.Date(sub('^20([0-9]{2})', '19\\1', as.character(err_dates)))
d0$date[err_date_inds] = fixed_dates

d0$Al_ICP = NA_real_

# for pre comparison_plot.Rmd ####
save.image('for_comparison_plot_before.rda')
# ANCILLARY plot the variables that appear to go farther back in the new file ####

# d = rename(d, site = Site)
# vv = colnames(d)
# vv = vv[! vv %in% c('date', 'timeEST', 'site', 'Discharge_ls', 'Pass', 'FieldCode', 'VarCode', 'hydroGraph', 'Al-Ferron', 'ANCMet', 'PrecipCatch')]
# for(v in vv){
#
#     v_portal = paste0(v, '_portal')
#     v_file = paste0(v, '_file')
#
#     dd0 = d0 %>%
#         # mutate(datetime = ymd_hms(paste(date, timeEST))) %>%
#         # select(site, datetime, !!v) %>%
#         select(site, date, !!v) %>%
#         # full_join(select(d, site, datetime = date, !!v), by = c('site', 'date'), suffix = c('_portal', '_file')) %>%
#         full_join(select(d, site, date, !!v), by = c('site', 'date'), suffix = c('_portal', '_file')) %>%
#         filter(if_any(starts_with(paste0(v, '_')), ~ ! is.na(.)))
#
#     dd0[v_portal < 0, v_portal] = 0
#     dd0[v_file < 0, v_file] = 0
#         # mutate(Mn_portal = ifelse(Mn_portal < 0, 0, Mn_portal)) %>%
#         # mutate(Mn_file = ifelse(Mn_file < 0, 0, Mn_file)) %>%
#         # ggplot(aes(x = datetime, y = Mn_portal, color = 'red')) +
#     dd0 %>%
#         ggplot(aes(x = date, y = !!sym(v_portal), color = 'red', size = 1.5)) +
#         geom_point() +
#         facet_wrap(.~site, scales = 'free_y') +
#         geom_point(aes(y = !!sym(v_file), size = 1), color = 'blue') +
#         labs(y = '', title = paste('variable:', v), subtitle = "red = data on portal; blue = data in Jeff's file") +
#         theme(legend.position="none")
#         # ggtitle()
#
#     ggsave(paste0('out/portal_vs_file_comparison/', v, '_by_site.png'), width = 10, height = 8)
# }
#
# # plt <- dygraphs::dygraph(xts::xts(select(d, -date),
# #                                   order.by = d$date),
# #                          main = paste0(sit_, ' (NSE ', nse, ')')) %>%
# #     dygraphs::dyAxis('x', drawGrid = FALSE) %>%
# #     dygraphs::dyAxis('y', drawGrid = FALSE, label = 'runoff (mm/d)') %>%
# #     dygraphs::dyRangeSelector()
#
# #still need to remove -999s and stuff that leaked through (notify jeff)
#

#clean up a few other things ####

# d_long = d %>%
#     select(-FieldCode, -VarCode, -Pass, -Discharge_ls) %>%
#     rename(Al_ferron = `Al-Ferron`) %>%
#     pivot_longer(-all_of(c('site', 'timeEST', 'date', 'hydroGraph', 'ANCMet')), names_to = 'var', values_to = 'val')
#
# d0_long = d0 %>%
#     pivot_longer(-all_of(c('site', 'timeEST', 'date', 'hydroGraph', 'ANCMet', 'duplicate', 'sampleType', 'canonical', 'datetime', 'uniqueID', 'gageHt', )), names_to = 'var', values_to = 'val')

# filter(d0, ! is.na(DOC)) %>% select(site, date, timeEST, DOC) %>% arrange(desc(date)) %>%
#     slice(1) %>% pull(timeEST) %>% as.character()
# filter(d, ! is.na(DOC)) %>% select(site, date, timeEST, DOC) %>% arrange(desc(date))

d = d %>%
    select(-FieldCode, -VarCode, -Pass, -Discharge_ls) %>%
    rename(Al_ferron = `Al-Ferron`)
    # filter(site != 'W101')
    # mutate(source = 'new')

# sdt = d0[, c('site', 'date', 'timeEST')]
# d0[duplicated(sdt) | duplicated(sdt, fromLast = TRUE),] %>%
#     arrange(site, date, timeEST) %>%
#     View()
# sdt = d[, c('site', 'date', 'timeEST')]
# d[duplicated(sdt) | duplicated(sdt, fromLast = TRUE),] %>%
#     arrange(site, date, timeEST) %>%
#     select(site, date, timeEST) %>%
#     mutate(a = paste(site, date, timeEST)) %>%
#     pull(a) %>%
#     table()
# # pull(Pass) %>% table()

d$duplicate = NA_character_
d$duplicate[duplicated(d[, c('site', 'date', 'timeEST')])] = 'Dup'

d0$Al_ferron = NA_real_
# d0$timeEST = as.character(d0$timeEST)
d0$DOC[d0$site == 'N' & ! is.na(d0$DOC)] = NA #just one value
# d0$source = 'orig'

# sum(substr(d$timeEST[! is.na(d$timeEST)], 1, 5) == '24:00')
# sum(substr(d0$timeEST[! is.na(d0$timeEST)], 1, 5) == '24:00')
# table(d0$canonical)
# class(d0$canonical)
d0$canonical = as.logical(d0$canonical)

#correct site N timeEST from UTC to EST in d0 ####
n_inds = d0$site == 'N'
n_times = d0$timeEST[n_inds]
n_dates = d0$date[n_inds]
n_datetimes = with_tz(ymd_hms(paste(n_dates, n_times), tz = 'UTC'), 'EST')
times_fixed = substr(n_datetimes, 12, 19)
times_fixed[times_fixed == ''] = NA
d0$timeEST[d0$site == 'N'] = times_fixed

# dout = full_join(d0, d, by = c('site', 'date', 'timeEST'), suffix = c('_orig', '_new'))
# d2 = anti_join(d, d0, by = c('site', 'date', 'timeEST'))
# filter(d0, site == 'W4', ! is.na(anionCharge))
# filter(d, site == 'W4', ! is.na(anionCharge))

# d_source = rep(NA, nrow(dout))
# d_source[! is.na(dout$source_orig)] = dout$source_orig[! is.na(dout$source_orig)]
# d_source[! is.na(dout$source_new)] = dout$source_new[! is.na(dout$source_new)]


#clean up w101 and hold it till later ####
d101 = filter(d, site == 'W101')
d = filter(d, site != 'W101')

qqq = paste(d0$site, d0$date, d0$timeEST)
d0[duplicated(qqq) | duplicated(qqq, fromLast = T), c('site', 'date', 'timeEST', 'duplicate')] %>% arrange(site, date, timeEST, duplicate) %>% print(n=100)

qqq = paste(d101$site, d101$date, d101$timeEST)
d101[duplicated(qqq) | duplicated(qqq, fromLast = T), c('site', 'date', 'timeEST', 'duplicate')] %>% arrange(site, date, timeEST, duplicate) %>% print(n=100)

#insert d records into d0 ####

d0$existing_ind = 1
for(vv in colnames(d)){

    print(vv)

    if(vv %in% c('site', 'timeEST', 'date', 'duplicate')) next

    d_sites = d %>%
        select(site, !!vv) %>%
        filter(! is.na(!!sym(vv))) %>%
        distinct(site) %>% pull()

    d0_sites = d0 %>%
        select(site, !!vv) %>%
        filter(! is.na(!!sym(vv))) %>%
        distinct(site) %>% pull()

    sites_to_incorporate = setdiff(d_sites, d0_sites)

    for(ss in sites_to_incorporate){
    # for(ss in unique(d$site)){

        print(paste('     ', ss))

        sitedatetimes_to_be_inserted = d %>%
            select(site, date, timeEST, !!vv) %>%
            filter(! is.na(!!sym(vv)), site == !!ss) %>%
            mutate(datetime = paste(date, timeEST))
            # pull(datetime)

        sitedatetimes_already_extant = d0 %>%
            select(site, date, timeEST, !!vv) %>%
            filter(site == !!ss) %>%
            mutate(datetime = paste(date, timeEST))
            # pull(datetime)

        # #to determine that site N from d is in UTC and must be converted to EST (run the two chunks above without "pull"
        # filter(d, date == as.Date('1995-09-18')) %>% select(site, date, timeEST)
        # qrq = gsub(':', '', sitedatetimes_already_extant$timeEST) %>% as.numeric()
        # qrz = gsub(':', '', d0$timeEST) %>% as.numeric()
        # plot(density(na.omit(qrq)))
        # plot(density(na.omit(qrz)))

        # try({
        # qrq = gsub(':', '', sitedatetimes_to_be_inserted$timeEST) %>% as.numeric()
        # qrz = gsub(':', '', sitedatetimes_already_extant$timeEST) %>% as.numeric()
        # dx = density(na.omit(qrq))$x
        # dy = density(na.omit(qrq))$y
        # d0x = density(na.omit(qrz))$x
        # d0y = density(na.omit(qrz))$y
        # ylim = range(c(dy, d0y))
        # xlim = range(c(dx, d0x))
        # plot(dx, dy, xlim = xlim, ylim = ylim, type = 'l', col = 'blue')
        # lines(d0x, d0y, col = 'red')
        # legend('topright', legend = c('new', 'extant'), lty = 1, col = c('blue', 'red'))
        # })
        #
        # catch = readLines(con = stdin(), 1)

        # if(! all(sitedatetimes_to_be_inserted$datetime %in% sitedatetimes_already_extant$datetime)) stop('!')
        # # if(! all(sitedatetimes_to_be_inserted %in% sitedatetimes_already_extant$timeEST)) message('!')
        # # next
        # diffs = setdiff(sitedatetimes_to_be_inserted$datetime,sitedatetimes_already_extant$datetime)
        # filter(d, timeEST %in% diffs) %>% select(site, date, timeEST, !!vv, duplicate)

        d_filt = filter(d, ! is.na(!!sym(vv)), site == !!ss) %>%
            select(site, date, timeEST, !!vv, duplicate) %>%
            mutate(new_ind = 1)

        # d0 = left_join(d0, d_filt, by=c('site', 'date', 'timeEST', 'duplicate'),
        d0 = full_join(d0, d_filt, by=c('site', 'date', 'timeEST', 'duplicate'),
                  suffix = c('', '_insert'))
        # print(nrow(d0))
        vv_insert = paste0(vv, '_insert')

        if('new_ind_insert' %in% colnames(d0)){
            insert_inds = ! is.na(d0$new_ind_insert)
            d0[insert_inds, 'new_ind'] = d0[insert_inds, 'new_ind_insert']
            d0$new_ind_insert = NULL
        }

        insert_inds = ! is.na(d0[[vv_insert]])
        d0[insert_inds, vv] = d0[insert_inds, vv_insert]
        d0[[vv_insert]] = NULL
    }
}

d0 = bind_rows(d0, d101)

#verify that new rows are legit. remove indicator cols, rebuild metadata cols for new records ####

newrows = ! is.na(d0$new_ind) & is.na(d0$existing_ind)
select(d0[newrows,], where(~any(! is.na(.))))

d0$new_ind = d0$existing_ind = NULL

to_rebuild = d0[newrows, ]
d0 = d0[! newrows, ]

repcols = setdiff(colnames(d0), colnames(d))
to_rebuild[, c('date', 'timeEST', repcols)]
d0[1:5, repcols]
to_rebuild$uniqueID = paste0(to_rebuild$site, '_',
                             gsub('[ \\-]', '', to_rebuild$date), '_',
                             substr(gsub('[ \\:]', '', to_rebuild$timeEST), 1, 4))
to_rebuild$waterYr = year(to_rebuild$date)
adj_wys = month(to_rebuild$date) < 6
to_rebuild$waterYr[adj_wys] = to_rebuild$waterYr[adj_wys] - 1
to_rebuild$datetime = format(ymd_hms(paste(to_rebuild$date, to_rebuild$timeEST)),
                             '%m/%d/%y %H:%M')
select(to_rebuild, where(~any(! is.na(.))))

# dupdts = paste(to_rebuild$site, to_rebuild$date, to_rebuild$timeEST)
# arrange(d0[paste(d0$site, d0$date, d0$timeEST) %in% dupdts, c('site', 'date', 'timeEST', 'duplicate')], site, date, timeEST)
# arrange(to_rebuild[, c('site', 'date', 'timeEST', 'duplicate')], site, date, timeEST)

d0 = bind_rows(d0, to_rebuild)

write_csv(filter(tail(d0, n=6), is.na(duplicate)), 'unmatched_rows.csv')
# filter(d0, site == 'W1', date == as.Date('2006-10-02'))
# filter(d0, site == 'W1', date == as.Date('2008-11-24'))
# d0 = d0[1:(nrow(d0) - 20),]

qqq = left_join(to_rebuild[, c('site', 'date', 'timeEST', 'duplicate')],
          d0[, c('site', 'date', 'timeEST', 'duplicate')],
          by = c('site', 'date'))
print(qqq, n=100)

# ANCILLARY plot again ####

# vv = colnames(d)
# vv = vv[! vv %in% c('date', 'timeEST', 'site', 'Discharge_ls', 'Pass', 'FieldCode', 'VarCode', 'hydroGraph', 'Al-Ferron', 'ANCMet', 'PrecipCatch', 'duplicate')]
# for(v in vv){
#
#     print(v)
#
#     v_portal = paste0(v, '_portal')
#     v_file = paste0(v, '_file')
#
#     dd0 = d0 %>%
#         # mutate(datetime = ymd_hms(paste(date, timeEST))) %>%
#         # select(site, datetime, !!v) %>%
#         select(site, date, !!v) %>%
#         # full_join(select(d, site, datetime = date, !!v), by = c('site', 'date'), suffix = c('_portal', '_file')) %>%
#         full_join(select(d, site, date, !!v), by = c('site', 'date'), suffix = c('_portal', '_file')) %>%
#         filter(if_any(starts_with(paste0(v, '_')), ~ ! is.na(.)))
#
#     dd0[v_portal < 0, v_portal] = 0
#     dd0[v_file < 0, v_file] = 0
#     # mutate(Mn_portal = ifelse(Mn_portal < 0, 0, Mn_portal)) %>%
#     # mutate(Mn_file = ifelse(Mn_file < 0, 0, Mn_file)) %>%
#     # ggplot(aes(x = datetime, y = Mn_portal, color = 'red')) +
#     out = dd0 %>%
#         ggplot(aes(x = date, y = !!sym(v_portal), color = 'red', size = 1.5)) +
#         geom_point() +
#         facet_wrap(.~site, scales = 'free_y') +
#         geom_point(aes(y = !!sym(v_file), size = 1), color = 'blue') +
#         labs(y = '', title = paste('variable:', v), subtitle = "red = data on portal; blue = data in Jeff's file") +
#         theme(legend.position="none")
#     # ggtitle()
#
#     print(out)
#     readLines(con = stdin(), 1)
#     # ggsave(paste0('out/portal_vs_file_comparison/', v, '_by_site.png'), width = 10, height = 8)
# }

# Al_ICP still needs work (nvm. just gonna remove it from d0 and use what's in d. finished above) ####
#
# qq = select(d0, site, date, timeEST, Al_ICP, TMAl, OMAl, Al_ferron) %>%
#     filter(if_any(all_of(c('Al_ICP', 'TMAl', 'OMAl', 'Al_ferron')), ~(! is.na(.))))
#
# # select(d0, site, date, timeEST, Al_ICP, TMAl, OMAl, Al_ferron) %>%
# #     filter(if_any(all_of(c('Al_ICP', 'TMAl', 'OMAl', 'Al_ferron')), ~(! is.na(.))))
#
# qq$alna = apply(qq[, 4:7], 1, function(x) sum(is.na(x)))
#
# filter(qq, alna == 1)
#
# for(ss in unique(d0$site)){
#     ddd = filter(d0, site == !!ss)
#     ddd$timeEST[is.na(ddd$timeEST)] = '12:00:00'
#     ddd$datetime = ymd_hms(paste(ddd$date, ddd$timeEST))
#     ddd = arrange(ddd, datetime)
#     plot(ddd$datetime, ddd$Al_ICP, type = 'l', col = 'blue')
#     lines(ddd$datetime, ddd$TMAl, col = 'red')
#     lines(ddd$datetime, ddd$OMAl, col = 'black')
#     lines(ddd$datetime, ddd$Al_ferron, col = 'green')
#     readLines(con = stdin(), 1)
# }
#
# gfg = apply(d[, c('TMAl', 'OMAl', 'Al_ICP')], 1, function(x) sum(is.na(x)))
# rtr = apply(d[, c('Al_ferron')], 1, function(x) sum(is.na(x)))
# table(gfg)
# table(rtr)
# sum(rtr == 0 & gfg != 0)

# other ####

d0 = arrange(d0, site, date, timeEST)
d0$theoryCond[! is.na(d0$theoryCond) & d0$theoryCond == -99.9] = NA
d0$theoryCond[! is.na(d0$theoryCond) & d0$theoryCond > 1500] = NA
d0$Al_ferron[! is.na(d0$Al_ferron) & d0$Al_ferron < 0] = 0
d0$NH4[! is.na(d0$NH4) & d0$NH4 == -999.99] = NA
d0$PO4[! is.na(d0$PO4) & d0$PO4 == -999.99] = NA
# plot(density(na.omit(d0$cationCharge)))
# range(d0$cationCharge, na.rm=T)
d0$cationCharge[! is.na(d0$cationCharge) & d0$cationCharge == 1000000000] = NA
d0$cationCharge[! is.na(d0$cationCharge) & d0$cationCharge == 999000000] = NA
d0$cationCharge[! is.na(d0$cationCharge) & d0$cationCharge < 0] = 0
# plot(density(na.omit(d0$anionCharge)))
# range(d0$anionCharge, na.rm=T)
d0$anionCharge = -abs(d0$anionCharge)

qqq = select(d0, where(is.numeric))
apply(qqq, 2, function(x) min(na.omit(x)))

# for post comparison_plot.Rmd ####
save.image('for_comparison_plot_after.rda')
