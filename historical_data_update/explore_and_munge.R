library(tidyverse)
library(lubridate)
library(ggplot2)

## REMOVE THE DOC AND DON VALUES WITH ALL 0S PRIOR TO 1992, OR JUST REPLACE D0 DOC AND DON WITH D DOC AND DON

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

#okay, so the EST column is bollocks
d = d %>%
    mutate(date = ymd(d$SampleDate),
           timeEST = substr(d$SampleTime, 12, 19)) %>%
    select(-EST, -SampleDate, -SampleTime)

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

# ANCILLARY earliest dates by variable ####

earliest_date_comparisons = tibble()
for(i in seq_along(var_map)){
    v = unlist(var_map[i], use.names = FALSE)
    tryCatch({
        d_earliest = filter(d, ! is.na(!!sym(v))) %>% summarize(mindate = min(date)) %>% pull() %>% as.Date()
        d0_earliest = filter(d0, ! is.na(!!sym(v))) %>% summarize(mindate = min(date)) %>% pull()
        earliest_date_comparisons = bind_rows(earliest_date_comparisons,
                  tibble(variable = v, first_record_portal = d0_earliest,
                         first_record_file = d_earliest,
                         earlier = case_when(d_earliest < d0_earliest ~ 'file',
                                             d_earliest == d0_earliest ~ '-',
                                             TRUE ~ 'portal')))
    }, warning = function(w) print('a'))
}

select(d0, site, date, !!v) %>% filter(! is.na(!!sym(v))) %>% arrange(date)
select(d0, site, date, !!v) %>% filter(! is.na(!!sym(v))) %>% arrange(date) %>% View()
select(d, Site, date, !!v) %>% filter(! is.na(!!sym(v))) %>% arrange(date)
select(d, Site, date, !!v) %>% filter(! is.na(!!sym(v))) %>% arrange(date) %>% View()

write_csv(earliest_date_comparisons, 'earliest_date_comparisons.csv')

# ANCILLARY last dates by variable ####

last_date_comparisons = tibble()
for(i in seq_along(var_map)){
    v = unlist(var_map[i], use.names = FALSE)
    tryCatch({
        d_earliest = filter(d, ! is.na(!!sym(v))) %>% summarize(mindate = max(date)) %>% pull() %>% as.Date()
        d0_earliest = filter(d0, ! is.na(!!sym(v))) %>% summarize(mindate = max(date)) %>% pull()
        last_date_comparisons = bind_rows(last_date_comparisons,
                                          tibble(variable = v, first_record_portal = d0_earliest,
                                                 first_record_file = d_earliest,
                                                 later = case_when(d_earliest < d0_earliest ~ 'portal',
                                                                   d_earliest == d0_earliest ~ '-',
                                                                   TRUE ~ 'file')))
    }, warning = function(w) print('a'))
}

#correct dates in future ####

err_date_inds = d0$date > Sys.Date()
err_dates = d0$date[err_date_inds]
fixed_dates = as.Date(sub('^20([0-9]{2})', '19\\1', as.character(err_dates)))
d0$date[err_date_inds] = fixed_dates

# ANCILLARY plot the variables that appear to go farther back in the new file ####
# d = rename(d, site = Site)
vv = colnames(d)
vv = vv[! vv %in% c('date', 'timeEST', 'site', 'Discharge_ls', 'Pass', 'FieldCode', 'VarCode', 'hydroGraph', 'Al-Ferron', 'ANCMet', 'PrecipCatch')]
for(v in vv){

    v_portal = paste0(v, '_portal')
    v_file = paste0(v, '_file')

    dd0 = d0 %>%
        # mutate(datetime = ymd_hms(paste(date, timeEST))) %>%
        # select(site, datetime, !!v) %>%
        select(site, date, !!v) %>%
        # full_join(select(d, site, datetime = date, !!v), by = c('site', 'date'), suffix = c('_portal', '_file')) %>%
        full_join(select(d, site, date, !!v), by = c('site', 'date'), suffix = c('_portal', '_file')) %>%
        filter(if_any(starts_with(paste0(v, '_')), ~ ! is.na(.)))

    dd0[v_portal < 0, v_portal] = 0
    dd0[v_file < 0, v_file] = 0
        # mutate(Mn_portal = ifelse(Mn_portal < 0, 0, Mn_portal)) %>%
        # mutate(Mn_file = ifelse(Mn_file < 0, 0, Mn_file)) %>%
        # ggplot(aes(x = datetime, y = Mn_portal, color = 'red')) +
    dd0 %>%
        ggplot(aes(x = date, y = !!sym(v_portal), color = 'red', size = 1.5)) +
        geom_point() +
        facet_wrap(.~site, scales = 'free_y') +
        geom_point(aes(y = !!sym(v_file), size = 1), color = 'blue') +
        labs(y = '', title = paste('variable:', v), subtitle = "red = data on portal; blue = data in Jeff's file") +
        theme(legend.position="none")
        # ggtitle()

    ggsave(paste0('out/portal_vs_file_comparison/', v, '_by_site.png'), width = 10, height = 8)
}

# plt <- dygraphs::dygraph(xts::xts(select(d, -date),
#                                   order.by = d$date),
#                          main = paste0(sit_, ' (NSE ', nse, ')')) %>%
#     dygraphs::dyAxis('x', drawGrid = FALSE) %>%
#     dygraphs::dyAxis('y', drawGrid = FALSE, label = 'runoff (mm/d)') %>%
#     dygraphs::dyRangeSelector()

#still need to remove -999s and stuff that leaked through (notify jeff)


#combine old and new datasets ####

# d_long = d %>%
#     select(-FieldCode, -VarCode, -Pass, -Discharge_ls) %>%
#     rename(Al_ferron = `Al-Ferron`) %>%
#     pivot_longer(-all_of(c('site', 'timeEST', 'date', 'hydroGraph', 'ANCMet')), names_to = 'var', values_to = 'val')
#
# d0_long = d0 %>%
#     pivot_longer(-all_of(c('site', 'timeEST', 'date', 'hydroGraph', 'ANCMet', 'duplicate', 'sampleType', 'canonical', 'datetime', 'uniqueID', 'gageHt', )), names_to = 'var', values_to = 'val')

d = d %>%
    select(-FieldCode, -VarCode, -Pass, -Discharge_ls) %>%
    rename(Al_ferron = `Al-Ferron`) %>%
    filter(site != 'W101')
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
d0$timeEST = as.character(d0$timeEST)
# d0$source = 'orig'

# dout = full_join(d0, d, by = c('site', 'date', 'timeEST'), suffix = c('_orig', '_new'))
# d2 = anti_join(d, d0, by = c('site', 'date', 'timeEST'))
# filter(d0, site == 'W4', ! is.na(anionCharge))
# filter(d, site == 'W4', ! is.na(anionCharge))

# d_source = rep(NA, nrow(dout))
# d_source[! is.na(dout$source_orig)] = dout$source_orig[! is.na(dout$source_orig)]
# d_source[! is.na(dout$source_new)] = dout$source_new[! is.na(dout$source_new)]

for(vv in colnames(d)){

    if(vv %in% c('site', 'timeEST', 'date')) next

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

        sitedatetimes_to_be_inserted = d %>%
            select(site, date, timeEST, !!vv) %>%
            filter(! is.na(!!sym(vv)), site == !!ss) %>%
            mutate(datetime = paste(date, timeEST)) %>%
            pull(datetime)

        sitedatetimes_already_extant = d0 %>%
            select(site, date, timeEST, !!vv) %>%
            filter(site == !!ss) %>%
            mutate(datetime = paste(date, timeEST)) %>%
            pull(datetime)

        if(! all(sitedatetimes_to_be_inserted %in% sitedatetimes_already_extant)) stop('!')

        d_filt = filter(d, site == !!ss) %>%
            select(site, date, timeEST, !!vv, duplicate)

        d0 = left_join(d0, d_filt, by=c('site', 'date', 'timeEST', 'duplicate'),
                  suffix = c('', '_insert'))
        vv_insert = paste0(vv, '_insert')

        insert_inds = ! is.na(d0[[vv_insert]])
        d0[insert_inds, vv] = d0[insert_inds, vv_insert]
        d0[[vv_insert]] = NULL
    }
}

# ANCILLARY plot again ####

vv = colnames(d)
vv = vv[! vv %in% c('date', 'timeEST', 'site', 'Discharge_ls', 'Pass', 'FieldCode', 'VarCode', 'hydroGraph', 'Al-Ferron', 'ANCMet', 'PrecipCatch')]
for(v in vv){

    v_portal = paste0(v, '_portal')
    v_file = paste0(v, '_file')

    dd0 = d0 %>%
        # mutate(datetime = ymd_hms(paste(date, timeEST))) %>%
        # select(site, datetime, !!v) %>%
        select(site, date, !!v) %>%
        # full_join(select(d, site, datetime = date, !!v), by = c('site', 'date'), suffix = c('_portal', '_file')) %>%
        full_join(select(d, site, date, !!v), by = c('site', 'date'), suffix = c('_portal', '_file')) %>%
        filter(if_any(starts_with(paste0(v, '_')), ~ ! is.na(.)))

    dd0[v_portal < 0, v_portal] = 0
    dd0[v_file < 0, v_file] = 0
    # mutate(Mn_portal = ifelse(Mn_portal < 0, 0, Mn_portal)) %>%
    # mutate(Mn_file = ifelse(Mn_file < 0, 0, Mn_file)) %>%
    # ggplot(aes(x = datetime, y = Mn_portal, color = 'red')) +
    dd0 %>%
        ggplot(aes(x = date, y = !!sym(v_portal), color = 'red', size = 1.5)) +
        geom_point() +
        facet_wrap(.~site, scales = 'free_y') +
        geom_point(aes(y = !!sym(v_file), size = 1), color = 'blue') +
        labs(y = '', title = paste('variable:', v), subtitle = "red = data on portal; blue = data in Jeff's file") +
        theme(legend.position="none")
    # ggtitle()

    ggsave(paste0('out/portal_vs_file_comparison/', v, '_by_site.png'), width = 10, height = 8)
}

#MAKE SURE DUP COL IS STILL CHILL
