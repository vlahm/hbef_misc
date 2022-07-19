library(tidyverse)

setwd('~/git/hbef/hbef_misc/historical_data_update/')
d = readxl::read_xlsx('20220713HBEFChem.xlsx', sheet = 2, na = 'NA', guess_max = 25000)
d0 = read_csv('hbef_historical.csv', na = '\\N', col_names = FALSE, guess_max = 25000)
hist_names = c('refNo','site','date','timeEST','pH','DIC','spCond','temp','ANC960','ANCMet','gageHt','hydroGraph','flowGageHt','precipCatch','fieldCode','notes','uniqueID','waterYr','datetime','Ca','Mg','K','Na','TMAl','OMAl','Al_ICP','NH4','SO4','NO3','Cl','PO4','DOC','TDN','DON','SiO2','Mn','Fe','F','cationCharge','anionCharge','theoryCond','ionError','duplicate','sampleType','ionBalance','canonical')
colnames(d0) = hist_names

#site names
known_sites = c('HBK', 'N', 'RG1', 'RG11', 'RG19', 'RG22', 'RG23', 'S', 'W1', 'W2', 'W3', 'W4', 'W5', 'W6', 'W7', 'W7-Precip', 'W8', 'W9')
d_sites = unique(d$Site)

setdiff(known_sites, d_sites)
setdiff(d_sites, known_sites)
paste(d_sites, collapse = ', ')

#time stuff
t1 = gsub(':', '', substr(d$SampleTime, 12, 19))
t2 = as.character(d$EST)
gg = tibble(t1, t2) %>%
    dplyr::filter(if_any(everything(), ~! is.na(.)))
sum(is.na(gg$t1))
sum(is.na(gg$t2))
table(substr(t1, 5, 6))

t1 = as.numeric(substr(t1, 1, 4))
t2 = as.numeric(t2)

gg2 = tibble(t1, t2) %>%
    filter(if_all(everything(), ~! is.na(.)))
identical(gg2$t1, gg2$t2)

mutate(gg2, diff = t1 - t2) %>% filter(diff != 0)
mutate(gg2, diff = t1 - t2) %>% filter(diff != 0) %>% pull(t2) %>% table()
mutate(gg2, diff = t1 - t2) %>% filter(diff != 0, ! grepl('\\.', as.character(t2)))

filter(d, round(as.numeric(as.character(t2)), 3) == 0.333)

#column names
d_names = colnames(d)
setdiff(hist_names, d_names)
setdiff(d_names, hist_names)
setdiff(tolower(hist_names), tolower(d_names))
setdiff(tolower(d_names), tolower(hist_names))
intersect(tolower(d_names), tolower(hist_names))
intersect(d_names, hist_names)

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
            SampleDate = 'date',
            SampleTime = 'timeest',
            Temp_C = 'temp')

inds = base::match(names(var_map), colnames(d))
colnames(d)[inds] = unname(var_map)

var_map2 = intersect(d_names, hist_names)
names(var_map2) = var_map2
var_map = append(var_map, as.list(var_map2))

setdiff(tolower(colnames(d0)), tolower(colnames(d)))
setdiff(tolower(colnames(d)), tolower(colnames(d0)))


#earliest dates by variable

earliest_date_comparisons = tibble()
for(i in seq_along(var_map)){
    v = unlist(var_map[i], use.names = FALSE)
    tryCatch({
        d_earliest = filter(d, ! is.na(!!sym(v))) %>% summarize(mindate = min(SampleDate)) %>% pull() %>% as.Date()
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
select(d, Site, SampleDate, !!v) %>% filter(! is.na(!!sym(v))) %>% arrange(SampleDate)
select(d, Site, SampleDate, !!v) %>% filter(! is.na(!!sym(v))) %>% arrange(SampleDate) %>% View()

write_csv(earliest_date_comparisons, 'earliest_date_comparisons.csv')

## REMOVE THE DOC AND DON VALUES WITH ALL 0S PRIOR TO 1992, OR JUST REPLACE D0 DOC AND DON WITH D DOC AND DON

#last dates by variable

last_date_comparisons = tibble()
for(i in seq_along(var_map)){
    v = unlist(var_map[i], use.names = FALSE)
    tryCatch({
        d_earliest = filter(d, ! is.na(!!sym(v))) %>% summarize(mindate = max(SampleDate)) %>% pull() %>% as.Date()
        d0_earliest = filter(d0, ! is.na(!!sym(v))) %>% summarize(mindate = max(date)) %>% pull()
        last_date_comparisons = bind_rows(last_date_comparisons,
                                              tibble(variable = v, first_record_portal = d0_earliest,
                                                     first_record_file = d_earliest,
                                                     later = case_when(d_earliest < d0_earliest ~ 'portal',
                                                                         d_earliest == d0_earliest ~ '-',
                                                                         TRUE ~ 'file')))
    }, warning = function(w) print('a'))
}

#correct dates in future

err_date_inds = d0$date > Sys.Date()
err_dates = d0$date[err_date_inds]
fixed_dates = as.Date(sub('^20([0-9]{2})', '19\\1', as.character(err_dates)))
d0$date[err_date_inds] = fixed_dates

#conform new file to existing db format

d0
d
