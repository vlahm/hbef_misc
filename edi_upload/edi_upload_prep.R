#for uploading hbef stuff to edi (also see .../hbef/edi_upload_preR_steps.txt)

library(tidyverse)

# gsub(',', "', '",
#     "refNo,site,date,timeEST,pH,pHmetrohm,DIC,spCond,temp,ANC960,ANCMet,gageHt,hydroGraph,flowGageHt,precipCatch,fieldCode,notes,archived,uniqueID,waterYr,datetime,Ca,Mg,K,Na,TMAl,OMAl,Al_ICP,NH4,SO4,NO3,Cl,PO4,DOC,TDN,DON,SiO2,Mn,Fe,F,cationCharge,anionCharge,theoryCond,ionError,duplicate,sampleType,ionBalance")
#
# gsub(',', "', '",
#     "refNo,site,date,timeEST,pH,DIC,spCond,temp,ANC960,ANCMet,gageHt,hydroGraph,flowGageHt,precipCatch,fieldCode,notes,uniqueID,waterYr,datetime,Ca,Mg,K,Na,TMAl,OMAl,Al_ICP,NH4,SO4,NO3,Cl,PO4,DOC,TDN,DON,SiO2,Mn,Fe,F,cationCharge,anionCharge,theoryCond,ionError,duplicate,sampleType,ionBalance,canonical")

get_unambiguous_barcodes <- function(zz, dd, allow_differing_times = TRUE){

    #zz: an archive sample dataset, either for precip or stream samples
    #dd: a field sample dataset, either for precip or stream samples

    if(allow_differing_times){
        cols <- c('site', 'date')
        zz$timeEST = NULL
        dd$timeEST = NULL
    } else {
        cols <- c('site', 'date', 'timeEST')
        zz$timeEST = substr(zz$timeEST, 1, 5)
    }

    zzsub = select(zz, all_of(cols))
    zz_same_datetime = zz[duplicated(zzsub) | duplicated(zzsub, fromLast = TRUE), ] %>%
        arrange(site, date)

    # zzsub = select(zz, site, date)
    # same_date = zz[duplicated(zzsub) | duplicated(zzsub, fromLast = TRUE), ]
    # same_date = anti_join(same_date, same_datetime) %>%
    #     arrange(site, date, timeEST)

    ddsub = select(dd, all_of(cols))
    dd_same_datetime = dd[duplicated(ddsub) | duplicated(ddsub, fromLast = TRUE), ] %>%
        arrange(site, date)

    zz_unambiguous = left_join(zz, ddsub, by = cols,
                               relationship = 'many-to-many') %>%
        anti_join(zz_same_datetime, by = cols) %>%
        anti_join(dd_same_datetime, by = cols)

    return(zz_unambiguous)
}

args = commandArgs(trailingOnly=TRUE)[1]
# args = '/home/mike/git/hbef/hbef_misc/edi_upload'
wd = args[1]

#stop('have we decided how to deal with duplicate archive and/or field records?')
arch = read_csv(file.path(wd, 'archive_samples.csv'),
                col_types = cols(.default = 'c')) %>%
    select(site, site_type, date, timeEST, barcode)

arch_p = arch %>%
    filter(site_type == 'precip gauge') %>%
    select(-site_type)
arch_s = arch %>%
    filter(site_type == 'stream') %>%
    select(-site_type)

# precip ####

pc = read.csv(file.path(wd, 'HubbardBrook_weekly_precipitation_chemistry_curr.csv'),
              stringsAsFactors = FALSE,
              colClasses = 'character') %>%
    as_tibble() %>%
    select(-starts_with('chla_')) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date('2013-06-01')) %>%
    mutate(date = as.character(date))

ph = read.csv(file.path(wd, 'HubbardBrook_weekly_precipitation_chemistry_hist.csv'),
              stringsAsFactors = FALSE,
              colClasses = 'character') %>%
    as_tibble() %>%
    mutate(date = as.Date(date)) %>%
    filter(! (site %in% c('N', 'S') & date >= as.Date('1969-06-01')) ) %>%
    mutate(date = as.character(date)) %>%
    anti_join(pc, by='date')

pc[pc == '\\N'] = NA
ph[ph == '\\N'] = NA

p = bind_rows(pc, ph) %>%
    arrange(date, site) %>%
    mutate_at(vars(-site, -date, -timeEST, -fieldCode, -notes,
                   -uniqueID, -datetime, -duplicate, -sampleType, -canonical,
                   -archived),
              as.numeric) %>%
    mutate(
        datetime = ifelse(is.na(timeEST),
                          NA,
                          format(as.POSIXct(paste(date, timeEST)),
                                 '%Y-%m-%d %H:%M')),
        timeEST = substr(timeEST, 1, 5)) %>%
    select(site, date, timeEST, pH, DIC, spCond, temp, ANC960, ANCMet,
           precipCatch, fieldCode, notes, uniqueID, waterYr, datetime, Ca, Mg,
           K, Na, TMAl, OMAl, Al_ICP, Al_ferron, NH4, SO4, NO3, Cl, PO4, DOC, TDN, DON,
           SiO2, Mn, Fe, `F`, cationCharge, anionCharge, ionError,
           duplicate, sampleType, ionBalance, canonical, pHmetrohm) %>%
    mutate(across(everything(), as.character))

barcodes_p = get_unambiguous_barcodes(arch_p, p, allow_differing_times = FALSE)

p = p %>%
    # left_join(barcodes_p, by = c('site', 'date')) %>%
    left_join(barcodes_p, by = c('site', 'date', 'timeEST')) %>%
    select(site, date, timeEST, barcode, everything())

# psub = select(p, site, date, timeEST)
# pdupes = duplicated(psub) | duplicated(psub, fromLast = TRUE)
# p = p[! pdupes, ]

# stream ####

sc = read.csv(file.path(wd, 'HubbardBrook_weekly_stream_chemistry_curr.csv'),
              stringsAsFactors = FALSE,
              colClasses = 'character') %>%
    as_tibble() %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date('2013-06-01')) %>%
    mutate(date = as.character(date))

sh = read.csv(file.path(wd, 'HubbardBrook_weekly_stream_chemistry_hist.csv'),
              stringsAsFactors = FALSE,
              colClasses = 'character') %>%
    as_tibble() %>%
    anti_join(sc, by='date')

sc[sc == '\\N'] = NA
sh[sh == '\\N'] = NA

s = bind_rows(sc, sh) %>%
    arrange(date, site) %>%
    mutate_at(vars(-site, -date, -timeEST, -hydroGraph, -fieldCode, -notes,
                   -uniqueID, -datetime, -duplicate, -sampleType, -canonical,
                   -archived),
              as.numeric) %>%
    mutate(
        datetime = ifelse(is.na(timeEST),
                             NA,
                             format(as.POSIXct(paste(date, timeEST)),
                                    '%Y-%m-%d %H:%M')),
        timeEST = substr(timeEST, 1, 5)) %>%
    select(site, date, timeEST, pH, DIC, spCond, temp, ANC960, ANCMet, gageHt,
           hydroGraph, flowGageHt, fieldCode, notes, uniqueID, waterYr,
           datetime, Ca, Mg, K, Na, TMAl, OMAl, Al_ICP, Al_ferron, NH4, SO4, NO3, Cl, PO4,
           DOC, TDN, DON, SiO2, Mn, Fe, `F`, cationCharge, anionCharge,
           ionError, duplicate, sampleType, ionBalance, canonical,
           pHmetrohm) %>%
    mutate(across(everything(), as.character))

barcodes_s = get_unambiguous_barcodes(arch_s, s, allow_differing_times = FALSE)

s = s %>%
    left_join(barcodes_s, by = c('site', 'date', 'timeEST')) %>%
    select(site, date, timeEST, barcode, everything())

# ssub = select(s, site, date, timeEST)
# sdupes = duplicated(ssub) | duplicated(ssub, fromLast = TRUE)
# s = s[! sdupes, ]


# gsub('\\t', ', ', strng)

# out ####

p$datetime = NULL
s$datetime = NULL
write_csv(p, file.path(wd, 'HubbardBrook_weekly_precipitation_chemistry.csv'))
write_csv(s, file.path(wd, 'HubbardBrook_weekly_stream_chemistry.csv'))

# zz = read.csv('/tmp/mozilla_mike0/HubbardBrook_weekly_precipitation_chemistry.csv',
#               stringsAsFactors = FALSE)
# table(nchar(zz$datetime))
# zz2 = read.csv('/tmp/mozilla_mike0/HubbardBrook_weekly_stream_chemistry.csv',
#               stringsAsFactors = FALSE)
# table(nchar(zz2$datetime))


# inspect date, time, datetime discrepancies (one time only) ####

# zz = p[which(apply(p[, c('date', 'timeEST', 'datetime')], 1,
#                    function(x) sum(is.na(x)) == 2)), c('date', 'timeEST', 'datetime')]
# as.data.frame(zz)
# zzz = s[which(apply(s[, c('date', 'timeEST', 'datetime')], 1,
#                     function(x) sum(is.na(x)) == 2)), c('date', 'timeEST', 'datetime')]
# as.data.frame(zzz)
#
# table(nchar(p$datetime))
# table(nchar(s$datetime))
#
# nrow(p)
# sum(is.na(p$datetime))
# sum(is.na(p$date))
# sum(is.na(p$timeEST))
# nrow(s)
# sum(is.na(s$datetime))
# sum(is.na(s$date))
# sum(is.na(s$timeEST))
# identical(which(is.na(p$timeEST)) , which(is.na(p$datetime)))
# identical(which(is.na(s$timeEST)) , which(is.na(s$datetime)))
# p[is.na(p$timeEST),]$date
# s[is.na(s$timeEST),]$date
#
# qq = bind_rows(p, s)
# nrow(qq)
# sum(is.na(qq$datetime))
# sum(is.na(qq$date))
# sum(is.na(qq$timeEST))
