#for uploading hbef stuff to edi (also see .../hbef/edi_upload_steps.txt)

library(tidyverse)

# gsub(',', "', '",
#     "refNo,site,date,timeEST,pH,pHmetrohm,DIC,spCond,temp,ANC960,ANCMet,gageHt,hydroGraph,flowGageHt,precipCatch,fieldCode,notes,archived,uniqueID,waterYr,datetime,Ca,Mg,K,Na,TMAl,OMAl,Al_ICP,NH4,SO4,NO3,Cl,PO4,DOC,TDN,DON,SiO2,Mn,Fe,F,cationCharge,anionCharge,theoryCond,ionError,duplicate,sampleType,ionBalance")
#
# gsub(',', "', '",
#     "refNo,site,date,timeEST,pH,DIC,spCond,temp,ANC960,ANCMet,gageHt,hydroGraph,flowGageHt,precipCatch,fieldCode,notes,uniqueID,waterYr,datetime,Ca,Mg,K,Na,TMAl,OMAl,Al_ICP,NH4,SO4,NO3,Cl,PO4,DOC,TDN,DON,SiO2,Mn,Fe,F,cationCharge,anionCharge,theoryCond,ionError,duplicate,sampleType,ionBalance,canonical")

# precip ####

args = commandArgs(trailingOnly=TRUE)[1]

pc = read.csv(file.path(args[1], 'HubbardBrook_weekly_precipitation_chemistry_curr.csv'),
              stringsAsFactors = FALSE,
              colClasses = 'character') %>%
    as_tibble() %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date('2013-06-01')) %>%
    mutate(date = as.character(date))

ph = read.csv(file.path(args[1], 'HubbardBrook_weekly_precipitation_chemistry_hist.csv'),
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
           K, Na, TMAl, OMAl, Al_ICP, NH4, SO4, NO3, Cl, PO4, DOC, TDN, DON,
           SiO2, Mn, Fe, `F`, cationCharge, anionCharge, ionError,
           duplicate, sampleType, ionBalance, canonical, pHmetrohm)

# stream ####

sc = read.csv(file.path(args[1], 'HubbardBrook_weekly_stream_chemistry_curr.csv'),
              stringsAsFactors = FALSE,
              colClasses = 'character') %>%
    as_tibble() %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date('2013-06-01')) %>%
    mutate(date = as.character(date))

sh = read.csv(file.path(args[1], 'HubbardBrook_weekly_stream_chemistry_hist.csv'),
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
           datetime, Ca, Mg, K, Na, TMAl, OMAl, Al_ICP, NH4, SO4, NO3, Cl, PO4,
           DOC, TDN, DON, SiO2, Mn, Fe, `F`, cationCharge, anionCharge,
           ionError, duplicate, sampleType, ionBalance, canonical,
           pHmetrohm)
# gsub('\\t', ', ', strng)

# out ####

p$datetime = NULL
s$datetime = NULL
write_csv(p, file.path(args[1], 'HubbardBrook_weekly_precipitation_chemistry.csv'))
write_csv(s, file.path(args[1], 'HubbardBrook_weekly_stream_chemistry.csv'))

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
