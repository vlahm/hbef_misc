#for uploading hbef stuff to edi (also see Dropbox/hbef/edi_upload_steps.txt)

library(tidyverse)

# gsub(',', "', '",
#     "refNo,site,date,timeEST,pH,pHmetrohm,DIC,spCond,temp,ANC960,ANCMet,gageHt,hydroGraph,flowGageHt,precipCatch,fieldCode,notes,archived,uniqueID,waterYr,datetime,Ca,Mg,K,Na,TMAl,OMAl,Al_ICP,NH4,SO4,NO3,Cl,PO4,DOC,TDN,DON,SiO2,Mn,Fe,F,cationCharge,anionCharge,theoryCond,ionError,duplicate,sampleType,ionBalance")
#
# gsub(',', "', '",
#     "refNo,site,date,timeEST,pH,DIC,spCond,temp,ANC960,ANCMet,gageHt,hydroGraph,flowGageHt,precipCatch,fieldCode,notes,uniqueID,waterYr,datetime,Ca,Mg,K,Na,TMAl,OMAl,Al_ICP,NH4,SO4,NO3,Cl,PO4,DOC,TDN,DON,SiO2,Mn,Fe,F,cationCharge,anionCharge,theoryCond,ionError,duplicate,sampleType,ionBalance,canonical")

# precip ####

pc = read.csv('~/HubbardBrook_weekly_precipitation_chemistry_curr.csv',
              stringsAsFactors = FALSE,
              colClasses = 'character') %>%
    as_tibble() %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date('2013-06-01')) %>%
    mutate(date = as.character(date))

ph = read.csv('~/HubbardBrook_weekly_precipitation_chemistry_hist.csv',
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
           SiO2, Mn, Fe, `F`, cationCharge, anionCharge, theoryCond, ionError,
           duplicate, sampleType, ionBalance, canonical, pHmetrohm, archived)

#stream ####

sc = read.csv('~/HubbardBrook_weekly_stream_chemistry_curr.csv',
              stringsAsFactors = FALSE,
              colClasses = 'character') %>%
    as_tibble() %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date('2013-06-01')) %>%
    mutate(date = as.character(date))

sh = read.csv('~/HubbardBrook_weekly_stream_chemistry_hist.csv',
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
           theoryCond, ionError, duplicate, sampleType, ionBalance, canonical,
           pHmetrohm, archived)
# gsub('\\t', ', ', strng)

# out ####

write_csv(p, '~/git/hbef/hbef_misc/edi_upload/HubbardBrook_weekly_precipitation_chemistry.csv')
write_csv(s, '~/git/hbef/hbef_misc/edi_upload/HubbardBrook_weekly_stream_chemistry.csv')
# zz = read.csv('/tmp/mozilla_mike0/HubbardBrook_weekly_precipitation_chemistry.csv',
#               stringsAsFactors = FALSE)
# table(nchar(zz$datetime))
# zz2 = read.csv('/tmp/mozilla_mike0/HubbardBrook_weekly_stream_chemistry.csv',
#               stringsAsFactors = FALSE)
# table(nchar(zz2$datetime))
