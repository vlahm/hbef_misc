library(tidyverse)
# s = read_csv('/home/mike/git/hbef/hbef_misc/edi_upload/HubbardBrook_weekly_stream_chemistry.csv', col_types=cols(.default = 'c'))
# p = read_csv('/home/mike/git/hbef/hbef_misc/edi_upload/HubbardBrook_weekly_precipitation_chemistry.csv', col_types=cols(.default = 'c'))
#
# xxx=select(p, site, date, timeEST)
# qqq = xxx[duplicated(xxx) | duplicated(xxx, fromLast = T), ]

#get all archive dupes
zz = read_csv('~/git/hbef/hbef_misc/edi_upload/archive_samples.csv')

zzsub = select(zz, site, site_type, date, timeEST)
zz_same_datetime = zz[duplicated(zzsub) | duplicated(zzsub, fromLast = TRUE), ] %>%
    arrange(site_type, site, date, timeEST, notes)
zzsub = select(zz, site, site_type, date)
zz_same_date = zz[duplicated(zzsub) | duplicated(zzsub, fromLast = TRUE), ]
zz_same_date = anti_join(same_date, same_datetime) %>% #! (watch out if running *** below)
    arrange(site_type, site, date, timeEST, notes)     #!
write_csv(zz_same_datetime, '/tmp/archive_duplicates_same_datetime.csv')
write_csv(zz_same_date, '/tmp/archive_duplicates_same_date_different_time.csv')

#get all precip/stream dupes
#(first run edi_upload_prep.R)

p$site_type = 'precip_gauge'
s$site_type = 'stream'
ps = bind_rows(p, s)

pssub = select(ps, site, site_type, date, timeEST)
ps_same_datetime = ps[duplicated(pssub) | duplicated(pssub, fromLast = TRUE), ] %>%
    arrange(site_type, site, date, timeEST, notes)
pssub = select(ps, site, site_type, date)
ps_same_date = ps[duplicated(pssub) | duplicated(pssub, fromLast = TRUE), ]
ps_same_date = anti_join(same_date, same_datetime) %>% #!
    arrange(site_type, site, date, timeEST, notes)     #!
write_csv(ps_same_datetime, '/tmp/field_duplicates_same_datetime.csv')
write_csv(ps_same_date, '/tmp/field_duplicates_same_date_different_time.csv')

# *** how many archive dupes match up with stream/precip dupes?
zzset = paste(zz_same_datetime$site, zz_same_datetime$site_type, zz_same_datetime$date, zz_same_datetime$timeEST)
psset = paste(ps_same_datetime$site, ps_same_datetime$site_type, ps_same_datetime$date, ps_same_datetime$timeEST)
length(zzset)
length(psset)
length(intersect(psset, zzset))

#triplicates:
sum(table(zzset)>2)
sum(table(psset)>2)

# *** what about just same day matches?
zzset = paste(zz_same_date$site, zz_same_date$site_type, zz_same_date$date)
psset = paste(ps_same_date$site, ps_same_date$site_type, ps_same_date$date)
length(zzset)
length(psset)
length(intersect(psset, zzset))

#triplicates:
sum(table(zzset)>2)
sum(table(psset)>2)

#seriously?
# qq = ps[substr(ps$uniqueID, nchar(ps$uniqueID) - 2, nchar(ps$uniqueID)) == 'Dup', ]

#how many archive samples are not duplicated? (tons)
zzsub = select(zz, site, site_type, date, timeEST)
zz_datetime_unique = zz[! (duplicated(zzsub) | duplicated(zzsub, fromLast = TRUE)), ]
# zzsub = select(zz, site, site_type, date)
# zz_date_unique = zz[! (duplicated(zzsub) | duplicated(zzsub, fromLast = TRUE)), ]

#okay, how many field samples are unique (also tons)
pssub = select(ps, site, site_type, date, timeEST)
ps_datetime_unique = ps[! (duplicated(pssub) | duplicated(pssub, fromLast = TRUE)), ]

#how much overlap is there?
zzset = paste(zz_datetime_unique$site, zz_datetime_unique$site_type, zz_datetime_unique$date, zz_datetime_unique$timeEST)
psset = paste(ps_datetime_unique$site, ps_datetime_unique$site_type, ps_datetime_unique$date, ps_datetime_unique$timeEST)
length(zzset)
length(psset)
length(intersect(psset, zzset))

sitedatetimes_with_unique_pairs = intersect(psset, zzset)

sitedatetimes_with_unique_pairs = str_match(sitedatetimes_with_unique_pairs,
                                            '([^ ]+) ([^ ]+) ([^ ]+)')[, 2:4]
ps_datetime_unique %>%
    filter(date %in% sitedatetimes_with_unique_pairs[, 3] &
               site %in% sitedatetimes_with_unique_pairs[, 1])
zz_datetime_unique %>%
    filter(as.character(date) %in% sitedatetimes_with_unique_pairs[, 3] &
               site %in% sitedatetimes_with_unique_pairs[, 1])

## same as the above chunk, but without considering timeEST

#how many archive samples are not duplicated? (tons)
zzsub = select(zz, site, site_type, date)
zz_date_unique = zz[! (duplicated(zzsub) | duplicated(zzsub, fromLast = TRUE)), ]

#okay, how many field samples are unique (also tons)
pssub = select(ps, site, site_type, date)
ps_date_unique = ps[! (duplicated(pssub) | duplicated(pssub, fromLast = TRUE)), ]

#how much overlap is there?
zzset = paste(zz_date_unique$site, zz_date_unique$site_type, zz_date_unique$date)
psset = paste(ps_date_unique$site, ps_date_unique$site_type, ps_date_unique$date)
length(zzset)
length(psset)
length(intersect(psset, zzset))

sitedates_with_unique_pairs = intersect(psset, zzset)

sitedates_with_unique_pairs = str_match(sitedates_with_unique_pairs,
                                        '([^ ]+) ([^ ]+) ([^ ]+)')[, 2:4]
ps_date_unique %>%
    filter(date %in% sitedates_with_unique_pairs[, 3] &
               site %in% sitedates_with_unique_pairs[, 1])
zz_date_unique %>%
    filter(as.character(date) %in% sitedates_with_unique_pairs[, 3] &
               site %in% sitedates_with_unique_pairs[, 1])

#AH! it's because the time values in the archive have 8 characters, and the time values
#in the field data only have 5! resolved in edi_upload_prep.R

##

# :0
zzsub2 = select(zz_datetime_unique, site, date, timeEST, barcode) %>%
    mutate(across(everything(), as.character))
qq = ps_datetime_unique %>%
    mutate(across(everything(), as.character)) %>%
    left_join(zzsub2, by = c('site', 'date', 'timeEST'))


#haphazard stuff
qq = zz %>%
    filter(X2 == 'stream',
           ! is.na(X4),
           ! is.na(X3)) %>%
    right_join(filter(s, ! is.na(date), ! is.na(timeEST)),
               by = c(X3='date', X1='site', X4='timeEST'))

xxx=select(zz, site, date, timeEST)
qqq = xxx[duplicated(xxx) | duplicated(xxx, fromLast = T), ]

xxx=zz %>%
    filter(site=='W1') %>%
    select(site, date, timeEST)
qqq = xxx[duplicated(xxx) | duplicated(xxx, fromLast = T), ]
zz %>%
    mutate(date = as.character(date),
           timeEST = as.character(timeEST)) %>%
    filter(site=='W1',
           date == '2012-05-18',
           timeEST == '12:00:00') %>%
    as.data.frame()

xxx=select(arch, watershed, sample_date)

#checking for replicate archive samples (same date time and site)
xxx=select(arch, site, sample_date, timeEST)
qqq = xxx[duplicated(xxx) | duplicated(xxx, fromLast = T), ]

qqq = apply(qqq, 2, as.character) %>% as.data.frame()
qqq = filter(qqq, ! is.na(timeEST))
arch %>%
    mutate(sample_date = as.character(sample_date),
           timeEST = as.character(timeEST)) %>%
    filter(site==qqq$site[1],
           sample_date == qqq$sample_date[1],
           timeEST == qqq$timeEST[1]) %>%
    as.data.frame()

#checking for possible mislabeled replicate samples (same date and site)
xxx=select(arch, site, sample_date)
qqq = xxx[duplicated(xxx) | duplicated(xxx, fromLast = T), ]

qqq = apply(qqq, 2, as.character) %>% as.data.frame()
arch %>%
    mutate(sample_date = as.character(sample_date)) %>%
    filter(site==qqq$site[1],
           sample_date == qqq$sample_date[1]) %>%
    as.data.frame()

#do the replicated archive bottles always correspond to replicate field samples? (no)
sh %>%
    mutate(date = as.character(date)) %>%
    filter(site==qqq$site[1],
           date == qqq$sample_date[1]) %>%
    as.data.frame()

xxx=select(dataHistorical, site, date, timeEST)
qqq = xxx[duplicated(xxx) | duplicated(xxx, fromLast = T), ]

xxx=select(pc, site, date, timeEST)
qqq = xxx[duplicated(xxx) | duplicated(xxx, fromLast = T), ]
xxx=select(ph, site, date, timeEST)
qqq = xxx[duplicated(xxx) | duplicated(xxx, fromLast = T), ]
xxx=select(sc, site, date, timeEST)
qqq = xxx[duplicated(xxx) | duplicated(xxx, fromLast = T), ]

xxx=select(sh, site, date, timeEST)
qqq = xxx[duplicated(xxx) | duplicated(xxx, fromLast = T), ]
qqq = apply(qqq, 2, as.character) %>% as.data.frame()
qqq = filter(qqq, ! is.na(timeEST))
sh %>%
    mutate(date = as.character(date),
           timeEST = as.character(timeEST)) %>%
    filter(site==qqq$site[1],
           date == qqq$date[1],
           timeEST == qqq$timeEST[1]) %>%
    select(refNo, site, date, timeEST, uniqueID, waterYr, duplicate, pH, DIC, spCond) %>%
    as.data.frame()
