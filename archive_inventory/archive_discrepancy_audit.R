#audit of the HBEF stream sample archive against published stream chemistry.
#successor to archive_and_field_duplicates_exploration.R. writes output/*.csv:
#   bottle_checklist.csv: bottles for site personnel to pull and read, sorted by bin
#   lab_duplicate_bottles.csv: bottles with >1 chemistry analysis (primary + Dup)
#inputs:
#   data/w6 sample archive collection through 2026.xlsx (from Amey, 2026-10)
#   data/hb water duplicates w6.xlsx (from Amey; her W6 "collisions")
#   shiny/restricted_QAQC/data/archive_data/HB physical archives stream samples.csv (last full archive, thru 2017-05)
#   edi_upload/HubbardBrook_weekly_stream_chemistry.csv (EDI, thru 2025-05-27)

library(tidyverse)
library(readxl)
library(lubridate)

hb = '~/git/hbef'
wd = file.path(hb, 'hbef_misc/archive_inventory')
dir.create(file.path(wd, 'output'), showWarnings = FALSE)
ws = c(paste0('W', 1:9), 'W101')

hhmm = function(x){
    x = str_pad(x, 4, 'left', '0')
    paste0(substr(x, 1, 2), ':', substr(x, 3, 4))
}

# read ####

old = read_csv(file.path(hb, 'shiny/restricted_QAQC/data/archive_data/HB physical archives stream samples.csv'),
               skip = 2, col_types = cols(.default = 'c')) %>%
    rename_with(~gsub('\\s+', '_', .)) %>%
    mutate(row = row_number() + 3,
           site = gsub('ws', 'W', watershed),
           date = mdy(sample_date),
           old_date = mdy(old_date),
           time_raw = time_EST,
           timeEST = ifelse(time_EST %in% c('-9999', NA), NA, hhmm(time_EST)))

#time EST is HHMM through 1988-11, then excel day fractions; also "n/a" and -9999
new = read_excel(file.path(wd, 'data/w6 sample archive collection through 2026.xlsx'),
                 skip = 3, col_types = 'text') %>%
    rename(weight_g = `weight g`, sample_date = `sample date`, time_raw = `Time EST`,
           bottle_type = `bottle type`, notes = `notes sample condition`) %>%
    mutate(row = row_number() + 4,
           site = 'W6',
           bin = sprintf('%.4f', as.numeric(bin)),
           date = as.Date(as.numeric(sample_date), origin = '1899-12-30'),
           tnum = suppressWarnings(as.numeric(time_raw)),
           tnum = ifelse(tnum == -9999, NA, tnum),
           timeEST = case_when(is.na(tnum) ~ NA_character_,
                               tnum < 1 ~ sprintf('%02d:%02d', round(tnum * 1440) %/% 60,
                                                  round(tnum * 1440) %% 60),
                               TRUE ~ hhmm(as.integer(tnum))),
           weight_g = round(as.numeric(weight_g), 2)) %>%
    select(-tnum)

s = read_csv(file.path(hb, 'hbef_misc/edi_upload/HubbardBrook_weekly_stream_chemistry.csv'),
             col_types = cols(.default = 'c')) %>%
    mutate(date = as.Date(date)) %>%
    filter(site %in% ws)
chem_end = max(s$date)

amey = read_excel(file.path(wd, 'data/hb water duplicates w6.xlsx'), col_types = 'text') %>%
    filter(! is.na(`CSV line`))

#current best archive: old file for non-W6, Amey's new file for W6
arc = bind_rows(
    old %>%
        filter(site != 'W6') %>%
        transmute(src = 'old archive csv', row, site, bin, barcode, date, timeEST, time_raw,
                  notes = notes_sample_condition, old_date),
    new %>%
        left_join(select(filter(old, site == 'W6'), barcode, old_date), by = 'barcode') %>%
        transmute(src = 'new W6 xlsx', row, site, bin, barcode, date, timeEST, time_raw,
                  notes, old_date)) %>%
    mutate(bc = as.numeric(barcode))

key = function(d, dt = d$date, tm = d$timeEST) paste(d$site, dt, tm)
chemkey = unique(key(s))
arckey = key(arc)
chem_dates = distinct(s, site, date)
checks = list()

add_check = function(d, group, priority, question, chem_date = NA, chem_time = NA){
    if(! nrow(d)) return(invisible())
    checks[[length(checks) + 1]] <<- d %>%
        transmute(group = group, priority = priority, site, bin, barcode,
                  archive_date = as.character(date), archive_time = timeEST,
                  chem_date = as.character(chem_date), chem_time = chem_time,
                  question = question)
}

# lab duplicates (Amey's W6 pairs, generalized) ####

#bottles whose site/date/time has more than one chemistry analysis. these
#appeared as "collisions" in the portal; archive_merger.R now flags them
labdup = arc %>%
    inner_join(s %>%
                   group_by(site, date, timeEST) %>%
                   summarize(chem_flags = paste(sort(coalesce(duplicate, 'primary')), collapse = '+'),
                             .groups = 'drop') %>%
                   filter(grepl('Dup', chem_flags)),
               by = c('site', 'date', 'timeEST')) %>%
    mutate(in_ameys_list = barcode %in% amey$barcode) %>%
    select(site, bin, barcode, date, timeEST, chem_flags, in_ameys_list)

write_csv(labdup, file.path(wd, 'output/lab_duplicate_bottles.csv'))

# A. barcode recorded on two bottles ####

d = arc %>% group_by(barcode) %>% filter(n() > 1) %>% ungroup()
other = function(i) d %>% filter(barcode == d$barcode[i], row_number() != i)
nb = function(i){
    x = arc %>% filter(site == d$site[i], src == d$src[i], abs(row - d$row[i]) == 1)
    paste(x$barcode, collapse = ' and ')
}
add_check(d, 'A. barcode on two bottles', 1,
          map_chr(seq_len(nrow(d)), function(i){
              o = filter(d, barcode == d$barcode[i], ! (site == d$site[i] & date == d$date[i]))
              sprintf('Barcode %s is also recorded for %s %s. Neighboring bottles in this bin are %s. What barcode is on this bottle?',
                      d$barcode[i], o$site, o$date, nb(i))
          }))

# B. archive date disagrees with chemistry date ####

#bottle date has no chemistry at this site; chemistry exists within 7 days at
#the identical time, and no other bottle already accounts for it
cand = arc %>%
    anti_join(chem_dates, by = c('site', 'date')) %>%
    filter(date <= chem_end, ! is.na(timeEST)) %>%
    inner_join(select(s, site, chem_date = date, chem_time = timeEST),
               by = c('site', 'timeEST' = 'chem_time'), relationship = 'many-to-many') %>%
    mutate(days = as.numeric(chem_date - date)) %>%
    filter(abs(days) <= 7, ! key(., chem_date) %in% arckey) %>%
    group_by(site, barcode) %>%
    slice_min(abs(days), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(date, chem_date) %>%
    mutate(same_day = map_chr(barcode, ~paste(setdiff(barcode, .x), collapse = ', '))) %>%
    ungroup()

add_check(cand, 'B. date differs from chemistry', 1,
          paste0('Archive date ', cand$date, '; the chemistry record for ', cand$site,
                 ' has a sample at the same time (', cand$timeEST, ') on ', cand$chem_date,
                 ' and none on ', cand$date, '. What date is on the label?',
                 ifelse(cand$same_day != '',
                        paste0(' Same disagreement on this date for barcode(s) ', cand$same_day, '.'), ''),
                 ifelse(! is.na(cand$old_date),
                        paste0(' The archive date was previously changed from ', cand$old_date, '.'), '')),
          cand$chem_date, cand$timeEST)

#a previous date change moved the bottle off its chemistry match
d = arc %>%
    filter(! is.na(old_date), ! key(.) %in% chemkey, key(., old_date) %in% chemkey,
           ! barcode %in% cand$barcode)
add_check(d, 'B. date differs from chemistry', 1,
          paste0('Archive date was changed from ', d$old_date, ' to ', d$date,
                 '. Chemistry has a sample at ', d$timeEST, ' on ', d$old_date,
                 ' but not at that time on ', d$date, '. What date and time are on the label?'),
          d$old_date, d$timeEST)

# C. archive time disagrees with chemistry time ####

chem_day = s %>%
    group_by(site, date) %>%
    summarize(chem_times = paste(sort(unique(timeEST)), collapse = ' '),
              any_chem_time = any(! is.na(timeEST)), .groups = 'drop')
tm = arc %>%
    filter(! key(.) %in% chemkey) %>%
    inner_join(chem_day, by = c('site', 'date')) %>%
    mutate(chem_time_has_bottle = map2_lgl(paste(site, date), chem_times,
                                           ~any(paste(.x, strsplit(.y, ' ')[[1]]) %in% arckey)))

#days on which most watersheds' times disagree
wholeday = tm %>%
    filter(any_chem_time, ! chem_time_has_bottle, ! is.na(timeEST)) %>%
    count(date) %>%
    filter(n >= 4) %>%
    pull(date)

d = filter(tm, any_chem_time, ! chem_time_has_bottle)
add_check(d, 'C. time differs from chemistry', ifelse(d$date %in% wholeday, 1, 2),
          paste0(ifelse(is.na(d$timeEST), 'Archive has no time',
                        paste0('Archive time ', d$timeEST)),
                 '; chemistry for this site and date: ', d$chem_times,
                 '. What time is on the label?',
                 ifelse(d$date %in% wholeday,
                        ' Times disagree at most watersheds on this date.', '')),
          d$date, d$chem_times)

# D. more bottles than chemistry samples ####

d = arc %>%
    filter(! is.na(timeEST)) %>%
    group_by(site, date, timeEST) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    left_join(count(s, site, date, timeEST, name = 'n_chem'), by = c('site', 'date', 'timeEST'))
q = ifelse(d$n_chem > 1,
           'Two bottles share this date and time, and chemistry has a primary and a duplicate analysis. Is either bottle marked as a duplicate, or otherwise distinguishable?',
           'Two bottles share this date and time, but chemistry has one sample. Is one a field duplicate, or is one mislabeled?')
q[d$barcode %in% c('11447', '11448')] = paste(q[d$barcode %in% c('11447', '11448')],
    'Chemistry has a W6 sample on 1988-01-10 at 10:25 with no bottle, and 11449 is 1988-01-17.')
add_check(d, 'D. more bottles than chemistry samples', 2, q)

d = filter(tm, chem_time_has_bottle)
add_check(d, 'D. more bottles than chemistry samples', 3,
          paste0('Another bottle already matches the chemistry sample at ', d$chem_times,
                 '. What date and time are on this label?'),
          d$date, d$chem_times)

# E. chemistry has no time; bottle does ####

d = filter(tm, ! any_chem_time, ! is.na(timeEST))
add_check(d, 'E. chemistry record has no time', 3,
          ifelse(d$timeEST == '24:00',
                 'Archive time is 2400. Is that midnight at the end of this date? The chemistry record has no time.',
                 'The chemistry record has no time. Please confirm the label time, which would fill it in.'),
          d$date, NA)

# F. no chemistry ####

d = arc %>%
    anti_join(chem_dates, by = c('site', 'date')) %>%
    filter(date <= chem_end, ! barcode %in% cand$barcode,
           ! (site == 'W101' & date > as.Date('2013-05-13')))
add_check(d, 'F. no chemistry record', 3,
          'No chemistry for this site within 7 days at this time. What date and time are on the label?')

# G. changed between previous archive and new W6 file ####

o6 = old %>%
    filter(site == 'W6') %>%
    select(barcode, notes_old = notes_sample_condition)
d = inner_join(new, o6, by = 'barcode') %>%
    filter(coalesce(notes, '') != coalesce(notes_old, ''))
add_check(d, 'G. note changed in new file', 2,
          paste0('Note was "', coalesce(d$notes_old, ''), '", now "', coalesce(d$notes, ''),
                 '". Which of 10576 and 10577 has cloudy debris?'))

# out ####

checks = bind_rows(checks) %>%
    distinct(site, barcode, archive_date, group, .keep_all = TRUE) %>%
    mutate(label_barcode = '', label_date = '', label_time = '', notes = '') %>%
    arrange(as.numeric(bin), as.numeric(barcode))

write_csv(checks, file.path(wd, 'output/bottle_checklist.csv'), na = '')

cat('lab duplicate bottles:', nrow(labdup), '(', sum(labdup$in_ameys_list), 'in Amey list )\n')
print(count(checks, group, priority))
