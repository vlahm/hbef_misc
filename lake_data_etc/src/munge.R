library(tidyverse)
library(readxl)
library(lubridate)

#TODO: figure out what to do with these new variables:
    #"H conc", "TOTP"

mlchemf <- list.files('data', pattern = '^ml7[0-9]', full.names = TRUE)

#prepare mirror lake chemistry data for HBWatER portal ####

chem_sets <- list()
for(f in mlchemf){

    print(f)

    if(length(unique(d$WS)) > 1) stop()

    d <- readxl::read_xlsx(f, skip = 1, guess_max = 10000) %>%
        mutate(date = ymd(paste(yr, mm, dd, sep = '-')),
               est = as.character(est),
               timeEST = ifelse(est %in% c('-9999', '9999'),
                                NA_character_,
                                paste0(sub('([0-9]{2})$',
                                           ':\\1',
                                           str_pad(est, 4, 'left', '0')),
                                       ':00')),
               datetime = ymd_hms(paste(date, timeEST)),
               site = paste0('ML', WS),
               # across(where(~is.numeric(.) & ! inherits(., 'Period')), ~ifelse(abs(.) %in% c(999.9, 999), NA_real_, .)),
               across(where(is.numeric), ~ifelse(abs(.) %in% c(999.9, 999), NA_real_, .)),
               across(where(is.numeric) & -any_of(c('fieldCode', 'ionError', 'ionBalance', 'Temp °C')),
                      ~ifelse(. < 0, NA_real_, .)),
               Anions = -Anions,
               temp = ifelse(`Temp °C` %in% c(-99.9), NA_real_, `Temp °C`),
               refNo = NA_integer_,
               pHmetrohm = NA_real_,
               `H graph` = ifelse(`H graph` %in% c("STAGE", "FLUME", "VNOTCH", "V-NOTCH"), NA_character_, `H graph`),
               precipCatch = NA_real_,
               notes = NA_character_,
               archived = NA_character_,
               uniqueID = paste(site,
                                format(date, '%Y%m%d'),
                                sub(':', '', timeEST),
                                sep = '_'),
               waterYr = ifelse(mm < 6, yr - 1, yr),
               DON = NA_real_,
               Mn = NA_real_,
               Fe = NA_real_,
               `F` = NA_real_,
               sampleType = NA_character_) %>%
        group_by(uniqueID) %>%
        summarize(across(where(is.numeric), mean, na.rm = TRUE),
                  `field code` = paste(`field code`, collapse = ' '),
                  across(where(~! is.numeric(.)), first)) %>%
        ungroup() %>%
        select(refNo, site, date, timeEST, pH, pHmetrohm, DIC, spCond = SpCond,
               temp, ANC960 = ANC, gageHt = Stage, hydroGraph = `H graph`, flowGageHt = Flow,
               precipCatch, fieldCode = `field code`, notes, archived,
               uniqueID, waterYr, datetime, Ca, Mg, K, Na, Al_ICP = `AL-icp`,
               NH4, SO4, NO3, Cl, PO4, DOC, TDN, DON, SiO2, Mn, Fe, `F`,
               cationCharge = Cations, anionCharge = Anions, theoryCond = `Calc cond`,
               ionError = `ION error`, sampleType, ionBalance = `ION bal`) %>%
        arrange(date, timeEST)

    d[is.na(d)] <- NA
    d$duplicate <- NA
    d <- relocate(d, duplicate, .after = ionError)

    if(any(duplicated(d$uniqueID))) stop('dupes')

    chem_sets[[d$site[1]]] <- d
}

portald <- read_csv('data/HBEFdata_All_2023-03-21.csv') %>%
    filter(site == 'ML70') %>%
    mutate(timeEST = as.character(timeEST),
           datetime = as_datetime(datetime, format = '%m/%d/%Y %H:%M'))

portald_add1 <- anti_join(portald, chem_sets$ML70, by = 'uniqueID')
portald_add2 <- semi_join(portald, chem_sets$ML70, by = 'uniqueID')
chem_sets_rm <- anti_join(chem_sets$ML70, portald, by = 'uniqueID')

chem_sets$ML70 <- chem_sets$ML70 %>%
    filter(! uniqueID %in% chem_sets_rm) %>%
    bind_rows(portald_add1) %>%
    bind_rows(portald_add2) %>%
    arrange(date, timeEST, duplicate)


# [UNDEVELOPED] prepare mirror lake chemistry data for EDI ####

for(f in mlchemf){

    print(f)

    # d <- d %>%
    #     rename(ws = WS, mo = mm, dy = dd,
    #            Ca, Mg, K, Na, NH4, pH, SO4, NO3, Cl, PO4, SiO2)

    d <- readxl::read_xlsx(f, skip = 1, guess_max = 10000) %>%
        mutate(date = ymd(paste(yr, mm, dd, sep = '-')),
               est = as.character(est),
               timeEST = ifelse(est %in% c('-9999', '9999'),
                                NA_character_,
                                paste0(sub('([0-9]{2})$',
                                           ':\\1',
                                           str_pad(est, 4, 'left', '0')))),
               datetime = ymd_hm(paste(date, timeEST)),
               site = paste0('ML', WS),
               across(where(is.numeric), ~ifelse(abs(.) %in% c(999.9, 999), NA_real_, .)),
               across(where(is.numeric) & -any_of(c('fieldCode', 'ionError', 'ionBalance', 'Temp °C')),
                      ~ifelse(. < 0, NA_real_, .)),
               Anions = -Anions,
               temp = ifelse(`Temp °C` %in% c(-99.9), NA_real_, `Temp °C`),
               refNo = NA_integer_,
               pHmetrohm = NA_real_,
               `H graph` = ifelse(`H graph` %in% c("STAGE", "FLUME", "VNOTCH", "V-NOTCH"), NA_character_, `H graph`),
               precipCatch = NA_real_,
               notes = NA_character_,
               archived = NA_character_,
               uniqueID = paste(site,
                                format(date, '%Y%m%d'),
                                sub(':', '', timeEST),
                                sep = '_'),
               waterYr = ifelse(mm < 6, yr - 1, yr),
               DON = NA_real_,
               Mn = NA_real_,
               Fe = NA_real_,
               `F` = NA_real_,
               # duplicate = ,
               sampleType = NA_character_) %>%
        select(refNo, site, date, timeEST, pH, pHmetrohm, DIC, spCond = SpCond,
               temp, ANC960 = ANC, gageHt = Stage, hydroGraph = `H graph`, flowGageHt = Flow,
               precipCatch, fieldCode = `field code`, notes, archived,
               uniqueID, waterYr, datetime, Ca, Mg, K, Na, Al_ICP = `AL-icp`,
               NH4, SO4, NO3, Cl, PO4, DOC, TDN, DON, SiO2, Mn, Fe, `F`,
               cationCharge = Cations, anionCharge = Anions, theoryCond = `Calc cond`,
               ionError = `ION error`, sampleType, ionBalance = `ION bal`) %>%
        group_by(uniqueID) %>%
        # summarize(fieldCode = paste(fieldCode, collapse = ' ')) %>%
        summarize(across(where(is.numeric), mean, na.rm = TRUE),
                  fieldCode = paste(fieldCode, collapse = ' '),
                  across(where(~! is.numeric(.)), first)) %>%
        ungroup() %>%
        arrange(date, timeEST)

    d[is.na(d)] <- NA

    print(max(d$date))
}

# prepare mirror lake depth profiles for EDI ####

d <- readxl::read_xlsx('data/Mlake-pf 1967-2014.xlsx', guess_max = 10000)

# prepare rediscovered DOC data for hbwater ####

# d <- readxl::read_xlsx('data/samples_with_doc.xlsx', guess_max = 10000)
