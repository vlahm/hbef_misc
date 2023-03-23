library(tidyverse)
library(readxl)
library(lubridate)

#TODO: figure out what to do with these new variables: "H conc", "TOTP"
#currently averaging duplicates. keep both?
#currently setting negatives to NA in the regular chem files. is 0 better?

mlchemf <- list.files('data', pattern = '^ml7[0-9]', full.names = TRUE)

#prepare mirror lake chemistry data for HBWatER portal ####

chem_sets <- list()
for(f in mlchemf){

    print(f)

    d <- readxl::read_xlsx(f, skip = 1, guess_max = 10000)
    if(length(unique(d$WS)) > 1) stop()

    d <- d %>%
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
               theoryCond = ifelse(`Calc cond` %in% c(-99.9), NA_real_, `Calc cond`),
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
               cationCharge = Cations, anionCharge = Anions, theoryCond,
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


# prepare mirror lake chemistry data for EDI ####

for(i in seq_along(chem_sets)){

    outname <- case_when(names(chem_sets)[i] == 'ML70' ~ 'ml-out',
                         names(chem_sets)[i] == 'ML71' ~ 'ml-ne',
                         names(chem_sets)[i] == 'ML72' ~ 'ml-nw',
                         names(chem_sets)[i] == 'ML73' ~ 'ml-west')

    chem_sets[[i]] %>%
        mutate(ws = substr(site, 3, 4),
               yr = year(date),
               mo = month(date),
               dy = day(date),
               across(-c(ws, yr, mo, dy), ~ifelse(is.na(.), -3, .))) %>%
        select(ws, yr, mo, dy, Ca, Mg, K, Na, NH4, pH, SO4, NO3, Cl, PO4, SiO2) %>%
        arrange(yr, mo, dy) %>%
        write_csv(paste0('out/', outname, '.txt'))
}


# prepare mirror lake depth profiles for EDI (currently basic mode. incorporate new vars) ####

#in for reals mode, watch out for -2 error codes!

depthprof <- readxl::read_xlsx('data/Mlake-pf 1967-2014.xlsx', guess_max = 10000) %>%
    mutate(DATE = format(as.Date(paste(year, month, day, sep = '-')), '%Y%m%d')) %>%
    select(DATE, Z = depth, Ca = `ca conc`, Mg = `mg conc`, K = `k conc`,
           Na = `na conc`, NH4 = `nh4 conc`, pH = ph, SO4 = `so4 conc`,
           NO3 = `no3 conc`, Cl = `cl conc`, PO4 = `po4 conc`, SiO2 = `sio2 conc`,
           ANC = anc, DIC = dic, DO = do, TEMP = temp, SpCond = spcond) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.) | . == -999.9, -3, .))) %>%
    arrange(DATE, desc(Z))

write_csv(depthprof, 'out/mlake-chm.txt')

# zz[is.na(zz$pass), ] %>% write_csv('out/potential_problem_records.csv')

# compile questionable negative values ####

depthprof[depthprof == -3] = NA
depthprof %>%
    filter(if_any(-Z, ~. < 0)) %>%
    write_csv('out/negative_value_records_depthprofile.csv')

negvals = tibble()
for(f in mlchemf){

    print(f)

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
               across(where(is.numeric), ~ifelse(abs(.) %in% c(999.9, 999), NA_real_, .))) %>%
        select(-est, -`field code`, -`ION error`, -`ION bal`, -`Temp °C`, -`Calc cond`, -datetime, -date, -`H graph`)

    d[d == -3] = NA
    d[d == -2] = NA
    negvals <- filter(d, if_any(everything(), ~. < 0)) %>%
        select(WS, yr, mm, dd, timeEST, where(~any(! is.na(.) & . < 0))) %>%
        relocate(timeEST, .after = 'dd') %>%
        bind_rows(negvals)
}

negvals %>%
    arrange(WS, yr, mm, dd, timeEST) %>%
    # print(n = 100) %>%
    write_csv('out/negative_value_records.csv')

# prepare rediscovered DOC data for hbwater ####

# d <- readxl::read_xlsx('data/samples_with_doc.xlsx', guess_max = 10000)
