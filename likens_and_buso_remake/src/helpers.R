get_edi_url <- function(prodcode, version){

    name_endpoint <- 'https://pasta.lternet.edu/package/name/eml/'
    dl_endpoint <- 'https://pasta.lternet.edu/package/data/eml/'
    domain_ref <- 'knb-lter-hbr'

    name_request <- paste0(name_endpoint, domain_ref, '/', prodcode, '/',
                           version)

    reqdata <- RCurl::getURLContent(name_request)

    reqdata <- strsplit(reqdata, '\n')[[1]]
    reqdata <- grep('Constants', reqdata, invert = TRUE, value = TRUE) #junk filter for hbef. might need flex
    reqdata <- str_match(reqdata, '([0-9a-zA-Z]+),(.+)')

    element_ids = reqdata[, 2]
    dl_urls <- paste0(dl_endpoint, domain_ref, '/', prodcode, '/', version,
                      '/', element_ids)

    avail_sets <- tibble(url = dl_urls,
                         filename = paste0(reqdata[, 3], '.csv'))

    return(avail_sets)
}

calc_vwc_wateryear <- function(d, var, sample_cutoff = NULL){

    #sample_cutoff: numeric or NULL. remove water years with < sample_cutoff sample
    #   days. if NULL, this parameter is ignored.

    vvar <- intersect(c('precip', 'discharge', 'flow_mm'), colnames(d))

    d <- d %>%
        select(site, waterYr, !!var, !!vvar) %>%
        filter(! if_any(c(!!var, !!vvar), is.na)) %>%
        group_by(site, waterYr) %>%
        summarize(!!sym(var) := mean(!!sym(var) * !!sym(vvar), na.rm = TRUE) /
                      mean(!!sym(vvar), na.rm = TRUE),
                  n = n(),
                  .groups = 'drop') %>%
        arrange(waterYr)

    d$source <- ifelse(vvar == 'precip', 'Precipitation', 'Streamwater')

    low_n <- which(d$n < sample_cutoff)
    if(length(low_n)){
        z <- ifelse(is.null(sample_cutoff), 'keeping', 'dropping')
        print(paste(z, 'water years with <', sample_cutoff, 'samples:'))
        print(arrange(d[low_n, ], site, waterYr),
              n = 100)
    }

    if(! is.null(sample_cutoff)){
        d <- filter(d, n >= sample_cutoff)
    }

    return(d)
}

calc_vwc_wateryear_v2 <- function(d, var, sample_cutoff = NULL){

    #sample_cutoff: numeric or NULL. remove water years with < sample_cutoff sample
    #   days. if NULL, this parameter is ignored.

    vvar <- intersect(c('precip', 'flow_mm'), colnames(d))

    d <- d %>%
        select(site, waterYr, date, !!var, !!vvar) %>%
        filter(! if_any(c(!!var, !!vvar), is.na)) %>%
        group_by(site, waterYr, month(date)) %>%
        summarize(!!sym(var) := !!sym(var) / !!sym(vvar),
                  n = n(),
                  .groups = 'drop') %>%
        group_by(site, waterYr) %>%
        summarize(!!sym(var) := sum(!!sym(var), na.rm = TRUE),
                  n = sum(n),
                  .groups = 'drop') %>%
        arrange(waterYr)

    d$source <- ifelse(vvar == 'precip', 'Precipitation', 'Streamwater')

    low_n <- which(d$n < sample_cutoff)
    if(length(low_n)){
        z <- ifelse(is.null(sample_cutoff), 'keeping', 'dropping')
        print(paste(z, 'water years with <', sample_cutoff, 'samples:'))
        print(arrange(d[low_n, ], site, waterYr),
              n = 100)
    }

    if(! is.null(sample_cutoff)){
        d <- filter(d, n >= sample_cutoff)
    }

    return(d)
}

calc_vwc_year <- function(d, var, sample_cutoff = NULL){

    #sample_cutoff: numeric or NULL. remove water years with < sample_cutoff sample
    #   days. if NULL, this parameter is ignored.

    vvar <- intersect(c('precip', 'discharge', 'flow_mm'), colnames(d))

    d <- d %>%
        mutate(year = year(date)) %>%
        select(site, year, !!var, !!vvar) %>%
        filter(! if_any(c(!!var, !!vvar), is.na)) %>%
        group_by(site, year) %>%
        summarize(!!sym(var) := mean(!!sym(var) * !!sym(vvar), na.rm = TRUE) /
                      mean(!!sym(vvar), na.rm = TRUE),
                  n = n(),
                  .groups = 'drop') %>%
        arrange(year)

    d$source <- ifelse(vvar == 'precip', 'Precipitation', 'Streamwater')

    low_n <- which(d$n < sample_cutoff)
    if(length(low_n)){
        z <- ifelse(is.null(sample_cutoff), 'keeping', 'dropping')
        print(paste(z, 'years with <', sample_cutoff, 'samples:'))
        print(arrange(d[low_n, ], site, year),
              n = 100)
    }

    if(! is.null(sample_cutoff)){
        d <- filter(d, n >= sample_cutoff)
    }

    d <- rename(d, waterYr = year) #for compatibility with get_trendline()

    return(d)
}

calc_mean_year <- function(d, var, sample_cutoff = NULL){

    #sample_cutoff: numeric or NULL. remove water years with < sample_cutoff sample
    #   days. if NULL, this parameter is ignored.

    vvar <- intersect(c('precip', 'discharge', 'flow_mm'), colnames(d))

    d <- d %>%
        mutate(year = year(date)) %>%
        select(site, year, !!var) %>%
        filter(! if_any(!!var, is.na)) %>%
        group_by(site, year) %>%
        summarize(!!sym(var) := mean(!!sym(var), na.rm = TRUE),
                  n = n(),
                  .groups = 'drop') %>%
        arrange(year)

    d$source <- ifelse(vvar == 'precip', 'Precipitation', 'Streamwater')

    low_n <- which(d$n < sample_cutoff)
    if(length(low_n)){
        z <- ifelse(is.null(sample_cutoff), 'keeping', 'dropping')
        print(paste(z, 'years with <', sample_cutoff, 'samples:'))
        print(arrange(d[low_n, ], site, year),
              n = 100)
    }

    if(! is.null(sample_cutoff)){
        d <- filter(d, n >= sample_cutoff)
    }

    d <- rename(d, waterYr = year) #for compatibility with get_trendline()

    return(d)
}

convert_to_long <- function(d){

    v_ <- setdiff(colnames(d), c('site', 'waterYr', 'n', 'source'))

    d %>%
        mutate(var = !!v_) %>%
        select(site, waterYr, var, val = !!v_, any_of('n'), source)
}

extend_x_axis <- function(d, to){
    tidyr::complete(d, site, waterYr = 2010:to)
}

get_trendline <- function(d, site, lims, filt = FALSE, poly = FALSE, smry = TRUE, baseline = NULL){
    #filt: if TRUE, filter the dataset by lims; otherwise regress on all data and
    #just plot within the lims

    if(filt) d <- filter(d, between(waterYr, lims[1], lims[2]))

    v_ <- setdiff(colnames(d), c('site', 'waterYr', 'n', 'source', 'var'))
    yrs <- seq(lims[1], lims[2])
    if(! is.null(baseline)) yrs <- yrs - baseline
    d <- filter(d, site == !!site)
    src <- ifelse(all(d$source == 'Precipitation'), 'trend_p', 'trend_s')
    fmla <- as.formula(paste(v_, "~ waterYr"))

    if(poly){
        # fmla <- update(fmla, . ~ poly(waterYr, 2, raw = TRUE))
        fmla <- as.formula(paste(deparse(fmla[[2]]), "~", "waterYr + I(waterYr^2)"))
    }

    if(! is.null(baseline)){
        d$waterYr <- d$waterYr - baseline
    }

    mod <- lm(fmla, data = d)
    if(smry) print(summary(mod))
    pred <- predict(mod, newdata = data.frame(waterYr = yrs))

    out <- tibble(site = rep(site, length = length(yrs)),
                  waterYr = yrs,
                  v_ = unname(pred),
                  source = rep(src, length = length(yrs))) %>%
        rename(!!v_ := v_)

    if(! is.null(baseline)) out$waterYr <- out$waterYr + baseline

    return(out)
}

convert_to_equivalents <- function(d){

    unconvertibles <- c('site', 'date', 'waterYr', 'discharge', 'precip', 'flow_mm',
                        'cationCharge', 'anionCharge', 'spCond', 'SpecCond_volwt', 'ANC')

    d_ms <- d %>%
        pivot_longer(-any_of(c('site', 'date', 'waterYr')),
                     names_to = 'var',
                     values_to = 'val') %>%
        mutate(ms_status = 0,
               ms_interp = 0) %>%
               # var = paste0('GN_', var)) %>%
        select(datetime = date, site_code = site, var, val, ms_status, ms_interp)

    # conv_list <- ms_drop_var_prefix(setdiff(unique(d_ms$var), unconvertibles))
    conv_list <- setdiff(unique(d_ms$var), unconvertibles)

    # d_msA <- filter(d_ms, ms_drop_var_prefix(var) %in% unconvertibles)
    # d_msB <- filter(d_ms, ! ms_drop_var_prefix(var) %in% unconvertibles)
    d_msA <- filter(d_ms, var %in% unconvertibles)
    d_msB <- filter(d_ms, ! var %in% unconvertibles)

    d_msB <- ms_conversions(d_msB,
                            convert_units_from = 'mg/L',
                            convert_units_to = 'ueq/L')

    d <- bind_rows(d_msA, d_msB) %>%
        # mutate(var = ms_drop_var_prefix(var)) %>%
        mutate(var = var) %>%
        select(date = datetime, site = site_code, var, val) %>%
        pivot_wider(names_from = var,
                    values_from = val) %>%
        mutate(waterYr = ifelse(month(date) < 6, year(date) - 1, year(date))) %>%
        select(site, date, waterYr, any_of(c('discharge', 'precip')),
               everything()) %>%
        arrange(site, date)

    return(d)
}
