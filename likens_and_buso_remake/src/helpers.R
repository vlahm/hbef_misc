calc_vwc_wateryear <- function(d, var){

    vvar <- intersect(c('precipCatch', 'flowGageHt'), colnames(d))

    d %>%
        select(site, waterYr, !!var, !!vvar) %>%
        group_by(site, waterYr) %>%
        summarize(!!sym(var) := mean(!!sym(var) * !!sym(vvar), na.rm = TRUE) /
                      mean(!!sym(vvar), na.rm = TRUE),
                  .groups = 'drop') %>%
        arrange(waterYr)
}

