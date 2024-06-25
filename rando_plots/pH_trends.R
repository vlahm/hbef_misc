library(ggplot2)
library(readxl)
library(tidyverse)
library(broom) # offers clean ways to parse model objects

d <- read_xlsx('~/Downloads/NH_DES_StreampH 25.xlsx') %>%
    pivot_longer(cols = -YEAR,
                 names_to = 'site',
                 values_to = 'pH')

reference_sites <- c('HB_HBEF', 'HBW6')

## there's a convenience method in ggplot for plotting trendlines, but we need
## to determine the positive, negative, and non-significant ones first.
## There's a lot of arcane stuff going on here, but this is currently the
## cleanest way to run a bunch of models. That said, it's not the most transparent.

model_results <- d %>%
    group_by(site) %>%
    nest() %>% # turns the groups into nested data.frames ("tibbles"), for clean regression without creating new data structures
    mutate(model = map(data, ~ lm(pH ~ YEAR, data = .x)), # `map` iterates through the data.frames in column "data"
           deets = map(model, tidy)) %>% # the `tidy` function extracts model details
    unnest(deets) %>%
    filter(term == 'YEAR') %>% # drop the rows that contain intercept data
    mutate(significant = p.value < 0.05) %>% # create a binary column representing significance
    ungroup()

## get the names of sites with positive and negative trends

pos_sites <- model_results %>%
    filter(significant,
           estimate > 0,
           ! site %in% reference_sites) %>%
    pull(site)

neg_sites <- model_results %>%
    filter(significant,
           estimate < 0,
           ! site %in% reference_sites) %>%
    pull(site)

d %>%
    ggplot(aes(x = YEAR, y = pH, group = site)) +#, color = site)) +
    geom_point(color = 'gray50', alpha = 0.5) +
    geom_smooth(data = filter(d, site %in% pos_sites),
                aes(color = 'Positive Trend'),
                method = 'lm',
                se = FALSE) + # switch these to TRUE for conf ints (gets messy though)
    geom_smooth(data = filter(d, site %in% neg_sites),
                aes(color = 'Negative Trend'),
                method = 'lm',
                se = FALSE) +
    geom_smooth(data = filter(d, site == 'HB_HBEF'),
                aes(color = 'Hubbard Main'),
                method = 'lm',
                se = FALSE) +
    geom_smooth(data = filter(d, site == 'HBW6'),
                aes(color = 'Hubbard W6'),
                method = 'lm',
                se = FALSE) +
    scale_color_manual(values = c('Positive Trend' = 'black',
                                  'Negative Trend' = 'red',
                                  # greens wouldn't be colorblind-friendly with red
                                  'Hubbard Main' = 'dodgerblue2',
                                  'Hubbard W6' = 'skyblue'),
                       name = '') +
    guides(colour = guide_legend(reverse = TRUE)) + # easier than custom-specifying legend element order in this case
    theme_minimal() +
    labs(title = 'Significant Linear Trends in pH (\u03B1 = 0.05)',
         x = '',
         y = 'pH',
         color = 'Trend') +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
