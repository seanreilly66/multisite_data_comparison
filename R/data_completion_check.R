library(tidyverse)

resp <- read_csv('data/field_data/plot_field_measurements.csv')

tls <- read_csv('data/predictor_df/tls_struct_predictors.csv') %>%
  filter(type == 'pntcld',
         h_thresh == 0.25)
  
uas <- read_csv('data/predictor_df/uas_struct_predictors.csv') %>%
  filter(type == 'pntcld',
         h_thresh == 0.25)

zeb <- read_csv('data/predictor_df/zeb_struct_predictors.csv') %>%
  filter(type == 'pntcld',
         h_thresh == 0.25)

planet <- read_csv('data/predictor_df/planet_spec_predictors.csv')

tls_missing = anti_join(resp, tls)
tls_samp = inner_join(resp, tls)
tls_extra = anti_join(tls,resp)

zeb_missing = anti_join(resp, zeb)
zeb_samp = inner_join(resp, zeb)
zeb_extra = anti_join(zeb, resp)


uas_missing = anti_join(resp, uas)
planet_missing = anti_join(resp, planet)

tls_samp %>%
  semi_join(planet, by = c('campaign', 'plot')) %>%
  nrow()

shared_samp = inner_join(resp, uas, by = c('campaign', 'plot')) %>%
  inner_join(tls, by = c('campaign', 'plot')) %>%
  inner_join(zeb, by = c('campaign', 'plot'))

site_samp = shared_samp %>%
  group_by(site) %>%
  summarize(n = n())
