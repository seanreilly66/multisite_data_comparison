library(tidyverse)
library(glue)

# Individual files

files <- list.files('data/ml_output/results', '.csv', full.names = T)

fusion <- files %>%
  str_subset('fusion') %>%
  str_subset('compstat') %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  write_csv('data/ml_output/rf_fusion_compstat.csv')


files %>%
  str_subset('fusion') %>%
  str_subset('compbest') %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  write_csv('data/ml_output/rf_fusion_compbest.csv')

reg <- files %>%
  str_subset('rf_struct') %>%
  str_subset('compstat') %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  write_csv('data/ml_output/rf_reg_compbest.csv')

files %>%
  str_subset('rf_struct') %>%
  str_subset('compbest') %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  write_csv('data/ml_output/rf_reg_compbest.csv')


# full_df parameter level join for stats

files <- list.files('data/ml_output/full_df', 'Rdata$', full.names = T)

for (i in files) {
  
  param = read_rds(i) %>%
    pull(resp_type) %>%
    unique()
  
  x = str_replace(i, 'rf', glue('{param}_rf'))
  
  file.rename(i, x)
  
}

param_list = c('lai', 'biomass', 'cbh', 'cbd', 'densiometer', 'h_mean')

for (i in param_list) {
  
  list.files('data/ml_output/full_df', pattern = i, full.names = T) %>%
    lapply(read_rds) %>%
    bind_rows() %>%
    write_rds(glue('data/ml_output/full_df/merged/{i}_rf_full.Rdata'))

}

# post stats join of all param

list.files('data/ml_output/results/merged', '.csv$', full.names = T) %>%
    str_subset('compstat') %>%
    lapply(read_csv) %>%
    bind_rows() %>%
    write_csv('data/ml_output/rf_results_master.csv')

