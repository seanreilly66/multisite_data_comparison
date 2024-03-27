library(tidyverse)

files <- list.files('data/ml_output/full_df', '.Rdata', full.names = T)

files <- files %>%
  str_subset('fusion')

files <- list.files('data/ml_output/full_df/merged', '.Rdata', full.names = T)

for (i in files) {
  
  message(i)
  
  x = read_rds(i)
  
  rsq_ext = function(rf_train) {
    rf = rf_train$resample$Rsquared
    
  }
  
  x = x %>%
    mutate(rf = map(.x = rf,
                    .f = ~ rsq_ext(rf_train = .x))) %>%
    select(-data) %>%
    arrange(desc(Rsquared)) %>%
    rowid_to_column() %>%
    unnest(rf)
  
  y = DescTools::DunnettTest(rf ~ rowid, data = x, control = 1) %>%
    .[[1]] %>%
    as_tibble() %>%
    rowid_to_column() %>%
    mutate(rowid = rowid + 1)
  
  x = x %>%
    group_by(rowid) %>%
    nest(data = rf) %>%
    select(-data) %>%
    left_join(y) %>%
    select(-rowid)
  
  output = str_replace(i, 'full_df', 'results') %>%
    str_replace('full', 'compstat') %>%
    str_replace('Rdata', 'csv')
  
  write_csv(x, output)
  
  output = str_replace(i, 'full_df', 'results') %>%
    str_replace('full', 'compbest') %>%
    str_replace('Rdata', 'csv')
  
  x = x %>%
    filter(is.na(pval) | pval >= 0.05)
  
  write_csv(x, output)
  
}
