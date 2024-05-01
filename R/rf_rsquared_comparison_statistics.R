library(tidyverse)

df_file <- 'data/ml_output/rf_results_master.Rdata'

output_file <- 'data/ml_output/rf_nofusion_stats.csv'

df_raw <- read_rds(df_file)

df_raw <- df_raw %>%
  mutate(struct_pred = replace_na(struct_pred, 'none'),
         spec_pred = replace_na(spec_pred, 'none')) %>%
  filter(struct_pred %in% c('none', 'tls', 'zeb', 'uas')) %>%
  filter(is.na(struct_type) | !(struct_type == 'pntcld' & h_thresh != 0.25))

x = df %>%
  group_by(resp_type, struct_pred, spec_pred, struct_type, vox_dim) %>%
  summarize(
    n = n()
  )
  


# Extract replicate r2 values

r2_extract = function(rf_train) {
  r2 = rf_train$resample$Rsquared
}

dunnet_nest <- function(dat, p_name) {
  
  dat = dat %>%
    arrange(desc(Rsquared)) %>%
    rowid_to_column() %>%
    unnest(r2)
  
  if (length(unique(dat$rowid)) == 1) {
    dat <- dat %>%
      group_by(rowid) %>%
      nest(r2 = r2) %>% 
      add_column(pval = 0) %>%
      rename(!!p_name := pval) %>%
      select(-rowid)
    
    return(dat)
  }
  
  dunnet <- DescTools::DunnettTest(r2 ~ rowid, data = dat, control = 1) %>%
    .[[1]] %>%
    as_tibble() %>%
    rowid_to_column() %>%
    mutate(rowid = rowid + 1) %>%
    select(rowid, pval)
  
  dat <- dat %>%
    group_by(rowid) %>%
    nest(r2 = r2) %>%
    left_join(dunnet, by = 'rowid') %>%
    mutate(pval = replace_na(pval, 100)) %>%
    rename(!!p_name := pval) %>%
    select(-rowid)
  
}

df <- df_raw %>%
  mutate(r2 = map(.x = rf,
                  .f = ~ r2_extract(rf_train = .x))) %>%
  select(-data) %>%
  arrange(desc(Rsquared))

# Testing all models per response
df <- df %>%
  group_by(resp_type) %>%
  nest() %>%
  mutate(data = map(.x = data, 
                       .f = ~ dunnet_nest(dat = .x, p_name = 'resp_p'),
                       .progress = T)) %>%
  unnest(data) %>%
  select(-rowid)

# Testing models per response by struct type
df <- df %>%
  group_by(resp_type, struct_pred) %>%
  nest() %>%
  mutate(data = map(.x = data, 
                    .f = ~ dunnet_nest(dat = .x, p_name = 'struct_p'),
                    .progress = T)) %>%
  unnest(data) %>%
  select(-rowid)

# Testing models per response by struct and spec type
df <- df %>%
  group_by(resp_type, struct_pred, spec_pred) %>%
  nest() %>%
  mutate(data = map(.x = data, 
                    .f = ~ dunnet_nest(dat = .x, p_name = 'struct_spec_p'),
                    .progress = T)) %>%
  unnest(data) %>%
  select(-rowid)


# Exporting final csv

df <- df %>%
  select(-r2)

write_csv(df, output_file)
