library(tidyverse)

x = read_rds('data/biomass_rf_full.Rdata') 

x = x %>%
  ungroup() %>%
  slice(1) %>%
  select(-data)

mdl_varimp <- function(mdl) {
  
  var_imp <- caret::varImp(mdl) %>%
    pluck('importance') %>%
    rownames_to_column() %>%
    rename(var = rowname, imp = Overall)
  
  return(var_imp)
  
}


y <- x %>%
  mutate(
    var_imp = map(.x = rf,
                  .f = ~ mdl_varimp(mdl = .x))
  ) %>%
  unnest(var_imp) %>%
  rename(pred_var = var) %>%
  select(-rf)

z <- y %>%
  mutate(
    var_type = case_when(
      pred_var %in% c('z_max', 'z_p65', 'z_p70', 'z_p75', 'z_p80', 'z_p85', 'z_p90', 'z_p95') ~ 'Upper',
      pred_var %in% c('z_mean', 'z_p35', 'z_p40', 'z_p45', 'z_p50', 'z_p55', 'z_p60') ~ 'Middle',
      pred_var %in% c('z_p0', 'z_p5', 'z_p10', 'z_p15', 'z_p20', 'z_p25', 'z_p30') ~  'Lower',
      pred_var %in% c('z_sd', 'z_cv', 'z_kurtosis', 'z_skewness', 'z_iqr', 'canopy_relief_ratio') ~ 'Variation',
      pred_var %in% c('d01', 'd02', 'd03', 'd04', 'd05', 'd06', 'd07', 'd08', 'd09', 'd10') ~ 'Density',
      pred_var %in% c('cc_1m', 'cc_mean') ~ 'Cover',
      .default = 'Spectral'
    )
  )

