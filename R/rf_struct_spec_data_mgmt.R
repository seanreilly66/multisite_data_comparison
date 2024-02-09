# =================================== Libraries ================================

library(tidyverse)
library(sf)
library(glue)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

# Input files

response_csv <- 'data/field_data/plot_field_measurements.csv'

tls_struct_csv <- 'data/predictor_df/tls_struct_predictors.csv'
zeb_struct_csv <- 'data/predictor_df/zeb_struct_predictors.csv'
uas_struct_csv <- 'data/predictor_df/uas_struct_predictors.csv'

uas_spec_csv <- 'data/predictor_df/uas_spec_predictors.csv'
planet_spec_csv <- 'data/predictor_df/planet_spec_predictors.csv'

spatial_cluster_shp <- 'data/spatial_cluster/spatial_cluster.shp'
cluster_lookup_file <-
  'data/spatial_cluster/spatial_cluster_lookup.csv'

# Output

full_folder <- 'data/ml_output/full_df/'
results_folder <- 'data/ml_output/results/'
file_base <- 'rf_struct_spec_{type}_{timestamp}.{ext}'

# ==============================================================================
# ================================= Read in data ===============================
# ==============================================================================

struct <- c(tls_struct_csv, zeb_struct_csv, uas_struct_csv) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  rename(struct_pred = method,
         struct_type = type) %>%
  select(-file)

spec <- c(uas_spec_csv, planet_spec_csv) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  rename(spec_pred = method)

response <- read_csv(response_csv) %>%
  select(!ends_with(c('_n', '_na')), -site) %>%
  pivot_longer(
    cols = !all_of(c('campaign', 'plot')),
    names_to = 'resp_type',
    values_to = 'resp_val',
    values_drop_na = TRUE
  )

cluster_lookup <- read_csv(cluster_lookup_file)
cluster_lookup = setNames(cluster_lookup$cluster, cluster_lookup$variable)

spatial_cluster <- read_sf(spatial_cluster_shp) %>%
  mutate(across(c('campaign', 'plot'), as.numeric)) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(campaign, plot, starts_with('cl')) %>%
  rename(all_of(cluster_lookup)) %>%
  pivot_longer(col = !all_of(c('campaign', 'plot')),
               names_to = 'resp_type',
               values_to = 'cluster_group')

# ==============================================================================
# ========================== Nested dataset preparation ========================
# ==============================================================================

mdl_df = left_join(struct, spec, relationship = 'many-to-many') %>%
  bind_rows(struct, spec, .) %>%
  left_join(response, relationship = 'many-to-many') %>%
  left_join(spatial_cluster) %>%
  group_by(struct_pred,
           struct_type,
           spec_pred,
           h_thresh,
           vox_dim,
           resp_type) %>%
  nest() %>%
  relocate(spec_pred, .after = struct_pred) %>%
  relocate(resp_type, .before = 1) %>%
  mutate(
    data = map(
      .x = data,
      .f = ~ janitor::remove_empty(.x, which = 'cols')
    ),
    data = map(.x = data, .f = ~ .x %>% mutate(
      across(.cols = starts_with('var'), ~ replace_na(.x, -9999))
    ))
  )

rm(struct, spec, response, spatial_cluster)

# ==============================================================================
# ================================ Apply model =================================
# ==============================================================================

k_folds <- 10
rfe_rep <- 10
training_rep <- 100

pre_process <- c('center', 'scale')

set_seed_val <- 111

n_cores <- detectCores() - 5

source('R/rf_spatial_fold_func.R')

tictoc::tic()

mdl_df <- mdl_df %>%
  mutate(
    rf = map(.x = data,
             .f = ~ mdl_func(
               df = .x,
               extra_col = c('campaign', 'plot', 'cluster_group')
             )),
    rf_stats = map(.x = rf,
                   .f = ~ mdl_stats(mdl = .x)),
    var_imp = map(.x = rf,
                  .f = ~ mdl_varimp(mdl = .x))
  ) %>%
  unnest(c(rf_stats, var_imp))

tictoc::toc()

# ==============================================================================
# =============================== Export output ================================
# ==============================================================================

timestamp = format(Sys.time(), "%Y%m%d%H%M")

write_rds(mdl_df, glue(full_folder, file_base, type = 'full', ext = 'Rdata'))

mdl_result <- mdl_df %>%
  select(-data, -rf)

write_csv(mdl_result,
          glue(results_folder, file_base, type = 'result', ext = 'csv'))

mdl_opt <- mdl_result %>%
  group_by(resp_type) %>%
  slice_max(order_by = Rsquared, n = 3)

write_csv(mdl_opt,
          glue(results_folder, file_base, type = 'opt', ext = 'csv'))

# ==============================================================================
# ==============================================================================
# ==============================================================================