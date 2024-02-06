# =================================== Libraries ================================

library(doParallel)
library(tidyverse)
library(sf)
library(caret)
library(glue)
library(MLmetrics)

# ==============================================================================
# ================================= User inputs ================================
# ==============================================================================

# Input files

response_csv <- 'data/field_data/plot_field_measurements.csv'
# structural_pred_csv <- 'data/predictor_df/tls_struct_predictors.csv'
spec_pred_csv <- 'data/predictor_df/planet_spec_predictors.csv'

structural_pred_csv <- NA
# spec_pred_csv <- NA

spatial_cluster_shp <- 'data/spatial_cluster/spatial_cluster.shp'
cluster_lookup_file <- 'data/spatial_cluster/spatial_cluster_lookup.csv'

# Output

output_file <- 'rf_spclst_{pred_type}_{type}_{format(timestamp, "%Y%m%d_%H%M")}'

# Model training parameters

k_folds <- 10
rfe_rep <- 10
training_rep <- 100

pre_process <- c('center', 'scale')

set_seed_val <- 111

n_cores <- detectCores() - 5


n_cores <- 5
output_file <- 'planet_rf_spclst_{pred_type}_{type}_{format(timestamp, "%Y%m%d_%H%M")}'

# ==============================================================================
# ============================== Data preparation ==============================
# ==============================================================================

# Spatial cluster

spatial_cluster <- read_sf(spatial_cluster_shp) %>%
  mutate(across(c('campaign', 'plot'), as.numeric)) %>%
  st_drop_geometry() %>%
  as_tibble() 

cluster_lookup <- read_csv(cluster_lookup_file)

# Response variables

response_df <- read_csv(response_csv)

response_var <- response_df %>%
  select(-site, -campaign, -plot) %>%
  colnames() %>%
  str_subset('_n', negate = TRUE)

# Predictor variables
  
if (!is.na(spec_pred_csv) & !is.na(structural_pred_csv)) {
  
  structural_pred <- read_csv(structural_pred_csv)
  
  spec_pred <- read_csv(spec_pred_csv) %>%
    rename(spec_method = method)
  
  predictor_df <- structural_pred %>%
    left_join(spec_pred)
  
  predictor_var <- predictor_df %>%
    select(-campaign, -plot, -method, -spec_method) %>%
    colnames()
  
} else if (!is.na(spec_pred_csv) & is.na(structural_pred_csv)) {
  
  predictor_df <- read_csv(spec_pred_csv) 
  
  predictor_var <- predictor_df %>%
    select(-campaign, -plot, -method) %>%
    colnames()
  
} else {
  
  predictor_df <- read_csv(structural_pred_csv)
  
  predictor_var <- predictor_df %>%
    select(-campaign, -plot, -method, -type, -h_thresh, -vox_dim) %>%
    colnames()
  
}

predictor_var <- predictor_var[! predictor_var %in% 'file']

n_predictors <- length(predictor_var)

# Combined df for modelling

model_df <- response_df %>%
  inner_join(predictor_df) %>%
  left_join(spatial_cluster)


rm(response_df, predictor_df, spatial_cluster)


# ==============================================================================
# =============================== Log file setup ===============================
# ==============================================================================

timestamp <- Sys.time()

structural_type <- model_df %>%
  filter(!is.na(method)) %>%
  pull(method) %>%
  unique()

if (!is.na(spec_pred_csv)) {
  
  spec_type <- model_df %>%
    filter(!is.na(spec_method)) %>%
    pull(spec_method) %>%
    unique()
  
  log_pred_type <- glue('{structural_type} structural metrics w/ {spec_type} spectral')
  pred_df <- glue('{structural_pred_csv}, 
                    {spec_pred_csv}')
  pred_type <- glue('{structural_type}_struct_{spec_type}_spec')
  
} else {
  
  log_pred_type <- glue('{structural_type} structural metrics')
  pred_df <- structural_pred_csv
  pred_type <- glue('{structural_type}_struct')
  
}

log_text <- glue(
  '=====================================================================
Random Forest w/ RFE canopy fuels prediction
Predictor type: {log_pred_type}
=====================================================================

author: Sean Reilly
initiated: {format(timestamp, "%Y-%m-%d %H:%M")}

=============================== Inputs ==============================

working directory: {getwd()}
predictor df: {pred_df}
response df: {response_csv}

N predictors: {length(predictor_var)}

Preprocessing: center and scale


========================== Model parameters =========================

random forest with RFE and spatial folds

rfe repeats: {rfe_rep}
training repeats: {training_rep}
set seed: {set_seed_val}
k folds: {k_folds}

=====================================================================
============================== Results ==============================
=====================================================================

'
)

# ==============================================================================
# ================================== Modelling ================================= 
# ==============================================================================

# ##### Testing setup #####
# log_text = log_text + '
# #### ###### TESTING RUN ###### ####
# 
# '
# model_df <- model_df %>%
#   add_column(foo = 1)
# 
# response_var = response_var[1:2]
# predictor_var <- predictor_var[1:5] %>%
#   append('foo')
# n_predictors = length(predictor_var)
# 
# response_i = response_var[1]
# 
# #####

ml_rfe = list()
ml_models = list()
ml_results = list()
ml_best = list()

for (response_i in response_var) {
  
  message('Response variable: ', response_i)
  
  log_text <- log_text + '
---------------------------------------------------------------------
------------------------------ {response_i} ------------------------------
---------------------------------------------------------------------
'
  # ---------------------------- Model input setup -----------------------------

  input_df <- model_df %>%
    filter(!is.na(!!sym(response_i)))

  ml_predictor <- input_df %>%
    select(all_of(predictor_var)) %>%
    mutate(across(.cols = everything(), ~ replace_na(.x, -9999)))

  ml_response <- input_df %>%
    pull(response_i)
  
  # ----------------------------- Summary function ----------------------------- 
  
  summary_func <- function(data, lev = NULL, model = NULL) {
    c(
      MAPE = MLmetrics::MAPE(data$pred, data$obs),
      RMSE = MLmetrics::RMSE(data$pred, data$obs),
      RMSPE = MLmetrics::RMSPE(data$pred, data$obs),
      MSE = MLmetrics::MSE(data$pred, data$obs),
      MAE = MLmetrics::MAE(data$pred, data$obs),
      R2 = summary(lm(pred ~ obs, data))$r.squared
    )
  }

  # --------------------- Repeated grouped K fold indexing ---------------------

  cluster_name <- cluster_lookup %>%
    filter(variable == response_i) %>%
    pull(cluster)
  
  cluster_index <- pull(input_df, cluster_name)
  
  set.seed(set_seed_val)

  rfe_folds <- list()

  for(i in 1:rfe_rep) {

    i_folds <- groupKFold(group = cluster_index, k = k_folds)

    pad_rep <- str_pad(i, nchar(rfe_rep), side = 'left', pad = '0')
    names(i_folds) <- sprintf('%s.Rep%s', names(i_folds), pad_rep)

    rfe_folds <- append(rfe_folds, i_folds)

  }

  train_folds <- list()

  for(i in 1:training_rep) {

    i_folds <- groupKFold(group = cluster_index, k = k_folds)

    pad_rep <- str_pad(i, nchar(training_rep), side = 'left', pad = '0')
    names(i_folds) <- sprintf('%s.Rep%s', names(i_folds), pad_rep)

    train_folds <- append(train_folds, i_folds)

  }

  # ----------------------------------- RFE ------------------------------------

  cl <- makeCluster(n_cores)
  registerDoParallel(cl)

  set.seed(set_seed_val)

  message('RFE initiated: ', Sys.time())

  ml_profile <- rfe(
    x = ml_predictor,
    y = ml_response,
    sizes = c(1:n_predictors),
    rfeControl = rfeControl(
      functions = rfFuncs,
      index = rfe_folds
    ),
    preProcess = pre_process,
    metric = "RMSE"
  )

  stopCluster(cl)

  ml_var <- predictors(ml_profile)

  ml_rfe[[glue('{response_i}_rf_rfe_spatial_folds')]] <- ml_profile

  # -------------------------------- Training ----------------------------------

  cl <- makeCluster(n_cores)
  registerDoParallel(cl)

  set.seed(set_seed_val)

  message('Random forest initiated: ', Sys.time())

  ml_train <- train(
    x = ml_predictor %>%
      select(all_of(ml_var)),
    y = ml_response,
    method = "rf",
    preProcess = pre_process,
    trControl = trainControl(
      index = train_folds,
      summaryFunction = summary_func,
      savePredictions = 'final',
      returnResamp = 'final',
      ),
    ntree = 1000,
    tuneLength = 100,
    metric = "RMSE"
  )

  stopCluster(cl)

  ml_models[[glue('{response_i}_rf_rfe_spatial_folds')]] <- ml_train

  ml_results[[glue('{response_i}_rf_rfe_spatial_folds')]] <-
    ml_train$results %>%
    add_column(response_var = response_i,
               model = 'rf_rfe_spatial_folds',
               method = pred_type,
               .before = 1)

  ml_stats <- ml_train$results %>%
    semi_join(ml_train$bestTune) %>%
    add_column(response_var = response_i,
               model = 'rf_rfe_spatial_folds',
               method = pred_type,
               .before = 1)

  ml_best[[glue('{response_i}_{pred_type}_rf_rfe_spatial_folds')]] <- ml_stats

  log_text <- log_text +
    '\n
spatial cluster: {cluster_name}
n samples: {nrow(input_df)}

RFE:
n variables: {length(ml_var)}

Results:

RMSE: {ml_stats$RMSE}
R2: {ml_stats$R2}
  
MAPE: {ml_stats$MAPE}
MAE: {ml_stats$MAE}
MSE: {ml_stats$MSE}
RMSPE: {ml_stats$RMSPE}
'
}

# ============================================================================
# ============================ Complete processing ===========================
# ============================================================================

saveRDS(ml_models,
        glue('data/ml_output/model/{glue(output_file, type = "model")}.RData'))

bind_rows(ml_best) %>%
  add_column(pre_process = glue_collapse(pre_process, sep = ", ")) %>%
  write_csv(glue(
    'data/ml_output/results/best/{glue(output_file, type = "results_best")}.csv'
  ))

bind_rows(ml_results) %>%
  add_column(pre_process = glue_collapse(pre_process, sep = ", ")) %>%
  write_csv(glue(
    'data/ml_output/results/full/{glue(output_file, type = "results_full")}.csv'
  ))

log_text <- log_text +
  '\n
---------------------------------------------------------------------

writing models to:
data/ml_output/model/{glue(output_file, type = "model")}.RData

writing stats to:
data/ml_output/results/best/{glue(output_file, type = "results_best")}.csv
data/ml_output/results/full/{glue(output_file, type = "results_full")}.csv

finished: {format(Sys.time(), "%Y-%m-%d %H:%M")}

---------------------------------------------------------------------
'

write(log_text,
      glue('data/ml_output/logs/{glue(output_file, type = "log")}.txt'))

# ==============================================================================

