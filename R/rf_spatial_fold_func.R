


rf_rfe <- function(df) {
  
  # --------------------- Repeated grouped K fold indexing ---------------------
  
  set.seed(set_seed_val)
  
  rfe_folds <- list()
  
  for(i in 1:rfe_rep) {
    
    i_folds <- groupKFold(group = df$cluster_group, k = k_folds)
    
    pad_rep <- str_pad(i, nchar(rfe_rep), side = 'left', pad = '0')
    names(i_folds) <- sprintf('%s.Rep%s', names(i_folds), pad_rep)
    
    rfe_folds <- append(rfe_folds, i_folds)
    
  }
  
  train_folds <- list()
  
  for(i in 1:training_rep) {
    
    i_folds <- groupKFold(group = df$cluster_group, k = k_folds)
    
    pad_rep <- str_pad(i, nchar(training_rep), side = 'left', pad = '0')
    names(i_folds) <- sprintf('%s.Rep%s', names(i_folds), pad_rep)
    
    train_folds <- append(train_folds, i_folds)
    
  }
  
  # ----------------------------------- RFE ------------------------------------
  
  df = df %>%
    mutate(across(.cols = everything(), ~ replace_na(.x, -9999)))
  
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  set.seed(set_seed_val)
  
  resp = df %>% pull(resp_val)
  pred = df %>% select(-resp_val, -cluster_group)
  
  rfe_profile <- rfe(
    x = pred,
    y = resp,
    sizes = c(1:ncol(pred)),
    rfeControl = rfeControl(
      functions = rfFuncs,
      index = rfe_folds
    ),
    preProcess = pre_process,
    metric = "RMSE"
  )
  
  stopCluster(cl)
  
  ml_var <- predictors(rfe_profile)
  
  # -------------------------------- Training ----------------------------------
  
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  set.seed(set_seed_val)
  
  pred = df %>% select(all_of(predictors(rfe_profile)))
  
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