k_folds <- 3
rfe_rep <- 3
training_rep <- 10

pre_process <- c('center', 'scale')

set_seed_val <- 111

n_cores <- 1


mdl_func <- function(df) {
  
  # --------------------- Repeated grouped K fold indexing ---------------------
  
  set.seed(set_seed_val)
  
  rfe_folds <- list()
  
  for (i in 1:rfe_rep) {
    
    i_folds <- groupKFold(group = df$cluster_group, k = k_folds)
    
    pad_rep <- str_pad(i, nchar(rfe_rep), side = 'left', pad = '0')
    names(i_folds) <- sprintf('%s.Rep%s', names(i_folds), pad_rep)
    
    rfe_folds <- append(rfe_folds, i_folds)
    
  }
  
  train_folds <- list()
  
  for (i in 1:training_rep) {
    
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
  
  rf_train <- train(
    x = pred,
    y = resp,
    method = "rf",
    preProcess = pre_process,
    trControl = trainControl(
      index = train_folds,
      savePredictions = 'final',
      returnResamp = 'final',
    ),
    ntree = 1000,
    tuneLength = 100,
    metric = "RMSE"
  )
  
  stopCluster(cl)
  
  return(rf_train)
  
}


mdl_stats <- function(mdl) {
  
  mtry_opt = mdl$bestTune %>%
    pull(mtry)
  
  mdl_stats <- mdl$results %>%
    filter(mtry == mtry_opt)
  
}

mdl_var <- function(mdl) {
  
  var_imp <- varImp(mdl)
  
  var_imp <- var_imp$importance %>%
    rownames()
  
  
}