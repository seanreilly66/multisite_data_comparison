library(caret)
library(doParallel)
library(glue)

mdl_func <- function(df, extra_col) {
  
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
  pred = df %>% select(-resp_val, -any_of(extra_col))
  
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
  
  return(mdl_stats)
}

mdl_varimp <- function(mdl) {
  
  var_imp <- varImp(mdl) %>%
    pluck('importance') %>%
    rownames_to_column() %>%
    rename(var = rowname, imp = Overall)  %>%
    slice_max(imp, n = 5)
  
  while (nrow(var_imp) < 5) {
    
    var_imp <- var_imp %>%
      add_row()
    
  }
  
  var_imp <- var_imp %>%
    add_column(var_lvl = 1:5) %>%
    pivot_wider(names_from = var_lvl, values_from = c(var, imp))
  
  return(var_imp)
 
}
