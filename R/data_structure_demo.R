library(tidyverse)
library(caret)

# ==============================================================================

a = tibble(
  id = 1:10,
  group1 = 'a',
  var1 = runif(10),
  var2 = runif(10),
  var3 = runif(10)
) 

b = tibble(
  id = 1:10,
  group1 = 'b',
  var1 = runif(10),
  var2 = runif(10),
  var3 = runif(10)
)

c = tibble(
  id = 1:10,
  group1 = 'c',
  var1 = runif(10),
  var2 = runif(10),
  var3 = runif(10)
)

x = tibble(
  id = 1:10,
  group2 = 'x',
  var4 = runif(10),
  var5 = runif(10)
) 

y = tibble(
  id = 1:10,
  group2 = 'y',
  var4 = runif(10),
  var5 = runif(10)
)

# ==============================================================================

response = tibble(
  id = 1:10,
  resp1 = runif(10),
  resp2 = runif(10),
  resp3 = c(1,2,3,4,NA,NA,7,8,9,10)
)

response = response %>%
  pivot_longer(
    cols = starts_with('resp'),
    names_to = 'resp_type',
    values_to = 'resp_val',
    values_drop_na = TRUE)

# ==============================================================================

spatial_cluster = tibble(
  id = 1:10,
  cl1 = c(1,1,1,2,2,2,3,3,3,4),
  cl2 = c(1,1,2,2,3,3,4,4,5,5),
  cl3 = 1:10
)

cluster_lookup = tibble(
  variable = c('resp1', 'resp2', 'resp3'),
  cluster = c('cl1', 'cl2', 'cl3')
)

cluster_lookup = setNames(cluster_lookup$cluster,cluster_lookup$variable)

spatial_cluster <- spatial_cluster %>%
  rename(all_of(cluster_lookup)) %>%
  pivot_longer(
    col = !all_of(c('id')),
    names_to = 'resp_type',
    values_to = 'cluster_group'
  )

# ==============================================================================

struct = bind_rows(a, b, c)

spec = bind_rows(x, y)

combo = left_join(struct, spec, relationship = 'many-to-many')

pred = bind_rows(struct, spec, combo)

full = left_join(pred, response, relationship = 'many-to-many')

full = left_join(full, spatial_cluster)

full = full %>%
  select(-id) %>%
  group_by(group1, group2, resp_type) %>%
  nest()

# ==============================================================================


k_folds <- 3
rfe_rep <- 3
training_rep <- 10

pre_process <- c('center', 'scale')

set_seed_val <- 111

n_cores <- detectCores() - 5




x = full[[4]][[1]]


set.seed(set_seed_val)

rfe_folds <- list()

for(i in 1:rfe_rep) {
  
  i_folds <- groupKFold(group = x$cluster_group, k = k_folds)
  
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









mdl_train <- function(df) {
  
  df = df %>%
    mutate(across(.cols = everything(), ~ replace_na(.x, -9999)))
  
  mdl_train = train(
    resp_val ~ .,
    data = df,
    method = 'rf'
  )
  
}

mdl_stats <- function(mdl) {
  
  mtry_opt = mdl$bestTune %>%
    pull(mtry)
  
  mdl_stats <- mdl$results %>%
    filter(mtry == mtry_opt)
  
}

full = full %>%
  mutate(rf = map(.x = data, 
                  .f = ~ mdl_train(df = .x)),
         rf_stats = map(.x = rf,
                        .f = ~mdl_stats(mdl = .x))) %>%
  unnest(rf_stats)






# --------------------- Repeated grouped K fold indexing ---------------------


spatial_cluster_shp <- 'data/spatial_cluster/spatial_cluster.shp'
cluster_lookup_file <- 'data/spatial_cluster/spatial_cluster_lookup.csv'

response_i = 'biomass_sum'

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

