library(tidyverse)
library(caret)
library(doParallel)

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


source('R/rf_spatial_fold_func.R')


full = full %>%
  mutate(rf = map(.x = data, 
                  .f = ~ mdl_func(df = .x)))




,
         rf_stats = map(.x = rf,
                        .f = ~mdl_stats(mdl = .x))) %>%
  unnest(rf_stats)
