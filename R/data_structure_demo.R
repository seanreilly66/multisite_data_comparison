library(tidyverse)
library(caret)

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

struct = bind_rows(a, b, c)

spec = bind_rows(x, y)

combo = left_join(struct, spec, relationship = 'many-to-many')

pred = bind_rows(struct, spec, combo)

full = left_join(pred, response, relationship = 'many-to-many')

full = full %>%
  select(-id) %>%
  group_by(group1, group2, resp_type) %>%
  nest()

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

