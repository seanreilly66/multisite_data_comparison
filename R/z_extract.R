library(tidyverse)
library(lidR)

las_folder <- 'C:/academic/oxford/uas_biomass_study/data/las/plots'
output <- 'uas_fig_data.csv'

files <- list.files(las_folder, full.names = T, recursive = T, pattern = '_uas_hnrm.las$')

z_extract <- function(file) {
  
  las <- readLAS(file, select = '', filter = '-keep_random_fraction 0.01')@data$Z %>%
    as_tibble()
  
  
}

fig_data <- tibble(
  file = files
) %>%
  mutate(
    c = str_extract(file, '(?<=[:punct:]c)[:digit:]+(?=_)'),
    p = str_extract(file, '(?<=_p)[:digit:]+'),
    las_method = str_extract(file, '[:alpha:]+(?=_p[:digit:])'),
    z = map(
      .x = file, 
      .f = ~ z_extract(file = .x))
  ) %>%
  unnest(col = c(z))

write_csv(fig_data, output)
# 
# fig_data <- fig_data %>%
#   mutate(las_method = 'uas')
