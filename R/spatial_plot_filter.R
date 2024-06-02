library(tidyverse)
library(sf)

plt_list <- read_csv('data/plot_list.csv')

plt_shp <- read_sf('data/ssu_3dforests.gdb', layer = 'field_plots') %>%
  mutate(plot = as.numeric(plot)) %>%
  semi_join(plt_list)

write_sf(plt_shp, 'data/spatial/plt_shp.shp')

uas_poly <- read_sf('data/ssu_3dforests.gdb', layer = 'uas_zones') %>%
  st_filter(plt_shp)

write_sf(uas_poly, 'data/spatial/uas_box.shp')
