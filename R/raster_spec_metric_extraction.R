# ==============================================================================
#
# Plot level satellite imagery spectral metric extraction
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 18 Dec 2023
# Last commit: 18 Dec 2023
#
# Status: Complete
#
# Created as part of 2023 Multisite data comparison study
#
# ==============================================================================
#
# Description:
#
# Produces a series of summary metrics from satellite spectral imagery. Averages
# data from multiple images when given more than one image per plot
#
# Output is a data frame containing all metrics for each plot
#
# ==============================================================================
#
# User inputs:
#
# 
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(terra)
library(sf)
library(tidyverse)
library(doParallel)
library(glue)

# ================================= User inputs ================================

gdb <- 'data/ssu_3dforests.gdb'
plot_shp_file <- 'field_plots'

rast_folder <- 'data/planet_imagery/site_raster'
raw_folder <- 'data/planet_imagery/raw_imagery'
file_pattern <- 'SR.tif$'

output_csv <- 'data/predictor_df/planet_spec_predictors'

n_cluster = 8

# =================================== Setup ==================================== 

plot_shp <- st_read(gdb, plot_shp_file)

record_id <- plot_shp %>%
  rownames_to_column(var = 'ID') %>%
  mutate(ID = as.numeric(ID)) %>%
  select(ID, campaign, plot) %>%
  st_drop_geometry()
  
rast_files <- list.files(raw_folder, pattern = file_pattern, full.names = T)

cl <- makeCluster(n_cluster)
registerDoParallel(cl)

rast_val <- foreach(
  file_i = rast_files,
  .combine = 'rbind',
  .packages = c('terra', 'tidyverse')
) %dopar% {
  
  rast_i <- rast(file_i)
  
  rast_i$ndvi <- (rast_i$nir - rast_i$red) / (rast_i$nir + rast_i$red)
  
  rast_val = terra::extract(rast_i, plot_shp, exact = T) %>%
    filter(!is.na(ndvi)) %>%
    left_join(record_id)
  
}

rast_metrics <- rast_val %>%
  group_by(campaign, plot) %>%
  summarize(
    blue_mean = mean(blue),
    green_mean = mean(green),
    red_mean = mean(red),
    nir_mean = mean(nir),
    ndvi_mean = mean(ndvi),
    blue_sd = sd(blue),
    green_sd = sd(green),
    red_sd = sd(red),
    nir_sd = sd(nir),
    ndvi_sd = sd(ndvi)
    
  )

write_csv(rast_metrics, output_csv)

