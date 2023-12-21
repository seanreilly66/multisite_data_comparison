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
library(Hmisc)
library(moments)

# ================================= User inputs ================================

gdb <- 'data/ssu_3dforests.gdb'
plot_shp_file <- 'field_plots'

raster_folder <- 'data/planet_imagery/raw_imagery'
file_pattern <- 'SR.tif$'

raw_val_csv <- 'data/spectral/planet_spec_val.csv'
metric_output_csv <- 'data/predictor_df/planet_spec_predictors.csv'

n_cluster = 8

# =================================== Setup ==================================== 

plot_shp <- st_read(gdb, plot_shp_file)

record_id <- plot_shp %>%
  rownames_to_column(var = 'ID') %>%
  mutate(ID = as.numeric(ID)) %>%
  select(ID, campaign, plot) %>%
  st_drop_geometry()
  
rast_files <- list.files(raster_folder, pattern = file_pattern, full.names = T)

cl <- makeCluster(n_cluster)
registerDoParallel(cl)

rast_val <- foreach(
  file_i = rast_files,
  .combine = 'rbind',
  .packages = c('terra', 'tidyverse')
) %dopar% {
  
  rast_i <- rast(file_i)
  
  rast_i$ndvi <- (rast_i$nir - rast_i$red) / (rast_i$nir + rast_i$red)
  rast_i$gndvi <- (rast_i$nir - rast_i$green) / (rast_i$nir + rast_i$green)
  
  rast_val = terra::extract(rast_i, plot_shp, exact = T) %>%
    filter(!is.na(ndvi)) %>%
    left_join(record_id)
  
}

stopCluster(cl)

rast_metrics <- rast_val %>%
  group_by(campaign, plot) %>%
  dplyr::summarize(
    blue_mean = wtd.mean(blue, fraction),
    green_mean = wtd.mean(green, fraction),
    red_mean = wtd.mean(red, fraction),
    nir_mean = wtd.mean(nir, fraction),
    ndvi_mean = wtd.mean(ndvi, fraction),
    gndvi_mean = wtd.mean(gndvi, fraction),
    blue_sd = sqrt(wtd.var(blue, fraction)),
    green_sd = sqrt(wtd.var(green, fraction)),
    red_sd = sqrt(wtd.var(red, fraction)),
    nir_sd = sqrt(wtd.var(nir, fraction)),
    ndvi_sd = sqrt(wtd.var(ndvi, fraction)),
    gndvi_sd = sqrt(wtd.var(gndvi, fraction)),
    blue_iqr = IQR(blue),
    green_iqr = IQR(green),
    red_iqr = IQR(red),
    nir_iqr = IQR(nir),
    ndvi_iqr = IQR(ndvi),
    gndvi_iqr = IQR(gndvi),
    blue_max = max(blue),
    green_max = max(green),
    red_max = max(red),
    nir_max = max(nir),
    ndvi_max = max(ndvi),
    gndvi_max = max(gndvi),
    blue_min = min(blue),
    green_min = min(green),
    red_min = min(red),
    nir_min = min(nir),
    ndvi_min = min(ndvi),
    gndvi_min = min(gndvi),
    blue_skew = skewness(blue),
    green_skew = skewness(green),
    red_skew = skewness(red),
    nir_skew = skewness(nir),
    ndvi_skew = skewness(ndvi),
    gndvi_skew = skewness(gndvi),
    blue_kurt = kurtosis(blue),
    green_kurt = kurtosis(green),
    red_kurt = kurtosis(red),
    nir_kurt = kurtosis(nir),
    ndvi_kurt = kurtosis(ndvi),
    gndvi_kurt = kurtosis(gndvi)
  )


write_csv(rast_metrics, metric_output_csv)
write_csv(rast_val, raw_val_csv)
