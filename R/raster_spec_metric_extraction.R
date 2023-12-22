# ==============================================================================
#
# Plot level imagery spectral metric extraction
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
# data from multiple images when given more than one image per plot. Calculates
# spectral indices depending on bands in given input rasters. Assumes RGB present.
#
# Output is a data frame containing all metrics for each plot
#
# Input raster band names must be among: blue, green, red, nir, rededge
#
# ==============================================================================
#
# User inputs:
#
# gdb: Geodatabase containing plot shapefile
# plot_shp_file: Name of plot shapefile layer
# 
# spec_method: Source of spectral data (e.g., planet for planet satellite data)
# raster_folder: Folder containing input raster files
# file_pattern: File search pattern to identify input rasters
# 
# raw_val_csv: CSV file name for export of extracted raster values
# metric_output_csv: CSV file name for export of summarized metrics per plot
# 
# n_cluster: Number of parallel processing threads
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

# Planet data
spec_method <- 'planet'
raster_folder <- 'data/spectral/planet/raw_imagery'
file_pattern <- 'SR.tif$'

# # UAS data
# spec_method <- 'uas'
# raster_folder <- 'data/spectral/uas/composite_rasters'
# file_pattern <- 'composite.tif$'

raw_val_csv <- 'data/spectral/{spec_method}_spec_val.csv'
metric_output_csv <- 'data/predictor_df/{spec_method}_spec_predictors.csv'

n_cluster = 8

# =================================== Setup ==================================== 

plot_shp <- st_read(gdb, plot_shp_file)

record_id <- plot_shp %>%
  rownames_to_column(var = 'ID') %>%
  mutate(ID = as.numeric(ID)) %>%
  select(ID, campaign, plot) %>%
  st_drop_geometry()
  
rast_files <- list.files(raster_folder, pattern = file_pattern, full.names = T)

# ====================== Extract plot raster cell values ======================= 

cl <- makeCluster(n_cluster)
registerDoParallel(cl)

rast_val <- foreach(
  file_i = rast_files,
  .combine = 'rbind',
  .packages = c('terra', 'tidyverse')
) %dopar% {
  
  rast_i <- rast(file_i)
  
  if ('nir' %in% names(rast_i)) {
    rast_i$ndvi <- (rast_i$nir - rast_i$red) / (rast_i$nir + rast_i$red)
    rast_i$gndvi <- (rast_i$nir - rast_i$green) / (rast_i$nir + rast_i$green)
  }
  
  if ('rededge' %in% names(rast_i)) {
    rast_i$ndre <- (rast_i$nir - rast_i$rededge) / (rast_i$nir + rast_i$rededge)
  }
  
  rast_val = terra::extract(rast_i, plot_shp, exact = T) %>%
    filter(!is.na(red)) %>%
    left_join(record_id) %>%
    select(-ID) %>%
    relocate(campaign, plot)
  
}

stopCluster(cl)

write_csv(rast_val, glue(raw_val_csv))

# ========================= Generate summary metrics =========================== 

spec_var <- rast_val %>%
  select(-campaign, -plot, -fraction) %>%
  colnames()

rast_metrics <- rast_val %>%
  group_by(campaign, plot) %>%
  dplyr::summarise(across(
    .cols = all_of(spec_var),
    .fns = list(
      mean = ~ wtd.mean(.x, fraction),
      sd = ~ sqrt(wtd.var(.x, fraction)),
      iqr = IQR,
      max = max,
      min = min,
      range = ~ max(.x) - min(.x),
      kurt = kurtosis,
      skew = skewness
    )
  )) %>%
  add_column(method = spec_method, .before = 3)

write_csv(rast_metrics, glue(metric_output_csv))

# ==============================================================================

