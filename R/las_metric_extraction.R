# ==============================================================================
#
# Plot level point cloud metric extraction
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 31 Jan 2022
# Last commit: 19 Jan 2024
#
# Status: Complete
#
# Created as part of 2021 UAS biomass study.
#
# ==============================================================================
#
# Description:
#
# Produces a series of summary metrics as defined in point cloud metrics function
# for point clouds that have been height normalized and clipped to plot
# boundaries.
#
# Output is a data frame containing all metrics for each plot
#
# ==============================================================================
#
# User inputs:
#
# las_folder = Folder containing las files. Setup for recursive file extraction
#               for nested folder structures.
# metric_fn = Path to metric extraction function
# csv_output = CSV file path for export of output df
# n_cluster = Cluster size for parallel operation, equates to number of files
#               to be processed at once
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(doParallel)
library(glue)

# ================================= User inputs ================================

las_folder <- 'data/las'
metric_fn <- 'R/las_metric_function.R'
csv_output <-
  'data/predictor_df/las_plot_metrics_{format(Sys.time(), "%Y%m%d%H%M")}.csv'

n_cluster <- 2

# ============================= Point cloud metrics ============================

las_files <- list.files(
  path = las_folder,
  pattern = '.las$',
  recursive = TRUE,
  full.names = TRUE
) %>%
  str_subset('DEMnorm')

# las_files <- las_files[1:2]

cl <- makeCluster(n_cluster)
registerDoParallel(cl)

las_metrics <- foreach(
  file_i = las_files,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'raster')
) %dopar% {
  
  source('R/las_metric_function.R')
  
  c <- str_extract(file_i, '(?<=[:punct:]c)[:digit:]+(?=_)')
  p <- str_extract(file_i, '(?<=_p)[:digit:]+')
  las_method <- str_extract(file_i, '[:alpha:]+(?=_p[:digit:])')
  
  # las_i <- readLAS(file_i)
  las_i <- readLAS(file_i, filter = '-keep_random_fraction 0.0001')
  
  h_thresh <- c(0.25, 0.5, 0.75, 0.9)
  
  pnt_metrics <- foreach(
    h_i = h_thresh,
    .combine = 'rbind'
  ) %do% {
    
    pnt_metrics <- las_i %>%
      filter_poi(Z > h_i) %>%
      cloud_metrics(~ las_cld_metrics(z = Z)) %>%
      as_tibble() %>%
      add_column(
        campaign = c,
        plot = p,
        method = las_method,
        type = 'pntcld',
        h_thresh = h_i,
        vox_dim = NA,
        .before = 1
      ) %>%
      add_column(file = file_i)
    
  }

  vox_dim <- c(0.1, 0.25, 0.5, 1, 3.7)
  
  vox_metrics <- foreach(
    vox_i = vox_dim,
    .combine = 'rbind'
  ) %do% {
    
    vox_metrics <- las_i %>%
      voxelize_points(res = vox_i) %>%
      cloud_metrics( ~ las_cld_metrics(z = Z)) %>%
      as_tibble() %>%
      add_column(
        campaign = c,
        plot = p,
        method = las_method,
        type = 'voxel',
        h_thresh = NA,
        vox_dim = vox_i,
        .before = 1
      ) %>%
      add_column(file = file_i)
  
  }
  
  las_metrics <- pnt_metrics %>%
    add_row(vox_metrics)
  
}

stopCluster(cl)

write_csv(las_metrics, glue(csv_output))

# ==============================================================================
