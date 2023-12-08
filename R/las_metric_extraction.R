# ==============================================================================
#
# Plot level point cloud metric extraction
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 31 Jan 2022
# Last commit: 5 Dec 2023
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

# =========================== UAS Point cloud metrics ==========================

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
  las_i = las_files,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'raster')
) %dopar% {
  
  source('R/las_metric_function.R')
  
  c <- str_extract(las_i, '(?<=[:punct:]c)[:digit:]+(?=_)')
  p <- str_extract(las_i, '(?<=_p)[:digit:]+')
  las_method <- str_extract(las_i, '[:alpha:]+(?=_p[:digit:])')
  
  las_i_metrics <- readLAS(las_i) %>%
  # las_i_metrics <- readLAS(las_i, filter = '-keep_random_fraction 0.01') %>%  
    cloud_metrics( ~ las_cld_metrics(z = Z)) %>%
    as_tibble() %>%
    add_column(
      campaign = c,
      plot = p,
      method = las_method,
      .before = 1
    ) %>%
    add_column(file = las_i)
  
}

stopCluster(cl)

write_csv(las_metrics, glue(csv_output))

# ==============================================================================
