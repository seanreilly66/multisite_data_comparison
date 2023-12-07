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
#
# ==============================================================================
#
# Known problems:
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(doParallel)

# ================================= User inputs ================================

las_folder <- 'data/las'
metric_fn <- 'R/las_metric_function.R'
csv_output <- 'data/predictor_df/las_plot_metrics_{format(timestamp, "%Y%m%d%H%M")}.csv'

# =========================== UAS Point cloud metrics ==========================

uas_files <- list.files(path = las_folder,
                        pattern = '.las$',
                        recursive = TRUE,
                        full.names = TRUE) %>%
  str_subset('DEMnorm')

cl <- makeCluster(10)
registerDoParallel(cl)

las_metrics <- foreach (
  las = las_files,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'raster')
) %dopar% {
  source(metric_fn)
  
  c <- str_extract(las, '(?<=_c)[:digit:]+')
  p <- str_extract(las, '(?<=_p)[:digit:]+')
  las_method <- str_extract(las, '(?<=_c[:digit:]+_[:alpha:]+')
  
  uas_metrics <- readLAS(uas) %>%
    cloud_metrics(
      ~ uas_cld_metrics(
        z = Z,
        r = red,
        g = green,
        b = blue,
        re = re,
        nir = nir,
        ndvi = ndvi,
        ndre = ndre,
        gndvi = gndvi
      )
    ) %>%
    as_tibble() %>%
    add_column(
      campaign = c,
      plot = p,
      method = 'uas',
      .before = 1
    )
  
}

write_csv(uas_metrics, uas_output)

# =========================== als Point cloud metrics ==========================

als_files <- list.files(path = las_folder,
                        pattern = '_als',
                        full.names = TRUE)

cl <- makeCluster(10)
registerDoParallel(cl)

als_metrics <- foreach (
  als = als_files,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'raster')
) %dopar% {
  source('R/als_metric_function.R')
  
  c <- str_extract(als, '(?<=_c)[:digit:]+')
  p <- str_extract(als, '(?<=_p)[:digit:]+')
  
  als_metrics <- readLAS(als) %>%
    cloud_metrics(~ als_cld_metrics(z = Z)) %>%
    as_tibble() %>%
    add_column(
      campaign = c,
      plot = p,
      method = 'als',
      .before = 1
    )
  
}

write_csv(als_metrics, als_output)

# ==============================================================================
