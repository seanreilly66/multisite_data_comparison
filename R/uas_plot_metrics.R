# ==============================================================================
#
# UAS plot level point cloud metrics
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 31 Jan 2022
# Last commit:
#
# Status: Needs documentation
#
# Created as part of 2021 UAS biomass study.
#
# ==============================================================================
#
# Description:
#
# Produces a series of summary metrics as defined in point cloud metrics function
# for UAS point clouds that have been height normalized and clipped to plot
# boundaries.
#
# Output is a data frame containing all metrics for each plot
#
# ==============================================================================
#
# User inputs:
#
# ==============================================================================
#
# Package dependencies:
#
# lidR, tidyverse
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

uas_las_folder <- 'data/las/plots'
output_file <- 'data/las/metrics/uas_plot_metrics_temp.csv'

# ============================= Point cloud metrics ============================

uas_files <- list.files(path = uas_las_folder,
                        pattern = '_uas',
                        full.names = TRUE)

cl <- makeCluster(10)
registerDoParallel(cl)

uas_metrics <- foreach (
  uas = uas_files,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'raster')
) %dopar% {
  
  source('R/uas_metric_function.R')
  
  c <- str_extract(uas, '(?<=_c)[:digit:]+')
  p <- str_extract(uas, '(?<=_p)[:digit:]+')
  
  uas_metrics <- readLAS(uas) %>%
    cloud_metrics(
      ~ uas_cld_metrics(
        z = Z,
        r = red,
        g = green,
        b = blue,
        re = red_edge,
        nir = near_ir,
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

write_csv(uas_metrics, output_file)
  