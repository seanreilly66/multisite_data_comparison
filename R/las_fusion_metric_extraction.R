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
  'data/predictor_df/las_fusion_plot_metrics_{format(Sys.time(), "%Y%m%d%H%M")}.csv'

n_cluster <- 2

# ================================ Identify files ==============================

las_files <- list.files(
  path = las_folder,
  pattern = '.las$',
  recursive = TRUE,
  full.names = TRUE
)

tls_files <- str_subset(las_files, 'tls') %>%
  str_subset('DEMnorm')
hmls_files <- str_subset(las_files, 'zeb') %>%
  str_subset('step4')
uas_files <- str_subset(las_files, 'uas')

# ========================= Fusion point cloud metrics ========================= 

cl <- makeCluster(n_cluster)
registerDoParallel(cl)

# i = 2
# ldr_i = c(tls_files, zeb_files)[i]

fusion_metrics <- foreach(
  ldr_i = c(tls_files, hmls_files),
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'raster', 'glue')
) %dopar% {
  
  source('R/las_metric_function.R')

  c <- str_extract(ldr_i, '(?<=[:punct:]c)[:digit:]+(?=_)')
  p <- str_extract(ldr_i, '(?<=_p)[:digit:]+')
  ldr_method <- str_extract(ldr_i, '[:alpha:]+(?=_p[:digit:])')
  
  uas_i <- str_subset(uas_files, glue('c{c}_p{p}'))
  
  # return NA values if no matching UAS file
  if (length(uas_i) != 1) {
    
    fusion_i_metrics = data.frame(X = -99999, Y = -99999, Z = -99999) %>%
      LAS() %>%
      cloud_metrics( ~ las_cld_metrics(z = Z)) %>%
      as_tibble() %>%
      mutate(across(.cols = everything(), ~ ifelse(T, NA, .))) %>%
      add_column(
        campaign = c,
        plot = p,
        method = ldr_method,
        .before = 1
      ) %>%
      add_column(lidar_file = ldr_i,
                 uas_file = NA)
    
  } else {
    
    uas_data <- readLAS(uas_i, select = '')@data
    ldr_data <- readLAS(ldr_i, select = '')@data
    
    fusion_i_metrics <- rbind(uas_data, ldr_data) %>%
      LAS() %>% 
      cloud_metrics( ~ las_cld_metrics(z = Z)) %>%
      as_tibble() %>%
      add_column(
        campaign = c,
        plot = p,
        method = ldr_method,
        .before = 1
      ) %>%
      add_column(lidar_file = ldr_i,
                 uas_file = uas_i)
    
  }
  
}

stopCluster(cl)


write_csv(las_metrics, glue(csv_output))

# ==============================================================================



    