# ==============================================================================
#
# LAS data fusion plot level point cloud metric extraction
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 12 Dec 2023
# Last commit: 25 Jan  2023
#
# Status: Complete
#
# Created as part of 2023 multisite data comparison study.
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

# n_cluster <- 2

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

# cl <- makeCluster(n_cluster)
# registerDoParallel(cl)
# 
# i = 2
# ldr_i = c(tls_files, hmls_files)[i]

fusion_metrics <- foreach(
  ldr_i = c(tls_files, hmls_files),
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'raster', 'glue', 'doParallel')
) %do% {
  
  source(metric_fn)

  c <- str_extract(ldr_i, '(?<=[:punct:]c)[:digit:]+(?=_)')
  p <- str_extract(ldr_i, '(?<=_p)[:digit:]+')
  ldr_method <- str_extract(ldr_i, '[:alpha:]+(?=_p[:digit:])')
  
  uas_i <- str_subset(uas_files, glue('c{c}_p{p}'))
  
  vox_dim <- c(0.1, 0.25, 0.5, 1, 3.7)
  
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
        lidar_method = ldr_method,
        pnt_cloud = NA,
        metric_type = NA,
        h_thresh = 0.25,
        vox_dim = NA,
        .before = 1
      ) %>%
      add_column(lidar_file = ldr_i,
                 uas_file = NA)
    
    return(fusion_i_metrics)
    
  } 

  # Read in data
  
  uas <- readLAS(uas_i, select = '', filter = '-drop_z_below 0.25')
  ldr <- readLAS(ldr_i, select = '', filter = '-drop_z_below 0.25')
  
  # Full data
  
  full_fusion <- rbind(uas@data, ldr@data) %>%
    LAS() %>%
    filter_poi(Z > 0.25)

  full_pnt_metrics <- full_fusion %>%
    cloud_metrics( ~ las_cld_metrics(z = Z)) %>%
    as_tibble() %>%
    add_column(
      campaign = c,
      plot = p,
      lidar_method = ldr_method,        
      pnt_cloud = 'full_fusion',
      metric_type = 'pntcld',
      h_thresh = 0.25,
      vox_dim = NA,
      .before = 1
    ) %>%
    add_column(lidar_file = ldr_i,
               uas_file = uas_i)
  
  full_vox_metrics <- foreach(
    vox_i = vox_dim,
    .combine = 'rbind'
  ) %do% {
    
    full_vox_metrics <- full_fusion %>%
      voxelize_points(res = vox_i) %>%
      cloud_metrics( ~ las_cld_metrics(z = Z)) %>%
      as_tibble() %>%
      add_column(
        campaign = c,
        plot = p,
        lidar_method = ldr_method,        
        pnt_cloud = 'full_fusion',
        metric_type = 'vox',
        h_thresh = 0.25,
        vox_dim = vox_i,
        .before = 1
      ) %>%
      add_column(lidar_file = ldr_i,
                 uas_file = uas_i)
    
  }

  
  # Decimated data
  
  dec_res = 1
  
  ldr_dens = grid_metrics(ldr, ~ length(Z), res = dec_res) %>%
    raster::cellStats(stat = 'mean')
  
  uas_dens = grid_metrics(uas, ~ length(Z), res = dec_res) %>%
    raster::cellStats(stat = 'mean')
  
  min_dens = min(c(ldr_dens, uas_dens))
  
  ldr_dec <- decimate_points(
    las = ldr, 
    algorithm = homogenize(
      density = min_dens,
      res = dec_res
    ))
  
  uas_dec <- decimate_points(
    las = uas, 
    algorithm = homogenize(
      density = min_dens,
      res = dec_res
    ))
  
  
  dec_fusion <- rbind(uas_dec@data, ldr_dec@data) %>%
    LAS() %>%
    filter_poi(Z > 0.25)
  
  dec_pnt_metrics <- dec_fusion %>%
    cloud_metrics( ~ las_cld_metrics(z = Z)) %>%
    as_tibble() %>%
    add_column(
      campaign = c,
      plot = p,
      lidar_method = ldr_method,        
      pnt_cloud = 'decimated_fusion',
      metric_type = 'pntcld',
      h_thresh = 0.25,
      vox_dim = NA,
      .before = 1
    ) %>%
    add_column(lidar_file = ldr_i,
               uas_file = uas_i)
  
  dec_vox_metrics <- foreach(
    vox_i = vox_dim,
    .combine = 'rbind'
  ) %do% {
    
    dec_vox_metrics <- dec_fusion %>%
      voxelize_points(res = vox_i) %>%
      cloud_metrics( ~ las_cld_metrics(z = Z)) %>%
      as_tibble() %>%
      add_column(
        campaign = c,
        plot = p,
        lidar_method = ldr_method,
        pnt_cloud = 'decimated_fusion',
        metric_type = 'vox',
        h_thresh = 0.25,
        vox_dim = vox_i,
        .before = 1
      ) %>%
      add_column(lidar_file = ldr_i,
                 uas_file = uas_i)
    
  }
  
  fusion_metrics <- full_pnt_metrics %>%
    add_row(full_vox_metrics) %>%
    add_row(dec_pnt_metrics) %>%
    add_row(dec_vox_metrics)
  
}

# stopCluster(cl)


write_csv(fusion_metrics, glue(csv_output))

# ==============================================================================