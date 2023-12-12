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

# ============================= Point cloud metrics ============================

las_files <- list.files(
  path = las_folder,
  pattern = '.las$',
  recursive = TRUE,
  full.names = TRUE
)

tls_files <- str_subset(las_files, 'tls')
zeb_files <- str_subset(las_files, 'zeb')
uas_files <- str_subset(las_files, 'uas')

tls_files <- tls_files[2]
uas_files <- uas_files[2]
zeb_files <- zeb_files[2]

uas <- readLAS(uas_files, filter = '-keep_random_fraction 0.5')
zeb <- readLAS(zeb_files, filter = '-keep_random_fraction 0.05')
tls <- readLAS(tls_files, filter = '-keep_random_fraction 0.01')

x = plot(uas, pal = 'red')
plot(tls, pal = 'grey', add = x)

plot(tls, pal = 'grey')

  