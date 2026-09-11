# =============================================================
# 00_config.R
# Wetland Model Validation Configuration File
# Author: Rachel Buchler
# Date: 2026
# =============================================================

# --- Paths ---------------------------------------------------

# model paths file
model_paths_file <- "/Users/rachelbuchler/Hoh-wetland-validation/data/model_paths.txt"

# validation data paths
delineation_path <- "/Users/rachelbuchler/Hoh-wetland-validation/data/hoh_validation_points/Hoh Reservation Wetland Points.shp"

# AOI path
aoi_path <- "/Users/rachelbuchler/Shared_data/Vector/hydrography/NHD_WA_HUC8/hoh_huc8_17100101.shp"

# output paths
output_dir <- "/Users/rachelbuchler/Hoh-wetland-validation/outputs/"
figures_dir <- "/Users/rachelbuchler/Hoh-wetland-validation/outputs/figures/"
tables_dir <- "/Users/rachelbuchler/Hoh-wetland-validation/outputs/tables/"
rasters_dir <- "/Users/rachelbuchler/Hoh-wetland-validation/outputs/classified_rasters/"

# --- Model Parameters ----------------------------------------

# f-1 value (we can also try F beta here todo this just change beta to 2) 
# set greater than 1 to weight recall over precision
# 1 = F1 score (equal weight to precision and recall)
beta <- 1

# reference threshold for comparison
reference_threshold <- 0.5

# --- Validation Parameters -----------------------------------

# wetland indicator statuses to include from plant data
# OBL = obligate wetland, FACW = facultative wetland
wetland_indicators <- c("OBL", "FACW")

# minimum model probability to flag as potential omission error
# in hydric soil check
omission_flag_threshold <- 0.3

# --- Projection ----------------------------------------------

# target CRS for all spatial data
# this should matches model raster projection its faster project a shp then a tif
target_crs <- "EPSG:2855"  

# =============================================================