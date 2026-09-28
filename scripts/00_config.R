# =============================================================
# 00_config.R
# Wetland Model Validation Configuration File
# Author: Rachel Buchler
# Date: 2026
# =============================================================

# --- Paths ---------------------------------------------------

# model paths file
model_paths_file <- "E:\\Hoh_wetland_mapping_project\\Hoh_wetland_model_val\\Data\\model_paths.txt"

# validation data paths
delineation_path <- "E:\\Hoh_wetland_mapping_project\\Data_from_Hoh\\Data_from_Hoh_9_3\\Updated Hoh Wetland Points\\Updated Hoh Wetland Points\\Updated Hoh Wetland Data Points.shp"

# AOI path
aoi_path <- "E:\\Hoh_wetland_mapping_project\\Vector_data_for_mapping\\HOH_2022_aoi\\HOH_2022_AOI\\HOH_2022_AOI.shp"

# output paths
output_dir <- "E:\\Hoh_wetland_mapping_project\\Hoh_wetland_model_val\\outputs"
figures_dir <- "E:\\Hoh_wetland_mapping_project\\Hoh_wetland_model_val\\outputs\\figures"
tables_dir <- "E:\\Hoh_wetland_mapping_project\\Hoh_wetland_model_val\\outputs\\tables"
rasters_dir <- "E:\\Hoh_wetland_mapping_project\\Hoh_wetland_model_val\\outputs\\classified_rasters"

# --- Model Parameters ----------------------------------------

# f-1 value (we can also try F beta here todo this just change beta to 2) 
# set greater than 1 to weight recall over precision
# 1 = F1 score (equal weight to precision and recall)
beta <- 1

# reference threshold for comparison
reference_threshold <- 0.5

# minimum model probability to flag as potential omission error
# in hydric soil check
omission_flag_threshold <- 0.3

# --- Projection ----------------------------------------------

# target CRS for all spatial data
# this should matches model raster projection its faster project a shp then a tif
target_crs <- "EPSG:2855"  

# =============================================================