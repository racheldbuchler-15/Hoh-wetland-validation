# =============================================================
# 01_extract_values.R
# Extract model probability values at validation points
# Author: Rachel Buchler
# Date: 2026
# =============================================================
# --- Load Libraries ------------------------------------------
library(terra)
library(sf)
library(dplyr)
library(tidyr)

# --- Source Config -------------------------------------------
source("E:\\Hoh_wetland_mapping_project\\Hoh_wetland_model_val\\scripts\\00_config.R")

# --- Load Model Rasters --------------------------------------
model_paths <- readLines(model_paths_file)
print(model_paths)
length(model_paths)

# --- Load Validation Data ------------------------------------
# delineation points (wet and dry) - primary validation
delineation <- st_read(delineation_path)

# --- Quick Column Checks -------------------------------------
# here we will check column names and metadata
 
colnames(delineation)
 
# confirm expected columns are present
stopifnot("Wetland_De" %in% names(delineation))
stopifnot("Wetland_Hy" %in% names(delineation))
stopifnot("Hydric_Soi" %in% names(delineation))
stopifnot("Hydrophyti" %in% names(delineation))
 
# check class distributions
cat("Delineation classes (overall):\n")
print(table(delineation$Wetland_De))
 
cat("Wetland hydrology classes:\n")
print(table(delineation$Wetland_Hy))
 
cat("Hydric soil classes:\n")
print(table(delineation$Hydric_Soi))
 
cat("Hydrophytic veg classes:\n")
print(table(delineation$Hydrophyti))
 
#View(delineation)

 
# --- Extract Raster Values -----------------------------------
extracted_values <- lapply(model_paths, function(path) {
  
  model_raster <- terra::rast(path)
  model_name <- tools::file_path_sans_ext(basename(path))
  
  cat("Processing:", model_name, "\n")
  
  # --- Check and Rescale to 0-1 if needed -------------------
  raster_range <- minmax(model_raster, compute = TRUE)
  cat("Raster range:", raster_range[1], "-", raster_range[2], "\n")
  
  if(raster_range[2] > 1.01){
    cat("Rescaling raster from 0-100 to 0-1\n")
    model_raster <- model_raster / 100
  }
  
  # reproject validation data to match raster CRS
  delineation_proj <- st_transform(delineation, crs(model_raster))
  
  # extract probability values
  delineation_proj$model_prob <- terra::extract(model_raster, delineation_proj)[, 2]

  # add model name column
  delineation_proj$model <- model_name
  # drop geometry for saving
  delineation_out <- st_drop_geometry(delineation_proj)
  delineation_out
})
 
# name list elements by model
names(extracted_values) <- tools::file_path_sans_ext(basename(model_paths))

# --- Combine All Models into One Wide Format Table -------------

combined <- do.call(rbind, extracted_values)

wide_df <- combined %>%
  select(GlobalID, Wetland_Hy, Hydric_Soi, Hydrophyti, Wetland_De, model, model_prob) %>%
  tidyr::pivot_wider(names_from = model, values_from = model_prob)

# --- Save --------------------------------------------------------
saveRDS(extracted_values,
        file.path(output_dir, "extracted_values.RDS"))

write.csv(wide_df,
          file.path(tables_dir, "delineation_all_models.csv"),
          row.names = FALSE)

cat("Extraction complete. Combined CSV saved to", tables_dir, "\n")