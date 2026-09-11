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

# --- Source Config -------------------------------------------
source("/Users/rachelbuchler/Hoh-wetland-validation/scripts/00_config.R")

# --- Load Model Rasters --------------------------------------
model_paths <- readLines(model_paths_file)
print(model_paths)
length(model_paths)

# --- Load Validation Data ------------------------------------
# delineation points (wet and dry) - primary validation
delineation <- st_read(delineation_path)

# hydric pedons (wet only) - secondary sensitivity check
hydric_pedons <- st_read(hydric_pedons_path)

# plant data (wet and dry) - tertiary sensitivity check
plants <- st_read(plant_data_path)

# --- Quick Column Checks -------------------------------------
# confirm expected columns are present
stopifnot("Wetland_De" %in% names(delineation))
stopifnot("hydric" %in% names(hydric_pedons))
stopifnot("wet_dry" %in% names(plants))
stopifnot("WMVC" %in% names(plants))

# check class distributions
cat("Delineation classes:\n")
print(table(delineation$Wetland_De))

cat("Hydric pedon classes:\n")
print(table(hydric_pedons$hydric))

cat("Plant wet/dry classes:\n")
print(table(plants$wet_dry))

cat("Plant wetland indicators:\n")
print(table(plants$WMVC))

# --- Extract Raster Values -----------------------------------
extracted_values <- lapply(model_paths, function(path) {
  
  model_raster <- rast(path)
  model_name <- basename(dirname(path))
  
  cat("Processing:", model_name, "\n")
  
  # --- Check and Rescale to 0-1 if needed -------------------
  raster_range <- minmax(model_raster, compute = TRUE)
  cat("Raster range:", raster_range[1], "-", raster_range[2], "\n")
  
  if(raster_range[2] > 1){
    cat("Rescaling raster from 0-100 to 0-1\n")
    model_raster <- model_raster / 100
  }
  
  # reproject all validation data to match raster CRS
  delineation_proj <- st_transform(delineation, crs(model_raster))
  hydric_proj <- st_transform(hydric_pedons, crs(model_raster))
  plants_proj <- st_transform(plants, crs(model_raster))
  
  # extract probability values
  delineation_proj$model_prob <- extract(model_raster, delineation_proj)[, 2]
  hydric_proj$model_prob <- extract(model_raster, hydric_proj)[, 2]
  plants_proj$model_prob <- extract(model_raster, plants_proj)[, 2]
  
  # add model name column to all datasets
  delineation_proj$model <- model_name
  hydric_proj$model <- model_name
  plants_proj$model <- model_name
  
  # drop geometry for saving - keeps files small and fast to reload
  delineation_out <- st_drop_geometry(delineation_proj)
  hydric_out <- st_drop_geometry(hydric_proj)
  plants_out <- st_drop_geometry(plants_proj)
  
  # return as named list
  list(
    delineation = delineation_out,
    hydric = hydric_out,
    plants = plants_out
  )
})

# name list elements by model
names(extracted_values) <- basename(dirname(model_paths))

# --- Save Extracted Values -----------------------------------
saveRDS(extracted_values,
        file.path(output_dir, "extracted_values.RDS"))

cat("Extraction complete, results saved to", output_dir, "\n") 

# --- Save Extracted Values as CSV ----------------------------
for(model_name in names(extracted_values)){
  
  cat("Saving outputs for:", model_name, "\n")
  
  # delineation points
  write.csv(extracted_values[[model_name]]$delineation,
            file.path(tables_dir, paste0(model_name, "_delineation.csv")),
            row.names = FALSE)
  
  # hydric pedons
  write.csv(extracted_values[[model_name]]$hydric,
            file.path(tables_dir, paste0(model_name, "_hydric.csv")),
            row.names = FALSE)
  
  # plant data
  write.csv(extracted_values[[model_name]]$plants,
            file.path(tables_dir, paste0(model_name, "_plants.csv")),
            row.names = FALSE)
}

cat("All extractions saved to", tables_dir, "\n")