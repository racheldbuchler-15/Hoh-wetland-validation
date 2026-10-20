# =============================================================
# 1m_data_prep.R
# Wetland Model Validation Data Preparation Script
# Author: Rachel Buchler
# Date: 2026
# =============================================================

library(terra)

wip_2023_files <- list.files(
  path       = "E:/TealWaters_WIP/HOH_WIP/Hoh_1m/wip_2023",
  pattern    = "\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)

wip_nwi_files <- list.files(
  path       = "E:/TealWaters_WIP/HOH_WIP/Hoh_1m/wip_nwi",
  pattern    = "\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)

length(wip_2023_files)
length(wip_nwi_files)

raster_list1 <- lapply(wip_2023_files, terra::rast)
raster_list2 <- lapply(wip_nwi_files, terra::rast)

##now I want to check the range of values in each raster, and see what the data types is 

unique(sapply(raster_list1, terra::datatype))
unique(sapply(raster_list2, terra::datatype)) 


r <- raster_list1[[1]]
terra::minmax(r, compute = TRUE)

samp <- terra::spatSample(r, size = 10000, na.rm = TRUE)[, 1]
summary(samp)
head(samp, 20)
all(samp == round(samp))


##using mosic here to combine them 

final_raster_2023 <- terra::mosaic(terra::sprc(raster_list1), fun = "mean",
                                   filename = "E:/TealWaters_WIP/HOH_WIP/Hoh_1m/combined_wip_2023.tif",
                                   overwrite = TRUE,
                                   wopt = list(datatype = "INT1U",
                                               gdal = c("COMPRESS=DEFLATE", "BIGTIFF=IF_SAFER")))
final_raster_nwi <- terra::mosaic(terra::sprc(raster_list2), fun = "mean",
                                   filename = "E:/TealWaters_WIP/HOH_WIP/Hoh_1m/combined_wip_nwi.tif",
                                   overwrite = TRUE,
                                   wopt = list(datatype = "INT1U",
                                               gdal = c("COMPRESS=DEFLATE", "BIGTIFF=IF_SAFER")))