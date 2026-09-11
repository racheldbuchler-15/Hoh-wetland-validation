library(dplyr)

delineation <- st_read(delineation_path)

# total points and wet/dry breakdown
total_points <- nrow(delineation)
wet_count <- sum(delineation$Wetland_De == "Yes", na.rm = TRUE)
dry_count <- sum(delineation$Wetland_De == "No", na.rm = TRUE)

cat("Total points:", total_points, "\n")
cat("Wetland (Yes):", wet_count, "(", round(100 * wet_count / total_points, 1), "%)\n")
cat("Not wetland (No):", dry_count, "(", round(100 * dry_count / total_points, 1), "%)\n")

# points with Yes across all three wetland parameters
all_three_yes <- delineation %>%
  filter(Hydric_Soi == "Yes", Wetland_Hy == "Yes", Hydrophyti == "Yes") %>%
  nrow()

cat("Points with Yes on all three parameters (hydric soil, hydrology, hydrophytic veg):",
    all_three_yes, "(", round(100 * all_three_yes / total_points, 1), "%)\n")