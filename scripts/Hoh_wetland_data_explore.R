
# load libraries 
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
 
# Source Config
source("E:\\Hoh_wetland_mapping_project\\Hoh_wetland_model_val\\scripts\\00_config.R")

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

   
# =============================================================
# QUESTION: Does hydric soil or veg presence imply hydrology
# presence? (i.e. is hydrology a near-universal precondition?)
# =============================================================

three_indicator <- delineation %>%
  st_drop_geometry() %>%
  filter(Wetland_Hy %in% c("Yes", "No"),
         Hydric_Soi %in% c("Yes", "No"),
         Hydrophyti %in% c("Yes", "No"))
 
cat("Plots with all three indicators recorded:", nrow(three_indicator), "\n\n")
 
# --- Among hydric-positive plots, what is hydrology status? -----
hydric_yes <- three_indicator %>% filter(Hydric_Soi == "Yes")
hydric_yes_hydrology_table <- table(hydric_yes$Wetland_Hy)
 
cat("Among", nrow(hydric_yes), "plots with Hydric Soil = Yes:\n")
print(hydric_yes_hydrology_table)
cat("  ->", hydric_yes_hydrology_table["No"] %>% ifelse(is.na(.), 0, .),
    "have Hydrology = No (violates 'hydrology always present' assumption)\n\n")
 
# --- Among veg-positive plots, what is hydrology status? --------
veg_yes <- three_indicator %>% filter(Hydrophyti == "Yes")
veg_yes_hydrology_table <- table(veg_yes$Wetland_Hy)
 
cat("Among", nrow(veg_yes), "plots with Hydrophytic Veg = Yes:\n")
print(veg_yes_hydrology_table)
cat("  ->", veg_yes_hydrology_table["No"] %>% ifelse(is.na(.), 0, .),
    "have Hydrology = No (violates 'hydrology always present' assumption)\n\n")
 
# --- Full three-way contingency table ----------------------------
cat("Full three-way contingency table (Hydrology x Hydric x Veg):\n")
full_table <- table(
  Hydrology = three_indicator$Wetland_Hy,
  Hydric    = three_indicator$Hydric_Soi,
  Veg       = three_indicator$Hydrophyti
)
print(full_table)
 
# save as a flat data frame for reference
contingency_df <- as.data.frame(full_table)
write.csv(contingency_df,
          file.path(tables_dir, "indicator_contingency_table.csv"),
          row.names = FALSE)
 
# =============================================================
# VISUALIZATION 1: Grouped bar chart - hydrology status among
# hydric-positive and veg-positive plots
# =============================================================
 
bar_data <- bind_rows(
  hydric_yes %>%
    mutate(indicator_group = "Hydric Soil = Yes") %>%
    select(indicator_group, Wetland_Hy),
  veg_yes %>%
    mutate(indicator_group = "Hydrophytic Veg = Yes") %>%
    select(indicator_group, Wetland_Hy)
) %>%
  count(indicator_group, Wetland_Hy)
 
precondition_plot <- ggplot(bar_data, aes(x = indicator_group, y = n, fill = Wetland_Hy)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("Yes" = "#2166ac", "No" = "#d6604d"),
                     name = "Wetland\nHydrology") +
  labs(
    title = "Is Wetland Hydrology Present When Hydric Soil or Veg Are Present?",
    x = NULL, y = "Number of Plots"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))
 
ggsave(file.path(figures_dir, "hydrology_precondition_check.png"),
       plot = precondition_plot, width = 8, height = 6, dpi = 300)
 
cat("\nPrecondition check figure saved\n")
 
# =============================================================
# VISUALIZATION 2: Counts across all 8 combinations (bar chart,
# analogous to the probability heatmap in 04, but showing raw
# plot counts instead of model probability)
# =============================================================
 
combo_counts <- three_indicator %>%
  mutate(
    combination = paste0(
      "Hy", ifelse(Wetland_Hy == "Yes", "+", "-"), " ",
      "Soil", ifelse(Hydric_Soi == "Yes", "+", "-"), " ",
      "Veg", ifelse(Hydrophyti == "Yes", "+", "-")
    )
  ) %>%
  count(combination) %>%
  arrange(desc(n))
 
combo_plot <- ggplot(combo_counts, aes(x = reorder(combination, n), y = n)) +
  geom_col(fill = "#4393c3", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.2, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Plot Counts by Indicator Combination",
    subtitle = "Hy = Hydrology, Soil = Hydric Soil, Veg = Hydrophytic Vegetation",
    x = "Combination", y = "Number of Plots"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10))
 
ggsave(file.path(figures_dir, "indicator_combination_counts.png"),
       plot = combo_plot, width = 9, height = 6, dpi = 300)
 
cat("\n plots saved!\n")
  
# --- Hydrology count among the 55 points with complete indicator data -----
complete_indicators <- delineation %>%
  filter(!is.na(Wetland_Hy) & !is.na(Hydric_Soi) & !is.na(Hydrophyti))

cat("Points with all three indicators present:", nrow(complete_indicators), "\n")
cat("Hydrology breakdown among those points:\n")
print(table(complete_indicators$Wetland_Hy))
sum(complete_indicators$Wetland_De == "Yes", na.rm = TRUE)
