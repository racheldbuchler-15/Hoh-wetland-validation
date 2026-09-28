# =============================================================
# 04_indicator_probability_patterns.R
# Descriptive Analysis: How Model Probabilities Track Indicator
# Presence/Absence (no thresholding, no regression - purely descriptive)
# Author: Rachel Buchler
# Date: 2026
# =============================================================

# --- Load Libraries ------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# --- Source Config -------------------------------------------
source("E:\\Hoh_wetland_mapping_project\\Hoh_wetland_model_val\\scripts\\00_config.R")

# --- Load Combined Field Dataset -------------------------------
# one row per plot, ground-truth indicator columns + one probability
# column per model
df <- read.csv(file.path(tables_dir, "delineation_all_models.csv"),
                stringsAsFactors = FALSE)

# --- Define Model Columns --------------------------------------
model_cols <- c(
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_s1_s2_tpi50_twi",
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_tpi50_twi",
  "prob_NWI",
  "prob_hydric_covariates",
  "prob_hydric_covariates_SOLUS",
  "hoh_pred_all",
  "joint_prob_times_rescaled"
)

# --- Model Display Labels ---------------------------------------
# edit the right-hand side to change plot titles / file-friendly
# labels. File names on disk still use the raw column name, so
# nothing breaks if you change these. (same labels as 02 and 03 -
# keep them in sync if you edit)
model_labels <- c(
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_s1_s2_tpi50_twi" = "Tw WIP (topo + Sentinel 1/2)",
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_tpi50_twi"       = "TW WIP (topo only)",
  "prob_NWI"                                                                = "WIP QC NWI",
  "prob_hydric_covariates"                                                 = "Hydric Soil Model",
  "prob_hydric_covariates_SOLUS"                                           = "Hydric Soil Model (SOLUS)",
  "hoh_pred_all"                                                           = "HIP (Hydrophytic Plant Model)",
  "joint_prob_times_rescaled"                                              = "Joint Probability"
)

# --- Combination Colors -------------------------------------------
# one color per exact Hy/Soil/Veg combination, used to color
# individual points in the indicator-count plots below, so within
# a given count (e.g. count = 1) you can see WHICH indicator that
# point represents, not just how many. Edit hex codes as desired -
# keep 8 entries, names must match the combination strings built
# below (Hy/Soil/Veg each +/-).
combo_colors <- c(
  "Hy+ Soil+ Veg+" = "#09a7f0",
  "Hy+ Soil+ Veg-" = "#e6770f",
  "Hy+ Soil- Veg+" = "#a995da",
  "Hy- Soil+ Veg+" = "#762a83",
  "Hy+ Soil- Veg-" = "#0c47eb",
  "Hy- Soil+ Veg-" = "#5c5606",
  "Hy- Soil- Veg+" = "#75e010",
  "Hy- Soil- Veg-" = "#d6604d"
)

# --- Restrict to Plots with All Three Indicators ----------------
# (this whole script only makes sense on the n=55 subset where
# hydrology, hydric soil, and vegetation are all recorded)
three_indicator_df <- df %>%
  filter(Wetland_Hy %in% c("Yes", "No"),
         Hydric_Soi %in% c("Yes", "No"),
         Hydrophyti %in% c("Yes", "No"))

cat("Plots with all three indicators:", nrow(three_indicator_df), "\n")

# --- Reshape to Long Format (one row per plot x model) -----------
long_df <- three_indicator_df %>%
  select(GlobalID, Wetland_Hy, Hydric_Soi, Hydrophyti, all_of(model_cols)) %>%
  pivot_longer(
    cols = all_of(model_cols),
    names_to = "model",
    values_to = "model_prob"
  ) %>%
  filter(!is.na(model_prob)) %>%
  mutate(
    hydrology_yes = ifelse(Wetland_Hy == "Yes", 1, 0),
    hydric_yes    = ifelse(Hydric_Soi == "Yes", 1, 0),
    veg_yes       = ifelse(Hydrophyti == "Yes", 1, 0),
    indicator_count = hydrology_yes + hydric_yes + veg_yes,
    # readable label for exact combination, e.g. "Hy+ Soil+ Veg-"
    combination = paste0(
      "Hy", ifelse(hydrology_yes == 1, "+", "-"), " ",
      "Soil", ifelse(hydric_yes == 1, "+", "-"), " ",
      "Veg", ifelse(veg_yes == 1, "+", "-")
    )
  )

# =============================================================
# 1. PROBABILITY BY INDICATOR COUNT (0-3), ONE PLOT PER MODEL
# Points colored by exact combination, so within a given count
# you can see WHICH indicator(s) are present, not just how many.
# =============================================================

for(mc in model_cols){

  display_name <- model_labels[[mc]]
  model_long <- long_df %>% filter(model == mc)

  # n per count group, for annotating the plot
  count_n <- model_long %>%
    group_by(indicator_count) %>%
    summarise(n = n(), .groups = "drop")

  count_plot <- ggplot(model_long, aes(x = factor(indicator_count), y = model_prob)) +
    geom_boxplot(fill = "gray90", alpha = 0.5, outlier.shape = NA) +
    geom_jitter(aes(color = combination), width = 0.2, size = 2.5, alpha = 0.85) +
    geom_text(
      data = count_n,
      aes(x = factor(indicator_count), y = 1.06, label = paste0("n=", n)),
      size = 3.5, color = "gray30", inherit.aes = FALSE
    ) +
    scale_color_manual(values = combo_colors, name = "Combination",
                        drop = TRUE) +
    scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
    labs(
      title = paste0("Model Probability by Number of Indicators Present\n", display_name),
      x = "Number of Indicators Present (of 3)",
      y = "Model Probability"
    ) +
    theme_bw(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "right")

  ggsave(
    filename = file.path(figures_dir, paste0(mc, "_probability_by_count.png")),
    plot = count_plot, width = 9, height = 6, dpi = 300
  )
}

cat("Per-model indicator count figures saved\n")

# =============================================================
# 2. PROBABILITY BY EXACT COMBINATION - HEATMAP (mean prob)
# =============================================================

combination_summary <- long_df %>%
  group_by(model, combination) %>%
  summarise(
    mean_prob = mean(model_prob, na.rm = TRUE),
    median_prob = median(model_prob, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# save the underlying table - the heatmap will hide exact numbers
write.csv(combination_summary,
          file.path(tables_dir, "probability_by_combination_summary.csv"),
          row.names = FALSE)

# order combinations from all-present to all-absent for readability
combo_order <- c(
  "Hy+ Soil+ Veg+",
  "Hy+ Soil+ Veg-", "Hy+ Soil- Veg+", "Hy- Soil+ Veg+",
  "Hy+ Soil- Veg-", "Hy- Soil+ Veg-", "Hy- Soil- Veg+",
  "Hy- Soil- Veg-"
)

combination_summary <- combination_summary %>%
  mutate(combination = factor(combination, levels = rev(combo_order)))

heatmap_plot <- ggplot(combination_summary,
                        aes(x = model, y = combination, fill = mean_prob)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(mean_prob, 2), "\n(n=", n, ")")),
            size = 3, color = "black") +
  scale_fill_gradient2(
    low = "#2166ac", mid = "#f7f7f7", high = "#d6604d",
    midpoint = 0.5, limits = c(0, 1), name = "Mean\nProbability"
  ) +
  scale_x_discrete(labels = model_labels) +
  labs(
    title = "Mean Model Probability by Indicator Combination",
    subtitle = "Hy = Hydrology, Soil = Hydric Soil, Veg = Hydrophytic Vegetation",
    x = "Model", y = "Indicator Combination"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

ggsave(
  filename = file.path(figures_dir, "probability_by_combination_heatmap.png"),
  plot = heatmap_plot, width = 11, height = 8, dpi = 300
)

cat("Combination heatmap saved\n")
cat("NOTE: check probability_by_combination_summary.csv for cell sizes -\n")
cat("      some combinations may have very few points (n < 5) and should\n")
cat("      be interpreted cautiously or excluded from the heatmap.\n")

# =============================================================
# 3. FALLBACK: PROBABILITY BY SINGLE INDICATOR (Yes/No), ONE
# PLOT PER MODEL (uses full n for each comparison - no splitting
# across the other two indicators - use this if the combination
# heatmap or count plots are too sparse to read)
# =============================================================

single_indicator_long <- long_df %>%
  select(model, model_prob, hydrology_yes, hydric_yes, veg_yes) %>%
  pivot_longer(
    cols = c(hydrology_yes, hydric_yes, veg_yes),
    names_to = "indicator",
    values_to = "present"
  ) %>%
  mutate(
    indicator = recode(indicator,
                        hydrology_yes = "Wetland Hydrology",
                        hydric_yes    = "Hydric Soil",
                        veg_yes       = "Hydrophytic Vegetation"),
    present = factor(present, levels = c(0, 1), labels = c("Absent", "Present"))
  )

for(mc in model_cols){

  display_name <- model_labels[[mc]]
  model_single <- single_indicator_long %>% filter(model == mc)

  single_n <- model_single %>%
    group_by(indicator, present) %>%
    summarise(n = n(), .groups = "drop")

  single_plot <- ggplot(model_single,
                         aes(x = present, y = model_prob, fill = present)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
    geom_text(
      data = single_n,
      aes(x = present, y = 1.06, label = paste0("n=", n)),
      size = 3.2, color = "gray30", inherit.aes = FALSE
    ) +
    facet_wrap(~ indicator, ncol = 3) +
    scale_fill_manual(values = c("Absent" = "#d6604d", "Present" = "#2166ac")) +
    scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
    labs(
      title = paste0("Model Probability by Single Indicator Presence/Absence\n", display_name),
      subtitle = "n=55 (plots with all three indicators recorded)",
      x = NULL, y = "Model Probability"
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      legend.position = "none"
    )

  ggsave(
    filename = file.path(figures_dir, paste0(mc, "_probability_by_single_indicator.png")),
    plot = single_plot, width = 10, height = 5, dpi = 300
  )
}

cat("Per-model single-indicator figures saved\n")

# =============================================================
# Summary table: simple separation stats per model x indicator
# (mean difference in probability between Present vs Absent -
#  a quick, threshold-free "how much does this indicator move
#  the probability" number, without regression)
# =============================================================

separation_table <- single_indicator_long %>%
  group_by(model, indicator, present) %>%
  summarise(mean_prob = mean(model_prob, na.rm = TRUE), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = present, values_from = c(mean_prob, n)) %>%
  mutate(mean_diff = mean_prob_Present - mean_prob_Absent,
         display_name = model_labels[model]) %>%
  arrange(model, desc(mean_diff)) %>%
  relocate(display_name, .after = model)

write.csv(separation_table,
          file.path(tables_dir, "indicator_separation_summary.csv"),
          row.names = FALSE)

cat("Separation summary saved to", tables_dir, "\n")
cat("(mean_diff = mean probability when indicator present minus when absent;\n")
cat(" larger mean_diff = model responds more strongly to that indicator)\n")

cat("\nAll probability-pattern figures and tables complete.\n")
cat("Figures saved to", figures_dir, "\n")
cat("Tables saved to", tables_dir, "\n")
# =============================================================
# 4. HYDRIC SOIL ACROSS ALL MODELS - ONE COMBINED PLOT
# Same idea as the single-indicator plots above, but flipped:
# one chart, all 7 models on the x-axis, so you can directly
# compare which models separate hydric-present from
# hydric-absent most clearly.
# =============================================================

hydric_across_models <- single_indicator_long %>%
  filter(indicator == "Hydric Soil") %>%
  mutate(display_name = model_labels[model])

hydric_n <- hydric_across_models %>%
  group_by(display_name, present) %>%
  summarise(n = n(), .groups = "drop")

hydric_plot <- ggplot(hydric_across_models,
                       aes(x = display_name, y = model_prob, fill = present)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA,
               position = position_dodge(width = 0.75)) +
  geom_point(aes(group = present), position = position_jitterdodge(
    jitter.width = 0.15, dodge.width = 0.75), alpha = 0.4, size = 1.3) +
  geom_text(
    data = hydric_n,
    aes(x = display_name, y = 1.06, label = paste0("n=", n), group = present),
    position = position_dodge(width = 0.75),
    size = 3, color = "gray30", inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c("Absent" = "#d6604d", "Present" = "#2166ac"),
                     name = "Hydric Soil") +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
  coord_flip() +
  labs(
    title = "Model Probability by Hydric Soil Presence, Across All Models",
    subtitle = "points separated by hydric present/ absent",
    x = "Model", y = "Model Probability"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9))

ggsave(
  filename = file.path(figures_dir, "hydric_soil_across_models.png"),
  plot = hydric_plot, width = 10, height = 7, dpi = 300
)

cat("Hydric soil cross-model comparison figure saved\n")

