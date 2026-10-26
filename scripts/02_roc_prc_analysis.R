# =============================================================
# 02_roc_prc_analysis.R
# ROC and Precision-Recall Curve Analysis
# Author: Rachel Buchler
# Date: 2026
# =============================================================

# --- Load Libraries ------------------------------------------
#install.packages
library(pROC)#for ROC and AUC calculations
library(PRROC)#for RRC and AUPRC calculations
library(ggplot2)
library(dplyr)
library(tidyr)

# --- Source Config -------------------------------------------
source("E:\\Hoh_wetland_mapping_project\\Hoh_wetland_model_val\\scripts\\00_config.R")

# --- Load CSV from 01 with Model values and delineation data --------------------------------------
df <- read.csv(file.path(tables_dir, "delineation_all_models.csv"),
               stringsAsFactors = FALSE)

# --- Define Model Columns --------------------------------------
#not sure why joint prob has a trailing period in the file name but this seems to work for now 
model_cols <- c(
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_s1_s2_tpi50_twi",
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_tpi50_twi",
  "prob_NWI",
  "prob_hydric_covariates",
  "prob_hydric_covariates_SOLUS",
  "hoh_pred_all",
  "joint_prob_times_rescaled.tif.", 
  "combined_wip_2023",
   "combined_wip_nwi", 
   "Hoh_WIP_Final_2855"
 

)

# --- Model Display Labels ---------------------------------------
model_labels <- c(
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_s1_s2_tpi50_twi" = "4m TW WIP (topo + Sentinel 1/2)",
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_tpi50_twi"       = "4m TW WIP (topo only)",
  "prob_NWI"                                                                = "4m WIP QC NWI",
  "prob_hydric_covariates"                                                 = "4m Hydric Soil Model",
  "prob_hydric_covariates_SOLUS"                                           = "Hydric Soil Model (SOLUS)",
  "hoh_pred_all"                                                           = "10m HIP (Hydrophytic Plant Model)",
  "joint_prob_times_rescaled.tif."                                              = "Joint Probability",
  "combined_wip_2023"                                                        = "1m WIP QC NWI",
  "combined_wip_nwi"                                                        = "1m WIP NWI", 
  "Hoh_WIP_Final_2855"                                                      = "4m WIP (Final 2023 Paper)"
)

# --- Define Indicator Levels -------------------------------------
# each level = which ground-truth column to validate against, adding labels 
levels_list <- list(
  overall   = list(truth_col = "Wetland_De",  label = "Overall Wetland Determination"),
  hydrology = list(truth_col = "Wetland_Hy",  label = "Wetland Hydrology"),
  hydric    = list(truth_col = "Hydric_Soi",  label = "Hydric Soil"),
  veg       = list(truth_col = "Hydrophyti",  label = "Hydrophytic Vegetation")
)

# --- Level Colors for Summary Bar Charts -------------------------
#I might want to ajust these colors later to make things more colorblind friendly, but for now these work. 
level_colors <- c(
  "Overall Wetland Determination" = "#cb86eb",
  "Wetland Hydrology"             = "#2c7ccc",
  "Hydric Soil"                   = "#5c1c12bb",
  "Hydrophytic Vegetation"        = "#4a970b"
)

# --- Helper Function: Calculate F-beta --------------------------------
# F1 = fbeta with beta = 1 also used for threshold selection in 03
calc_fbeta <- function(predicted_binary, actual_binary, beta){
  TP <- sum(predicted_binary == 1 & actual_binary == 1)
  FP <- sum(predicted_binary == 1 & actual_binary == 0)
  FN <- sum(predicted_binary == 0 & actual_binary == 1)

  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall    <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  fbeta     <- ifelse((beta^2 * precision + recall) == 0, 0,
                      (1 + beta^2) * (precision * recall) /
                        (beta^2 * precision + recall))

  return(list(precision = precision, recall = recall, fbeta = fbeta))
}

# --- Helper Function: Prep subset for one model x one level -----------
#this fucntion helps to subset the data for a given model and indicator level, and prepares it for ROC/PRC analysis.
prep_level_subset <- function(df, truth_col, prob_col){
  df %>%
    select(truth = all_of(truth_col), prob = all_of(prob_col)) %>%
    filter(truth %in% c("Yes", "No")) %>%
    filter(!is.na(prob)) %>%
    mutate(binary = ifelse(truth == "Yes", 1, 0))
}

# --- Initialize Results Table ------------------------------------------
results_table <- data.frame(
  model            = character(),
  display_name     = character(),
  level            = character(),
  n                = integer(),
  n_wet            = integer(),
  n_dry            = integer(),
  AUC              = numeric(),
  AUPRC            = numeric(),
  youden_threshold = numeric(),
  precision        = numeric(),
  recall           = numeric(),
  F1               = numeric(),
  stringsAsFactors = FALSE
)

# --- Loop Over Levels x Models ------------------------------------------
for(lvl_name in names(levels_list)){

  lvl <- levels_list[[lvl_name]]
  cat("=== Level:", lvl$label, "===\n")

  for(model_name in model_cols){

    display_name <- model_labels[[model_name]]

    cat("  Model:", model_name, "\n")

    d <- prep_level_subset(df, lvl$truth_col, model_name)

    n_total <- nrow(d)
    n_wet   <- sum(d$binary == 1)
    n_dry   <- sum(d$binary == 0)

    cat("    n =", n_total, " (wet =", n_wet, ", dry =", n_dry, ")\n")

    # safe guard (all one class, or too few points, so the loop does not crash)
    if(n_total < 5 || n_wet == 0 || n_dry == 0){
      cat("    WARNING: insufficient data for this model x level - skipping\n")
      results_table <- rbind(results_table, data.frame(
        model = model_name, display_name = display_name, level = lvl$label,
        n = n_total, n_wet = n_wet, n_dry = n_dry,
        AUC = NA, AUPRC = NA, youden_threshold = NA,
        precision = NA, recall = NA, F1 = NA
      ))
      next
    }

    # --- ROC / AUC ---
    roc_obj <- roc(d$binary, d$prob, levels = c(0, 1), direction = "<", quiet = TRUE)
    auc_value <- as.numeric(auc(roc_obj))
    cat("    AUC:", round(auc_value, 3), "\n")

    # --- PRC / AUPRC ---
    prc_obj <- pr.curve(
      scores.class0 = d$prob[d$binary == 1],
      scores.class1 = d$prob[d$binary == 0],
      curve = TRUE
    )
    auprc_value <- prc_obj$auc.integral
    cat("    AUPRC:", round(auprc_value, 3), "\n")

    # --- Youden threshold, then F1 at that threshold ---
    best <- coords(roc_obj, "best", best.method = "youden",
                   ret = c("threshold", "sensitivity", "specificity"),
                   transpose = FALSE)
    youden_thresh <- best$threshold[1]  # take first in case of ties

    pred <- ifelse(d$prob >= youden_thresh, 1, 0)
    fbeta_metrics <- calc_fbeta(pred, d$binary, beta = beta)
    cat("    F1:", round(fbeta_metrics$fbeta, 3), "\n")

    # --- Plot ROC Curve (per model x level) ---
    roc_df <- data.frame(
      specificity = rev(roc_obj$specificities),
      sensitivity = rev(roc_obj$sensitivities)
    )

    roc_plot <- ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
      geom_line(color = "#2166ac", linewidth = 1) +
      geom_abline(linetype = "dashed", color = "gray50") +
      annotate("text", x = 0.75, y = 0.1,
               label = paste0("AUC = ", round(auc_value, 3)),
               size = 5, color = "#2166ac") +
      labs(
        title = paste0("ROC Curve - ", display_name, "\n(", lvl$label, ")"),
        x = "False Positive Rate (1 - Specificity)",
        y = "True Positive Rate (Sensitivity)"
      ) +
      theme_bw(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave(
      filename = file.path(figures_dir, paste0(model_name, "_", lvl_name, "_ROC.png")),
      plot = roc_plot, width = 8, height = 6, dpi = 300
    )

    # --- Plot PRC Curve (per model x level) ---
    prc_df <- data.frame(
      recall    = prc_obj$curve[, 1],
      precision = prc_obj$curve[, 2]
    )

    prc_plot <- ggplot(prc_df, aes(x = recall, y = precision)) +
      geom_line(color = "#d6604d", linewidth = 1) +
      annotate("text", x = 0.75, y = 0.1,
               label = paste0("AUPRC = ", round(auprc_value, 3)),
               size = 5, color = "#d6604d") +
      labs(
        title = paste0("Precision-Recall Curve - ", display_name, "\n(", lvl$label, ")"),
        x = "Recall (Sensitivity)",
        y = "Precision"
      ) +
      theme_bw(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))

    ggsave(
      filename = file.path(figures_dir, paste0(model_name, "_", lvl_name, "_PRC.png")),
      plot = prc_plot, width = 8, height = 6, dpi = 300
    )

    # --- Append to Results Table -----------------------------
    results_table <- rbind(results_table, data.frame(
      model = model_name,
      display_name = display_name,
      level = lvl$label,
      n = n_total, n_wet = n_wet, n_dry = n_dry,
      AUC = round(auc_value, 3),
      AUPRC = round(auprc_value, 3),
      youden_threshold = round(youden_thresh, 3),
      precision = round(fbeta_metrics$precision, 3),
      recall = round(fbeta_metrics$recall, 3),
      F1 = round(fbeta_metrics$fbeta, 3)
    ))
  }
}

# --- Save Results Table --------------------------------------
write.csv(results_table,
          file.path(tables_dir, "model_auc_summary.csv"),
          row.names = FALSE)

cat("Analysis complete\n")
cat("Results saved to", tables_dir, "\n")
cat("Per-model ROC/PRC figures saved to", figures_dir, "\n")

# --- Summary Plot: AUC by Model, Grouped by Level -----------------------
auc_plot <- ggplot(results_table, aes(x = display_name, y = AUC, fill = level)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = level_colors) +
  labs(
    title = "AUC by Model Across Indicator Levels",
    x = "Model", y = "AUC", fill = "Indicator Level"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path(figures_dir, "AUC_by_model_and_level.png"),
       plot = auc_plot, width = 10, height = 6, dpi = 300)

# --- Summary Plot: AUPRC by Model, Grouped by Level ---------------------
auprc_plot <- ggplot(results_table, aes(x = display_name, y = AUPRC, fill = level)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = level_colors) +
  labs(
    title = "AUPRC by Model Across Indicator Levels",
    x = "Model", y = "AUPRC", fill = "Indicator Level"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path(figures_dir, "AUPRC_by_model_and_level.png"),
       plot = auprc_plot, width = 10, height = 6, dpi = 300)

# --- Summary Plot: F1 by Model, Grouped by Level ------------------------
f1_plot <- ggplot(results_table, aes(x = display_name, y = F1, fill = level)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = level_colors) +
  labs(
    title = "F1 (at Youden Threshold) by Model Across Indicator Levels",
    x = "Model", y = "F1", fill = "Indicator Level"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path(figures_dir, "F1_by_model_and_level.png"),
       plot = f1_plot, width = 10, height = 6, dpi = 300)

cat("Summary figures saved to", figures_dir, "\n")

# =============================================================
# AUC PLOT FOR SELECTED MODELS ONLY
# =============================================================

# --- Choose which models to plot ------------------------------
# any subset of model_cols; the order here is the order on the plot from top to bottom 
auc_plot_models <- c(
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_s1_s2_tpi50_twi",
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_tpi50_twi",
  "prob_NWI",
  "combined_wip_2023",
  "combined_wip_nwi", 
  "Hoh_WIP_Final_2855"
)
# --- Choose which indicator levels to show --------------------
# these can also be changed to any subset of the four levels
auc_plot_levels <- c(
  "Overall Wetland Determination",
  "Wetland Hydrology",
  "Hydric Soil",
  "Hydrophytic Vegetation"
)

# catch typos
stopifnot(all(auc_plot_models %in% model_cols))
stopifnot(all(auc_plot_levels %in% names(level_colors)))

auc_selected <- results_table %>%
  filter(model %in% auc_plot_models,
         level %in% auc_plot_levels) %>%
  mutate(
    # rev() because coord_flip puts the first level at the bottom
    display_name = factor(display_name,
                          levels = rev(unname(model_labels[auc_plot_models]))),
    level = factor(level, levels = auc_plot_levels)
  )

auc_selected_plot <- ggplot(auc_selected,
                            aes(x = display_name, y = AUC, fill = level)) +
  geom_col(position = position_dodge2(width = 0.8, reverse = TRUE),
           width = 0.7) +
  # AUC = 0.5 is no better than random
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
  coord_flip() +
  scale_fill_manual(values = level_colors) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "AUC by Model Across Indicator Levels",
    x = "Model", y = "AUC", fill = "Indicator Level"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path(figures_dir, "AUC_by_model_and_level_selected.png"),
       plot = auc_selected_plot, width = 10, height = 6, dpi = 300)

cat("Selected-model AUC figure saved to", figures_dir, "\n")