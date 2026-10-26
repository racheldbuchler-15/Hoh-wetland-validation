# =============================================================
# 03_threshold_selection.R
# Threshold Selection using Youden's J and F-1 Analysis
# Author: Rachel Buchler
# Date: 2026
# =============================================================

# --- Load Libraries ------------------------------------------
library(pROC) #for ROC analysis and J here 
library(ggplot2)
library(dplyr)

# --- Source Config -------------------------------------------
source("E:\\Hoh_wetland_mapping_project\\Hoh_wetland_model_val\\scripts\\00_config.R")


# --- Load Field Dataset -------------------------------
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
  "joint_prob_times_rescaled.tif.", 
  "combined_wip_2023", 
  "combined_wip_nwi", 
  "Hoh_WIP_Final_2855"
)
 
# --- Model Display Labels ---------------------------------------
model_labels <- c(
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_s1_s2_tpi50_twi" = "Tw WIP (topo + Sentinel 1/2)",
  "wip_dtw_grad50.300.1000_plan50.300.1000_prof50.300.1000_tpi50_twi"       = " TW WIP (topo only)",
  "prob_NWI"                                                                = "WIP QC NWI",
  "prob_hydric_covariates"                                                 = "Hydric Soil Model",
  "prob_hydric_covariates_SOLUS"                                           = "Hydric Soil Model (SULUS)",
  "hoh_pred_all"                                                           = "HIP (Hydrophytic Plant Model)",
  "joint_prob_times_rescaled.tif."                                              = "Joint Probability",
  "combined_wip_2023"                                                        = "1m WIP 2023",
  "combined_wip_nwi"                                                        = "1m WIP NWI",
  "Hoh_WIP_Final_2855"                                                      = "4m WIP (Final 2023 Paper)"

)


# --- Initialize Results Table --------------------------------
results_table <- data.frame(
  model    = character(),
  display_name = character(),
  youden_threshold  = numeric(),
  youden_sensitivity = numeric(),
  youden_specificity = numeric(),
  youden_precision  = numeric(),
  youden_recall     = numeric(),
  youden_fbeta      = numeric(),
  maxf_threshold     = numeric(),
  maxf_precision     = numeric(),
  maxf_recall        = numeric(),
  maxf_fbeta         = numeric(),
  ref_threshold     = numeric(),
  ref_precision     = numeric(),
  ref_recall        = numeric(),
  ref_fbeta         = numeric(),
  stringsAsFactors  = FALSE
)

# --- Helper Function: Calculate F-beta -----------------------
 #we are using a beta of 1 here so we are effectively calculating F1 score where P and R are weighted equally. 
 #If we wanted to weight recall more than precision we would set beta > 1. 
 #If we wanted to weight precision more than recall we would set beta < 1, this can be tuned in the config file.
calc_fbeta <- function(predicted_binary, actual_binary, beta){
  TP <- sum(predicted_binary == 1 & actual_binary == 1)
  FP <- sum(predicted_binary == 1 & actual_binary == 0)
  FN <- sum(predicted_binary == 0 & actual_binary == 1)
 
  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall    <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  fbeta     <- ifelse((beta^2 * precision + recall) == 0, 0,
                      (1 + beta^2) * (precision * recall) /
                        (beta^2 * precision + recall))
 
  return(list(precision = precision,
              recall = recall,
              fbeta = fbeta))
}
 
# --- Loop Over Models ----------------------------------------
for(model_name in model_cols){
 
  cat("Processing:", model_name, "\n")
 
  # --- Prepare This Model's Data -----------------------------
  # (overall Wetland_De determination only - this script is not
  #  level-stratified; see 04_indicator_probability_patterns.R
  #  for the hydrology/hydric/veg-specific work)
  model_df <- df %>%
    select(Wetland_De, model_prob = all_of(model_name)) %>%
    filter(Wetland_De %in% c("Yes", "No")) %>%
    filter(!is.na(model_prob)) %>%
    mutate(wetland_binary = ifelse(Wetland_De == "Yes", 1, 0))
 
  cat("Points available:", nrow(model_df), "\n")
 
  if(nrow(model_df) < 5 ||
     sum(model_df$wetland_binary == 1) == 0 ||
     sum(model_df$wetland_binary == 0) == 0){
    cat("WARNING: insufficient data for", model_name, "- skipping\n")
    next
  }
 
  # --- ROC and Youden's J ----------------------------------
  roc_obj <- roc(model_df$wetland_binary,
                 model_df$model_prob,
                 levels = c(0, 1),
                 direction = "<", quiet = TRUE)
 
  best_threshold <- coords(roc_obj, "best",
                           best.method = "youden",
                           ret = c("threshold",
                                   "sensitivity",
                                   "specificity"))
 
  youden_thresh <- best_threshold$threshold[1]  # first, in case of ties
 
  cat("Youden threshold:", round(youden_thresh, 3), "\n")
 
  # --- F-beta Across All Thresholds ------------------------
  thresholds <- seq(0, 1, by = 0.01)
 
  fbeta_values <- sapply(thresholds, function(t){
    predicted <- ifelse(model_df$model_prob >= t, 1, 0)
    calc_fbeta(predicted, model_df$wetland_binary, beta)$fbeta
  })
 
  precision_values <- sapply(thresholds, function(t){
    predicted <- ifelse(model_df$model_prob >= t, 1, 0)
    calc_fbeta(predicted, model_df$wetland_binary, beta)$precision
  })
 
  recall_values <- sapply(thresholds, function(t){
    predicted <- ifelse(model_df$model_prob >= t, 1, 0)
    calc_fbeta(predicted, model_df$wetland_binary, beta)$recall
  })
 
  # find threshold that maximizes F-beta
  best_fbeta_threshold <- thresholds[which.max(fbeta_values)]
  cat("Best F-beta threshold:", round(best_fbeta_threshold, 3), "\n")
 
  # --- Plot F-1 Across Thresholds -----------------------
  fbeta_df <- data.frame(
    threshold = thresholds,
    fbeta     = fbeta_values,
    precision = precision_values,
    recall    = recall_values
  )
 
  fbeta_plot <- ggplot(fbeta_df, aes(x = threshold)) +
    geom_line(aes(y = fbeta, color = "F-beta"), linewidth = 1) +
    geom_line(aes(y = precision, color = "Precision"),
              linewidth = 0.8, linetype = "dashed") +
    geom_line(aes(y = recall, color = "Recall"),
              linewidth = 0.8, linetype = "dashed") +
    geom_vline(xintercept = youden_thresh,
               color = "gray30", linetype = "dotted", linewidth = 0.8) +
    geom_vline(xintercept = best_fbeta_threshold,
               color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
    geom_vline(xintercept = reference_threshold,
               color = "gray60", linetype = "dotted", linewidth = 0.8) +
    annotate("text", x = youden_thresh + 0.02, y = 0.05,
             label = paste0("Youden\n", round(youden_thresh, 3)),
             size = 3.5, color = "gray30", hjust = 0) +
    annotate("text", x = best_fbeta_threshold + 0.02, y = 0.15,
             label = paste0("Max F-beta\n", round(best_fbeta_threshold, 3)),
             size = 3.5, color = "darkgreen", hjust = 0) +
    annotate("text", x = reference_threshold + 0.02, y = 0.25,
             label = paste0("Reference\n", reference_threshold),
             size = 3.5, color = "gray60", hjust = 0) +
    scale_color_manual(values = c("F-beta" = "#d6604d",
                                  "Precision" = "#2166ac",
                                  "Recall" = "#4dac26")) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    labs(
      title = paste("F-beta, Precision and Recall by Threshold -", model_labels[model_name]),
      x = "Threshold",
      y = "Score",
      color = "Metric"
    ) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")
 
  # save figure
  ggsave(
    filename = file.path(figures_dir, paste0(model_name, "_fbeta.png")),
    plot = fbeta_plot,
    width = 10, height = 6, dpi = 300
  )
 
  cat("F-beta figure saved\n")
 
  # --- Calculate Metrics at Youden Threshold ---------------
  youden_pred <- ifelse(model_df$model_prob >= youden_thresh, 1, 0)
  youden_metrics <- calc_fbeta(youden_pred, model_df$wetland_binary, beta)
 
  # --- Calculate Metrics at Reference Threshold ------------
  ref_pred <- ifelse(model_df$model_prob >= reference_threshold, 1, 0)
  ref_metrics <- calc_fbeta(ref_pred, model_df$wetland_binary, beta)
 
   # --- Calculate Metrics at Max F-beta Threshold -----------
  maxf_pred <- ifelse(model_df$model_prob >= best_fbeta_threshold, 1, 0)
  maxf_metrics <- calc_fbeta(maxf_pred, model_df$wetland_binary, beta)

   results_table <- rbind(results_table, data.frame(
    model              = model_name,
    display_name       = model_labels[model_name],
    youden_threshold   = round(youden_thresh, 3),
    youden_sensitivity = round(best_threshold$sensitivity[1], 3),
    youden_specificity = round(best_threshold$specificity[1], 3),
    youden_precision   = round(youden_metrics$precision, 3),
    youden_recall      = round(youden_metrics$recall, 3),
    youden_fbeta       = round(youden_metrics$fbeta, 3),
    maxf_threshold     = best_fbeta_threshold,
    maxf_precision     = round(maxf_metrics$precision, 3),
    maxf_recall        = round(maxf_metrics$recall, 3),
    maxf_fbeta         = round(maxf_metrics$fbeta, 3),
    ref_threshold      = reference_threshold,
    ref_precision      = round(ref_metrics$precision, 3),
    ref_recall         = round(ref_metrics$recall, 3),
    ref_fbeta          = round(ref_metrics$fbeta, 3)
  ))
}
 
# --- Save Results Table --------------------------------------
write.csv(results_table,
          file.path(tables_dir, "threshold_selection_summary.csv"),
          row.names = FALSE)
 
cat("Threshold selection complete\n")
cat("Results saved to", tables_dir, "\n")
cat("Figures saved to", figures_dir, "\n")
 