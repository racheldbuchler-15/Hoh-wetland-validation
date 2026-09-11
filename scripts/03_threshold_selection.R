# =============================================================
# 03_threshold_selection.R
# Threshold Selection using Youden's J and F-1 Analysis
# Author: Rachel Buchler
# Date: 2026
# =============================================================

# --- Load Libraries ------------------------------------------
library(pROC)
library(ggplot2)
library(dplyr)

# --- Source Config -------------------------------------------
source("/Users/rachelbuchler/Hoh-wetland-validation/scripts/00_config.R")

# --- Load Model Paths ----------------------------------------
model_paths <- readLines(model_paths_file)
model_paths <- model_paths[nchar(trimws(model_paths)) > 0]
model_names <- basename(dirname(model_paths))

# --- Initialize Results Table --------------------------------
results_table <- data.frame(
  model             = character(),
  youden_threshold  = numeric(),
  youden_sensitivity = numeric(),
  youden_specificity = numeric(),
  youden_precision  = numeric(),
  youden_recall     = numeric(),
  youden_f1      = numeric(),
  ref_threshold     = numeric(),
  ref_precision     = numeric(),
  ref_recall        = numeric(),
  ref_f1         = numeric(),
  stringsAsFactors  = FALSE
)

# --- Helper Function: Calculate F-1 -----------------------
calc_f1 <- function(predicted_binary, actual_binary, beta){
  TP <- sum(predicted_binary == 1 & actual_binary == 1)
  FP <- sum(predicted_binary == 1 & actual_binary == 0)
  FN <- sum(predicted_binary == 0 & actual_binary == 1)
  
  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall    <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  f1     <- ifelse((beta^2 * precision + recall) == 0, 0,
                      (1 + beta^2) * (precision * recall) / 
                        (beta^2 * precision + recall))
  
  return(list(precision = precision, 
              recall = recall, 
              f1 = f1))
}

# --- Loop Over Models ----------------------------------------
for(model_name in model_names){
  
  cat("Processing:", model_name, "\n")
  
  # --- Load Data -------------------------------------------
  delineation <- read.csv(file.path(tables_dir,
                                    paste0(model_name, "_delineation.csv")))
  
  # convert to binary
  delineation <- delineation %>%
    mutate(wetland_binary = ifelse(Wetland_De == "Yes", 1, 0)) %>%
    filter(!is.na(model_prob))
  
  # --- ROC and Youden's J ----------------------------------
  roc_obj <- roc(delineation$wetland_binary,
                 delineation$model_prob,
                 levels = c(0, 1),
                 direction = "<")
  
  best_threshold <- coords(roc_obj, "best",
                           best.method = "youden",
                           ret = c("threshold",
                                   "sensitivity",
                                   "specificity"))
  
  youden_thresh <- best_threshold$threshold
  
  cat("Youden threshold:", round(youden_thresh, 3), "\n")
  
  # --- F-1 Across All Thresholds ------------------------
  thresholds <- seq(0, 1, by = 0.01)
  
  f1_values <- sapply(thresholds, function(t){
    predicted <- ifelse(delineation$model_prob >= t, 1, 0)
    calc_f1(predicted, delineation$wetland_binary, beta)$f1
  })
  
  precision_values <- sapply(thresholds, function(t){
    predicted <- ifelse(delineation$model_prob >= t, 1, 0)
    calc_f1(predicted, delineation$wetland_binary, beta)$precision
  })
  
  recall_values <- sapply(thresholds, function(t){
    predicted <- ifelse(delineation$model_prob >= t, 1, 0)
    calc_f1(predicted, delineation$wetland_binary, beta)$recall
  })
  
  # find threshold that maximizes F-1
  best_f1_threshold <- thresholds[which.max(f1_values)]
  cat("Best F-1 threshold:", round(best_f1_threshold, 3), "\n")
  
  # --- Plot F-1 Across Thresholds -----------------------
  f1_df <- data.frame(
    threshold = thresholds,
    f1     = f1_values,
    precision = precision_values,
    recall    = recall_values
  )
  
  f1_plot <- ggplot(f1_df, aes(x = threshold)) +
    geom_line(aes(y = f1, color = "F-1"), linewidth = 1) +
    geom_line(aes(y = precision, color = "Precision"), 
              linewidth = 0.8, linetype = "dashed") +
    geom_line(aes(y = recall, color = "Recall"), 
              linewidth = 0.8, linetype = "dashed") +
    geom_vline(xintercept = youden_thresh, 
               color = "gray30", linetype = "dotted", linewidth = 0.8) +
    geom_vline(xintercept = best_f1_threshold,
               color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
    geom_vline(xintercept = reference_threshold,
               color = "gray60", linetype = "dotted", linewidth = 0.8) +
    annotate("text", x = youden_thresh + 0.02, y = 0.05,
             label = paste0("Youden\n", round(youden_thresh, 3)),
             size = 3.5, color = "gray30", hjust = 0) +
    annotate("text", x = best_f1_threshold + 0.02, y = 0.15,
             label = paste0("Max F-1\n", round(best_f1_threshold, 3)),
             size = 3.5, color = "darkgreen", hjust = 0) +
    annotate("text", x = reference_threshold + 0.02, y = 0.25,
             label = paste0("Reference\n", reference_threshold),
             size = 3.5, color = "gray60", hjust = 0) +
    scale_color_manual(values = c("F-1" = "#d6604d",
                                  "Precision" = "#2166ac",
                                  "Recall" = "#4dac26")) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    labs(
      title = paste("F-1, Precision and Recall by Threshold -", model_name),
      x = "Threshold",
      y = "Score",
      color = "Metric"
    ) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")
  
  # save figure
  ggsave(
    filename = file.path(figures_dir, paste0(model_name, "_f1.png")),
    plot = f1_plot,
    width = 10, height = 6, dpi = 300
  )
  
  cat("F-1 figure saved\n")
  
  # --- Calculate Metrics at Youden Threshold ---------------
  youden_pred <- ifelse(delineation$model_prob >= youden_thresh, 1, 0)
  youden_metrics <- calc_f1(youden_pred, delineation$wetland_binary, beta)
  
  # --- Calculate Metrics at Reference Threshold ------------
  ref_pred <- ifelse(delineation$model_prob >= reference_threshold, 1, 0)
  ref_metrics <- calc_f1(ref_pred, delineation$wetland_binary, beta)
  
  # --- Append to Results Table -----------------------------
  results_table <- rbind(results_table, data.frame(
    model              = model_name,
    youden_threshold   = round(youden_thresh, 3),
    youden_sensitivity = round(best_threshold$sensitivity, 3),
    youden_specificity = round(best_threshold$specificity, 3),
    youden_precision   = round(youden_metrics$precision, 3),
    youden_recall      = round(youden_metrics$recall, 3),
    youden_f1       = round(youden_metrics$f1, 3),
    ref_threshold      = reference_threshold,
    ref_precision      = round(ref_metrics$precision, 3),
    ref_recall         = round(ref_metrics$recall, 3),
    ref_f1          = round(ref_metrics$f1, 3)
  ))
}

# --- Save Results Table --------------------------------------
write.csv(results_table,
          file.path(tables_dir, "threshold_selection_summary.csv"),
          row.names = FALSE)

cat("Threshold selection complete\n")
cat("Results saved to", tables_dir, "\n")
cat("Figures saved to", figures_dir, "\n") 

