# =============================================================
# 02_roc_prc_analysis.R
# ROC and Precision-Recall Curve Analysis
# Author: Rachel Buchler
# Date: 2026
# =============================================================

# --- Load Libraries ------------------------------------------
library(pROC)
library(PRROC)
library(ggplot2)
library(dplyr)

# --- Source Config -------------------------------------------
source("/Users/rachelbuchler/Hoh-wetland-validation/scripts/00_config.R")

# --- Load Model Paths ----------------------------------------
model_paths <- readLines(model_paths_file)
model_paths <- model_paths[nchar(trimws(model_paths)) > 0]
model_names <- basename(dirname(model_paths))

# --- Initialize Results Table --------------------------------
# this will store AUC and AUPRC for all models
results_table <- data.frame(
  model    = character(),
  AUC      = numeric(),
  AUPRC    = numeric(),
  stringsAsFactors = FALSE
)

# --- Loop Over Models ----------------------------------------
for(model_name in model_names){
  
  cat("Processing:", model_name, "\n")
  
  # --- Load Extracted Delineation Data ---------------------
  delineation <- read.csv(file.path(tables_dir, 
                                    paste0(model_name, "_delineation.csv")))
  
  # --- Check Data ------------------------------------------
  cat("Total points:", nrow(delineation), "\n")
  cat("Class distribution:\n")
  print(table(delineation$Wetland_De))
  
  # --- Prepare Labels --------------------------------------
  # convert Yes/No to binary 1/0 for ROC analysis
  delineation <- delineation %>%
    mutate(wetland_binary = ifelse(Wetland_De == "Yes", 1, 0))
  
  # remove any rows with missing probability values
  delineation <- delineation %>%
    filter(!is.na(model_prob))
  
  cat("Points after removing NA:", nrow(delineation), "\n")
  
  # --- ROC Analysis ----------------------------------------
  roc_obj <- roc(delineation$wetland_binary, 
                 delineation$model_prob,
                 levels = c(0, 1),
                 direction = "<")
  
  auc_value <- auc(roc_obj)
  cat("AUC:", round(auc_value, 3), "\n")
  
  # --- PRC Analysis ----------------------------------------
  prc_obj <- pr.curve(
    scores.class0 = delineation$model_prob[delineation$wetland_binary == 1],
    scores.class1 = delineation$model_prob[delineation$wetland_binary == 0],
    curve = TRUE
  )
  
  auprc_value <- prc_obj$auc.integral
  cat("AUPRC:", round(auprc_value, 3), "\n")
  
  # --- Plot ROC Curve --------------------------------------
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
      title = paste("ROC Curve -", model_name),
      x = "False Positive Rate (1 - Specificity)",
      y = "True Positive Rate (Sensitivity)"
    ) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # save ROC figure
  ggsave(
    filename = file.path(figures_dir, paste0(model_name, "_ROC.png")),
    plot = roc_plot,
    width = 8, height = 6, dpi = 300
  )
  
  cat("ROC figure saved\n")
  
  # --- Plot PRC Curve --------------------------------------
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
      title = paste("Precision-Recall Curve -", model_name),
      x = "Recall (Sensitivity)",
      y = "Precision"
    ) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # save PRC figure
  ggsave(
    filename = file.path(figures_dir, paste0(model_name, "_PRC.png")),
    plot = prc_plot,
    width = 8, height = 6, dpi = 300
  )
  
  cat("PRC figure saved\n")
  
  # --- Append to Results Table -----------------------------
  results_table <- rbind(results_table, data.frame(
    model = model_name,
    AUC   = round(auc_value, 3),
    AUPRC = round(auprc_value, 3)
  ))
}

# --- Save Results Table --------------------------------------
write.csv(results_table,
          file.path(tables_dir, "model_auc_summary.csv"),
          row.names = FALSE)

cat("Analysis complete\n")
cat("Results saved to", tables_dir, "\n")
cat("Figures saved to", figures_dir, "\n") 

