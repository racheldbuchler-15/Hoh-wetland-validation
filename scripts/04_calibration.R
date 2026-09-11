# =============================================================
# 04a_reliability_check.R
# Reliability Diagram Check Before Calibration
# Author: Rachel Buchler
# Date: 2026
# =============================================================

# --- Load Libraries ------------------------------------------
library(ggplot2)
library(dplyr)

# --- Source Config -------------------------------------------
source("/Users/rachelbuchler/Hoh-wetland-validation/scripts/00_config.R")

# --- Load Model Paths ----------------------------------------
model_paths <- readLines(model_paths_file)
model_paths <- model_paths[nchar(trimws(model_paths)) > 0]
model_names <- basename(dirname(model_paths))

# --- Helper Functions ----------------------------------------

# prepare reliability diagram data using 5 bins
prep_reliability <- function(probs, labels, n_bins = 5){
  bins <- cut(probs, breaks = seq(0, 1, length.out = n_bins + 1),
              include.lowest = TRUE)
  data.frame(probs, labels, bins) %>%
    group_by(bins) %>%
    summarise(
      mean_predicted = mean(probs, na.rm = TRUE),
      fraction_wet   = mean(labels, na.rm = TRUE),
      n              = n(),
      .groups        = "drop"
    )
}

# plot reliability diagram
plot_reliability <- function(rel_data, title){
  ggplot(rel_data, aes(x = mean_predicted, y = fraction_wet)) +
    geom_abline(linetype = "dashed", color = "gray50") +
    geom_line(color = "#2166ac") +
    geom_point(aes(size = n), color = "#2166ac") +
    scale_size_continuous(name = "n points") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    annotate("text", x = 0.75, y = 0.05,
             label = paste("Total n =", sum(rel_data$n, na.rm = TRUE)),
             size = 4, color = "gray30") +
    labs(
      title = title,
      x = "Mean Predicted Probability",
      y = "Observed Fraction Wetland"
    ) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5))
}

# --- Loop Over Models ----------------------------------------
for(model_name in model_names){
  
  cat("Processing:", model_name, "\n")
  
  # --- Load Extracted Delineation Data ---------------------
  csv_path <- file.path(tables_dir,
                        paste0(model_name, "_delineation.csv"))
  
  # check file exists before trying to load
  if(!file.exists(csv_path)){
    cat("WARNING: No delineation CSV found for", model_name, "- skipping\n")
    next
  }
  
  delineation <- read.csv(csv_path)
  
  # convert to binary and remove NAs
  delineation <- delineation %>%
    mutate(wetland_binary = ifelse(Wetland_De == "Yes", 1, 0)) %>%
    filter(!is.na(model_prob))
  
  cat("Points available:", nrow(delineation), "\n")
  cat("Wet points:", sum(delineation$wetland_binary), "\n")
  cat("Dry points:", sum(delineation$wetland_binary == 0), "\n")
  
  # --- Raw Probability Reliability Diagram -----------------
  rel_raw <- prep_reliability(delineation$model_prob,
                              delineation$wetland_binary)
  
  # print bin summary to console for quick review
  cat("Bin summary:\n")
  print(rel_raw)
  
  plot_raw <- plot_reliability(rel_raw,
                               paste("Reliability Diagram\n", model_name))
  
  ggsave(
    filename = file.path(figures_dir,
                         paste0(model_name, "_reliability.png")),
    plot = plot_raw,
    width = 8, height = 6, dpi = 300
  )
  
  cat("Reliability diagram saved for", model_name, "\n\n")
}

cat("All reliability diagrams complete\n")
cat("Review figures in", figures_dir, "\n")
cat("If calibration looks needed run 04b_calibration.R\n")