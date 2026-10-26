# Hoh River Watershed Wetland Model Validation

This repository contains scripts for validating wetland probability models
in the Hoh River Watershed against field delineation data. We use receiver
operating characteristic (ROC) curves and precision-recall (PR) curves to
assess model performance and compare models. In addition to these curves,
we calculate threshold-based metrics to understand how each model balances
different types of error: Youden's J, which balances sensitivity and
specificity, and the F1 score, which balances precision and recall.

## Workflow

### `00_config.R`: configuration

Holds the settings shared by all scripts: input file paths, output
directories, the beta value, and the reference threshold. Models are listed
in a text file of model paths (`model_paths.txt`), so new models can be
added to the analysis by adding a line to that file.

- **`beta`** sets which F-score is calculated. F1 is the F-beta score with
  beta = 1, which weights precision and recall equally. Setting beta > 1
  weights recall more heavily (e.g. beta = 2 gives F2, useful when missing a
  wetland is more costly than a false positive), and beta < 1 weights
  precision more heavily. This analysis uses beta = 1 witch weights them equaly. 
- **`reference_threshold`** is set to 0.5, a common default for
  probabilistic classifiers, and serves as a baseline for comparison.

### `01_extract_values.R`: extract model probabilities

Extracts each model's predicted probability at every validation point using
`terra::extract()`. Validation points are reprojected to match each model
raster's CRS before extraction, and rasters on a 0–100 scale are rescaled
to 0–1. The output is a table for each model containing the field
delineation results and the extracted model probabilities.

### `02_roc_prc_analysis.R`: model comparison

The main script for comparing models. For each model, and for each
indicator level (overall wetland determination, wetland hydrology, hydric
soil, and hydrophytic vegetation), it calculates:

- the ROC curve and its area under the curve (AUC), using `pROC`
- the PR curve and its area under the curve (AUPRC), using `PRROC`
- the optimal threshold by maximizing Youden's J (`pROC::coords()`), and
  the precision, recall, and F1 score at that threshold

Outputs include a summary table (`model_auc_summary.csv`), ROC and PR curve
figures for each model and indicator level, and summary bar charts
comparing AUC, AUPRC, and F1 across models and indicator levels.

### `03_threshold_selection.R`: threshold comparison

Compares three ways of choosing a classification threshold, using the
overall wetland determination:

- the Youden threshold (maximizes sensitivity + specificity)
- the threshold that maximizes the F-score (searched from 0 to 1 in steps
  of 0.01; the step size can be changed for a finer or coarser search)
- the reference threshold set in the config file (0.5)

Outputs include a table (`threshold_selection_summary.csv`) with precision,
recall, and F-score at each threshold for every model, and one figure per
model showing precision, recall, and F-score across all thresholds, with
the three candidate thresholds marked.

### `04_indicator_probability_patterns.R`: exploratory plots

Descriptive plots showing how model probabilities are distributed where
each wetland indicator (hydrology, hydric soil, hydrophytic vegetation) is
present or absent, and across combinations of indicators. Uses only the
plots where all three indicators were recorded. No thresholds or
statistical tests are applied.
