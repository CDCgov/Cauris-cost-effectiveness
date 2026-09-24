# Analysis folder

Generally, this folder contains R scripts that are used in data processing. This include manipulating data that was ultimately used a inputs into the model or calibration, as well as the scripts to actually run the model in HPC and process the model output and make figures.

## model_runs
This folder has an individual R script that runs a set of model scenarios (e.g., LTACH IPC50) including the baseline models and all sensitivity analyses.

## CE_analysis
All the cost-effectiveness and incremental net benefit analyses and figures.

## Figures
Scripts to create all the figures used in the manuscript (excluding cost-effectiveness figures).

## LTACH_IPC50_70_outcome_analysis.R
This file runs the statistical analyses for the cumulative colonization cases for each transmission rate and importation rate combination to evaluate the differences between each intervention, the IPC efficacy level, and their interaction. Produces the files in the `data_output/stat_tests`.
This file also produces the `cumulative_case_totals_clean.csv` and `colonization_RRR_clean.csv` files.
