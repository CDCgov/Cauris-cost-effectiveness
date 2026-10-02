# Running of incremental Net Benefit (INB) analysis for LTACH IPC 70%

Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)

hpc <- FALSE  # set to TRUE when running on an HPC cluster
if(hpc) {
  source(".Rprofile") # if in hpc environment then need to source this to load up renv
  #renv::restore(packages = "renv")
  #renv::diagnostics()
  #renv::activate()
}


# load libraries
library(here)
library(data.table)
library(tidyverse)
library(estimatr)
library(future)
library(purrr)
library(furrr)



# Source INC calculation function
source(here("R/INB_calculation_function.R"))


# load cost effect data for LTACH IPC 50%
cost_effect_data_df <- read.csv(here("data_output/LTACH_IPC70_model_update/cost_effect_data.csv"))

# update interventions to a factor and order so that No Screening is the reference
cost_effect_data_df <- cost_effect_data_df %>%
  mutate(intervention = factor(intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", 
                                                        "Containment Admissions + PPS (14 days) w/ taper", "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", 
                                                        "PPS (30 days)", "PPS (14 days)", "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                        "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")))


# combinations to iterate over
trans <- c(0.0175, 0.0660)
import <- c(0.01, 0.04, 0.1)
cost_type <- c("screen", "total")

# Use expand.grid to create combinations of trans and import values
combinations <- expand.grid(trans=trans, import=import, cost_type = cost_type) %>%
  arrange(cost_type, trans, import)



# Set WTP value ranges for each scenario
combinations <- combinations %>%
  mutate(WTP_lower = c(1000, 400, 300, 0 , 0 , 0, 2000, 2000, 3000, 100, 300, 750),
         WTP_upper = c(11000, 3500, 1600, 500, 500, 400, 14000, 8000, 5000, 750, 1300, 2000))





# Set up parallel processing.
# There are only 12 parameter combinations (2 transmission x 3 importation x 2 cost types),
# so there is no benefit to more than 12 workers. Wall time is bounded by the single longest
# combination rather than by the worker count, so using fewer workers costs little time and
# saves memory - each worker holds its own copy of cost_effect_data_df (~25 MB).
# Expect a run time of about 5 minutes locally on a multi-core laptop (measured at
# 4.7 min with 9 workers); roughly 7-10 minutes on a 2-core machine.
if(hpc) {
  plan(multisession, workers = 12)
} else {
  plan(multisession, workers = max(1, min(12, future::availableCores(logical = FALSE) - 1)))
}

# Use future_pmap_dfr for parallel processing
INB_results <- future_pmap_dfr(combinations, function(trans, import, cost_type, WTP_lower, WTP_upper) {
  calc_INB(trans, import, cost_type, WTP_lower, WTP_upper)
})

# Clean up after parallel processing if necessary
plan(sequential)  # Reset back to sequential execution if desired


# Save output from INB analysis
write.csv(INB_results, here("data_output/LTACH_IPC70_model_update/INB_results.csv"), row.names = FALSE)
