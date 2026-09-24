# Final Individual simulation model run

# The objective of this study was to simulate C. auris transmission within an 
# average long-term care hospital (LTACH) and evaluate the efficacy of admission 
# and point prevalence survey (PPS) screenings approaches on reducing C. auris 
# colonization prevalence under different transmission and facility importation rates.

Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)

hpc <- TRUE
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
library(future)
library(purrr)
library(furrr)

# check number of cores available on EDAV session = 64 available cores
future::availableCores(logical = FALSE)

# source functions to run the models
source(here("R/sample_LOS_function.R"))
source(here("R/output_processing_functions.R"))
source(here("R/individual.R"))


# Get LOS data for CA, 2019, and appropriate facility type
CA_2019_LTACH_LOS_data <- dplyr::filter(all_LOS_data, location == "CA", year == 2019, facility_type == "LTACH")


# set data frame of parameter combinations to test ---------------------------------

# parameters when no preventative screening is running, but containment screening could be running (only for Tier 2 facilities)
params_df1 <- tidyr::expand_grid(beta = c(0.0175, 0.066),  # transmission rate
                                 iota = c(0.01, 0.04),    # admission prevalence (Tier 2 and Tier 3 only)
                                 p_a = 0,           # proportion screened on admission
                                 p_pps = 0,               # proportion screened on pps
                                 p_c = 0.90,              # sensitivity of screening, proportion of colonized that will test positive
                                 contain_pps = c(0, 1),           # switch for containment based pps screening
                                 contain_a = c(0, 1),             # switch for containment based admission screening
                                 contain_taper = c(0, 1),         # switch for containment taper off instead of hard stop to PPS screening
                                 ipc_eff = 0.50,          # proportional reduction in transmission for colonized individuals that are detected through screening
                                 t_pps = 14,              # frequency of pps screening
                                 start_pps = 30,          # start PPS at day X (individual model only)
                                 dt = 1,                  # daily time step (individual model only)
                                 clinical_case_prob = 0.0688, # Probability of a colonized patient developing a clinical Candida infection
                                 inf_prob = 0.401,            # Probability that a clinical infection case is an invasive infection
                                 avg_extra_LOS = 5.6,         # Average extra length of stay for patients that develop an invasive infection 
                                 fac_max_cap = 48,       # maximum facility bed capacity (this creates a hard cap on bed number)
                                 occupancy = 0.90,        # proportion of facility occupancy (based on average occupancy for each type of facility)
                                 fac_type = "LTACH") %>%     # indicate facility type to set the correct Length of Stay (LOS) sampling function based on facility LOS data
  filter(!(contain_pps == 0 & contain_a == 0) & !(contain_pps == 0 & contain_a == 1)) # remove scenarios where containment is off (included below), and remove where only containment admission screening is on


# parameters when only admission screening or no interventions are running (i.e. preventative pps and containment is not running)
params_df2 <- tidyr::expand_grid(beta = c(0.0175, 0.066),  # transmission rate
                                 iota = c(0.01, 0.04, 0.10),    # admission prevalence (low = Tier 2, med = Tier 3, high = Tier 4)
                                 p_a = c(0, 1),           # proportion screened on admission
                                 p_pps = 0,               # proportion screened on pps
                                 p_c = 0.90,              # sensitivity of screening, proportion of colonized that will test positive
                                 contain_pps = 0,           # switch for containment based pps screening
                                 contain_a = 0,             # switch for containment based admission screening
                                 contain_taper = 0,               # switch for containment taper off instead of hard stop to PPS screening
                                 ipc_eff = 0.50,          # proportional reduction in transmission for colonized individuals that are detected through screening
                                 t_pps = 14,              # frequency of pps screening
                                 start_pps = 30,          # start PPS at day X (individual model only)
                                 dt = 1,                  # daily time step (individual model only)
                                 clinical_case_prob = 0.0688, # Probability of a colonized patient developing a clinical Candida infection
                                 inf_prob = 0.401,            # Probability that a clinical infection case is an invasive infection
                                 avg_extra_LOS = 5.6,         # Average extra length of stay for patients that develop an invasive infection 
                                 fac_max_cap = 48,       # maximum facility bed capacity (this creates a hard cap on bed number)
                                 occupancy = 0.90,        # proportion of facility occupancy (based on average occupancy for each type of facility)
                                 fac_type = "LTACH")      # indicate facility type to set the correct Length of Stay (LOS) sampling function based on facility LOS data

# parameters when pps is running vary frequency of pps, combine with parameters when pps is not running
params_df3 <- tidyr::expand_grid(beta = c(0.0175, 0.066), # transmission rate
                                 iota = c(0.01, 0.04, 0.10),   # admission prevalence
                                 p_a = c(0, 1),          # proportion screened on admission
                                 p_pps = 1,              # proportion screened on pps
                                 p_c = 0.90,             # sensitivity of screening, proportion of colonized that will test positive
                                 contain_pps = 0,           # switch for containment based pps screening
                                 contain_a = 0,             # switch for containment based admission screening
                                 contain_taper = 0,               # switch for containment taper off instead of hard stop to PPS screening
                                 ipc_eff = 0.50,         # proportional reduction in transmission for colonized individuals that are detected through screening
                                 t_pps = c(14, 30, 60, 90, 180),  # frequency of pps screening
                                 start_pps = 30,         # start PPS at day X (individual model only)
                                 dt = 1,                 # daily time step (individual model only)
                                 clinical_case_prob = 0.0688, # Probability of a colonized patient developing a clinical Candida infection
                                 inf_prob = 0.401,            # Probability that a clinical infection case is an invasive infection
                                 avg_extra_LOS = 5.6,         # Average extra length of stay for patients that develop an invasive infection 
                                 fac_max_cap = 48,      # maximum facility bed capacity (this creates a hard cap on bed number)
                                 occupancy = 0.90,       # proportion of facility occupancy (based on average occupancy for each type of facility)
                                 fac_type = "LTACH") %>% # indicate facility type to set the correct Length of Stay (LOS) sampling function based on facility LOS data
  rbind(., params_df1) %>%
  rbind(., params_df2) 



# set initial conditions
# individual
initial_ind_state <- c(S = 47, C = 1, C_s = 0, I = 0)  

# set final time
tmax <- 730


# set n_sim for the stochastic models
n_sim <- 1000

# run the individual model ----------------------------------------------------

# plan to run the model in parallel over 16 cores
if(hpc) {
  cl <- parallel::makeCluster(16) # should be same number as in sge submission script
  plan(cluster, workers = cl)
} else {
  plan(multisession, workers =  8) 
}

# 1000 simulations takes about 39 minutes with 24 parameter combinations and 730 simulation days (0.0973 seconds per simulation)
# 1000 simulations takes about 40 minutes with 64 parameter combinations
# 1000 simulations takes about ___ minutes with 76 parameter combinations and 730 simulation days (___ seconds per simulation)
# run model
system.time({
  individual_res <- individual_fun_batch(params_df = params_df3, 
                                         data = CA_2019_LTACH_LOS_data,
                                         initial_ind_state = initial_ind_state, 
                                         tmax = tmax, 
                                         n_sim = n_sim,
                                         n_params_per_split = 1,
                                         run_type = "Paired")
})


# go back to non-parallel session
plan(sequential)


# Use the cost_effect_avg on the cost_effect_df to figure out how to fix the error

# save each data frame ---------------------------------------------------------
write.csv(individual_res$avg_state_res, here("data_output/LTACH_IPC50_model_update/avg_state_data.csv"), row.names = FALSE)
write.csv(individual_res$cost_effect_res, here("data_output/LTACH_IPC50_model_update/cost_effect_data.csv"), row.names = FALSE)
write.csv(individual_res$cost_effect_avg_res, here("data_output/LTACH_IPC50_model_update/cost_effect_avg_data.csv"), row.names = FALSE)
write.csv(individual_res$last_6mo_prev_inc_res, here("data_output/LTACH_IPC50_model_update/last_6mo_prev_inc_data.csv"), row.names = FALSE)
write.csv(individual_res$percent_pos_timeseries, here("data_output/LTACH_IPC50_model_update/percent_pos_timeseries.csv"), row.names = FALSE)
write.csv(individual_res$percent_pos_summary, here("data_output/LTACH_IPC50_model_update/percent_pos_summary.csv"), row.names = FALSE)
write.csv(individual_res$params_df_sim, here("data_output/LTACH_IPC50_model_update/params_df_sim_data.csv"), row.names = FALSE)



