# Running the Approximate Bayesian Computation with Sequential calibration of the individual simulation model
# Modified to run on HPC with parallelization 


Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = FALSE)

hpc <- TRUE  # change to TRUE once testing on hpc
if(hpc){
  source(".Rprofile") # if in hpc environment then need to source this to load up renv
  renv::restore(packages = "renv")
  #renv::diagnostics()
} 

array <- TRUE
if(array) { 
  argus <- commandArgs(trailingOnly = TRUE) 
  row_ind <- as.numeric(argus[1])
} else {
    row_ind <- 5 # run a single facility, period, ipc_eff combination
  }



# load libraries
library(here)
library(data.table)
library(tidyverse)
library(purrr)
library(furrr)
library(future)
library(tmvtnorm)
library(WeightIt)

# source functions to run the models
source(here("R/sample_LOS_function.R"))
source(here("R/individual.R"))
source(here("R/calibration_helper_functions.R"))
source(here("R/calibration_functions_hpc.R"))


# read in the OC data with the initial conditions and final outputs
init_data <- read.csv(here("data/init_data.csv"))
percent_pos <- read.csv(here("data/percent_pos_data.csv"))

# read in facility params dataframe with combinations to run through calibration
facility_params <- read.csv(here("data/facility_params.csv"))[row_ind, ]


# set facility, period, and other characteristics to use for calibration
facility_name <- facility_params$facility_name
period_num <- facility_params$period_num
amount_screened <- facility_params$amount_screened
year <- facility_params$year
site <- facility_params$site
ipc_eff <- facility_params$ipc_eff
fac_type = facility_params$fac_type
generation_seed <- as.numeric(select(facility_params, Gen_1:Gen_6))


# amount_screened indicates whether a PPS covered the full facility; all LTACH calibration runs are full facility
if(amount_screened == "vent floor"){ fac_screened <- "vent"} else {fac_screened <- "full"}


# get data to match for cumulative screened colonized and percent positive on each PPS
match_data <- get_match_data(facility_name = facility_params$facility_name, period_num = facility_params$period_num, screened = facility_params$amount_screened, init_data = init_data, percent_pos_data = percent_pos)


# get the parameters to simulate the model
param_list <- get_params(facility_name = facility_params$facility_name, period_num = facility_params$period_num, ipc_eff = facility_params$ipc_eff, screened = facility_params$amount_screened, init_data = init_data, percent_pos_data = percent_pos)

# get the LOS distribution to simulate the model (likely CA 2019 LTACHs)
LOS_dist <- get_LOS(fac_type = fac_type, year = year, site = site, LOS_data = all_LOS_data)

# set sims_max = maximum number of sims to run per generation
sims_max <- 250000


# Number of particles
N <- 1000


# Number of generations
G <- 6


# Number of simulations for each parameter set (match to n_sim so it is the same across functions?)
n <- 1

# consider updating this to a quantile based method: https://pyabc.readthedocs.io/en/latest/examples/optimal_threshold.html
# Set epsilon quantile to determine threshold sequence based on distances
epsilon_quantile <- 0.25  # use this for the OC LTACHs (LTACH A, B, and C)
#epsilon_quantile <- 0.3  # use this for the CDPH LTACH (LTACH D)

# Set initial epsilon values for cumulative proportion positive out of all screening tests data
epsilon_col <- 0.4
#epsilon_col <- c(0.40, 0.30, 0.25, 0.20, 0.15, 0.10, 0.05, 0.03, 0.02) 


# Set initial epsilon values for percent positive on pps data 
#epsilon_pps <- c(30, 25, 20, 18, 16, 15.5, 15, 14.5, 14) 
#epsilon_pps <- c(30, 25, 20, 18, 16, 15, 14, 13, 10)
epsilon_pps <- 30
#epsilon_pps <- c(25, 20, 16, 14, 12, 10, 8, 6, 4)


#  Lower and upper boundaries for priors - beta (transmission)
lm.low <- c(0.01)  # 0.01 for LTACHs
lm.upp <- c(0.07)


# set number of cores to use to determine how to split up the simulations
cores <- 20




# Test the calibration functions ----------------------------------------------------------

# plan to run the model in parallel over 16 cores
if(hpc) {
  cl <- parallel::makeCluster(20) # should be same number as in sge submission script
  plan(cluster, workers = cl)
} else {
  plan(multisession, workers =  5) 
}


# running calibration
system.time(
  abc_stats <- run_calibration_hpc(N = N, 
                                   G = G, 
                                   n = n, 
                                   lm.low = lm.low, 
                                   lm.upp = lm.upp, 
                                   epsilon_quantile = epsilon_quantile,
                                   epsilon_col = epsilon_col, 
                                   epsilon_pps = epsilon_pps,
                                   match_data = match_data, 
                                   param_list = param_list,
                                   LOS_data = LOS_dist,
                                   sims_max = sims_max, 
                                   cores = cores, 
                                   generation_seed = generation_seed)
)
# run time with 5 cores for 1 G, 1000 N, 1 sim, total time = 412.081 seconds (~7 min), total sims = 1472, time/sim = 0.2799463 secs/sim, 1.472 sims/particle
# run time with 5 cores for 1 G, 1000 N, 2 sims, total time = 1242.180 seconds (~21 min), total sims = 2688, time/sim = 0.4621205 secs/sim, 2.688 sims/particle
# run time with 5 cores for 2 G, 1000 N, 1 sim, total time =  1683.533 seconds (~28 min), total sims = 2811 (1357 2nd gen only), time/sim = 0.5989089 secs/sim, 2.811 sims/particle


# go back to non-parallel session
plan(sequential)

abc_stats

# number of generations completed
n_gen <- length(abc_stats$sims_per_generation)

# convert abc_stats to a dataframe to save
abc_stats_df <- tibble(facility = facility_name,
                       amount_screened = fac_screened,
                       period = period_num,
                       ipc_eff = ipc_eff,
                       generation = seq(1,n_gen),
                       total_sims = c(abc_stats$total_sims, rep(NA, (n_gen-1))),
                       total_acceptance_rate = c(abc_stats$total_acceptance_rate, rep(NA, (n_gen-1))),
                       sims_per_generation = abc_stats$sims_per_generation,
                       accepted_particles_per_generation = abc_stats$accepted_particles_per_generation,
                       acceptance_rate_per_generation = abc_stats$acceptance_rate,
                       epsilon_col_per_generation = abc_stats$epsilon_col_per_generation,
                       epsilon_pps_per_generation = abc_stats$epsilon_pps_per_generation,
                       ESS_per_generation = abc_stats$ESS_per_generation,
                       sims_expected = c(abc_stats$sims_expected, rep(NA, (n_gen-1))))


write.csv(abc_stats_df, here(paste("calibration/full_results/abc_stats_", facility_name,"_period_", period_num, "_IPC_", ipc_eff, "_", fac_screened, ".csv", sep="")))
