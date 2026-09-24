# Calibration helper functions



#' Function to extract the needed parameters from data for calibration
#'
#' This function grabs all of the important parameters to match our simulation to the conditions 
#' of the specified facility and screening methods used during a given time period and converts 
#' that information into the formats needed to put into the simulation.
#'
#' @param facility_name The unique name of the facility 
#' @param period_num The period to be simulated that indicates the types of screening occurring during that time
#'                    period 1 = PPS screening only
#'                    period 2 = PPS and Admission screening
#'                    period 3 = Admission screening only
#' @param ipc_eff The percent that IPC reduces transmission. Set this value for the calibration run based on the facility type.
#' @param init_data Data frame with the initial numbers of Susceptible, Colonized and total N of patients in the 
#'    facility at the start of each screening method period. Also include the maximum facility capacity, mean occupancy, 
#'    admission prevalence for each period and total number of days in each period.
#' @param percent_pos_data Time series data for each PPS conducted in the facility specifying differences between
#'    periods, the days that PPS were conducted starting from 1 at the beginning of each period, and the percent positive
#'    detected from each PPS.
#'
#' @return List including:
#'          param_df = All the key parameters for running the simulation set to match the facility data
#'          initial_ind_state = The initial state for the facility at the start of the given period
#'          tmax = The total number of days in the given period to simulate
#'          t_pps_vec = Vector of days when PPS occured in the facility for the given period
#'          n_sim = Number of sims run at a time, should be 1 here.
#'
get_params <- function(facility_name,
                       period_num, 
                       ipc_eff,
                       screened,
                       init_data, 
                       percent_pos_data){
  
  # filter sim_dat and percent_pos_data a to get only one facility and period combination
  selected_init_data <- filter(init_data, facility == facility_name, period == period_num, amount_screened == screened)
  selected_percent_pos_data <- filter(percent_pos_data, facility == facility_name, period == period_num, amount_screened == screened)
  
  # pull out initial states
  initial_ind_state <- unlist(selected_init_data[ , c("S", "C", "C_s", "I")])
  
  # pull out tmax
  tmax <- unlist(selected_init_data[ , "tmax"])
  
  # pull out time series for PPS's (t_pps_vec) for periods 1 and 2
  if(period_num < 3){
    t_pps_vec <- unlist(selected_percent_pos_data[ , "T_PPS"])
    t_pps_manual <- 1
  } else{
    t_pps_vec <- NULL
    t_pps_manual <- 0
  }
  
  # get parameters from data to simulate
  params_df <- data.frame(beta = NA,                                                              # transmission rate (we are estimating this value)
                          iota = unlist(selected_init_data[ , "period_adjusted_admit_prev"]),      # admission prevalence
                          p_a = unlist(selected_init_data[ , "p_a"]),                              # proportion screened on admission
                          p_pps = unlist(selected_init_data[ , "p_pps"]),                          # proportion screened on pps
                          p_c = 0.90,                                                             # sensitivity of screening, proportion of colonized that will test positive
                          contain_pps = 0,                                                        # switch for containment based pps screening
                          contain_a = 0,                                                          # switch for containment based admission screening
                          contain_taper = 0,                                                      # switch for containment PPS to taper off after no new cases for two consecutive PPSs
                          ipc_eff = ipc_eff,                                                         # proportional reduction in transmission for colonized individuals that are detected through screening  (we are estimating this value)
                          t_pps = NA,                                                             # frequency of pps screening, set to NA since we are providing t_pps_vec
                          t_pps_manual = t_pps_manual,                                            # switch to turn on/off using a set t_pps frequency vs using a vector with specified pps days
                          start_pps = 1,                                                          # start PPS at day X (individual model only)
                          dt = 1,                                                                 # daily time step (individual model only)
                          clinical_case_prob = 0.086,                                             # Probability of a colonized patient developing a clinical Candida infection
                          inf_prob = 0.36,                                                        # Probability that a clinical infection case is an invasive infection
                          avg_extra_LOS = 5.6,                                                    # Average extra length of stay for patients that develop an invasive infection 
                          fac_max_cap = unlist(selected_init_data[ , "fac_max_cap"]),              # maximum facility bed capacity (this creates a hard cap on bed number)
                          occupancy = unlist(selected_init_data[ , "occupancy_mean"]),             # proportion of facility occupancy (based on average occupancy for each type of facility)
                          fac_type = unlist(selected_init_data[ , "fac_type"]))
  
  # set nsim
  n_sim = 1
  
  return(list(params_df = params_df, initial_ind_state = initial_ind_state, tmax = tmax, t_pps_vec = t_pps_vec, n_sim = n_sim))
}





#' Function to extract the real data to match to the simulation
#' 
#' We need to find the real facility data that captures the total number of screened
#' colonized patients (detected) during each period with a given set of screening methods
#' for each type of facility. 
#'
#' @param facility_name The unique name of the facility 
#' @param period_num The period to be simulated that indicates the types of screening occuring during that time
#'                    period 1 = PPS screening only
#'                    period 2 = PPS and Admission screening
#'                    period 3 = Admission screening only
#' @param init_data Data frame with the initial numbers of Susceptible, Colonized and total N of patients in the 
#'    facility at the start of each screening method period. Also include the maximum facility capacity, mean occupancy, 
#'    admission prevalence for each period and total number of days in each period.
#' @param percent_pos_data Time series data for each PPS conducted in the facility specifying differences between
#'    periods, the days that PPS were conducted starting from 1 at the beginning of each period, and the percent positive
#'    detected from each PPS.
#'
#' @return Returns a list that includes
#'            data_cum_percent_pos_col = the total number of screened colonized patients detected divided by the total screened during the specified period within the facility
#'            data_pps_percent_positive = vector of percent positive from each PPS during the specified period within the facility.

get_match_data <- function(facility_name,
                           period_num, 
                           screened,
                           init_data, 
                           percent_pos_data){
  
  # filter sim_dat and percent_pos_data a to get only one facility and period combination
  selected_init_data <- filter(init_data, facility == facility_name, period == period_num, amount_screened == screened)
  selected_percent_pos_data <- filter(percent_pos_data, facility == facility_name, period == period_num, amount_screened == screened)
  
  # pull out the cumulative number of screened colonized patients (detected)
  data_cum_percent_pos_col <- unlist(selected_init_data[ , "cum_percent_pos_col"])
  
  
  # pull out time series for PPS's percent positive (t_pps_vec) for periods 1 and 2
  if(period_num < 3){
    data_pps_percent_positive <- unlist(selected_percent_pos_data[ , "PERCENT_POSITIVE"])
  } else{
    data_pps_percent_positive <- NULL
  }
  return(list(data_cum_percent_pos_col = data_cum_percent_pos_col, data_pps_percent_positive = data_pps_percent_positive))
}




#' Function to select a LOS distribution for specific facility type, year and location site
#'
#' @param fac_type Specify the type of facility: LTACH for this analysis
#' @param year_selected Specify the year of LOS data to select: 2019, 2020, or 2021
#' @param site The location source of the data: CA or a specific OC LTACH facility
#' @param LOS_data Provide the full LOS distribution data to pull from
#'
#' @return Returns a data frame of the LOS distribution for the facility type,
#'          year, and location site specified.
#' 
get_LOS <- function(fac_type,
                    year_selected,
                    site = CA,
                    LOS_data = all_LOS_data){
  
  # filter data to specific facility type, location, and year
  LOS_dist <- LOS_data %>% dplyr::filter(facility_type == fac_type, year == year_selected, location == site)
  return(LOS_dist)
}










#' Function to add intervention labels to the parameter data frame based on specific conditions for calibration simulation runs
#'
#' This function runs within the individual_fun_calibrate_hpc function to help label parameter combos correctly.
#'
#' @param params_df_sim the data frame of all simulation's parameter combinations produced within the individual_fun series of functions
#'
#' @return Updated parameter data frame that is reorganized and includes clear labels for each intervention to group similar parameter combinations
#'
#' @examples
#' #' params_df_sim1 <- param_intervention_labels(individual_res$params_df_sim)
#' 
param_intervention_labels_calibrate <- function(params_df_sim){
  
  params_output_df <- as.data.frame(params_df_sim) %>%
    mutate(intervention = factor(case_when(
      p_a == 0 & p_pps == 0 ~ "No Screening",
      p_a == 1 & p_pps == 0 ~ "Admissions only",
      p_a == 0 & p_pps == 1 ~ "PPS only",
      p_a == 1 & p_pps == 1 ~ "Admissions + PPS"),
      levels = c("No Screening", "Admissions only", "PPS only", "Admissions + PPS")),
      import = round(iota, 2)) %>%
    select(seed, nsim, intervention, beta, iota, import, intervention, p_a, p_pps, p_c, ipc_eff:fac_type)
  
  return(params_output_df)
}




#' Calculate the percent positive from PPS and admissions screening for calibration
#' 
#' This function runs within the individual_fun_calibrate_hpc function to calculate 
#' the simulated data to compare to the real facility data for percent positive for each PPS.
#'
#' @param screen_df Data frame of PPS and admission screening results for each simulation
#' @param param_df Data table with all of the details of of each parameter combination
#' 
#' @return A list of one vector:
#'          sim_pps_percent_positive = vector of the percent positive during each PPS in the time series from a given facility
#'          sim_total_screened = total number of screened during the period
#'
#' 
percent_positive_calibrate <- function(combined_screen_df, param_df){
  
  # make a smaller subset of the params to join to other dfs
  params_id_df <- param_df %>%
    select(seed, beta, iota, import, intervention, p_a, p_pps, nsim)
  
  # calculate the total number of screened patients during the period
  sim_total_screened <- nrow(combined_screen_df)
  
  
  # if pps is running in the simulation
  if(params_id_df$p_pps == 1){
    
    # calculate the percent positivity for PPS and admission at 1 yr and 2 yrs
    PPS_results <- combined_screen_df %>%
      dplyr::filter(type == "pps") %>%
      group_by(seed, tstep_screened, status, type) %>%
      count() %>%
      pivot_wider(id_cols = seed:tstep_screened, names_from = status, names_prefix = "n_", values_from = n) %>%
      full_join(., params_id_df, by = c("seed")) %>%
      group_by(seed) %>%
      mutate(last_date = if_else(seed == lag(seed), lag(tstep_screened), NA),
             days_since_last_PPS = as.numeric(tstep_screened - last_date),
             n_0 = if_else(is.na(n_0), 0, n_0),
             n_1 = if_else(is.na(n_1), 0, n_1),
             n_total = n_0+n_1,
             percent_positive = round((n_1/n_total)*100, 2),
             pps_round = seq(length(tstep_screened))) %>%
      select(seed, pps_round, tstep_screened, days_since_last_PPS, n_0, n_1, n_total:pps_round, beta:nsim)
    
    ## get timeseries of percent positive from each PPS
    sim_pps_percent_positive <- PPS_results$percent_positive
    
    
    # calculate the PPS screening summary at end of period
    PPS_results_summary <- PPS_results %>%
      group_by(seed) %>%
      summarize(n_0 = sum(n_0),
                n_1 = sum(n_1),
                n_total = sum(n_0, n_1, na.rm = T),
                overall_percent_positive = (n_1/n_total)*100,
                end_point = tmax,
                type = "pps") %>%
      select(seed, end_point, type, n_0:overall_percent_positive)
  } else {
    sim_pps_percent_positive <- NA  # set to null so it is still found when PPS screening is NOT operating
  }
  
  return(list(sim_pps_percent_positive = sim_pps_percent_positive, sim_total_screened = sim_total_screened))
}








#' Function to calculate the distance between the simulated and real data - method Root mean squared error (RMSE)
#'
#' @param sim_cum_screened_colonized Simulated output for cumulative screened colonized patients
#' @param data_cum_screened_colonized Real facility data for cumulative screened colonized patients
#' @param sim_percent_positive Simulated output for the time series of percent positive on each PPS
#' @param data_percent_positive Real facility data for the time series of percent positive on each PPS
#'
#' @return Returns a vector with the RMSE result for the cumulative screened colonized
#'          and the percent positive on PPS across the time series.

calc_distance_RMSE <- function(sim_cum_percent_pos_col,
                               data_cum_percent_pos_col,
                               sim_pps_percent_positive, 
                               data_pps_percent_positive){
  RMSE_col <- sqrt(mean((sim_cum_percent_pos_col - data_cum_percent_pos_col)^2))
  
  RMSE_pps <- sqrt(mean((sim_pps_percent_positive - data_pps_percent_positive)^2))
  
  return(c(RMSE_col, RMSE_pps))
}


###  copied functions from Minter et al 2019 code for ABC SMC and ABC AMC MNN ---------------------------

# Perturbation kernel 
rK <- function(mean, sigma){   
  return(rtmvnorm(1,mean=mean, sigma=sigma, lower=lm.low, upper=lm.upp)) 
}

# Identity function: H(x)= 1 if x=T
H <- function(x) as.numeric(x>0)

#  Test if prior is non zero
prior.non.zero<-function(par){
  prod(sapply(1, function(a) H(par[a]-lm.low[a])* H(lm.upp[a]-par[a])))
}

# Normal Euclidean distance calculation
Norm.Eucl.dist<-function(p1,p2){
  sqrt(sum(((p1-p2)/(lm.upp-lm.low))^2)) }

# Covariance based on M neighbors
getSigmaNeighbours<-function(M, theta, Theta){
  dist<- sapply(1:N, function(a) Norm.Eucl.dist(as.numeric(theta), as.numeric(Theta[a,])))
  temp<-data.frame(no=seq(1,N), dist)
  temp<-temp[order(temp$dist),]
  sigma<-cov(Theta[temp$no[1:(M+1)],])
  return(sigma)
}


