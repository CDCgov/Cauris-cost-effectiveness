#' Simulate C. auris transmission in a healthcare facility at the patient level
#'
#' @description 
#' Simulates transmission at the individual level but assuming mass action transmission:
#' transmission is proportional to number of colonized individuals in the facility and
#' risk is uniform across all susceptible patients. We assume persistent colonization
#' for C. auris. Patients are screened either through admission screening or via PPS's
#' that occur at some regular frequency. Colonized patients that have been detected 
#' through screening are not re-screened. Screening results in reduced transmission 
#' as colonized and detected individuals are placed in IPC protocols with contact 
#' precautions, which have a lower transmission rate than undetected colonized patients. 
#' We assume a steady facility size that on average is equivalent to the percent occupancy
#' of the maximum facility capacity, which is determined by the facility type. 
#' The facility type also determines the patient length of stay (LOS) distribution, which 
#' is determined based on Centers for Medicare and Medicaid Services LOS data. We sample 
#' each patient's LOS from the appropriate LOS distribution for the given facility type.
#' 
#' In this model, events happen sequentially. First we simulate colonization and the development 
#' of clinical infections (more details below). Second, we simulate discharge based on each patient's LOS, 
#' such that patients who were colonized but discharged in that time step contribute to the 
#' force of infection. Third, newly admitted patients neither contribute to transmission or are at risk 
#' of acquiring transmission or infection on the day they are admitted. Finally, we simulate preventative
#' PPS screening and/or admission screening (if operating in the simulation), which moves 
#' patients into the detected colonized state and IPC to reduce their contribution to transmission.
#' In some cases, only containment PPS and admission may be turned on after the first invasive infection
#' is detected in the simulation, and can be turned off after two consecutive PPSs do not detect 
#' any more colonization cases.
#' 
#' For the development of clinical infection cases, we assume an exponential rate 
#' of the probability that a colonized patient will develop a clinical infection 
#' (any kind of Candida auris infection) at time t. This is based off of our estimates 
#' from our data on the number of days between date of screening and date of clinical 
#' case, but assuming that our screening date is reasonably close to the date of colonization.
#' All patients that develop a clinical case are also moved into the colonized detected 
#' state where the would be placed on IPC/contact precautions. We estimate based on data that 
#' about 50% of clinical cases represent invasive infections (more severe bloodstream 
#' or wound infections). Therefore using that probability of invasive infections we 
#' randomly determine whether each new clinical case will be an invasive infection or not. 
#' Patients that have an invasive infection will have additional LOS sampled from a poisson 
#' distribution with a mean based on data-estimated average extra LOS for invasive infected patients.
#' 
#' Michelle to describe clinical cases and invasive infections here. 
#' 
#' @param iota admission prevalence (probability of new admit being positive)
#' @param t_pps pps frequency (time in days between pps's)
#' @param start_pps start day of pps screening
#' @param t_pps_manual switch to turn on/off using a set t_pps frequency vs using a vector with specified pps days
#' @param p_a proportion screened on admission (switch for prevention based admission screening)
#' @param p_pps proportion screened on pps (switch for prevention based pps)
#' @param p_c sensitivity of screening test, probability that a colonized person will test positive (thought to be very high for C. auris)
#' @param contain_pps switch for containment based pps screening
#' @param contain_a switch for containment based admission screening
#' @param contain_taper switch for containment PPS to taper off after no new cases for two consecutive PPSs
#' @param dt the number of days in each time step (default: 1, daily)
#' @param beta transmission rate
#' @param ipc_eff proportional reduction in transmission for colonized individuals that are detected through screening
#' @param tmax max time step (how many time steps to simulate)
#' @param fac_max_cap maximum facility bed capacity (this creates a hard cap where the number of patients cannot be above this number)
#' @param occupancy proportion of facility occupancy (based on average occupancy for each type of facility)
#' @param clinical_case_prob Probability of a colonized patient developing a clinical Candida infection (including vaginal, oral, or invasive infections). 
#'      This value is based on C. auris case surveillance data that is voluntarily submitted by states.
#' @param inf_prob Probability that a clinical infection case is an invasive infection
#' @param avg_extra_LOS Average extra length of stay for patients that develop an invasive infection 
#' @param fac_type indicate facility type to set the correct Length of Stay (LOS) sampling function based on facility LOS data
#'      This analysis uses LTACH. Any facility type present in the CMS LOS data can be supplied.
#' @param t_pps_vec If t_pps_manual is set to 1, then a vector of specific PPS days in sequential order can be supplied instead of using t_pps intervals
#' @param data data frame with LOS and percent of patients that had each possible LOS from CMS data. 
#'        The data frame also includes the data year, location, and facility type. We are using 2019 data from CA only.
#' @param initial_ind_state vector of initial states (S = 199, C = 1, C_s = 0, I = 0, I_s = 0), sum of this vector is the max number of beds allowed in the facility
#'      S = Susceptible, C = all colonized (undetected and screened), C_s = screened positive for colonization (detected), I = invasive infection, I_s = screened positive for infection (detected)
#' @param seed Specify a different set seed for each simulation of the model and to allow for reproducibility of results
#' @param unique_id A unique ID for each simulation to keep track of individual simulation results and connect data sets later
#'
#'
#' @return a list with three objects:
#'      screen_res are the screening results,
#'      clinical_res are the clinical infection results,
#'      state res is the number of patients in each state (S = susceptible, C = colonized total, 
#'          C_s = colonized screened/detected, C_tot = cumulative colonized at each time point, 
#'          I = invasive infection, I_tot = cumulative infections at each time point),
#'      admit res are the patient admit and discharge rates 
#'
#' @require 
#' library(data.table)
#' library(tidyverse)
#' library(purrr)
#' library(furrr)
#' library(future)
#' 
#' 


sim_fac_individuals <- function(iota = 0.01, 
                                t_pps = 60, 
                                start_pps = 60, 
                                t_pps_manual = 0,
                                p_a = 1, 
                                p_pps = 1, 
                                p_c = 0.90, 
                                contain_pps = 0,
                                contain_a = 0,
                                contain_taper = 0,
                                dt = 1, 
                                beta = 0.035, 
                                ipc_eff = 0.5, 
                                tmax = 730, 
                                fac_max_cap = 100, 
                                occupancy = 0.90,
                                clinical_case_prob = 0.086,
                                inf_prob = 0.36,
                                avg_extra_LOS = 5.6,
                                fac_type = "LTACH", 
                                t_pps_vec = NA,
                                data = CA_2019_LOS_data,
                                initial_ind_state, 
                                seed = NA,
                                unique_id = NA) { 
                                  
  # browser() # just lets you step in to the function to work through
  
  # Set seed for reproducible runs
  set.seed(seed)
  
  # Set unique id to seed if not specified
  if(is.na(unique_id)){
    unique_id <- seed
  }
  
  # translate to proportion of transmission for patients not screened
  beta_ipc <- beta * (1 - ipc_eff)

  
  # set up sequence of PPS screening dates -----
  if(p_pps > 0 & t_pps_manual == 0){
    ts_pps <- seq(from = start_pps, to = tmax, by = t_pps)
  } else if(p_pps > 0 & t_pps_manual == 1){
    ts_pps <- t_pps_vec
  } else{
    ts_pps <- NULL
  }
  
  # set up t0 ----
  N_0 <- sum(initial_ind_state)
  
  if(N_0 > fac_max_cap){ 
    warning("Initial conditions have more patients than the maximum facility capacity! (i.e. N_0 > fac_max_cap)")
  }
  
  C_0 <- sum(initial_ind_state["C"])
  C_s <- sum(initial_ind_state["C_s"])
  I_0 <- sum(initial_ind_state["I"])
  
  patients <- seq(N_0) # ids
  n_patients_total <- N_0  
  colonized <- c(rep(0, N_0 - C_0), rep(1, C_0))
  clinical_cases <- rep(0, N_0)  
  invasive_infections <- c(rep(0, N_0 - I_0), rep(1, I_0))

  
  if(C_s == 0){ 
    detected <- rep(0, N_0) 
  } else {    
    detected <- c(rep(0, N_0 - C_s), rep(1, C_s))  
  }
  
  
  # for triggering on and off admission screening
  detected_on_pps <- rep(0, tmax) # recording patients detected on containment pps
  p_pps_start <- p_pps # record starting p_pps condition
  p_a_start <- p_a     # # record starting p_a condition
  
  # for triggering on and off containment screening
  if(iota == 0.01){
    containment_trigger_on <- 1 # 1 clinical case in Tier 2
  } else if(iota == 0.04){
    containment_trigger_on <- 2 # 2+ clinical cases in Tier 3
  } else {
    containment_trigger_on <- 1
  }
  containment_trigger_off <- 0 # 0 new cases for two consecutive pps's
  taper_trigger_off <- 5  # threshold for 5% positive on last pps, if greater than 5% then restart containment procedures
  
  
  tracked_clinical <- rep(0, tmax)
  tracked_invasive <- rep(0, tmax)
  
  
  colonized_total <- sum(colonized) 
  colonized_screened_total <- sum(detected)
  invasive_infections_total <- sum(invasive_infections)
  clinical_cases_total <- sum(clinical_cases)
  new_patient_index <- n_patients_total + 1 
  
  # set median number of days for a colonization case to become a clinical infection for all facilities
  # based on C. auris case surveillance data voluntarily submitted by states
  days_to_infection <- 46
  
  
  # initiate vectors to hold ids and colonization/infection status for each timestep
  admit_ids <- admit_screened_ids <- pps_ids <- admit_status <- pps_status <- discharged_ids <- clinical_case_ids <- invasive_infection_ids <- NULL  
  
  # sample the LOS for all current patients based on the facility type using a gamma distribution
  discharge_tstep <- sample_LOS(n = length(patients), data = data, fac_type = fac_type) 
  
  # for storing the screening results
  screen_res <- data.table(patient_id = 0, 
                           tstep_screened = 0L,
                           status = 0L,     # colonization status (admission and pps)
                           type = NA_character_, # admission or pps
                           seed = seed,
                           unique_id = unique_id)
  
  # for storing infection results
  clinical_res <- data.table(patient_id = 0, 
                             tstep = 0L, 
                             invasive = 0L,
                             seed = seed,
                             unique_id = unique_id)
  
  # for storing patient admit and discharge dates (initialize data table with initial patients)
  admit_res <- data.table(patient_id = patients, 
                          admit_tstep = 0L, 
                          discharge_tstep = 0L, 
                          seed = seed,
                          unique_id = unique_id)
  
  # for tracking #s in each state daily
  state_res <- data.table(tstep = 0, S = N_0 - C_0, C = C_0, C_s = C_s, 
                          C_tot = colonized_total, C_s_tot = colonized_screened_total, 
                          I_clinical = I_0, I = I_0, I_tot = invasive_infections_total, 
                          Clinical_tot = clinical_cases_total, seed = seed, unique_id = unique_id) 
  
  
  # Loop through time and all events sequentially -----
  for (t in seq(tmax)) {
    
    # set t to step through this interactively
    # t = 2
    
    # Step 1A: Transmission -----
    # force of infection (to become colonized) at time t
    trans_rate <- beta * (sum(colonized) - sum(detected)) + beta_ipc * sum(detected)
    foi_prob <- 1 - exp(-(trans_rate / length(patients)) * dt)  
    
    newly_colonized <- rbinom(n = sum(!colonized), 
                              size = 1,            
                              prob = foi_prob)     
    
    colonized[!colonized] <- newly_colonized
    colonized_total <- sum(colonized_total, newly_colonized)
    
    
    # Step 1B: Infection -----
    
    # development of invasive infections, assuming that we detect invasive infections right away.
    # rate for colonized patients to become clinically infected at time t
    clinical_case_rate <- 1 - exp(-(clinical_case_prob / days_to_infection))


    # simulate generation of clinical cases
    new_clinical_case <- rbinom(n = (sum(colonized)- sum(clinical_cases)),     
                                size = 1,                                 
                                prob = clinical_case_rate)                 
    
    # assign any that would be invasive
    new_invasive_infections <- rbinom(n = sum(new_clinical_case), 
                                      size = 1, 
                                      prob = inf_prob)
    
    
    # record of previous new clinical and invasive cases
    tracked_clinical[t] <- sum(new_clinical_case)
    tracked_invasive[t] <- sum(new_invasive_infections)
    
    
    if(sum(new_clinical_case) > 0 ) {
      
      # track ids for reporting clinical results
      clinical_case_ids <- patients[!clinical_cases & colonized == 1][new_clinical_case == TRUE]
      
      #update new clinical cases
      clinical_cases[colonized == 1 & !clinical_cases] <- new_clinical_case
      
      # update cumulative screened colonized (detected) for new colonized cases that were not previously detected
      new_clinical_previously_undetected <- sum(detected[clinical_cases == 1] == 0)
      colonized_screened_total <- sum(colonized_screened_total, new_clinical_previously_undetected)
      
      # if a patient with a clinical case then make sure their status for detection == 1
      detected[clinical_cases == 1] <- 1
      
      # update invasive infections
      invasive_infections[patients %in% clinical_case_ids] <- new_invasive_infections
      
      # update cumulative clinical and invasive infected totals
      clinical_cases_total <- sum(clinical_cases_total, new_clinical_case)
      invasive_infections_total <- sum(invasive_infections_total, new_invasive_infections)
      
      
      # sample additional number of days to stay in the facility for patients that develop invasive infections
      if(sum(new_invasive_infections) > 0){ 
        
        invasive_infection_ids <- clinical_case_ids[new_invasive_infections == TRUE]
        
        extra_los <- rpois(n = sum(new_invasive_infections), lambda = avg_extra_LOS)
        
        discharge_tstep[patients %in% invasive_infection_ids] <- discharge_tstep[patients %in% invasive_infection_ids] + extra_los
        
      }
      
    }

    
    # Step 2: Discharge -----
    
    discharged <- discharge_tstep == t   
    
    # remove discharged patients
    if(sum(discharge_tstep == t) > 0) {
      
      discharged_ids <- patients[discharged == TRUE]    
      patients <- patients[!discharged]                 
      colonized <- colonized[!discharged]               
      detected <- detected[!discharged]        
      invasive_infections <- invasive_infections[!discharged]                 
      clinical_cases <- clinical_cases[!discharged]    
      discharge_tstep <- discharge_tstep[!discharged]   
      
    }
    
    # Step 3: Admissions ----
    
    expected_admits <- (fac_max_cap * occupancy) - length(patients) 
    admits <- rpois(n = 1, lambda = pmax(expected_admits, 0)) 
    
    admits_col_status <- rbinom(n = admits, size = 1, prob = iota) 
    
    # add admitted patients
    if(admits > 0) {
      
      admit_ids <- seq(new_patient_index, n_patients_total + admits)  
      patients <- c(patients, admit_ids)                              
      n_patients_total <- n_patients_total + admits                   
      colonized <- c(colonized, admits_col_status)                    
      detected <- c(detected, rep(0, admits))                 
      colonized_total <- sum(colonized_total, admits_col_status) 
      invasive_infections <- c(invasive_infections, rep(0, admits))   
      clinical_cases <- c(clinical_cases, rep(0, admits))             
      
      new_admit_LOS <- sample_LOS(n = admits, data = data, fac_type = fac_type) 
      discharge_tstep <- c(discharge_tstep, (new_admit_LOS + t))      
      
    }
    
    # Step 4: Screening (Admissions and PPS)  -----
    
    if(contain_pps == 1 & iota == 0.01){ # set trigger for Tier 2 to be 1+ clinical cases on that day
      n_new_clinical <- sum(tracked_clinical[t])
    } else if(contain_pps == 1 & iota == 0.04){ # set trigger for Tier 3 to be 2+ clinical cases within 14 days
      n_new_clinical <- sum(tracked_clinical[pmax(t-14, 0):t])
    } else {
      n_new_clinical <- 0
    }
    
    
    # Turn on containment screening at time of clinical case detected (when no other preventative screening is occurring)
    if((contain_pps == 1 | contain_a == 1) & p_pps == 0 & n_new_clinical >= containment_trigger_on) { 
      
      
      # turn on containment pps 
      if(contain_pps == 1 & t < (tmax-1)){ 
        
        # adjust ts_pps frequency based on date of first trigger
        t_pps_contain <- 14
        t_delay <- rpois(n = 1, lambda = 10)
        start_contain <- sum(t, t_delay)
        if(start_contain < tmax){  # only start pps if date is before the end of the simulation
          if(p_pps_start > 0 & p_pps_start < 1){ 
            p_pps <- p_pps_start
          } else {
              p_pps <- 1 
            }
          ts_pps <- seq(start_contain, tmax, by = t_pps_contain)
          taper <- NULL  # set this to NULL to indicate that containment PPS is NOT currently in a tapering off period
        }
        
      }
      # turn on containment admission screening
      if(contain_a == 1){ 
        p_a <- 1
      }
      
    }
    
    if(t %in% ts_pps) {
       
       # don't screen people if they were already screened positive for colonization (i.e., detected)
       pps_screened <- rbinom(n = sum(!detected),  
                              size = 1, 
                              prob = p_pps)       
       
       # all patients screened
       pps_ids <- patients[!detected][pps_screened == TRUE]
       
       pps_status <- rbinom(n = sum(!detected),
                                size = 1,
                                prob = p_c * pps_screened * colonized[!detected]) 
       
       detected[!detected] <- pps_status
       detected_on_pps[t] <- sum(pps_status)
       colonized_screened_total <- sum(colonized_screened_total, pps_status)
      
    } else {
      
      # admission screening (only need to be explicit if this is not in a pps)
      if(p_a > 0 & admits > 0) {
        
        admits_screened <- rbinom(n = admits, size = 1, prob = p_a)
        
        admits_screened_pos <- rbinom(n = admits, 
                                      size = 1, 
                                      prob = p_c * admits_screened * admits_col_status) 
        
        detected[patients >= new_patient_index] <- admits_screened_pos  
        
        admit_screened_ids <- admit_ids[admits_screened == TRUE]        
        admit_status <- admits_screened_pos[admits_screened == TRUE]    
        colonized_screened_total <- sum(colonized_screened_total, admit_status)
      }
    }
    
    
    # stop containment screening if no new cases identified for two consecutive pps's,
    # or taper off if contain_taper == 1
    if(contain_pps == 1 & !is.null(ts_pps)) {
      
      # don't do this check if it's the first containment pps
      if(t != start_contain & t %in% ts_pps) {
        
        # check whether containment screening should continue
        contain_criteria <- pmax(detected_on_pps[t], detected_on_pps[t - 14])
        
        # find percent positive on last pps - check whether the percent positive on last pps during taper period is above threshold allowed (set by taper_trigger_off)
        last_pps_percent_positive <- (detected_on_pps[t]/sum(pps_screened))*100
        
        
        # if 2 consecutive containment PPS with 0 new cases and taper param switch is on, but the taper period has not started yet (taper is NULL),
        # then end containment PPS and turn on taper PPS sequence
        if(contain_criteria <= containment_trigger_off & contain_taper == 1 & is.null(taper)){
          
          # start taper off containment screening
          taper <- c(30, (30 + 90), (30 + 90 + 180))  # set the taper intervals at 30 days, 90 days, and 180 day intervals
          ts_pps <- t + taper
          
          
          # set criteria to end the taper containment screening
          taper_end <- ts_pps[3]
          

          # if already in the taper period after containment PPS has ended (taper is not NULL)
        } else if(contain_taper == 1 & !is.null(taper)){
            
          # and if the last pps has a percent positive greater than the taper_trigger_off threshold (in percent units)
          if (last_pps_percent_positive >= taper_trigger_off){
            
            # then turn containment screening back on if lots more cases detected on last 3 pps
            if(contain_pps == 1 & t < (tmax-1)){ 
              
              # adjust ts_pps frequency based on date of first trigger
              t_pps_contain <- 14
              start_contain <- sum(t, t_pps_contain)  # restart containment 14 days after last PPS rather than using the random selected delay
              if(start_contain < tmax){
                if(p_pps_start > 0 & p_pps_start < 1){ 
                  p_pps <- p_pps_start
                } else {
                  p_pps <- 1 
                }
                ts_pps <- seq(start_contain, tmax, by = t_pps_contain)
                taper <- NULL
              }
              
            }
            # turn on containment admission screening
            if(contain_a == 1){ 
              p_a <- 1
            }
            
            # otherwise if at the end of the the taper period and last pps has percent positive that is less than the taper_trigger_off threshold (in percent units)
          } else if (t == taper_end & last_pps_percent_positive < taper_trigger_off){
            
            # then turn off the taper after finishing the taper sequence and no new containment is needed
            taper <- NULL
            ts_pps <- NULL
            p_pps <- p_pps_start
            p_a <- p_a_start
            
          }
          
          

        } else if(contain_criteria <= containment_trigger_off & contain_taper == 0){
          
          # turn off containment screening (when taper is not turned on)
          if(p_pps_start > 0){
            ts_pps <- seq(t, tmax, by = t_pps)
            p_pps <- p_pps_start
          } else{
            ts_pps <- NULL
            p_pps <- 0
          }
          
          p_a <- p_a_start 
          
        }
        
      }
      
    }
      
    
    
    
    # Step 5: Update data tables for tracking results
    
    # data table for tracking screening results
    if(!is.null(admit_screened_ids) | !is.null(pps_ids)) {
      screen_res <- rbind(screen_res, 
                          data.table(patient_id = c(admit_screened_ids, pps_ids), 
                                     tstep_screened = t, 
                                     status = c(admit_status, pps_status),
                                     type = c(rep("admission", length(admit_status)), 
                                              rep("pps", length(pps_status))),
                                     seed = seed,
                                     unique_id = unique_id))
    }
    
    
    # data table for tracking clinical results
    if(!is.null(clinical_case_ids)) {
      clinical_res <- rbind(clinical_res, 
                            data.table(tstep = t, 
                                       patient_id = clinical_case_ids, 
                                       invasive = new_invasive_infections,
                                       seed = seed,
                                       unique_id = unique_id))
    }
    
    
    # data table for storing patient admit and discharge dates
    if(sum(discharged) > 0) {
      admit_res[patient_id %in% discharged_ids]$discharge_tstep <- t
    }
    
    
    if(admits > 0) {
      admit_res <- rbind(admit_res, 
                         data.table(patient_id = admit_ids, 
                                    admit_tstep = t, 
                                    discharge_tstep = 0L, 
                                    seed = seed,
                                    unique_id = unique_id))
    }
    

    
    # data table for tracking #s in each state daily
    state_res <- rbind(state_res, 
                       data.table(tstep = t, S = sum(!colonized), C = sum(colonized), 
                                  C_s = sum(detected), C_tot = colonized_total, C_s_tot = colonized_screened_total,
                                  I_clinical = sum(clinical_cases), I = sum(invasive_infections), I_tot = invasive_infections_total, 
                                  Clinical_tot = clinical_cases_total, seed = seed, unique_id = unique_id))
                                  
    
    # update result trackers with daily results
    new_patient_index <- n_patients_total + 1
    
    # reset to nulls
    admit_ids <- admit_screened_ids <- pps_ids <- admit_status <- pps_status <- discharged_ids <- clinical_case_ids <- invasive_infection_ids <- extra_los <- NULL

  } # end time series for loop
  
  # clean up results ----
  screen_res <- screen_res[screen_res$patient_id != 0, ]
  clinical_res <- clinical_res[clinical_res$patient_id != 0, ]
  admit_res$discharge_tstep[ admit_res$discharge_tstep == 0 ] <- NA
  
  return(list(screen_res = screen_res, clinical_res = clinical_res, state_res = state_res, admit_res = admit_res))



} # end function





# iterate the stochastic simulation model N times ----


#' Run individual model with different parameter combinations split by each row (1 parameter set with nsim simulations)
#'
#' This function runs the individual-level stochastic model using the sim_fac_individuals() function
#' in parallel using furrr for a data frame of parameter combinations, where the data frame 
#' is split based on the unique set seeds (each row of the parameter df) to split up
#' the work among the different cores.
#'
#'
#' @param params_df A data frame where each row is a different parameter combination to run
#' @param data data frame with LOS and percent of patients that had each possible LOS from CMS data. 
#'    The data frame also includes the data year, location, and facility type. We are using 2019 data from CA only.
#' @param initial_ind_state vector of initial states (numbers of patients)
#'    S = all susceptible patients, 
#'    C = all colonized patients, 
#'    C_s = colonized detected/screened patients only
#' @param t_pps_vec Vector of the sequence of PPS days if providing PPS data (need to have t_pps_manual = 1 in the model for this to work)
#' @param tmax max time step (how many time steps to simulate)
#' @param n_sim Number of simulations to run for each parameter combination
#' @n_split Number of parameter combinations to split over
#' @run_type Select an option for whether the model runs with all unique set seeds or with paired seeds for each intervention
#'    "Unique" = All unique set seeds for every simulation
#'    "Paired" = Match set seeds for each intervention using the nsim value
#'
#' @return A list that contains 4 data frames that combine all data from all simulations 
#' and parameter combinations with a unique seed column to indicate separate model runs:
#'    state_res = data frame of the number of patients in each state (S = susceptible, 
#'                C = colonized total, C_s = colonized screened/detected) at each 
#'                time point for all simulations
#'    admit_res = data frame of all admissions and discharges across all simulations
#'    screen_res = data frame of all screening results across all simulations
#'    clincial_res = data frame of all clinical results across all simulations
#'    params_df_sim = data frame of all parameter combinations used and n_sim 
#'                simulation runs with their unique seed to match to data frames above
#'
#' @require
#' library(data.table)
#' library(tidyverse)
#' library(purrr)
#' library(furrr)
#' library(future)
#'
#' @examples
#' params_df <- tidyr::expand_grid(beta = c(0.019, 0.035),     # transmission rate
#'                                 iota = c(0.01, 0.07),       # admission prevalence
#'                                 p_a = c(0, 1),              # proportion screened on admission
#'                                 p_pps = c(0, 1),            # proportion screened on pps
#'                                 p_c = 0.90,                 # sensitivity of screening, proportion of colonized that will test positive
#'                                 contain_pps = 0,            # switch for containment response pps screening
#'                                 contain_a = 0,              # switch for containment response admission screening
#'                                 contain_taper = 0,          # Switch for containment taper off instead of hard stop to PPS screening
#'                                 ipc_eff = 0.50,             # proportional reduction in transmission for colonized individuals that are detected through screening
#'                                 t_a = 1,                    # frequency of admission screening, daily
#'                                 t_pps = 60,                 # frequency of pps screening
#'                                 start_pps = 60,             # start PPS at day X (individual model only)
#'                                 t_pps_manual = 0,         # switch to turn on/off using a set t_pps frequency vs using a vector with specified pps days
#'                                 dt = 1,                     # # daily time step (individual model only)
#'                                 fac_max_cap = 100,          # maximum facility bed capacity (this creates a hard cap on bed number)
#'                                 occupancy = 0.90,           # proportion of facility occupancy (based on average occupancy for each type of facility)
#'                                 clinical_case_prob = 0.08,  # probability that a screening case (colonization) will become a clinical case
#'                                 inf_prob = 0.36,             # probability that a clinical case is an invasive infection
#'                                 avg_extra_LOS = 5,          # mean additional length of stay for patients with an invasive infection compared to all other patients
#'                                 fac_type = "LTACH") %>%     # indicate facility type to set the correct Length of Stay (LOS) sampling function based on facility LOS data
#'               mutate(param_id = 1:nrow(.),                  # add a number identifying each parameter combination
#'                      split = rep(1:(nrow(.)/2), each = 2))  # add a way to split up the parameter combinations across the different cores
#'
#' t_pps_vec <- c(1, 60, 132, 197, 215, 380, 467, 545, 723) # series of pps dates in consecutive order
#' data <- CA_LOS_data # load data with lengths of stays
#' initial_ind_state <- c(47, 1, 0)
#' nsim = 10
#'
#'
#' # plan to run the model in parallel over 16 cores
#' plan(multisession, workers =  16)
#'
#'
#'
#' # 1000 simulations takes about 140 seconds with 16 parameter combinations
#' # run model
#' individual_res <- individual_fun_rowwise(params_df, initial_ind_state, t_pps_vec, tmax, n_sim, n_split)
#' 
#' # go back to non-parallel session
#' plan(sequential)
#' 

individual_fun_rowwise <- function(params_df, 
                           data = CA_LOS_data,
                           initial_ind_state,
                           t_pps_vec = NULL,
                           tmax = 730, 
                           n_sim = 10,
                           n_split = 3,
                           run_type = "Paired"){
  


  if(run_type == "Unique") {
    # expand grid with the number of sims for each parameter combo
    params_df_sim <- params_df %>%
      mutate(param_id = 1:nrow(.),                            
             split = rep(1:(nrow(.)/n_split), each = n_split)) %>%            
      tidyr::expand_grid(., nsim = 1:n_sim) %>%
      mutate(seed = as.integer(param_id + split + row_number() * 145), # keep a unique seed for each simulation
             unique_id = 1:nrow(.)) %>%  
      select(beta:fac_type, seed, param_id:nsim, unique_id)
    
  } else if(run_type == "Paired") {
    # expand grid with the number of sims for each parameter combo
    params_df_sim <- params_df %>%
      mutate(param_id = 1:nrow(.),                            
             split = rep(1:(nrow(.)/n_split), each = n_split)) %>%            
      tidyr::expand_grid(., nsim = 1:n_sim) %>%
      mutate(seed = as.integer(nsim * 145),  # paired seed for each set of nsim simulations
             unique_id = 1:nrow(.)) %>%      # unique id for all simulations for matching 
      select(beta:fac_type, seed, param_id:nsim, unique_id)
    
  } else {
    stop("Run_type not selected. Please determine how set seeds should be selected")
  }
  
  
  # filter data to specific facility type
  data <- filter(data, facility_type == unique(params_df$fac_type), year == 2019, location == "CA")
  
  # run model in parallel
  full_ind_mod_res <- 
    params_df_sim %>% # your full parameter data frame
    split(.$unique_id) %>%  # splits it into a list for each row
    furrr::future_map(\(i) # anonymous function where i is the list input piped through to purrr
                      sim_fac_individuals(beta = i$beta[[1]],
                                          iota = i$iota[[1]],
                                          p_a = i$p_a[[1]],
                                          p_pps = i$p_pps[[1]],
                                          p_c = i$p_c[[1]],
                                          contain_pps = i$contain_pps[[1]],
                                          contain_a = i$contain_a[[1]],
                                          contain_taper = i$contain_taper[[1]],
                                          ipc_eff = i$ipc_eff[[1]],
                                          t_pps = i$t_pps[[1]],
                                          start_pps = i$start_pps[[1]],
                                          t_pps_manual =i$t_pps_manual[[1]],
                                          dt = i$dt[[1]],
                                          fac_max_cap = i$fac_max_cap[[1]],
                                          occupancy = i$occupancy[[1]],
                                          clinical_case_prob = i$clinical_case_prob[[1]],
                                          inf_prob = i$inf_prob[[1]],
                                          avg_extra_LOS = i$avg_extra_LOS[[1]],
                                          fac_type = i$fac_type[[1]],
                                          t_pps_vec = t_pps_vec,
                                          data = data,
                                          initial_ind_state = initial_ind_state,
                                          tmax = tmax,
                                          seed = i$seed[[1]],
                                          unique_id = i$unique_id[[1]]),
                      .options = furrr_options(seed = TRUE)) # ensures proper RNGS with futures 
  
  # combine rows for similar data tables across all simulations
  combined_state_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["state_res"]]))
  combined_admit_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["admit_res"]]))
  combined_screen_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["screen_res"]]))
  combined_clinical_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["clinical_res"]]))
  
  return(list(state_res = combined_state_df, admit_res = combined_admit_df, screen_res = combined_screen_df, clinical_res = combined_clinical_df, params_df_sim = params_df_sim))
}  
  

# 10 simulations with 16 parameter combinations took 224.286 seconds with 16 cores
# 100 simulations with 16 parameter combinations took 144.54 seconds
# 1000 simulations with 16 parameter combinations took 873.661 seconds (14.5 min) with 16 cores




#' Run individual model with a single simulation of a parameter set
#'
#' This function runs the individual-level stochastic model using the sim_fac_individuals() function
#' in parallel using furrr for a data frame with one row of parameter conditions and a
#' set seed is specified
#'
#'
#' @param params_df A data frame with one row  of all the parameters to run
#' @param data data frame with LOS and percent of patients that had each possible LOS from CMS data. 
#'    The data frame also includes the data year, location, and facility type. We are using 2019 data from CA only.
#' @param initial_ind_state vector of initial states (numbers of patients)
#'    S = all susceptible patients, 
#'    C = all colonized patients, 
#'    C_s = colonized detected/screened patients only
#' @param t_pps_vec Vector of the sequence of PPS days if providing PPS data (need to have t_pps_manual = 1 in the model for this to work)
#' @param tmax max time step (how many time steps to simulate)
#' @param n_sim Number of simulations to run for each parameter combination
#'
#' @return A list that contains 4 data frames that combine all data from all simulations 
#' and parameter combinations with a unique seed column to indicate separate model runs:
#'    state_res = data frame of the number of patients in each state (S = susceptible, 
#'                C = colonized total, C_s = colonized screened/detected) at each 
#'                time point for all simulations
#'    admit_res = data frame of all admissions and discharges across all simulations
#'    screen_res = data frame of all screening results across all simulations
#'    clincial_res = data frame of all clinical results across all simulations
#'    params_df_sim = data frame of all parameter combinations used and n_sim 
#'                simulation runs with their unique seed to match to data frames above
#'
#' @require
#' library(data.table)
#' library(tidyverse)
#' library(purrr)
#' library(furrr)
#' library(future)
#'
#' @examples
#' params_df <- tidyr::expand_grid(beta = c(0.019, 0.035),     # transmission rate
#'                                 iota = c(0.01, 0.07),       # admission prevalence
#'                                 p_a = c(0, 1),              # proportion screened on admission
#'                                 p_pps = c(0, 1),            # proportion screened on pps
#'                                 p_c = 0.90,                 # sensitivity of screening, proportion of colonized that will test positive
#'                                 contain_pps = 0,            # switch for containment response pps screening
#'                                 contain_a = 0,              # switch for containment response admission screening
#'                                 contain_taper = 0,          # Switch for containment taper off instead of hard stop to PPS screening
#'                                 ipc_eff = 0.50,             # proportional reduction in transmission for colonized individuals that are detected through screening
#'                                 t_a = 1,                    # frequency of admission screening, daily
#'                                 t_pps = 60,                 # frequency of pps screening
#'                                 start_pps = 60,             # start PPS at day X (individual model only)
#'                                 t_pps_manual = 0,           # switch to turn on/off using a set t_pps frequency vs using a vector with specified pps days
#'                                 dt = 1,                     # # daily time step (individual model only)
#'                                 fac_max_cap = 100,          # maximum facility bed capacity (this creates a hard cap on bed number)
#'                                 occupancy = 0.90,           # proportion of facility occupancy (based on average occupancy for each type of facility)
#'                                 clinical_case_prob = 0.08,  # probability that a screening case (colonization) will become a clinical case
#'                                 inf_prob = 0.5,             # probability that a clinical case is an invasive infection
#'                                 avg_extra_LOS = 5,          # mean additional length of stay for patients with an invasive infection compared to all other patients
#'                                 fac_type = "LTACH")         # indicate facility type to set the correct Length of Stay (LOS) sampling function based on facility LOS data
#'
#' t_pps_vec <- c(1, 60, 132, 197, 215, 380, 467, 545, 723) # series of pps dates in consecutive order
#' data <- CA_LOS_data # load data with lengths of stays
#' initial_ind_state <- c(47, 1, 0)
#' 
#' # run model
#' individual_res <- individual_fun_single(params_df, initial_ind_state, t_pps_vec, tmax, n_sim)
#' 
#' 

individual_fun_single <- function(params_df, 
                                  data = CA_LOS_data,
                                  initial_ind_state, 
                                  t_pps_vec = NULL,
                                  tmax = 730, 
                                  n_sim = 1,
                                  seed = 1234){
  
  params_df_sim <- params_df %>% data.frame(params_df, nsim = 1:n_sim) %>%
    mutate(nsim = nsim,
           seed = seed) %>%
    select(beta:fac_type, seed, nsim)

  
  # filter data to specific facility type
  data <- filter(data, facility_type == unique(params_df$fac_type), year == 2019, location == "CA")
  
  # run model in parallel
  full_ind_mod_res <- 
    params_df_sim %>% # your full parameter data frame
    split(.$seed) %>%  # splits it into a list for each row
    furrr::future_map(\(i) # anonymous function where i is the list input piped through to purrr
                      sim_fac_individuals(beta = i$beta[[1]],
                                          iota = i$iota[[1]],
                                          p_a = i$p_a[[1]],
                                          p_pps = i$p_pps[[1]],
                                          p_c = i$p_c[[1]],
                                          contain_pps = i$contain_pps[[1]],
                                          contain_a = i$contain_a[[1]],
                                          contain_taper = i$contain_taper[[1]],
                                          ipc_eff = i$ipc_eff[[1]],
                                          t_pps = i$t_pps[[1]],
                                          start_pps = i$start_pps[[1]],
                                          t_pps_manual =i$t_pps_manual[[1]],
                                          dt = i$dt[[1]],
                                          fac_max_cap = i$fac_max_cap[[1]],
                                          occupancy = i$occupancy[[1]],
                                          clinical_case_prob = i$clinical_case_prob[[1]],
                                          inf_prob = i$inf_prob[[1]],
                                          avg_extra_LOS = i$avg_extra_LOS[[1]],
                                          fac_type = i$fac_type[[1]],
                                          t_pps_vec = t_pps_vec,
                                          data = data,
                                          initial_ind_state = initial_ind_state,
                                          tmax = tmax,
                                          seed = i$seed[[1]]),
                      .options = furrr_options(seed = TRUE)) # ensures proper RNGS with futures 
  
  # combine rows for similar data tables across all simulations
  combined_state_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["state_res"]]))
  combined_admit_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["admit_res"]]))
  combined_screen_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["screen_res"]]))
  combined_clinical_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["clinical_res"]]))
  
  return(list(state_res = combined_state_df, admit_res = combined_admit_df, screen_res = combined_screen_df, clinical_res = combined_clinical_df, params_df_sim = params_df_sim))
} 










#' Run individual model with different parameter combinations in batches
#'
#' This function runs the individual-level stochastic model using the sim_fac_individuals() function
#' in parallel using furrr for a data frame of parameter combinations, where the data frame 
#' is split based on batches of parameter sets from the parameter df to split up
#' the work among the different cores. Use this function when you have a larger number of parameter combinations.
#' 
#' NOTE: This batch version is currently designed specifically for running the CSTE version of the model
#' and does not allow for the t_pps_vec parameter to specific alternate pps dates
#'
#'
#' @param params_df A data frame where each row is a different parameter combination to run
#' @param data data frame with LOS and percent of patients that had each possible LOS from CMS data. 
#'    The data frame also includes the data year, location, and facility type. We are using 2019 data from CA only.
#' @param initial_ind_state vector of initial states (numbers of patients)
#'    S = all susceptible patients, 
#'    C = all colonized patients, 
#'    C_s = colonized detected/screened patients only
#' @param t_pps_vec Vector of the sequence of PPS days if providing PPS data (need to have t_pps_manual = 1 in the model for this to work)
#' @param tmax max time step (how many time steps to simulate)
#' @param n_sim Number of simulations to run for each parameter combination
#' @param n_params_per_split Number of parameters per split. Needs to be a multiple of the 
#'        total number of parameter sets in order for the splits to be evenly distributed
#' @run_type Select an option for whether the model runs with all unique set seeds or with paired seeds for each intervention
#'    "Unique" = All unique set seeds for every simulation
#'    "Paired" = Match set seeds for each intervention using the nsim value
#'
#' @return A list that contains 4 data frames that combine all data from all simulations 
#' and parameter combinations with a unique seed column to indicate separate model runs:
#'    state_res = data frame of the number of patients in each state (S = susceptible, 
#'                C = colonized total, C_s = colonized screened/detected) at each 
#'                time point for all simulations
#'    admit_res = data frame of all admissions and discharges across all simulations
#'    screen_res = data frame of all screening results across all simulations
#'    clincial_res = data frame of all clinical results across all simulations
#'    params_df_sim = data frame of all parameter combinations used and n_sim 
#'                simulation runs with their unique seed to match to data frames above
#'
#' @require
#' library(data.table)
#' library(tidyverse)
#' library(purrr)
#' library(furrr)
#' library(future)
#'
#' @examples
#' params_df <- tidyr::expand_grid(beta = c(0.019, 0.035),     # transmission rate
#'                                 iota = c(0.01, 0.07),       # admission prevalence
#'                                 p_a = c(0, 1),              # proportion screened on admission
#'                                 p_pps = c(0, 1),            # proportion screened on pps
#'                                 p_c = 0.90,                 # sensitivity of screening, proportion of colonized that will test positive
#'                                 contain_pps = 0,            # switch for containment response pps screening
#'                                 contain_a = 0,              # switch for containment response admission screening
#'                                 contain_taper = 0,          # Switch for containment taper off instead of hard stop to PPS screening
#'                                 ipc_eff = 0.50,             # proportional reduction in transmission for colonized individuals that are detected through screening
#'                                 t_a = 1,                    # frequency of admission screening, daily
#'                                 t_pps = 60,                 # frequency of pps screening
#'                                 start_pps = 60,             # start PPS at day X (individual model only)
#'                                 dt = 1,                     # # daily time step (individual model only)
#'                                 fac_max_cap = 100,          # maximum facility bed capacity (this creates a hard cap on bed number)
#'                                 occupancy = 0.90,           # proportion of facility occupancy (based on average occupancy for each type of facility)
#'                                 clinical_case_prob = 0.08,  # probability that a screening case (colonization) will become a clinical case
#'                                 inf_prob = 0.5,             # probability that a clinical case is an invasive infection
#'                                 avg_extra_LOS = 5,          # mean additional length of stay for patients with an invasive infection compared to all other patients
#'                                 fac_type = "LTACH") %>%     # indicate facility type to set the correct Length of Stay (LOS) sampling function based on facility LOS data
#'               mutate(param_id = 1:nrow(.),                  # add a number identifying each parameter combination
#'                      split = rep(1:(nrow(.)/2), each = 2))  # add a way to split up the parameter combinations across the different cores
#'
#'
#' t_pps_vec <- c(1, 60, 132, 197, 215, 380, 467, 545, 723) # series of pps dates in consecutive order
#' data <- CA_LOS_data # load data with lengths of stays
#' initial_ind_state <- c(47, 1, 0)
#' nsim = 10
#'
#' # plan to run the model in parallel over 16 cores
#' plan(multisession, workers =  16)
#'
#' # 1000 simulations takes about 140 seconds with 16 parameter combinations
#' # run model
#' individual_res <- individual_fun_batch(params_df, initial_ind_state, t_pps_vec, tmax, n_sim, n_params_per_split, run_type)
#' 
#' # go back to non-parallel session
#' plan(sequential)
#' 

individual_fun_batch <- function(params_df, 
                           data = CA_LOS_data,
                           initial_ind_state, 
                           t_pps_vec = NULL,
                           tmax = 365, 
                           n_sim = 10,
                           n_params_per_split = 1,
                           run_type = "Paired"){
  
  if(n_params_per_split >= nrow(params_df)) stop("Too many parameters per split, will run the model sequentially")

  if(run_type == "Unique") {
    # expand grid with the number of sims for each parameter combo
    params_df_sim <- params_df %>%
      mutate(param_id = 1:nrow(.),                            
             split = ceiling(rep(1:nrow(.)/n_params_per_split))) %>%            
      tidyr::expand_grid(., nsim = 1:n_sim) %>%
      mutate(seed = as.integer(param_id + split + row_number() * 145), # keep a unique seed for each simulation
             unique_id = 1:nrow(.)) %>%  
      select(beta:fac_type, seed, param_id:nsim, unique_id)
    
  } else if(run_type == "Paired") {
    # expand grid with the number of sims for each parameter combo
    params_df_sim <- params_df %>%
      mutate(param_id = 1:nrow(.),                            
             split = ceiling(rep(1:nrow(.)/n_params_per_split))) %>%            
      tidyr::expand_grid(., nsim = 1:n_sim) %>%
      mutate(seed = as.integer(nsim * 145),  # paired seed for each set of nsim simulations
             unique_id = 1:nrow(.)) %>%      # unique id for all simulations for matching 
      select(beta:fac_type, seed, param_id:nsim, unique_id)
    
  } else {
    stop("Run_type not selected. Please determine how set seeds should be selected")
  }

  
  
  # filter data to specific facility type
  data <- filter(data, facility_type == unique(params_df$fac_type), year == 2019, location == "CA")
  
  
  # run model in parallel
  full_ind_mod_res <- 
    params_df_sim %>% # your full parameter data frame
    split(.$split) %>%  # splits it into a batch for each core based on the split column 
    furrr::future_map( function(batch){
      
      # anonymous function where i is the list input piped through to purrr
      batch_ind_mod_res <- batch %>%
        split(.$unique_id) %>%   # splits it into a list for each row 
        purrr::map(function(i){
          sim_fac_individuals(beta = i$beta[[1]],
                              iota = i$iota[[1]],
                              p_a = i$p_a[[1]],
                              p_pps = i$p_pps[[1]],
                              p_c = i$p_c[[1]],
                              contain_pps = i$contain_pps[[1]],
                              contain_a = i$contain_a[[1]],
                              contain_taper = i$contain_taper[[1]],
                              ipc_eff = i$ipc_eff[[1]],
                              t_pps = i$t_pps[[1]],
                              start_pps = i$start_pps[[1]],
                              dt = i$dt[[1]],
                              fac_max_cap = i$fac_max_cap[[1]],
                              occupancy = i$occupancy[[1]],
                              clinical_case_prob = i$clinical_case_prob[[1]],
                              inf_prob = i$inf_prob[[1]],
                              avg_extra_LOS = i$avg_extra_LOS[[1]],
                              fac_type = i$fac_type[[1]],
                              data = data,
                              initial_ind_state = initial_ind_state,
                              tmax = tmax,
                              seed = i$seed[[1]],
                              unique_id = i$unique_id[[1]])
          }) # end purrr_map
      
      # combine rows for similar data tables within each batch
      batched_state_df <- rbindlist(lapply(batch_ind_mod_res, function(x) x[["state_res"]]))
      batched_admit_df <- rbindlist(lapply(batch_ind_mod_res, function(x) x[["admit_res"]]))
      batched_screen_df <- rbindlist(lapply(batch_ind_mod_res, function(x) x[["screen_res"]]))
      batched_clinical_df <- rbindlist(lapply(batch_ind_mod_res, function(x) x[["clinical_res"]]))
      
      batch_ind_mod_res <- list(state_res = batched_state_df, admit_res = batched_admit_df, screen_res = batched_screen_df, clinical_res = batched_clinical_df, params_df_sim = params_df_sim)
      },  # end furrr_map
      .options = furrr_options(seed = TRUE)) # ensures proper RNGS with futures 
  
  
  # combine rows for similar data tables across all simulations
  combined_state_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["state_res"]]))
  combined_admit_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["admit_res"]]))
  combined_screen_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["screen_res"]]))
  combined_clinical_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["clinical_res"]]))
  
  
  # process data output 
  
  ## parameter data frame
  params_df_sim_clean <- param_intervention_labels(params_df_sim)
  
  ## state data frame
  state_df_clean <- state_df_processing(combined_state_df, params_df_sim_clean)
  avg_state_df <- state_avg_per_param_combo(state_df_clean)
  
  ## cost_effect data frame for each simulation
  cost_effect_df <- cost_effect_processing(combined_clinical_df, combined_screen_df, combined_admit_df, state_df_clean, params_df_sim_clean)
  
  # average cost_effect data 
  cost_effect_avg_df <- cost_effect_avg(cost_effect_df)
  
  # end point data frame (differences between colonization prevalence and incidence in last 180 days of simulation)
  last_6mo_prev_inc_df <- last_6mo_calc(avg_state_df)
  
  # percent positive from screening data frame
  percent_positive_screen <- percent_positive_calc(combined_screen_df, params_df_sim_clean)
  percent_pos_timeseries_df <- as.data.frame(percent_positive_screen$PPS_results)
  percent_pos_summary_df <- as.data.frame(percent_positive_screen$screen_summary)
  
  return(list(avg_state_res = avg_state_df, cost_effect_res = cost_effect_df, 
              cost_effect_avg_res = cost_effect_avg_df, last_6mo_prev_inc_res = last_6mo_prev_inc_df,
              percent_pos_timeseries_res = percent_pos_timeseries_df, percent_pos_summary_res = percent_pos_summary_df,
              params_df_sim = params_df_sim_clean))
}







