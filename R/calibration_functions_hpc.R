# Main Calibration functions

# This code includes the individual_fun_calibrate_hpc, abc_smc_hpc, and run_calibration_hpc functions
# that are designed to run on the HPC, briefly described below:

        # individual_fun_calibrate_hpc = This function runs the simulation model 
                                       # that matches the facility parameter conditions.
        # abc_smc_hpc = This function runs the approximate bayesian computation (abc) 
                     # processes to compare selected particles simulation output with 
                     # the real data to determine whether to accept a particle or not.
        # run_calibration_hpc = This function runs the whole calibration process, 
                              # including stepping through the generations of the sequential monte carlo (SMC).







#' @title Run single simulation of individual model with a data-based parameter set to get 
#' simulated PPS percent positive and number of cumulative screened colonized patients for calibration
#'
#' This function runs the individual-level stochastic model using the sim_fac_individuals() function
#' in parallel using furrr for a data frame with one row of parameter conditions that are based on 
#' real facility data to produce simulated data on percent positive from each PPS and the cumulative number of 
#' screened colonized patients for calibration. Uses a provided set seed each time the simulation
#' is run.
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
#' @return A list that contains 2 vectors with the data to simulated data to match the 
#' observed data:
#'    sim_cum_percent_pos_col = cumulative percent positive across all screening tests in the period (cumulative # screened colonized / total # tests)
#'    sim_pps_percent_positive = vector of the percent positive during each PPS in the time series from a given facility
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
#' individual_res <- individual_fun_calibrate(params_df, initial_ind_state, tmax, n_sim)
#' 
#' 

individual_fun_calibrate_hpc <- function(params_df, 
                                     data,
                                     initial_ind_state,
                                     t_pps_vec,
                                     tmax = 730, 
                                     n_sim = 1,
                                     seed = 1234){
  
  params_df_sim <- data.frame(params_df) %>%
    mutate(nsim = n_sim,
           seed = seed,
           param_id = 1) %>%
    select(beta:fac_type, seed, nsim, param_id)
  
  
  # run model for each seed
  full_ind_mod_res <- 
    params_df_sim %>% # your full parameter data frame
    split(.$seed) %>%  # splits it into a list for each row
    purrr::map(\(i) # anonymous function where i is the list input piped through to purrr
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
                                          seed = i$seed[[1]])) 
  
  # combine rows for similar data tables across all simulations
  combined_state_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["state_res"]]))
  combined_screen_df <- rbindlist(lapply(full_ind_mod_res, function(x) x[["screen_res"]]))
  

  # process data output 
  
  ## parameter data frame
  params_df_sim_clean <- param_intervention_labels_calibrate(params_df_sim)
  
  
  # condition to figure out type == pps error, save outputs
  if( !("type" %in% colnames(combined_screen_df)) ){ # if type is not in combined_screen_df column names, then save 
    print(paste0("Simulation failed for seed ", params_df_sim$seed, " with beta ", params_df_sim$beta))
    write.csv(combined_screen_df, here(paste("calibration/full_results/screen_output_", facility_name,"_period_", period_num, "_IPC_", ipc_eff, "_", fac_screened, ".csv", sep="")))
    write.csv(combined_state_df, here(paste("calibration/full_results/state_output_", facility_name,"_period_", period_num, "_IPC_", ipc_eff, "_", fac_screened, ".csv", sep="")))
    write.csv(params_df_sim_clean, here(paste("calibration/full_results/param_output_", facility_name,"_period_", period_num, "_IPC_", ipc_eff, "_", fac_screened, ".csv", sep="")))
  }
    
  ## percent positive from screening data frame
  percent_positive_screen <- percent_positive_calibrate(combined_screen_df, params_df_sim_clean)
  sim_pps_percent_positive <- percent_positive_screen$sim_pps_percent_positive
  sim_total_screened <- percent_positive_screen$sim_total_screened
  
  ## get cumulative number of screened colonized patients
  final_states <- combined_state_df %>%
    filter(tstep == tmax) %>%
    unlist()
  sim_cum_percent_pos_col <- final_states["C_s_tot"]/sim_total_screened
  names(sim_cum_percent_pos_col) <- NULL  
  
  return(list(sim_cum_percent_pos_col = sim_cum_percent_pos_col, sim_pps_percent_positive = sim_pps_percent_positive))
} 

















#' Function to run the ABC process to select accepted particles
#' 
#' Using the pre-generated beta particles generated in the run_calibration_hpc
#' function and the provided epsilon acceptance threshold values for the given generation,
#' this function will simulate the model to generate the simulated output data for 
#' cumulative screened colonized patients and percent positive on each PPS, then
#' compare it to the real facility data and if below the epsilon values accept the particle.
#' The function returns the data table of the accepted particles and the number of simulations 
#' completed.
#'
#' @param N_particles Vector of the sequential particle ids for the current batch of particles
#' @param G Vector of the current generation the length of the the current batch of particles
#' @param n Vector of the simulation number for each particle, the length of the the current batch of particles
#' @param beta_star Vector of the sampled beta values to test in the simulation, the length of the the current batch of particles.
#' @param epsilon_col Vector of the epsilon value for the cumulative colonized screened patients for the current generation, the length of the the current batch of particles
#' @param epsilon_pps Vector of the epsilon value for the percent positive on PPS for the current generation, the length of the the current batch of particles.
#' @param seed A unique set seed for each particle, the length of the the current batch of particles
#' @param match_data List with the appropriate facility and period data for the cumulative colonized screened patients and percent positive on PPS time series which will be used to compare to simulated outputs.
#' @param param_list List with the following elements:
#'          params_df = Vector with all of the key parameter elements that match the conditions of the facility to run the simulation
#'          initial_ind_state = The initial number of susceptible, colonized, screened colonized and infected in the facility at the start of the time period we are testing
#'          tmax = Maximum number of days in the current period of where a screening method was used for a facility
#'          t_pps_vec = The days that PPS was conducted in the facility durign the specified period
#'          n_sim = Sould be just 1 to indicate that we are only running a single simulation at a time
#' @param LOS_data A data frame with the Length of Stay distribution for the specific facility type, year and location that matches the facility data.
#'
#' @return Returns a list with the following elements:
#'          res.new = a data table with all the accepted particle values for beta and ipc_eff 
#'          sim_count = a vector with the total simulation count 
#' 

run_abc_hpc <- function(N_particles, 
                        G, 
                        n, 
                        beta_star, 
                        epsilon_col, 
                        epsilon_pps, 
                        seed, 
                        match_data, 
                        param_list, 
                        LOS_data = LOS_data){
  
  
  # Unlist the data frames and assign them to individual objects with names
  list2env(param_list, envir = .GlobalEnv)
  
  
  N <- length(unique(N_particles))
  
  # Empty matrices to store results (2 model parameters)
  res.new <- data.table(beta = numeric(), 
                        m = 0L,
                        dist_col = numeric(),
                        dist_pps = numeric())

  
  # Initialize vector to store sims count
  sims <- 0
  k <- 0
  
  # get data to match
  data_cum_percent_pos_col <- match_data$data_cum_percent_pos_col
  data_pps_percent_positive <- match_data$data_pps_percent_positive
  
  
  for(i in 1:N){ 
    
    
    #  Test if prior non zero (step 5)
    if(prior.non.zero(c(beta_star[i]))) {
      # Set number of accepted simulations to zero
      m <- 0
      distance <- matrix(ncol=2,nrow=N)
      
      
      # generate a random seed for the simulation run
      ind_seed <- seed[i]
      
      #update beta and ipc_eff
      params_df$beta <- beta_star[i]
      
      # run simulation
      D_star <- individual_fun_calibrate_hpc(params_df = params_df, 
                                             data = LOS_data,
                                             initial_ind_state = initial_ind_state,
                                             t_pps_vec = t_pps_vec,
                                             tmax = tmax, 
                                             n_sim = n_sim,
                                             seed = ind_seed)   
      sim_cum_percent_pos_col <- D_star$sim_cum_percent_pos_col
      sim_pps_percent_positive <- D_star$sim_pps_percent_positive
      # simulation counter
      k <- k + 1
      
      
      # Calculate distances between simulated and real data
      calc.dist <- calc_distance_RMSE(sim_cum_percent_pos_col, data_cum_percent_pos_col, sim_pps_percent_positive, data_pps_percent_positive)
      distance[i,] <- calc.dist    
      
      # If both distances are less than their tolerances,
      # then accept & store particle and calculate weights (step 9)
      if((calc.dist[1] <= epsilon_col[i]) & (calc.dist[2] <= epsilon_pps[i])){ 
        
        # if running multiple sims per particle, check if particle has already been accepted
        if(n[i] > 1){ # NOTE: May be able to remove this if statement and the second else statement for more concise code
          
          index <- which(res.new$beta == beta_star[i])
          
          # if already accepted, then just update m to reflect multiple acceptances for that particle, without adding the particle to res.new again
          if (length(index) > 0) {
            res.new[index, "m" ] <- m + 1
          } else { # otherwise, store the new accepted particle
            m <- 1
            # store results
            res.new <- rbind(res.new,
                             data.table(beta = c(beta_star[i]),
                                        m = m,
                                        dist_col = c(calc.dist[1]),
                                        dist_pps = c(calc.dist[2])))
          }
          
          
        } else{ # otherwise, store the new accepted particle
          m <- 1
          # store results
          res.new <- rbind(res.new,
                           data.table(beta = c(beta_star[i]),
                                      m = m,
                                      dist_col = c(calc.dist[1]),
                                      dist_pps = c(calc.dist[2])))
        }
      }
    } 
  } # end for loop
  
  # sim counter
  sims <- sims + k
  
  
  return(list(res.new = res.new, sim_count = sims))
}


















#' Function to run the full ABC SMC calibration process 
#'
#' Function to run the calibration process to select N particles per G generations, with n simulations per generation
#'
#' @param N The total number of particles to accept
#' @param G The total number of generations to run
#' @param n The number of simulations to run per sampled particle.
#' @param lm.low Vector of the lower limits for the priors for beta and ipc_eff
#' @param lm.upp Vector of the upper limits for the priors for beta and ipc_eff
#' @param epsilon_col Vector the length of the number of generations that specifies the descending epsilon threshold for the allowed difference in the cumulative colonized screened patients based on RMSE.
#' @param epsilon_pps Vector the length of the number of generations that specifies the descending epsilon threshold for the allowed difference in the percent positive on PPS time series based on RMSE.
#' @param match_data List with the appropriate facility and period data for the cumulative colonized screened patients and percent positive on PPS time series which will be used to compare to simulated outputs.
#' @param param_list List with the following elements:
#'          params_df = Vector with all of the key parameter elements that match the conditions of the facility to run the simulation
#'          initial_ind_state = The initial number of susceptible, colonized, screened colonized and infected in the facility at the start of the time period we are testing
#'          tmax = Maximum number of days in the current period of where a screening method was used for a facility
#'          t_pps_vec = The days that PPS was conducted in the facility durign the specified period
#'          n_sim = Sould be just 1 to indicate that we are only running a single simulation at a time
#' @param LOS_data A data frame with the Length of Stay distribution for the specific facility type, year and location that matches the facility data.
#' @param sims_max Set a maximum number of simulations for a generation that will cause the calibration process to stop once reached.
#' @param cores The number of cores used to run the calibration
#' @param generation_seed Vector of values to use as a set seed for each generation to ensure reproducibility in our calibration process.
#'
#' @return Returns a list including:
#'            total sims = total number of simulation run across all generations
#'            total acceptance rate = total acceptance rate across all generations
#'            sims_per_generation = vector of number of simulation for each generation
#'            acceptance_rate = vector of acceptance rate for each generation
#'            ESS_per_generation = vector of effective sample size (ESS) for each generation
#' 

run_calibration_hpc <- function(N, 
                                G, 
                                n, 
                                lm.low, 
                                lm.upp, 
                                epsilon_quantile,
                                epsilon_col, 
                                epsilon_pps,
                                match_data, 
                                param_list,
                                LOS_data = LOS_dist,
                                sims_max, 
                                cores, 
                                generation_seed){
  
  # Empty matrices to store results (2 model parameters)
  res.old<-matrix(ncol=1,nrow=N) 
  colnames(res.old) <- c("beta")
  
  
  # Empty vectors to store weights
  w.old<-matrix(ncol=1,nrow=N)
  w.new<-matrix(ncol=1,nrow=N)
  
  
  # Empty vector for storing number of sims, ESS acceptance rate and epsilon threshold from each generation 
  sims_per_generation <- NULL
  ESS_per_generation <- NULL
  acceptance_rate <- NULL
  epsilon_col_per_generation <- NULL
  epsilon_pps_per_generation <- NULL
  accepted_particles_per_generation <- NULL
  
  for(g in 1:G){  
    
    # Empty data table to store final accepted particles for each generation 
    total_accepted_particles <- data.table(beta = numeric(),
                                           m = 0L,
                                           dist_col = numeric(),
                                           dist_pps = numeric())
    
    # initially set N_particles_needed and sims-expected to match N to run calibration for N simulations 
    N_particles_needed <- N
    sims_expected <- N * n # expected number of sims is N particles times number of sims per parameter set
    
    
    # set sim_seeds to Null at the start of each generation
    sim_seeds <- NULL
    
    # set counter of sims in this generation to zero
    total_sims_this_gen <- 0
    
    # set m to NULL at the start of each generation
    # m records whether multiple simulations for the same particle were accepted if n sims per particle is > 1
    m <- NULL
    
    # record current epsilon thresholds
    epsilon_col_per_generation <- c(epsilon_col_per_generation, epsilon_col)
    epsilon_pps_per_generation <- c(epsilon_pps_per_generation, epsilon_pps)
    
    
    # keep running calibration until the number of accepted particles needed is zero
    while(N_particles_needed > 0){
      
      
      # at the start of a new generation, create random series of set seeds for each simulation in the generation
      if(is.null(sim_seeds)){
        
        set.seed(generation_seed[g])
        sim_seeds <- sample(1:100000000, sims_max*2, replace = F)
        
        # check if we are running out of new seeds for each simulation needed, sample more if needed
      } else if (sims_expected > length(sim_seeds)){
        set.seed(generation_seed[g])
        new_sim_seeds <- sample(1:100000000, sims_max*2, replace = F)
        sim_seeds <- c(sim_seeds, new_sim_seeds[sims_max+1:length(new_sim_seeds)])
      }
      
      # select the new subset of sim_seeds needed for updated sims_expected
      # and remove used seeds from sim_seeds vec
      sim_seeds_subset <- sim_seeds[1:sims_expected]
      sim_seeds <- sim_seeds[sims_expected+1:length(sim_seeds)]
      
      
      # step 4
      if(g == 1){
        # Sample beta and ipc_eff from prior distributions
        beta_values <- runif(sims_expected/n, min=lm.low[1], max=lm.upp[1])
        beta_star <- rep(beta_values, each = n)

        
        # data frame of params to run calibration
        cal_df <- tibble(N_particles = rep(seq(1:(sims_expected/n)), each = n),
                         G = g,
                         n = rep(1:n, sims_expected/n),
                         beta_star = beta_star, 
                         epsilon_col = epsilon_col,
                         epsilon_pps = epsilon_pps,
                         seed = sim_seeds_subset) 
        
      } else {
        #  Select N particles from previous generation
        if((sims_expected/n) > N){ sample_replace <- TRUE } else {sample_replace <- FALSE } # NOTE: Only allow Replace = T when sims_expected is greater than N
        p <- sample(seq(1,N), sims_expected/n, prob = w.old, replace = sample_replace)  
        par <- p |> purrr::map(\(x) rK(mean = res.old[x[[1]],], sigma))  # perturb the kernel for each particle
        par <- as.data.frame(do.call(rbind, par))  # convert to a data frame
        
        # select new beta and ipc_eff based on previous generation
        beta_star <- rep(par[,1], each = n)
        
        cal_df <- tibble(N_particles = rep(seq(1:(sims_expected/n)), each = n),
                         G = g,
                         n = rep(1:n, sims_expected/n),
                         beta_star = beta_star, 
                         epsilon_col = epsilon_col,
                         epsilon_pps = epsilon_pps,
                         seed = sim_seeds_subset) 
      }
      
      n_rows_per_split <- ceiling(sims_expected/cores)
      
      # initial run of N simulations
      # calibration in parallel, evenly split up N particles
      calibrate_res <- 
        cal_df %>% # your full parameter data frame
        mutate(split = ceiling(rep(1:nrow(.)/n_rows_per_split))) %>%
        split(.$split) %>%  # splits it into a list for each set of particles
        furrr::future_map(\(i) # anonymous function where i is the list input piped through to furrr
                          run_abc_hpc(N = i$N_particles,
                                      G = i$G,
                                      n = i$n,
                                      beta_star = i$beta_star,
                                      epsilon_col = i$epsilon_col,
                                      epsilon_pps = i$epsilon_pps,
                                      seed = i$seed,
                                      match_data = match_data,
                                      param_list = param_list,
                                      LOS_data = LOS_data),
                          .options = furrr_options(seed = TRUE)) # ensures proper RNGS with futures 
      
      # combine rows for similar data tables across all simulations from most recent run
      combined_accepted_particles <- rbindlist(lapply(calibrate_res, function(x) x[["res.new"]])) 
      combined_sims <- unlist(rbind(c(lapply(calibrate_res, function(x) x[["sim_count"]]))))
      
      
      # add new accepted particles to previous accepted particles, then check for duplicates
      total_accepted_particles <- rbind(total_accepted_particles, combined_accepted_particles)
      number_accepted_particles <- nrow(total_accepted_particles)
      
      # calculate total sims from this generation so far
      total_sims_this_gen <- total_sims_this_gen + sum(combined_sims)
      
      # check if zero particles were accepted in first 1000 sims, then set number_accepted_particles to 10 (should generate 100k sims_expected)
      if (number_accepted_particles < 2){
        number_accepted_particles <- 10
      }
      
      # determine rate of accepted particles and estimate how many more runs are needed to complete the generation
      sim_to_particle_ratio <- total_sims_this_gen/number_accepted_particles      # ratio of sims needed to accept one particle
      N_particles_needed <- max(N - number_accepted_particles, 0)            # number of additional particles needed to reach target N particles, or zero if target N particles achieved
      sims_expected <- ceiling(N_particles_needed * sim_to_particle_ratio) * n                  # calculate the expected additional simulations needed to achieve target N particles
      
      # EARLY RETURN if total_sims_this_gen hits sims_max to save accepted particles for this generation so far
      if(total_sims_this_gen > sims_max & nrow(total_accepted_particles) >= N){
        
        print(paste0("Calibration completed through generation ", g)) 
        print(paste0("Calibration exceeded max sims with ", total_sims_this_gen, " sims in generation ", g, " with ", nrow(total_accepted_particles), " of ", N, " particles accepted"))
        break # exit while loop
      }
      
      # EARLY RETURN if too many simulations are expected to avoid taking the time to run them all.
      if ((sims_expected > sims_max | (sims_expected + total_sims_this_gen) > 400000)){ 
        
        # save data file of accepted particles for each generation
        res.new.save <- as.data.frame(total_accepted_particles)[1:N,]
        write.csv(res.new.save, file = here(paste("calibration/full_results/results_ABC_SMC_", facility_name,"_period_", period_num, "_IPC_", ipc_eff, "_", fac_screened, "_gen_",g,".csv",sep="")), row.names=FALSE)
        
        # calculate some summary stats like ESS and acceptance rate to gauge how calibration went and efficiency
        sims_per_generation <- c(sims_per_generation, total_sims_this_gen)
        accepted_particles_per_generation <- c(accepted_particles_per_generation, number_accepted_particles)
        total_sims <- sum(sims_per_generation)
        total_num_accepted_particles <- (N * (g-1) + number_accepted_particles)
        total_acceptance_rate <- (total_num_accepted_particles/total_sims)
        
        # calculate the effective sample size (ESS) for each generation 
        ESS_per_generation <- c(ESS_per_generation, NA)
        acceptance_rate_this_gen <- (number_accepted_particles/total_sims_this_gen)
        acceptance_rate <- c(acceptance_rate, acceptance_rate_this_gen)
        
        print(paste0("Calibration completed through generation ", g-1))
        print(paste0("Expected sims ", sims_expected, " exceeded max sims ", sims_max, " in generation ", g, " with ", nrow(total_accepted_particles), " of ", N, " particles accepted"))
        return(list(total_sims = total_sims, total_acceptance_rate = total_acceptance_rate, sims_per_generation = sims_per_generation, accepted_particles_per_generation = accepted_particles_per_generation, acceptance_rate = acceptance_rate, epsilon_col_per_generation = epsilon_col_per_generation,
                    epsilon_pps_per_generation = epsilon_pps_per_generation, ESS_per_generation = ESS_per_generation, sims_expected = sims_expected))
        
      }
      
    } # end while loop  
    
    
    # once N particles accepted, then calculate the weights for all accepted particles and epsilon for next generation based on distances
    # remove extra accepted particles 
    res.new <- as.matrix(total_accepted_particles[ 1:N, 1 ]) # remove the m column from res.new
    m <- as.matrix(total_accepted_particles[1:N, "m"]) # create a vector of m values for each accepted particle
    dist <- as.matrix(total_accepted_particles[ 1:N, c("dist_col", "dist_pps") ]) # create a matrix of distance values for all accepted particles
    
    # for all accepted particles, calculate the weights (step 9)
    for (i in 1:N){
      
      # Calculate weights
      w1 <- prod(sapply(1, function(b) dunif(res.new[i,b], min=lm.low[b], max=lm.upp[b])))
      if(g==1){
        w2 <- 1
      } else {  
        w2 <- sum(sapply(1:N, function(a) w.old[a]* dtmvnorm(res.new[i,], mean=res.old[a,], sigma=sigma, lower=lm.low, upper=lm.upp)))
      }
      w.new[i] <- (m[i]/n)*w1/w2
      
    } # end calculate weights for loop
    
    # calculate epsilon for next generation based on distances
    epsilon_col <- quantile(dist[ , "dist_col"], epsilon_quantile)
    epsilon_pps <- quantile(dist[ , "dist_pps"], epsilon_quantile)
    
    
    # calculate covariance matrix   
    sigma <- cov(res.new)
    
    # store accepted particles for next generation
    res.old<-res.new
    
    # normalize weights so that the sum of the weights = 1 (step 11)
    w.old<-w.new/sum(w.new)
    
    # calculate some summary stats like ESS and acceptance rate for each generation to gauge how calibration went and efficiency
    # calculate the effective sample size (ESS) for each generation 
    ESS_per_generation <- c(ESS_per_generation, ESS(w.new))  # function from WeightIt package uses: (sum(w.new)^2 / sum(w.new^2))
    acceptance_rate_this_gen <- (number_accepted_particles/total_sims_this_gen)
    acceptance_rate <- c(acceptance_rate, acceptance_rate_this_gen)
    
    
    # add all sims and number accepted particles for the generation (across all cores) from the sim counter data frame
    sims_per_generation <- c(sims_per_generation, total_sims_this_gen)
    accepted_particles_per_generation <- c(accepted_particles_per_generation, number_accepted_particles)
    
    
    # save data file of accepted particles for each generation
    res.new.save <- as.data.frame(total_accepted_particles)[1:N,]
    write.csv(res.new.save, file = here(paste("calibration/full_results/results_ABC_SMC_", facility_name,"_period_", period_num, "_IPC_", ipc_eff, "_", fac_screened, "_gen_",g,".csv",sep="")), row.names=FALSE)
    
    
    # calculate totals so far for each generation
    total_sims <- sum(sims_per_generation)
    total_num_accepted_particles <- sum(accepted_particles_per_generation)
    total_acceptance_rate <- (total_num_accepted_particles/total_sims)
    
    
    # convert abc_stats to a dataframe to save after each completed generation
    abc_stats_df <- tibble(facility = facility_name,
                           amount_screened = fac_screened,
                           period = period_num,
                           ipc_eff = ipc_eff,
                           generation = seq(1,g),
                           total_sims = c(total_sims, rep(NA, (g-1))),
                           total_acceptance_rate = c(total_acceptance_rate, rep(NA, (g-1))),
                           sims_per_generation = sims_per_generation,
                           accepted_particles_per_generation = accepted_particles_per_generation,
                           acceptance_rate_per_generation = acceptance_rate,
                           epsilon_col_per_generation = epsilon_col_per_generation,
                           epsilon_pps_per_generation = epsilon_pps_per_generation,
                           ESS_per_generation = ESS_per_generation,
                           sims_expected = c(sims_expected, rep(NA, (g-1))))
    
    
    write.csv(abc_stats_df, here(paste("calibration/full_results/abc_stats_", facility_name,"_period_", period_num, "_IPC_", ipc_eff, "_", fac_screened, ".csv", sep="")))
    
    if(total_sims_this_gen > sims_max){
      break # exit for loop
    }
    
  } # end generation for loop
  
  
  
  print(paste0("Calibration completed for all generations"))
  return(list(total_sims = total_sims, total_acceptance_rate = total_acceptance_rate, sims_per_generation = sims_per_generation, 
              accepted_particles_per_generation = accepted_particles_per_generation, acceptance_rate = acceptance_rate, epsilon_col_per_generation = epsilon_col_per_generation,
              epsilon_pps_per_generation = epsilon_pps_per_generation, ESS_per_generation = ESS_per_generation, sims_expected = sims_expected))
  
} # end function











