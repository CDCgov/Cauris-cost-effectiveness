# Functions to manipulate data output


#' Function to add intervention labels to the parameter data frame based on specific conditions
#'
#' @param params_df_sim the data frame of all simulation's parameter combinations produced within the individual_fun series of functions
#'
#' @return Updated parameter data frame that is reorganized and includes clear labels for each intervention to group similar parameter combinations
#'
#' @examples
#' #' params_df_sim1 <- param_intervention_labels(individual_res$params_df_sim)
#' 
param_intervention_labels <- function(params_df_sim){
  
  params_output_df <- as.data.frame(params_df_sim) %>%
    mutate(intervention = factor(case_when(
      p_a == 0 & p_pps == 0 & contain_pps == 0 & contain_a == 0 ~ "No Screening",
      p_a == 1 & p_pps == 0 & contain_pps == 0 & contain_a == 0 ~ "Admissions",
      p_a == 0 & p_pps > 0 & contain_pps == 0 & contain_a == 0 & t_pps == 14 ~ "PPS (14 days)",
      p_a == 0 & p_pps > 0 & contain_pps == 0 & contain_a == 0 & t_pps == 30 ~ "PPS (30 days)",
      p_a == 0 & p_pps > 0 & contain_pps == 0 & contain_a == 0 & t_pps == 60 ~ "PPS (60 days)",
      p_a == 0 & p_pps > 0 & contain_pps == 0 & contain_a == 0 & t_pps == 90 ~ "PPS (90 days)",
      p_a == 0 & p_pps > 0 & contain_pps == 0 & contain_a == 0 & t_pps == 180 ~ "PPS (180 days)",
      p_a == 1 & p_pps > 0 & contain_pps == 0 & contain_a == 0 & t_pps == 14 ~ "Admissions + PPS (14 days)",
      p_a == 1 & p_pps > 0 & contain_pps == 0 & contain_a == 0 & t_pps == 30 ~ "Admissions + PPS (30 days)",
      p_a == 1 & p_pps > 0 & contain_pps == 0 & contain_a == 0 & t_pps == 60 ~ "Admissions + PPS (60 days)",
      p_a == 1 & p_pps > 0 & contain_pps == 0 & contain_a == 0 & t_pps == 90 ~ "Admissions + PPS (90 days)",
      p_a == 1 & p_pps > 0 & contain_pps == 0 & contain_a == 0 & t_pps == 180 ~ "Admissions + PPS (180 days)",
      p_a == 0 & p_pps == 0 & contain_pps == 1 & contain_a == 0 & contain_taper == 0 ~ "Containment PPS (14 days)",
      p_a == 0 & p_pps == 0 & contain_pps == 1 & contain_a == 1 & contain_taper == 0 ~ "Containment Admissions + PPS (14 days)",
      p_a == 0 & p_pps == 0 & contain_pps == 1 & contain_a == 0 & contain_taper == 1 ~ "Containment PPS (14 days) w/ taper",
      p_a == 0 & p_pps == 0 & contain_pps == 1 & contain_a == 1 & contain_taper == 1 ~ "Containment Admissions + PPS (14 days) w/ taper"),
      levels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", 
                 "Containment Admissions + PPS (14 days) w/ taper", "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", 
                 "PPS (30 days)", "PPS (14 days)", "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                 "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")),
      import = round(iota, 2)) %>%
    select(unique_id, seed, param_id, nsim, intervention, beta, iota, import, intervention, p_a, p_pps, contain_pps, contain_a, contain_taper, p_c, ipc_eff:fac_type, split)
  
  return(params_output_df)
}




#' Function for initial processing of the state data over time 
#' 
#' This function does the initial processing of the state data frame within the 
#' individual_fun_batch function calculate the total N, prop colonized, daily 
#' incidence, weekly incidence, monthly incidence, and IPC costs.
#'
#' @param combined_state_df The combined state data frame of time 0 to 730 from all 
#'                  of the simulations that are rbinded together
#' @param param_df The cleaned params_df_sim output from the function above that 
#'                  has the labeled interventions for each simulation's parameter set
#'
#' @return Data frame of the states over time for each simulation that have additional calculations and intervention labels added.
#'
#' @examples
#' state_df1 <- state_avg_per_param_combo(individual_res$state_res, params_df_sim1)
#' 
#' 
state_df_processing <- function(combined_state_df, param_df){
  
  # make a smaller subset of the params to join to other dfs
  params_id_df <- param_df %>%
    select(unique_id, seed, beta, iota, import, intervention, p_a, p_pps, contain_pps, contain_a, contain_taper, param_id, nsim)
  
  # add week, month indicators, calculate total N, prop colonized, daily incidence, and IPC costs
  state_df <- as.data.frame(combined_state_df) %>%
    full_join(params_id_df, by = c("unique_id", "seed")) %>%
    dplyr::rename("time" = "tstep") %>%
    mutate(N = S + C, C_u = C - C_s, prop_colonized = C/N,  # calculate total N, and undetected colonized patients (C_u), and proportion of colonized patients
           daily_col_incidence = if_else(time == 0, initial_ind_state["C"], C_tot - lag(C_tot, default = first(C_tot))), # calculate the number of new colonization cases each day (daily incidence)
           week = if_else(time == 0, 0, (time - 1) %/% 7 + 1), # find the week for each time point, where time = 0 is week 0
           month = if_else(time == 0, 0, (time - 1) %/% 30 + 1), # find the month for each time point (assuming a 30 day month), where time = 0 is month 0
           IPC_cost = C_s * 46.83) %>%  # estimated $46.83 additional cost per patient receiving IPC per day
    select(time, week, month, S, C_u, C_s, C, C_tot, C_s_tot, I_clinical, I, I_tot, Clinical_tot, N, prop_colonized, daily_col_incidence, IPC_cost, unique_id, seed, beta, iota, import, intervention, p_a, p_pps, contain_pps, contain_a, param_id, nsim)
  
  
  # calculate the weekly incidence for each simulation
  week_inc_df <- state_df %>%
    group_by(beta, iota, intervention, unique_id, seed, week) %>%
    summarize(weekly_col_incidence = sum(daily_col_incidence))
  
  # calculate the weekly incidence for each simulation
  month_inc_df <- state_df %>%
    group_by(beta, iota, intervention, unique_id, seed, month) %>%
    summarize(monthly_col_incidence = sum(daily_col_incidence))
  
  # join the weekly and monthly incidence to the state_df
  state_df_clean <- state_df %>%
    left_join(., week_inc_df, by = c("beta", "iota", "intervention", "unique_id", "seed", "week")) %>%
    left_join(., month_inc_df, by = c("beta", "iota", "intervention", "unique_id", "seed", "month")) %>%
    select(time:daily_col_incidence, weekly_col_incidence, monthly_col_incidence, IPC_cost:nsim)
  
  
  return(state_df_clean)
}









#' Function average the state data over time for each parameter combination
#' 
#' This function processes the state data frame within the individual_fun_batch function
#' to reduce the number of rows and average across all 1000 simulations for a given 
#' parameter set of each value for each time step.
#'
#' @param state_df_clean The cleaned state data frame of time 0 to 730 from all 
#'                  of the simulations that are rbinded together
#'
#' @return Data frame of average values for each time point for each parameter combination
#'
#' @examples
#' state_df1 <- state_avg_per_param_combo(individual_res$state_res, params_df_sim1)
#' 
#' 
state_avg_per_param_combo <- function(state_df_clean){
  
  # calculate the average across all 1000 simulations for a parameter set of each value for each time step 
  avg_state_df <- 
    state_df_clean %>% 
    mutate(across(c("C_u", "C_s", "S", "C",), ~.x/N)) %>% # turn these into proportions
    dplyr::group_by(import, beta, intervention, time) %>%
    summarize(across(c("N", "C_u", "C_s", "S", "C", "prop_colonized", "C_tot", "C_s_tot", "I_clinical", "I", "I_tot", "Clinical_tot", "daily_col_incidence", "weekly_col_incidence", "monthly_col_incidence", "IPC_cost"), 
                     list(mean = mean, 
                          upper = \(x) quantile(x, 0.975), 
                          lower = \(x) quantile(x, 0.025)))) %>%
    mutate(week = if_else(time == 0, 0, (time - 1) %/% 7 + 1), # find the week for each time point, where time = 0 is week 0
           month = if_else(time == 0, 0, (time - 1) %/% 30 + 1), # find the month for each time point (assuming a 30 day month), where time = 0 is month 0
           beta_import = paste0("Beta = ", beta,", Import = ", import)) %>%
    select(beta:time, week, month, N_mean:beta_import) %>%
    arrange(desc(beta), desc(import), .by_group = TRUE)
  
  return(avg_state_df)
}









#' Generate totals of costs and effects for each simulation
#'
#' @param combined_clinical_df Data table with each individual's clinical test date and result from all simulations
#' @param combined_screen_df Data table with each individual's screening date, type, and result from all simulations
#' @param combined_admit_df Data table with each individual's admission and discharge date from all simulations
#' @param state_df_clean The cleaned state data frame of time 0 to 730 from all 
#'                       of the simulations that are rbinded together.
#' @param param_df Data table with all of the details of of each parameter combination
#'
#' @return Data table where there is a row for each simulation that summarizes the 
#'    main effects (cumulative colonization, clinical cases, and infections) and the
#'    total costs for screening, clinical tests, and IPC and the summed total cost.
#'
cost_effect_processing <- function(combined_clinical_df, combined_screen_df, combined_admit_df, state_df_clean, param_df){
  
  # make a smaller subset of the params to join to other dfs
  params_id_df <- param_df %>%
    select(unique_id, seed, beta, iota, import, intervention, p_a, p_pps, contain_pps, contain_a, contain_taper, param_id, nsim)
  
  
  # calculate number of admits and mean LOS
  admit_df <- as.data.frame(combined_admit_df) %>%
    mutate(LOS = discharge_tstep - admit_tstep) %>%
    group_by(unique_id) %>%
    summarize(n_admit = n(),
              mean_LOS = mean(LOS, na.rm = T))
  
  
  # calculate cost of clinical tests
  clincial_cost_df <- as.data.frame(combined_clinical_df) %>%
    group_by(unique_id) %>%
    count() %>%
      mutate(clinical_cost = n*65.26) %>% # cost for culture and AFST
    rename(n_clinical_tests = "n") %>%
    select(unique_id, n_clinical_tests, clinical_cost)
  
  
  # calculate the costs of screening tests
  screen_cost_df <- as.data.frame(combined_screen_df) %>%
    group_by(unique_id, status, type) %>%
    count() %>%
    mutate(screen_cost = case_when(
      status == 0 ~ 23.04 * n,        # negative tests are cheaper because only include PCR
      status == 1 ~ 35.60 * n)) %>%   # positive tests are more expensive because include PCR and culture
    ungroup() %>%
    group_by(unique_id, type) %>%
    summarize(screen_cost = sum(screen_cost), 
              swab = sum(n)) %>%
    pivot_wider(names_from = type, values_from = c(screen_cost, swab)) %>%
    rename(screen_cost_admit = screen_cost_admission, swab_admit = swab_admission) %>%
    mutate(screen_cost_pps = if_else(is.na(screen_cost_pps), 0, screen_cost_pps),
           screen_cost_admit = if_else(is.na(screen_cost_admit), 0, screen_cost_admit),
           swab_pps = if_else(is.na(swab_pps), 0, swab_pps),
           swab_admit = if_else(is.na(swab_admit), 0, swab_admit),
           screen_cost = screen_cost_pps + screen_cost_admit,
           swab_tot = swab_pps + swab_admit)
  
  
  # calculate costs of IPC per patient per day
  IPC_cost_df <- state_df_clean %>%
    group_by(unique_id) %>%
    summarize(IPC_cost = sum(IPC_cost),
              patient_days_on_IPC = sum(C_s),
              C_tot = max(C_tot), 
              C_s_tot = max(C_s_tot),
              Clinical_tot = max(Clinical_tot),
              I_tot = max(I_tot))
  
  # combine screening and IPC cost dfs into one
  cost_effect_df <- full_join(IPC_cost_df, clincial_cost_df) %>%
    full_join(., screen_cost_df, by = c("unique_id")) %>%
    full_join(., admit_df, by = c("unique_id")) %>%
    arrange(unique_id) %>%
    mutate(screen_cost = if_else(is.na(screen_cost), 0, screen_cost),
           swab_tot = if_else(is.na(swab_tot), 0, swab_tot),
           clinical_cost = if_else((is.na(clinical_cost) & Clinical_tot == 0), 0, clinical_cost),
           n_clinical_tests = if_else((is.na(n_clinical_tests) & Clinical_tot == 0), 0, n_clinical_tests),
           total_cost = IPC_cost + clinical_cost + screen_cost) %>%
    full_join(.,params_id_df, by = c("unique_id")) %>%
    select(unique_id, seed, beta, import, intervention, C_tot, C_s_tot, Clinical_tot, I_tot, swab_tot, swab_pps, swab_admit, n_clinical_tests, patient_days_on_IPC, 
           n_admit, mean_LOS, screen_cost, screen_cost_pps, screen_cost_admit, clinical_cost, IPC_cost, total_cost, iota, p_a:nsim)
  
  
  no_screen_cost_effect_df <- cost_effect_df %>%
    filter(intervention == "No Screening") %>%
    rename_with(~paste0("ns_", .x, recycle0 = TRUE), .cols = c(C_tot:swab_tot, screen_cost:total_cost)) %>%
    select(seed, beta, import, contains("ns_"))
  
  cost_effect_df <- cost_effect_df %>%
    left_join(., no_screen_cost_effect_df, by = c("seed", "beta", "import")) %>%
    mutate(undetected_col_cases = C_tot - C_s_tot,
           col_cases_averted = ns_C_tot - C_tot,
           clinical_cases_averted = ns_Clinical_tot - Clinical_tot,
           invasive_cases_averted = ns_I_tot - I_tot,
           delta_swab_total = swab_tot - ns_swab_tot,
           delta_screen_cost = screen_cost - ns_screen_cost,
           delta_clinical_cost = clinical_cost - ns_clinical_cost,
           delta_IPC_cost = IPC_cost- ns_IPC_cost,
           delta_total_cost = total_cost - ns_total_cost,
           RRR_col = col_cases_averted/ns_C_tot,
           RRR_clinical = clinical_cases_averted/ns_Clinical_tot,
           RRR_invasive = invasive_cases_averted/ns_I_tot)
  
  return(cost_effect_df)
}



# function to calculate the standard error
standard_error <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
}



#' Average the cost_effect data frame for each intervention, transmission and importation rate combination
#'
#' @param cost_effect_df Data frame with each row summarizing the main effects and costs for each simulation
#'
#' @return Data frame where simulations have been averaged together based on the intervention, 
#'    transmission, and importation rate combination that includes the main effects and costs
#' 
cost_effect_avg <- function(cost_effect_df){
  
  
  # calculate the mean across all sims for each parameter combination
  cost_avg_df <- cost_effect_df %>%
    group_by(import, beta, intervention) %>%
    summarize(across(c(C_tot, C_s_tot, Clinical_tot, I_tot,
                       swab_tot, patient_days_on_IPC,
                       IPC_cost, n_clinical_tests,
                       clinical_cost, screen_cost,
                       total_cost, col_cases_averted, clinical_cases_averted,
                       invasive_cases_averted, invasive_cases_averted, 
                       delta_swab_total, delta_screen_cost, delta_clinical_cost, 
                       delta_IPC_cost, delta_total_cost, RRR_col, RRR_clinical,
                       RRR_invasive), 
                     list(mean = ~ mean(.x, na.rm = TRUE),
                          median = ~ median(.x, na.rm = TRUE),
                          se = ~ standard_error(.x),
                          lower = ~ quantile(.x, 0.025, na.rm = TRUE),
                          upper = ~ quantile(.x, 0.975, na.rm = TRUE)),
                     .names = "{.col}_{.fn}")) %>%
    ungroup() %>%
    mutate(ICER_total = delta_total_cost_mean/col_cases_averted_mean,
           ICER_screen = delta_screen_cost_mean/col_cases_averted_mean,
           screen_efficiency = delta_swab_total_mean/col_cases_averted_mean)

  return(cost_avg_df)
}





#' Calculate mean and upper/lower confidence intervals for last 6 months of time series
#'
#' @param avg_state_df Data frame of average values for each time point for each parameter combination
#'
#' @return Data frame with the mean, upper and lower confidence intervals of prevalence 
#'         and monthly incidence over the last 6 months of time series
#'
last_6mo_calc <- function(avg_state_df){
  
  
  # get the mean of the last 180 days rather than just the end point because 
  # the scenarios with PPS can vary a lot depending on the part of the wave that you capture at the end point
  avg_last_6mo_df <- avg_state_df %>%
    filter(time > (max(time)-180)) %>%  # filter to the last 180 days of the simulation
    select(beta:time, prop_colonized_mean, prop_colonized_upper, prop_colonized_lower, monthly_col_incidence_mean, monthly_col_incidence_upper, monthly_col_incidence_lower) %>%
    group_by(import, beta, intervention) %>%
    summarize(across(prop_colonized_mean:monthly_col_incidence_lower, \(x) mean(x, na.rm = TRUE))) %>%
    ungroup()
  
  
  no_screen_avg_last_6m_df <- avg_last_6mo_df %>%
    filter(intervention == "No Screening") %>%
    rename_with(~paste0("ns_", .x, recycle0 = TRUE), .cols = c(prop_colonized_mean:monthly_col_incidence_lower)) %>%
    select(-intervention)
  
  avg_last_6mo_df <- avg_last_6mo_df %>%
    left_join(., no_screen_avg_last_6m_df, by = c("import", "beta")) %>%
    mutate(colonization_change = ns_prop_colonized_mean - prop_colonized_mean,
           percent_reduction_colonization = 100*(ns_prop_colonized_mean - prop_colonized_mean)/ns_prop_colonized_mean,
           monthly_inc_change = ns_monthly_col_incidence_mean - monthly_col_incidence_mean,
           percent_reduction_monthly_inc = 100*(ns_monthly_col_incidence_mean - monthly_col_incidence_mean)/ns_monthly_col_incidence_mean)
  
  
  return(avg_last_6mo_df)
}




#' Calculate the percent positive from PPS and admissions screening
#'
#' @param screen_df Data frame of PPS and admission screening results for each simulation
#' @param param_df Data table with all of the details of of each parameter combination
#' 
#' @return A list of two data frames:
#'            1. Contains the summarized results for each PPS conducted, with the raw percent positivity
#'                and rolling mean and rolling sum of percent positivity over time for each simulation
#'            2. A summary of the 1 year and 2 year percent positive from PPS and admissions screening for each simulation
#'
#' 
percent_positive_calc <- function(combined_screen_df, param_df){
  
  # make a smaller subset of the params to join to other dfs
  params_id_df <- param_df %>%
    select(unique_id, seed, beta, iota, import, intervention, p_a, p_pps, t_pps, contain_pps, contain_a, contain_taper, param_id, nsim)
  
  
  
  # calculate the percent positivity for PPS and admission at 1 yr and 2 yrs
  PPS_results <- combined_screen_df %>%
    filter(type == "pps") %>%
    group_by(unique_id, seed, tstep_screened, status, type) %>%
    count() %>%
    pivot_wider(id_cols = unique_id:tstep_screened, names_from = status, names_prefix = "n_", values_from = n) %>%
    full_join(., params_id_df, by = c("unique_id", "seed")) %>%
    group_by(unique_id) %>%
    mutate(last_date = if_else(unique_id == lag(unique_id), lag(tstep_screened), NA),
           days_since_last_PPS = as.numeric(tstep_screened - last_date),
           n_0 = if_else(is.na(n_0), 0, n_0),
           n_1 = if_else(is.na(n_1), 0, n_1),
           n_total = n_0+n_1,
           percent_positive = (n_1/n_total)*100,
           rollmean_last6PPS = zoo::rollmean(percent_positive, k = 6, fill = NA, align = "right"),
           rollsum_last6PPS = zoo::rollsum(n_1, k = 6, fill = NA, align = "right")/zoo::rollsum(n_total, k = 3, fill = NA, align = "right")*100,
           rollmean_last3PPS = zoo::rollmean(percent_positive, k = 3, fill = NA, align = "right"),
           rollsum_last3PPS = zoo::rollsum(n_1, k = 3, fill = NA, align = "right")/zoo::rollsum(n_total, k = 3, fill = NA, align = "right")*100,
           pps_round = seq(length(tstep_screened))) %>%
    select(unique_id, seed, pps_round, tstep_screened, days_since_last_PPS, n_0, n_1, n_total:rollsum_last3PPS, beta:nsim)
    
  
  # calculate the PPS screening summary at 1 yr
  PPS_results_1yr <- PPS_results %>%
    select(unique_id, seed:percent_positive) %>%
    filter(tstep_screened <= tmax/2) %>%
    group_by(unique_id) %>%
    summarize(n_0 = sum(n_0),
              n_1 = sum(n_1),
              n_total = sum(n_0, n_1, na.rm = T),
              mean_percent_positive = mean(percent_positive),
              overall_percent_positive = (n_1/n_total)*100,
              end_point = tmax/2,
              type = "pps") %>%
    select(unique_id, end_point, type, n_0:overall_percent_positive)
  
  # calculate the PPS screening summary at 2 yrs
  PPS_results_2yr <- PPS_results %>%
    group_by(unique_id) %>%
    summarize(n_0 = sum(n_0),
              n_1 = sum(n_1),
              n_total = sum(n_0, n_1, na.rm = T),
              mean_percent_positive = mean(percent_positive),
              overall_percent_positive = (n_1/n_total)*100,
              end_point = tmax,
              type = "pps") %>%
    select(unique_id, end_point, type, n_0:overall_percent_positive)
  
  # rbind the pps screening summaries at 1 yr and 2 yrs together
  PPS_results_summary <- rbind(PPS_results_1yr, PPS_results_2yr)
  
  
  # calculate the admission screening summary at 1 yr
  admit_results_1yr <- combined_screen_df %>%
    filter(type == "admission", tstep_screened >= tmax/2) %>%
    group_by(unique_id, status, type) %>%
    count() %>%
    pivot_wider(names_from = status, names_prefix = "n_", values_from = n) %>%
    mutate(n_total = sum(n_0, n_1, na.rm = T),
           mean_percent_positive = NA,
           overall_percent_positive = (n_1/n_total)*100,
           end_point = tmax/2)
  
  # calculate the admission screening summary at 2 yrs
  admit_results_2yr <- combined_screen_df %>%
    filter(type == "admission") %>%
    group_by(unique_id, status, type) %>%
    count() %>%
    pivot_wider(names_from = status, names_prefix = "n_", values_from = n) %>%
    mutate(n_total = sum(n_0, n_1, na.rm = T),
           mean_percent_positive = NA,
           overall_percent_positive = (n_1/n_total)*100,
           end_point = tmax)
  
  # rbind the admission screening summaries at 1 yr and 2 yrs together
  admit_results_summary <- rbind(admit_results_1yr, admit_results_2yr) %>%
    select(unique_id, end_point, type:overall_percent_positive)
  
  # rbind the pps and admission screening summaries at 1 yr and 2 yrs together
  screen_summary <- rbind(PPS_results_summary, admit_results_summary) %>%
    arrange(unique_id, type, end_point) %>%
    full_join(., params_id_df, by = c("unique_id"))
  
  
  return(list(PPS_results = PPS_results, screen_summary = screen_summary))
}

