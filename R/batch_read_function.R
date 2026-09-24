
# libraries
library(here)
library(tidyverse)



#' Read in batch of csv files and manipulate into one data frame
#'
#' Function to read in a batch of csv files and add columns based on the parts of the file name
#' to differentiate the different data sets, then combine into one data frame or one list.
#'
#' @param file.list List of file pathways to csv files to be read in and combined
#' @param format Output format as a single "dataframe" or "list" with each file 
#'        as an element in the list
#'
#' @return a data frame with all of the files combined into a single dataframe, or 
#'          a list where each file is a separate element of the list
#' 
#' @require
#' library(here)
#' library(tidyverse)
#'
#' @examples
#' # List all the CSV files in the folder
#' file.list <- list.files(here("data/CMS_CA_LOS_data"), pattern = "*.csv", full.names = TRUE)
#' 
#' LOS_data <- read_batch_csv_fun(file.list = file.list, format = "dataframe")
#' 

read_batch_csv_fun = function(file.list, format = "dataframe") {
  
  df_list = list()
  # Read each file and store it in a list
  for (file in file.list) {
    # Read each file and store it in a data frame
    df <- read.csv(file, header = T)
    
    # pull out the location, facility type, and year from the file name
    pattern <- "(\\w+)_(\\w+)_(\\d{4})"
    matches <- regmatches(file, regexpr(pattern, file, perl = TRUE))
    location_result <- toupper(gsub(pattern, "\\1", matches[1]))  # "ca"
    facility_result <- toupper(gsub(pattern, "\\2", matches[1]))  # "ltach"
    year_result <- gsub(pattern, "\\3", matches[1])  # "2019"
    
    # add those as a new columns to the df
    df <- df %>% 
      mutate(location = location_result,
             facility_type = facility_result,
             year = year_result) %>%
      dplyr::rename(percent = PERCENT)
    
    # Append the data frame to the list
    df_list[[matches]] <- df
  }
  
  if(format == "dataframe"){
    # Combine all the data frames in the list into one dataframe
    append_df <- bind_rows(df_list)
    return(append_df)
  } else if(format == "list"){
    return(df_list)
  }
  
}



read_batch_cal_res_fun = function(file.list, N) {
  
  df_list = list()
  # Read each file and store it in a list
  for (file in file.list) {
    # Read each file and store it in a data frame
    df <- read.csv(file, header = T)
    
    
    if(grepl("vent", file) | grepl("_full_", file)){
      # pull out the location, facility type, and year from the file name
      pattern <- "results_ABC_SMC_(.*?)_period_(\\d+)_IPC_([\\d.]+)_(.*?)_gen_(\\d+)\\.csv"
      matches <- str_match(file, pattern)
      facility_result <- matches[1, 2] # LTACH_A
      period_result <- matches[1, 3]  # period
      ipc_result <- matches[1, 4]  # ipc_eff value
      amount_result <- matches[1, 5]  # full facility or vent floor
      gen_result <- matches[1, 6] # generation
    } else{
      # pull out the location, facility type, and year from the file name
      pattern <- "results_ABC_SMC_(.*?)_period_(\\d+)_IPC_([\\d.]+)_gen_(\\d+)\\.csv"
      matches <- str_match(file, pattern)
      facility_result <- matches[1, 2] # LTACH_A
      period_result <- matches[1, 3]  # period
      ipc_result <- matches[1, 4]  # ipc_eff value
      amount_result <- "full"  # all are full facility
      gen_result <- matches[1, 5] # generation
      
    }

    # remove any NAs from the data if the generation is incomplete
    if(sum(is.na(df$beta)) > 0) {
      df <- filter(df, !is.na(beta)) 
    }
    
    # check if N particles reached
    n_particles <- nrow(df)
    
    # add those as a new columns to the df
    df <- df %>% 
      mutate(facility_name = facility_result,
             amount_screened = amount_result,
             period = period_result, 
             ipc_eff = ipc_result,
             G = gen_result,
             completed = if_else(n_particles == N, T, F),
             period_labs = case_when(
               period == 1 ~ "Period 1: PPS only",
               period == 2 ~ "Period 2: Admission + PPS"
             ))
    

    # Append the data frame to the list
    df_list[[ matches[ 1, 1] ]] <- df
    
    
  } # end for loop
  
  # Combine all the data frames in the list into one data frame
  append_df <- bind_rows(df_list)
  return(append_df)
  
}




read_batch_cal_abc_stats_fun = function(file.list) {
  
  df_list = list()
  # Read each file and store it in a list
  for (file in file.list) {
    # Read each file and store it in a data frame
    df <- read.csv(file, header = T)
    
    
    if(grepl("vent", file) | grepl("_full", file)){
      # pull out the location, facility type, and year from the file name
      pattern <- "abc_stats_(.*?)_period_(\\d+)_IPC_([\\d.]+)_(.*?).csv"
      matches <- str_match(file, pattern)
      facility_result <- matches[1, 2] # LTACH_A
      period_result <- matches[1, 3]  # period
      ipc_result <- matches[1, 4]  # ipc_eff value
      amount_result <- matches[1, 5]  # full facility or vent floor
    } else{
      # pull out the location, facility type, and year from the file name
      pattern <- "abc_stats_(.*?)_period_(\\d+)_IPC_([\\d.]+).csv"
      matches <- str_match(file, pattern)
      facility_result <- matches[1, 2] # LTACH_A
      period_result <- matches[1, 3]  # period
      ipc_result <- matches[1, 4]  # ipc_eff value
      amount_result <- "full"  # all are full facility
      
    }

    
    # add those as a new columns to the df
    df <- df %>% 
      mutate(facility = facility_result,
             amount_screened = amount_result,
             period = period_result, 
             ipc_eff = ipc_result) %>%
      select(facility, amount_screened, period, ipc_eff, generation:sims_expected)
    
    
    # Append the data frame to the list
    df_list[[ matches[ 1, 1] ]] <- df
    
    
  } # end for loop
  
  # Combine all the data frames in the list into one data frame
  append_df <- bind_rows(df_list)
  return(append_df)
  
}



