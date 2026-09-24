# Length of stay sampling function

# libraries
library(here)
library(tidyverse)

source(here("R/batch_read_function.R"))

# List all the CSV files in the folder
file.list <- list.files(here("data/CMS_LOS_data"), pattern = "*.csv", full.names = TRUE)

all_LOS_data <- read_batch_csv_fun(file.list = file.list, format = "dataframe")

# filter to just get the CA LOS data
CA_LOS_data <- dplyr::filter(all_LOS_data, location == "CA")

# filter to just get the CA LOS data
CA_2019_LOS_data <- dplyr::filter(all_LOS_data, location == "CA", year == 2019)


# LOS sampling function --------------------------------------------------------


#' Length-of-stay sampling function for each facility type using 2019 CMS data
#'
#' @description 
#' Sample a length of stay (LOS) for n observations from 2019 length of stay data from
#' the Centers for Medicare and Medicaid Services (provided by DHQP) for LTACH
#' facilities in California (CA). This function is used in the
#' individual.R simulation model to determine the length of stay for each individual
#' patient in the simulation.
#'
#' @param n number of observations (number of times to sample from the distribution)
#' @param data data frame with LOS and percent of patients that had each possible LOS. 
#'        The data frame also includes the data year, location, and facility type. Needs
#'        to be filtered to the appropriate facility, year, and location before being used 
#'        in this function.
#' @param fac_type character indicating the type of facility (LTACH for this analysis).
#'        Dictates the specific alpha and theta values to use based on 2019 CMS data.
#'
#' @return a vector of the lengths of stay for n observations that are rounded up 
#'        to the nearest integer. Note that values between zero and 1 are rounded up to 1.
#'
#' @require
#' library(tidyverse)
#'
#' @examples
#' patients_test <- seq(200)
#' 
#' discharge_tstep_test <- sample_LOS(n = length(patients_test), data = CA_LOS_data, fac_type = "LTACH")
#' 
#' 
sample_LOS <- function(n = 1, data, fac_type){
  
  # filter data by facility type
  data <- filter(data, facility_type == fac_type)
  
  # pull out vectors of the LOS and percents
  los <- data$LOS
  weights <- data$percent
  
  # sample from data
  new_los <- sample(x = los, size = n, prob = weights, replace = TRUE)
  
  # for any LOS that is equal to zero, round up to 1 day
  new_los[ new_los == 0] <- 1

  # return a vector of integers for LOS based on the facility type
  return(new_los)
}
