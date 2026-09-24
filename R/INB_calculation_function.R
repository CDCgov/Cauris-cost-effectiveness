#' Function to process each combination of beta and import for INB analysis
#'
#' @param i Transmission rate (trans) combinations
#' @param j Importation rate (import) combinations
#' @param cost_type Cost type combinations: screening or total costs
#'
#' @return Data frame with all INB calculations across all transmission, importation, cost type
#' 

calc_INB <- function(i, j, cost_type, WTP_lower, WTP_upper) {
  
  # Filter data to a single beta and import combination
  subset_df <- cost_effect_data_df %>%
    filter(beta == i & import == j)
  
  # Vector of all interventions except 'no screening'
  interventions <- unique(subset_df$intervention)
  interventions <- interventions[interventions != "No Screening"]
  
  # Select appropriate WTP line based on i and j
  #WTP_range <- input_WTP_range %>%
  #  filter(trans == i & import == j, cost_type == cost_type) 
  
  # Loop through different values of WTP for screening costs
  map_dfr(seq(WTP_lower, WTP_upper), function(WTP) {
    
    # Use specified cost type (either screen_cost or total_cost)
    if (cost_type == "screen") {
      subset_df$INB <- WTP * subset_df$col_cases_averted - subset_df$screen_cost
    } else if (cost_type == "total") {
      subset_df$INB <- WTP * subset_df$col_cases_averted - subset_df$total_cost
    } else {
      stop("Invalid cost type specified.")
    }
    
    # Linear regression for INB
    inb_lm <- estimatr::lm_robust(INB ~ intervention, se_type = "HC1", data = subset_df)
    
    coefs_summary <- summary(inb_lm)$coefficients
    
    # Map over interventions to create results data frame
    map_dfr(interventions, function(k) {
      intervention_lm <- paste0("intervention", k)
      
      if (intervention_lm %in% rownames(coefs_summary)) {
        inb_mean <- coefs_summary[intervention_lm,"Estimate"]  
        inb_p <- coefs_summary[intervention_lm,"Pr(>|t|)"]  
        inb_lower <- coefs_summary[intervention_lm,"CI Lower"]
        inb_upper <- coefs_summary[intervention_lm,"CI Upper"]
        
        prob <- ifelse(inb_mean > 0,
                       1 - (inb_p / 2),
                       (inb_p / 2))
        
        return(data.frame(
          beta = i,
          import = j,
          cost_type = cost_type,
          intervention = k,
          WTP = WTP,
          INB_mean = inb_mean,
          INB_lower = inb_lower,
          INB_upper = inb_upper,
          INB_p = inb_p,
          prob = prob
        ))
      } else {
        return(NULL)
      }
    })
  })
}
