# Analysis of cumulative colonization, clinical, and invasive infection cases
# LTACH IPC 50%



# load libraries
library(here)
library(data.table)
library(tidyverse)
library(broom)
library(glmmTMB)
library(emmeans)
library(car)


# read in data for figures
LTACH_IPC50_cost_effect_df <- read.csv(here("data_output/LTACH_IPC50_model_update/cost_effect_data.csv")) %>%
  mutate(facility = "LTACH", ipc_eff = "IPC50") %>%
  dplyr::select(unique_id, seed, facility, ipc_eff, beta:RRR_invasive)
LTACH_IPC70_cost_effect_df <- read.csv(here("data_output/LTACH_IPC70_model_update/cost_effect_data.csv"))  %>%
  mutate(facility = "LTACH", ipc_eff = "IPC70") %>%
  dplyr::select(unique_id, seed, facility, ipc_eff, beta:RRR_invasive)


# combine data
combined_cost_effect_df <- rbind(LTACH_IPC50_cost_effect_df, LTACH_IPC70_cost_effect_df)

combined_cost_effect_df <- combined_cost_effect_df %>%
  mutate(intervention = factor(intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment Admissions + PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days) w/ taper",
                                                        "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                        "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                        "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")),
         trans = case_when(
           beta == 0.0175 ~ "Low Transmission",
           beta == 0.066 ~ "High Transmission"),
         trans = factor(trans, levels = c("Low Transmission", "High Transmission")),
         ipc_eff = factor(ipc_eff, levels = c("IPC50", "IPC70")))






# test model with interventions * IPC eff
# test model for other scenarios to see if consistent which model is best fit
# iterate through all scenarios and extract results

# test cases averted model - can use a normal distribution? if not, try to transform?



# combinations to iterate over
beta_values <- c(0.0175, 0.066)
import <- c(0.01, 0.04, 0.1)
group <- c("Low_Tier2", "Low_Tier3", "Low_Tier4", "High_Tier2", "High_Tier3", "High_Tier4")
outcome <- c("col", "clinical", "invasive")

# Use expand.grid to create combinations of trans and import values
# combinations_analysis <- expand.grid(trans=trans, import=import, outcome = outcome) %>%
#   arrange(outcome, trans, import)
combinations_analysis <- expand.grid(beta=beta_values, import=import) %>%
  arrange(beta, import) %>%
  mutate(group = group)

combined_cost_effect_df <- left_join(combined_cost_effect_df, combinations_analysis)




fit_best_glm_and_save <- function(data, group_name) {


  # Fit the Poisson model
  effect_glm_poisson <- glmmTMB(C_tot ~ intervention * ipc_eff, family = poisson, data = data)
  
  # Check for overdispersion in Poisson model
  overdispersion_test_poisson <- sum(residuals(effect_glm_poisson, type = "pearson")^2) / df.residual(effect_glm_poisson)
  
  # Deviance goodness-of-fit test for Poisson model
  deviance_poisson <- deviance(effect_glm_poisson)
  df_poisson <- df.residual(effect_glm_poisson)
  p_value_poisson <- 1 - pchisq(deviance_poisson, df_poisson)
  
  # Pearson's Chi-Square goodness-of-fit test for Poisson model
  pearson_chi_sq_poisson <- sum(residuals(effect_glm_poisson, type = "pearson")^2)
  p_value_pearson_poisson <- 1 - pchisq(pearson_chi_sq_poisson, df_poisson)
  
  
  
  # Fit the Negative Binomial 1 model
  effect_glm_nb1 <- glmmTMB(C_tot ~ intervention * ipc_eff, family = nbinom1, data = data)
  
  # Check for overdispersion in Negative Binomial 1 model
  overdispersion_test_nb1 <- sum(residuals(effect_glm_nb1, type = "pearson")^2) / df.residual(effect_glm_nb1)
  
  # Deviance goodness-of-fit test for Negative Binomial 1 model
  deviance_nb1 <- deviance(effect_glm_nb1)
  df_nb1 <- df.residual(effect_glm_nb1)
  p_value_nb1 <- 1 - pchisq(deviance_nb1, df_nb1)
  
  # Pearson's Chi-Square goodness-of-fit test for Negative Binomial 1 model
  pearson_chi_sq_nb1 <- sum(residuals(effect_glm_nb1, type = "pearson")^2)
  p_value_pearson_nb1 <- 1 - pchisq(pearson_chi_sq_nb1, df_nb1)
  
  
  
  # Fit the Negative Binomial 2 model
  effect_glm_nb2 <- glmmTMB(C_tot ~ intervention * ipc_eff, family = nbinom2, data = data)
  
  # Check for overdispersion in Negative Binomial 2 model
  overdispersion_test_nb2 <- sum(residuals(effect_glm_nb2, type = "pearson")^2) / df.residual(effect_glm_nb2)
  
  # Deviance goodness-of-fit test for Negative Binomial 2 model
  deviance_nb2 <- deviance(effect_glm_nb2)
  df_nb2 <- df.residual(effect_glm_nb2)
  p_value_nb2 <- 1 - pchisq(deviance_nb2, df_nb2)
  
  # Pearson's Chi-Square goodness-of-fit test for Negative Binomial 2 model
  pearson_chi_sq_nb2 <- sum(residuals(effect_glm_nb2, type = "pearson")^2)
  p_value_pearson_nb2 <- 1 - pchisq(pearson_chi_sq_nb2, df_nb2)
  
  
  
  # Fit the Gamma model
  effect_glm_gamma <- glmmTMB(C_tot ~ intervention * ipc_eff, family = Gamma, data = data)
  
  # Check for overdispersion in Gamma model
  overdispersion_test_gamma <- sum(residuals(effect_glm_gamma, type = "pearson")^2) / df.residual(effect_glm_gamma)
  
  # Deviance goodness-of-fit test for Gamma model
  deviance_gamma <- deviance(effect_glm_gamma)
  df_gamma <- df.residual(effect_glm_gamma)
  p_value_gamma <- 1 - pchisq(deviance_gamma, df_gamma)
  
  # Pearson's Chi-Square goodness-of-fit test for Gamma model
  pearson_chi_sq_gamma <- sum(residuals(effect_glm_gamma, type = "pearson")^2)
  p_value_pearson_gamma <- 1 - pchisq(pearson_chi_sq_gamma, df_gamma)
  
  

  
  # Compare models AIC and goodness of fit 
  model_compare <- data.frame(
    Group = group_name,
    Model = c("Poisson", "Negative Binomial 1", "Negative Binomial 2", "Gamma"),
    mod_name= c("effect_glm_poisson", "effect_glm_nb1", "effect_glm_nb2", "effect_glm_gamma"),
    AIC = c(AIC(effect_glm_poisson), AIC(effect_glm_nb1), AIC(effect_glm_nb2), AIC(effect_glm_gamma)),
    overdispersion = c(overdispersion_test_poisson, overdispersion_test_nb1, overdispersion_test_nb2, overdispersion_test_gamma),
    deviance_p_value = c(p_value_poisson, p_value_nb1, p_value_nb2, p_value_gamma),
    pearson_p_value = c(p_value_pearson_poisson, p_value_pearson_nb1, p_value_pearson_nb2, p_value_pearson_gamma)
  )
  print(model_compare)
  
  # Determine which is the best model to use
  top_model <- model_compare %>% filter((overdispersion > 0.5 & overdispersion < 1.25), 
                                        (deviance_p_value > 0.05 | pearson_p_value > 0.05),
                                        AIC == min(AIC, na.rm = T))
  top_model_name <- top_model$Model
  
  print(top_model)
  print(top_model_name)
  
  # Fit the best model
  if (top_model_name == "Poisson") {
    best_model <- effect_glm_poisson
  } else if (top_model_name == "Negative Binomial 1") {
    best_model <- effect_glm_nb1
  } else if (top_model_name == "Negative Binomial 2") {
    best_model <- effect_glm_nb2
  } else if (top_model_name == "Gamma") {
    best_model <- effect_glm_gamma
  } else {
    print("No model was identified")
  }
  
  # Extract coefficients and their statistics
  model_summary <- summary(best_model)
  
  # Create a tidy data frame from the model summary
  best_model_summary <- data.frame(
    term = rownames(model_summary$coefficients$cond),
    estimate = model_summary$coefficients$cond[, "Estimate"],
    std.error = model_summary$coefficients$cond[, "Std. Error"],
    statistic = model_summary$coefficients$cond[, "z value"],
    p.value = model_summary$coefficients$cond[, "Pr(>|z|)"]
  )
  
  # Create a tidy data frame from the model anova
  model_anvoa <- tidy(Anova(best_model))

  
  # Pairwise comparisons of interventions for each IPC efficacy value
  effect_contrasts <- emmeans(best_model, specs = pairwise ~ intervention | ipc_eff, type = "response")
  Contrasts_by_intervention <- tidy(effect_contrasts$contrasts)

  # Pairwise comparisons of IPC efficacy value for each interventions
  effect_contrasts2 <- emmeans(best_model, specs = pairwise ~ ipc_eff | intervention, type = "response")
  Contrasts_by_IPC <- tidy(effect_contrasts2$contrasts)
  
  
  
  # Save the outputs to a CSV file
  GLM_info_file <- paste0("data_output/stat_tests/GLM_info_", group_name, ".csv")
  write.csv(top_model, GLM_info_file, row.names = FALSE)
  
  GLM_output_file <- paste0("data_output/stat_tests/GLM_output_", group_name, ".csv")
  write.csv(best_model_summary, GLM_output_file, row.names = FALSE)
  
  ANOVA_output_file <- paste0("data_output/stat_tests/Anova_output_", group_name, ".csv")
  write.csv(model_anvoa, ANOVA_output_file, row.names = FALSE)
  
  Contrasts_by_intervention_output_file <- paste0("data_output/stat_tests/Contrasts_by_intervention_output_", group_name, ".csv")
  write.csv(Contrasts_by_intervention, Contrasts_by_intervention_output_file, row.names = FALSE)
  
  Contrasts_by_IPC_output_file <- paste0("data_output/stat_tests/Contrasts_by_IPC_output_", group_name, ".csv")
  write.csv(Contrasts_by_IPC, Contrasts_by_IPC_output_file, row.names = FALSE)
  
  return(best_model_summary)
}


# Run the analysis and save output files ----------------------------------
combined_cost_effect_df %>%
  group_by(group) %>%
  group_modify(~ {
    fit_best_glm_and_save(.x, .y)
  })




# Export a cleaned table for model averages and 95% prediction intervals for supplement

# read in data for figures
LTACH_IPC50_cost_effect_avg_df <- read.csv(here("data_output/LTACH_IPC50_model_update/cost_effect_avg_data.csv")) %>%
  mutate(facility = "LTACH", ipc_eff = "IPC50") %>%
  select(facility, ipc_eff, import:screen_efficiency)
LTACH_IPC70_cost_effect_avg_df <- read.csv(here("data_output/LTACH_IPC70_model_update/cost_effect_avg_data.csv"))  %>%
  mutate(facility = "LTACH", ipc_eff = "IPC70") %>%
  select(facility, ipc_eff, import:screen_efficiency)


# combine data
combined_cost_effect_avg_df <- rbind(LTACH_IPC50_cost_effect_avg_df, LTACH_IPC70_cost_effect_avg_df)



partial_clean_case_totals_avg_df <- combined_cost_effect_avg_df %>%
  mutate(intervention = factor(intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment Admissions + PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days) w/ taper",
                                                        "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                        "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                        "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")),
         trans = factor(case_when(
           beta == 0.0175 ~ "Low Transmission",
           beta == 0.066 ~ "High Transmission"), levels = c("Low Transmission", "High Transmission")),
         ipc_eff = factor(ipc_eff, levels = c("IPC50", "IPC70"))) %>%
  select(facility:intervention, contains(c("C_tot", "Clinical_tot", "I_tot", "col_cases_averted", "clinical_cases_averted", "invasive_cases_averted", "RRR_col"))) %>%
  select(-contains(c("_se"))) %>%
  rename_with(~ gsub("^C_tot_", "col_", .), starts_with("C_tot_")) %>%
  rename_with(~ gsub("^Clinical_tot_", "clinical_", .), starts_with("Clinical_tot")) %>%
  rename_with(~ gsub("^I_tot_", "invasive_", .), starts_with("I_tot_")) %>%
  rename_with(~ gsub("^RRR_col_", "col_RRR_", .), starts_with("RRR_col_"))


Clean_RRR_df <- partial_clean_case_totals_avg_df %>%
  select(facility:intervention, starts_with("col_RRR_")) %>%
  pivot_longer(cols = starts_with("col_RRR_"),
               names_to = c("case_type", ".value"),
               names_pattern = "^(col_RRR)_(.*)") %>%
  mutate(case_type = if_else(case_type == "col_RRR", "colonization", NA),
         mean = round(mean, 3),
         median = round(median, 3),
         lower = round(lower, 3),
         upper = round(upper, 3),
         `95% prediction interval` = paste(lower, upper, sep = " – ")) %>%
  filter(intervention != "No Screening") %>%
  select(-lower, -upper)
write.csv(Clean_RRR_df, "data_output/colonization_RRR_clean.csv", row.names = FALSE)

clean_case_totals_avg_df <- partial_clean_case_totals_avg_df %>%
  select(-starts_with("col_RRR_")) %>%
  pivot_longer(
    cols = starts_with("col_") | starts_with("clinical_") | starts_with("invasive_"),
    names_to = c("case_type", ".value"),
    names_pattern = "^(col|clinical|invasive)_(.*)"
  ) %>%
  mutate(case_type = factor(if_else(case_type == "col", "colonization", case_type), levels = c("colonization", "clinical", "invasive")),
         `95% prediction interval` = paste(lower, upper, sep = " – "),
         `cases averted_95% prediction interval` = if_else(intervention != "No Screening", paste(cases_averted_lower, cases_averted_upper, sep = " – "), NA)) %>%
  select(facility:case_type, mean, median, `95% prediction interval`, cases_averted_mean, cases_averted_median, `cases averted_95% prediction interval`) %>%
  arrange(, import, beta, case_type, intervention, ipc_eff)
write.csv(clean_case_totals_avg_df, "data_output/cumulative_case_totals_clean.csv", row.names = FALSE)












# Old versions of analyses testing models and assumptions --------------------------

## Cumulative Colonization Cases Regression -------------------------------------------------
hist(subset_data$C_tot)


# linear regression 
effect_lm <- lm(C_tot ~ intervention * ipc_eff, data = subset_data)
summary(effect_lm)
Anova(effect_lm)
tidy(effect_lm)

qqnorm(residuals(effect_lm))
qqline(residuals(effect_lm))
# Residuals are not linear - violates assumptions

lm_residuals_sample <- sample(residuals(effect_lm), size = 5000)
shapiro.test(lm_residuals_sample)



## Poisson model
effect_glm_poisson <- glmmTMB(C_tot ~ intervention * ipc_eff, family = poisson, data = subset_data)
summary(effect_glm_poisson)
tidy(Anova(effect_glm_poisson))
#tidy(effect_glm_poisson)

# Check for overdispersion in Poisson model
overdispersion_test_poisson <- sum(residuals(effect_glm_poisson, type = "pearson")^2) / df.residual(effect_glm_poisson)
print(overdispersion_test_poisson)
# poisson model is overdispersed

# Residual plot for Poisson model
ggplot(data.frame(fitted = fitted(effect_glm_poisson), residuals = residuals(effect_glm_poisson)), aes(x=fitted, y=residuals)) +
  geom_point() +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  labs(title="Residual Plot - Poisson Model", x="Fitted Values", y="Residuals")

# Q-Q plot for Poisson model residuals
qqnorm(residuals(effect_glm_poisson))
qqline(residuals(effect_glm_poisson), col="red")
title("Q-Q Plot - Poisson Model")

# Deviance goodness-of-fit test for Poisson model
deviance_poisson <- deviance(effect_glm_poisson)
df_poisson <- df.residual(effect_glm_poisson)
p_value_poisson <- 1 - pchisq(deviance_poisson, df_poisson)

cat("Deviance Goodness-of-Fit Test for Poisson Model:\n")
cat("Deviance:", deviance_poisson, "\n")
cat("Degrees of Freedom:", df_poisson, "\n")
cat("P-value:", p_value_poisson, "\n\n")

# Pearson's Chi-Square goodness-of-fit test for Poisson model
pearson_chi_sq_poisson <- sum(residuals(effect_glm_poisson, type = "pearson")^2)
p_value_pearson_poisson <- 1 - pchisq(pearson_chi_sq_poisson, df_poisson)

cat("Pearson's Chi-Square Goodness-of-Fit Test for Poisson Model:\n")
cat("Chi-Square Statistic:", pearson_chi_sq_poisson, "\n")
cat("Degrees of Freedom:", df_poisson, "\n")
cat("P-value:", p_value_pearson_poisson, "\n\n")





## Negative binomial 1 model
effect_glm_nb1 <- glmmTMB(C_tot ~ intervention * ipc_eff, family = nbinom1, data = subset_data)
summary(effect_glm_nb1)
AIC(effect_glm_nb1)
Anova(effect_glm_nb1)

# Check for overdispersion in Negative binomial 1 model
overdispersion_test_nb1 <- sum(residuals(effect_glm_nb1, type = "pearson")^2) / df.residual(effect_glm_nb1)
print(overdispersion_test_nb1)
# just about 1, good

# Residual plot for Negative binomial 1 model
ggplot(data.frame(fitted = fitted(effect_glm_nb1), residuals = residuals(effect_glm_nb1)), aes(x=fitted, y=residuals)) +
  geom_point() +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  labs(title="Residual Plot - Negative binomial 1 Model", x="Fitted Values", y="Residuals")

# Q-Q plot for Negative binomial 1 model residuals
qqnorm(residuals(effect_glm_nb1))
qqline(residuals(effect_glm_nb1), col="red")
title("Q-Q Plot - Negative binomial 1 Model")

# Deviance goodness-of-fit test for Negative binomial 1 model
deviance_nb1 <- deviance(effect_glm_nb1)
df_nb1 <- df.residual(effect_glm_nb1)
p_value_nb1 <- 1 - pchisq(deviance_nb1, df_nb1)

cat("Deviance Goodness-of-Fit Test for Negative binomial 1 Model:\n")
cat("Deviance:", deviance_nb1, "\n")
cat("Degrees of Freedom:", df_nb1, "\n")
cat("P-value:", p_value_nb1, "\n\n")

# Pearson's Chi-Square goodness-of-fit test for Negative binomial 1 model
pearson_chi_sq_nb1 <- sum(residuals(effect_glm_nb1, type = "pearson")^2)
p_value_pearson_nb1 <- 1 - pchisq(pearson_chi_sq_nb1, df_nb1)

cat("Pearson's Chi-Square Goodness-of-Fit Test for Negative binomial 1 Model:\n")
cat("Chi-Square Statistic:", pearson_chi_sq_nb1, "\n")
cat("Degrees of Freedom:", df_nb1, "\n")
cat("P-value:", p_value_pearson_nb1, "\n\n")





## Negative binomial 2 model
effect_glm_nb2 <- glmmTMB(C_tot ~ intervention * ipc_eff, family = nbinom2, data = subset_data)
summary(effect_glm_nb2)
Anova(effect_glm_nb2)

# Check for overdispersion in Negative binomial 2 model
overdispersion_test_nb2 <- sum(residuals(effect_glm_nb2, type = "pearson")^2) / df.residual(effect_glm_nb2)
print(overdispersion_test_nb2)
# just about 1, good

# Residual plot for Negative binomial 2 model
ggplot(data.frame(fitted = fitted(effect_glm_nb2), residuals = residuals(effect_glm_nb2)), aes(x=fitted, y=residuals)) +
  geom_point() +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  labs(title="Residual Plot - Negative binomial 2 Model", x="Fitted Values", y="Residuals")

# Q-Q plot for Negative binomial 2 model residuals
qqnorm(residuals(effect_glm_nb2))
qqline(residuals(effect_glm_nb2), col="red")
title("Q-Q Plot - Negative binomial 2 Model")

# Deviance goodness-of-fit test for Negative binomial 2 model
deviance_nb2 <- deviance(effect_glm_nb2)
df_nb2 <- df.residual(effect_glm_nb2)
p_value_nb2 <- 1 - pchisq(deviance_nb2, df_nb2)

cat("Deviance Goodness-of-Fit Test for Negative binomial 2 Model:\n")
cat("Deviance:", deviance_nb2, "\n")
cat("Degrees of Freedom:", df_nb2, "\n")
cat("P-value:", p_value_nb2, "\n\n")

# Pearson's Chi-Square goodness-of-fit test for Negative binomial 2 model
pearson_chi_sq_nb2 <- sum(residuals(effect_glm_nb2, type = "pearson")^2)
p_value_pearson_nb2 <- 1 - pchisq(pearson_chi_sq_nb2, df_nb2)

cat("Pearson's Chi-Square Goodness-of-Fit Test for Negative binomial 2 Model:\n")
cat("Chi-Square Statistic:", pearson_chi_sq_nb2, "\n")
cat("Degrees of Freedom:", df_nb2, "\n")
cat("P-value:", p_value_pearson_nb2, "\n\n")




## Gamma model
effect_glm_gamma <- glmmTMB(C_tot ~ intervention * ipc_eff, family = Gamma(), data = subset_data)
summary(effect_glm_gamma)
Anova(effect_glm_gamma)

# Check for overdispersion in Gamma model
overdispersion_test_gamma <- sum(residuals(effect_glm_gamma, type = "pearson")^2) / df.residual(effect_glm_gamma)
print(overdispersion_test_gamma)
# Gamma model is underdispersed

# Residual plot for Poisson model
ggplot(data.frame(fitted = fitted(effect_glm_gamma), residuals = residuals(effect_glm_gamma)), aes(x=fitted, y=residuals)) +
  geom_point() +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  labs(title="Residual Plot - Gamma Model", x="Fitted Values", y="Residuals")

# Q-Q plot for Gamma model residuals
qqnorm(residuals(effect_glm_gamma))
qqline(residuals(effect_glm_gamma), col="red")
title("Q-Q Plot - Gamma Model")

# Deviance goodness-of-fit test for Gamma model
deviance_gamma <- deviance(effect_glm_gamma)
df_gamma <- df.residual(effect_glm_gamma)
p_value_gamma <- 1 - pchisq(deviance_gamma, df_gamma)

cat("Deviance Goodness-of-Fit Test for Gamma Model:\n")
cat("Deviance:", deviance_gamma, "\n")
cat("Degrees of Freedom:", df_gamma, "\n")
cat("P-value:", p_value_gamma, "\n\n")

# Pearson's Chi-Square goodness-of-fit test for Gamma model
pearson_chi_sq_gamma <- sum(residuals(effect_glm_gamma, type = "pearson")^2)
p_value_pearson_gamma <- 1 - pchisq(pearson_chi_sq_gamma, df_gamma)

cat("Pearson's Chi-Square Goodness-of-Fit Test for Gamma Model:\n")
cat("Chi-Square Statistic:", pearson_chi_sq_gamma, "\n")
cat("Degrees of Freedom:", df_gamma, "\n")
cat("P-value:", p_value_pearson_gamma, "\n\n")





# Compare models AIC and goodness of fit 
model_compare <- data.frame(
  Model = c("Poisson", "Negative Binomial 1", "Negative Binomial 2", "Gamma"),
  mod_name= c("effect_glm_poisson", "effect_glm_nb1", "effect_glm_nb2", "effect_glm_gamma"),
  AIC = c(AIC(effect_glm_poisson), AIC(effect_glm_nb1), AIC(effect_glm_nb2), AIC(effect_glm_gamma)),
  overdispersion = c(overdispersion_test_poisson, overdispersion_test_nb1, overdispersion_test_nb2, overdispersion_test_gamma),
  deviance_p_value = c(p_value_poisson, p_value_nb1, p_value_nb2, p_value_gamma),
  pearson_p_value = c(p_value_pearson_poisson, p_value_pearson_nb1, p_value_pearson_nb2, p_value_pearson_gamma)
)

print(model_compare)

# determine which is the best model to use
top_model <- filter(model_compare, overdispersion > 0.75 & overdispersion < 1.25, AIC == min(AIC), deviance_p_value > 0.05 | pearson_p_value > 0.05)
top_model_name <- top_model$Model

# Fit the best model based on AIC
if (top_model_name == "Poisson") {
  best_model <- effect_glm_poisson
} else if (top_model_name == "Negative Binomial 1") {
  best_model <- effect_glm_nb1
} else if (top_model_name == "Negative Binomial 2") {
  best_model <- effect_glm_nb2
} else if (top_model_name == "Gamma") {
  best_model <- effect_glm_gamma
}else {
  print("No model was identified")
}

# Tidy the best model output
best_model_summary <- tidy(best_model)

# Save the output to a CSV file
output_file <- paste0("best_glm_output_group_", group_name, ".csv")
write.csv(best_model_summary, output_file, row.names = FALSE)


## Use the negative binomial 1 model, it has the best fit for total colonized cases


# Pairwise comparisons of interventions for each IPC efficacy value
effect_contrasts <- emmeans(effect_glm_nb1, specs = pairwise ~ intervention | ipc_eff, type = "response")
effect_contrasts
tidy(effect_contrasts$contrasts)
tidy(effect_contrasts$emmeans)

# Pairwise comparisons of IPC efficacy value for each interventions
effect_contrasts2 <- emmeans(effect_glm_nb1, specs = pairwise ~ ipc_eff | intervention, type = "response")
effect_contrasts2
