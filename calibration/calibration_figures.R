# Calibration posterior for beta and ipc_eff figures

library(here)
library(tidyverse)
library(ggridges)

# beta and ipc_eff colors
param_colors = c("#5C126EFE", "#C43C4EFE", "darkgreen", "blue")


source(here("R/batch_read_function.R"))

# List all the CSV files in the folder
folder_path <- here("calibration/full_results")

abc_stats_files <- list.files(folder_path, pattern = "^abc_stats", full.names = TRUE)
results_files <- list.files(folder_path, pattern = "^results_ABC_SMC", full.names = TRUE)


# Combine abc_stats files into a single data frame
abc_stats_df <- read_batch_cal_abc_stats_fun(abc_stats_files)
write_csv(abc_stats_df, 'calibration/full_results/abc_stats_all_combos.csv')

# Combine results_ABC_SMC files into a single data frame
N <- 1000 # number of particles to accept per generation
results_df <- read_batch_cal_res_fun(results_files, N)





# Priors
#  Lower and upper boundaries for the transmission (beta) prior
lm.low <- c(0.01)
lm.upp <- c(0.07)

# uniform prior
set.seed(596)
LTACH_beta_prior <- runif(1000, min=lm.low[1], max=lm.upp[1])

prior_df <- tibble(beta_LTACH = LTACH_beta_prior,
                   G = "Prior") %>%
  pivot_longer(beta_LTACH, names_to = "facility_type", names_prefix = "beta_", values_to = "beta_prior")


beta_dist_prior <- ggplot(prior_df, aes(x = beta_prior, y = G, fill = facility_type)) +
  geom_density_ridges(scale = 0.9, quantile_lines = T, alpha = 0.75) +
  scale_fill_manual(values = param_colors[1], aesthetics = "fill", name = "Facility type", labels = c("LTACH")) +
  labs(x = "Beta", y = "Prior") +
  geom_vline(aes(xintercept = 0.07), linetype = "dashed", color = "black") +
  geom_vline(aes(xintercept = 0.01), linetype = "dashed", color = param_colors[1]) +
  theme_bw()
beta_dist_prior
ggsave(here("calibration/results_figs/beta_prior_dist.png"), plot = beta_dist_prior, width = 6, height = 3, unit = "in", dpi = 300)




# plots of final parameter distributions from all generations ran for each facility, period, and ipc_eff

results_df <- as_tibble(results_df) %>%
  mutate(ipc_eff = factor(ipc_eff, levels = c(0.5, 0.7)),
                     G = as.character(G))

all_gen_beta_dist <- ggplot(results_df, aes(x = beta, y = G, fill = ipc_eff)) +
  stat_density_ridges(scale = 5, quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), alpha = 0.75) +
  facet_grid(period_labs ~ facility_name, scales = "free") +
  scale_fill_manual(values = param_colors[3:4], aesthetics = "fill", name = "IPC efficacy", labels = c("50%", "70%")) +
  labs(x = "Beta", y = "Generation") +
  theme_bw()
all_gen_beta_dist
ggsave(here("calibration/results_figs/all_gen_beta_dist.png"), plot = all_gen_beta_dist, width = 9, height = 6, unit = "in", dpi = 300)


# filter out generations that didn't finish running
completed_results_df <- results_df %>%
  filter(completed == TRUE)

complete_gen_beta_dist <- ggplot(completed_results_df, aes(x = beta, y = G, fill = ipc_eff)) +
  stat_density_ridges(scale = 5, quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), alpha = 0.75) +
  facet_grid(period_labs ~ facility_name, scales = "free") +
  scale_fill_manual(values = param_colors[3:4], aesthetics = "fill", name = "IPC efficacy", labels = c("50%", "70%")) +
  labs(x = "Beta", y = "Generation") +
  theme_bw()
complete_gen_beta_dist
ggsave(here("calibration/results_figs/complete_gen_beta_dist.png"), plot = complete_gen_beta_dist, width = 9, height = 6, unit = "in", dpi = 300)


# find the last generation for each facility, period, and ipc_eff
last_gen <- completed_results_df %>%
  group_by(facility_name, amount_screened, period, ipc_eff) %>%
  summarize(last_gen = max(G))


# create a data frame with only the final generation for each facility, period, and ipc_eff combination
last_gen_results_df <- completed_results_df %>%
  full_join(., last_gen) %>%
  mutate(last_gen_indicator = if_else(as.numeric(G) == as.numeric(last_gen), 1, 0)) %>%
  filter(last_gen_indicator == TRUE)
  

## All facilities
final_gen_beta_dist <- ggplot(last_gen_results_df, aes(x = beta, y = G, fill = ipc_eff)) +
  stat_density_ridges(scale = 5, quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), alpha = 0.75) +
  facet_grid(period_labs ~ facility_name, scales = "free_y") +
  scale_fill_manual(values = param_colors[3:4], aesthetics = "fill", name = "IPC efficacy", labels = c("50%", "70%")) +
  labs(x = "Beta", y = "Final Generation Completed") +
  theme_bw()
final_gen_beta_dist
ggsave(here("calibration/results_figs/final_gen_beta_dist.png"), plot = final_gen_beta_dist, width = 10, height = 6, unit = "in", dpi = 300)



final_gen_beta_dist_LTACHs <- last_gen_results_df %>%
  filter(facility_name == "LTACH_A" | facility_name == "LTACH_B" | facility_name == "LTACH_C" | facility_name == "LTACH_D") %>%
  ggplot(aes(x = beta, y = G, fill = ipc_eff)) +
  stat_density_ridges(scale = 5, quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), alpha = 0.75) +
  facet_grid(period_labs ~ facility_name, scales = "free_y") +
  scale_fill_manual(values = param_colors[3:4], aesthetics = "fill", name = "IPC efficacy", labels = c("50%", "70%")) +
  labs(x = "Beta", y = "Final Generation Completed") +
  geom_vline(aes(xintercept = 0.0175), linetype = "dashed", color = "darkgreen") +
  geom_vline(aes(xintercept = 0.066), linetype = "dashed", color = "darkgreen") +
  geom_vline(aes(xintercept = 0.0209), linetype = "dashed", color = "blue") +
  geom_vline(aes(xintercept = 0.065), linetype = "dashed", color = "blue") +
  theme_bw()
final_gen_beta_dist_LTACHs
ggsave(here("calibration/results_figs/final_gen_beta_dist_LTACHs.png"), plot = final_gen_beta_dist_LTACHs, width = 10, height = 6, unit = "in", dpi = 300)


## Check Bayesplot for posterior distribution



full_param_summary <- last_gen_results_df %>%
  group_by(facility_name, amount_screened, period, ipc_eff) %>%
  summarize(mean = mean(beta),
            median = median(beta),
            lower_95CI = quantile(beta, 0.025), # want to update this to the Highest (Posterior) Density Interval https://search.r-project.org/CRAN/refmans/HDInterval/html/hdi.html
            upper_95CI = quantile(beta, 0.975))
write.csv(full_param_summary, here("calibration/full_results/beta_posterior_all_combos.csv"))



# calculating the quantiles for each distance measure from each generation

quantile_summary <- results_df %>%
  group_by(facility_name, period, ipc_eff, G) %>%
  summarize(dist_col_q20 = quantile(dist_col, 0.2),
            dist_col_q25 = quantile(dist_col, 0.25),
            dist_col_q30 = quantile(dist_col, 0.3),
            dist_col_q40 = quantile(dist_col, 0.4),
            dist_col_q80 = quantile(dist_col, 0.8),
            dist_pps_q20 = quantile(dist_pps, 0.2),
            dist_pps_q25 = quantile(dist_pps, 0.25),
            dist_pps_q30 = quantile(dist_pps, 0.3),
            dist_pps_q40 = quantile(dist_pps, 0.4),
            dist_pps_q80 = quantile(dist_pps, 0.8))





