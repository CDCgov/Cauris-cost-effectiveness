# LTACH IPC 70% model results figures

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggh4x)

here("data_output/LTACH_IPC70_update_model/")


# Assign labels for figures
fac_type_label <- "LTACH"
ipc_eff_label <- "70%"

# read in data for figures
avg_state_df <- read.csv(here("data_output/LTACH_IPC70_model_update/avg_state_data.csv"))
cost_effect_avg_df <- read.csv(here("data_output/LTACH_IPC70_model_update/cost_effect_avg_data.csv"))
last_6mo_prev_inc_df <- read.csv(here("data_output/LTACH_IPC70_model_update/last_6mo_prev_inc_data.csv"))
cost_effect_data_df <- read.csv(here("data_output/LTACH_IPC70_model_update/cost_effect_data.csv"))


# fix intervention order
avg_state_df$intervention <- factor(avg_state_df$intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                                                             "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                             "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                             "Admissions + PPS (30 days)", "Admissions + PPS (14 days)"))
cost_effect_avg_df$intervention <- factor(cost_effect_avg_df$intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment Admissions + PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                      "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                      "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                      "Admissions + PPS (30 days)", "Admissions + PPS (14 days)"))
last_6mo_prev_inc_df$intervention <- factor(last_6mo_prev_inc_df$intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment Admissions + PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                          "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                          "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                          "Admissions + PPS (30 days)", "Admissions + PPS (14 days)"))



# pivot avg state df longer for figures
avg_state_long_df <- avg_state_df %>%
  pivot_longer(cols = -c(beta, import, intervention, time, week, month, beta_import),
               names_to = c("state", ".value"),
               names_pattern = "(.*)_(.*)")



# set labels for facets
trans_labs <- c("Low Transmisssion", "High Transmission")
names(trans_labs) <- c(0.0175, 0.066)

import_labs <- c("Tier 2", "Tier 3", "Tier 4")
names(import_labs) <- c(0.01, 0.04, 0.1)

n_sim <- 1000


# color labels for intervention
intervention_colors <- c("#E31A1C", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#FFDB00", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594")
names(intervention_colors) <- c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")

#display.brewer.pal(n = 8, name = 'Oranges')
#brewer.pal(n = 8, name = "Oranges")

# Figures building up from a single simulation to more complex figure
# Can't run these versions of the figures with this data - need a different output file
# buildup_fig_1_sim <- state_df1 %>%
#   filter(intervention == "No Screening", nsim == 1, beta == 0.066, import == 0.07) %>%
#   group_by(seed) %>%
#   ggplot(aes(x = time, y = prop_colonized, color= intervention, group = seed)) +
#   geom_line(linewidth = 1) +
#   facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
#   labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
#   scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
#   theme_bw()
# buildup_fig_1_sim
# ggsave("figures/LTACH_IPC70_update_update/buildup_prop_colonized_timeseries_1sim_noscreen.png", plot = buildup_fig_1_sim, width = 5.5, height = 4, unit = "in", dpi = 300)
# 
# buildup_fig_5_sim <- state_df1 %>%
#   filter(intervention == "No Screening", nsim <= 5, beta == 0.066, import == 0.07) %>%
#   group_by(seed) %>%
#   ggplot(aes(x = time, y = prop_colonized, color= intervention, group = seed)) +
#   geom_line(linewidth = 0.75) +
#   facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
#   labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
#   scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
#   theme_bw()
# buildup_fig_5_sim
# ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_5sim_noscreen.png", plot = buildup_fig_5_sim, width = 5.5, height = 4, unit = "in", dpi = 300)
# 
# 
# 
# 
# buildup_fig_100_sim <- state_df1 %>%
#   filter(intervention == "No Screening", nsim <= 100, beta == 0.066, import == 0.07) %>%
#   group_by(seed) %>%
#   ggplot(aes(x = time, y = prop_colonized, color= intervention, group = seed)) +
#   geom_line(linewidth = 0.5, alpha = 0.7) +
#   facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
#   labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
#   scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
#   theme_bw()
# buildup_fig_100_sim
# ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_100sim_noscreen.png", plot = buildup_fig_100_sim, width = 5.5, height = 4, unit = "in", dpi = 300)
# 
# 
# buildup_fig_1000_sim <- state_df1 %>%
#   filter(intervention == "No Screening", beta == 0.066, import == 0.07) %>%
#   group_by(seed) %>%
#   ggplot(aes(x = time, y = prop_colonized, color= intervention, group = seed)) +
#   geom_line(linewidth = 0.5, alpha = 0.3) +
#   facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
#   labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
#   scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
#   theme_bw()
# buildup_fig_1000_sim
# ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_noscreen.png", plot = buildup_fig_1000_sim, width = 5.5, height = 4, unit = "in", dpi = 300)


# Figure of individual model proportion colonized time series mean with 95% quantile range
buildup_fig_avg_no_screen_only <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized"), intervention == "No Screening", beta == 0.066, import == 0.04) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw()
buildup_fig_avg_no_screen_only
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_noscreen.png", plot = buildup_fig_avg_no_screen_only, width = 5, height = 4, unit = "in", dpi = 300)


buildup_fig_avg_no_screen_contain <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized"), (intervention == "No Screening" | intervention == "Containment PPS (14 days)" | intervention == "Containment Admissions + PPS (14 days)"), beta == 0.066, import == 0.04) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw()
buildup_fig_avg_no_screen_contain
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_noscreen_contain.png", plot = buildup_fig_avg_no_screen_contain, width = 6.25, height = 4, unit = "in", dpi = 300)


buildup_fig_avg_no_screen_contain_taper <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized"), (intervention == "No Screening" | intervention == "Containment PPS (14 days)" | intervention == "Containment Admissions + PPS (14 days)" | intervention == "Containment PPS (14 days) w/ taper" | intervention == "Containment Admissions + PPS (14 days) w/ taper"), beta == 0.066, import == 0.04) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw()
buildup_fig_avg_no_screen_contain_taper
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_noscreen_contain_taper.png", plot = buildup_fig_avg_no_screen_contain_taper, width = 6.25, height = 4, unit = "in", dpi = 300)


buildup_fig_avg_no_screen_admit <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized"), (intervention == "No Screening" | intervention == "Admissions"), beta == 0.066, import == 0.04) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw()
buildup_fig_avg_no_screen_admit
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_noscreen_admit.png", plot = buildup_fig_avg_no_screen_admit, width = 5, height = 4, unit = "in", dpi = 300)


buildup_fig_avg_no_screen_admit_contain <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized"), (intervention == "No Screening" | intervention == "Admissions" | intervention == "Containment PPS (14 days)" | intervention == "Containment Admissions + PPS (14 days)" | intervention == "Containment PPS (14 days) w/ taper" | intervention == "Containment Admissions + PPS (14 days) w/ taper"), beta == 0.066, import == 0.04) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw()
buildup_fig_avg_no_screen_admit_contain
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_noscreen_admit_contain.png", plot = buildup_fig_avg_no_screen_admit_contain, width = 6.25, height = 4, unit = "in", dpi = 300)



buildup_fig_avg_no_screen_admit_contain_pps <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized"), (intervention == "No Screening" | intervention == "Admissions" | intervention == "Containment PPS (14 days)" | intervention == "Containment Admissions + PPS (14 days)" | intervention == "Containment PPS (14 days) w/ taper" | intervention == "Containment Admissions + PPS (14 days) w/ taper" | intervention == "PPS (60 days)"), beta == 0.066, import == 0.04) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw()
buildup_fig_avg_no_screen_admit_contain_pps
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_noscreen_admit_contain_pps.png", plot = buildup_fig_avg_no_screen_admit_contain_pps, width = 6.25, height = 4, unit = "in", dpi = 300)



buildup_fig_avg_no_screen_admit_contain_all_pps <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized"), (intervention == "No Screening" | intervention == "Admissions" | intervention == "Containment PPS (14 days)" | intervention == "Containment Admissions + PPS (14 days)" | intervention == "Containment PPS (14 days) w/ taper" | intervention == "Containment Admissions + PPS (14 days) w/ taper"| intervention == "PPS (14 days)" | intervention == "PPS (30 days)" | intervention == "PPS (60 days)" | intervention == "PPS (90 days)" | intervention == "PPS (180 days)"), beta == 0.066, import == 0.04) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw()
buildup_fig_avg_no_screen_admit_contain_all_pps
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_noscreen_admit_contain_all_pps.png", plot = buildup_fig_avg_no_screen_admit_contain_all_pps, width = 6.25, height = 4, unit = "in", dpi = 300)



buildup_fig_avg_all_screen <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized"), beta == 0.066, import == 0.04) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw()
buildup_fig_avg_all_screen
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_all_screen.png", plot = buildup_fig_avg_all_screen, width = 6.25, height = 4, unit = "in", dpi = 300)


buildup_fig_avg_all_both_transmission <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized"), import == 0.04) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw()
buildup_fig_avg_all_both_transmission
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_all_both_trans.png", plot = buildup_fig_avg_all_both_transmission, width = 8.75, height = 4, unit = "in", dpi = 300)


buildup_fig_avg_all_both_importation <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized"), beta == 0.066) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw()
buildup_fig_avg_all_both_importation
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_all_both_import.png", plot = buildup_fig_avg_all_both_importation, width = 6.5, height = 6, unit = "in", dpi = 300)


buildup_fig_avg_all <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized")) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw() +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 12))
buildup_fig_avg_all
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_everything.png", plot = buildup_fig_avg_all, width = 9, height = 8, unit = "in", dpi = 300)


buildup_fig_avg_all_free_y <- avg_state_long_df %>%
  filter(state %in% c("prop_colonized")) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Time (days)", y = "Proportion of Colonized Individuals", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw() +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 12))
buildup_fig_avg_all_free_y
ggsave("figures/LTACH_IPC70_update/buildup_prop_colonized_timeseries_1000sim_avg_everything_free_y.png", plot = buildup_fig_avg_all_free_y, width = 9, height = 8, unit = "in", dpi = 300)



# Time Series of Monthly Incidence, Cumulative Colonization, Clinical, and Invasive infections -----------------------------------------


monthly_inc_timeseries <- avg_state_long_df %>%
  filter(state %in% c("monthly_col_incidence")) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Time (days)", y = "Monthly Incidence of Colonization", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw() +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 12))
monthly_inc_timeseries
ggsave("figures/LTACH_IPC70_update/monthly_inc_timeseries_1000sim_avg_everything.png", plot = monthly_inc_timeseries, width = 9, height = 8, unit = "in", dpi = 300)


cum_col_timeseries <- avg_state_long_df %>%
  filter(state %in% c("C_tot")) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Time (days)", y = "Cumulatitive Colonization Cases", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw() +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 12))
cum_col_timeseries
ggsave("figures/LTACH_IPC70_update/cum_colonization_timeseries_1000sim_avg_everything.png", plot = cum_col_timeseries, width = 9, height = 8, unit = "in", dpi = 300)


cum_clinical_timeseries <- avg_state_long_df %>%
  filter(state %in% c("Clinical_tot")) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Time (days)", y = "Cumulatitive Clinical Cases", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw() +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 12))
cum_clinical_timeseries
ggsave("figures/LTACH_IPC70_update/cum_clinical_timeseries_1000sim_avg_everything.png", plot = cum_clinical_timeseries, width = 9, height = 8, unit = "in", dpi = 300)


cum_infection_timeseries <- avg_state_long_df %>%
  filter(state %in% c("I_tot")) %>%
  ggplot(aes(x= time, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(aes(y = mean), linewidth = 1) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Time (days)", y = "Cumulatitive Invasive Infections Cases", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(name = "Intervention", values = intervention_colors, aesthetics = c("color", "fill")) +
  theme_bw() +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 12))
cum_infection_timeseries
ggsave("figures/LTACH_IPC70_update/cum_infections_timeseries_1000sim_avg_everything.png", plot = cum_infection_timeseries, width = 9, height = 8, unit = "in", dpi = 300)





# End point of Cumulative Colonization, Clinical, and Invasive infections -----------------------------------------

# Figure of cumulative colonization cases on average after 2 years - Only High Transmission in Tier 3
end_cum_colonization_tier3_hightrans <- cost_effect_avg_df %>%
  filter(import == 0.04 & beta == 0.066) %>%
  ggplot(aes(x= intervention, y = C_tot_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative colonization cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_colonization_tier3_hightrans
ggsave("figures/LTACH_IPC70_update/cumulative_colonization_Tier3_HighTrans_end2yr.png", plot = end_cum_colonization_tier3_hightrans, width = 5.5, height = 6.5, unit = "in", dpi = 300)



# Figure of cumulative colonization cases on average after 2 years - Only High Transmission in Tier 3
# with error bars for upper and lower 95% prediction interval
hightrans_tier3_prediction_interval <- filter(avg_state_long_df, time == 730, state == "C_tot", beta == 0.066, import == 0.04)

end_cum_colonization_tier3_hightrans_errorbars <- avg_state_long_df %>%
  filter(time == 730, state == "C_tot", import == 0.04 & beta == 0.066) %>%
  ggplot(aes(x= intervention, y = mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative colonization cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_colonization_tier3_hightrans_errorbars
ggsave("figures/LTACH_IPC70_update/cumulative_colonization_Tier3_HighTrans_end2yr_errorbars.png", plot = end_cum_colonization_tier3_hightrans_errorbars, width = 5.5, height = 6.5, unit = "in", dpi = 300)



# Figure of cumulative colonization cases on average after 2 years
end_cum_colonization <- cost_effect_avg_df %>%
  ggplot(aes(x= intervention, y = C_tot_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative colonization cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_colonization
ggsave("figures/LTACH_IPC70_update/cumulative_colonization_end2yr.png", plot = end_cum_colonization, width = 8, height = 9, unit = "in", dpi = 300)

end_cum_colonization_errorbar <- avg_state_long_df %>%
  filter(time == 730, state == "C_tot") %>%
  ggplot(aes(x= intervention, y = mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative colonization cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_colonization_errorbar
ggsave("figures/LTACH_IPC70_update/cumulative_colonization_end2yr_errorbars.png", plot = end_cum_colonization_errorbar, width = 8, height = 9, unit = "in", dpi = 300)


end_cum_colonization_errorbar_free_y <- avg_state_long_df %>%
  filter(time == 730, state == "C_tot") %>%
  ggplot(aes(x= intervention, y = mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Avg cumulative colonization cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_colonization_errorbar_free_y
ggsave("figures/LTACH_IPC70_update/cumulative_colonization_end2yr_errorbars_free_y.png", plot = end_cum_colonization_errorbar_free_y, width = 8, height = 9, unit = "in", dpi = 300)



# Figure of cumulative colonization cases averted on average after 2 years
end_cum_colonization_averted <- cost_effect_avg_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = col_cases_averted_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative colonization cases averted after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_colonization_averted
ggsave("figures/LTACH_IPC70_update/averted_cumulative_colonization_end2yr.png", plot = end_cum_colonization_averted, width = 10, height = 9, unit = "in", dpi = 300)


end_cum_colonization_averted_free_y <- cost_effect_avg_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = col_cases_averted_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Avg cumulative colonization cases averted after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_colonization_averted_free_y
ggsave("figures/LTACH_IPC70_update/averted_cumulative_colonization_end2yr_free_y.png", plot = end_cum_colonization_averted_free_y, width = 10, height = 9, unit = "in", dpi = 300)




# Figure of cumulative clinical cases on average after 2 years
end_cum_clinical <- cost_effect_avg_df %>%
  ggplot(aes(x= intervention, y = Clinical_tot_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative clinical cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_clinical
ggsave("figures/LTACH_IPC70_update/cumulative_clinical_end2yr.png", plot = end_cum_clinical, width = 8, height = 8, unit = "in", dpi = 300)


end_cum_clinical_errorbars <- avg_state_long_df %>%
  filter(time == 730, state == "Clinical_tot") %>%
  ggplot(aes(x= intervention, y = mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative clinical cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_clinical_errorbars
ggsave("figures/LTACH_IPC70_update/cumulative_clinical_end2yr_errorbars.png", plot = end_cum_clinical_errorbars, width = 8, height = 9, unit = "in", dpi = 300)



end_cum_clinical_errorbars_free_y <- avg_state_long_df %>%
  filter(time == 730, state == "Clinical_tot") %>%
  ggplot(aes(x= intervention, y = mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Avg cumulative clinical cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_clinical_errorbars_free_y
ggsave("figures/LTACH_IPC70_update/cumulative_clinical_end2yr_errorbars_free_y.png", plot = end_cum_clinical_errorbars_free_y, width = 8, height = 9, unit = "in", dpi = 300)




# Figure of cumulative clinical cases averted on average after 2 years
end_cum_clinical_averted <- cost_effect_avg_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = clinical_cases_averted_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative clinical cases averted after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_clinical_averted
ggsave("figures/LTACH_IPC70_update/averted_cumulative_clinical_end2yr.png", plot = end_cum_clinical_averted, width = 10, height = 9, unit = "in", dpi = 300)





# Figure of cumulative invasive infections cases on average after 2 years
end_cum_infections <- cost_effect_avg_df %>%
  ggplot(aes(x= intervention, y = I_tot_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative invasive infections cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_infections
ggsave("figures/LTACH_IPC70_update/cumulative_infections_end2yr.png", plot = end_cum_infections, width = 8, height = 9, unit = "in", dpi = 300)


end_cum_infections_errorbars <- avg_state_long_df %>%
  filter(time == 730, state == "I_tot") %>%
  ggplot(aes(x= intervention, y = mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative invasive infections cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_infections_errorbars
ggsave("figures/LTACH_IPC70_update/cumulative_infections_end2yr_errorbars.png", plot = end_cum_infections_errorbars, width = 8, height = 9, unit = "in", dpi = 300)


end_cum_infections_errorbars_free_y <- avg_state_long_df %>%
  filter(time == 730, state == "I_tot") %>%
  ggplot(aes(x= intervention, y = mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black") +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Avg cumulative invasive infections cases after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_infections_errorbars_free_y
ggsave("figures/LTACH_IPC70_update/cumulative_infections_end2yr_errorbars_free_y.png", plot = end_cum_infections_errorbars_free_y, width = 8, height = 9, unit = "in", dpi = 300)





# Figure of cumulative invasive infections cases averted on average after 2 years
end_cum_infections_averted <- cost_effect_avg_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = invasive_cases_averted_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg cumulative invasive infections cases averted after 2 yrs", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_cum_infections_averted
ggsave("figures/LTACH_IPC70_update/averted_cumulative_infections_end2yr.png", plot = end_cum_infections_averted, width = 8, height = 8, unit = "in", dpi = 300)




# Last 6 months mean proportion of colonized patients------------------------------------
# Figure of facility colonization prevalence on average in last 6 months of simulation
end_prev <- last_6mo_prev_inc_df %>%
  ggplot(aes(x= intervention, y = prop_colonized_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymax = prop_colonized_upper, ymin = prop_colonized_lower), width = 0.15, alpha = 0.5) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg proportion colonized patients last 6 months", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_prev
ggsave("figures/LTACH_IPC70_update/prop_colonized_last6mo.png", plot = end_prev, width = 8, height = 9, unit = "in", dpi = 300)

end_prev_free_y <- last_6mo_prev_inc_df %>%
  ggplot(aes(x= intervention, y = prop_colonized_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymax = prop_colonized_upper, ymin = prop_colonized_lower), width = 0.15, alpha = 0.5) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Avg proportion colonized patients last 6 months", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_prev_free_y
ggsave("figures/LTACH_IPC70_update/prop_colonized_last6mo_free_y.png", plot = end_prev_free_y, width = 8, height = 9, unit = "in", dpi = 300)



# Percent reduction in colonization prevalence in last 6 months
last_6mo_prev_percent_reduction <- last_6mo_prev_inc_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = percent_reduction_colonization, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Percent reduction in proportion colonized patients", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
last_6mo_prev_percent_reduction
ggsave("figures/LTACH_IPC70_update/percent_reduced_prop_colonized_last6mo.png", plot = last_6mo_prev_percent_reduction, width = 8, height = 9, unit = "in", dpi = 300)


last_6mo_prev_percent_reduction_free_y <- last_6mo_prev_inc_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = percent_reduction_colonization, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Percent reduction in proportion colonized patients", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
last_6mo_prev_percent_reduction_free_y
ggsave("figures/LTACH_IPC70_update/percent_reduced_prop_colonized_last6mo_free_y.png", plot = last_6mo_prev_percent_reduction_free_y, width = 8, height = 9, unit = "in", dpi = 300)




# Change in colonization prevalence in last 6 months
last_6mo_prev_change <- last_6mo_prev_inc_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = colonization_change, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Change in proportion colonized patients", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
last_6mo_prev_change
ggsave("figures/LTACH_IPC70_update/change_prop_colonized_last6mo.png", plot = last_6mo_prev_change, width = 8, height = 9, unit = "in", dpi = 300)


last_6mo_prev_change_free_y <- last_6mo_prev_inc_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = colonization_change, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Change in proportion colonized patients", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
last_6mo_prev_change_free_y
ggsave("figures/LTACH_IPC70_update/change_prop_colonized_last6mo_free_y.png", plot = last_6mo_prev_change_free_y, width = 8, height = 9, unit = "in", dpi = 300)




# Figure of facility monthly incidence on average in last 6 months of simulation
end_monthlyinc <- last_6mo_prev_inc_df %>%
  ggplot(aes(x= intervention, y = monthly_col_incidence_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymax = monthly_col_incidence_upper, ymin = monthly_col_incidence_lower), width = 0.15, alpha = 0.5) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Avg monthly incidence of colonization cases last 6 months", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_monthlyinc
ggsave("figures/LTACH_IPC70_update/monthly_incidence_last6mo.png", plot = end_monthlyinc, width = 8, height = 9, unit = "in", dpi = 300)

end_monthlyinc_free_y <- last_6mo_prev_inc_df %>%
  ggplot(aes(x= intervention, y = monthly_col_incidence_mean, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymax = monthly_col_incidence_upper, ymin = monthly_col_incidence_lower), width = 0.15, alpha = 0.5) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Avg monthly incidence of colonization cases last 6 months", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
end_monthlyinc_free_y
ggsave("figures/LTACH_IPC70_update/monthly_incidence_last6mo_free_y.png", plot = end_monthlyinc_free_y, width = 8, height = 9, unit = "in", dpi = 300)



# Percent reduction in colonization monthly incidence in last 6 months
last_6mo_inc_percent_reduction <- last_6mo_prev_inc_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = percent_reduction_monthly_inc, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Percent reduction in average monthly incidence", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
last_6mo_inc_percent_reduction
ggsave("figures/LTACH_IPC70_update/percent_reduced_monthly_incidence_last6mo.png", plot = last_6mo_inc_percent_reduction, width = 8, height = 9, unit = "in", dpi = 300)


last_6mo_inc_percent_reduction_free_y <- last_6mo_prev_inc_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = percent_reduction_monthly_inc, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Percent reduction in average monthly incidence", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
last_6mo_inc_percent_reduction_free_y
ggsave("figures/LTACH_IPC70_update/percent_reduced_monthly_incidence_last6mo_free_y.png", plot = last_6mo_inc_percent_reduction_free_y, width = 8, height = 9, unit = "in", dpi = 300)



# Change in colonization monthly incidence in last 6 months
last_6mo_inc_change <- last_6mo_prev_inc_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = monthly_inc_change, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_grid(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs)) +
  labs(x = "Intervention", y = "Change in average monthly incidence", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
last_6mo_inc_change
ggsave("figures/LTACH_IPC70_update/change_monthly_incidence_last6mo.png", plot = last_6mo_inc_change, width = 8, height = 9, unit = "in", dpi = 300)


last_6mo_inc_change_free_y <- last_6mo_prev_inc_df %>%
  filter(intervention != "No Screening") %>%
  ggplot(aes(x= intervention, y = monthly_inc_change, fill = intervention)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Change in average monthly incidence", subtitle = paste0(fac_type_label, ", IPC efficacy ", ipc_eff_label, ", n_sim = ", n_sim, sep = "")) +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, angle = 70, vjust = 1, hjust=1), axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 13), strip.text = element_text(size = 11))
last_6mo_inc_change_free_y
ggsave("figures/LTACH_IPC70_update/change_monthly_incidence_last6mo_free_y.png", plot = last_6mo_inc_change_free_y, width = 8, height = 9, unit = "in", dpi = 300)

