# Figures comparing LTACH facilities with IPC 50% vs IPC 70%

# libraries
library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggh4x)
library(ggpattern)
library(patchwork)

# Set figure conditions
# set labels for facets
trans_labs <- c("Low Transmisssion", "High Transmission")
names(trans_labs) <- c(0.0175, 0.066)

import_labs <- c("Tier 2", "Tier 3", "Tier 4")
names(import_labs) <- c(0.01, 0.04, 0.1)

ipc_shape <- c(1,2)
names(ipc_shape) <- c("IPC 50%", "IPC 70%")

n_sim <- 1000


# color labels for intervention
intervention_colors <- c("#E31A1C", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#FFDB00", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594")
names(intervention_colors) <- c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")





# read in data for figures
LTACH_IPC50_cost_effect_avg_df <- read.csv(here("data_output/LTACH_IPC50_model_update/cost_effect_avg_data.csv")) %>%
  mutate(facility = "LTACH", ipc_eff = "IPC50") %>%
  select(facility, ipc_eff, import:screen_efficiency)
LTACH_IPC70_cost_effect_avg_df <- read.csv(here("data_output/LTACH_IPC70_model_update/cost_effect_avg_data.csv"))  %>%
  mutate(facility = "LTACH", ipc_eff = "IPC70") %>%
  select(facility, ipc_eff, import:screen_efficiency)


# combine data
combined_cost_effect_avg_df <- rbind(LTACH_IPC50_cost_effect_avg_df, LTACH_IPC70_cost_effect_avg_df)



fig_long_all_df <- combined_cost_effect_avg_df %>%
  mutate(intervention = factor(intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment Admissions + PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                     "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                     "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                     "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")),
         trans = case_when(
           beta == 0.0175 ~ "Low Transmission",
           beta == 0.066 ~ "High Transmission"),
         trans = factor(trans, levels = c("Low Transmission", "High Transmission")),
         ipc_eff = factor(ipc_eff, levels = c("IPC50", "IPC70")))

fig_long_df <- fig_long_all_df %>%
  filter(intervention != "No Screening")

# Baseline No Screening figures -------------------------------

# Mean cumulative colonization after 2 years for No Screening only
# Faceted by transmission, Tiers on x-axis, IPC 50% (solid) vs 70% (stripe)

no_screen_baseline_df <- fig_long_all_df %>%
  filter(intervention == "No Screening") %>%
  mutate(tier = factor(case_when(
    import == 0.01 ~ "Tier 2",
    import == 0.04 ~ "Tier 3",
    import == 0.1  ~ "Tier 4"
  ), levels = c("Tier 2", "Tier 3", "Tier 4")))

no_screen_cum_col <- no_screen_baseline_df %>%
  ggplot(aes(x = tier, y = C_tot_mean, pattern = ipc_eff)) +
  geom_bar_pattern(
    position     = position_dodge(width = 0.8),
    stat         = "identity",
    fill         = "#E31A1C",
    color        = "black",
    pattern_fill = "black",
    pattern_angle   = 45,
    pattern_density = 0.15,
    pattern_spacing = 0.05,
    pattern_key_scale_factor = 0.2,
    width = 0.7
  ) +
  geom_errorbar(
    aes(ymin = C_tot_lower, ymax = C_tot_upper),
    col      = "darkgray",
    position = position_dodge(width = 0.8),
    width    = 0.25
  ) +
  facet_wrap(~ trans) +
  labs(
    x       = "Admission Prevalence Tier",
    y       = "Mean Cumulative Colonization\nCases After 2 Years",
    title   = "No Screening",
    pattern = "IPC Efficacy"
  ) +
  scale_pattern_manual(
    values = c(IPC50 = "none", IPC70 = "stripe"),
    labels = c("IPC 50%", "IPC 70%")
  ) +
  theme_bw() +
  theme(
    axis.text    = element_text(size = 11, color = "black"),
    axis.title   = element_text(size = 13, color = "black"),
    strip.text   = element_text(size = 12, color = "black"),
    legend.text  = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold")
  ) +
  guides(pattern = guide_legend(override.aes = list(fill = "#E31A1C")))
no_screen_cum_col
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_no_screening_cum_colonization_by_tiers.png",
       plot = no_screen_cum_col, width = 8, height = 5, unit = "in", dpi = 300)



# Colonization figures -------------------------------


# Figure of cumulative colonization cases on average after 2 years comparing IPC 50 and 70
# Side-by-side bar plot with 95% prediction interval
end_cum_colonization_bar_95PI <- fig_long_all_df %>%
  ggplot(aes(x= intervention, y = C_tot_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = C_tot_lower, ymax = C_tot_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative colonization cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",),
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_colonization_bar_95PI
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_cumulative_colonization_bar_95PI_gray.png", plot = end_cum_colonization_bar_95PI, width = 13, height = 9, unit = "in", dpi = 300)


# Combined panel figure: Panel A (No Screening baseline) + Panel B (All interventions)
# Pattern legend from panel A only (renders stripe correctly); fill legend from panel B only
panel_a_combined <- no_screen_cum_col +
  labs(title = "A    No Screening", pattern = "IPC Efficacy") +
  theme(plot.title = element_text(size = 13, face = "bold")) +
  guides(pattern = guide_legend(order = 1, override.aes = list(fill = "white")))

panel_b_combined <- end_cum_colonization_bar_95PI +
  labs(subtitle = NULL, title = "B    All Interventions") +
  theme(plot.title = element_text(size = 13, face = "bold")) +
  guides(pattern = "none",
         fill = guide_legend(order = 2, ncol = 4, override.aes = list(pattern = "none")))

combined_col_panel_fig <- (panel_a_combined / panel_b_combined) +
  plot_layout(heights = c(1, 1.8), guides = "collect") &
  theme(
    legend.position  = "bottom",
    legend.box       = "vertical",
    legend.box.just  = "center",
    legend.margin    = margin(t = 5, b = 5),
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 11, face = "bold"),
    legend.key.size  = unit(0.8, "lines")
  )

combined_col_panel_fig
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_combined_colonization_panel.tiff",
       plot = combined_col_panel_fig, width = 13, height = 14, unit = "in", dpi = 400)




# Figure of cumulative colonization cases averted on average after 2 years comparing IPC 50 and 70
# Side-by-side bar plot without error bars
end_cum_colonization_averted_bar <- fig_long_df %>%
  ggplot(aes(x= intervention, y = col_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative colonization cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_colonization_averted_bar
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_colonization_bar_free_y.png", plot = end_cum_colonization_averted_bar, width = 13, height = 9, unit = "in", dpi = 300)





# Side-by-side bar plot with 95% prediction interval
end_cum_colonization_averted_bar_95PI <- fig_long_df %>%
  ggplot(aes(x= intervention, y = col_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = col_cases_averted_lower, ymax = col_cases_averted_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative colonization cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_colonization_averted_bar_95PI
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_colonization_bar_95PI_gray.png", plot = end_cum_colonization_averted_bar_95PI, width = 13, height = 9, unit = "in", dpi = 300)


# Side-by-side bar plot with standard error bars
end_cum_colonization_averted_bar_se <- fig_long_df %>%
  ggplot(aes(x= intervention, y = col_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = col_cases_averted_mean-col_cases_averted_se, ymax = col_cases_averted_mean+col_cases_averted_se), col = "black", position = position_dodge(width = 1), width = 0.5) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative colonization cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_colonization_averted_bar_se
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_colonization_bar_SE.png", plot = end_cum_colonization_averted_bar_se, width = 13, height = 9, unit = "in", dpi = 300)




# Figure of relative risk reduction (RRR) of colonization cases on average after 2 years comparing IPC 50 and 70
# RRR is essentially the percent reduction

# Side-by-side bar plot without error bars
end_col_RRR_bar <- fig_long_df %>%
  ggplot(aes(x= intervention, y = RRR_col_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "RRR of colonization cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_col_RRR_bar
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_col_RRR_bar_free_y.png", plot = end_col_RRR_bar, width = 13, height = 9, unit = "in", dpi = 300)


# Side-by-side bar plot with 95% prediction interval
end_col_RRR_bar_95PI <- fig_long_df %>%
  ggplot(aes(x= intervention, y = RRR_col_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = RRR_col_lower, ymax = RRR_col_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "RRR of colonization cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_col_RRR_bar_95PI
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_col_RRR_bar_95PI.png", plot = end_col_RRR_bar_95PI, width = 13, height = 9, unit = "in", dpi = 300)



# Side-by-side bar plot with standard error bars
end_col_RRR_bar_SE <- fig_long_df %>%
  ggplot(aes(x= intervention, y = RRR_col_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = RRR_col_mean-RRR_col_se, ymax = RRR_col_mean+RRR_col_se), col = "black", position = position_dodge(width = 1), width = 0.5) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "RRR of colonization cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_col_RRR_bar_SE
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_col_RRR_bar_SE.png", plot = end_col_RRR_bar_SE, width = 13, height = 9, unit = "in", dpi = 300)




# Colonization figures - subset of interventions -------------------------------
# Duplicate set of the colonization figures above with 3 containment scenarios removed.
# The interventions are dropped from the data, the x-axis, and the fill legend.

dropped_interventions <- c("Containment Admissions + PPS (14 days)",
                           "Containment PPS (14 days) w/ taper",
                           "Containment Admissions + PPS (14 days) w/ taper")

fig_long_all_sub_df <- fig_long_all_df %>%
  filter(!intervention %in% dropped_interventions) %>%
  mutate(intervention = droplevels(intervention))

fig_long_sub_df <- fig_long_df %>%
  filter(!intervention %in% dropped_interventions) %>%
  mutate(intervention = droplevels(intervention))


# Figure of cumulative colonization cases on average after 2 years comparing IPC 50 and 70
# Side-by-side bar plot with 95% prediction interval
end_cum_colonization_bar_95PI_sub <- fig_long_all_sub_df %>%
  ggplot(aes(x= intervention, y = C_tot_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = C_tot_lower, ymax = C_tot_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative colonization cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",),
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_colonization_bar_95PI_sub
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_cumulative_colonization_bar_95PI_gray_subset.png", plot = end_cum_colonization_bar_95PI_sub, width = 13, height = 9, unit = "in", dpi = 300)


# Combined panel figure: Panel A (No Screening baseline) + Panel B (subset of interventions)
# Pattern legend from panel A only (renders stripe correctly); fill legend from panel B only
panel_b_combined_sub <- end_cum_colonization_bar_95PI_sub +
  labs(subtitle = NULL, title = "B    All Interventions") +
  theme(plot.title = element_text(size = 13, face = "bold")) +
  guides(pattern = "none",
         fill = guide_legend(order = 2, ncol = 4, override.aes = list(pattern = "none")))

combined_col_panel_fig_sub <- (panel_a_combined / panel_b_combined_sub) +
  plot_layout(heights = c(1, 1.8), guides = "collect") &
  theme(
    legend.position  = "bottom",
    legend.box       = "vertical",
    legend.box.just  = "center",
    legend.margin    = margin(t = 5, b = 5),
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 11, face = "bold"),
    legend.key.size  = unit(0.8, "lines")
  )

combined_col_panel_fig_sub
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_combined_colonization_panel_subset.tiff",
       plot = combined_col_panel_fig_sub, width = 13, height = 14, unit = "in", dpi = 400)




# Figure of cumulative colonization cases averted on average after 2 years comparing IPC 50 and 70
# Side-by-side bar plot without error bars
end_cum_colonization_averted_bar_sub <- fig_long_sub_df %>%
  ggplot(aes(x= intervention, y = col_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative colonization cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",),
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_colonization_averted_bar_sub
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_colonization_bar_free_y_subset.png", plot = end_cum_colonization_averted_bar_sub, width = 13, height = 9, unit = "in", dpi = 300)




# Side-by-side bar plot with 95% prediction interval
end_cum_colonization_averted_bar_95PI_sub <- fig_long_sub_df %>%
  ggplot(aes(x= intervention, y = col_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = col_cases_averted_lower, ymax = col_cases_averted_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative colonization cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",),
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_colonization_averted_bar_95PI_sub
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_colonization_bar_95PI_gray_subset.png", plot = end_cum_colonization_averted_bar_95PI_sub, width = 13, height = 9, unit = "in", dpi = 300)


# Side-by-side bar plot with standard error bars
end_cum_colonization_averted_bar_se_sub <- fig_long_sub_df %>%
  ggplot(aes(x= intervention, y = col_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = col_cases_averted_mean-col_cases_averted_se, ymax = col_cases_averted_mean+col_cases_averted_se), col = "black", position = position_dodge(width = 1), width = 0.5) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative colonization cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",),
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_colonization_averted_bar_se_sub
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_colonization_bar_SE_subset.png", plot = end_cum_colonization_averted_bar_se_sub, width = 13, height = 9, unit = "in", dpi = 300)




# Figure of relative risk reduction (RRR) of colonization cases on average after 2 years comparing IPC 50 and 70
# RRR is essentially the percent reduction

# Side-by-side bar plot without error bars
end_col_RRR_bar_sub <- fig_long_sub_df %>%
  ggplot(aes(x= intervention, y = RRR_col_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "RRR of colonization cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",),
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_col_RRR_bar_sub
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_col_RRR_bar_free_y_subset.png", plot = end_col_RRR_bar_sub, width = 13, height = 9, unit = "in", dpi = 300)


# Side-by-side bar plot with 95% prediction interval
end_col_RRR_bar_95PI_sub <- fig_long_sub_df %>%
  ggplot(aes(x= intervention, y = RRR_col_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = RRR_col_lower, ymax = RRR_col_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "RRR of colonization cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",),
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_col_RRR_bar_95PI_sub
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_col_RRR_bar_95PI_subset.png", plot = end_col_RRR_bar_95PI_sub, width = 13, height = 9, unit = "in", dpi = 300)



# Side-by-side bar plot with standard error bars
end_col_RRR_bar_SE_sub <- fig_long_sub_df %>%
  ggplot(aes(x= intervention, y = RRR_col_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1),
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = RRR_col_mean-RRR_col_se, ymax = RRR_col_mean+RRR_col_se), col = "black", position = position_dodge(width = 1), width = 0.5) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "RRR of colonization cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",),
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_col_RRR_bar_SE_sub
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_col_RRR_bar_SE_subset.png", plot = end_col_RRR_bar_SE_sub, width = 13, height = 9, unit = "in", dpi = 300)




# Clinical figures -------------------------------


# Figure of cumulative clinical cases on average after 2 years comparing IPC 50 and 70
# Side-by-side bar plot with 95% prediction interval
end_cum_clinical_bar_95PI <- fig_long_all_df %>%
  ggplot(aes(x= intervention, y = Clinical_tot_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = Clinical_tot_lower, ymax = Clinical_tot_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative clinical cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_clinical_bar_95PI
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_cumulative_clinicial_bar_95PI_gray.png", plot = end_cum_clinical_bar_95PI, width = 13, height = 9, unit = "in", dpi = 300)








# Figure of cumulative clinical cases averted on average after 2 years comparing IPC 50 and 70
# Side-by-side bar plot without error bars
end_cum_clinical_averted_bar <- fig_long_df %>%
  ggplot(aes(x= intervention, y = clinical_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative clinical cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_clinical_averted_bar
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_clinical_bar_free_y.png", plot = end_cum_clinical_averted_bar, width = 13, height = 9, unit = "in", dpi = 300)





# Side-by-side bar plot with 95% prediction interval
end_cum_clinical_averted_bar_95PI <- fig_long_df %>%
  ggplot(aes(x= intervention, y = clinical_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = clinical_cases_averted_lower, ymax = clinical_cases_averted_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative clinical cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_clinical_averted_bar_95PI
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_clinical_bar_95PI_gray.png", plot = end_cum_clinical_averted_bar_95PI, width = 13, height = 9, unit = "in", dpi = 300)


# Side-by-side bar plot with standard error bars
end_cum_clinical_averted_bar_se <- fig_long_df %>%
  ggplot(aes(x= intervention, y = clinical_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = clinical_cases_averted_mean-clinical_cases_averted_se, ymax = clinical_cases_averted_mean+clinical_cases_averted_se), col = "black", position = position_dodge(width = 1), width = 0.5) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative clinical cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_clinical_averted_bar_se
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_clinical_bar_SE.png", plot = end_cum_clinical_averted_bar_se, width = 13, height = 9, unit = "in", dpi = 300)


### NOTE: The RRR calculation leads to many undefined values when calculated on the individual sims because of zeros in the no screening scenario
# Recommend either not using these figures because this calculation of the mean RRR will depend on substituting a value close to zero 
# and likely change the mean depending on how small to make that replacement value.

# Figure of relative risk reduction (RRR) of clinical cases on average after 2 years comparing IPC 50 and 70
# RRR is essentially the percent reduction

# Side-by-side bar plot without error bars
end_clinical_RRR_bar <- fig_long_df %>%
  ggplot(aes(x= intervention, y = RRR_clinical_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "RRR of clinical cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_clinical_RRR_bar
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_clinical_RRR_bar_free_y.png", plot = end_clinical_RRR_bar, width = 13, height = 9, unit = "in", dpi = 300)


# Side-by-side bar plot with 95% prediction interval
end_clinical_RRR_bar_95PI <- fig_long_df %>%
  ggplot(aes(x= intervention, y = RRR_clinical_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = RRR_clinical_lower, ymax = RRR_clinical_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "RRR of clinical cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_clinical_RRR_bar_95PI
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_clinical_RRR_bar_95PI.png", plot = end_clinical_RRR_bar_95PI, width = 13, height = 9, unit = "in", dpi = 300)



# Side-by-side bar plot with standard error bars
end_clinical_RRR_bar_SE <- fig_long_df %>%
  ggplot(aes(x= intervention, y = RRR_clinical_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = RRR_clinical_mean-RRR_clinical_se, ymax = RRR_clinical_mean+RRR_clinical_se), col = "black", position = position_dodge(width = 1), width = 0.5) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "RRR of clinical cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_clinical_RRR_bar_SE
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_clinical_RRR_bar_SE.png", plot = end_clinical_RRR_bar_SE, width = 13, height = 9, unit = "in", dpi = 300)






# Invasive infection figures -------------------------------




# Figure of cumulative invasive infections cases on average after 2 years comparing IPC 50 and 70
# Side-by-side bar plot with 95% prediction interval
end_cum_invasive_bar_95PI <- fig_long_all_df %>%
  ggplot(aes(x= intervention, y = I_tot_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = I_tot_lower, ymax = I_tot_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative invasive cases after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_invasive_bar_95PI
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_cumulative_invasive_bar_95PI_gray.png", plot = end_cum_invasive_bar_95PI, width = 13, height = 9, unit = "in", dpi = 300)







# Figure of cumulative invasive infection cases averted on average after 2 years comparing IPC 50 and 70
# Side-by-side bar plot without error bars
end_cum_invasive_averted_bar <- fig_long_df %>%
  ggplot(aes(x= intervention, y = invasive_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative invasive cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_invasive_averted_bar
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_invasive_bar_free_y.png", plot = end_cum_invasive_averted_bar, width = 13, height = 9, unit = "in", dpi = 300)





# Side-by-side bar plot with 95% prediction interval
end_cum_invasive_averted_bar_95PI <- fig_long_df %>%
  ggplot(aes(x= intervention, y = invasive_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = invasive_cases_averted_lower, ymax = invasive_cases_averted_upper), col = "darkgray", position = position_dodge(width = 1), width = 0.35) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative invasive cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_invasive_averted_bar_95PI
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_invasive_bar_95PI_gray.png", plot = end_cum_invasive_averted_bar_95PI, width = 13, height = 9, unit = "in", dpi = 300)


# Side-by-side bar plot with standard error bars
end_cum_invasive_averted_bar_se <- fig_long_df %>%
  ggplot(aes(x= intervention, y = invasive_cases_averted_mean, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  geom_errorbar(aes(ymin = invasive_cases_averted_mean-invasive_cases_averted_se, ymax = invasive_cases_averted_mean+invasive_cases_averted_se), col = "black", position = position_dodge(width = 1), width = 0.5) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean cumulative invasive cases averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
end_cum_invasive_averted_bar_se
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_averted_cumulative_invasive_bar_SE.png", plot = end_cum_invasive_averted_bar_se, width = 13, height = 9, unit = "in", dpi = 300)


# Screening efficiency ---------------------------------------------------


# Figure of screening efficiency per averted colonization case on average after 2 years comparing IPC 50 and 70
# Side-by-side bar plot without error bars
screen_efficiency_col_averted_bar <- fig_long_df %>%
  ggplot(aes(x= intervention, y = screen_efficiency, fill = intervention, pattern = ipc_eff)) +
  geom_bar_pattern(position = position_dodge(width = 1), 
                   stat = "identity",
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.2) +
  ggh4x::facet_grid2(import ~ trans, labeller = labeller(import = import_labs), scales = "free_y", independent = "y") +
  labs(x = "Intervention", y = "Mean No. of swabs per case averted after 2 yrs", subtitle = "LTACH IPC 50% vs. IPC 70%", pattern = "IPC efficacy", fill = "Intervention") +
  scale_fill_manual(values = intervention_colors, aesthetics = "fill") +
  scale_pattern_manual(values = c(IPC50 = "none", IPC70 = "stripe"), labels = c("IPC 50%", "IPC 70%")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11, color = "black", angle = 80, vjust = 1, hjust=1), axis.text.y = element_text(size = 11, color = "black",), 
        axis.title = element_text(size = 13, color = "black",), strip.text = element_text(size = 11, color = "black",)) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))
screen_efficiency_col_averted_bar
ggsave("figures/LTACH_IPC50_update/LTACH_IPC50_v_70_screening_efficiency_bar_free_y.png", plot = screen_efficiency_col_averted_bar, width = 13, height = 9, unit = "in", dpi = 300)



