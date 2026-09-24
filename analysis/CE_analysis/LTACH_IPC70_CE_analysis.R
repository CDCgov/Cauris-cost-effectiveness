# LTACH IPC 70 Cost-effectiveness of screening analysis


# Effect: Colonization cases averted due to screening
# Costs: Screening costs, IPC costs, Total costs (screening + clinical tests + IPC)



# Load libraries and data ------------------------------------------------------------------------------
# load libraries
library(here)        # reproducible file directories when sharing repositories
library(tidyverse)   # manipulate data and visualizations
library(estimatr)    # robust linear regression
library(glmmTMB)     # generalized linear regressions
library(emmeans)     # evaluate the differences in interactions between terms
library(dampack)     # decision analytic modeling package (cost-effectiveness analysis)
library(purrr)       # convert loops into functional programming
library(future)      # allow for parallelization
library(furrr)       # combine purrr map and future parallelization


# read in LTACH IPC70 model output data
LTACH_IPC70_cost_effect_avg_df <- read.csv(here("data_output/LTACH_IPC70_model_update/cost_effect_avg_data.csv"))
LTACH_IPC70_cost_effect_data_df <- read.csv(here("data_output/LTACH_IPC70_model_update/cost_effect_data.csv"))



# make intervention a factor and arrange data in the same order
LTACH_IPC70_cost_effect_data_df <- LTACH_IPC70_cost_effect_data_df %>%
  mutate(intervention = factor(intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", 
                                                        "Containment Admissions + PPS (14 days) w/ taper", "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", 
                                                        "PPS (30 days)", "PPS (14 days)", "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                        "Admissions + PPS (30 days)", "Admissions + PPS (14 days)"))) %>%
  arrange(import, beta, intervention)


LTACH_IPC70_cost_effect_avg_df <- LTACH_IPC70_cost_effect_avg_df %>%
  mutate(intervention = factor(intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", 
                                                        "Containment Admissions + PPS (14 days) w/ taper", "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", 
                                                        "PPS (30 days)", "PPS (14 days)", "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                        "Admissions + PPS (30 days)", "Admissions + PPS (14 days)"))) %>%
  arrange(import, beta, intervention)

str(LTACH_IPC70_cost_effect_data_df)

# Calculate cost and effect  and ICERs ------------------------------------------------------------------------------


# Test calculate icers for each combination of beta and importation rates

trans <- c(0.0175, 0.0660)
import <- c(0.01, 0.04, 0.1)


LTACH_IPC70_total_results <- data.frame(Strategy = character(),
                            Cost = numeric(),
                            Effect = numeric(), 
                            Inc_Cost = numeric(),
                            Inc_Effect = numeric(),
                            ICER = numeric(),
                            Status = character(),
                            beta = numeric(),
                            import = numeric())

LTACH_IPC70_screen_results <- data.frame(Strategy = character(),
                             Cost = numeric(),
                             Effect = numeric(), 
                             Inc_Cost = numeric(),
                             Inc_Effect = numeric(),
                             ICER = numeric(),
                             Status = character(),
                             beta = numeric(),
                             import = numeric())


for(i in trans){
  for(j in import){
    
    # filter data to a single beta and import combination
    subset_df <- LTACH_IPC70_cost_effect_avg_df %>%
      filter(beta == i, import == j) 
    
    # calculate total and screening icers and which interventions are dominated per group
    icer_total <- calculate_icers(cost = subset_df$total_cost_mean,
                                  effect = subset_df$col_cases_averted_mean,
                                  strategies = subset_df$intervention) %>%
      mutate(beta = i, import = j) %>%
      rename(intervention = "Strategy")
    icer_screen <- calculate_icers(cost = subset_df$screen_cost_mean,
                                   effect = subset_df$col_cases_averted_mean,
                                   strategies = subset_df$intervention) %>%
      mutate(beta = i, import = j)  %>%
      rename(intervention = "Strategy")
    
    # store results
    LTACH_IPC70_total_results <- rbind(LTACH_IPC70_total_results, icer_total)
    LTACH_IPC70_screen_results <- rbind(LTACH_IPC70_screen_results, icer_screen)
    
  }
}

# look at final data frames
LTACH_IPC70_total_results

LTACH_IPC70_screen_results

# filter to only include the non-dominated interventions
LTACH_IPC70_non_dominated_total_results <- filter(LTACH_IPC70_total_results, Status == "ND")

LTACH_IPC70_non_dominated_screen_results <- filter(LTACH_IPC70_screen_results, Status == "ND")


# smaller data frame with just the scenario meta data and dominated status
LTACH_IPC70_total_dominated_status <- LTACH_IPC70_total_results %>%
  select(beta, import, intervention, Status)


LTACH_IPC70_screen_dominated_status <- LTACH_IPC70_screen_results %>%
  select(beta, import, intervention, Status)

# create combined data frame to export to data_output
LTACH_IPC70_total_results2 <- LTACH_IPC70_total_results %>% 
  mutate(cost_type = "total costs")

LTACH_IPC70_screen_results2 <- LTACH_IPC70_screen_results  %>% 
  mutate(cost_type = "screening costs")

LTACH_IPC70_ICER_results <- rbind(LTACH_IPC70_screen_results2, LTACH_IPC70_total_results2) %>% 
  select(cost_type, beta, import, intervention, Cost, Effect, Inc_Cost, Inc_Effect, ICER, Status)
write.csv(LTACH_IPC70_ICER_results, here("data_output/LTACH_IPC70_model_update/LTACH_IPC70_ICER_results.csv"))

# Create cost-effectiveness figures --------------------------------------------

# color labels for intervention
intervention_colors <- c("#E31A1C", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#FFDB00", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594")
names(intervention_colors) <- c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")

trans_labs <- c("Low Transmisssion", "High Transmission")
names(trans_labs) <- c(0.0175, 0.066)

import_labs <- c("Tier 2", "Tier 3", "Tier 4")
names(import_labs) <- c(0.01, 0.04, 0.1)



# scatter plot of screening costs vs number averted colonized cases (delta effect)
LTACH_IPC70_raw_scatter_screening <- ggplot(LTACH_IPC70_cost_effect_data_df, aes(x = col_cases_averted, y = screen_cost, color = intervention)) +
  geom_hline(yintercept = 0, col = "black", linetype = "dotted") +
  geom_vline(xintercept = 0, col = "black", linetype = "dotted") +
  geom_point(size = 2, alpha = 0.4, shape = 1) +
  geom_point(aes(x = Effect, y = Cost, fill = intervention), data = LTACH_IPC70_screen_results, size = 4, shape = 24, color = "black", show.legend = F) +
  geom_line(aes(x = Effect, y = Cost), data = LTACH_IPC70_non_dominated_screen_results, color = "black") +
  scale_color_manual(values = intervention_colors, aesthetics = c("color", "fill"), labels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                               "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                               "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                               "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")) +
  labs( x = "Number of Colonization Cases Averted (Effect)", y = "Public Health Screening-Cost over 2 years", color = "Screening Interventions", title = "LTACH IPC 70%") +
  scale_y_continuous(labels = scales::comma) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_x", independent = "x") +
  theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black",), 
        axis.title = element_text(size = 12, color = "black",), strip.text = element_text(size = 10, color = "black",)) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) # Override alpha in legend
LTACH_IPC70_raw_scatter_screening
ggsave(here("figures/Cost-effect/LTACH_IPC70_Cost_effect_scatter_screen.png"), plot= LTACH_IPC70_raw_scatter_screening, width = 10, height = 8, unit = "in", dpi = 300)



# scatter plot of total costs vs number averted colonized cases (delta effect)
LTACH_IPC70_raw_scatter_total <- ggplot(LTACH_IPC70_cost_effect_data_df, aes(x = col_cases_averted, y = total_cost, color = intervention)) +
  geom_hline(yintercept = 0, col = "black", linetype = "dotted") +
  geom_vline(xintercept = 0, col = "black", linetype = "dotted") +
  geom_point(size = 2, alpha = 0.4, shape = 1) +
  geom_point(aes(x = Effect, y = Cost, fill = intervention), data = LTACH_IPC70_total_results, size = 4, shape = 24, color = "black", show.legend = F) +
  geom_line(aes(x = Effect, y = Cost), data = LTACH_IPC70_non_dominated_total_results, color = "black") +
  scale_color_manual(values = intervention_colors, aesthetics = c("color", "fill"), labels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                               "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                               "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                               "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")) +
  labs( x = "Number of Colonization Cases Averted (Effect)", y = "Healthcare Facility-Cost over 2 years", color = "Screening Interventions", title = "LTACH IPC 70%") +
  scale_y_continuous(labels = scales::comma) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free", independent = "all") +
  theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black",), 
        axis.title = element_text(size = 12, color = "black",), strip.text = element_text(size = 10, color = "black",)) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) # Override alpha in legend
LTACH_IPC70_raw_scatter_total
ggsave(here("figures/Cost-effect/LTACH_IPC70_Cost_effect_scatter_total.png"), plot= LTACH_IPC70_raw_scatter_total, width = 10, height = 8, unit = "in", dpi = 300)




# Efficiency frontier figures - subset of interventions ------------------------
# Duplicate set of the efficiency frontier figures above with 3 containment scenarios removed.
# The underlying analysis is unchanged - the same ICER results calculated above are simply
# filtered so the dropped interventions are removed from the data, points, and legend.

dropped_interventions <- c("Containment Admissions + PPS (14 days)",
                           "Containment PPS (14 days) w/ taper",
                           "Containment Admissions + PPS (14 days) w/ taper")

LTACH_IPC70_cost_effect_data_sub_df <- LTACH_IPC70_cost_effect_data_df %>%
  filter(!intervention %in% dropped_interventions) %>%
  mutate(intervention = droplevels(intervention))

LTACH_IPC70_total_results_sub <- LTACH_IPC70_total_results %>%
  filter(!intervention %in% dropped_interventions) %>%
  mutate(intervention = droplevels(intervention))

LTACH_IPC70_screen_results_sub <- LTACH_IPC70_screen_results %>%
  filter(!intervention %in% dropped_interventions) %>%
  mutate(intervention = droplevels(intervention))

LTACH_IPC70_non_dominated_total_results_sub <- LTACH_IPC70_non_dominated_total_results %>%
  filter(!intervention %in% dropped_interventions) %>%
  mutate(intervention = droplevels(intervention))

LTACH_IPC70_non_dominated_screen_results_sub <- LTACH_IPC70_non_dominated_screen_results %>%
  filter(!intervention %in% dropped_interventions) %>%
  mutate(intervention = droplevels(intervention))


# scatter plot of screening costs vs number averted colonized cases (delta effect) - subset of interventions
LTACH_IPC70_raw_scatter_screening_sub <- ggplot(LTACH_IPC70_cost_effect_data_sub_df, aes(x = col_cases_averted, y = screen_cost, color = intervention)) +
  geom_hline(yintercept = 0, col = "black", linetype = "dotted") +
  geom_vline(xintercept = 0, col = "black", linetype = "dotted") +
  geom_point(size = 2, alpha = 0.4, shape = 1) +
  geom_point(aes(x = Effect, y = Cost, fill = intervention), data = LTACH_IPC70_screen_results_sub, size = 4, shape = 24, color = "black", show.legend = F) +
  geom_line(aes(x = Effect, y = Cost), data = LTACH_IPC70_non_dominated_screen_results_sub, color = "black") +
  scale_color_manual(values = intervention_colors, aesthetics = c("color", "fill")) +
  labs( x = "Number of Colonization Cases Averted (Effect)", y = "Public Health Screening-Cost over 2 years", color = "Screening Interventions", title = "LTACH IPC 70%") +
  scale_y_continuous(labels = scales::comma) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free_x", independent = "x") +
  theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black",),
        axis.title = element_text(size = 12, color = "black",), strip.text = element_text(size = 10, color = "black",)) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) # Override alpha in legend
LTACH_IPC70_raw_scatter_screening_sub
ggsave(here("figures/Cost-effect/LTACH_IPC70_Cost_effect_scatter_screen_subset.png"), plot= LTACH_IPC70_raw_scatter_screening_sub, width = 10, height = 8, unit = "in", dpi = 300)


# scatter plot of total costs vs number averted colonized cases (delta effect) - subset of interventions
LTACH_IPC70_raw_scatter_total_sub <- ggplot(LTACH_IPC70_cost_effect_data_sub_df, aes(x = col_cases_averted, y = total_cost, color = intervention)) +
  geom_hline(yintercept = 0, col = "black", linetype = "dotted") +
  geom_vline(xintercept = 0, col = "black", linetype = "dotted") +
  geom_point(size = 2, alpha = 0.4, shape = 1) +
  geom_point(aes(x = Effect, y = Cost, fill = intervention), data = LTACH_IPC70_total_results_sub, size = 4, shape = 24, color = "black", show.legend = F) +
  geom_line(aes(x = Effect, y = Cost), data = LTACH_IPC70_non_dominated_total_results_sub, color = "black") +
  scale_color_manual(values = intervention_colors, aesthetics = c("color", "fill")) +
  labs( x = "Number of Colonization Cases Averted (Effect)", y = "Healthcare Facility-Cost over 2 years", color = "Screening Interventions", title = "LTACH IPC 70%") +
  scale_y_continuous(labels = scales::comma) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free", independent = "all") +
  theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black",),
        axis.title = element_text(size = 12, color = "black",), strip.text = element_text(size = 10, color = "black",)) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) # Override alpha in legend
LTACH_IPC70_raw_scatter_total_sub
ggsave(here("figures/Cost-effect/LTACH_IPC70_Cost_effect_scatter_total_subset.png"), plot= LTACH_IPC70_raw_scatter_total_sub, width = 10, height = 8, unit = "in", dpi = 300)




# scatter plot of delta screening costs vs number averted colonized cases (delta effect)
LTACH_IPC70_dc_de_scatter_screen <- ggplot(LTACH_IPC70_cost_effect_data_df, aes(x = col_cases_averted, y = delta_screen_cost, color = intervention)) +
  geom_hline(yintercept = 0, col = "black", linetype = "dotted") +
  geom_vline(xintercept = 0, col = "black", linetype = "dotted") +
  geom_point(size = 2, alpha = 0.4, shape = 1) +
  geom_point(aes(x = Effect, y = Cost, fill = intervention), data = LTACH_IPC70_screen_results, size = 4, shape = 24, color = "black", show.legend = F) +
  scale_color_manual(values = intervention_colors, aesthetics = c("color", "fill"), labels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                               "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                               "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                               "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")) +
  labs( x = "Number of Colonization Cases Averted (delta Effect)", y = "Delta Public Health Screening-Cost (delta Cost)", color = "Screening Interventions", title = "LTACH IPC 70%") +
  scale_y_continuous(labels = scales::comma) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free", independent = "all") +
  theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black",), 
        axis.title = element_text(size = 12, color = "black",), strip.text = element_text(size = 10, color = "black",)) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) # Override alpha in legend
LTACH_IPC70_dc_de_scatter_screen
ggsave(here("figures/Cost-effect/LTACH_IPC70_delta_Cost_effect_scatter_screen.png"), plot= LTACH_IPC70_dc_de_scatter_screen, width = 10, height = 8, unit = "in", dpi = 300)


# scatter plot of delta total costs vs number averted colonized cases (delta effect)
LTACH_IPC70_dc_de_scatter_total <- ggplot(LTACH_IPC70_cost_effect_data_df, aes(x = col_cases_averted, y = delta_total_cost, color = intervention)) +
  geom_hline(yintercept = 0, col = "black", linetype = "dotted") +
  geom_vline(xintercept = 0, col = "black", linetype = "dotted") +
  geom_point(size = 2, alpha = 0.4, shape = 1) +
  geom_point(aes(x = Effect, y = Cost, fill = intervention), data = LTACH_IPC70_total_results, size = 4, shape = 24, color = "black", show.legend = F) +
  scale_color_manual(values = intervention_colors, aesthetics = c("color", "fill"), labels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                               "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                               "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                               "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")) +
  labs( x = "Number of Colonization Cases Averted (delta Effect)", y = "Delta Healthcare Facility-Cost (delta Cost)", color = "Screening Interventions", title = "LTACH IPC 70%") +
  scale_y_continuous(labels = scales::comma) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free", independent = "all") +
  theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black",), 
        axis.title = element_text(size = 12, color = "black",), strip.text = element_text(size = 10, color = "black",)) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) # Override alpha in legend
LTACH_IPC70_dc_de_scatter_total
ggsave(here("figures/Cost-effect/LTACH_IPC70_delta_Cost_effect_scatter_total.png"), plot= LTACH_IPC70_dc_de_scatter_total, width = 10, height = 8, unit = "in", dpi = 300)




# Net benefit analysis figures ----------------------------------------


# Net benefit analysis is run through HPC using the INB-analysis-run.sh script,
# and LTACH_IPC50_INB_analysis.R files because took a long time to run locally


# load the output from the HPC run
LTACH_IPC70_INB_results_df <- read.csv(here("data_output/LTACH_IPC70_model_update/INB_results.csv"))

LTACH_IPC70_INB_results_df <- mutate(LTACH_IPC70_INB_results_df, intervention = factor(intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", 
                                                                                        "Containment Admissions + PPS (14 days) w/ taper", "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", 
                                                                                        "PPS (30 days)", "PPS (14 days)", "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                        "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")))



# filter data to separate the screening vs total cost INB results
screen_LTACH_IPC70_INB_results_df <- filter(LTACH_IPC70_INB_results_df, cost_type == "screen")
total_LTACH_IPC70_INB_results_df <- filter(LTACH_IPC70_INB_results_df, cost_type == "total")


# linetype settings
status <- c("solid", "dashed", "dotted")
names(status) <- c("ND", "ED", "D")



## Net benefit analysis for screening costs ----------------

screen_LTACH_IPC70_INB_results_df <- screen_LTACH_IPC70_INB_results_df %>%
  left_join(., LTACH_IPC70_screen_dominated_status, by = join_by("beta", "import", "intervention")) %>%
  mutate(Status = factor(Status, levels = c("ND", "ED", "D")),
         intervention = factor(intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", 
                                                        "Containment Admissions + PPS (14 days) w/ taper", "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", 
                                                        "PPS (30 days)", "PPS (14 days)", "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                        "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")))



# Cost effectiveness acceptability curve for screening costs
LTACH_IPC70_CEAC_plot_screen <- ggplot(screen_LTACH_IPC70_INB_results_df, aes(x = WTP, y = prob, color = intervention, linetype = Status)) +
  geom_line(linewidth = 1) +
  labs(x = "Willingness-to-pay for an additional \ncolonization case averted ($)", y = "Probability that the intervention is cost-effective", title = "Cost-effectiveness acceptibility curve for public health screening-cost", subtitle = "LTACH IPC 70%", color = "Intervention") +
  scale_color_manual(values = intervention_colors, aesthetics = c("color", "fill"), labels = c("Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                               "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                               "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                               "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")) +
  scale_linetype_manual(values = status, labels = c("Non-dominated", "Weakly Dominated", "Strongly Dominated")) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free", independent = "all") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black"), 
        axis.title = element_text(size = 12, color = "black"), strip.text = element_text(size = 10, color = "black"))
LTACH_IPC70_CEAC_plot_screen
ggsave(here("figures/Cost-effect/LTACH_IPC70_CEAC_plot_screen.png"), plot= LTACH_IPC70_CEAC_plot_screen, width = 12, height = 8, unit = "in", dpi = 300)



# WTP vs INB plot for screening costs
LTACH_IPC70_INB_WTP_plot_screen <- ggplot(screen_LTACH_IPC70_INB_results_df, aes(x = WTP, y = INB_mean, color = intervention)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  #geom_vline(xintercept = 0, linetype = "dotted") +
  geom_line(aes(linetype = Status), linewidth = 1) +
  geom_ribbon(aes(ymin = INB_lower, ymax = INB_upper, fill = intervention), alpha = 0.2, color = NA) +
  scale_color_manual(values = intervention_colors, aesthetics = c("color", "fill"), labels = c("Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                               "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                               "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                               "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")) +
  scale_linetype_manual(values = status, labels = c("Non-dominated", "Weakly Dominated", "Strongly Dominated")) +
  labs(x="Willingness-to-pay for an additional \ncolonization case averted ($)", y = bquote("Incremental Net Benefit (INB) \n(extra benefit - extra cost)"), title = "INB vs WTP plot for public health screening-cost", subtitle = "LTACH IPC 70%", color = "Intervention", fill = "Intervention") +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free", independent = "all") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black"), 
        axis.title = element_text(size = 12, color = "black"), strip.text = element_text(size = 10, color = "black"))
LTACH_IPC70_INB_WTP_plot_screen
ggsave(here("figures/Cost-effect/LTACH_IPC70_INB_WTP_plot_screen.png"), plot= LTACH_IPC70_INB_WTP_plot_screen, width = 12, height = 8, unit = "in", dpi = 300)





## Net benefit analysis for total costs ----------------

total_LTACH_IPC70_INB_results_df <- total_LTACH_IPC70_INB_results_df %>%
  left_join(., LTACH_IPC70_total_dominated_status, by = join_by("beta", "import", "intervention")) %>%
  mutate(Status = factor(Status, levels = c("ND", "ED", "D")),
         intervention = factor(intervention, levels = c("No Screening", "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", 
                                                        "Containment Admissions + PPS (14 days) w/ taper", "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", 
                                                        "PPS (30 days)", "PPS (14 days)", "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                        "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")))



# Cost effectiveness acceptability curve for total costs
LTACH_IPC70_CEAC_plot_total <- ggplot(total_LTACH_IPC70_INB_results_df, aes(x = WTP, y = prob, color = intervention, linetype = Status)) +
  geom_line(linewidth = 1) +
  labs(x = "Willingness-to-pay for an additional \ncolonization case averted ($)", y = "Probability that intervention \nis cost-effective", title = "Cost-effectiveness acceptibility curve for healthcare facility-cost", subtitle = "LTACH IPC 70%", color = "Intervention") +
  scale_color_manual(values = intervention_colors, aesthetics = c("color"), labels = c("Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                       "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                       "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                       "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")) +
  scale_linetype_manual(values = status, labels = c("Non-dominated", "Weakly Dominated", "Strongly Dominated")) +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free", independent = "all") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black"), 
        axis.title = element_text(size = 12, color = "black"), strip.text = element_text(size = 10, color = "black"))
LTACH_IPC70_CEAC_plot_total
ggsave(here("figures/Cost-effect/LTACH_IPC70_CEAC_plot_total.png"), plot= LTACH_IPC70_CEAC_plot_total, width = 12, height = 8, unit = "in", dpi = 300)



# WTP vs INB plot for total costs
LTACH_IPC70_INB_WTP_plot_total <- ggplot(total_LTACH_IPC70_INB_results_df, aes(x = WTP, y = INB_mean, color = intervention)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line(aes(linetype = Status), linewidth = 1) +
  geom_ribbon(aes(ymin = INB_lower, ymax = INB_upper, fill = intervention), alpha = 0.2, color = NA) +
  scale_color_manual(values = intervention_colors, aesthetics = c("color", "fill"), labels = c("Containment PPS (14 days)", "Containment PPS (14 days) w/ taper", "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
                                                                                               "Admissions", "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)", 
                                                                                               "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)", 
                                                                                               "Admissions + PPS (30 days)", "Admissions + PPS (14 days)")) +
  scale_linetype_manual(values = status, labels = c("Non-dominated", "Weakly Dominated", "Strongly Dominated")) +
  labs(x="Willingness-to-pay for an additional \ncolonization case averted ($)", y = bquote("Incremental Net Benefit (INB) \n(extra benefit - extra cost)"), title = "INB vs WTP plot for healthcare facility-cost", subtitle = "LTACH IPC 70%", color = "Intervention", fill = "Intervention") +
  ggh4x::facet_grid2(import ~ beta, labeller = labeller(import = import_labs, beta = trans_labs), scales = "free", independent = "all") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black"), 
        axis.title = element_text(size = 12, color = "black"), strip.text = element_text(size = 10, color = "black"))
LTACH_IPC70_INB_WTP_plot_total
ggsave(here("figures/Cost-effect/LTACH_IPC70_INB_WTP_plot_total.png"), plot= LTACH_IPC70_INB_WTP_plot_total, width = 12, height = 8, unit = "in", dpi = 300)


