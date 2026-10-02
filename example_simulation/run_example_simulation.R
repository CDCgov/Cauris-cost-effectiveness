# ==============================================================================
# EXAMPLE SIMULATION - runs locally on a laptop in roughly 3 minutes
# ==============================================================================
#
# ******************************************************************************
# *  IMPORTANT: THIS EXAMPLE USES COMPLETELY SIMULATED LENGTH-OF-STAY DATA.    *
# *                                                                            *
# *  The length-of-stay distribution in simulated_LOS_distribution.csv is      *
# *  ARTIFICIAL. It was generated from a gamma distribution chosen only to be  *
# *  broadly plausible for a long-term acute care hospital. It is NOT the CMS  *
# *  length-of-stay data used in the manuscript, and it was not derived from   *
# *  or fitted to that data.                                                   *
# *                                                                            *
# *  THE RESULTS THIS SCRIPT PRODUCES WILL DIFFER FROM THE PUBLISHED RESULTS   *
# *  AND SHOULD NOT BE CITED, COMPARED AGAINST, OR INTERPRETED AS THE          *
# *  MANUSCRIPT'S FINDINGS. This example exists to demonstrate how the model   *
# *  is set up and run, not to reproduce the published analysis.               *
# ******************************************************************************
#
# This example is a deliberately reduced version of the full analysis:
#
#                        Full model              This example
#   Interventions        16                      5
#   Transmission rates   2 (low + high)          1 (high, beta = 0.066)
#   Admission prevalence 3 (Tiers 2, 3, 4)       1 (Tier 3, iota = 0.04)
#   Simulations          1,000 per combination   200 per combination
#   Compute              HPC cluster             laptop, ~3 minutes
#
# The full model run scripts are in analysis/model_runs/ and require an HPC
# cluster plus the non-public input data described in the top-level README.
# ==============================================================================


# Libraries --------------------------------------------------------------------
# Note: this example deliberately avoids dampack, glmmTMB, and estimatr so that
# it runs with a minimal set of packages. See the top-level README if you hit
# an OpenMP error loading those packages on macOS.
library(here)
library(data.table)
library(tidyverse)
library(future)
library(purrr)
library(furrr)


# Model functions --------------------------------------------------------------
# We source the simulation and output-processing functions directly. We do NOT
# source R/sample_LOS_function.R, because that script reads the CMS length-of-
# stay files from data/CMS_LOS_data/, which are not included in this repository.
# Instead we define sample_LOS() here with identical behaviour and feed it the
# simulated distribution below.
source(here("R/output_processing_functions.R"))
source(here("R/individual.R"))

#' Sample lengths of stay (identical logic to R/sample_LOS_function.R)
sample_LOS <- function(n = 1, data, fac_type) {
  data <- dplyr::filter(data, facility_type == fac_type)
  new_los <- sample(x = data$LOS, size = n, prob = data$percent, replace = TRUE)
  new_los[new_los == 0] <- 1
  new_los
}


# Simulated length-of-stay data ------------------------------------------------
# See the warning at the top of this script: this distribution is artificial.
LOS_sim <- read.csv(here("example_simulation/simulated_LOS_distribution.csv"))

# The simulation functions filter their LOS input on facility_type, location and
# year. These three columns are internal filter keys only - they are required for
# the model code to accept the data frame. The "CA" and 2019 values below are NOT
# a claim about the provenance of this simulated distribution.
LOS_sim <- LOS_sim %>%
  mutate(facility_type = "LTACH",
         location      = "CA",
         year          = 2019)

cat(sprintf("Simulated LOS distribution: mean %.1f days, median %.0f days\n",
            sum(LOS_sim$LOS * LOS_sim$percent) / 100,
            LOS_sim$LOS[which.min(abs(cumsum(LOS_sim$percent) - 50))]))


# Parameter combinations -------------------------------------------------------
# Five screening strategies at a single transmission rate and admission
# prevalence. Parameter meanings are documented in R/individual.R.

beta_example <- 0.066   # high transmission rate
iota_example <- 0.04    # admission prevalence, Tier 3

make_params <- function(p_a, p_pps, t_pps, contain_pps) {
  tidyr::expand_grid(
    beta               = beta_example,
    iota               = iota_example,
    p_a                = p_a,           # 1 = screen on admission
    p_pps              = p_pps,         # 1 = point prevalence surveys on
    p_c                = 0.90,          # test sensitivity
    contain_pps        = contain_pps,   # 1 = response-based containment PPS
    contain_a          = 0,
    contain_taper      = 0,
    ipc_eff            = 0.50,          # IPC efficacy (50%)
    t_pps              = t_pps,         # days between PPSs
    start_pps          = 30,
    dt                 = 1,
    clinical_case_prob = 0.0688,
    inf_prob           = 0.401,
    avg_extra_LOS      = 5.6,
    fac_max_cap        = 48,            # LTACH bed capacity
    occupancy          = 0.90,
    fac_type           = "LTACH"
  )
}

# All PPS-based strategies use a 14-day interval so that the response-based
# containment strategy and the prevention-based PPS strategies are directly
# comparable at the same screening frequency.
params_df <- rbind(
  make_params(p_a = 0, p_pps = 0, t_pps = 14, contain_pps = 0),  # No Screening
  make_params(p_a = 1, p_pps = 0, t_pps = 14, contain_pps = 0),  # Admissions
  make_params(p_a = 0, p_pps = 0, t_pps = 14, contain_pps = 1),  # Containment PPS (14 days)
  make_params(p_a = 0, p_pps = 1, t_pps = 14, contain_pps = 0),  # PPS (14 days)
  make_params(p_a = 1, p_pps = 1, t_pps = 14, contain_pps = 0)   # Admissions + PPS (14 days)
)

cat("Parameter combinations to run:", nrow(params_df), "\n")


# Run the model ----------------------------------------------------------------

# Initial facility state. NOTE: the output-processing code reads this from the
# global environment, so it must be defined here as a top-level object.
initial_ind_state <- c(S = 47, C = 1, C_s = 0, I = 0)

tmax  <- 730   # 2 years
n_sim <- 200   # simulations per combination (the full model uses 1,000)

# Scale workers to the machine. There are only a handful of parameter
# combinations, so there is nothing to gain from more workers than that.
plan(multisession, workers = max(1, min(nrow(params_df),
                                        future::availableCores(logical = FALSE) - 1)))
cat("Running with", nbrOfWorkers(), "parallel workers...\n")

run_time <- system.time({
  example_res <- individual_fun_batch(params_df         = params_df,
                                      data              = LOS_sim,
                                      initial_ind_state = initial_ind_state,
                                      tmax              = tmax,
                                      n_sim             = n_sim,
                                      n_params_per_split = 1,
                                      run_type          = "Paired")
})

plan(sequential)
cat(sprintf("\nModel run completed in %.1f minutes\n", run_time[["elapsed"]] / 60))


# Figures ----------------------------------------------------------------------
# Two reduced versions of manuscript figures. Colours match the full analysis.

dir.create(here("example_simulation/figures"), showWarnings = FALSE, recursive = TRUE)

intervention_levels <- c("No Screening", "Containment PPS (14 days)", "Admissions",
                         "PPS (14 days)", "Admissions + PPS (14 days)")
intervention_colors <- c("No Screening"               = "#E31A1C",
                         "Containment PPS (14 days)"  = "#FDAE6B",
                         "Admissions"                 = "#FFDB00",
                         "PPS (14 days)"              = "#005A32",
                         "Admissions + PPS (14 days)" = "#084594")

sim_caption <- paste("EXAMPLE ONLY - run with simulated length-of-stay data.",
                     "Does not reproduce the published results.")


## Figure 1: colonized patients over time --------------------------------------
avg_state_df <- example_res$avg_state_res %>%
  mutate(intervention = factor(intervention, levels = intervention_levels))

# NOTE: C_mean is a prevalence (the output processing divides the colonized count
# by the facility population N), so this is a proportion, plotted here as a percent.
fig1 <- ggplot(avg_state_df, aes(x = time, y = C_mean,
                                 color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = C_lower, ymax = C_upper), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = intervention_colors, aesthetics = c("color", "fill")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Day of simulation",
       y = "Colonization prevalence\n(% of patients in the facility)",
       color = "Screening intervention", fill = "Screening intervention",
       title = "EXAMPLE: colonization prevalence over 2 years",
       subtitle = "LTACH, IPC 50%, high transmission, Tier 3 admission prevalence",
       caption = sim_caption) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, face = "italic", color = "grey30"),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         fill  = guide_legend(nrow = 2, byrow = TRUE))

ggsave(here("example_simulation/figures/example_colonization_timeseries.png"),
       plot = fig1, width = 9, height = 6, units = "in", dpi = 300)


## Figure 2: cost-effectiveness plane ------------------------------------------
# Mean screening cost against mean colonization cases averted, one point per
# strategy. The full analysis additionally computes ICERs and the efficiency
# frontier using the dampack package (see analysis/CE_analysis/).
cost_effect_avg_df <- example_res$cost_effect_avg_res %>%
  mutate(intervention = factor(intervention, levels = intervention_levels))

fig2 <- ggplot(cost_effect_avg_df,
               aes(x = col_cases_averted_mean, y = screen_cost_mean)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(aes(fill = intervention), size = 5, shape = 24, color = "black") +
  scale_fill_manual(values = intervention_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Colonization cases averted over 2 years (effect)",
       y = "Public health screening-cost over 2 years",
       fill = "Screening intervention",
       title = "EXAMPLE: cost-effectiveness plane",
       subtitle = "LTACH, IPC 50%, high transmission, Tier 3 admission prevalence",
       caption = sim_caption) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, face = "italic", color = "grey30"),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(here("example_simulation/figures/example_cost_effectiveness_plane.png"),
       plot = fig2, width = 9, height = 6, units = "in", dpi = 300)


# Summary ----------------------------------------------------------------------
summary_tbl <- cost_effect_avg_df %>%
  select(intervention, C_tot_mean, col_cases_averted_mean,
         screen_cost_mean, total_cost_mean) %>%
  arrange(intervention)

cat("\n--- EXAMPLE RESULTS (simulated LOS data - not the published results) ---\n")
print(as.data.frame(summary_tbl), row.names = FALSE)

write.csv(summary_tbl,
          here("example_simulation/figures/example_summary_table.csv"),
          row.names = FALSE)

cat("\nFigures written to example_simulation/figures/\n")
