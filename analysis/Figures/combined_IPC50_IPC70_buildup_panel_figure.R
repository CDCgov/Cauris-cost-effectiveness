# Combined IPC50 (Panel A) and IPC70 (Panel B) proportion colonized time series figure
# Publication-ready panel figure with shared legend
# Layout: side by side, sized for landscape Word page

library(here)
library(tidyverse)
library(ggplot2)
library(ggh4x)
library(patchwork)


# Shared settings ---------------------------------------------------------------

n_sim <- 1000

intervention_levels <- c(
  "No Screening",
  "Containment PPS (14 days)", "Containment PPS (14 days) w/ taper",
  "Containment Admissions + PPS (14 days)", "Containment Admissions + PPS (14 days) w/ taper",
  "Admissions",
  "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)",
  "Admissions + PPS (180 days)", "Admissions + PPS (90 days)", "Admissions + PPS (60 days)",
  "Admissions + PPS (30 days)", "Admissions + PPS (14 days)"
)

# Wrapped labels for legend readability (insert line breaks for long names)
intervention_labels <- c(
  "No Screening",
  "Containment\nPPS (14 days)", "Containment\nPPS (14 days) w/ taper",
  "Containment Admissions\n+ PPS (14 days)", "Containment Admissions\n+ PPS (14 days) w/ taper",
  "Admissions",
  "PPS (180 days)", "PPS (90 days)", "PPS (60 days)", "PPS (30 days)", "PPS (14 days)",
  "Admissions +\nPPS (180 days)", "Admissions +\nPPS (90 days)", "Admissions +\nPPS (60 days)",
  "Admissions +\nPPS (30 days)", "Admissions +\nPPS (14 days)"
)
names(intervention_labels) <- intervention_levels

intervention_colors <- c(
  "#E31A1C",
  "#FDAE6B", "#FD8D3C", "#F16913", "#D94801",
  "#FFDB00",
  "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32",
  "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"
)
names(intervention_colors) <- intervention_levels

trans_labs <- c("Low Transmission", "High Transmission")
names(trans_labs) <- c(0.0175, 0.066)

import_labs <- c("Tier 2", "Tier 3", "Tier 4")
names(import_labs) <- c(0.01, 0.04, 0.1)


# Load and process data ---------------------------------------------------------

load_avg_state <- function(path) {
  df <- read.csv(here(path))
  df$intervention <- factor(df$intervention, levels = intervention_levels)
  df %>%
    pivot_longer(
      cols = -c(beta, import, intervention, time, week, month, beta_import),
      names_to = c("state", ".value"),
      names_pattern = "(.*)_(.*)"
    )
}

avg_state_long_50 <- load_avg_state("data_output/LTACH_IPC50_model_update/avg_state_data.csv")
avg_state_long_70 <- load_avg_state("data_output/LTACH_IPC70_model_update/avg_state_data.csv")


# Plot function -----------------------------------------------------------------

make_buildup_panel <- function(data, panel_title) {
  data %>%
    filter(state == "prop_colonized") %>%
    ggplot(aes(x = time, color = intervention, fill = intervention)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
    geom_line(aes(y = mean), linewidth = 0.9) +
    ggh4x::facet_grid2(
      import ~ beta,
      labeller = labeller(import = import_labs, beta = trans_labs),
      scales = "free_y",
      independent = "y"
    ) +
    labs(
      x = "Time (days)",
      y = "Proportion of Colonized Individuals",
      title = panel_title
    ) +
    scale_fill_manual(
      name = "Intervention",
      values = intervention_colors,
      labels = intervention_labels,
      aesthetics = c("color", "fill")
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title        = element_text(size = 12, face = "bold"),
      axis.text         = element_text(size = 9),
      axis.title        = element_text(size = 10),
      strip.text        = element_text(size = 9),
      panel.spacing     = unit(0.4, "lines")
    )
}

panel_a <- make_buildup_panel(avg_state_long_50, "A  IPC Efficacy 50%")
panel_b <- make_buildup_panel(avg_state_long_70, "B  IPC Efficacy 70%")


# Combine panels ----------------------------------------------------------------

combined_fig <- (panel_a | panel_b) +
  plot_layout(guides = "collect") &
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(size = 10, face = "bold"),
    legend.text      = element_text(size = 8),
    legend.key.size  = unit(0.6, "lines")
  ) &
  guides(fill = guide_legend(ncol = 4), color = guide_legend(ncol = 4))

combined_fig


# Save as TIF -------------------------------------------------------------------
# Sized for landscape Word page (~9" x 6.5" usable area at 1" margins).
# At 14" x 10", scale to ~64% in Word to fill a landscape page.

ggsave(
  filename = here("figures/combined_IPC50_IPC70_buildup_free_y.tif"),
  plot     = combined_fig,
  width    = 14,
  height   = 10,
  units    = "in",
  dpi      = 300,
  compression = "lzw"
)
