# cauris-cost-effectiveness-PUBLIC ![Build Status](https://img.shields.io/badge/Pre-Release%20-20B2AA?style=for-the-badge)

NOTE: PRE-RELEASE VERSION

Organization: CDC/NCEZID/DHQP/ERIB   
Contact email: ncezid_shareit@cdc.gov,  yxj6@cdc.gov, leo3@cdc.gov   
Status: Archived    
Languages: R


# This project is under governance review. When the review is complete per local and agency processes, this notice will be removed. This material is draft.

# Modeling the Impact of Screening Approaches on Reducing *Candida* (*Candidozyma*) *auris* Colonization in Long-Term Acute Care Hospitals

**General disclaimer** This repository was created for use by CDC programs to collaborate on public health related projects in support of the [CDC mission](https://www.cdc.gov/about/cdc/#cdc_about_cio_mission-our-mission). GitHub is not hosted by the CDC, but is a third party website used by CDC and its partners to share information and collaborate on software. CDC use of GitHub does not imply an endorsement of any one particular service, product, or enterprise.

## Related documents

* [Open Practices](open_practices.md)
* [Rules of Behavior](rules_of_behavior.md)
* [Disclaimer](DISCLAIMER.md)
* [Contribution Notice](CONTRIBUTING.md)
* [Code of Conduct](code-of-conduct.md)

## Overview

This repository contains the individual-level stochastic simulation model, calibration code, and analysis code supporting the manuscript:

> Fearon Scales M, Wolford H, Octaria R, Slayton R, Whitham H, Hast M, Drexler N, Healy JM, Adil H, Thigpen S, Roome T, Zahn M, Brown J, Baker AD, Jones S, Chow N, Lyman M, Gold JAW. Modeling the Impact of Screening Approaches on Reducing *Candida* (*Candidozyma*) *auris* Colonization in Long-Term Acute Care Hospitals. *Infection Control & Hospital Epidemiology*. Manuscript in revision.

Figures referenced in this repository's scripts are presented in the manuscript; they are not reproduced here.

**Research question:** What is the most effective and cost-effective screening strategy to control *C. auris* transmission within long-term acute care hospitals (LTACHs)?

**Objective:** Simulate how different screening methods and frequencies (e.g., admission screening and point prevalence surveys at different intervals) affect the relative reduction in *C. auris* colonization in an LTACH, and evaluate the cost-effectiveness of each screening strategy across a range of transmission rates and admission prevalence rates.

The model simulates a single average LTACH and tracks how *C. auris* transmission progresses within that facility under different transmission rates, importation rates, and screening intervention strategies. It estimates the costs associated with screening and patient care in order to identify the strategies that provide the greatest reduction in *C. auris* colonization for the most reasonable cost to the facility.

**Code written by:** Michelle Fearon Scales, PhD, MS, and Malavika Rajeev, PhD

**Point of contact:** Jessica M. Healy, PhD, Centers for Disease Control and Prevention

**Corresponding author (manuscript):** Jeremy A. W. Gold, MD, MS (jgold@cdc.gov)

## Model assumptions and inputs

-   Assumes a homogeneous patient population (i.e., all patients have equivalent risk of becoming colonized). Colonized patients are infectious.

-   Transmission risk is not frequency dependent and is a function of the number of colonized patients at a given time.

-   Admission prevalence and transmission rate are constant over time.

-   Colonized patients on contact precautions have transmission reduced by the infection prevention and control (IPC) efficacy parameter (based on Cincotta et al. 2024). We evaluated IPC efficacies of 50% and 70% for LTACHs.

-   Allows for demographic stochasticity in the number of admissions and discharges from the facility, and in which individual patients become colonized.

-   Facility type determines the relative bed number in the simulated facility, and patient length of stay (LOS) is based on LOS data for that facility type. We simulated an LTACH with a maximum capacity of 48 patients and 90% mean occupancy.

-   Transmission rate (per day): a low and a high rate, calibrated to real facility screening data (see below).

-   Admission prevalence of colonized patients to the facility: 1% (low, equivalent to Tier 2), 4% (intermediate, Tier 3), or 10% (high, Tier 4) of admissions are colonized. These rates were based on admission prevalence for *C. auris* in California LTACHs that fell into each of these tier statuses. Orange County LTACHs, an endemic Tier 4 region in California, have about 10% admission prevalence; Chicago LTACHs, another historically endemic area, have 25–27% admission prevalence.

-   Each simulation runs for 730 days (2 years) and begins with one colonized patient and 47 susceptible patients. Each combination of model conditions was run 1,000 times.

## Screening interventions

We modeled our screening interventions after the CDC multidrug-resistant organism (MDRO) prevention and response-based screening guidelines for Tiers 2, 3, and 4, and estimated the admission prevalence for each tier based on colonized-on-admission data from California LTACHs in each tier status.

All tiers were evaluated for prevention-based screening interventions, including admission screening only, point prevalence survey (PPS) screening only at intervals of every 14, 30, 60, 90, or 180 days, and combined admission and PPS screening at each interval. All interventions were compared to a scenario in which no screening was conducted.

Additionally, response-based containment screening was evaluated for Tiers 2 and 3 based on CDC guidance; no containment screening is recommended for Tier 4. Containment PPS, or combined containment PPS and admission screening, conditionally turn on when triggered by a potential outbreak and turn off after 2 PPSs without any new cases. We also evaluated scenarios in which PPSs taper off with increasing intervals between surveys after 2 PPSs without new cases, reflecting policies in some jurisdictions.

## Calibration of the transmission parameter

We calibrated the transmission rate, beta, to real screening data from LTACHs using approximate Bayesian computation with sequential Monte Carlo (ABC-SMC). In brief, beta is randomly sampled from a uniform prior, and values that produce simulated data matching the real data within a given epsilon threshold are retained. This process repeats over multiple generations, resampling the retained beta values from the previous generation and lowering the epsilon threshold in each generation.

We matched our simulation to admission and PPS screening data from four LTACHs (referred to as LTACH A–D), calculating the percent positivity on each PPS as a time series and the cumulative proportion of screened colonized patients relative to total screening tests. We used a mean root squared error distance metric and reduced the epsilon threshold using the 25th or 30th quantile of the distances from the previous generation. Each generation ran until 1,000 beta values were accepted, with 1 simulation per sampled beta value.

Simulation parameters were matched as closely as possible to each real facility. Calibration was split over two time periods reflecting differences in screening practices: period 1 had PPS only, and period 2 had admission screening plus PPS. We otherwise matched key facility characteristics such as admission prevalence, bed capacity, screening frequency, and mean occupancy, and used LOS distributions from each facility's state for 2019 (period 1) and 2021 (period 2).

### Calibration details

**Transmission prior:** Uniform (0.01, 0.07)

**Test statistics (data to match):**
-   Percent positivity on PPS, as a time series
-   Cumulative proportion of screened colonized patients to total tests (single value)

**Distance measure:** Mean root squared error

**Epsilon thresholds:** Determined by the 25th quantile (LTACH A, B, C) or 30th quantile (LTACH D) of the distances for each test statistic from the previous generation

**Stopping rule:** Simulate until N = 1,000 parameter sets are accepted, with 1 simulation per particle

**Data sources for screening:**
-   Admission and PPS screening data from three LTACHs in Orange County, California (2019–2023) — LTACH A, B, and C
-   Admission and PPS screening data from one LTACH in Chicago, Illinois (2017–2024) — LTACH D
-   For each LTACH, time periods were split based on screening practices (period 1: PPS only; period 2: admission + PPS)
-   Limited admission prevalence data were available for the Chicago facility; we assumed 15% admission prevalence for LTACH D period 1, while LTACH D period 2 had 27% admission prevalence

### Lowest and highest median transmission rates selected

| Facility type | IPC efficacy | Low beta | High beta |
| ------ | ------ | ------ | ------ |
| LTACH | 50% | 0.0175 | 0.066 |
| LTACH | 70% | 0.0175 | 0.066 |

**Note:** We used the same low and high transmission rates regardless of IPC efficacy level to make it easier to compare outcomes across screening scenarios.

## Repository structure

### `R/`
Core functions for running the simulation model, calibrating the transmission rate, and processing model output. All functions include roxygen documentation. See [R/README.md](R/README.md) for a description of each script.

-   `individual.R` — the individual-level simulation function (`sim_fac_individuals`) and wrappers for running it singly, rowwise, or in batches
-   `sample_LOS_function.R` — samples patient length of stay from facility-type-specific LOS distributions
-   `output_processing_functions.R` — post-processing of raw model output
-   `calibration_functions_hpc.R` — the nested ABC-SMC calibration functions
-   `calibration_helper_functions.R` — parameter assembly, distance metrics, and ABC-SMC kernel helpers
-   `INB_calculation_function.R` — incremental net benefit calculation
-   `batch_read_function.R` — batch reading and combining of csv output files

### `analysis/`
Scripts that run the model scenarios and process the model output. See [analysis/README.md](analysis/README.md).

-   `model_runs/` — scripts that run the full set of model scenarios for each IPC efficacy level (`LTACH_IPC50_model_run_hpc.R`, `LTACH_IPC70_model_run_hpc.R`)
-   `CE_analysis/` — cost-effectiveness and incremental net benefit analyses and figures
-   `Figures/` — scripts to create the manuscript figures (excluding cost-effectiveness figures)
-   `LTACH_IPC50_70_outcome_analysis.R` — statistical analyses of cumulative colonization cases by intervention, IPC efficacy level, and their interaction

### `calibration/`
-   `run_calibration_hpc.R` — runs the ABC-SMC calibration for a single facility, period, and IPC efficacy combination
-   `calibration_figures.R` — posterior distribution figures and summaries for the calibrated transmission rate

## Data availability

**This repository contains code only.** The `data/`, `data_output/`, and `figures/` directories referenced by the scripts are not included.

The scripts expect the following directories, which are supplied by the user or generated when the code is run:

-   `data/` — model and calibration inputs, including CMS length-of-stay distributions and the facility screening data used for calibration. The facility screening data are not publicly available; they were provided by participating health departments under data use agreements and contain facility-identifiable information.
-   `data_output/` — model output written by the model run scripts and read by the analysis scripts
-   `figures/` and `calibration/results_figs/` — figure output directories

Because the input data are not included, the scripts in this repository are provided for methodological transparency and reuse rather than as a directly reproducible pipeline. The simulation model itself (`R/individual.R`) requires only a length-of-stay distribution and a parameter data frame, and can be run independently of the facility screening data.

## Model outputs

### Raw model outputs

-   **Individual patient tracking** — admission and discharge dates; screening date, screening type (admission vs. PPS), and result; whether an individual became colonized through transmission or entered the facility colonized but undetected; when an individual was identified as positive; and whether that individual went on to develop a clinical or invasive infection.
-   **Health outcomes** — the number of colonized patients in the facility and the cumulative number of colonized patients (including those discharged) at each time point; the same for invasive infections.
-   **Screening and facility costs** — screening costs (materials and personnel, per test); clinical test costs (materials and personnel, per test); and costs associated with additional IPC/precautions for colonized or infected patients (per patient per day).

### Post-processed model outputs

-   **Average state data** (`avg_state_data`) — mean and 95% prediction intervals of total susceptible, colonized, and infected patients at each time point from day 0 to 730, for each parameter combination. Used for prevalence time series figures.
-   **Cost-effect data** — per-simulation cumulative totals of colonization, clinical, and invasive infection cases, screening and total costs, total swabs used, and related measures. Cases averted are calculated by differencing each intervention against its paired 'no screening' simulation using matched seeds; relative risk reduction is the cases averted divided by the cases in the paired 'no screening' simulation.
-   **Cost-effect average data** — mean, median, standard error, and 95% prediction interval across the 1,000 simulations for each parameter combination, plus the incremental cost-effectiveness ratio (ICER).
-   **Last 6 months prevalence and incidence data** (`last_6mo_prev_inc_data`) — mean and 95% prediction interval of facility prevalence and weekly and monthly incidence over the final 6 months of the simulation. Averaging over the tail of the time series is necessary because these values fluctuate, particularly under PPS screening scenarios.
-   **Parameter data frame** (`param_df_sim_data`) — unique ID, seed, and all parameter values for each simulation run, used to join output back to the parameters that generated it.
-   **Percent positive time series and summary** — per-simulation percent positive on each PPS, and the mean and 95% prediction interval for each parameter combination. Used for comparison against real facility screening data and in the calibration setup.

## Running the model

All analyses were done in R 4.4.0. The following packages are required:

`here`, `tidyverse`, `data.table`, `purrr`, `furrr`, `future`, `tmvtnorm`, `WeightIt`, `ggplot2`, `RColorBrewer`, `ggh4x`, `ggpattern`, `ggridges`, `patchwork`, `scales`, `zoo`, `broom`, `car`, `emmeans`, `estimatr`, `glmmTMB`, and `dampack`.

The model run and calibration scripts were written to run on a high-performance computing (HPC) cluster, using array jobs to distribute parameter combinations or facility/period/IPC-efficacy combinations across nodes, and `future`/`furrr` to parallelize simulations within a job. Each script has an `hpc` flag near the top that controls how the parallel backend is configured and whether a project-level `.Rprofile` is sourced; set `hpc <- FALSE` to run locally with a smaller number of workers.

Suggested order of operations:

1.  `calibration/run_calibration_hpc.R` — calibrate the transmission rate for each facility, period, and IPC efficacy combination
2.  `calibration/calibration_figures.R` — summarize the posterior distributions and select the low and high transmission rates
3.  `analysis/model_runs/LTACH_IPC50_model_run_hpc.R` and `analysis/model_runs/LTACH_IPC70_model_run_hpc.R` — run the full set of screening scenarios
4.  `analysis/LTACH_IPC50_70_outcome_analysis.R`, `analysis/CE_analysis/`, and `analysis/Figures/` — statistical analyses, cost-effectiveness analyses, and manuscript figures

## Acknowledgements

We thank Malavika Rajeev, Carlo Devila Payan, Jeffrey Hoch, Tisha Mitsunaga, and Sam Horwich-Scholefield for their invaluable feedback on the development of this model and the cost-effectiveness analyses.

## Funding

This work was supported in part by Epidemiology and Laboratory Capacity grant funding for California Department of Public Health staff.

## Disclaimer

The findings and conclusions in this repository are those of the authors and do not necessarily represent the official position of the Centers for Disease Control and Prevention.

## Public Domain Standard Notice

This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the
[CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

## License Standard Notice

The repository utilizes code licensed under the terms of the Apache Software
License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify
it under the terms of the Apache Software License version 2, or (at your option)
any later version.

This source code in this repository is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Apache Software
License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

The source code forked from other open source projects will inherit its license.

## Privacy Standard Notice

This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](DISCLAIMER.md) and [Code of Conduct](code-of-conduct.md).
For more information about CDC's privacy policy, please visit
[http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

## Contributing Standard Notice

Anyone is encouraged to contribute to the repository by
[forking](https://help.github.com/articles/fork-a-repo) and submitting a pull
request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including
but not limited to the Federal Records Act, and may be archived. Learn more at
[http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

## Records Management Standard Notice

This repository is not a source of government records, but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

## Additional Standard Notices

Please refer to [CDC's Template Repository](https://github.com/CDCgov/template) for
more information about [contributing to this repository](https://github.com/CDCgov/template/blob/main/CONTRIBUTING.md),
[public domain notices and disclaimers](https://github.com/CDCgov/template/blob/main/DISCLAIMER.md),
and [code of conduct](https://github.com/CDCgov/template/blob/main/code-of-conduct.md).
