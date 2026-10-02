# Example Simulation

A small, self-contained demonstration of the *C. auris* transmission model that runs on a laptop in **about 3 minutes**.

---

## ⚠️ This example uses simulated data and does not reproduce the published results

**The length-of-stay distribution used here is completely artificial.**

`simulated_LOS_distribution.csv` was generated from a gamma distribution chosen only to be broadly plausible for a long-term acute care hospital (mean ≈ 26 days, median ≈ 20 days). It is **not** the CMS length-of-stay data used in the manuscript, and it was **not** derived from, fitted to, or otherwise based on that data. The real length-of-stay data cannot be redistributed.

Because patient length of stay directly drives how long colonized patients remain in the facility, **every number this example produces will differ from the published results.**

The output of this example:

- **must not** be cited, quoted, or reported as results of the study
- **must not** be compared against the published estimates
- **should not** be interpreted as evidence about the relative effectiveness of any screening strategy

This example exists to show **how the model is structured and run**, not to reproduce the analysis. For the published analysis, see the `analysis/` directory and the [top-level README](../README.md).

---

## What this example does

It runs the individual-level stochastic simulation for five screening strategies in a single scenario, then produces two reduced versions of manuscript-style figures.

| | Full model | This example |
| --- | --- | --- |
| Screening strategies | 16 | 5 |
| Transmission rates | 2 (low and high) | 1 (high, `beta = 0.066`) |
| Admission prevalence | 3 (Tiers 2, 3, 4) | 1 (Tier 3, `iota = 0.04`) |
| Simulations per combination | 1,000 | 200 |
| Length-of-stay data | CMS (not public) | **simulated** |
| Compute | HPC cluster | laptop, ~3 minutes |

The five strategies are:

1. **No Screening** — baseline
2. **Admissions** — screening on admission only
3. **Containment PPS (14 days)** — response-based containment screening triggered by a detected case
4. **PPS (14 days)** — prevention-based point prevalence surveys every 14 days
5. **Admissions + PPS (14 days)** — both combined

All point-prevalence strategies use the same 14-day interval, so the response-based containment strategy and the prevention-based PPS strategies are directly comparable at equal screening frequency.

All other parameters (IPC efficacy 50%, 48-bed capacity, 90% occupancy, 730-day horizon) match the manuscript.

## Running it

From the root of the repository:

```r
source("example_simulation/run_example_simulation.R")
```

Or from a terminal:

```bash
Rscript example_simulation/run_example_simulation.R
```

You will need the packages listed under **Setting up the R environment** in the [top-level README](../README.md). This example deliberately avoids `dampack`, `glmmTMB`, and `estimatr`, so it needs only:

`here`, `data.table`, `tidyverse`, `future`, `purrr`, `furrr`

That also means it sidesteps the macOS OpenMP issue described in the top-level README, which affects `dampack` and `glmmTMB` but not these packages.

The script scales the number of parallel workers to your machine, so no configuration is needed.

## Output

Written to `example_simulation/figures/`:

| File | Contents |
| --- | --- |
| `example_colonization_timeseries.png` | Colonization prevalence over the 2-year simulation, by strategy, with 95% prediction intervals |
| `example_cost_effectiveness_plane.png` | Mean screening cost against mean colonization cases averted, one point per strategy |
| `example_summary_table.csv` | Cumulative cases, cases averted, and mean screening and facility costs per strategy |

Both figures carry a caption marking them as example output.

## Using your own length-of-stay data

To run the example against real data, replace `simulated_LOS_distribution.csv` with a CSV containing the same two columns:

- `LOS` — length of stay in days (integer)
- `percent` — percent of patients with that length of stay (the column is used as sampling weights, so it need not sum to exactly 100)

The script adds the `facility_type`, `location`, and `year` columns that the model's internal filters require. Note that those values are filter keys used by the model code, not a statement about where the data came from.

## Notes on the code

- The script defines `sample_LOS()` directly rather than sourcing `R/sample_LOS_function.R`, because that script reads the CMS length-of-stay files from `data/CMS_LOS_data/`, which are not included in this repository. The sampling logic is identical.
- `initial_ind_state` is defined as a top-level object because the output-processing code reads it from the global environment.
- Increasing `n_sim` gives smoother estimates at a roughly linear cost in runtime: 200 simulations takes about 3 minutes, 500 takes about 8 minutes.
