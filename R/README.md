# R Folder

The files in this folder are the key functions for running the simulation model, calibrating the transmission rate, and processing model output. Each R script contains full roxygen documentation for its functions.

## individual.R
This script has the individual simulation function (`sim_fac_individuals`) that runs a single run of the simulation. It also includes several wrapper functions to run `sim_fac_individuals` singly (`individual_fun_single`), rowwise over a set of parameters (`individual_fun_rowwise`), or in batches by feeding in a data frame of parameter combinations to test over (`individual_fun_batch`). We primarily used `individual_fun_batch` to run the model via HPC using parallelization to reduce the model runtime.

## sample_LOS_function.R
`sample_LOS` samples from the length-of-stay (LOS) distributions in a provided dataset to determine how long each patient in the simulation should stay in the facility.

## output_processing_functions.R
These functions are used within `individual_fun_batch` to process the model output into a form that is more readily useful for analyses. They include `param_intervention_labels` (adds intervention labels to the parameter data frame), `state_df_processing` and `state_avg_per_param_combo` (patient state time series and their averages), `cost_effect_processing` and `cost_effect_avg` (cumulative case, cost, and cases-averted calculations and their summaries), `last_6mo_calc` (prevalence and incidence over the final 6 months), `percent_positive_calc` (percent positive on each PPS), and `standard_error`.

## calibration_functions_hpc.R
This script has three nested functions. The innermost function `individual_fun_calibrate_hpc` is a wrapper that runs the simulation model and produces output matching the type of data we have from real facilities. The next function up, `run_abc_hpc`, runs the approximate Bayesian computation (ABC) process, where we repeatedly test new particles and match them to the real facility data to determine whether a particle should be kept or thrown out. The highest-level function `run_calibration_hpc` runs the sequential Monte Carlo process, calling the ABC process with lower and lower epsilon thresholds (the allowed difference between the simulation and the real facility data).

## calibration_helper_functions.R
Additional functions that carry out specific tasks within the larger calibration functions. `get_params`, `get_match_data`, and `get_LOS` assemble the simulation parameters, the facility data to match against, and the LOS distribution for a given facility and period. `param_intervention_labels_calibrate` and `percent_positive_calibrate` format the simulated output for comparison to the real data, `calc_distance_RMSE` computes the mean root squared error distance metric, and the remaining helpers (`rK`, `H`, `prior.non.zero`, `Norm.Eucl.dist`, `getSigmaNeighbours`) support the perturbation kernel and weighting steps of the ABC-SMC algorithm.

## INB_calculation_function.R
`calc_INB` calculates the incremental net benefit for each set of parameter combinations over all the relevant willingness-to-pay values.

## batch_read_function.R
Functions to read in a batch of csv files from a folder and combine and manipulate them: `read_batch_csv_fun` for general csv batches, and `read_batch_cal_res_fun` and `read_batch_cal_abc_stats_fun` for the calibration results and ABC summary statistics files.
