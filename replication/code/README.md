# Code guide

Run scripts from the repository root and follow the order in the main README.
Settings and input paths appear near the top of each script. Economically
meaningful transformations are kept in the analysis scripts so they can be
followed sequentially.

## Data construction

| File | Description |
|---|---|
| `01_data/01_build_estimation_datasets.R` | Reads the survey and coral-cover inputs, constructs the complete and aggregated choice sets, creates adjusted and unadjusted wide datasets, and writes metadata checks. |
| `01_data/02_build_welfare_dataset.R` | Combines population-grid, route, site, and travel-cost information to construct the population-by-site dataset used for welfare aggregation. |

## Model estimation

| File | Description |
|---|---|
| `02_estimation/01_estimate_covariate_full.R` | Estimates the conditional-logit specification using the complete choice set. |
| `02_estimation/02_estimate_covariate_aggregated.R` | Estimates the corresponding conditional-logit specification after aggregating non-coastal alternatives. |
| `02_estimation/03_estimate_FE_full.R` | Estimates the first-stage site fixed effects for the complete choice set. |
| `02_estimation/04_estimate_FE_aggregated.R` | Estimates the first-stage site fixed effects for the aggregated choice set. |
| `02_estimation/05_estimate_second_stage_models.R` | Runs the complete and aggregated second-stage regressions and applies the bootstrap correction to their covariance matrices. |
| `02_estimation/06_compile_estimation_results.R` | Collects the model coefficients and corrected standard errors used in Table 1. |

## Bootstrap

| File | Description |
|---|---|
| `03_bootstrap/01_bootstrap_2stage_murdock.R` | Performs the respondent-level outer bootstrap and site-level inner simulation used to account for uncertainty in the generated first-stage fixed effects. |
| `03_bootstrap/02_single_replica_2stage_murdock.R` | Optional diagnostic that runs one bootstrap replication before the full calculation. |

## Welfare analysis

| File | Description |
|---|---|
| `04_welfare/01_welfare_calculations.R` | Records the welfare formulas and prepares the required model and population-by-site inputs. |
| `04_welfare/02_estimate_welfare.R` | Calculates the eight welfare cases and their simulation-based uncertainty intervals. |
| `04_welfare/03_compile_welfare_table.R` | Converts the welfare results to annual values and compiles Table 2. |

## Empirical figures and supporting tables

| File | Description |
|---|---|
| `05_figures_tables/01_figure3_recreation_sites_map.R` | Creates Figure 3, the map of coastal and non-coastal recreation sites. |
| `05_figures_tables/02_figure4_bwe_frequency_map.R` | Creates Figure 4, the spatial distribution of brown water event frequency. |
| `05_figures_tables/03_figure5_welfare_loss.R` | Creates Figure 5 and its supporting plotting files from the welfare simulation results. |
| `05_figures_tables/05_site_visitation_breakdown.R` | Calculates site visits and site shares by recreation category for Appendix Table C.1. |

## Monte Carlo analysis

| File | Description |
|---|---|
| `06_monte_carlo/01_generate_synthetic_choice_data.R` | Defines the data-generating process for the site-aggregation simulation. |
| `06_monte_carlo/02_run_monte_carlo_aggregation.R` | Runs 1,000 replications for each of the 27 simulation scenarios and stores the coefficient and bias results. |
| `06_monte_carlo/03_plot_monte_carlo_results.R` | Summarizes the simulation results and creates Figure 1. |
