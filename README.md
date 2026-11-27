# Replication files for site aggregation in recreational demand modelling

This repository contains the code, documentation, and archived reference
outputs for:

*Site aggregation in recreational demand modelling with an application to the
impact of sediments runoffs in coastal waters*

The analysis compares recreation-demand estimates obtained from the complete
set of Maui recreation sites with estimates obtained after combining all
non-coastal sites into one Other Outdoor Recreation (OOR) alternative.

- Code repository: [GitHub](https://github.com/cevefabio/Site_aggregation_effect_ERE/tree/main)
- Data archive: [Zenodo](https://doi.org/10.5281/zenodo.23811094)

## Repository contents

- `code/01_data/` constructs the estimation and welfare datasets.
- `code/02_estimation/` estimates the conditional-logit and two-stage models.
- `code/03_bootstrap/` corrects two-stage uncertainty for the generated
  first-stage fixed effects.
- `code/04_welfare/` calculates welfare losses from brown water events.
- `code/05_figures_tables/` creates the empirical tables and figures.
- `code/06_monte_carlo/` runs and plots the site-aggregation simulation.
- `logos/` contains the two map symbols used in Figure 3.
- `data/` contains documentation and an inventory for the separately archived
  data.
- `output/reference_outputs/` contains fixed copies of the figures, tables,
  and appendix corresponding to the submitted paper.

A description of every code file is provided in
[`code/README.md`](replication/code/README.md). Data sources and individual data files are
described in [`data/README.md`](replication/data/README.md) and
[`data/DATA_INVENTORY.csv`](replication/data/DATA_INVENTORY.csv).

## Software requirements

The final project files were produced with:

- R 4.4.2;
- Apollo 0.3.7;
- `tidyverse` 2.0.0, including the directly loaded `dplyr` and `ggplot2`
  packages;
- `openxlsx` 4.2.7.1;
- `MASS` 7.3-61;
- `sf` 1.0-21;
- `ggspatial` 1.1.9;
- `terra` 1.9-11;
- `ggimage` 0.3.3;
- `png` 0.1-8;
- the `grid` and `parallel` packages supplied with R.

The scripts do not install packages automatically. Install any missing
packages before beginning the replication.

## Obtain and place the data

Code and data are distributed separately. Download
`hawaii_bw_replication_data.zip` from the
[Zenodo record](https://doi.org/10.5281/zenodo.23811094) and extract it into
the `replication/` folder. The archive already contains `data/`, which should
sit alongside `code/` inside `replication/`. Paths below are relative to
`replication/`.

```text
code/01_data/01_build_estimation_datasets.R
data/raw/for_estimation_withbio.dat
data/external/sites.xlsx
data/external/BWevents.xlsx
```

Do not rename the folders or move individual data files. The scripts use
project-relative paths, so no working-directory lines need to be edited. Start
R in `replication/` before running a script.

The complete data structure and citations are in
[`data/README.md`](replication/data/README.md). File sizes and SHA-256 checksums are in
[`data/DATA_INVENTORY.csv`](replication/data/DATA_INVENTORY.csv).

## Step-by-step replication

### Empirical analysis

Run the following scripts in order from `replication/`:

1. `code/01_data/01_build_estimation_datasets.R`
2. `code/01_data/02_build_welfare_dataset.R`
3. `code/02_estimation/01_estimate_covariate_full.R`
4. `code/02_estimation/02_estimate_covariate_aggregated.R`
5. `code/02_estimation/03_estimate_FE_full.R`
6. `code/02_estimation/04_estimate_FE_aggregated.R`
7. `code/03_bootstrap/01_bootstrap_2stage_murdock.R`
8. `code/02_estimation/05_estimate_second_stage_models.R`
9. `code/02_estimation/06_compile_estimation_results.R`
10. Inspect `code/04_welfare/01_welfare_calculations.R`, which records the
    welfare formulas and data preparation, and then run
    `code/04_welfare/02_estimate_welfare.R`.
11. `code/04_welfare/03_compile_welfare_table.R`
12. Run the scripts in `code/05_figures_tables/` needed for Figures 3--5 and
    Appendix Table C.1, as listed in the output table below.

The bootstrap uses 500 respondent-level outer draws and 1,000 site-level inner
draw attempts for each outer draw. It is the most computationally intensive
part of the workflow; the final run took approximately four hours using 12
cores. It writes partial diagnostics and final covariance estimates to
`output/bootstrap/two_stage_murdock/`.

`code/03_bootstrap/02_single_replica_2stage_murdock.R` is optional. It runs one
replication and can be used to check the computing environment before the full
bootstrap.

### Site-aggregation Monte Carlo

The theoretical simulation is independent of the empirical-data workflow:

1. Inspect `code/06_monte_carlo/01_generate_synthetic_choice_data.R` for the
   data-generating process.
2. Run `code/06_monte_carlo/02_run_monte_carlo_aggregation.R` to estimate the
   27 scenarios with 1,000 replications per scenario.
3. Run `code/06_monte_carlo/03_plot_monte_carlo_results.R` to create Figure 1.

Random-number seeds are set in the Monte Carlo, bootstrap, and welfare
uncertainty calculations. Source data are not overwritten; constructed data
are written to `data/processed/`.

## Outputs corresponding to the paper

| Paper item | Output produced by the replication | Producing code | Archived reference output |
|---|---|---|---|
| Figure 1 | `output/models/monte_carlo_outputs_aggregated_only/plots/figure1_average_bias_heatmap.png` | `code/06_monte_carlo/03_plot_monte_carlo_results.R` | `output/reference_outputs/figure1_average_bias_heatmap.png` |
| Figure 2 | Not code-generated; photograph credited to Don Mcleish | Not applicable | `output/reference_outputs/figure2_brown_water_event.jpeg` |
| Figure 3 | `output/figures/figure3_recreation_sites_map.png` | `code/05_figures_tables/01_figure3_recreation_sites_map.R` | `output/reference_outputs/figure3_recreation_sites_map.png` |
| Figure 4 | `output/figures/figure4_bwe_frequency_map.png` | `code/05_figures_tables/02_figure4_bwe_frequency_map.R` | `output/reference_outputs/figure4_bwe_frequency_map.png` |
| Figure 5 | `output/figures/figure5_welfare_simulations.jpeg` | `code/05_figures_tables/03_figure5_welfare_loss.R` | `output/reference_outputs/figure5_welfare_simulations.jpeg` |
| Table 1 | `output/tables/paper_model_estimates.xlsx` | `code/02_estimation/06_compile_estimation_results.R` | `output/reference_outputs/paper_model_estimates.xlsx` |
| Table 2 | `output/tables/paper_welfare_estimates.xlsx` | `code/04_welfare/03_compile_welfare_table.R` | `output/reference_outputs/paper_welfare_estimates.xlsx` |
| Appendix A | Simulation procedure and confidence intervals for the welfare estimates | `code/04_welfare/02_estimate_welfare.R` | `output/reference_outputs/Appendix.docx` |
| Appendix Table B.1 | Underlying adjusted-model estimates and welfare calculations | `code/02_estimation/01_estimate_covariate_full.R`, `code/02_estimation/02_estimate_covariate_aggregated.R`, and `code/04_welfare/02_estimate_welfare.R` | `output/reference_outputs/Appendix.docx` |
| Appendix Table C.1 | `output/tables/site_visitation_breakdown_non_corrected.xlsx` | `code/05_figures_tables/05_site_visitation_breakdown.R` | `output/reference_outputs/site_visitation_breakdown.xlsx` and `output/reference_outputs/Appendix.docx` |

The files in `output/reference_outputs/` are fixed copies of the outputs used
in the submitted paper. Replication scripts write their results elsewhere
under `output/` and do not overwrite these reference copies.

## Citation

Please cite the article and the
[Zenodo data record](https://doi.org/10.5281/zenodo.23811094) when using these
materials.
