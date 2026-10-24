############################################################
## Welfare losses from brown water events
############################################################

# This script evaluates the two policy scenarios for four empirical models:
# the conditional logit and two-stage estimator, each using either the complete
# or aggregated choice set. Results are five-year totals in millions of USD;
# the next script converts them to annual losses for Table 2.

rm(list = ls())

library(MASS)
library(openxlsx)
library(parallel)

source(file.path("code", "04_welfare", "01_welfare_calculations.R"))

number_of_draws <- 1000
random_seed <- 12345
number_of_cores <- min(12, max(1, detectCores(logical = FALSE) - 1))
set.seed(random_seed)

output_dir <- file.path("output", "welfare", "post_estimation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


############################################################
## 1. Load model and welfare inputs
############################################################

cl_full <- readRDS(file.path(
  "output", "models", "covariate_full_adjusted",
  "EC_model3_reduced_model.rds"
))
cl_aggregated <- readRDS(file.path(
  "output", "models", "covariate_aggregated_adjusted",
  "MNL_cov_aggregated_reduced_adj_model.rds"
))
fe_full <- readRDS(file.path(
  "output", "models", "FE_full_adjusted", "FE-extended_adj_model.rds"
))
fe_aggregated <- readRDS(file.path(
  "output", "models", "FE_aggregated_adjusted",
  "FE-aggregated_model.rds"
))

bootstrap_full <- readRDS(file.path(
  "output", "bootstrap", "two_stage_murdock",
  "bootstrap_2stage_full_results.rds"
))
bootstrap_aggregated <- readRDS(file.path(
  "output", "bootstrap", "two_stage_murdock",
  "bootstrap_2stage_aggregated_results.rds"
))

full_data <- read_welfare_data("full")
aggregated_data <- read_welfare_data("aggregated")

full_clm_X <- build_covariate_matrix(full_data$x, "full")
aggregated_clm_X <- build_covariate_matrix(
  aggregated_data$x,
  "aggregated"
)

full_two_stage_X <- build_second_stage_matrix(full_data$x, "full")
aggregated_two_stage_X <- build_second_stage_matrix(
  aggregated_data$x,
  "aggregated"
)

full_fixed_effect_X <- cbind(
  site_dummy_matrix(full_data$x$siteID),
  cost = full_data$x$cost
)
aggregated_fixed_effect_X <- cbind(
  site_dummy_matrix(aggregated_data$x$siteID),
  cost = aggregated_data$x$cost
)


############################################################
## 2. Parallel evaluation of already-generated parameter draws
############################################################

cluster <- NULL
if (number_of_cores > 1) {
  cluster <- makeCluster(number_of_cores)
  clusterExport(
    cluster,
    c("total_site_closure_loss", "total_water_restriction_loss"),
    envir = .GlobalEnv
  )
}

evaluate_draws <- function(draw_ids, draw_function, export_names, label) {
  if (is.null(cluster)) {
    values <- vapply(draw_ids, draw_function, numeric(1))
  } else {
    clusterExport(cluster, export_names, envir = .GlobalEnv)
    values <- unlist(parLapplyLB(cluster, draw_ids, draw_function))
  }
  cat(label, "completed:", length(values), "draws\n")
  values
}

results <- list()


############################################################
## 3. Site-closure scenario
############################################################

### 3.1 Conditional logit, all sites

full_clm_vcov <- cl_full$varcov
full_clm_beta <- cl_full$estimate[rownames(full_clm_vcov)]
full_clm_X <- full_clm_X[, names(full_clm_beta), drop = FALSE]

full_clm_closure_point <- total_site_closure_loss(
  beta = full_clm_beta,
  X = full_clm_X,
  site_id = full_data$x$siteID,
  grid_id = full_data$x$id1km,
  population = full_data$population,
  closures = full_data$closures,
  events = full_data$events,
  cost_parameter = "beta_tc"
)

full_clm_closure_parameters <- mvrnorm(
  number_of_draws,
  full_clm_beta,
  full_clm_vcov
)
colnames(full_clm_closure_parameters) <- names(full_clm_beta)

full_clm_closure_draw <- function(draw_number) {
  total_site_closure_loss(
    beta = full_clm_closure_parameters[draw_number, ],
    X = full_clm_X,
    site_id = full_data$x$siteID,
    grid_id = full_data$x$id1km,
    population = full_data$population,
    closures = full_data$closures,
    events = full_data$events,
    cost_parameter = "beta_tc"
  )
}

full_clm_closure_draws <- evaluate_draws(
  seq_len(number_of_draws),
  full_clm_closure_draw,
  c(
    "full_clm_closure_parameters", "full_clm_X", "full_data",
    "full_clm_closure_draw"
  ),
  "Conditional logit, all sites, site closure"
)

results$CL_full_siteclosure <- summarize_welfare_draws(
  full_clm_closure_point,
  full_clm_closure_draws
)


### 3.2 Two-stage model, all sites

full_fe_beta <- fe_full$estimate
full_fixed_effect_X <- full_fixed_effect_X[
  , names(full_fe_beta), drop = FALSE
]

full_two_stage_closure_point <- total_site_closure_loss(
  beta = full_fe_beta,
  X = full_fixed_effect_X,
  site_id = full_data$x$siteID,
  grid_id = full_data$x$id1km,
  population = full_data$population,
  closures = full_data$closures,
  events = full_data$events,
  cost_parameter = "cost"
)

full_fe_draw_names <- rownames(fe_full$varcovBGW)
full_fe_closure_parameters <- mvrnorm(
  number_of_draws,
  full_fe_beta[full_fe_draw_names],
  fe_full$varcovBGW
)
colnames(full_fe_closure_parameters) <- full_fe_draw_names

full_two_stage_closure_draw <- function(draw_number) {
  beta_draw <- full_fe_beta
  beta_draw[full_fe_draw_names] <-
    full_fe_closure_parameters[draw_number, ]

  total_site_closure_loss(
    beta = beta_draw,
    X = full_fixed_effect_X,
    site_id = full_data$x$siteID,
    grid_id = full_data$x$id1km,
    population = full_data$population,
    closures = full_data$closures,
    events = full_data$events,
    cost_parameter = "cost"
  )
}

full_two_stage_closure_draws <- evaluate_draws(
  seq_len(number_of_draws),
  full_two_stage_closure_draw,
  c(
    "full_fe_beta", "full_fe_draw_names", "full_fe_closure_parameters",
    "full_fixed_effect_X", "full_data", "full_two_stage_closure_draw"
  ),
  "Two-stage, all sites, site closure"
)

results$FE2_full_siteclosure <- summarize_welfare_draws(
  full_two_stage_closure_point,
  full_two_stage_closure_draws
)


### 3.3 Conditional logit, aggregated sites

aggregated_clm_vcov <- cl_aggregated$varcov
aggregated_clm_beta <- cl_aggregated$estimate[
  rownames(aggregated_clm_vcov)
]
aggregated_clm_X <- aggregated_clm_X[
  , names(aggregated_clm_beta), drop = FALSE
]

aggregated_clm_closure_point <- total_site_closure_loss(
  beta = aggregated_clm_beta,
  X = aggregated_clm_X,
  site_id = aggregated_data$x$siteID,
  grid_id = aggregated_data$x$id1km,
  population = aggregated_data$population,
  closures = aggregated_data$closures,
  events = aggregated_data$events,
  cost_parameter = "beta_tc"
)

aggregated_clm_closure_parameters <- mvrnorm(
  number_of_draws,
  aggregated_clm_beta,
  aggregated_clm_vcov
)
colnames(aggregated_clm_closure_parameters) <- names(aggregated_clm_beta)

aggregated_clm_closure_draw <- function(draw_number) {
  total_site_closure_loss(
    beta = aggregated_clm_closure_parameters[draw_number, ],
    X = aggregated_clm_X,
    site_id = aggregated_data$x$siteID,
    grid_id = aggregated_data$x$id1km,
    population = aggregated_data$population,
    closures = aggregated_data$closures,
    events = aggregated_data$events,
    cost_parameter = "beta_tc"
  )
}

aggregated_clm_closure_draws <- evaluate_draws(
  seq_len(number_of_draws),
  aggregated_clm_closure_draw,
  c(
    "aggregated_clm_closure_parameters", "aggregated_clm_X",
    "aggregated_data", "aggregated_clm_closure_draw"
  ),
  "Conditional logit, aggregated sites, site closure"
)

results$CL_aggregated_siteclosure <- summarize_welfare_draws(
  aggregated_clm_closure_point,
  aggregated_clm_closure_draws
)


### 3.4 Two-stage model, aggregated sites

aggregated_fe_beta <- fe_aggregated$estimate
aggregated_fixed_effect_X <- aggregated_fixed_effect_X[
  , names(aggregated_fe_beta), drop = FALSE
]

aggregated_two_stage_closure_point <- total_site_closure_loss(
  beta = aggregated_fe_beta,
  X = aggregated_fixed_effect_X,
  site_id = aggregated_data$x$siteID,
  grid_id = aggregated_data$x$id1km,
  population = aggregated_data$population,
  closures = aggregated_data$closures,
  events = aggregated_data$events,
  cost_parameter = "cost"
)

aggregated_fe_draw_names <- rownames(fe_aggregated$varcovBGW)
aggregated_fe_closure_parameters <- mvrnorm(
  number_of_draws,
  aggregated_fe_beta[aggregated_fe_draw_names],
  fe_aggregated$varcovBGW
)
colnames(aggregated_fe_closure_parameters) <- aggregated_fe_draw_names

aggregated_two_stage_closure_draw <- function(draw_number) {
  beta_draw <- aggregated_fe_beta
  beta_draw[aggregated_fe_draw_names] <-
    aggregated_fe_closure_parameters[draw_number, ]

  total_site_closure_loss(
    beta = beta_draw,
    X = aggregated_fixed_effect_X,
    site_id = aggregated_data$x$siteID,
    grid_id = aggregated_data$x$id1km,
    population = aggregated_data$population,
    closures = aggregated_data$closures,
    events = aggregated_data$events,
    cost_parameter = "cost"
  )
}

aggregated_two_stage_closure_draws <- evaluate_draws(
  seq_len(number_of_draws),
  aggregated_two_stage_closure_draw,
  c(
    "aggregated_fe_beta", "aggregated_fe_draw_names",
    "aggregated_fe_closure_parameters", "aggregated_fixed_effect_X",
    "aggregated_data", "aggregated_two_stage_closure_draw"
  ),
  "Two-stage, aggregated sites, site closure"
)

results$FE2_aggregated_siteclosure <- summarize_welfare_draws(
  aggregated_two_stage_closure_point,
  aggregated_two_stage_closure_draws
)


############################################################
## 4. Water-restriction scenario
############################################################

full_clm_water_columns <- c(
  "beta_surf", "beta_swim", "beta_snork", "beta_fish", "beta_coral"
)
aggregated_clm_water_columns <- full_clm_water_columns
two_stage_water_columns <- c(
  "surf", "swim", "snork", "res_fish_bio", "PNAS"
)


### 4.1 Conditional logit, all sites

full_clm_quality_point <- total_water_restriction_loss(
  beta = full_clm_beta,
  X = full_clm_X,
  water_columns = full_clm_water_columns,
  site_id = full_data$x$siteID,
  grid_id = full_data$x$id1km,
  population = full_data$population,
  closures = full_data$closures,
  events = full_data$events,
  cost_parameter = "beta_tc"
)

full_clm_quality_parameters <- mvrnorm(
  number_of_draws,
  full_clm_beta,
  full_clm_vcov
)
colnames(full_clm_quality_parameters) <- names(full_clm_beta)

full_clm_quality_draw <- function(draw_number) {
  total_water_restriction_loss(
    beta = full_clm_quality_parameters[draw_number, ],
    X = full_clm_X,
    water_columns = full_clm_water_columns,
    site_id = full_data$x$siteID,
    grid_id = full_data$x$id1km,
    population = full_data$population,
    closures = full_data$closures,
    events = full_data$events,
    cost_parameter = "beta_tc"
  )
}

full_clm_quality_draws <- evaluate_draws(
  seq_len(number_of_draws),
  full_clm_quality_draw,
  c(
    "full_clm_quality_parameters", "full_clm_X",
    "full_clm_water_columns", "full_data", "full_clm_quality_draw"
  ),
  "Conditional logit, all sites, water restriction"
)

results$CL_full_bwquality <- summarize_welfare_draws(
  full_clm_quality_point,
  full_clm_quality_draws
)


### 4.2 Two-stage model, all sites

full_second_stage <- second_stage_from_fixed_effects(
  fe_full$estimate,
  full_data$sites,
  "full"
)
full_theta <- full_second_stage$theta
full_theta_vcov <- bootstrap_full$bootstrap$theta_vcov[
  names(full_theta), names(full_theta)
]
full_two_stage_X <- full_two_stage_X[, names(full_theta), drop = FALSE]

full_two_stage_quality_point <- total_water_restriction_loss(
  beta = full_theta,
  X = full_two_stage_X,
  water_columns = two_stage_water_columns,
  site_id = full_data$x$siteID,
  grid_id = full_data$x$id1km,
  population = full_data$population,
  closures = full_data$closures,
  events = full_data$events,
  cost_parameter = "cost",
  site_residuals = full_second_stage$residuals
)

full_two_stage_quality_parameters <- mvrnorm(
  number_of_draws,
  full_theta,
  full_theta_vcov
)
colnames(full_two_stage_quality_parameters) <- names(full_theta)

full_two_stage_quality_draw <- function(draw_number) {
  total_water_restriction_loss(
    beta = full_two_stage_quality_parameters[draw_number, ],
    X = full_two_stage_X,
    water_columns = two_stage_water_columns,
    site_id = full_data$x$siteID,
    grid_id = full_data$x$id1km,
    population = full_data$population,
    closures = full_data$closures,
    events = full_data$events,
    cost_parameter = "cost",
    site_residuals = full_second_stage$residuals
  )
}

full_two_stage_quality_draws <- evaluate_draws(
  seq_len(number_of_draws),
  full_two_stage_quality_draw,
  c(
    "full_two_stage_quality_parameters", "full_two_stage_X",
    "two_stage_water_columns", "full_data", "full_second_stage",
    "full_two_stage_quality_draw"
  ),
  "Two-stage, all sites, water restriction"
)

results$FE2_full_bwquality <- summarize_welfare_draws(
  full_two_stage_quality_point,
  full_two_stage_quality_draws
)


### 4.3 Conditional logit, aggregated sites

aggregated_clm_quality_point <- total_water_restriction_loss(
  beta = aggregated_clm_beta,
  X = aggregated_clm_X,
  water_columns = aggregated_clm_water_columns,
  site_id = aggregated_data$x$siteID,
  grid_id = aggregated_data$x$id1km,
  population = aggregated_data$population,
  closures = aggregated_data$closures,
  events = aggregated_data$events,
  cost_parameter = "beta_tc"
)

aggregated_clm_quality_parameters <- mvrnorm(
  number_of_draws,
  aggregated_clm_beta,
  aggregated_clm_vcov
)
colnames(aggregated_clm_quality_parameters) <- names(aggregated_clm_beta)

aggregated_clm_quality_draw <- function(draw_number) {
  total_water_restriction_loss(
    beta = aggregated_clm_quality_parameters[draw_number, ],
    X = aggregated_clm_X,
    water_columns = aggregated_clm_water_columns,
    site_id = aggregated_data$x$siteID,
    grid_id = aggregated_data$x$id1km,
    population = aggregated_data$population,
    closures = aggregated_data$closures,
    events = aggregated_data$events,
    cost_parameter = "beta_tc"
  )
}

aggregated_clm_quality_draws <- evaluate_draws(
  seq_len(number_of_draws),
  aggregated_clm_quality_draw,
  c(
    "aggregated_clm_quality_parameters", "aggregated_clm_X",
    "aggregated_clm_water_columns", "aggregated_data",
    "aggregated_clm_quality_draw"
  ),
  "Conditional logit, aggregated sites, water restriction"
)

results$CL_aggregated_bwquality <- summarize_welfare_draws(
  aggregated_clm_quality_point,
  aggregated_clm_quality_draws
)


### 4.4 Two-stage model, aggregated sites

aggregated_second_stage <- second_stage_from_fixed_effects(
  fe_aggregated$estimate,
  aggregated_data$sites,
  "aggregated"
)
aggregated_theta <- aggregated_second_stage$theta
aggregated_theta_vcov <- bootstrap_aggregated$bootstrap$theta_vcov[
  names(aggregated_theta), names(aggregated_theta)
]
aggregated_two_stage_X <- aggregated_two_stage_X[
  , names(aggregated_theta), drop = FALSE
]

aggregated_two_stage_quality_point <- total_water_restriction_loss(
  beta = aggregated_theta,
  X = aggregated_two_stage_X,
  water_columns = two_stage_water_columns,
  site_id = aggregated_data$x$siteID,
  grid_id = aggregated_data$x$id1km,
  population = aggregated_data$population,
  closures = aggregated_data$closures,
  events = aggregated_data$events,
  cost_parameter = "cost",
  site_residuals = aggregated_second_stage$residuals
)

aggregated_two_stage_quality_parameters <- mvrnorm(
  number_of_draws,
  aggregated_theta,
  aggregated_theta_vcov
)
colnames(aggregated_two_stage_quality_parameters) <- names(aggregated_theta)

aggregated_two_stage_quality_draw <- function(draw_number) {
  total_water_restriction_loss(
    beta = aggregated_two_stage_quality_parameters[draw_number, ],
    X = aggregated_two_stage_X,
    water_columns = two_stage_water_columns,
    site_id = aggregated_data$x$siteID,
    grid_id = aggregated_data$x$id1km,
    population = aggregated_data$population,
    closures = aggregated_data$closures,
    events = aggregated_data$events,
    cost_parameter = "cost",
    site_residuals = aggregated_second_stage$residuals
  )
}

aggregated_two_stage_quality_draws <- evaluate_draws(
  seq_len(number_of_draws),
  aggregated_two_stage_quality_draw,
  c(
    "aggregated_two_stage_quality_parameters", "aggregated_two_stage_X",
    "two_stage_water_columns", "aggregated_data",
    "aggregated_second_stage", "aggregated_two_stage_quality_draw"
  ),
  "Two-stage, aggregated sites, water restriction"
)

results$FE2_aggregated_bwquality <- summarize_welfare_draws(
  aggregated_two_stage_quality_point,
  aggregated_two_stage_quality_draws
)


############################################################
## 5. Save the simulation results
############################################################

if (!is.null(cluster)) stopCluster(cluster)

saveRDS(results, file.path(output_dir, "welfare_results.rds"))
cat("Saved welfare results in", output_dir, "\n")
