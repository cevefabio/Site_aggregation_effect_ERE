############################################################
#### STANDALONE SINGLE-REPLICA TWO-STAGE BOOTSTRAP      ####
#### Classical ML first-stage FE-MNL, no Apollo          ####
############################################################

rm(list = ls())

library(tidyverse)
library(openxlsx)

# Run this script from the project root.
#
# This file is intentionally standalone. It runs one complete bootstrap cycle:
#   1. load the unadjusted wide dataset;
#   2. resample respondents with replacement;
#   3. add one trip to every site that is absent in the resample;
#   4. re-estimate the FE first stage with classical ML, without Hessian/var-cov;
#   5. run the second-stage Murdock regression;
#   6. save the single-replica results.


# ===========================================================
# 0. Settings
# ===========================================================

read_int_env <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || value == "") return(default)
  as.integer(value)
}

progress_message <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", ..., "\n")
  flush.console()
}

coverage <- tolower(Sys.getenv("SINGLE_REPLICA_COVERAGE", "aggregated"))
if (!(coverage %in% c("full", "aggregated"))) {
  stop("SINGLE_REPLICA_COVERAGE must be 'full' or 'aggregated'.")
}

bootstrap_seed   <- read_int_env("BOOTSTRAP_SEED", 20260614)
ml_max_iterations <- read_int_env(
  "ML_MAX_ITERATIONS",
  read_int_env("APOLLO_MAX_ITERATIONS", 200)
)

set.seed(bootstrap_seed)

output_dir <- file.path("output", "bootstrap", "two_stage_murdock", "single_replica")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

paths <- list(
  full_database = file.path("data", "processed", "HW_wide_full_unadjusted.rds"),
  agg_database  = file.path("data", "processed", "HW_wide_aggregated_unadjusted.rds"),
  full_sites = file.path("data", "external", "sites.xlsx"),
  agg_sites  = file.path("data", "external", "sites_aggregated.xlsx"),
  full_point_model = file.path("output", "models", "FE_full_adjusted", "FE-extended_adj_model.rds"),
  agg_point_model  = file.path("output", "models", "FE_aggregated_adjusted", "FE-aggregated_model.rds")
)


# ===========================================================
# 1. I/O and model components
# ===========================================================

read_estimation_database <- function(coverage) {
  if (coverage == "full") readRDS(paths$full_database) else readRDS(paths$agg_database)
}

read_sites <- function(coverage) {
  if (coverage == "full") {
    sites <- read.xlsx(paths$full_sites)
    sites$snork[sites$siteID == 14] <- 0
  } else {
    sites <- read.xlsx(paths$agg_sites)
    sites$oor <- ifelse(sites$siteID == 999, 1, 0)
  }
  sites
}

extract_fe_components <- function(fe_model, sites) {
  beta <- fe_model$estimate
  asc_names <- paste0("ASC_", sites$siteID)
  
  if (!("cost" %in% names(beta))) stop("Could not find coefficient 'cost'.")
  missing_asc <- setdiff(asc_names, names(beta))
  if (length(missing_asc) > 0) {
    stop("Missing ASC coefficients: ", paste(missing_asc, collapse = ", "))
  }
  
  list(
    asc = beta[asc_names],
    cost = beta[["cost"]],
    beta = beta[c(asc_names, "cost")]
  )
}

second_stage_formula <- function(coverage) {
  if (coverage == "full") {
    FE ~ home + coastal + citypark + trailall +
      haleakala + molokini + parking + showers +
      lifeguard + rock + manmade + beachmedium + beachlarge +
      surf + swim + snork + res_fish_bio + PNAS +
      playground + sports
  } else {
    FE ~ home + oor + molokini + parking + showers +
      lifeguard + rock + manmade + beachmedium + beachlarge +
      surf + swim + snork + res_fish_bio + PNAS
  }
}

fit_second_stage_from_fe <- function(asc, cost, sites, coverage) {
  ss_data <- sites
  ss_data$FE <- as.numeric(asc[paste0("ASC_", sites$siteID)])
  
  model <- lm(second_stage_formula(coverage), data = ss_data)
  theta <- c(cost = cost, coef(model)[-1])
  
  list(
    model = model,
    coef_full = coef(model),
    residuals = resid(model),
    theta = theta,
    site_order = sites$siteID
  )
}


# ===========================================================
# 2. One bootstrap resample and adjustment
# ===========================================================

bootstrap_wide_by_respondent <- function(database) {
  ids <- unique(database$respid)
  blocks <- split(database, database$respid)
  sampled_ids <- sample(ids, size = length(ids), replace = TRUE)
  
  boot_blocks <- vector("list", length(sampled_ids))
  for (i in seq_along(sampled_ids)) {
    block <- blocks[[as.character(sampled_ids[i])]]
    block$respid <- i
    boot_blocks[[i]] <- block
  }
  
  bind_rows(boot_blocks)
}

get_home_site_by_row <- function(database, sites) {
  home_cols <- paste0("home_", sites$siteID)
  missing_cols <- setdiff(home_cols, names(database))
  if (length(missing_cols) > 0) {
    stop("Missing home columns: ", paste(missing_cols, collapse = ", "))
  }
  
  home_mat <- as.matrix(database[, home_cols])
  storage.mode(home_mat) <- "double"
  
  home_index <- max.col(home_mat, ties.method = "first")
  no_home <- rowSums(home_mat, na.rm = TRUE) <= 0
  home_site <- sites$siteID[home_index]
  home_site[no_home] <- NA
  home_site
}

ensure_all_alternatives_chosen <- function(database, sites) {
  all_sites <- sites$siteID
  never_chosen <- setdiff(all_sites, unique(database$choice))
  home_site <- get_home_site_by_row(database, sites)
  
  adjustment_log <- tibble(
    siteID = numeric(),
    respid = numeric(),
    home_siteID = numeric(),
    row = integer(),
    assigned = logical()
  )
  
  if (length(never_chosen) == 0) {
    return(list(database = database, log = adjustment_log))
  }
  
  first_nonmissing <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else x[1]
  }
  
  for (site in never_chosen) {
    cost_col <- paste0("cost_", site)
    
    resp_costs <- database |>
      mutate(.home_site = home_site) |>
      group_by(respid) |>
      summarise(
        target_cost = min(.data[[cost_col]], na.rm = TRUE),
        home_siteID = first_nonmissing(.home_site),
        .groups = "drop"
      ) |>
      arrange(target_cost)
    
    assigned <- FALSE
    assigned_resp <- NA_real_
    assigned_home <- NA_real_
    assigned_row <- NA_integer_
    
    for (i in seq_len(nrow(resp_costs))) {
      if (!is.finite(resp_costs$target_cost[i]) || is.na(resp_costs$home_siteID[i])) next
      
      candidate_rows <- which(
        database$respid == resp_costs$respid[i] &
          database$choice == resp_costs$home_siteID[i]
      )
      if (length(candidate_rows) == 0) next
      
      assigned_row <- candidate_rows[1]
      database$choice[assigned_row] <- site
      assigned <- TRUE
      assigned_resp <- resp_costs$respid[i]
      assigned_home <- resp_costs$home_siteID[i]
      break
    }
    
    adjustment_log <- bind_rows(
      adjustment_log,
      tibble(
        siteID = site,
        respid = assigned_resp,
        home_siteID = assigned_home,
        row = assigned_row,
        assigned = assigned
      )
    )
    
    if (!assigned) warning(paste("No valid respondent with home visits for site:", site))
  }
  
  list(database = database, log = adjustment_log)
}


# ===========================================================
# 3. First-stage classical ML FE-MNL estimation
# ===========================================================

fit_first_stage_ml <- function(database, sites, beta_start, coverage) {
  site_ids <- sites$siteID
  J <- length(site_ids)
  
  cost_cols <- paste0("cost_", site_ids)
  missing_cols <- setdiff(cost_cols, names(database))
  if (length(missing_cols) > 0) {
    stop("Missing cost columns: ", paste(missing_cols, collapse = ", "))
  }
  
  choice_index <- match(as.character(database$choice), as.character(site_ids))
  if (anyNA(choice_index)) {
    unknown_choices <- unique(database$choice[is.na(choice_index)])
    stop("Choices not found in site list: ", paste(unknown_choices, collapse = ", "))
  }
  
  cost_mat <- as.matrix(database[, cost_cols])
  storage.mode(cost_mat) <- "double"
  
  if (anyNA(cost_mat)) {
    na_cols <- cost_cols[colSums(is.na(cost_mat)) > 0]
    stop(
      "NA values found in cost columns: ",
      paste(na_cols, collapse = ", "),
      ". Classical ML cannot estimate with NA utilities."
    )
  }
  
  if (any(!is.finite(cost_mat))) {
    bad_cols <- cost_cols[colSums(!is.finite(cost_mat)) > 0]
    stop(
      "Non-finite values found in cost columns: ",
      paste(bad_cols, collapse = ", "),
      ". Classical ML cannot estimate with Inf or NaN utilities."
    )
  }
  
  asc_names <- paste0("ASC_", site_ids)
  fixed_asc <- "ASC_1"
  
  if (!(fixed_asc %in% asc_names)) {
    stop("The fixed base alternative ", fixed_asc, " is not present in sites$siteID.")
  }
  
  free_asc_names <- setdiff(asc_names, fixed_asc)
  param_names <- c(free_asc_names, "cost")
  
  start <- setNames(rep(0, length(param_names)), param_names)
  common <- intersect(names(start), names(beta_start))
  start[common] <- beta_start[common]
  
  free_asc_index <- match(free_asc_names, asc_names)
  n_obs <- nrow(database)
  obs_index <- seq_len(n_obs)
  
  loglik_mnl <- function(par) {
    asc <- setNames(rep(0, J), asc_names)
    asc[free_asc_names] <- par[free_asc_names]
    
    beta_cost <- par[["cost"]]
    
    V <- beta_cost * cost_mat
    V <- sweep(V, 2, asc, "+")
    
    row_max <- apply(V, 1, max)
    expV <- exp(V - row_max)
    denom <- rowSums(expV)
    
    chosen_V <- V[cbind(obs_index, choice_index)]
    ll <- sum(chosen_V - row_max - log(denom))
    
    if (!is.finite(ll)) return(-1e100)
    ll
  }
  
  grad_loglik_mnl <- function(par) {
    asc <- setNames(rep(0, J), asc_names)
    asc[free_asc_names] <- par[free_asc_names]
    
    beta_cost <- par[["cost"]]
    
    V <- beta_cost * cost_mat
    V <- sweep(V, 2, asc, "+")
    
    row_max <- apply(V, 1, max)
    expV <- exp(V - row_max)
    P <- expV / rowSums(expV)
    
    chosen_counts <- tabulate(choice_index, nbins = J)
    grad_asc_full <- chosen_counts - colSums(P)
    
    chosen_cost <- cost_mat[cbind(obs_index, choice_index)]
    expected_cost <- rowSums(P * cost_mat)
    grad_cost <- sum(chosen_cost - expected_cost)
    
    grad <- c(
      grad_asc_full[free_asc_index],
      cost = grad_cost
    )
    
    names(grad) <- param_names
    grad
  }
  
  optim_res <- optim(
    par = start,
    fn = function(par) -loglik_mnl(par),
    gr = function(par) -grad_loglik_mnl(par),
    method = "BFGS",
    control = list(
      maxit = ml_max_iterations,
      reltol = 1e-10
    ),
    hessian = FALSE
  )
  
  estimate <- setNames(rep(0, length(asc_names) + 1), c(asc_names, "cost"))
  estimate[free_asc_names] <- optim_res$par[free_asc_names]
  estimate["cost"] <- optim_res$par["cost"]
  
  loglik <- -optim_res$value
  grad_at_solution <- grad_loglik_mnl(optim_res$par)
  
  if (optim_res$convergence != 0) {
    warning(
      "First-stage ML did not fully converge. Code: ",
      optim_res$convergence,
      "; message: ",
      optim_res$message
    )
  }
  
  list(
    model = optim_res,
    estimate = estimate[c(asc_names, "cost")],
    asc = estimate[asc_names],
    cost = estimate[["cost"]],
    loglik = loglik,
    convergence = list(
      code = optim_res$convergence,
      message = optim_res$message,
      iterations = unname(optim_res$counts[["function"]]),
      maximum = loglik,
      optimizer = "stats::optim_BFGS",
      hessian_routine = "none",
      gradient_inf_norm = max(abs(grad_at_solution))
    )
  )
}


# ===========================================================
# 4. Run one complete replica
# ===========================================================

progress_message("Starting standalone single bootstrap replica for", coverage)

database <- read_estimation_database(coverage)
sites <- read_sites(coverage)

point_model_path <- if (coverage == "full") paths$full_point_model else paths$agg_point_model
point_fe_model <- readRDS(point_model_path)
point_fe <- extract_fe_components(point_fe_model, sites)

progress_message("Resampling respondents")
boot_database <- bootstrap_wide_by_respondent(database)

progress_message("Applying missing-site trip adjustment")
boot_adjusted <- ensure_all_alternatives_chosen(boot_database, sites)
boot_database <- boot_adjusted$database

progress_message(
  "Adjustment assigned",
  sum(boot_adjusted$log$assigned),
  "missing-site trips"
)

progress_message("Estimating first-stage FE model with classical ML")
boot_fe <- fit_first_stage_ml(
  database = boot_database,
  sites = sites,
  beta_start = point_fe$beta,
  coverage = coverage
)

progress_message("Estimating second-stage regression")
boot_ts <- fit_second_stage_from_fe(
  asc = boot_fe$asc,
  cost = boot_fe$cost,
  sites = sites,
  coverage = coverage
)

result <- list(
  coverage = coverage,
  settings = list(
    estimator = "standalone_single_replica_classical_ML_FE_MNL",
    seed = bootstrap_seed,
    first_stage_optimizer = "stats::optim_BFGS_with_analytic_gradient",
    ml_max_iterations = ml_max_iterations,
    resampling_database = if (coverage == "full") paths$full_database else paths$agg_database,
    adjustment = "after_resample_reassign_home_trip_to_never_chosen_sites"
  ),
  point = list(
    first_stage_model_path = point_model_path,
    point_first_stage_estimate = point_fe$beta
  ),
  bootstrap = list(
    first_stage_estimate = boot_fe$estimate,
    first_stage_diagnostics = boot_fe$convergence,
    second_stage_coefficients = boot_ts$coef_full,
    theta = boot_ts$theta,
    residuals = boot_ts$residuals,
    adjustment_log = boot_adjusted$log,
    site_order = boot_ts$site_order
  )
)

summary_table <- tibble(
  term = names(result$bootstrap$theta),
  estimate = as.numeric(result$bootstrap$theta)
)

rds_file <- file.path(output_dir, paste0("standalone_single_replica_", coverage, "_results.rds"))
xlsx_file <- file.path(output_dir, paste0("standalone_single_replica_", coverage, "_theta.xlsx"))

saveRDS(result, rds_file)
write.xlsx(summary_table, file = xlsx_file, overwrite = TRUE)

progress_message("Saved RDS:", normalizePath(rds_file, winslash = "/", mustWork = FALSE))
progress_message("Saved table:", normalizePath(xlsx_file, winslash = "/", mustWork = FALSE))
progress_message("Standalone single replica completed")