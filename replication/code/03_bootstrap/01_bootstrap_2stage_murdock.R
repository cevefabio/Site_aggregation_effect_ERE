############################################################
#### PARAMETER BOOTSTRAP FOR TWO-STAGE MURDOCK (2006)   ####
#### Classical ML first-stage FE-MNL, no Apollo          ####
############################################################

rm(list = ls())

library(tidyverse)
library(openxlsx)
library(parallel)

# Run this script from the project root.
#
# This script bootstraps the full two-stage Murdock procedure, but it does
# not compute welfare. Its only job is to produce a bootstrap-corrected
# covariance matrix for the two-stage parameter vector:
#
#   theta = c(cost, second-stage coefficients without intercept)
#
# Welfare estimates and welfare confidence intervals are computed later in
# code/04_welfare/01_estimate_welfare.R, using this covariance matrix.


# ===========================================================
# 0. User settings
# ===========================================================

progress_message <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", ..., "\n")
  flush.console()
}

bootstrap_seed <- 86
set.seed(bootstrap_seed)

n_boot_outer <- 500
n_boot_inner <- 1000
max_attempts <- 1500
ml_max_iterations <- 200

bootstrap_n_workers <- min(
  max(1, parallel::detectCores() - 1),
  12
)

output_dir <- file.path("output", "bootstrap", "two_stage_murdock")
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
# 1. Project I/O
# ===========================================================

read_estimation_database <- function(coverage = c("full", "aggregated")) {
  coverage <- match.arg(coverage)
  
  if (coverage == "full") {
    readRDS(paths$full_database)
  } else {
    readRDS(paths$agg_database)
  }
}


read_sites <- function(coverage = c("full", "aggregated")) {
  coverage <- match.arg(coverage)
  
  if (coverage == "full") {
    sites <- read.xlsx(paths$full_sites)
  } else {
    sites <- read.xlsx(paths$agg_sites)
    sites$oor <- ifelse(sites$siteID == 999, 1, 0)
  }
  
  sites
}


extract_fe_components <- function(fe_model, sites) {
  beta <- fe_model$estimate
  
  if (!("cost" %in% names(beta))) {
    stop("Could not find a coefficient named 'cost' in the FE model.")
  }
  
  asc_names <- paste0("ASC_", sites$siteID)
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


# ===========================================================
# 2. Second stage
# ===========================================================

second_stage_formula <- function(coverage = c("full", "aggregated")) {
  coverage <- match.arg(coverage)
  
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


fit_second_stage_from_fe <- function(asc, cost, sites,
                                     coverage = c("full", "aggregated")) {
  coverage <- match.arg(coverage)
  
  ss_data <- sites
  ss_data$FE <- as.numeric(asc[paste0("ASC_", sites$siteID)])
  
  model <- lm(second_stage_formula(coverage), data = ss_data)
  
  theta <- c(cost = cost, coef(model)[-1])
  
  list(
    model = model,
    coef_full = coef(model),
    coef_no_intercept = coef(model)[-1],
    residuals = resid(model),
    theta = theta,
    site_order = sites$siteID
  )
}


fit_second_stage_from_fe_model <- function(fe_model, sites,
                                           coverage = c("full", "aggregated")) {
  coverage <- match.arg(coverage)
  fe <- extract_fe_components(fe_model, sites)
  fit_second_stage_from_fe(fe$asc, fe$cost, sites, coverage)
}


fit_second_stage_from_site_table <- function(ss_data,
                                             coverage = c("full", "aggregated"),
                                             check_rank = TRUE) {
  coverage <- match.arg(coverage)
  formula <- second_stage_formula(coverage)
  
  if (check_rank) {
    X <- model.matrix(formula, data = ss_data)
    if (qr(X)$rank < ncol(X)) {
      stop("Second-stage design is rank deficient after site resampling.")
    }
  }
  
  model <- lm(formula, data = ss_data)
  coef_full <- coef(model)
  
  if (anyNA(coef_full)) {
    stop("Second-stage model produced NA coefficients after site resampling.")
  }
  
  list(
    model = model,
    coef_full = coef_full,
    coef_no_intercept = coef_full[-1],
    residuals = resid(model),
    theta = c(cost = ss_data$.cost[1], coef_full[-1]),
    site_order = ss_data$siteID
  )
}


resample_second_stage_sites <- function(ss_data,
                                        coverage = c("full", "aggregated")) {
  coverage <- match.arg(coverage)
  
  if (coverage == "aggregated" && any(ss_data$siteID == 999)) {
    oor_row <- ss_data[ss_data$siteID == 999, , drop = FALSE]
    other_rows <- ss_data[ss_data$siteID != 999, , drop = FALSE]
    sampled_other <- sample(
      seq_len(nrow(other_rows)),
      size = nrow(other_rows),
      replace = TRUE
    )
    
    return(list(
      data = bind_rows(oor_row, other_rows[sampled_other, , drop = FALSE]),
      sampled_rows = c(which(ss_data$siteID == 999), which(ss_data$siteID != 999)[sampled_other])
    ))
  }
  
  sampled_rows <- sample(seq_len(nrow(ss_data)),
                         size = nrow(ss_data),
                         replace = TRUE)
  list(
    data = ss_data[sampled_rows, , drop = FALSE],
    sampled_rows = sampled_rows
  )
}


# ===========================================================
# 3. First-stage FE model through classical ML
# ===========================================================

fit_first_stage_ml <- function(database, sites, beta_start = NULL,
                               coverage = c("full", "aggregated"),
                               draw_id = NULL) {
  coverage <- match.arg(coverage)
  
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
  
  full_start <- c(
    setNames(rep(0, length(site_ids)), asc_names),
    cost = 0
  )
  
  if (!is.null(beta_start)) {
    common <- intersect(names(full_start), names(beta_start))
    full_start[common] <- beta_start[common]
  }
  
  free_asc_names <- setdiff(asc_names, fixed_asc)
  param_names <- c(free_asc_names, "cost")
  
  par_start <- setNames(rep(0, length(param_names)), param_names)
  common <- intersect(names(par_start), names(full_start))
  par_start[common] <- full_start[common]
  
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
    par = par_start,
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
      optimizer = "stats::optim_BFGS_with_analytic_gradient",
      hessian_routine = "none",
      gradient_inf_norm = max(abs(grad_at_solution)),
      draw_id = draw_id
    )
  )
}


# ===========================================================
# 4. Bootstrap helpers
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
  out <- sites$siteID[home_index]
  out[no_home] <- NA
  out
}


ensure_all_alternatives_chosen <- function(database, sites) {
  all_sites <- sites$siteID
  chosen_sites <- unique(database$choice)
  never_chosen <- setdiff(all_sites, chosen_sites)
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
    
    if (!(cost_col %in% names(database))) {
      adjustment_log <- bind_rows(
        adjustment_log,
        tibble(
          siteID = site,
          respid = NA_real_,
          home_siteID = NA_real_,
          row = NA_integer_,
          assigned = FALSE
        )
      )
      next
    }
    
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
# 5. Bootstrap engine
# ===========================================================

bootstrap_two_stage_worker <- function(coverage = c("full", "aggregated"),
                                       n_success = n_boot_outer,
                                       n_inner = n_boot_inner,
                                       max_attempts = max_attempts,
                                       worker_id = 1,
                                       worker_seed = bootstrap_seed) {
  coverage <- match.arg(coverage)
  set.seed(worker_seed)
  
  progress_message("Starting nested classical ML two-stage bootstrap:", coverage, "- worker", worker_id)
  progress_message("Worker", worker_id, "- target successful outer draws:", n_success)
  progress_message("Worker", worker_id, "- inner site draws per outer draw:", n_inner)
  progress_message("Worker", worker_id, "- maximum attempts:", max_attempts)
  progress_message("Worker", worker_id, "- seed:", worker_seed)
  
  database <- read_estimation_database(coverage)
  sites <- read_sites(coverage)
  
  point_model_path <- if (coverage == "full") paths$full_point_model else paths$agg_point_model
  point_fe_model <- readRDS(point_model_path)
  point_fe <- extract_fe_components(point_fe_model, sites)
  point_ts <- fit_second_stage_from_fe_model(point_fe_model, sites, coverage)
  
  theta_draws <- list()
  second_stage_coef_draws <- list()
  cost_draws <- numeric()
  residual_draws <- list()
  first_stage_diagnostics <- list()
  bootstrap_adjustments <- list()
  outer_draw_summary <- list()
  failures <- tibble(attempt = integer(), message = character())
  inner_failures <- tibble(
    attempt = integer(),
    inner = integer(),
    message = character()
  )
  
  attempt <- 0
  success <- 0
  
  while (success < n_success && attempt < max_attempts) {
    attempt <- attempt + 1
    progress_message(
      "Worker", worker_id,
      "- attempt", attempt, "/", max_attempts,
      "- successful draws", success, "/", n_success
    )
    
    result <- tryCatch({
      boot_database <- bootstrap_wide_by_respondent(database)
      boot_adjusted <- ensure_all_alternatives_chosen(boot_database, sites)
      boot_database <- boot_adjusted$database
      
      boot_fe <- fit_first_stage_ml(
        database = boot_database,
        sites = sites,
        beta_start = point_fe$beta,
        coverage = coverage,
        draw_id = paste0(worker_id, "_", attempt)
      )
      
      ss_data <- sites
      ss_data$FE <- as.numeric(boot_fe$asc[paste0("ASC_", sites$siteID)])
      ss_data$.cost <- boot_fe$cost
      
      inner_theta_draws <- list()
      inner_coef_draws <- list()
      inner_residual_draws <- list()
      inner_site_samples <- list()
      draw_inner_failures <- tibble(
        attempt = integer(),
        inner = integer(),
        message = character()
      )
      
      for (inner in seq_len(n_inner)) {
        inner_result <- tryCatch({
          site_resample <- resample_second_stage_sites(ss_data, coverage)
          inner_data <- site_resample$data
          inner_ts <- fit_second_stage_from_site_table(
            ss_data = inner_data,
            coverage = coverage,
            check_rank = TRUE
          )
          list(ts = inner_ts, sampled_rows = site_resample$sampled_rows)
        }, error = function(e) {
          e
        })
        
        if (inherits(inner_result, "error")) {
          draw_inner_failures <- bind_rows(
            draw_inner_failures,
            tibble(
              attempt = attempt,
              inner = inner,
              message = conditionMessage(inner_result)
            )
          )
          next
        }
        
        inner_success <- length(inner_theta_draws) + 1
        inner_theta_draws[[inner_success]] <- inner_result$ts$theta
        inner_coef_draws[[inner_success]] <- inner_result$ts$coef_full
        inner_residual_draws[[inner_success]] <- inner_result$ts$residuals
        inner_site_samples[[inner_success]] <- inner_result$sampled_rows
      }
      
      if (length(inner_theta_draws) == 0) {
        stop("No successful inner site-resampling draws.")
      }
      
      list(
        theta_draws = inner_theta_draws,
        second_stage_coef_draws = inner_coef_draws,
        residual_draws = inner_residual_draws,
        site_samples = inner_site_samples,
        first_stage = boot_fe$convergence,
        cost = boot_fe$cost,
        adjustment_log = boot_adjusted$log,
        inner_failures = draw_inner_failures,
        n_inner_success = length(inner_theta_draws)
      )
    }, error = function(e) {
      e
    })
    
    if (inherits(result, "error")) {
      failures <- bind_rows(
        failures,
        tibble(attempt = attempt, message = conditionMessage(result))
      )
      progress_message(
        "Worker", worker_id,
        "- attempt", attempt,
        "failed:", conditionMessage(result)
      )
      next
    }
    
    success <- success + 1
    progress_message(
      "Worker", worker_id,
      "- completed outer draw", success, "/", n_success,
      "after attempt", attempt
    )
    
    draw_index <- length(theta_draws)
    for (i in seq_along(result$theta_draws)) {
      theta_draws[[draw_index + i]] <- result$theta_draws[[i]]
      second_stage_coef_draws[[draw_index + i]] <-
        result$second_stage_coef_draws[[i]]
      residual_draws[[draw_index + i]] <- result$residual_draws[[i]]
    }
    cost_draws[success] <- result$cost
    first_stage_diagnostics[[success]] <- result$first_stage
    bootstrap_adjustments[[success]] <- result$adjustment_log
    outer_draw_summary[[success]] <- tibble(
      attempt = attempt,
      n_inner_success = result$n_inner_success
    )
    inner_failures <- bind_rows(inner_failures, result$inner_failures)
  }
  
  if (success == 0) stop("No successful bootstrap draws for ", coverage)
  if (success < n_success) {
    warning("Only ", success, " successful draws obtained for ", coverage)
  }
  
  progress_message(
    "Worker", worker_id,
    "- finished with", success, "successful draws and",
    nrow(failures), "failures"
  )
  
  theta_mat <- do.call(rbind, theta_draws)
  colnames(theta_mat) <- names(point_ts$theta)
  theta_vcov <- cov(theta_mat)
  theta_se <- sqrt(diag(theta_vcov))
  if (length(cost_draws) > 1) {
    theta_se[["cost"]] <- sd(cost_draws)
    theta_vcov["cost", "cost"] <- stats::var(cost_draws)
  }
  
  coef_mat <- do.call(rbind, second_stage_coef_draws)
  colnames(coef_mat) <- names(point_ts$coef_full)
  
  list(
    coverage = coverage,
    settings = list(
    estimator = "classical_ML_FE_MNL",
      bootstrap_type = "nested_individual_then_site_mean_utility",
      n_outer_success_target = n_success,
      n_inner_per_outer = n_inner,
      max_attempts = max_attempts,
      n_outer_success = success,
      n_theta_draws = nrow(theta_mat),
      n_failures = nrow(failures),
      n_inner_failures = nrow(inner_failures),
      seed = worker_seed,
      worker_id = worker_id,
      first_stage_optimizer = "stats::optim_BFGS_with_analytic_gradient",
      ml_max_iterations = ml_max_iterations,
      bootstrap_n_workers = 1,
      resampling_database = if (coverage == "full") paths$full_database else paths$agg_database,
      adjustment = "after_each_individual_resample_reassign_home_trip_to_never_chosen_sites",
      site_resampling = "after_first_stage_resample_site_mean_utilities_with_replacement"
    ),
    point = list(
      first_stage_model_path = point_model_path,
      theta = point_ts$theta,
      second_stage_coefficients = point_ts$coef_full,
      cost = point_ts$theta[["cost"]],
      residuals = point_ts$residuals,
      site_order = point_ts$site_order
    ),
    bootstrap = list(
      theta_draws = theta_mat,
      theta_vcov = theta_vcov,
      theta_se = theta_se,
      second_stage_coef_draws = coef_mat,
      second_stage_vcov = cov(coef_mat),
      second_stage_se = sqrt(diag(cov(coef_mat))),
      cost_draws = cost_draws,
      residual_draws = residual_draws,
      first_stage_diagnostics = first_stage_diagnostics,
      bootstrap_adjustments = bootstrap_adjustments,
      outer_draw_summary = bind_rows(outer_draw_summary),
      failures = failures,
      inner_failures = inner_failures
    )
  )
}


split_integer <- function(total, n_parts) {
  base <- rep(floor(total / n_parts), n_parts)
  remainder <- total %% n_parts
  if (remainder > 0) base[seq_len(remainder)] <- base[seq_len(remainder)] + 1
  base
}


combine_bootstrap_worker_results <- function(worker_results,
                                             coverage,
                                             n_success,
                                             n_inner,
                                             max_attempts) {
  point <- worker_results[[1]]$point
  
  theta_mat <- do.call(
    rbind,
    lapply(worker_results, function(x) x$bootstrap$theta_draws)
  )
  colnames(theta_mat) <- names(point$theta)
  cost_draws <- unlist(lapply(worker_results, function(x) x$bootstrap$cost_draws))
  theta_vcov <- cov(theta_mat)
  theta_se <- sqrt(diag(theta_vcov))
  if (length(cost_draws) > 1) {
    theta_se[["cost"]] <- sd(cost_draws)
    theta_vcov["cost", "cost"] <- stats::var(cost_draws)
  }
  
  coef_mat <- do.call(
    rbind,
    lapply(worker_results, function(x) x$bootstrap$second_stage_coef_draws)
  )
  colnames(coef_mat) <- names(point$second_stage_coefficients)
  
  failures <- bind_rows(lapply(worker_results, function(x) {
    out <- x$bootstrap$failures
    if (nrow(out) == 0) return(out)
    out$worker_id <- x$settings$worker_id
    out
  }))
  
  inner_failures <- bind_rows(lapply(worker_results, function(x) {
    out <- x$bootstrap$inner_failures
    if (nrow(out) == 0) return(out)
    out$worker_id <- x$settings$worker_id
    out
  }))
  
  list(
    coverage = coverage,
    settings = list(
      estimator = "classical_ML_FE_MNL",
      bootstrap_type = "nested_individual_then_site_mean_utility",
      n_outer_success_target = n_success,
      n_inner_per_outer = n_inner,
      max_attempts = max_attempts,
      n_outer_success = sum(vapply(
        worker_results,
        function(x) x$settings$n_outer_success,
        numeric(1)
      )),
      n_theta_draws = nrow(theta_mat),
      n_failures = nrow(failures),
      n_inner_failures = nrow(inner_failures),
      seed = bootstrap_seed,
      first_stage_optimizer = "stats::optim_BFGS_with_analytic_gradient",
      ml_max_iterations = ml_max_iterations,
      bootstrap_n_workers = length(worker_results),
      resampling_database = if (coverage == "full") paths$full_database else paths$agg_database,
      adjustment = "after_each_individual_resample_reassign_home_trip_to_never_chosen_sites",
      site_resampling = "after_first_stage_resample_site_mean_utilities_with_replacement",
      worker_settings = lapply(worker_results, `[[`, "settings")
    ),
    point = point,
    bootstrap = list(
      theta_draws = theta_mat,
      theta_vcov = theta_vcov,
      theta_se = theta_se,
      second_stage_coef_draws = coef_mat,
      second_stage_vcov = cov(coef_mat),
      second_stage_se = sqrt(diag(cov(coef_mat))),
      cost_draws = cost_draws,
      residual_draws = unlist(
        lapply(worker_results, function(x) x$bootstrap$residual_draws),
        recursive = FALSE
      ),
      first_stage_diagnostics = unlist(
        lapply(worker_results, function(x) x$bootstrap$first_stage_diagnostics),
        recursive = FALSE
      ),
      bootstrap_adjustments = unlist(
        lapply(worker_results, function(x) x$bootstrap$bootstrap_adjustments),
        recursive = FALSE
      ),
      outer_draw_summary = bind_rows(lapply(
        worker_results,
        function(x) x$bootstrap$outer_draw_summary
      )),
      failures = failures,
      inner_failures = inner_failures,
      worker_results = worker_results
    )
  )
}


bootstrap_two_stage <- function(coverage = c("full", "aggregated"),
                                n_success = n_boot_outer,
                                n_inner = n_boot_inner,
                                max_attempts = max_attempts,
                                n_workers = bootstrap_n_workers) {
  coverage <- match.arg(coverage)
  n_workers <- min(max(1, as.integer(n_workers)), 12, n_success)
  
  if (n_workers == 1) {
    return(
      bootstrap_two_stage_worker(
        coverage = coverage,
        n_success = n_success,
        n_inner = n_inner,
        max_attempts = max_attempts,
        worker_id = 1,
        worker_seed = bootstrap_seed
      )
    )
  }
  
  success_chunks <- split_integer(n_success, n_workers)
  attempt_chunks <- pmax(
    success_chunks,
    ceiling(max_attempts * success_chunks / n_success)
  )
  worker_seeds <- bootstrap_seed + seq_len(n_workers) * 10000 +
    ifelse(coverage == "full", 0, 500000)
  
  worker_args <- Map(
    function(worker_id, worker_success, worker_attempts, worker_seed) {
      list(
        coverage = coverage,
        n_success = worker_success,
        n_inner = n_inner,
        max_attempts = worker_attempts,
        worker_id = worker_id,
        worker_seed = worker_seed
      )
    },
    worker_id = seq_len(n_workers),
    worker_success = success_chunks,
    worker_attempts = attempt_chunks,
    worker_seed = worker_seeds
  )
  
  progress_message("Starting parallel nested classical ML two-stage bootstrap:", coverage)
  progress_message("Target successful outer draws:", n_success)
  progress_message("Inner site draws per outer draw:", n_inner)
  progress_message("Maximum attempts:", max_attempts)
  progress_message("Workers:", n_workers)
  progress_message("First-stage optimizer: stats::optim BFGS with analytic gradient")
  
  cl <- parallel::makeCluster(n_workers, outfile = "")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterEvalQ(cl, {
    library(tidyverse)
    library(openxlsx)
    NULL
  })
  
  parallel::clusterExport(
    cl,
    varlist = c(
      "read_estimation_database", "read_sites", "extract_fe_components",
      "second_stage_formula", "fit_second_stage_from_fe",
      "fit_second_stage_from_fe_model", "fit_second_stage_from_site_table",
      "resample_second_stage_sites",
      "fit_first_stage_ml",
      "bootstrap_wide_by_respondent", "get_home_site_by_row",
      "ensure_all_alternatives_chosen", "bootstrap_two_stage_worker",
      "progress_message",
      "paths", "output_dir", "bootstrap_seed",
      "ml_max_iterations", "n_boot_inner"
    ),
    envir = .GlobalEnv
  )
  
  worker_results <- parallel::parLapply(cl, worker_args, function(args) {
    do.call(bootstrap_two_stage_worker, args)
  })
  
  progress_message("All workers finished for", coverage, "- combining draws")
  
  combine_bootstrap_worker_results(
    worker_results = worker_results,
    coverage = coverage,
    n_success = n_success,
    n_inner = n_inner,
    max_attempts = max_attempts
  )
}


# ===========================================================
# 6. Compact coefficient tables
# ===========================================================

make_second_stage_table <- function(results) {
  tibble(
    term = names(results$point$theta),
    estimate = as.numeric(results$point$theta),
    se_bootstrap = as.numeric(results$bootstrap$theta_se)
  )
}


# ===========================================================
# 7. Run full and aggregated bootstrap
# ===========================================================

full_results <- bootstrap_two_stage(
  coverage = "full",
  n_success = n_boot_outer,
  n_inner = n_boot_inner,
  max_attempts = max_attempts
)
  
saveRDS(
  full_results,
  file.path(output_dir, "bootstrap_2stage_full_results.rds")
)
  
aggregated_results <- bootstrap_two_stage(
  coverage = "aggregated",
  n_success = n_boot_outer,
  n_inner = n_boot_inner,
  max_attempts = max_attempts
)
  
saveRDS(
  aggregated_results,
  file.path(output_dir, "bootstrap_2stage_aggregated_results.rds")
)
  
summary_tables <- list(
  full_theta = make_second_stage_table(full_results),
  aggregated_theta = make_second_stage_table(aggregated_results)
)
  
saveRDS(
  summary_tables,
  file.path(output_dir, "bootstrap_2stage_summary_tables.rds")
)
  
write.xlsx(
  summary_tables,
  file = file.path(output_dir, "bootstrap_2stage_summary_tables.xlsx"),
  overwrite = TRUE
)
  
cat("\nBootstrap outputs saved in:\n")
cat(normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n")
