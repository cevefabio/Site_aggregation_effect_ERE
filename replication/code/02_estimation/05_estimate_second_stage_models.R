############################################################
## Estimate the second-stage site-characteristic regressions
############################################################

# Run this script after:
#   1. the two fixed-effects choice models; and
#   2. the two-stage bootstrap.
#
# The first-stage site fixed effects are the dependent variable. The bootstrap
# output is used to correct the standard errors for first-stage uncertainty.

rm(list = ls())

library(openxlsx)

full_model_file <- file.path(
  "output", "models", "FE_full_adjusted", "FE-extended_adj_model.rds"
)
aggregated_model_file <- file.path(
  "output", "models", "FE_aggregated_adjusted",
  "FE-aggregated_model.rds"
)
full_bootstrap_file <- file.path(
  "output", "bootstrap", "two_stage_murdock",
  "bootstrap_2stage_full_results.rds"
)
aggregated_bootstrap_file <- file.path(
  "output", "bootstrap", "two_stage_murdock",
  "bootstrap_2stage_aggregated_results.rds"
)

required_files <- c(
  full_model_file,
  aggregated_model_file,
  full_bootstrap_file,
  aggregated_bootstrap_file,
  file.path("data", "external", "sites.xlsx"),
  file.path("data", "external", "sites_aggregated.xlsx")
)

if (!all(file.exists(required_files))) {
  stop(
    "Missing required file(s): ",
    paste(required_files[!file.exists(required_files)], collapse = ", ")
  )
}

tables_dir <- file.path("output", "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)


############################################################
## 1. All-sites specification
############################################################

full_first_stage <- readRDS(full_model_file)
full_bootstrap <- readRDS(full_bootstrap_file)
full_sites <- read.xlsx(file.path("data", "external", "sites.xlsx"))

full_asc_names <- paste0("ASC_", full_sites$siteID)
missing_full_asc <- setdiff(full_asc_names, names(full_first_stage$estimate))
if (length(missing_full_asc) > 0) {
  stop("Missing full-model fixed effects: ", paste(missing_full_asc, collapse = ", "))
}

full_sites$FE <- as.numeric(full_first_stage$estimate[full_asc_names])

full_second_stage_formula <- FE ~ home + coastal + citypark + trailall +
  haleakala + molokini + parking + showers +
  lifeguard + rock + manmade + beachmedium + beachlarge +
  surf + swim + snork + res_fish_bio + PNAS +
  playground + sports

model1 <- lm(full_second_stage_formula, data = full_sites)
save(
  model1,
  file = file.path(
    "output", "models", "FE_full_adjusted", "2stage_extended_new.mod"
  )
)

full_results <- as.data.frame(summary(model1)$coefficients)
full_results$term <- rownames(full_results)
rownames(full_results) <- NULL
names(full_results) <- c("estimate", "se_ols", "t_ols", "p_ols", "term")
full_results <- full_results[
  , c("term", "estimate", "se_ols", "t_ols", "p_ols")
]

full_X <- model.matrix(full_second_stage_formula, data = full_sites)
full_df <- nrow(full_X) - ncol(full_X)
full_XtX_inverse <- solve(crossprod(full_X))

full_residual_variances <- sapply(
  full_bootstrap$bootstrap$residual_draws,
  function(residuals) sum(as.numeric(residuals)^2) / full_df
)

full_vcov_within <- mean(full_residual_variances) * full_XtX_inverse
full_vcov_between <- cov(
  full_bootstrap$bootstrap$second_stage_coef_draws
)
full_vcov_between <- full_vcov_between[
  colnames(full_X), colnames(full_X)
]
full_vcov_corrected <- full_vcov_within + full_vcov_between

full_results$se_boot_between <-
  sqrt(diag(full_vcov_between))[full_results$term]
full_results$se_corrected <-
  sqrt(diag(full_vcov_corrected))[full_results$term]
full_results$t_corrected <-
  full_results$estimate / full_results$se_corrected
full_results$p_corrected <-
  2 * pnorm(-abs(full_results$t_corrected))

full_cost_estimate <- unname(full_first_stage$estimate[["cost"]])
full_cost_se <- sd(full_bootstrap$bootstrap$theta_draws[, "cost"])
full_cost_row <- data.frame(
  term = "cost",
  estimate = full_cost_estimate,
  se_ols = NA_real_,
  t_ols = NA_real_,
  p_ols = NA_real_,
  se_boot_between = full_cost_se,
  se_corrected = full_cost_se,
  t_corrected = full_cost_estimate / full_cost_se,
  p_corrected = 2 * pnorm(-abs(full_cost_estimate / full_cost_se))
)

full_results <- rbind(full_cost_row, full_results)

write.xlsx(
  full_results,
  file.path(tables_dir, "results_2stage_all.xlsx"),
  overwrite = TRUE
)


############################################################
## 2. Aggregated specification
############################################################

aggregated_first_stage <- readRDS(aggregated_model_file)
aggregated_bootstrap <- readRDS(aggregated_bootstrap_file)
aggregated_sites <- read.xlsx(
  file.path("data", "external", "sites_aggregated.xlsx")
)
aggregated_sites$oor <- as.integer(aggregated_sites$siteID == 999)

aggregated_asc_names <- paste0("ASC_", aggregated_sites$siteID)
missing_aggregated_asc <- setdiff(
  aggregated_asc_names,
  names(aggregated_first_stage$estimate)
)
if (length(missing_aggregated_asc) > 0) {
  stop(
    "Missing aggregated-model fixed effects: ",
    paste(missing_aggregated_asc, collapse = ", ")
  )
}

aggregated_sites$FE <- as.numeric(
  aggregated_first_stage$estimate[aggregated_asc_names]
)

aggregated_second_stage_formula <- FE ~ home + oor + molokini +
  parking + showers + lifeguard + rock + manmade +
  beachmedium + beachlarge + surf + swim + snork +
  res_fish_bio + PNAS

model2 <- lm(aggregated_second_stage_formula, data = aggregated_sites)
save(
  model2,
  file = file.path(
    "output", "models", "FE_aggregated_adjusted",
    "2stage_aggregated_adj_new.mod"
  )
)

aggregated_results <- as.data.frame(summary(model2)$coefficients)
aggregated_results$term <- rownames(aggregated_results)
rownames(aggregated_results) <- NULL
names(aggregated_results) <- c(
  "estimate", "se_ols", "t_ols", "p_ols", "term"
)
aggregated_results <- aggregated_results[
  , c("term", "estimate", "se_ols", "t_ols", "p_ols")
]

aggregated_X <- model.matrix(
  aggregated_second_stage_formula,
  data = aggregated_sites
)
aggregated_df <- nrow(aggregated_X) - ncol(aggregated_X)
aggregated_XtX_inverse <- solve(crossprod(aggregated_X))

aggregated_residual_variances <- sapply(
  aggregated_bootstrap$bootstrap$residual_draws,
  function(residuals) sum(as.numeric(residuals)^2) / aggregated_df
)

aggregated_vcov_within <-
  mean(aggregated_residual_variances) * aggregated_XtX_inverse
aggregated_vcov_between <- cov(
  aggregated_bootstrap$bootstrap$second_stage_coef_draws
)
aggregated_vcov_between <- aggregated_vcov_between[
  colnames(aggregated_X), colnames(aggregated_X)
]
aggregated_vcov_corrected <-
  aggregated_vcov_within + aggregated_vcov_between

aggregated_results$se_boot_between <-
  sqrt(diag(aggregated_vcov_between))[aggregated_results$term]
aggregated_results$se_corrected <-
  sqrt(diag(aggregated_vcov_corrected))[aggregated_results$term]
aggregated_results$t_corrected <-
  aggregated_results$estimate / aggregated_results$se_corrected
aggregated_results$p_corrected <-
  2 * pnorm(-abs(aggregated_results$t_corrected))

aggregated_cost_estimate <- unname(
  aggregated_first_stage$estimate[["cost"]]
)
aggregated_cost_se <- sd(
  aggregated_bootstrap$bootstrap$theta_draws[, "cost"]
)
aggregated_cost_row <- data.frame(
  term = "cost",
  estimate = aggregated_cost_estimate,
  se_ols = NA_real_,
  t_ols = NA_real_,
  p_ols = NA_real_,
  se_boot_between = aggregated_cost_se,
  se_corrected = aggregated_cost_se,
  t_corrected = aggregated_cost_estimate / aggregated_cost_se,
  p_corrected = 2 * pnorm(-abs(aggregated_cost_estimate / aggregated_cost_se))
)

aggregated_results <- rbind(aggregated_cost_row, aggregated_results)

write.xlsx(
  aggregated_results,
  file.path(tables_dir, "results_2stage_agg.xlsx"),
  overwrite = TRUE
)

cat("Saved the full and aggregated second-stage results.\n")
