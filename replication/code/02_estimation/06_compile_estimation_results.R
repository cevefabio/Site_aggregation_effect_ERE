############################################################
#### COMPILE PAPER MODEL TABLES                         ####
############################################################

rm(list = ls())

library(tidyverse)
library(openxlsx)

tables_dir <- file.path("output", "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

model_paths <- list(
  mnl_full = file.path(
    "output", "models", "covariate_full_adjusted",
    "EC_model3_reduced_model.rds"
  ),
  mnl_aggregated = file.path(
    "output", "models", "covariate_aggregated_adjusted",
    "MNL_cov_aggregated_reduced_adj_model.rds"
  ),
  two_stage_full = file.path("output", "tables", "results_2stage_all.xlsx"),
  two_stage_aggregated = file.path("output", "tables", "results_2stage_agg.xlsx")
)

read_apollo_table <- function(path, model_label) {
  if (!file.exists(path)) {
    warning("Missing model file: ", path)
    return(tibble())
  }

  model <- readRDS(path)
  estimates <- model$estimate

  se <- if (!is.null(model$robse) && any(is.finite(model$robse))) {
    model$robse[names(estimates)]
  } else if (!is.null(model$se)) {
    model$se[names(estimates)]
  } else {
    rep(NA_real_, length(estimates))
  }

  tibble(
    model = model_label,
    term = names(estimates),
    estimate = as.numeric(estimates),
    se = as.numeric(se),
    t_value = estimate / se,
    p_value = 2 * pnorm(-abs(t_value))
  )
}

read_two_stage_table <- function(path, model_label) {
  if (!file.exists(path)) {
    warning("Missing two-stage table: ", path)
    return(tibble())
  }

  read.xlsx(path) |>
    as_tibble() |>
    mutate(model = model_label, .before = 1)
}

mnl_full <- read_apollo_table(model_paths$mnl_full, "MNL full")
mnl_aggregated <- read_apollo_table(model_paths$mnl_aggregated, "MNL aggregated")
two_stage_full <- read_two_stage_table(model_paths$two_stage_full, "Two-stage full")
two_stage_aggregated <- read_two_stage_table(
  model_paths$two_stage_aggregated,
  "Two-stage aggregated"
)

workbook <- list(
  mnl_full = mnl_full,
  mnl_aggregated = mnl_aggregated,
  two_stage_full = two_stage_full,
  two_stage_aggregated = two_stage_aggregated,
  all_models = bind_rows(
    mnl_full,
    mnl_aggregated,
    two_stage_full,
    two_stage_aggregated
  )
)

write.xlsx(
  workbook,
  file = file.path(tables_dir, "paper_model_estimates.xlsx"),
  overwrite = TRUE
)

cat("\nSaved compiled model workbook:\n")
cat(normalizePath(file.path(tables_dir, "paper_model_estimates.xlsx"),
                  winslash = "/", mustWork = FALSE), "\n")
