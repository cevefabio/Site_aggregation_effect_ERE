############################################################
## First-stage fixed-effects model with the complete choice set
############################################################

# This is the first stage of the Murdock (2006) estimator. Utility contains a
# common travel-cost coefficient and one alternative-specific constant for each
# site. The ASC for site 1 is the normalization used in the original analysis.

rm(list = ls())

library(apollo)
library(openxlsx)

data_file <- file.path(
  "data", "processed", "HW_wide_full_adjusted.rds"
)
site_file <- file.path("data", "external", "sites.xlsx")

if (!file.exists(data_file) || !file.exists(site_file)) {
  stop("Run the data-preparation scripts before estimating this model.")
}

database <- readRDS(data_file)
site_ids <- read.xlsx(site_file)$siteID

if (site_ids[1] != 0) {
  stop("The first alternative must be the stay-at-home option (siteID 0).")
}

# The adjusted dataset should contain at least one observed choice for every
# alternative. This is required to estimate the full set of site fixed effects.
unchosen_sites <- setdiff(site_ids, unique(database$choice))
if (length(unchosen_sites) > 0) {
  stop(
    "The adjusted dataset still contains unchosen alternatives: ",
    paste(unchosen_sites, collapse = ", ")
  )
}

apollo_initialise()

apollo_control <- list(
  modelName = "FE-extended_adj",
  modelDescr = paste(
    "First-stage conditional logit with one fixed effect per alternative",
    "and a common travel-cost coefficient"
  ),
  indivID = "respid",
  outputDirectory = file.path("output", "models", "FE_full_adjusted"),
  nCores = 10
)
dir.create(apollo_control$outputDirectory, recursive = TRUE, showWarnings = FALSE)

apollo_beta <- setNames(
  rep(0, length(site_ids)),
  paste0("ASC_", site_ids)
)
apollo_beta <- c(apollo_beta, cost = 0)

# Fix site 1 (H.A. Baldwin Beach Park) at zero. A different normalization would
# shift every ASC and the second-stage intercept but should not be introduced
# in a replication of the reported estimates.
apollo_fixed <- "ASC_1"

apollo_inputs <- apollo_validateInputs()
apollo_inputs$site_ids <- site_ids


############################################################
## Choice probabilities
############################################################

apollo_probabilities <- function(
    apollo_beta,
    apollo_inputs,
    functionality = "estimate") {

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  probabilities <- list()
  utility <- list()

  for (site in apollo_inputs$site_ids) {
    utility[[paste0("site_", site)]] <-
      cost * get(paste0("cost_", site)) +
      get(paste0("ASC_", site))
  }

  mnl_settings <- list(
    alternatives = setNames(apollo_inputs$site_ids, names(utility)),
    choiceVar = choice,
    utilities = utility
  )

  probabilities[["model"]] <- apollo_mnl(mnl_settings, functionality)
  probabilities <- apollo_panelProd(
    probabilities,
    apollo_inputs,
    functionality
  )
  apollo_prepareProb(probabilities, apollo_inputs, functionality)
}


############################################################
## Estimation and saved output
############################################################

model <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs
)

apollo_modelOutput(model)
apollo_saveOutput(model)

saveRDS(
  model,
  file.path(
    apollo_control$outputDirectory,
    paste0(apollo_control$modelName, "_model.rds")
  )
)
