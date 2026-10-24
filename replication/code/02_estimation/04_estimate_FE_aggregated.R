############################################################
## First-stage fixed-effects model with the aggregated choice set
############################################################

# This is the first stage of the Murdock (2006) estimator after non-coastal
# sites have been combined into the OOR alternative. Utility contains a common
# travel-cost coefficient and one alternative-specific constant per option.

rm(list = ls())

library(apollo)
library(openxlsx)

data_file <- file.path(
  "data", "processed", "HW_wide_aggregated_adjusted.rds"
)
site_file <- file.path("data", "external", "sites_aggregated.xlsx")

if (!file.exists(data_file) || !file.exists(site_file)) {
  stop("Run the data-preparation scripts before estimating this model.")
}

database <- readRDS(data_file)
site_ids <- read.xlsx(site_file)$siteID

if (site_ids[1] != 0 || tail(site_ids, 1) != 999) {
  stop("The site inventory must begin with home (0) and end with OOR (999).")
}

unchosen_sites <- setdiff(site_ids, unique(database$choice))
if (length(unchosen_sites) > 0) {
  stop(
    "The adjusted dataset still contains unchosen alternatives: ",
    paste(unchosen_sites, collapse = ", ")
  )
}

apollo_initialise()

apollo_control <- list(
  modelName = "FE-aggregated",
  modelDescr = paste(
    "First-stage conditional logit with one fixed effect per alternative",
    "after aggregation of non-coastal sites"
  ),
  indivID = "respid",
  outputDirectory = file.path(
    "output", "models", "FE_aggregated_adjusted"
  )
)
dir.create(apollo_control$outputDirectory, recursive = TRUE, showWarnings = FALSE)

apollo_beta <- setNames(
  rep(0, length(site_ids)),
  paste0("ASC_", site_ids)
)
apollo_beta <- c(apollo_beta, cost = 0)

# Preserve the normalization used in the reported analysis.
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
