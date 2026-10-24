############################################################
## Conditional logit with non-coastal sites aggregated into OOR
############################################################

# Unit of observation: respondent-choice occasion.
# Dependent variable: recreation alternative chosen on that occasion.
# Choice set: stay home, 94 coastal alternatives, and one aggregate Other
# Outdoor Recreation (OOR) alternative.
# Standard errors: Apollo robust standard errors, clustered by respondent.

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

apollo_initialise()

apollo_control <- list(
  modelName = "MNL_cov_aggregated_reduced_adj",
  modelDescr = paste(
    "Conditional logit with coastal attributes and non-coastal sites",
    "aggregated into OOR"
  ),
  indivID = "respid",
  outputDirectory = file.path(
    "output", "models", "covariate_aggregated_adjusted"
  )
)
dir.create(apollo_control$outputDirectory, recursive = TRUE, showWarnings = FALSE)


############################################################
## Parameters and reference category
############################################################

apollo_beta <- c(
  beta_tc = -0.067,
  ASC_home = 4.50660,
  ASC_OOR = 0,
  ASC_nohome = 0,
  beta_molok = 0,
  beta_parking = 0,
  beta_showers = 0,
  beta_lifeg = 0,
  beta_rock = 0,
  beta_manmade = 0,
  beta_medium = 0,
  beta_large = 0,
  beta_surf = 0,
  beta_swim = 0,
  beta_snork = 0,
  beta_fish = 0,
  beta_coral = 0
)

# ASC_nohome is fixed at zero, making coastal sites the reference category.
apollo_fixed <- "ASC_nohome"

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

  utility[["home"]] <- ASC_home + beta_tc * cost_0

  # Coastal alternatives occupy the rows between home and OOR in the site
  # inventory. Their utility equation is visible here term by term.
  for (j in 2:(length(apollo_inputs$site_ids) - 1)) {
    site <- apollo_inputs$site_ids[j]

    utility[[paste0("site_", site)]] <-
      beta_tc * get(paste0("cost_", site)) +
      beta_molok * get(paste0("molokini_", site)) +
      beta_parking * get(paste0("parking_", site)) +
      beta_showers * get(paste0("showers_", site)) +
      beta_lifeg * get(paste0("lifeguard_", site)) +
      beta_rock * get(paste0("rock_", site)) +
      beta_manmade * get(paste0("manmade_", site)) +
      beta_medium * get(paste0("beachmedium_", site)) +
      beta_large * get(paste0("beachlarge_", site)) +
      beta_surf * get(paste0("surf_", site)) +
      beta_swim * get(paste0("swim_", site)) +
      beta_snork * get(paste0("snork_", site)) +
      beta_fish * get(paste0("res_fish_bio_", site)) +
      beta_coral * get(paste0("PNAS_", site)) +
      ASC_nohome
  }

  # The OOR alternative is represented only by its mean utility (ASC). Its
  # travel cost and observed attributes are set to zero in data preparation.
  utility[["OOR"]] <- ASC_OOR + beta_tc * cost_999

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
