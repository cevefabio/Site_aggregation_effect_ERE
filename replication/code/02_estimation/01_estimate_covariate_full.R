############################################################
## Conditional logit with the complete choice set
############################################################

# Unit of observation: respondent-choice occasion.
# Dependent variable: recreation alternative chosen on that occasion.
# Choice set: stay home and all 176 recreation sites.
# Standard errors: Apollo robust standard errors, clustered by respondent.

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

apollo_initialise()

apollo_control <- list(
  modelName = "EC_model3_reduced",
  modelDescr = paste(
    "Conditional logit with travel cost, site types,",
    "coastal attributes, and amenities"
  ),
  indivID = "respid",
  outputDirectory = file.path(
    "output", "models", "covariate_full_adjusted"
  )
)
dir.create(apollo_control$outputDirectory, recursive = TRUE, showWarnings = FALSE)


############################################################
## Parameters and reference categories
############################################################

apollo_beta <- c(
  beta_tc = -0.067,
  ASC_home = 4.50660,
  ASC_nohome = 0,
  beta_coast = 0,
  beta_city = 0,
  beta_trail = 0,
  beta_halea = 0,
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
  beta_coral = 0,
  beta_play = 0,
  beta_sport = 0
)

# ASC_nohome is fixed at zero. Site-type coefficients are therefore relative
# to other non-coastal site types (farms, golf courses, and miscellaneous
# recreation sites), as described in Table 1 of the paper.
apollo_fixed <- "ASC_nohome"

apollo_inputs <- apollo_validateInputs()
apollo_inputs$site_ids <- site_ids


############################################################
## Choice probabilities
############################################################

# Apollo requires the likelihood to be written as a function. The utility
# equation itself is kept here, rather than hidden in a general-purpose helper,
# so that the empirical specification can be inspected term by term.
apollo_probabilities <- function(
    apollo_beta,
    apollo_inputs,
    functionality = "estimate") {

  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  probabilities <- list()
  utility <- list()

  utility[["home"]] <- ASC_home + beta_tc * cost_0

  for (j in 2:length(apollo_inputs$site_ids)) {
    site <- apollo_inputs$site_ids[j]

    utility[[paste0("site_", site)]] <-
      beta_tc * get(paste0("cost_", site)) +
      beta_coast * get(paste0("coastal_", site)) +
      beta_city * get(paste0("citypark_", site)) +
      beta_trail * get(paste0("trailall_", site)) +
      beta_halea * get(paste0("haleakala_", site)) +
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
      beta_play * get(paste0("playground_", site)) +
      beta_sport * get(paste0("sports_", site)) +
      ASC_nohome
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
