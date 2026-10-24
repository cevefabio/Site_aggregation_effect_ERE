############################################################
## Welfare-calculation formulas and data preparation
############################################################

# This file defines the small set of operations repeated across the eight
# welfare calculations. It does not estimate models or run simulations.


read_welfare_data <- function(coverage = c("full", "aggregated")) {
  coverage <- match.arg(coverage)

  population <- openxlsx::read.xlsx(
    file.path("data", "external", "gridpop_zipcode3.xlsx")
  )
  welfare_data <- as.data.frame(
    readRDS(file.path("data", "processed", "data_for_post_estimation.rds"))
  )
  events <- openxlsx::read.xlsx(
    file.path("data", "external", "BWevents.xlsx")
  )

  if (coverage == "full") {
    sites <- openxlsx::read.xlsx(
      file.path("data", "external", "sites.xlsx")
    )
  } else {
    sites <- openxlsx::read.xlsx(
      file.path("data", "external", "sites_aggregated.xlsx")
    )
    sites$oor <- as.integer(sites$siteID == 999)
  }

  closures <- lapply(events$closed_sites, function(closed_sites) {
    suppressWarnings(
      as.numeric(strsplit(as.character(closed_sites), ", ", fixed = TRUE)[[1]])
    )
  })

  all_coastal_sites <- sites$siteID[sites$coastal == 1]
  closures <- lapply(closures, function(closed_sites) {
    if (any(is.na(closed_sites))) all_coastal_sites else closed_sites
  })

  if (coverage == "aggregated") {
    # Retain home and all coastal alternatives as separate rows.
    retained_rows <- welfare_data$siteID %in% sites$siteID
    coastal_and_home <- welfare_data[retained_rows, ]
    non_coastal <- welfare_data[!retained_rows, ]

    # Construct one OOR alternative per population grid cell. The original
    # workflow used the first non-coastal row only as a row template and then
    # set every site characteristic and travel cost to zero.
    oor <- non_coastal[!duplicated(non_coastal$id1km), ]
    oor$siteID <- 999
    oor$name <- "OOR"

    first_attribute <- match("beach", names(oor))
    last_attribute <- match("cost", names(oor))
    if (is.na(first_attribute) || is.na(last_attribute)) {
      stop("Could not identify the OOR columns that must be set to zero.")
    }
    oor[, first_attribute:last_attribute] <- 0

    welfare_data <- rbind(coastal_and_home, oor)
    welfare_data <- welfare_data[
      order(welfare_data$id1km, welfare_data$siteID),
    ]
    welfare_data$oor <- as.integer(welfare_data$siteID == 999)
  }

  alternatives_per_grid <- table(welfare_data$id1km)
  if (any(alternatives_per_grid != nrow(sites))) {
    stop("Some population grid cells do not contain the complete choice set.")
  }

  list(
    sites = sites,
    x = welfare_data,
    population = population,
    events = events,
    closures = closures
  )
}


build_covariate_matrix <- function(welfare_data, coverage) {
  if (coverage == "full") {
    X <- cbind(
      beta_tc = welfare_data$cost,
      ASC_home = welfare_data$home,
      beta_coast = welfare_data$coastal,
      beta_city = welfare_data$citypark,
      beta_trail = welfare_data$trailall,
      beta_halea = welfare_data$haleakala,
      beta_molok = welfare_data$molokini,
      beta_parking = welfare_data$parking,
      beta_showers = welfare_data$showers,
      beta_lifeg = welfare_data$lifeguard,
      beta_rock = welfare_data$rock,
      beta_manmade = welfare_data$manmade,
      beta_medium = welfare_data$beachmedium,
      beta_large = welfare_data$beachlarge,
      beta_surf = welfare_data$surf,
      beta_swim = welfare_data$swim,
      beta_snork = welfare_data$snork,
      beta_fish = welfare_data$res_fish_bio,
      beta_coral = welfare_data$PNAS,
      beta_play = welfare_data$playground,
      beta_sport = welfare_data$sports
    )
  } else {
    X <- cbind(
      beta_tc = welfare_data$cost,
      ASC_home = welfare_data$home,
      ASC_OOR = welfare_data$oor,
      beta_molok = welfare_data$molokini,
      beta_parking = welfare_data$parking,
      beta_showers = welfare_data$showers,
      beta_lifeg = welfare_data$lifeguard,
      beta_rock = welfare_data$rock,
      beta_manmade = welfare_data$manmade,
      beta_medium = welfare_data$beachmedium,
      beta_large = welfare_data$beachlarge,
      beta_surf = welfare_data$surf,
      beta_swim = welfare_data$swim,
      beta_snork = welfare_data$snork,
      beta_fish = welfare_data$res_fish_bio,
      beta_coral = welfare_data$PNAS
    )
  }

  X[is.na(X)] <- 0
  X
}


build_second_stage_matrix <- function(welfare_data, coverage) {
  if (coverage == "full") {
    X <- cbind(
      cost = welfare_data$cost,
      home = welfare_data$home,
      coastal = welfare_data$coastal,
      citypark = welfare_data$citypark,
      trailall = welfare_data$trailall,
      haleakala = welfare_data$haleakala,
      molokini = welfare_data$molokini,
      parking = welfare_data$parking,
      showers = welfare_data$showers,
      lifeguard = welfare_data$lifeguard,
      rock = welfare_data$rock,
      manmade = welfare_data$manmade,
      beachmedium = welfare_data$beachmedium,
      beachlarge = welfare_data$beachlarge,
      surf = welfare_data$surf,
      swim = welfare_data$swim,
      snork = welfare_data$snork,
      res_fish_bio = welfare_data$res_fish_bio,
      PNAS = welfare_data$PNAS,
      playground = welfare_data$playground,
      sports = welfare_data$sports
    )
  } else {
    X <- cbind(
      cost = welfare_data$cost,
      home = welfare_data$home,
      oor = welfare_data$oor,
      molokini = welfare_data$molokini,
      parking = welfare_data$parking,
      showers = welfare_data$showers,
      lifeguard = welfare_data$lifeguard,
      rock = welfare_data$rock,
      manmade = welfare_data$manmade,
      beachmedium = welfare_data$beachmedium,
      beachlarge = welfare_data$beachlarge,
      surf = welfare_data$surf,
      swim = welfare_data$swim,
      snork = welfare_data$snork,
      res_fish_bio = welfare_data$res_fish_bio,
      PNAS = welfare_data$PNAS
    )
  }

  X[is.na(X)] <- 0
  X
}


site_dummy_matrix <- function(site_id) {
  site_factor <- factor(site_id)
  dummy_matrix <- diag(nlevels(site_factor))[site_factor, , drop = FALSE]
  colnames(dummy_matrix) <- paste0("ASC_", levels(site_factor))
  dummy_matrix
}


second_stage_from_fixed_effects <- function(
    first_stage_parameters,
    sites,
    coverage) {

  cost_position <- length(first_stage_parameters)
  sites$FE <- as.numeric(first_stage_parameters[-cost_position])

  if (coverage == "full") {
    second_stage_formula <- FE ~ home + coastal + citypark + trailall +
      haleakala + molokini + parking + showers +
      lifeguard + rock + manmade + beachmedium + beachlarge +
      surf + swim + snork + res_fish_bio + PNAS +
      playground + sports
  } else {
    sites$oor <- as.integer(sites$siteID == 999)
    second_stage_formula <- FE ~ home + oor + molokini +
      parking + showers + lifeguard + rock + manmade +
      beachmedium + beachlarge + surf + swim + snork +
      res_fish_bio + PNAS
  }

  second_stage <- lm(second_stage_formula, data = sites)

  list(
    theta = c(
      cost = unname(first_stage_parameters[cost_position]),
      coef(second_stage)[-1]
    ),
    residuals = resid(second_stage)
  )
}


total_site_closure_loss <- function(
    beta,
    X,
    site_id,
    grid_id,
    population,
    closures,
    events,
    cost_parameter) {

  systematic_utility <- drop(X %*% beta)
  utility_numerator <- exp(systematic_utility)
  utility_denominator <- as.numeric(
    rowsum(utility_numerator, grid_id, reorder = FALSE)
  )

  grid_order <- unique(grid_id)
  population_weight <- population$totpop18plus[
    match(grid_order, population$id1km)
  ]

  five_year_loss <- 0

  for (event_number in seq_along(closures)) {
    closed_sites <- closures[[event_number]]
    closed_numerator <- as.numeric(
      rowsum(
        utility_numerator * (site_id %in% closed_sites),
        grid_id,
        reorder = FALSE
      )
    )

    probability_closed <- closed_numerator / utility_denominator
    willingness_to_pay <-
      log1p(-probability_closed) / beta[cost_parameter]

    five_year_loss <- five_year_loss + sum(
      willingness_to_pay * population_weight * events$days[event_number],
      na.rm = TRUE
    )
  }

  five_year_loss / 1e6
}


total_water_restriction_loss <- function(
    beta,
    X,
    water_columns,
    site_id,
    grid_id,
    population,
    closures,
    events,
    cost_parameter,
    site_residuals = NULL) {

  baseline_utility <- drop(X %*% beta)

  if (!is.null(site_residuals)) {
    number_of_grids <- nrow(X) / length(site_residuals)
    baseline_utility <- baseline_utility +
      rep(site_residuals, number_of_grids)
  }

  baseline_denominator <- as.numeric(
    rowsum(exp(baseline_utility), grid_id, reorder = FALSE)
  )
  baseline_logsum <- log(baseline_denominator)

  grid_order <- unique(grid_id)
  population_weight <- population$totpop18plus[
    match(grid_order, population$id1km)
  ]

  five_year_change <- 0

  for (event_number in seq_along(closures)) {
    closed_sites <- closures[[event_number]]
    affected_rows <- site_id %in% closed_sites

    scenario_utility <- baseline_utility
    scenario_utility[affected_rows] <-
      scenario_utility[affected_rows] -
      drop(
        X[affected_rows, water_columns, drop = FALSE] %*%
          beta[water_columns]
      )

    scenario_denominator <- as.numeric(
      rowsum(exp(scenario_utility), grid_id, reorder = FALSE)
    )

    willingness_to_pay <- -(
      log(scenario_denominator) - baseline_logsum
    ) / beta[cost_parameter]

    five_year_change <- five_year_change + sum(
      willingness_to_pay * population_weight * events$days[event_number],
      na.rm = TRUE
    )
  }

  five_year_change / 1e6
}


summarize_welfare_draws <- function(point, draws) {
  list(
    point = point,
    draws = draws,
    mean = mean(draws),
    ci_lower = unname(quantile(draws, 0.025)),
    ci_upper = unname(quantile(draws, 0.975))
  )
}
