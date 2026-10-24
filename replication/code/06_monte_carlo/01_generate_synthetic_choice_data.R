

# Data generator for the Monte Carlo simulation in Cevenini et al. (2026)
# =============================================================================
# The generator creates simplified repeated recreation choice data from a
# multinomial logit data-generating process. The simulated world has:

# - focal sites, labelled F1, F2, ...
# - peripheral sites, labelled A1, A2, ...

# The A sites are the alternatives that will later be collapsed into one
# "Other Outdoor Recreation" (OOR) alternative. The point of the simulation is
# to compare coefficient-ratio estimates when the researcher keeps all sites
# separate versus when the researcher aggregates A into OOR.

softmax_prob <- function(v) {
  # Convert utilities into multinomial-logit probabilities.
  #
  # The formula is:
  #   p_j = exp(V_j) / sum_k exp(V_k)
  #
  # Subtracting max(v) prevents numerical problems with exp(v). It does not change
  # the probabilities because the same constant is subtracted from every utility.
  v <- v - max(v)
  ev <- exp(v)
  ev / sum(ev)
}


calibrate_A_intercept_fast <- function(base_data, target_share, tol = 1e-5,
                                       max_iter = 200) {
  # Find the alternative-specific intercept for the A group.
  #
  # The user controls share_aggregated_choices, which is the expected share of
  # choices going to sites in A. To hit that target, the generator adds an
  # intercept gamma_A to every A site:
  #
  #   utility_ij = base_utility_ij + gamma_A * 1(j belongs to A)
  #
  # If gamma_A increases, A sites become more attractive and their expected
  # choice share rises. This function uses a contraction mapping algortithm (BLP 1995) 
  # to find the gamma_A that makes the expected A choice share equal to the target_share. (conditional on the covariates)
  
  if (target_share <= 0 || target_share >= 1) {
    stop("share_aggregated_choices must be strictly between 0 and 1.")
  }

  # obs_index gives each long-format row a numeric choice-occasion ID.
  # is_A is 1 for rows corresponding to alternatives that will be aggregated.
  obs_index <- as.integer(factor(base_data$obs_id))
  is_A <- base_data$is_A
  n_obs <- max(obs_index)

  # Wide initial interval for gamma_A. At -20 A is almost never chosen; at +20
  # A is almost always chosen.
  low <- -20
  high <- 20

  expected_A_share <- function(gamma_A) {
    # Compute the expected probability of choosing any A site, averaged over all
    # choice occasions. This uses probabilities, not simulated random choices.
    utility <- base_data$utility_no_A_intercept + gamma_A * is_A

    # Build the logit denominator separately for each choice occasion.
    max_u <- ave(utility, obs_index, FUN = max)
    exp_u <- exp(utility - max_u)
    denom <- rowsum(exp_u, obs_index, reorder = FALSE)[, 1]

    # Build the numerator for the probability of choosing the A group.
    num_A <- rowsum(exp_u * is_A, obs_index, reorder = FALSE)[, 1]
    mean(num_A / denom[seq_len(n_obs)])
  }

  # Actual contraction mapping at work
  for (iter in seq_len(max_iter)) {
    mid <- (low + high) / 2
    p_A <- expected_A_share(mid)
    if (abs(p_A - target_share) < tol) return(mid)
    if (p_A < target_share) low <- mid else high <- mid
  }

  # If the tolerance was not reached, approximate with the midpoint after max_iter steps.
  (low + high) / 2
}

collapse_to_OOR <- function(full_data, site_data) {
  
  # Collapse all A sites into a single OOR row for each choice occasion.
  #
  # Input:
  # - full_data has one row per alternative per choice occasion.
  # - site_data has one row per site.
  #
  # Output:
  # - all focal F sites remain separate;
  # - all A sites are replaced by one row called "OOR";
  # - OOR is marked as chosen if the chosen full-data site was any A site.
  # Keep focal alternatives unchanged.
  
  focal_data <- full_data[full_data$is_A == 0, ]

  # Select the alternatives that will be collapsed.
  A_data <- full_data[full_data$is_A == 1, ]

  # Create one attribute-free OOR row per choice occasion.
  A_choice <- aggregate(choice ~ obs_id + resp_id + choice_id,
                        data = A_data, FUN = sum)
  A_summary <- A_choice
  A_summary$cost <- 0
  A_summary$x1 <- 0
  A_summary$choice <- as.integer(A_summary$choice)

  # Add the site-level fields expected by the estimator.
  A_summary$site_id <- "OOR"
  A_summary$is_A <- 1
  A_summary$utility <- NA_real_

  # Bind focal rows and OOR rows using the same columns.
  keep_cols <- c("obs_id", "resp_id", "choice_id", "site_id", "is_A",
                 "cost", "x1", "utility", "choice")
  aggregated_data <- rbind(
    focal_data[, keep_cols],
    A_summary[, keep_cols]
  )

  # Sort for readability 
  aggregated_data <- aggregated_data[order(aggregated_data$obs_id,
                                           aggregated_data$is_A,
                                           aggregated_data$site_id), ]
  row.names(aggregated_data) <- NULL

  # Create a corresponding site table. This is mostly metadata, but it is useful
  # for checking that the generated data look sensible.
  aggregated_site_data <- rbind(
    site_data[site_data$is_A == 0, ],
    data.frame(
      site_id = "OOR",
      is_A = 1,
      x1 = 0
    )
  )

  list(data = aggregated_data, sites = aggregated_site_data)
}

generate_mc_recreation_data <- function(
    n_sites_total,
    n_sites_aggregated,
    share_aggregated_choices,
    within_A_resp_sd,
    lambda_substitution,
    n_resp,
    n_choices_per_resp,
    beta_cost = -1,
    beta_x1 = 1,
    seed = NULL) {
  # Main synthetic-data generator.
  #
  # Arguments: 
  #
  # n_sites_total:
  #   Total number of recreation alternatives before aggregation.
  #
  # n_sites_aggregated:
  #   Number of peripheral A sites collapsed into OOR.
  #
  # share_aggregated_choices:
  #   Expected share of choices going to A sites before aggregation.
  #
  # within_A_resp_sd:
  #   Standard deviation of the respondent-specific taste for A sites. Larger
  #   values mean the aggregate hides more person-specific heterogeneity.
  #
  # lambda_substitution:
  #   Strength of the link between respondent-specific A taste and focal-site
  #   substitution patterns. If lambda_substitution = 0, A taste is orthogonal
  #   to focal-site choices. If it is positive, respondents with stronger A
  #   taste also have a different coefficient on x1 at focal sites.
  #
  # n_resp and n_choices_per_resp:
  #   Sample-size controls.
  #
  # beta_cost and beta_x1:
  #   True coefficients in the data-generating utility function. The Monte Carlo
  #   evaluates beta_x1 / -beta_cost, which is scale invariant.
  #
  if (!is.null(seed)) set.seed(seed)

  # Basic checks for logically impossible parameter values.
  if (n_sites_aggregated >= n_sites_total) {
    stop("n_sites_aggregated must be smaller than n_sites_total.")
  }

  # Define site labels and group membership.
  n_sites_focal <- n_sites_total - n_sites_aggregated
  focal_ids <- paste0("F", seq_len(n_sites_focal))
  A_ids <- paste0("A", seq_len(n_sites_aggregated))
  site_ids <- c(focal_ids, A_ids)

  # Generate one simple continuous site attribute, x1, from the same
  # distribution for focal and peripheral sites.
  site_data <- data.frame(
    site_id = site_ids,
    is_A = as.integer(site_ids %in% A_ids),
    x1 = rnorm(n_sites_total, mean = 0, sd = 0.5)
  )

  # Build repeated choice occasions.
  # For example, 500 respondents and 8 choices each generate 4,000 occasions.
  obs_grid <- expand.grid(
    choice_id = seq_len(n_choices_per_resp),
    resp_id = seq_len(n_resp)
  )
  obs_grid$obs_id <- seq_len(nrow(obs_grid))
  obs_grid <- obs_grid[order(obs_grid$obs_id), ]

  # Generate respondent-level taste for A. This is the individual-specific
  # attractiveness of the alternatives that will be collapsed into OOR.
  taste_A_mean <- rnorm(n_resp, 0, within_A_resp_sd)

  # Expand to long format: one row per alternative per choice occasion.
  full_data <- merge(obs_grid, site_data, by = NULL)
  full_data <- full_data[order(full_data$obs_id, full_data$site_id), ]

  # Indexes used to attach respondent-level and site-level quantities to rows.
  resp_index <- full_data$resp_id
  site_index <- match(full_data$site_id, site_data$site_id)
  is_A <- full_data$is_A

  # Draw one standard-lognormal travel cost per respondent-site pair. The same
  # cost is used across all repeated choice occasions for that pair.
  respondent_site_cost <- rlnorm(
    n_resp * n_sites_total,
    meanlog = 0,
    sdlog = 1
  )
  respondent_site_index <- (resp_index - 1L) * n_sites_total + site_index
  full_data$cost <- respondent_site_cost[respondent_site_index]

  # The only respondent-specific heterogeneity term is a respondent-level taste
  # for the peripheral alternatives.
  A_shift <- ifelse(is_A == 1, taste_A_mean[resp_index], 0)

  # This is the key substitution channel in the simulation.
  #
  # If lambda_substitution = 0, the respondent-specific taste for A affects only
  # A alternatives. It is orthogonal to focal-site substitution patterns.
  #
  # If lambda_substitution > 0, respondents with stronger A taste also have a
  # different coefficient on x1 at focal sites. Travel-cost sensitivity is not
  # affected by lambda_substitution.
  focal_substitution_shift <- ifelse(
    is_A == 0,
    lambda_substitution * taste_A_mean[resp_index] *
      beta_x1 * full_data$x1,
    0
  )

  # Deterministic utility before the common A intercept is added.
  observed_utility <- beta_cost * full_data$cost + beta_x1 * full_data$x1
  full_data$utility_no_A_intercept <-
    observed_utility +
    focal_substitution_shift +
    A_shift

  # Calibrate the common A intercept to hit the requested expected A share.
  gamma_A <- calibrate_A_intercept_fast(full_data, share_aggregated_choices)
  A_intercept <- gamma_A * is_A
  full_data$utility <- full_data$utility_no_A_intercept + A_intercept

  # Diagnostic decomposition of the utility scale. For each replication, take
  # the variance of each systematic component and the theoretical variance of
  # a standard Gumbel error. Normalizing these standalone variances produces
  # descriptive percentages that sum to 100. Covariances are intentionally not
  # allocated, so this is a scale diagnostic rather than an R-squared measure.
  component_variances <- c(
    observed_utility_V = stats::var(observed_utility),
    respondent_heterogeneity_a = stats::var(A_shift),
    nonseparability_lambda_a_x = stats::var(focal_substitution_shift),
    peripheral_ASC_gamma = stats::var(A_intercept),
    Gumbel_error_epsilon = pi^2 / 6
  )
  utility_scale_decomposition <- data.frame(
    component = names(component_variances),
    variance = unname(component_variances),
    percentage = 100 * unname(component_variances) /
      sum(component_variances),
    row.names = NULL
  )

  # Draw one chosen site per choice occasion from the logit probabilities.
  full_data$choice <- 0L
  obs_split <- split(seq_len(nrow(full_data)), full_data$obs_id)
  for (idx in obs_split) {
    probs <- softmax_prob(full_data$utility[idx])
    chosen_row <- idx[sample.int(length(idx), size = 1, prob = probs)]
    full_data$choice[chosen_row] <- 1L
  }

  # Remove the temporary utility component used only for calibration.
  full_data$utility_no_A_intercept <- NULL
  row.names(full_data) <- NULL

  # Build the corresponding aggregated-OOR dataset.
  collapsed <- collapse_to_OOR(full_data, site_data)

  # Return data, metadata, and the true coefficient ratios.
  list(
    full_data = full_data,
    aggregated_data = collapsed$data,
    site_data = site_data,
    aggregated_site_data = collapsed$sites,
    utility_scale_decomposition = utility_scale_decomposition,
    true_parameters = c(
      beta_cost = beta_cost,
      beta_x1 = beta_x1,
      gamma_A = gamma_A,
      ratio_x1_cost = beta_x1 / (-beta_cost)
    ),
    settings = list(
      n_sites_total = n_sites_total,
      n_sites_aggregated = n_sites_aggregated,
      share_aggregated_choices = share_aggregated_choices,
      within_A_resp_sd = within_A_resp_sd,
      lambda_substitution = lambda_substitution,
      n_resp = n_resp,
      n_choices_per_resp = n_choices_per_resp
    )
  )
}
