############################################################
## Prepare the four estimation datasets
############################################################

# Run this script from the root of the replication package.
# It creates the full and aggregated choice sets, both before and after the
# small adjustment required by the fixed-effects estimator for sites that were
# never chosen in the survey.

rm(list = ls())

choice_file <- file.path("data", "raw", "for_estimation_withbio.dat")
coral_file <- file.path("data", "external", "coralPNAS.csv")
processed_dir <- file.path("data", "processed")
metadata_dir <- file.path("data", "metadata")

required_files <- c(choice_file, coral_file)
if (!all(file.exists(required_files))) {
  stop(
    "Missing input file(s): ",
    paste(required_files[!file.exists(required_files)], collapse = ", ")
  )
}

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)

choice_occasions <- 28
oor_site_id <- 999

# These respondents did not complete the survey. This exclusion reproduces the
# estimation sample used in the paper.
incomplete_respondents <- c(
  327, 341, 345, 346, 348, 350, 352, 355, 362, 366, 367,
  370, 375, 378, 381, 383, 385
)


############################################################
## 1. Read and check the source data
############################################################

choice_data <- read.table(choice_file, sep = "\t", header = TRUE)
coral_data <- read.csv(coral_file)

if (anyDuplicated(choice_data[c("respID", "siteID")])) {
  stop("The respondent-site key is not unique in the choice data.")
}
if (anyDuplicated(coral_data$siteID)) {
  stop("siteID is not unique in coralPNAS.csv.")
}

cat("Raw choice-data rows:", nrow(choice_data), "\n")
cat("Raw respondents:", length(unique(choice_data$respID)), "\n")

choice_data <- choice_data[
  !(choice_data$respID %in% incomplete_respondents),
]

# PNAS coral cover is a site-level variable. The original workflow used a full
# merge. The explicit key below makes that operation auditable.
n_before_merge <- nrow(choice_data)
choice_data <- merge(
  choice_data,
  coral_data,
  by = "siteID",
  all = TRUE,
  sort = FALSE
)

if (nrow(choice_data) != n_before_merge) {
  stop("Merging coral cover changed the number of respondent-site rows.")
}

choice_data <- choice_data[order(choice_data$respID, choice_data$siteID), ]
rownames(choice_data) <- NULL


############################################################
## 2. Reproduce the site-attribute recodes used in estimation
############################################################

# Black Rock beach was classified as a coastal trail in the source inventory.
choice_data$beach[choice_data$siteID == 130] <- 1
choice_data$coast_trail[choice_data$siteID == 130] <- 0
choice_data$res_fish_bio[choice_data$siteID == 130] <- 15.45

# Molokini values used in the empirical specification.
choice_data$coral[choice_data$siteID == 146] <- 95
choice_data$PNAS[is.na(choice_data$PNAS)] <- 0
choice_data$PNAS[choice_data$siteID == 146] <-
  choice_data$PNAS[choice_data$siteID == 146] + 20

choice_data$beach_group2 <- as.character(choice_data$beach_group)
choice_data$beach_group2[is.na(choice_data$beach_group2)] <- "0"

choice_data$coastal <-
  choice_data$beach + choice_data$harbor + choice_data$molokini
choice_data$trailall <-
  choice_data$trailandpark + choice_data$coast_trail
choice_data$manmade <- choice_data$harbor

choice_data$rock[is.na(choice_data$rock)] <- 0

# Beach size categories. The strict inequalities reproduce the original code;
# a beach of exactly 500 metres is therefore the omitted size category.
choice_data$beachlarge <- as.integer(choice_data$length > 500)
choice_data$beachmedium <- as.integer(
  choice_data$length > 200 & choice_data$length < 500
)
choice_data$beachsmall <- as.integer(choice_data$length <= 200)

# Missing ecological measures denote sites for which the characteristic is not
# applicable in this specification, rather than measured zero at coastal sites.
choice_data$coral[is.na(choice_data$coral)] <- 0
choice_data$tot_fish_bio[is.na(choice_data$tot_fish_bio)] <- 0
choice_data$res_fish_bio[is.na(choice_data$res_fish_bio)] <- 0

park_variables <- c(
  "playground", "tennis", "pool", "fountain", "baseball",
  "basketball", "multipurposefield", "park.size"
)
for (variable in park_variables) {
  choice_data[[variable]][is.na(choice_data[[variable]])] <- 0
}

choice_data$sports <-
  choice_data$tennis +
  choice_data$basketball +
  choice_data$multipurposefield
choice_data$sports[choice_data$sports > 1] <- 1

estimation_variables <- c(
  "siteID", "respID", "times", "name", "cost", "home",
  "coastal", "citypark", "trailall", "haleakala", "molokini",
  "parking", "showers", "lifeguard", "rock", "manmade",
  "beachmedium", "beachlarge", "surf", "swim", "snork",
  "res_fish_bio", "PNAS", "playground", "sports", "length"
)

full_unadjusted_long <- choice_data[, estimation_variables]


############################################################
## 3. Aggregate non-coastal recreation into OOR
############################################################

peripheral_row <-
  full_unadjusted_long$coastal == 0 & full_unadjusted_long$home == 0

coastal_and_home <- full_unadjusted_long[!peripheral_row, ]
peripheral_sites <- full_unadjusted_long[peripheral_row, ]

oor_visits <- tapply(
  peripheral_sites$times,
  peripheral_sites$respID,
  sum
)

oor_rows <- peripheral_sites[!duplicated(peripheral_sites$respID), ]
oor_rows$siteID <- oor_site_id
oor_rows$times <- as.numeric(oor_visits[as.character(oor_rows$respID)])
oor_rows$name <- "OOR"

# The aggregate alternative is represented by an alternative-specific
# constant. It therefore carries no observed site attributes or travel cost.
columns_set_to_zero <- setdiff(
  names(oor_rows),
  c("siteID", "respID", "times", "name")
)
oor_rows[, columns_set_to_zero] <- 0

aggregated_unadjusted_long <- rbind(coastal_and_home, oor_rows)
aggregated_unadjusted_long <- aggregated_unadjusted_long[
  order(aggregated_unadjusted_long$respID,
        aggregated_unadjusted_long$siteID),
]
rownames(aggregated_unadjusted_long) <- NULL

# Length is not used in the aggregated specification and was not retained in
# the analysis dataset used for that model.
aggregated_unadjusted_long$length <- NULL


############################################################
## 4. Adjustment for alternatives with zero observed visits
############################################################

assign_one_visit_to_unchosen_sites <- function(data) {
  total_visits <- tapply(data$times, data$siteID, sum)
  unchosen_sites <- as.numeric(names(total_visits)[total_visits == 0])

  adjustment_log <- data.frame(
    siteID = numeric(0),
    respID = numeric(0),
    home_siteID = numeric(0),
    assigned = logical(0)
  )

  for (site in unchosen_sites) {
    candidates <- data[data$siteID == site, ]
    candidates <- candidates[order(candidates$cost), ]

    assigned <- FALSE
    selected_respondent <- NA_real_
    selected_home_site <- NA_real_

    for (respondent in candidates$respID) {
      home_row <- which(data$respID == respondent & data$home == 1)

      if (length(home_row) > 0 && data$times[home_row[1]] > 0) {
        data$times[home_row[1]] <- data$times[home_row[1]] - 1
        data$times[
          data$respID == respondent & data$siteID == site
        ] <- 1

        assigned <- TRUE
        selected_respondent <- respondent
        selected_home_site <- data$siteID[home_row[1]]
        break
      }
    }

    adjustment_log <- rbind(
      adjustment_log,
      data.frame(
        siteID = site,
        respID = selected_respondent,
        home_siteID = selected_home_site,
        assigned = assigned
      )
    )

    if (!assigned) {
      warning("No home visit could be reassigned to site ", site, ".")
    }
  }

  list(data = data, log = adjustment_log)
}

full_adjustment <- assign_one_visit_to_unchosen_sites(full_unadjusted_long)
full_adjusted_long <- full_adjustment$data

aggregated_adjustment <-
  assign_one_visit_to_unchosen_sites(aggregated_unadjusted_long)
aggregated_adjusted_long <- aggregated_adjustment$data


############################################################
## 5. Convert visit counts into repeated choice occasions
############################################################

long_to_wide_choices <- function(data, occasions_per_respondent) {
  visits_by_respondent <- tapply(data$times, data$respID, sum)

  if (any(visits_by_respondent != occasions_per_respondent)) {
    incorrect_ids <- names(
      visits_by_respondent[visits_by_respondent != occasions_per_respondent]
    )
    stop(
      "The following respondents do not have ", occasions_per_respondent,
      " choice occasions: ", paste(incorrect_ids, collapse = ", ")
    )
  }

  site_ids <- unique(data$siteID)
  respondent_ids <- unique(data$respID)
  attribute_names <- names(data)[-(1:4)]

  n_sites <- length(site_ids)
  n_attributes <- length(attribute_names)
  n_respondents <- length(respondent_ids)

  wide_data <- as.data.frame(
    matrix(
      0,
      nrow = n_respondents * occasions_per_respondent,
      ncol = n_sites * n_attributes + 2
    )
  )

  wide_attribute_names <- paste(
    rep(attribute_names, n_sites),
    rep(site_ids, each = n_attributes),
    sep = "_"
  )
  names(wide_data) <- c("respid", "choice", wide_attribute_names)

  wide_data$respid <- rep(
    respondent_ids,
    each = occasions_per_respondent
  )
  wide_data$choice <- unlist(
    mapply(rep, data$siteID, each = data$times)
  )

  for (attribute in attribute_names) {
    values_by_site <- split(data[, attribute], data$siteID)
    destination_columns <- which(
      grepl(paste0("^", attribute, "_"), names(wide_data))
    )

    attribute_matrix <- do.call(cbind, values_by_site)
    attribute_matrix <- attribute_matrix[
      rep(seq_len(nrow(attribute_matrix)),
          each = occasions_per_respondent),
      ,
      drop = FALSE
    ]

    wide_data[, destination_columns] <- attribute_matrix
  }

  wide_data
}

full_unadjusted <- long_to_wide_choices(
  full_unadjusted_long,
  choice_occasions
)
full_adjusted <- long_to_wide_choices(
  full_adjusted_long,
  choice_occasions
)
aggregated_unadjusted <- long_to_wide_choices(
  aggregated_unadjusted_long,
  choice_occasions
)
aggregated_adjusted <- long_to_wide_choices(
  aggregated_adjusted_long,
  choice_occasions
)


############################################################
## 6. Save datasets and an audit record
############################################################

saveRDS(
  full_unadjusted,
  file.path(processed_dir, "HW_wide_full_unadjusted.rds")
)
saveRDS(
  full_adjusted,
  file.path(processed_dir, "HW_wide_full_adjusted.rds")
)
saveRDS(
  aggregated_unadjusted,
  file.path(processed_dir, "HW_wide_aggregated_unadjusted.rds")
)
saveRDS(
  aggregated_adjusted,
  file.path(processed_dir, "HW_wide_aggregated_adjusted.rds")
)

metadata <- list(
  created_at = Sys.time(),
  choice_occasions_per_respondent = choice_occasions,
  excluded_incomplete_respondents = incomplete_respondents,
  full_sample = list(
    respondents = length(unique(full_unadjusted_long$respID)),
    alternatives = length(unique(full_unadjusted_long$siteID)),
    choice_occasions = nrow(full_unadjusted),
    artificial_visits = full_adjustment$log
  ),
  aggregated_sample = list(
    respondents = length(unique(aggregated_unadjusted_long$respID)),
    alternatives = length(unique(aggregated_unadjusted_long$siteID)),
    choice_occasions = nrow(aggregated_unadjusted),
    artificial_visits = aggregated_adjustment$log
  )
)

saveRDS(
  metadata,
  file.path(metadata_dir, "estimation_dataset_metadata.rds")
)

cat("Prepared", nrow(full_adjusted), "choice occasions.\n")
cat("Full alternatives:", length(unique(full_adjusted_long$siteID)), "\n")
cat(
  "Aggregated alternatives:",
  length(unique(aggregated_adjusted_long$siteID)),
  "\n"
)
cat("Artificial visits in full data:", nrow(full_adjustment$log), "\n")
cat(
  "Artificial visits in aggregated data:",
  nrow(aggregated_adjustment$log),
  "\n"
)
