############################################################
#### SITE VISITATION BREAKDOWN FOR FOCAL/PERIPHERAL SITES ####
############################################################

rm(list = ls())

library(dplyr)
library(openxlsx)

# Run this script from the root of the replication package.

input_choice_file <- file.path("data", "raw", "for_estimation_withbio.dat")
input_coral_file  <- file.path("data", "external", "coralPNAS.csv")
output_file       <- file.path("output", "tables", "site_visitation_breakdown_non_corrected.xlsx")

incomplete_respondents <- c(
  327, 341, 345, 346, 348, 350, 352, 355, 362, 366, 367,
  370, 375, 378, 381, 383, 385
)


# ===========================================================
# 1. Rebuild the long estimation data used before aggregation
# ===========================================================

load_raw_choice_data <- function(choice_file, coral_file) {
  data <- read.table(choice_file, sep = "\t", header = TRUE)
  data <- subset(data, !(respID %in% incomplete_respondents))

  coral <- read.table(coral_file, sep = ",", header = TRUE)
  data <- merge(data, coral, all = TRUE)
  data <- data[order(data$respID, data$siteID), ]

  data
}

recode_site_attributes <- function(data) {
  # Keep these recodes aligned with code/01_data/01_build_estimation_datasets.R.
  data$beach[data$siteID == 130] <- 1
  data$coast_trail[data$siteID == 130] <- 0
  data$res_fish_bio[data$siteID == 130] <- 15.45

  data$coral[data$siteID == 146] <- 95
  data$PNAS[is.na(data$PNAS)] <- 0
  data$PNAS[data$siteID == 146] <- data$PNAS[data$siteID == 146] + 20

  data$coastal <- data$beach + data$harbor + data$molokini

  data
}

raw_data <- load_raw_choice_data(input_choice_file, input_coral_file)
base_data <- recode_site_attributes(raw_data)

visitation_data <- base_data %>%
  select(siteID, respID, times, name, cost, home, coastal) %>%
  mutate(
    treatment_in_aggregated_model = if_else(
      coastal == 0 & home == 0,
      "Peripheral site aggregated into OOR",
      "Focal singleton site"
    )
  )


# ===========================================================
# 2. Count observed visits before aggregation
# ===========================================================

total_visits <- sum(visitation_data$times)

detailed_table <- visitation_data %>%
  group_by(siteID, name, treatment_in_aggregated_model) %>%
  summarise(observed_visits = sum(times), .groups = "drop") %>%
  group_by(treatment_in_aggregated_model) %>%
  mutate(share_within_group = observed_visits / sum(observed_visits)) %>%
  ungroup() %>%
  mutate(share_all_visits = observed_visits / total_visits) %>%
  arrange(treatment_in_aggregated_model, desc(observed_visits), siteID) %>%
  select(
    siteID,
    site_name = name,
    treatment_in_aggregated_model,
    observed_visits,
    share_all_visits,
    share_within_group
  )

summary_table <- detailed_table %>%
  group_by(treatment_in_aggregated_model) %>%
  summarise(
    n_sites = n(),
    observed_visits = sum(observed_visits),
    share_all_visits = sum(share_all_visits),
    .groups = "drop"
  ) %>%
  arrange(desc(observed_visits))

top_peripheral_sites <- detailed_table %>%
  filter(treatment_in_aggregated_model == "Peripheral site aggregated into OOR") %>%
  arrange(desc(observed_visits), siteID)

notes <- data.frame(
  item = c(
    "Purpose",
    "Aggregation rule",
    "Adjustment",
    "Output"
  ),
  description = c(
    "Distribution of observed visits across focal singleton sites and peripheral sites before aggregation.",
    "A row is peripheral when coastal == 0 and home == 0, matching the OOR aggregation rule in code/01_data/01_build_estimation_datasets.R.",
    "Counts use the non-corrected long data; no one-visit reassignment for never-chosen sites is applied.",
    output_file
  )
)


# ===========================================================
# 3. Export to Excel
# ===========================================================

dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)

wb <- createWorkbook()

addWorksheet(wb, "summary")
writeData(wb, "summary", summary_table)

addWorksheet(wb, "site_breakdown")
writeData(wb, "site_breakdown", detailed_table)

addWorksheet(wb, "peripheral_sites")
writeData(wb, "peripheral_sites", top_peripheral_sites)

addWorksheet(wb, "notes")
writeData(wb, "notes", notes)

percent_style <- createStyle(numFmt = "0.0%")
integer_style <- createStyle(numFmt = "#,##0")

for (sheet in c("summary", "site_breakdown", "peripheral_sites")) {
  freezePane(wb, sheet, firstRow = TRUE)
  setColWidths(wb, sheet, cols = 1:10, widths = "auto")
}

addStyle(wb, "summary", integer_style, rows = 2:(nrow(summary_table) + 1), cols = 2:3, gridExpand = TRUE)
addStyle(wb, "summary", percent_style, rows = 2:(nrow(summary_table) + 1), cols = 4, gridExpand = TRUE)

addStyle(wb, "site_breakdown", integer_style, rows = 2:(nrow(detailed_table) + 1), cols = 4, gridExpand = TRUE)
addStyle(wb, "site_breakdown", percent_style, rows = 2:(nrow(detailed_table) + 1), cols = 5:6, gridExpand = TRUE)

if (nrow(top_peripheral_sites) > 0) {
  addStyle(wb, "peripheral_sites", integer_style, rows = 2:(nrow(top_peripheral_sites) + 1), cols = 4, gridExpand = TRUE)
  addStyle(wb, "peripheral_sites", percent_style, rows = 2:(nrow(top_peripheral_sites) + 1), cols = 5:6, gridExpand = TRUE)
}

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("\nSaved visitation breakdown table to:\n")
cat(output_file, "\n")
