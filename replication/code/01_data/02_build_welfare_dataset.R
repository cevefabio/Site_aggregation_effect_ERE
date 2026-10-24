############################################################
## Prepare the population-by-site dataset for welfare analysis
############################################################

# Run this script from the root of the replication package, after placing the
# external input files in data/external/. The resulting dataset contains one
# row for each 1-km population grid cell and recreation alternative.

rm(list = ls())

library(openxlsx)

route_file <- file.path(
  "data", "external", "grid_centroid_sites_routes3.csv"
)
population_file <- file.path(
  "data", "external", "gridpop_zipcode3.xlsx"
)
site_file <- file.path("data", "external", "sites.xlsx")
output_file <- file.path(
  "data", "processed", "data_for_post_estimation.rds"
)

required_files <- c(route_file, population_file, site_file)
if (!all(file.exists(required_files))) {
  stop(
    "Missing input file(s): ",
    paste(required_files[!file.exists(required_files)], collapse = ", ")
  )
}

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)


############################################################
## 1. Read and verify the geographic inputs
############################################################

routes <- read.csv(route_file)
population <- read.xlsx(population_file)
sites <- read.xlsx(site_file)

if (anyDuplicated(routes[c("id1km", "finalsiteID")])) {
  stop("The id1km-finalsiteID key is not unique in the route data.")
}
if (anyDuplicated(population$id1km)) {
  stop("id1km is not unique in the population data.")
}
if (anyDuplicated(sites$siteID)) {
  stop("siteID is not unique in the site inventory.")
}

cat("Population grid cells:", nrow(population), "\n")
cat("Sites in the current inventory:", nrow(sites), "\n")
cat("Route records:", nrow(routes), "\n")


############################################################
## 2. Add the stay-at-home alternative and population weights
############################################################

home_routes <- data.frame(
  id1km = unique(routes$id1km),
  finalsiteID = 0,
  dist = 0,
  time = 0
)

routes <- rbind(routes, home_routes)
names(routes)[names(routes) == "finalsiteID"] <- "siteID"

# The legacy preparation retained only route alternatives that were present in
# the final site inventory. Route IDs 9, 51, and 115 are absent from that
# inventory and are therefore dropped by the site merge below.
route_sites_not_in_inventory <- setdiff(unique(routes$siteID), sites$siteID)
inventory_sites_without_routes <- setdiff(sites$siteID, unique(routes$siteID))

if (!identical(sort(route_sites_not_in_inventory), c(9, 51, 115))) {
  warning(
    "Unexpected route site IDs absent from the site inventory: ",
    paste(sort(route_sites_not_in_inventory), collapse = ", ")
  )
}
if (length(inventory_sites_without_routes) > 0) {
  stop(
    "Site inventory contains alternatives without route or home records: ",
    paste(inventory_sites_without_routes, collapse = ", ")
  )
}

n_routes_before_population_merge <- nrow(routes)
routes <- merge(
  routes,
  population,
  by = "id1km",
  all = FALSE,
  sort = FALSE
)

if (nrow(routes) != n_routes_before_population_merge) {
  stop("The population merge changed the number of route records.")
}


############################################################
## 3. Attach site characteristics
############################################################

# Site coordinates are removed because welfare is assigned to population grid
# cells. The retained latitude and longitude therefore refer to residences.
site_characteristics <- sites[, !names(sites) %in% c("lat", "lon")]

welfare_data <- merge(
  routes,
  site_characteristics,
  by = "siteID",
  all = FALSE,
  sort = FALSE
)
welfare_data <- welfare_data[
  order(welfare_data$id1km, welfare_data$siteID),
]
rownames(welfare_data) <- NULL

expected_rows <- nrow(population) * nrow(sites)
if (nrow(welfare_data) != expected_rows) {
  stop(
    "Unexpected number of population-site rows: ", nrow(welfare_data),
    ". Expected ", expected_rows, "."
  )
}


############################################################
## 4. Calculate round-trip travel cost
############################################################

# Sites 40 and 116 require an additional 40 minutes of walking.
welfare_data$time[
  welfare_data$siteID %in% c(40, 116)
] <- welfare_data$time[
  welfare_data$siteID %in% c(40, 116)
] + 40

# Monetary vehicle cost. The conversion reproduces the original analysis:
# approximately $0.20 per mile, with distance recorded in kilometres.
welfare_data$cost <- welfare_data$dist * 0.20 / 1.5

# Opportunity cost of travel time: three quarters of the $24.14 hourly wage,
# equal to approximately $18.10 per hour.
welfare_data$cost <-
  welfare_data$cost + welfare_data$time / 60 * (3 / 4) * 24.14

# Convert the one-way route measures to a round trip.
welfare_data$cost <- welfare_data$cost * 2

# Molokini requires a commercial boat tour. Its cost is the travel cost to
# Makena Landing Beach Park (site 52) plus $100 for the tour.
molokini_rows <- welfare_data$siteID == 146
makena_cost <- welfare_data$cost[welfare_data$siteID == 52]

if (sum(molokini_rows) != length(makena_cost)) {
  stop("Molokini and Makena rows do not align by population grid cell.")
}
welfare_data$cost[molokini_rows] <- makena_cost + 100

saveRDS(welfare_data, output_file)

cat("Saved welfare dataset:", output_file, "\n")
cat("Rows:", nrow(welfare_data), "\n")
cat("Population grid cells:", length(unique(welfare_data$id1km)), "\n")
cat("Alternatives:", length(unique(welfare_data$siteID)), "\n")
