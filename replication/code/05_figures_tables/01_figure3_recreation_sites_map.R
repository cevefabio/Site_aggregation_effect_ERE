############################################################
## Figure 3: Coastal and non-coastal recreation sites
############################################################

rm(list = ls())

library(sf)
library(openxlsx)
library(ggplot2)
library(ggspatial)
library(terra)
library(ggimage)
library(grid)
library(png)

elevation_file <- file.path(
  "data", "external", "mappe_GIS", "hillmau.tif", "hillmau.tif"
)
urban_file <- file.path(
  "data", "external", "mappe_GIS", "uac15.shp", "uac15.shp"
)
site_file <- file.path("data", "external", "sites.xlsx")
beach_icon_file <- file.path("logos", "beach_ppt.png")
park_icon_file <- file.path("logos", "park_ppt.png")

required_files <- c(
  elevation_file,
  urban_file,
  site_file,
  beach_icon_file,
  park_icon_file
)
if (!all(file.exists(required_files))) {
  stop(
    "Missing input file(s): ",
    paste(required_files[!file.exists(required_files)], collapse = ", ")
  )
}

dir.create(file.path("output", "figures"), recursive = TRUE, showWarnings = FALSE)


############################################################
## 1. Site groups and map layers
############################################################

sites <- read.xlsx(site_file)
coastal_sites <- sites[sites$coastal == 1, ]
non_coastal_sites <- sites[sites$coastal != 1 & sites$home == 0, ]

coastal_sites$icon <- beach_icon_file
non_coastal_sites$icon <- park_icon_file

elevation <- rast(elevation_file)

# Lower resolution keeps the background light enough for the site markers and
# substantially reduces plotting time without altering site locations.
elevation_reduced <- aggregate(elevation, fact = 4, fun = mean)
elevation_frame <- as.data.frame(elevation_reduced, xy = TRUE)
names(elevation_frame)[3] <- "elevation"

# Convert the non-missing elevation cells to a polygon representing Maui.
maui_raster <- elevation
maui_raster[maui_raster >= 0] <- 1
maui_raster <- mask(maui_raster, elevation)
maui_polygon <- as.polygons(maui_raster, dissolve = TRUE)
maui <- st_as_sf(maui_polygon)
maui <- st_simplify(maui, dTolerance = 0.0005)
maui <- st_transform(maui, crs = 4326)

urban <- read_sf(urban_file)
urban <- st_transform(urban, crs = 4326)
urban_maui <- suppressWarnings(st_intersection(urban, maui))

coastal_sf <- st_as_sf(
  coastal_sites,
  coords = c("lon", "lat"),
  crs = 4326
)
coastal_sf <- st_transform(coastal_sf, crs = crs(elevation_reduced))

non_coastal_sf <- st_as_sf(
  non_coastal_sites,
  coords = c("lon", "lat"),
  crs = 4326
)
non_coastal_sf <- st_transform(
  non_coastal_sf,
  crs = crs(elevation_reduced)
)


############################################################
## 2. Draw Figure 3
############################################################

figure3 <- ggplot() +
  geom_sf(data = maui, fill = "white", color = "black") +
  geom_raster(
    data = elevation_frame,
    aes(x = x, y = y, fill = elevation)
  ) +
  scale_fill_gradient(low = "grey60", high = "white", guide = "none") +
  geom_sf(data = urban_maui, fill = "grey30", color = NA) +
  geom_image(
    data = coastal_sf,
    aes(
      x = st_coordinates(coastal_sf)[, 1],
      y = st_coordinates(coastal_sf)[, 2],
      image = icon
    ),
    size = 0.025
  ) +
  geom_image(
    data = non_coastal_sf,
    aes(
      x = st_coordinates(non_coastal_sf)[, 1],
      y = st_coordinates(non_coastal_sf)[, 2],
      image = icon
    ),
    size = 0.025
  ) +
  coord_sf(crs = crs(elevation_reduced), expand = TRUE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  annotation_scale(location = "bl", width_hint = 0.25, style = "bar") +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering()
  )

# The legend uses the same icons as the site markers.
beach_legend <- rasterGrob(
  readPNG(beach_icon_file),
  width = unit(0.6, "cm")
)
park_legend <- rasterGrob(
  readPNG(park_icon_file),
  width = unit(0.6, "cm")
)
urban_legend <- rectGrob(
  width = unit(0.6, "cm"),
  height = unit(0.6, "cm"),
  gp = gpar(fill = "grey30", col = NA)
)

x_right <- max(elevation_frame$x) - 10000
y_base <- max(elevation_frame$y) - 5000

figure3 <- figure3 +
  annotation_custom(
    beach_legend,
    xmin = x_right - 5000,
    xmax = x_right,
    ymin = y_base + 3000,
    ymax = y_base + 5000
  ) +
  annotation_custom(
    park_legend,
    xmin = x_right - 5000,
    xmax = x_right,
    ymin = y_base,
    ymax = y_base + 2000
  ) +
  annotation_custom(
    urban_legend,
    xmin = x_right - 5000,
    xmax = x_right,
    ymin = y_base - 3000,
    ymax = y_base - 1000
  ) +
  annotate(
    "text", x = x_right + 200, y = y_base + 4000,
    label = "Coastal", hjust = 0, size = 5
  ) +
  annotate(
    "text", x = x_right + 200, y = y_base + 1000,
    label = "Non-coastal", hjust = 0, size = 5
  ) +
  annotate(
    "text", x = x_right + 200, y = y_base - 2000,
    label = "Urban area", hjust = 0, size = 5
  )

ggsave(
  file.path("output", "figures", "figure3_recreation_sites_map.png"),
  plot = figure3,
  width = 8,
  height = 6,
  dpi = 300
)
