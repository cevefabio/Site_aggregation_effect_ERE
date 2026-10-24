

############################################################
## Figure 4: Annual frequency of brown water event closures
############################################################

rm(list = ls())

library(sf)
library(openxlsx)
library(tidyverse)
library(ggspatial)
library(terra)

## import the impacts, and the grid code 

elev<- rast(file.path("data", "external", "mappe_GIS", "hillmau.tif", "hillmau.tif")) ## hillshade of maui in raster format 


##import bw events dataset
bw<- read.xlsx(file.path("data", "external", "BWevents.xlsx"))
sites <- read.xlsx(file.path("data", "external", "sites.xlsx"))

closures<-as.list(bw$closed_sites)

for (i in 1: length(closures)){
  closures[[i]]<-as.numeric(strsplit(closures[[i]],split=", ",fixed=TRUE)[[1]])
}
## NA are produced because of "all" label let's fix this

beach<-sites[sites$coastal == 1,]  # only coastal sites become unavailable
all_coastal_site_ids<-unique(beach$siteID) 


for (i in 1: length(closures)){
  if(any(is.na(closures[[i]]))){closures[[i]] <- all_coastal_site_ids}
}

rm(beach)


closures_rep<-mapply(function(x,y) rep(x,y), closures,bw$days)

vec<-unlist(closures_rep)
freq<-table(vec)


bw.freq<-data.frame("siteID" = as.numeric(names(freq)), 
                    "n_bw" = as.numeric(freq))

sites<-left_join(sites,bw.freq)

coastal<-sites[!is.na(sites$n_bw),]


### now let's plot 

# Reduce the raster resolution before converting it to a plotting data frame.
elev_reduced <- aggregate(elev, fact = 4, fun = mean) 
elev_df <- as.data.frame(elev_reduced, xy = TRUE)
names(elev_df)[3] <- "elevation"


# Convert coastal site coordinates to the raster coordinate system.
coastal_sf <- st_as_sf(coastal, coords = c("lon", "lat"), crs = 4326)
coastal_sf <- st_transform(coastal_sf, crs = crs(elev_reduced))


coastal_sf$n_bw<-round(coastal_sf$n_bw/5)
## Discretize annual closure incidence.
coastal_sf <- coastal_sf %>%
  mutate(n_bw_class = cut(
    n_bw,
    breaks = c(-Inf, 35, 45, Inf),
    labels = c("<35", "35–45", ">45"),
    right = FALSE
  ))

# Okabe-Ito colors remain distinguishable under common forms of color-vision
# deficiency. The ordering moves from blue to orange to vermillion.
palette <- c(
  "<35" = "#0072B2",
  "35–45" = "#E69F00",
  ">45" = "#D55E00"
)

## Obtain the Maui outline from the elevation raster.

maui_vect <- elev
maui_vect[maui_vect >= 0] <- 1        # assegna valore binario: 1 per terra, NA per mare (o viceversa)
maui_vect <- mask(maui_vect, elev)    # mantieni forma esatta
maui_vect_poly <- as.polygons(maui_vect, dissolve = TRUE)  # unisci in un unico poligono

maui_sf <- st_as_sf(maui_vect_poly)

maui_sf <- st_simplify(maui_sf, dTolerance = 0.0005)
maui_sf <-st_transform(maui_sf, crs = 4326)

#### Add the urban-area layer.

urban<-read_sf(file.path("data", "external", "mappe_GIS", "uac15.shp", "uac15.shp"))
urban <-st_transform(urban, crs = 4326)

urban_maui<-st_intersection(urban, maui_sf)



# Draw the final map.
figure4 <- ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradient(low = "grey60", high = "white", guide = FALSE) +
  
  geom_sf(data = urban_maui , aes(geometry = geometry), fill = "grey30") + ## urban area 
  
  geom_sf(data = maui_sf, fill = NA ,color = "black") +
  
  geom_sf(data = coastal_sf, aes(color = n_bw_class), size = 3) +
  
    scale_color_manual(values = palette, name = "Annual days of closures") +
  coord_sf(crs = crs(elev_reduced), expand = TRUE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  labs(title = "", x = "", y  = "") +
  
  annotation_scale(location = "bl",
                   width_hint = 0.25,
                   style = "bar") +
  
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering()
  )

ggsave(
  file.path("output", "figures", "figure4_bwe_frequency_map.png"),
  plot = figure4,
  width = 8,
  height = 6,
  dpi = 300
)


