###
###
### The Wild Owyhee
###
###

rm(list = ls())


###
### Libraries
###

# Data manipulation
library(terra)
library(tidyterra)
library(tidyverse)
library(sf)

# Get the data
library(geodata)
library(elevatr)  # DEM from Amazon Web Services
library(osmdata)
library(nhdplusTools)  # river basin data

# Plotting
library(ggplot2)
library(scales)
library(colorspace)
library(showtext)
library(pdftools)


###
### Global variables
###

wgs <- "epsg:4326"
utm <- "epsg:32611"  # UTM Zone 11N


################################################################################
### Get spatial data
################################################################################

###
### River basins from National Hydrology Dataset
###

# USGS Dashboard: https://dashboard.waterdata.usgs.gov/app/nwd/en/

# Lower basin: Owyhee River Below Owyhee Dam, OR - USGS-13183000
basin <- get_nldi_basin(nldi_feature = list(featureSource = "nwissite", featureID =
                                              "USGS-13183000"))


###
### Define bounding box for data downloads
###

bbox <- st_buffer(basin %>% st_transform(utm), dist = 10000) %>%
  st_transform(wgs) %>%
  st_bbox()


###
### DEM and other terrain products
###

# DEM from Amazon Web Services (via elevatr package)
dem <- get_aws_terrain(bbox, z = 7, prj = wgs)  # resolution: z=7 is ~860m, z=10 is ~108m
dem <- dem %>% crop(bbox)
plot(dem)
plot(basin, add = TRUE)

# Slope
slope <- terrain(dem, "slope", unit = "radians")
plot(slope)

# Aspect
aspect <- terrain(dem, "aspect", unit = "radians")
plot(aspect)

# Conventional hillshade
hillshade <- shade(slope, aspect, 30, 45)
plot(hillshade)


###
### Vector data from osmdata
###

set_overpass_url("https://overpass-api.de/api/interpreter")

# Water features
rivers <- bbox %>%  # rivers
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

water <- bbox %>%  # natural water features?
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

# State boundaries
states <- bbox %>%
  opq() %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "admin_level", value = "4") %>%
  osmdata_sf()
states

# Isolate important features
owyhee <- rivers$osm_lines %>%  # Owyhee River and forks, etc.
  filter(str_detect(name, "Owyhee"))
plot(dem)
plot(owyhee, add = TRUE)

snake <- rivers$osm_lines %>%  # Snake River
  filter(str_detect(name, "Snake River"))
plot(snake, add = TRUE)

owyhee_lake <- water$osm_multipolygons %>%  # Lake Owyhee
  filter(str_detect(name, "Owyhee Lake"))
plot(owyhee_lake, add = TRUE)


###
### Precipitation
###

precip <- geodata::worldclim_country("USA", "prec", tempdir())  # precip data
precip <- precip %>% crop(bbox)

precip_sum <- sum(precip)  # sum all layers
autoplot(precip_sum)



################################################################################
### Setup map
################################################################################

# Add Google fonts
font_add_google(name = "Shalimar", family = "shalimar") # add custom fonts
font_add_google(name = "Tangerine", family = "tangerine") # add custom fonts
font_add_google(name = "Cormorant SC", family = "cormorant_sc") # add custom fonts
showtext_auto()

# Base text size
base_text_size <- 10

# Mask spatial data to basin
hillshade_mask <- hillshade %>% terra::mask(basin)

dem_mask <- dem %>% terra::mask(basin)

precip_mask <- precip_sum %>% terra::mask(basin)

basin_mask <- st_as_sfc(bbox) %>% st_difference(basin)
plot(basin_mask, col = 2)

states_mask <- states$osm_lines %>%
  st_intersection(st_as_sfc(bbox)) %>%
  st_difference(basin)
plot(states_mask$geometry)



################################################################################
### Plot hillshade
################################################################################

# Color palette for hillshade
pal_greys <- hcl.colors(1000, "Grays")

# Index of color by cell
names(hillshade_mask) <- "shades"
idx <- hillshade_mask %>%
  mutate(idx_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(idx_col = round(idx_col)) %>%
  pull(idx_col)

# Colors
hillshade_cols <- pal_greys[idx]

# Base hillshade plot
hillshade_plot <-
  ggplot() +
  geom_spatraster(
    data = hillshade_mask,
    fill = hillshade_cols,
    maxcell = Inf,
    alpha = 1
  )
hillshade_plot



################################################################################
### Create hillshade base map
################################################################################

owyhee_plot <- hillshade_plot +
  geom_sf(data = states_mask,
          linewidth = 0.25,
          color = "gray") +
  labs(title = "The Wild Owyhee") +
  annotate(
    geom = "text",
    x = -Inf,
    y = -Inf,
    label = "B. Brost | Ochotona Analytics",
    hjust = -0.1,
    vjust = -0.5,
    size = base_text_size * 0.5,
    family = "shalimar"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(
      family = "shalimar",
      face = "bold",
      size = base_text_size * 3,
      hjust = 0.5,
      vjust = -1.
    ),
    plot.title.position = "plot",
    plot.subtitle = element_text(
      family = "gilda",
      size = base_text_size * 1,
      hjust = 0.5,
      vjust = -6
    ),
    legend.key = element_rect("grey50"),
    legend.title = element_text(size = base_text_size * 0.9, family =
                                  "gilda"),
    legend.text = element_text(size = base_text_size * 0.9, family =
                                 "gilda")
  )
owyhee_plot

# Add labels for state boundaries
label_locs <- data.frame(
  loc = c("left", "right", "top"),
  x = c(-117.9, -115.75, -117.025),
  y = c(42.0, 42.0, 43.6)
) %>%
  st_as_sf(coords = c("x", "y"), crs = wgs)

map_labels <- list(
  geom_sf_label(
    data = label_locs[1, ],
    aes(label = "Ore."),
    vjust = 0.1,
    border.color = NA,
    fill = NA,
    size = 2.5,
    family = "cormorant_sc"
  ),
  geom_sf_label(
    data = label_locs[2, ],
    aes(label = "Idaho"),
    vjust = 0.1,
    border.color = NA,
    fill = NA,
    size = 2.5,
    family = "cormorant_sc"
  ),
  geom_sf_label(
    data = label_locs[1:2, ],
    aes(label = "Nev."),
    vjust = 0.995,
    border.color = NA,
    fill = NA,
    size = 2.5,
    family = "cormorant_sc"
  )
)
owyhee_plot + map_labels



################################################################################
### Create precipitation plot
################################################################################

# Precipitation limits
values(precip_mask) %>% range(na.rm = TRUE)
precip_limits = c(200, 600)

# Precipitation color palette with colorspace
mypal <- sequential_hcl(
  n = 16,
  h = c(320, 80),
  c = c(60, 65, 20),
  l = c(30, 95),
  power = c(0.7, 1.3),
  rev = TRUE
)
show_col(mypal)

# Precipitation plot
precip_plot <- owyhee_plot +
  geom_spatraster(data = precip_mask, maxcell = Inf) +  # precip. raster
  geom_sf(data = basin_mask,
          color = NA,
          fill = "white") +
  geom_sf(data = states_mask,
          linewidth = 0.25,
          color = "gray") +
  geom_sf(data = basin,
          color = "black",
          fill = NA) +
  geom_sf(  # add Owyhee River
    data = owyhee %>% st_intersection(basin),
    linewidth = 0.2,
    color = "cyan4"
  ) +
  geom_sf(  # add Owyhee Lake
    data = owyhee_lake %>% st_intersection(basin),
    linewidth = 0.1,
    fill = "cyan4",
    color = "cyan4"
  ) +
  scale_fill_gradientn(
    colours = alpha(mypal, 0.7),
    na.value = NA,
    labels = label_comma(),
    breaks = seq(0, precip_limits[2], 50)
  ) +
  map_labels +  # add labels for state boundaries
  labs(subtitle = "Plate 6. Average annual precipitation.") +
  guides(
    fill = guide_legend(
      title = "  mm.",
      title.position = "top",
      keywidth = .5,
      reverse = TRUE,
      override.aes = list(alpha = 0.8)
    )
  ) +
  theme(legend.text = element_text(hjust = 0), legend.position = "right")
precip_plot

ggsave("out/precip.pdf", height = 7, width = 6)
pdf_convert(
  "out/precip.pdf",
  filenames = "out/precip.png",
  format = "png",
  dpi = 600
)
