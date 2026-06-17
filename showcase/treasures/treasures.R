###
###
### Create Unknown "Treasures" Map (a la Joy Division's album Unknown Pleasures)
###
###


###
### Libraries and subroutines
###

library(tidyverse)

# Spatial
library(sf)
library(terra)
library(giscoR) # Shapes
library(elevatr)
library(osmdata)
library(nhdplusTools)  # river basin data
library(rnaturalearth)  # polygon data

# Data viz and wrangling
library(ggplot2)
library(dplyr)
library(ggridges)
library(units)
library(showtext)
library(pdftools)

wgs <- "epsg:4326"  # WGS 84
utm <- "epsg:32610"  # Zone 10N - British Columbia and Washington

# Import font
font_add_google(name = "Kedebideri", family = "kedebideri") # add custom fonts
font_add_google(name = "Gilda Display", family = "gilda") # add custom fonts

showtext_auto()


###
### Get polygon data for Vancouver Island and San Juan Islands
###

# Vancouver Island
canada <- ne_states(country = "Canada", returnclass = "sf")  # Canada

bc <- canada %>% 
  filter(name == "British Columbia") %>%  # British Columbia
  st_cast("POLYGON") %>%  # multipolygon to polygon
  mutate(area = st_area(.)) %>%  # calculate areas
  arrange(desc(area))

# Plot polygons by idx
bc %>% 
  mutate(idx = as.factor(1:nrow(.))) %>%
  ggplot() +
  geom_sf(aes(fill = idx))

vi <- bc[2, ]  # Vancouver Island

vi %>%
  ggplot() +
  geom_sf()

# San Juan Islands
us <- ne_states(country = "United States of America", returnclass = "sf")

wa <- us %>% 
  filter(name == "Washington") %>%
  st_cast("POLYGON") %>%  # multipolygon to polygon
  mutate(area = st_area(.)) %>%  # calculate areas
  arrange(desc(area))

# Plot polygons by idx
wa %>% 
  mutate(idx = as.factor(1:nrow(.))) %>%
  ggplot() +
  geom_sf(aes(fill = idx))

sji <- wa[c(3, 4, 6), ]  # San Juan Islands

sji %>%
  ggplot() +
  geom_sf()

# Combine island polygons
sa <- st_union(vi, sji) %>%  # study area
  st_combine() %>%
  st_transform(utm)

sa %>%
  ggplot() +
  geom_sf()


###
### Get and wrangle DEM data
###

dem <- get_aws_terrain(sa, z = 7, prj = utm)  # resolution: z=7 is ~860m, z=10 is ~108m
plot(dem)
plot(sa, add = TRUE)

# Crop and mask DEM
dem <- dem %>%  # crop AWS tiles to bounding box
  crop(sa %>% st_buffer(dist = 25000)) %>%  # define extent
  mask(terra::vect(sa))  # mask values to study area

# Rename layer
names(dem) <- "elev"

# Aggregate DEM to lower resolution
nrow(dem)
factor <- round(nrow(dem) / 75)
dem_agg <- aggregate(dem, factor)
nrow(dem_agg)


###
### Create Unknown Pleasures figure
###

# Convert raster to tibble for plotting
dem_df <- dem_agg %>% 
  as.data.frame(xy = TRUE, na.rm = FALSE) %>%
  as_tibble() %>%
  replace_na(list(elev = 0))  # replace NA values with 0

# Create 'unknown pleasures' plot
ggplot() +
  geom_ridgeline(
    aes(
      x = x,
      y = y,
      group = y,
      height = elev
    ),
    data = dem_df %>% 
      filter(y > 5359000 & y < 5635000),
    scale = 25,
    fill = "black",
    color = "white",
    linewidth = .25
  ) +
  labs(caption = "B. Brost \U2022 Ochotona Analytics", 
       title = "UNKNOWN TREASURES") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(
      size = 28,
      family = "kedebideri",
      color = "white",
      hjust = 0.5,
      vjust = -6
    ),
    plot.caption = element_text(
      color = "white",
      size = 14,
      margin = margin(b = 10, r = 40),
      hjust = 1,
      family = "gilda"
    )
  )

ggsave("out/treasures.pdf", height = 10, width = 10)
pdf_convert(
  "out/treasures.pdf",
  filenames = "out/treasures.png",
  format = "png",
  dpi = 600
)
