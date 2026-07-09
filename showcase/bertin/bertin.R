###
### Population density of the Colorado Plateau, Arizona/New Mexico Plateau, and
### Southern Rockies in the way of Jacques Bertin's valued dots map
###

# Resources:
#   1. https://r-graph-gallery.com/web-valued-dots-map-bertin.html
#   2. https://dieghernan.github.io/202312_bertin_dots/


################################################################################ 
### Libraries and subroutines
################################################################################ 

library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(ggtext)
library(geodata)
library(exactextractr)  # for exact_extract to hexagonal grid
library(osmdata)
library(showtext)
library(pdftools)

wgs <- "epsg:4326"
albers <- "epsg:5070"  # equal area CRS

font_add_google(name = "Gilda Display", family = "gilda") # add custom fonts
showtext_auto()


################################################################################ 
### Download spatial data
################################################################################ 

###
### Boundary of Colorado Plateau, AZ/NM Plateau, and Southern Rockies
###

# Note: Boundary is based on the EPA's Level III Ecoregions as detailed here: 
#   https://www.epa.gov/eco-research/ecoregions-north-america

# URL to EPA Level III Shapefile
eco_url <- "https://dmap-prod-oms-edc.s3.us-east-1.amazonaws.com/ORD/Ecoregions/cec_na/NA_CEC_Eco_Level3.zip"

# Download zip
tmp_zip <- tempfile(fileext = ".zip")
download.file(eco_url, tmp_zip, mode = "wb")

# Unzip
tmp_dir <- tempfile()
unzip(tmp_zip, exdir = tmp_dir)

# Load shapefile
eco <- st_read(list.files(tmp_dir, pattern = "\\.shp$", full.names = TRUE)[1])

# Extract CO/AZ/NM Plateaus and Southern Rockies Ecoregions
eco <- eco %>%
  filter(NA_L3NAME %in% c("Colorado Plateaus",
                          "Arizona/New Mexico Plateau",
                          "Southern Rockies")) %>%
  st_transform(albers)

# Quick check
eco %>%
  ggplot() + 
  geom_sf()

# Derivatives used for plotting
eco_union <- eco %>% st_union()
eco_bbox <- st_as_sfc(st_bbox(eco))
eco_inverse <- eco_bbox %>% st_difference(eco_union)


###
### Population data
###

pop <- geodata::population(year = 2020, res = 0.5, path = tmp_dir)

pop_crop <- pop %>% 
  terra::crop(eco %>% st_transform(wgs), mask=TRUE) %>%  # crop to ecoregions
  terra::project(albers, method="bilinear")  # project to albers

plot(pop_crop)

# Reduce resolution 
f <- round(nrow(pop_crop) / 100)  # factor to reduce raster to ~100 rows
pop_agg <- terra::aggregate(pop_crop, fact = f, fun = "sum", na.rm = TRUE)

plot(pop_agg)

# Square grid
# pop_points <- pop_hex %>%
#   mutate(area = values(terra::cellSize(pop_hex, unit = "km")),
#     density = count / area) %>%  # density by cell
#   select(density) %>%
#   as.points() %>%  # convert to points
#   mutate(class = case_when(  # categorize into density classes
#     density < 1 ~ "A",
#     density < 5 ~ "B",
#     density < 20 ~ "C",
#     density < 100 ~ "D",
#     density < 500 ~ "E",
#     density < 1500 ~ "F",
#     TRUE ~ "G"
#   ))


###
### Convert to hexagonal grid
###

area <- cellSize(pop_agg, unit = "km") %>%  # average cell size of pop_agg
  pull() %>%
  mean() %>%
  units::as_units("km^2")
area

# Create hexagonal grid
pop_hex <- st_make_grid(eco, square = FALSE,
                        cellsize = sqrt(2 * area / sqrt(3)))  # infer hex diameter

area <- st_area(pop_hex) %>%  # area of hexagons
  units::set_units("km^2") %>%
  as.double()

pop_hex <- st_sf(area = area, geom = pop_hex)

# Extract aggregated population by hex cell
pop_hex$count <- exact_extract(pop_agg, y = pop_hex, progress = FALSE,
                               fun = "sum")

# Convert population per cell to density (people per km2)
pop_points <- pop_hex %>%
  mutate(# area = values(terra::cellSize(pop_hex, unit = "km")),
         density = count / area) %>%  # density by cell
  select(density) %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  st_intersection(eco) %>%
  # as.points() %>%  # convert to points
  mutate(class = case_when(  # categorize into density classes
    density < 1 ~ "A",
    density < 5 ~ "B",
    density < 20 ~ "C",
    density < 100 ~ "D",
    density < 500 ~ "E",
    density < 1500 ~ "F",
    TRUE ~ "G"
  ))


###
### Rivers
###

set_overpass_url("https://overpass-api.de/api/interpreter")

all_rivers <- eco_bbox %>% st_transform(wgs) %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

# all_rivers$osm_lines$name

river_names <- c("Colorado River",
                 "Little Colorado River",
                 "San Juan River",
                 "Green River",
                 "Dolores River",
                 "Gunnison River",
                 "Rio Grande",
                 "Arkansas River")

river_lines <- all_rivers$osm_lines %>%
  filter(name%in%river_names) %>%
  st_transform(albers) %>%
  st_intersection(eco)

river_multilines <- all_rivers$osm_multilines %>%
  filter(name%in%river_names) %>%
  st_transform(albers) %>%
  st_intersection(eco)



################################################################################ 
### Make map
################################################################################ 

ggplot() +
  geom_sf(  # add Level III Ecoregion boundaries
    data = eco,
    color = "black",
    fill = "gray97",
    linewidth = 0.25
  ) +
  geom_sf(  # add rivers
    data = river_lines,
    color="cyan4",
    linewidth=0.25
  ) +
  geom_sf(  # add rivers
    data = river_multilines,
    color="cyan4",
    linewidth=0.25
  ) +
  geom_sf(  # add valued dots
    data = pop_points,
    mapping=aes(size=class),
    # mapping = aes(geometry = geometry, size = class),  # for square grid
    pch = 21, color = "gray97", fill = "black", stroke = 0.5
  ) +
  geom_sf(  # add inverse mask
    data = eco_inverse,
    color=NA,
    fill="white",
  ) +
  geom_sf(  # add permiter of Ecoregions
    data = eco_union,
    color="black",
    fill=NA,
    linewidth=0.3
  ) +
  scale_size_manual(
    values = c(0.5, 0.75, 1, 1.5, 2, 3, 4),  # 7 classes total
    labels = c(
      "0", "[1,5)", "[5,20)", "[20,100)", "[100,500)","[500,1500)","\u22651500"),
    guide = guide_legend(
      ncol = 1,
      title.position = "top",
      keywidth = 1,
      label.position = "right"
    )
  ) +
  # annotate(
  #   geom="text", 
  #   x = -Inf, y = -Inf, 
  #   label = "B. Brost \U2022 Ochotona Analytics",
  #   hjust = -0.1, vjust = -0.5, size=3,family="gilda"
  # ) +
  labs(
    title = "Population density of the Colorado Plateau and So. Rockies",
    subtitle = "Following Jacques Bertin's Valued Points",
    size = expression(paste("People per ",km^2)),
    caption = "B. Brost \U2022 Ochotona Analytics"
  ) +
  theme_void() +
  # theme_bw() +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(family="gilda",hjust=0.5,vjust=-0.25,size=10),
    legend.text = element_text(family="gilda",size=8),
    plot.title = element_text(family="gilda",hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="gilda", hjust = 0.5, color = "grey50"),
    plot.caption = element_text(family="gilda",hjust=1.35,vjust=-5,size=8)
  )


ggsave("out/bertin.pdf", height = 6, width = 6)
pdf_convert(
  "out/bertin.pdf",
  filenames = "out/bertin.png",
  format = "png",
  dpi = 600
)
