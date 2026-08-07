###
###
### Create a star map for the International Dark Sky Community of Paonia, CO
###
###

# ------ LIBRARIES AND SUBROUTINES ------

library(sf)
library(tidyverse)
library(grid)  # for polygonGrob
library(s2)

library(ggfx)
library(ggshadow)
library(showtext)
library(ggtext)
library(pdftools)  # for pdf_convert(...)

source("r/utils.R")  # helper functions

font_add(family = "montegar_regular", regular="MontegarDemoRegular-ovD1A.ttf")
font_add(family = "montegar", regular="MontegarDemoCondensed-4nrZ6.ttf")
showtext_auto()


# ------ GET DATA ------

# Constellations
url <- "https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/constellations.lines.json"
constellations <- st_read(url,stringsAsFactors = FALSE) %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>%
  st_cast("MULTILINESTRING")

# Stars
url <- "https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/stars.6.json"
stars <- st_read(url,stringsAsFactors = FALSE)

# Planets
url <- "https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/planets.cn.json"
planets <- st_read(url,stringsAsFactors = FALSE)

# Milky Way
mw <- load_celestial("mw.min.geojson")

# Add colors to MW to use on fill
cols <- colorRampPalette(c("white", "yellow"))(5)
mw$fill <- factor(cols, levels = cols)


# ------ PLOT SETUP ------

loc <- c(38.867608,-107.597820)  # coords of Paonia, CO

dtime <- make_datetime(year = 2024, month = 9, day = 5,  # time of star chart
                      hour = 1, min = 00, 
                      tz = "US/Mountain")  # MDT (Daylight Savings)
lubridate::with_tz(dtime, "UTC")  # MDT is -6:00 from UTC

lon <- get_mst(dtime, loc[2])  # adjust longitude for date and time
lat <- loc[1]  # latitude

# Define Airy projection
airy <- paste0("+proj=airy +x_0=0 +y_0=0 +lon_0=", lon, " +lat_0=", lat)

# Affine transformation to flip perspective of night sky
affine_matrix <- matrix(c(-1, 0, 0, 1), 2, 2)

# Visible hemisphere at loc
hemisphere_s2 <- s2_buffer_cells(
  as_s2_geography(
    paste0("POINT(", lon, " ", lat, ")")
  ),
  9800000,
  max_cells = 5000
)

hemisphere <- hemisphere_s2 %>%
  st_as_sf() %>%
  st_transform(crs = airy) %>%
  st_make_valid()

plot(hemisphere_s2)


# ------ CLIP DATA TO VISIBLE HEMISPHERE ------

# Constellations
constellations_clip <- sf_spherical_cut(constellations,
                                        the_buff = hemisphere_s2,
                                        the_crs = airy,
                                        flip = affine_matrix)

# Stars
stars_clip <- sf_spherical_cut(stars,
                               the_buff = hemisphere_s2,
                               the_crs = airy,
                               flip = affine_matrix)

star_colors <- tibble(bv=c(0.330,-0.25,0.00,0.30,0.60,0.90,1.40),
                      hex=c("#9bb0ff","#aabfff","#ffffff","#ffffc8","#ffff80","#ffc864","#ff6464"))

# Milky Way
mw_clip <- sf_spherical_cut(mw,
                            the_buff = hemisphere_s2,
                            the_crs = airy,
                            flip = affine_matrix)

# Graticules
graticules <- st_graticule(
  ndiscr = 5000,
  lat = seq(-90, 90, 10),
  lon = seq(-180, 180, 30)
)

graticules_clip <- sf_spherical_cut(
  x = graticules,
  the_buff = hemisphere_s2,
  the_crs = airy
)


# ------ PREP PLOT ------

lat_lab <- pretty_lonlat(loc[1], type = "lat")
lon_lab <- pretty_lonlat(loc[2], type = "lon")

pretty_labs <- paste(lat_lab, "/", lon_lab)
cat(pretty_labs)


# ------ CREATE PLOT ------

constellations_clip <- constellations_clip %>%
  st_cast("MULTILINESTRING") %>%
  st_coordinates() %>%
  as.data.frame()

hemisphere_mask <- st_buffer(hemisphere,dist=1000000) %>% 
  st_difference(hemisphere)

st_bbox(hemisphere)  
x_mid <- mean(st_bbox(hemisphere)[c(1,3)])
y_min <- st_bbox(hemisphere)[2]  

ggplot() +
  geom_sf(data = hemisphere, # border of visible hemisphere
          fill = "black", color = "black", linewidth = 0.25) +
  geom_sf(data = graticules_clip,  # graticules
          color = "grey60", linewidth = 0.25, alpha = 0.3) +
  with_blur(sigma = 8,  # blurry Milky Way
    geom_sf(data = mw_clip, aes(fill = fill), alpha = 0.2, color = NA,)
  ) +
  scale_fill_identity() +
  geom_glowpoint(  # glowing stars
    data = stars_clip, aes(
      size=-sqrt(mag),
      alpha = -sqrt(mag),
      geometry = geometry
    ),
    color = "white", fill="white", pch=19, stat = "sf_coordinates"
  ) +
  scale_size_continuous(range = c(0.001, 0.25)) +
  scale_alpha_continuous(range = c(0.1, 0.75)) +
  geom_glowpoint(
    data = stars_clip, aes(
      size=-sqrt(mag),
      color=exp(as.numeric(bv)),
      geometry = geometry
    ),
    alpha = 0.25, stat = "sf_coordinates"
  ) +
  scale_color_gradient2(high="white",mid="#ffffc8",low="#ff6464",midpoint=1) +  # high="#9bb0ff"
  scale_size_continuous(range = c(0.001, 0.25)) +
  geom_glowpath(  # glowing constellations
    data = constellations_clip, aes(X, Y, group = interaction(L1, L2)),
    color = "white", size = 0.35, alpha = 0.8, shadowsize = 0, shadowalpha = 0.01,
    shadowcolor = "white", linejoin = "round", lineend = "round"
  ) +
  geom_sf(data=hemisphere_mask,  # mask
          fill="#005249",color=NA) +
  geom_sf(data = hemisphere, # border of visible hemisphere
          fill = NA, color = "white", linewidth = 0.25) +
  annotate("text", label = "Dark Skies P\u00E6onia",  # \u2021 
           size = 12, hjust=0.5, vjust=0.5, family="montegar_regular",color="white",
           x = x_mid, y = y_min-1600000) +
  annotate("text", label = "International Dark Sky Community",
           size = 5, hjust=0.5, vjust=0.5, family="montegar",color="white",
           x = x_mid, y = y_min-3770000) +
  annotate("text", label = paste0(pretty_labs,"\n",
                                  format(dtime,"%B %d, %Y")),
           size = 4, hjust=0.5, vjust=0.5, family="montegar",color="white",
           x = x_mid, y = y_min-6000000) +
  geom_text(aes(y=-Inf,x=Inf),
            label="Starscape as seen from Grand Ave. at 1:00 AM on 9/4/24",
            size=2,vjust=6.0,hjust=1.1,family="montegar",color="black") +
  geom_line(aes(y=c(y_min,y_min)-2870000, x=c(-2655315,2655693)),
            linewidth=0.25,color="white") +
  geom_point(aes(y=y_min-2850000, x=x_mid),
             color="#005249",pch=19,size=3) +
  geom_point(aes(y=y_min-2850000, x=x_mid),
             fill="#005249",color="white",pch=21) +
  coord_sf(clip="off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill ="#005249", color = "#005249"),
    plot.margin = margin(0, 0, 25, 0),
    panel.border = element_blank(),
    legend.position = "none",
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank()
  )

ggsave("out/dark_night.pdf",device="pdf",width=6,height=8)
pdf_convert(
  "out/dark_night.pdf",
  filenames = "out/dark_night.png",
  format = "png",
  dpi = 600
)