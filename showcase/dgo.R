###
###
### Create map of Durango, CO using OpenStreetMap
###
###

rm(list=ls())


###
### Resources
###

# https://wiki.openstreetmap.org/wiki/Map_features
# https://allisonhorst.github.io/posts/2022-03-08-nw-reno-roads/
# https://www.joshuamccrain.com/tutorials/maps/streets_tutorial.html


###
### Libraries and subroutines
###

# Attach packages:
library(tidyverse)
library(osmdata)
library(showtext)
library(ggmap)
library(ggforce)
library(here)
library(sf)
library(grid)  # for grid.locator()

# Import font
font_add_google(name = "Gilda Display", family = "gilda") # add custom fonts
font_add_google(name = "Tangerine", family = "tangerine") # add custom fonts

showtext_auto()


###
### Get basemaps
###

# Bounding box for Durango
dgo_box <- getbb("Durango Colorado")

# Get roads
big_roads <- dgo_box %>%  # main roads
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway","trunk", "primary", "motorway_link", "trunk_link","primary_link")) %>%
  osmdata_sf()

med_roads <- dgo_box %>%  # medium roads
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_roads <- dgo_box %>%  # small roads
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street")) %>%
  osmdata_sf()

# Get rivers
rivers <- dgo_box %>%  # all rivers
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

animas <- rivers[["osm_lines"]] %>%  # Animas River only
  filter(name=="Animas River")

# Durango & Silverton Narrow Gauge Railroad
rail <- dgo_box %>%
  opq()%>%
  add_osm_feature(key = "railway", value = "narrow_gauge") %>%
  osmdata_sf()

# Highway labels and placements
dgo_labs <- tibble(type="highway",
                   x=c(-107.86,-107.91,-107.876),
                   y=c(37.31,37.2695,37.25),
                   label=c("550","160",""))

###
### Create a map
###

ggplot() +
  geom_sf(data=rivers$osm_lines, linewidth=0.21, color="cyan4") +
  geom_sf(data=animas, linewidth=0.5, color="cyan4") +
  geom_sf(data=small_roads$osm_lines, linewidth=0.075) +
  geom_sf(data=med_roads$osm_lines, linewidth=0.1) +
  geom_sf(data=big_roads$osm_lines, linewidth=0.5) +
  geom_sf(data=rail$osm_lines, linewidth=0.25, color="darkred") +
  coord_sf(xlim=c(-107.94,-107.817), ylim=c(37.235,37.3375), expand=TRUE, clip="on") +
  geom_circle(aes(x0=x, y0=y, r=0.00275), data=dgo_labs, fill="white", 
              color="gray20", size=0.3) +
  geom_text(aes(x=x, y=y, label=label),data=dgo_labs, family="gilda", size=3) +
  annotate(geom="text", x=-107.876, y=37.25+0.0009, label="550", family="gilda", size=3)+
  annotate(geom="text", x=-107.876, y=37.25-0.0009, label="160", family="gilda", size=3)+
  geom_line(aes(y=c(37.3425,37.3425), x=c(-107.94+0.01,-107.817-0.01)),linewidth=5,color="#fcf9f4") +
  geom_line(aes(y=c(37.3425,37.3425), x=c(-107.94+0.01,-107.817-0.01)),linewidth=0.1,color="gray10") +
  annotate(geom = "text", x = -107.905, y = 37.2875, label = "Animas\nRiver",
           family = "gilda", color = "cyan4", size = 6) +
  geom_curve(aes(x = -107.897, y = 37.2865, xend = -107.8835, yend = 37.278),
             curvature=-0.2, arrow = arrow(length = unit(0.02, "npc"),type="closed"),
             color = "cyan4") +
  labs(title = "Durango", subtitle = "37°16'26.0\"N  107°52'45.6\"W") +
  theme_void() +  # no grid lines, background color, etc.
  theme(plot.background=element_rect(fill="#fcf9f4",color="#fcf9f4"),
        text=element_text(family="gilda"),
        plot.title = element_text(size = 30, family = "tangerine",face="bold", hjust=.5),
        plot.subtitle = element_text(family = "gilda", size = 12, hjust=.5, margin=margin(2, 0, 5, 0)))

ggsave("dgo.pdf",height=7,width=6)
# ggsave("durango.png", height = 7, width = 5, dpi = 600)