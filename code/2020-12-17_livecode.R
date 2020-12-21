# 2020-12-17 DeBugging Buddies --------------------------------------------


## HIDDEN FUN TIP!! Add YuleTide Theme to your Rstudio:
#devtools::install_github("gadenbuie/rsthemes")
#rstudioapi::addTheme("https://raw.githubusercontent.com/gadenbuie/yule-rstudio/master/Yule-RStudio.rstheme", apply=TRUE)

# making maps with tmap and ggplot2

# first load libraries
library(tidyverse)
library(sf)
library(USAboundaries)
library(mapview)
library(janitor)
library(tmap)
library(tmaptools)



# Download Data -----------------------------------------------------------


# delta_crop file
# rda vs rds:
# rdata (rda, RDA, RDATA): this is a file that is compressed, and can store multiple within it. On import it comes into R as what it was saved as.

# rds (compressed R specific file): this is for a single file or object. On import, you can name this whatever you want.

load(file = "data/delta_crop_sf.rda")
# from a URL
load(file=url("https://github.com/ryanpeek/cws_code_club/raw/main/data/delta_crop_sf.rda"))

# grab the other rda file:
load(file="data/final_map_components.rda")


# Making a Mapview Map ----------------------------------------------------


# interactive web map, use mapview

# map with a single color for a layer
mapview(delta_crop) + mapview(mainstems, color="steelblue")

# map a color as a variable: use zcol to refer to a column in dataframe, must be quoted
mapview(delta_crop) + mapview(mainstems, zcol="station", layer.name="Zooplankton IEP")


# Using ggplot2 -------------------------------------------------------------

# ggplot uses layer, and geoms
ggplot() +
  geom_sf(data=delta_crop, fill="steelblue") +
  # this is point (similar to geom_point)
  geom_sf(data=cities_sf, color="gray", size=4, pch=17) + # look up pch types with ?pch
  # this is lines (similar to geom_lines)
  geom_sf(data=mainstems, color="darkblue", lwd=0.7, alpha=0.8) +
  # this is polygons
  geom_sf(data=cos_cnty_fp, fill="orange", color="gold") +
  theme_minimal() +
  labs(title="Cool Delta map",
       subtitle="There aren't llamas on this map...yet",
       caption="These data are from previously generated data",
       x="longitude", y="latitude") +
  ggspatial::annotation_north_arrow()

# ggspatial package: allows you to add north arrow and scale bars

ggsave(filename = "data_output/delta_map_example_ggplot.png", width = 11, height = 8,
       dpi=300, units = "in")


# Tmap Map ----------------------------------------------------------------

# to get a spatial basemap layer
base_map1 <- read_osm(x = delta_crop, type="esri-topo", raster=TRUE)

# tmap requires both a tm_shape and then the aesthetic call separately
(map2 <- tm_shape(base_map1) + tm_rgb()) # wrap code in paranthesis to run and view at same time

(map3 <- map2 +
    tm_shape(delta_crop) + tm_polygons(border.col = "steelblue", alpha = 0.4, border.alpha = 0.1) +
    tm_shape(mainstems) + tm_lines(col="steelblue") +
    tm_shape(cities_sf) + tm_dots(col="black", shape=19) +
    tm_compass(type="4star", position=c("left", "bottom")) +
    tm_scale_bar(position=c("left", "bottom")))

# how to save out
tmap_save(map3, filename = "data_output/delta_map_example_tmap_w_basemap.png", width = 11, height = 8,
          dpi=300, units = "in" )
