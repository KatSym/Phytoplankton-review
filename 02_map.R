
#AQUACOSM Review Map

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")
library("ggspatial")
library("ggrepel")
library("tidyverse")

full_table_map <- dplyr::select(full_table_2, rayyan_id, system, country, lat, long)
full_table_map <- na.omit(full_table_map)
sf_locations <- st_as_sf(full_table_map, 
                         coords = c("long", "lat"), 
                         remove = FALSE, 
                         crs = 4326, 
                         agr = "constant")

#Map
world <- ne_countries(scale = "medium", returnclass = "sf")
map_plot <- ggplot(data = world) +
  geom_sf() + 
  geom_sf(data = sf_locations, aes(size = 0.5)) +
  #coord_sf(xlim = c(-16.1, 40), ylim = c(30, 70), expand = TRUE)+ #Here you can only pick a part of the world
  theme_light()+
  theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")
  )
map_plot
