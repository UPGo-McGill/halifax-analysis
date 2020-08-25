#### Halifax animation #########################################################

library(tidyverse)
library(sf)
library(osmdata)
library(gganimate)
library(upgo)
library(strr)
library(extrafont)


### Import data ################################################################

map_point <- 
  st_sfc(st_point(x = c(-63.575094, 44.642584)), crs = 4326) %>% 
  st_transform(32617)

buffer <- 
  map_point %>% 
  st_buffer(1000)

map_streets <- 
  HRM_streets %>% 
  st_intersection(buffer)

map_property <- 
  property %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32617) %>% 
  st_intersection(st_sf(name = "map", geometry = buffer)) %>% 
  select(property_ID, listing_type)

map_daily <- 
  daily %>% 
  as_tibble() %>% 
  filter(property_ID %in% map_property$property_ID, 
         date >= "2019-08-01", date <= "2019-08-31", status == "R")

map_points <- 
  map_daily %>% 
  select(property_ID, date, price, listing_type) %>% 
  left_join(map_property) %>% 
  st_as_sf() %>% 
  select(property_ID:listing_type)

map_animation <-
  map_points %>% 
  ggplot() +
  geom_sf(data = map_streets, size = 0.15, colour = alpha("grey60", 1)) +
  geom_sf(data = map_point, colour = "black") +
  geom_sf(aes(colour = listing_type, size = price, group = property_ID),
          alpha = 0.5) +
  scale_size(range = c(.1, 5), guide = FALSE) +
  scale_color_discrete(name = "Listing type") +
  theme_minimal() +
  theme(text = element_text(family = "Futura-Medium"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        legend.justification = c(0, 0)) +
  transition_time(date) +
  ggtitle("Reservations on {frame_time}")

anim_save("output/halifax_animation.gif",
          animation = map_animation,
          width = 6, height = 6, units = "in", res = 150, nframes = 30, 
          fps = 3)


map_intro <-
  ggplot() +
  geom_sf(data = map_streets, size = 0.15, colour = alpha("grey60", 1)) +
  geom_sf(data = map_point, colour = "black") +
  theme_minimal() +
  theme(text = element_text(family = "Futura-Medium"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        legend.justification = c(0, 0))

ggsave("output/halifax_intro.png", plot = map_intro, width = 6, height = 6)




GH_listings <- GH %>% 
  filter(date == "2019-08-31") %>% 
  pull(property_IDs) %>% 
  unlist %>% 
  unique()

GH_map <- 
  property %>% 
  filter(property_ID %in% GH_listings) %>%
  ggplot() +
  geom_sf(data = HRM_streets, size = 0.15, colour = alpha("grey60", 1)) +
  geom_sf(colour = "#335DA3", stroke = 0, alpha = 0.9) +
  theme_void() +
  gg_bbox(filter(neighbourhoods, neighbourhood %in% c("H_1", "H_2", "H_3", "H_4",
                                                      "H_5", "H_6", "H_11")))


ggsave("output/GH_map.pdf", plot = GH_map, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)
embed_fonts("output/GH_map.pdf")

GH_map_2 <- 
  GH %>% 
  filter(date == "2019-08-31") %>% 
  ggplot() +
  geom_sf(data = HRM_streets, size = 0.15, colour = alpha("grey60", 1)) +
  geom_sf(aes(geometry = st_centroid(geometry)), colour = "#A14350", stroke = 0,
          alpha = 0.9, size = 2) +
  theme_void() +
  gg_bbox(filter(neighbourhoods, neighbourhood %in% c("H_1", "H_2", "H_3", "H_4",
                                                      "H_5", "H_6", "H_11")))


ggsave("output/GH_map_2.pdf", plot = GH_map_2, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)
embed_fonts("output/GH_map_2.pdf")


