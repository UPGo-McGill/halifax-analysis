### March SHIFT conference update ##############################################

library(tidyverse)
library(upgo)
library(strr)
library(sf)
library(extrafont)
library(lubridate)
library(gganimate)
library(future)
plan(multiprocess)

upgo_connect()

property <- 
  property_all %>% 
  filter(country == "Canada", city == "Halifax Regional Municipality") %>% 
  collect()

daily <- 
  daily_all %>% 
  filter(property_ID %in% !! property$property_ID) %>% 
  collect()

host <- 
  host_all %>% 
  filter(host_ID %in% !! property$host_ID) %>% 
  collect()

daily <- 
  strr_expand(daily)

host <- 
  strr_expand(host)

daily <- 
  daily %>% 
  strr_multi(host)

exchange_rate <- 
  map_dbl(0:11, ~{ 
    ex_table <- 
      fixerapi::fixer_historical(
        date = (as.Date("2019-12-31") %m-% months(.x)), symbols = c("CAD", "USD"))
    ex_table[1,]$value / ex_table[2,]$value
  }) %>% mean()

FREH <-
  daily %>% 
  filter(housing) %>% 
  strr_FREH("2017-01-01", "2019-12-31")

GH <- 
  property %>% 
  filter(housing) %>% 
  strr_ghost("2016-07-01", "2019-12-31")
  
### Figures ####################################################################

active_listings_graph <-
  daily %>% 
  filter(housing == TRUE, status != "U") %>% 
  count(date) %>% 
  mutate(n = data.table::frollmean(n, 7)) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "#A84268", size = 1.5) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  theme_minimal() +
  scale_x_date(name = NULL, limits = c(as.Date("2016-05-08"), NA)) +
  theme(text = element_text(family = "Futura"))

ggsave("output/SHIFT/figure_2.pdf", plot = active_listings_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)
embed_fonts("output/SHIFT/figure_2.pdf")



ML_summary <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price * (status == "R") * multi * exchange_rate, 
                          na.rm = TRUE) / sum(price * (status == "R") * 
                                                exchange_rate, na.rm = TRUE))

ML_graph <-
  ML_summary %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value) %>% 
  ggplot() +
  geom_line(aes(date, Value, colour = `Multilisting percentage`), alpha = 0.2) +
  geom_smooth(aes(date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.25) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-06-01"), NA)) +
  scale_colour_manual(values = c("#9DBF9E", "#A84268")) +
  theme(legend.position = "bottom", 
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))

ggsave("output/SHIFT/figure_7.pdf", plot = ML_graph, width = 8, height = 6, 
       units = "in", useDingbats = FALSE)
embed_fonts("output/SHIFT/figure_7.pdf")



GH_total <-
  GH %>%
  st_drop_geometry() %>%
  group_by(date) %>%
  summarize(GH_units = sum(housing_units)) %>%
  mutate(GH_average = data.table::frollmean(GH_units, 30, align = "right")) %>%
  mutate(GH_average = if_else(is.na(GH_average), 8, GH_average)) %>%
  select(-GH_units)

housing_loss <-
  FREH %>%
  filter(FREH == TRUE) %>% 
  group_by(date) %>%
  summarize(`Entire home/apt` = n()) %>%
  left_join(GH_total, by = "date") %>%
  rename(`Private room` = GH_average) %>%
  gather(`Entire home/apt`, `Private room`, 
         key = `Listing type`, value = `Housing units`) 



housing_graph <- 
  ggplot(housing_loss) +
  geom_col(aes(date, `Housing units`, fill = `Listing type`),
           lwd = 0) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 800)) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-01-01"), NA)) +
  scale_fill_manual(values = c("#9DBF9E", "#A84268")) +
  theme(legend.position = "bottom", 
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))

ggsave("output/SHIFT/figure_8.pdf", plot = housing_graph, width = 8, height = 7, 
       units = "in", useDingbats = FALSE)
embed_fonts("output/SHIFT/figure_8.pdf")




### Rent increases #############################################################

listings_2016 <- 
  property %>% 
  filter(housing, created <= "2016-01-01", scraped >= "2016-01-01") %>% 
  nrow()

listings_2017 <- 
  property %>% 
  filter(housing, created <= "2017-01-01", scraped >= "2017-01-01") %>% 
  nrow()

listings_2018 <- 
  property %>% 
  filter(housing, created <= "2018-01-01", scraped >= "2018-01-01") %>% 
  nrow()


listings_2019 <- 
  property %>% 
  filter(housing, created <= "2019-01-01", scraped >= "2019-01-01") %>% 
  nrow()

listings_2020 <- 
  property %>% 
  filter(housing, created <= "2020-01-01", scraped >= "2020-01-01") %>% 
  nrow()

magic <- 0.00651547619

rent_2017 <- listings_2018 / listings_2017 * magic
rent_2018 <- listings_2019 / listings_2018 * magic
rent_2019 <- listings_2020 / listings_2019 * magic

rent_increase_2019 <- 0.038
rent_increase_2018 <- 0.021
rent_increase_2017 <- 0.023

total_rent_increase <- 
  (1 + rent_increase_2017) * (1 + rent_increase_2018) * (1 + rent_increase_2019) - 1
str_rent_increase <- (1 + rent_2017) * (1 + rent_2018) * (1 + rent_2019) - 1

proportion <- str_rent_increase / total_rent_increase
  
#' halifax vacancy rate down to 1.0%



### Animation ##################################################################

map_point <- 
  st_sfc(st_point(x = c(-63.585499, 44.654283)), crs = 4326) %>% 
  st_transform(32617)

buffer <- 
  map_point %>% 
  st_buffer(2000)

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

anim_save("output/SHIFT/halifax_animation.gif",
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

ggsave("output/SHIFT/halifax_intro.png", plot = map_intro, width = 6, height = 6)




GH_listings <- 
  GH %>% 
  filter(date == "2019-12-31") %>% 
  pull(property_IDs) %>% 
  unlist %>% 
  unique()

GH_map_0 <-
  property %>% 
  strr_as_sf(32617) %>% 
  filter(listing_type == "Private room", scraped >= "2019-12-31", created <= "2019-12-31") %>%
  st_filter(buffer) %>% 
  ggplot() +
  geom_sf(data = map_streets, size = 0.15, colour = alpha("grey60", 1)) +
  geom_sf(colour = "#335DA3", stroke = 0, alpha = 0.9) +
  theme_void() +
  gg_bbox(map_streets)

ggsave("output/SHIFT/GH_map_0.pdf", plot = GH_map_0, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

GH_map <-
  property %>% 
  strr_as_sf(32617) %>% 
  filter(property_ID %in% GH_listings) %>%
  st_filter(buffer) %>% 
  ggplot() +
  geom_sf(data = map_streets, size = 0.15, colour = alpha("grey60", 1)) +
  geom_sf(colour = "#335DA3", stroke = 0, alpha = 0.9) +
  theme_void() +
  gg_bbox(map_streets)

ggsave("output/SHIFT/GH_map.pdf", plot = GH_map, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

GH_map_2 <-
  GH %>% 
  st_transform(32617) %>% 
  filter(date == "2019-12-31") %>% 
  st_filter(buffer) %>% 
  ggplot() +
  geom_sf(data = map_streets, size = 0.15, colour = alpha("grey60", 1)) +
  geom_sf(aes(geometry = st_centroid(geometry)), colour = "#A14350", stroke = 0,
          alpha = 0.9, size = 2) +
  theme_void() +
  gg_bbox(map_streets)

ggsave("output/SHIFT/GH_map_2.pdf", plot = GH_map_2, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

GH %>% filter(date == "2019-12-31") %>% pull(listing_count) %>% sum()





daily %>% 
  filter(status == "R", date >= "2019-01-01") %>% 
  count(property_ID)

property %>% 
  filter(created <= "2019-12-31", scraped >= "2019-01-01")






CMHC <- read_sf("data/CMHC/CMHC_NBHD_2016-mercWGS84.shp")

CMHC <- 
  CMHC %>% 
  filter(METCODE == "0580") %>%
  select(-OBJECTID, -NBHDNAME_F, -NBHDNAME_1, -NBHDNAME_L, -NBHDNAME_E,
         -NAME_FR, -GEO_LAYER_, -SHAPE_Leng, -SHAPE_Area) %>% 
  group_by(ZONECODE) %>% 
  summarize() %>% 
  mutate(zone = c("Peninsula South", "Peninsula North", "Mainland South",
                  "Mainland North", "Dartmouth North", "Dartmouth South",
                  "Dartmouth East", "Bedford", "Sackville", "Remainder of CMA"),
         rental_units = c(9702, 6691, 2782, 14581, 7024, 3311, 2360, 1983, 1548,
                          1405),
         rental_vacancy = c(0.004, 0.011, 0.017, 0.010, 0.021, 0.01, 0.006,
                            0.008, 0.005, 0.02),
         condo_units = round(2623 * rental_units / sum(rental_units))) %>% 
  select(zone, rental_units, rental_vacancy, condo_units, geometry) %>% 
  st_transform(32617)

property <-
  property %>% 
  strr_as_sf(32617) %>% 
  st_intersection(select(CMHC, zone))

GH_CMHC <- 
  GH %>% 
  st_transform(32617) %>% 
  group_by(ghost_ID) %>% 
  slice(1) %>%
  ungroup() %>% 
  st_centroid() %>% 
  st_intersection(CMHC) %>% 
  st_drop_geometry() %>% 
  select(ghost_ID, zone = zone) %>% 
  arrange(ghost_ID)

GH <- 
  GH %>% left_join(GH_CMHC)

CMHC <-
  GH %>% 
  st_drop_geometry() %>% 
  filter(date >= "2019-12-31") %>% 
  select(-host_ID, -property_IDs, -data) %>% 
  group_by(date, zone) %>% 
  summarize(GH = sum(housing_units)) %>% 
  group_by(zone) %>% 
  summarize(GH = round(mean(GH, na.rm = TRUE))) %>% 
  left_join(CMHC, ., by = "zone") %>% 
  mutate(GH = replace_na(GH, 0)) %>% 
  select(zone:condo_units, GH, geometry)

CMHC <-
  FREH %>% 
  filter(date == "2019-12-31", FREH) %>% 
  left_join(select(st_drop_geometry(property), property_ID, zone)) %>% 
  count(zone) %>% 
  left_join(CMHC, .) %>% 
  mutate(FREH = n,
         housing_loss = GH + FREH,
         new_vacancy = (housing_loss * 0.8 + rental_vacancy * rental_units) / 
           rental_units) %>% 
  select(zone:condo_units, new_vacancy, GH, FREH, housing_loss, geometry)

CMHC_tidy <-
  CMHC %>% 
  select(zone, rental_vacancy, new_vacancy, geometry) %>% 
  pivot_longer(cols = c(rental_vacancy, new_vacancy))

vacancy_rate_map <-
  CMHC_tidy %>% 
  st_as_sf() %>% 
  mutate(name = if_else(name == "rental_vacancy", "Current vacancy rate",
                        "Post-regulation vacancy rate"),
         name = factor(
           name, 
           levels = c("Current vacancy rate",
                      "Post-regulation vacancy rate")),
         pct = paste0(substr(round(value, 3), 4, 4), ".", 
                      substr(round(value, 3), 5, 5), "%"),
         pct = if_else(nchar(pct) == 4, pct, paste0(substr(pct, 1, 2), 
                                                    "0%"))) %>% 
  filter(zone != "Remainder of CMA") %>% 
  ggplot() +
  geom_sf(data = CMHC, fill = "grey90", lwd = 0, colour = "transparent") +
  geom_sf(data = HRM_streets$geometry, colour = alpha("grey", 0.3), lwd = 0.2) +
  geom_sf(aes(fill = value), lwd = 0, colour = "white", alpha = 0.6) +
  geom_sf_label(aes(label = pct), size = 1.5, family = "Futura") +
  scale_fill_gradientn(colors = c("#A84268", "#FCB97D", "#9DBF9E"),
                       values = (c(0, 0.5, 1)),
                       limits = c(0.01, 0.03),
                       oob = scales::squish,
                       labels = scales::percent) +
  coord_sf(expand = FALSE) +
  facet_wrap(vars(name), nrow = 1) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank(),
        legend.position = "none",
        text = element_text(family = "Futura", face = "plain"),
        strip.text = element_text(family = "Futura", face = "bold", 
                                  size = 12)) +
  gg_bbox(CMHC %>% slice(1:9))

ggsave("output/SHIFT/figure_11.pdf", plot = vacancy_rate_map, width = 8, height = 4, 
       units = "in", useDingbats = FALSE)
embed_fonts("output/SHIFT/figure_11.pdf")

