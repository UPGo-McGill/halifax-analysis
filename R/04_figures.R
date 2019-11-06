#### HALIFAX ANALYSIS ##########################################################

source("R/01_helper_functions.R")

load("data/active_listings_filtered.Rdata")
load("data/HRM_property.Rdata")
load("data/HRM.Rdata")
load("data/HRM_daily.Rdata")
load("data/housing_loss.Rdata")
load("data/airbnb_neighbourhoods.Rdata")
load("data/HRM_streets.Rdata")
load("data/HRM_GH.Rdata")
load("data/HRM_FREH.Rdata")
load("data/CTs_halifax.Rdata")
load("data/neighbourhoods.Rdata")
load("data/legal.Rdata")

# Set up dates
start_date <- as.Date("2018-09-01")
end_date <- as.Date("2019-08-31")
date_yoy <- as.Date("2018-08-31")

# Exchange rate (average over last twelve months)
exchange_rate <- mean(1.3037, 1.3010, 1.3200,
                      1.3432, 1.3301, 1.3206, 
                      1.3368, 1.3378, 1.3438,
                      1.3188, 1.3046, 1.3316)


### FIGURE 1 - spatial distribution of listings ################################

property_in_HRM <-
  property %>% 
  select(-revenue) %>% 
  st_intersection(st_buffer(HRM, 250))

property_2016 <- 
  daily %>% 
  filter(status == "R", date >= "2015-09-01", date <= "2016-08-31") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2016") %>% 
  filter(revenue > 0)

property_2017 <- 
  daily %>% 
  filter(status == "R", date >= "2016-09-01", date <= "2017-08-31") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2017") %>% 
  filter(revenue > 0)

property_2018 <- 
  daily %>% 
  filter(status == "R", date >= "2017-09-01", date <= "2018-08-31") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2018") %>% 
  filter(revenue > 0)

property_2019 <- 
  daily %>% 
  filter(status == "R", date >= start_date, date <= end_date) %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2019") %>% 
  filter(revenue > 0)

map <- 
  rbind(property_2016, property_2017, property_2018, property_2019) %>%
  ggplot() +
  geom_sf(data = HRM_streets, colour = alpha("grey", 0.3), lwd = 0.2) +
  geom_sf(aes(size = revenue, colour = listing_type), stroke = 0, alpha = 0.4, 
          show.legend = "point") +
  facet_wrap(vars(Year), nrow = 2) +
  scale_colour_manual(name = "Listing type",
                      values = c("#A84268", "#9DBF9E", "#FCB97D")) +
  scale_size_continuous(name = "Annual revenue",
                        breaks = c(20000, 40000, 60000, 80000, 100000),
                        labels = c("$20,000", "$40,000", "$60,000", "$80,000",
                                   "$100,000"),
                        range = c(0.15, 3)) +
  guides(size = guide_legend(nrow = 3, byrow = TRUE),
         colour = guide_legend(
           override.aes = list(fill = c("#A84268", "#9DBF9E", "#FCB97D"), 
                               alpha = 1), nrow = 3, byrow = TRUE)) +
  theme(legend.position = "bottom",
        legend.spacing.y = unit(10, "pt"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        strip.text = element_text(family = "Futura", face = "bold", size = 12))

ggsave("output/figure_1.pdf", plot = map, width = 8, height = 9, units = "in",
       useDingbats = FALSE)



### FIGURE 2 - active listings #################################################

active_listings_graph <-
  daily %>% 
  filter(housing == TRUE, status != "U", date >= created, 
         date <= scraped) %>% 
  count(date) %>% 
  mutate(n = data.table::frollmean(n, 7)) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "#A84268", size = 1.5) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  theme_minimal() +
  scale_x_date(name = NULL, limits = c(as.Date("2016-05-08"), NA)) +
  theme(text = element_text(family = "Futura"))

ggsave("output/figure_2.pdf", plot = active_listings_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)



### FIGURE 3 - neighbourhoods ##################################################

names <- read_csv("data/names.csv")

neighbourhoods <- 
  neighbourhoods %>% 
  select(-name) %>% 
  left_join(names) %>% 
  mutate(urban_rural = case_when(
    urban_rural == "dartmouth" ~ "Dartmouth",
    urban_rural == "halifax" ~ "Halifax",
    urban_rural == "other_urban" ~ "Suburban",
    urban_rural == "rural" ~ "Rural",
    TRUE ~ urban_rural
  ))

main_neighbourhood_map <-
  ggplot(st_simplify(neighbourhoods, dTolerance = 10)) +
  geom_sf(aes(fill = urban_rural), lwd = 0.2, colour = "white") +
  geom_rect(xmin = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[1]],
            ymin = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[2]],
            xmax = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[3]],
            ymax = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[4]],
            fill = NA, colour = "black", size = 1) +
  ggrepel::geom_text_repel(
    aes(x = st_centroid(geometry) %>% st_coordinates() %>% `[`(,1),
        y = st_centroid(geometry) %>% st_coordinates() %>% `[`(,2),
        label = name, family = "Futura", size = 1.5),
    data = neighbourhoods %>% 
      filter(!urban_rural %in% c("Dartmouth", "Halifax")),
    nudge_x = c(-12000, 0,     0,     0,     0, 
                20000,  0,     -5000, 25000, 25000,
                -20000, 0,     10000, 0,     -10000,
                -10000, 35000, 10000, 10000, -10000,
                0,      20000, 20000, 12000, 0, 
                20000),
    nudge_y = c(0,      -5000, 0,     0,     0,
                0,      22000, 15000, 15000, 30000, 
                0,      0,     -8000, 15000, 0, 
                0,      0,     0,     5000,  -10000,
                -10000, 0,     -6000, 0,     0,
                5000),
    size = 2, segment.size = 0.2) +
  scale_fill_manual(name = "Sub-area", 
                    values = c("#9DBF9E", "#A84268", "#FCB97D", "#4A6C6F")) +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))


subset <- 
  neighbourhoods %>% filter(urban_rural %in% c("Dartmouth", "Halifax"))

inset_neighbourhood_map <-
  subset %>% 
  st_simplify(dTolerance = 10) %>% 
  ggplot() +
  # geom_sf(data = CMHC, fill = "grey90", lwd = 0, colour = "transparent") +
  geom_sf(aes(fill = urban_rural), lwd = 0.2, colour = "white",
          data = neighbourhoods) +
  geom_rect(xmin = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[1]],
            ymin = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[2]],
            xmax = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[3]],
            ymax = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[4]],
            fill = NA, colour = "black", size = 1) +
  ggrepel::geom_text_repel(
    aes(x = st_centroid(geometry) %>% st_coordinates() %>% `[`(,1),
        y = st_centroid(geometry) %>% st_coordinates() %>% `[`(,2),
        label = name),
    nudge_x = c(0,    0,    0,    -6000, 0, 
                0,    0,    2000, 0,     0, 
                0,    2000, 0,    -3000, -3000, 
                0,    3000, 0,    0),
    nudge_y = c(0,    0,    0,    0,     0, 
                1500, 0,    0,    -2000, 0, 
                0,    0,    0,    0,     2000, 
                0,    0,    0,    0),
    min.segment.length = 0.2,
    family = "Futura", size = 1.5, segment.size = 0.2) +
  scale_fill_manual(name = "Sub-area", 
                    values = c("#9DBF9E", "#A84268", "#FCB97D", "#4A6C6F")) +
  theme(legend.position = "none",
        rect = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "Futura", face = "plain")) +
  gg_bbox(subset, expand = FALSE)

  
neighbourhood_map <- 
  ggdraw(clip = "on") +
  draw_plot(main_neighbourhood_map) +
  draw_plot(inset_neighbourhood_map,
    x = 0.62, 
    y = 0,
    width = 0.42, 
    height = 0.42)

ggsave("output/figure_3.pdf", plot = neighbourhood_map, width = 8, height = 6, 
       units = "in", useDingbats = FALSE)
  


### FIGURE 4 - Nova Scotia map #################################################

main_nova_scotia <-
  DA_NS %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 5) %>% 
  ggplot() +
  geom_sf(aes(fill = n / Dwellings), lwd = 0, colour = "white") +
  geom_rect(
    xmin = st_bbox(filter(neighbourhoods, 
                          urban_rural %in% c("halifax", "dartmouth")))[[1]],
    ymin = st_bbox(filter(neighbourhoods,
                          urban_rural %in% c("halifax", "dartmouth")))[[2]],
    xmax = st_bbox(filter(neighbourhoods,
                          urban_rural %in% c("halifax", "dartmouth")))[[3]],
    ymax = st_bbox(filter(neighbourhoods, 
                          urban_rural %in% c("halifax", "dartmouth")))[[4]],
            fill = NA, colour = "black", size = 0.6) +
  scale_fill_gradientn(colors = c("#9DBF9E", "#FCB97D", "#A84268"),
                       na.value = "grey80",
                       limits = c(0, 0.1),
                       oob = scales::squish,
                       labels = scales::percent) +
  coord_sf(expand = FALSE) +
  guides(fill = guide_colorbar(
    title = "Active STRs as share of total dwellings")) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, .95),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))

nova_scotia_map <- 
  ggdraw(clip = "on") +
  draw_plot(main_nova_scotia) +
  draw_plot(
    {main_nova_scotia + 
        gg_bbox(filter(neighbourhoods, 
                       urban_rural %in% c("halifax", "dartmouth")),
                expand = FALSE) +
        theme(legend.position = "none")},
    x = 0.58, 
    y = 0,
    width = 0.46, 
    height = 0.46)

ggsave("output/figure_4.pdf", plot = nova_scotia_map, width = 8,
       height = 6.5, units = "in", useDingbats = FALSE)




### FIGURE 5 - bedroom breakdowns ##############################################

var <- filter(property, created <= end_date, scraped >= end_date, 
              housing == TRUE, listing_type == "Entire home/apt")$bedrooms
nrows <- 20
df <- expand.grid(y = 1:20, x = 1:20)
categ_table <- round(table(var) * ((20*20)/(length(var))))
# note: sum from categ_table such that there are only 6 categories
# manually enter the following numbers.
categ_table <- c(25, 132, 128, 84, 21, 10)
names(categ_table) <- c("0 (studio)", "1",  "2", "3", "4", "5+")
df$category <- factor(rep(names(categ_table), categ_table))  

bedrooms_graph <-
  ggplot(df, aes(x = x, y = y, fill = category)) +
  geom_tile(color = "white", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual(name = "Bedrooms", 
                    values = c("#9DBF9E", "#A84268", "#FCB97D", "#C0BCB5",
                               "#4A6C6F", "#FF5E5B")) +
  theme(plot.title = element_text(size = rel(1.2)),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom", 
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))

ggsave("output/figure_5.pdf", plot = bedrooms_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)



### FIGURE 6 - host revenue percentiles graph ##################################

revenue_graph <-
  daily %>%
  filter(housing == TRUE, date >= start_date, status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)*exchange_rate) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev))) %>% 
  gather(`Top 1%`, `Top 5%`, `Top 10%`, `Top 20%`, key = "percentile", 
         value = "value") %>% 
  mutate(percentile = factor(percentile, 
                             levels = c('Top 1%', 'Top 5%', 'Top 10%', 
                                        'Top 20%'))
  ) %>% 
  ggplot() +
  geom_bar(aes(percentile, value), stat = "identity", fill = "#A84268") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        legend.position = "none")

ggsave("output/figure_6.pdf", plot = revenue_graph, width = 8, height = 4, 
       units = "in", useDingbats = FALSE)


### FIGURE 7 - multilistings graph #############################################

ML_summary <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(ML),
            Revenue = sum(price * (status == "R") * ML * exchange_rate, 
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

ggsave("output/figure_7.pdf", plot = ML_graph, width = 8, height = 6, 
       units = "in", useDingbats = FALSE)

  
### FIGURE 8 - housing loss ####################################################

housing_graph <- 
  ggplot(housing_loss) +
  geom_col(aes(date, `Housing units`, fill = `Listing type`),
           lwd = 0) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 800)) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-10-01"), NA)) +
  scale_fill_manual(values = c("#9DBF9E", "#A84268")) +
  theme(legend.position = "bottom", 
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))

ggsave("output/figure_8.pdf", plot = housing_graph, width = 8, height = 7, 
       units = "in", useDingbats = FALSE)



### FIGURE 9 - housing loss map ################################################

main_housing_nbhd <-
  airbnb_neighbourhoods %>% 
  ggplot() +
  geom_sf(aes(fill = housing_loss_pct, geometry = geometry), lwd = 0, 
          colour = "white") +
  geom_rect(xmin = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[1]],
            ymin = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[2]],
            xmax = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[3]],
            ymax = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[4]],
            fill = NA, colour = "black", size = 1) +
  scale_fill_gradientn(colors = c("#9DBF9E", "#FCB97D", "#A84268"),
                       values = (c(0, 0.5, 1)),
                       limits = c(0, 0.02),
                       oob = scales::squish,
                       labels = scales::percent) + 
  coord_sf(expand = FALSE) +
  # geom_sf(data = HRM_streets, colour = alpha("grey", 0.5)) +
  guides(fill = guide_colorbar(
    title = "Percentage of housing lost to STRs")) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))

housing_nbhd <- 
  ggdraw(clip = "on") +
  draw_plot(main_housing_nbhd) +
  draw_plot(
    {main_housing_nbhd + 
      gg_bbox(filter(airbnb_neighbourhoods, 
                     urban_rural %in% c("Halifax", "Dartmouth")),
              expand = FALSE) +
        theme(legend.position = "none")},
    x = 0.62, 
    y = 0,
    width = 0.42, 
    height = 0.42)

ggsave("output/figure_9.pdf", plot = housing_nbhd, width = 8, height = 6, 
       units = "in", useDingbats = FALSE)



### FIGURE 10 - legal listings (principal residences) ###########################

main_legal <-
  legal %>% 
  mutate(legal = if_else(legal == TRUE, FALSE, TRUE)) %>% 
  ggplot() +
  geom_sf(data = HRM_streets, colour = alpha("grey", 0.3), lwd = 0.2) +
  geom_sf(aes(colour = legal), size = 0.7, stroke = 0, alpha = 0.5,
          show.legend = "point") +
  geom_rect(xmin = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[1]],
            ymin = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[2]],
            xmax = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[3]],
            ymax = st_bbox(filter(airbnb_neighbourhoods, 
                                  urban_rural %in% c("Halifax",
                                                     "Dartmouth")))[[4]],
            fill = NA, colour = "black", size = 1) +
  scale_colour_manual(name = "Listings needing to register",
                      values = c("#9DBF9E", "#A84268")) + 
  guides(colour = guide_legend(
    override.aes = list(colour = c("#9DBF9E", "#A84268"),
                        fill = c("#9DBF9E", "#A84268"), alpha = 1, size = 3,
                        shape = 21))) +
  coord_sf(expand = FALSE) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))

legal_map <- 
  ggdraw(clip = "on") +
  draw_plot(main_legal) +
  draw_plot(
    {main_legal + 
        gg_bbox(filter(airbnb_neighbourhoods, 
                       urban_rural %in% c("Halifax", "Dartmouth")),
                expand = FALSE) +
        theme(legend.position = "none")},
    x = 0.62, 
    y = 0,
    width = 0.42, 
    height = 0.42)

ggsave("output/figure_10.pdf", plot = legal_map, width = 8, height = 6, 
       units = "in", useDingbats = FALSE)


### FIGURE 11 - vacancy rate change ############################################

vacancy_rate_map <-
  CMHC_tidy %>% 
  st_as_sf() %>% 
  mutate(name = if_else(name == "rental_vacancy", "Current vacancy rate",
                        "Post-regulation vacancy rate"),
         name = factor(
           name, 
           levels = c("Current vacancy rate",
                      "Post-regulation vacancy rate")),
         pct = paste0(substr(value, 4, 4), ".", 
                      substr(round(CMHC_tidy$value, 3), 5, 5), "%"),
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

ggsave("output/figure_11.pdf", plot = vacancy_rate_map, width = 8, height = 4, 
       units = "in", useDingbats = FALSE)
