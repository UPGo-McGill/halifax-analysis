#### HALIFAX ANALYSIS ##########################################################

source("R/01_helper_functions.R")

load("data/active_listings_filtered.Rdata")
load("data/HRM_property.Rdata")
load("data/HRM.Rdata")
load("data/HRM_daily.Rdata")
load("data/housing_loss.Rdata")
load("data/airbnb_neighbourhoods.Rdata")
load("data/HRM_streets.Rdata")

# Set up dates
start_date <- "2018-09-01"
end_date <- "2019-08-31"
date_yoy <- "2018-08-31"


### FIGURE 1 - active listings
active_listings_graph <-
  active_listings_filtered %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "#4295A8", size = 1.5) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = comma) +
  theme_minimal() +
  scale_x_date(name = NULL)
#+
#theme(text = element_text(family = "Futura"))

ggsave("output/figure_1.pdf", plot = active_listings_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

### FIGURE 2 - spatial distribution of listings 
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
  geom_sf(data = HRM_streets, colour = alpha("grey", 0.5)) +
  geom_sf(aes(size = revenue, colour = listing_type), alpha = 0.2, 
          show.legend = "point") +
  facet_wrap(vars(Year), nrow = 2) +
  scale_colour_manual(name = "Listing type",
                      values = c("#4295A8", "#B4656F", "#C7F2A7")) +
  scale_size_continuous(name = "Annual revenue",
                        breaks = c(20000, 40000, 60000, 80000, 100000),
                        labels = c("$20,000", "$40,000", "$60,000", "$80,000",
                                   "$100,000"),
                        range = c(0.05, 2.5)) +
  guides(size = guide_legend(nrow = 3, byrow = TRUE),
         colour = guide_legend(
           override.aes = list(fill = c("#4295A8", "#B4656F", "#C7F2A7"), 
                               alpha = 1), nrow = 3, byrow = TRUE)) +
  theme(legend.position = "bottom",
        legend.spacing.y = unit(10, "pt"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank())
#text = element_text(family = "Futura", face = "plain"),
# legend.title = element_text(family = "Futura", face = "bold", 
#                           size = 10),
#legend.text = element_text(family = "Futura", size = 10),
#strip.text = element_text(family = "Futura", face = "bold", size = 12)) 

ggsave("output/figure_3.pdf", plot = map, width = 8, height = 9, units = "in",
       useDingbats = FALSE)

## FIGURE 3 - bedroom breakdowns

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

bedroom_graph <-
  ggplot(df, aes(x = x, y = y, fill = category)) +
  geom_tile(color = "white", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual(name = "Bedrooms", 
                    values = c("#4295A8", "#B4656F", "#C7F2A7", "#96897B",
                               "#DFD5A5", "#FFCAB1")) +
  theme(plot.title = element_text(size = rel(1.2)),
        panel.border = element_rect(size = 1, fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")
#   legend.title = element_text(family = "Futura", face = "bold",
#                           size = 10),
#  legend.text = element_text(family = "Futura", size = 10)
# )

ggsave("output/figure_4.pdf", plot = bedroom_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

### FIGURE 4 - host revenue percentiles graph

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
                             levels = c('Top 1%', 'Top 5%', 'Top 10%', 'Top 20%'))
  ) %>% 
  ggplot() +
  geom_bar(aes(percentile, value), stat = "identity", fill = "#4295A8") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        #text = element_text(family = "Futura", face = "plain"),
        # legend.title = element_text(family = "Futura", face = "bold", 
        #                            size = 10),
        #legend.text = element_text(family = "Futura", size = 10),
        legend.position = "none")

ggsave("output/figure_5.pdf", plot = revenue_graph, width = 8, height = 4, 
       units = "in", useDingbats = FALSE)

### FIGURE 5 - multilistings graph
ML_summary <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(ML),
            Revenue = sum(price * (status == "R") * ML * exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE))

ML_graph <- 
  ML_summary %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value) %>% 
  ggplot() +
  geom_line(aes(date, Value, colour = `Multilisting percentage`), alpha = 0.2) +
  geom_smooth(aes(date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.25) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_date(name = NULL) +
  scale_colour_manual(values = c("#4295A8", "#B4656F")) +
  theme(legend.position = "bottom")
#text = element_text(family = "Futura", face = "plain"),
#legend.title = element_text(family = "Futura", face = "bold", 
#                            size = 10),
#legend.text = element_text(family = "Futura", size = 10))

ggsave("output/figure_6.pdf", plot = ML_graph, width = 8, height = 7, 
       units = "in", useDingbats = FALSE)

  # alternate multilistings graph
ML_table <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(ML),
            Revenue = sum(price * (status == "R") * ML * exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE)) %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value)

ML_table %>% 
  filter(date == end_date)

ML_table %>% 
  ggplot() +
  #geom_line(aes(Date, Value, colour = `Multilisting percentage`)) +
  geom_smooth(aes(date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.2) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = percent)

### FIGURE 6 - housing loss

housing_graph <- 
  ggplot(housing_loss) +
  geom_col(aes(date, `Housing units`, fill = `Listing type`),
           lwd = 0) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 1000)) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-05-01"), NA)) +
  scale_fill_manual(values = c("#4295A8", "#B4656F")) +
  theme(legend.position = "bottom")
      #  text = element_text(family = "Futura", face = "plain"),
       # legend.title = element_text(family = "Futura", face = "bold", 
       #                             size = 10),
       # legend.text = element_text(family = "Futura", size = 10))

ggsave("output/figure_7.pdf", plot = housing_graph, width = 8, height = 7, 
       units = "in", useDingbats = FALSE)

### FIGURE 7 - housing loss as percentage of dwellings
airbnb_neighbourhoods %>% 
  ggplot() +
  geom_sf(aes(fill = housing_loss_pct, geometry = geometry)) +
  scale_fill_gradientn(colors = c("darkblue", "lightblue", "white"),
                       values = (c(0, 0.6, 1)),
                       limits = c(0, 0.028)) + 
  # geom_sf(data = HRM_streets, colour = alpha("grey", 0.5)) +
  guides(fill = guide_colorbar(title = "Percentage of housing lost to short-term rentals"))+
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_blank(),
        legend.justification = c(1,0.1),
        legend.position = c(1,0.1)) 

### FIGURE 8 - illegal listings
  legal %>%
    filter(legal == TRUE) %>% 
  ggplot() +
  geom_sf(data = HRM_streets, colour = alpha("grey", 0.5)) +
  geom_sf(aes(colour = legal), alpha = 0.2, 
          show.legend = FALSE) +
  scale_colour_manual(name = "Listings operating out of non-primary residences",
                      values = c("#4295A8")) + 
  theme(legend.position = "bottom",
        legend.spacing.y = unit(10, "pt"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        rect = element_blank())
  
 # colour palette:  #B4656F","#4295A8"