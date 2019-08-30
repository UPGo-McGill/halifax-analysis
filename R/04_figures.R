#### HALIFAX ANALYSIS ##########################################################

source("R/01_helper_functions.R")

load("data/HRM_property.Rdata")
load("data/HRM_LTM_property.Rdata")
load("data/HRM_FREH.Rdata")
load("data/HRM_GH.Rdata")
load("data/HRM_daily.Rdata")
load("data/HRM_streets.Rdata")
load("data/HRM.Rdata")
load("data/HRM_daily_compressed.Rdata")
load("data/airbnb_neighbourhoods.Rdata")

# Set up dates
start_date <- "2018-05-01"
end_date <- "2019-04-30"
date_yoy <- "2018-04-30"


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

### FIGURE 2 - STR activity in  Canada - not included

### FIGURE 3 - spatial distribution of listings 
exchange_rate <- mean(1.2873,	1.3129, 1.3130, 1.3041, 1.3037, 1.3010, 1.3200,
                      1.3432, 1.3301, 1.3206, 1.3368, 1.3378)

property_in_HRM <-
  property %>% 
  select(-revenue) %>% 
  st_intersection(st_buffer(HRM, 250))

property_2016 <- 
  daily %>% 
  filter(status == "R", date >= "2015-05-01", date <= "2016-04-30") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2016") %>% 
  filter(revenue > 0)

property_2017 <- 
  daily %>% 
  filter(status == "R", date >= "2016-05-01", date <= "2017-04-30") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2017") %>% 
  filter(revenue > 0)

property_2018 <- 
  daily %>% 
  filter(status == "R", date >= "2017-05-01", date <= "2018-04-30") %>% 
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

## FIGURE 4 - bedroom breakdowns

var <- filter(property, created <= end_date, scraped >= end_date, 
              housing == TRUE, listing_type == "Entire home/apt")$bedrooms
nrows <- 20
df <- expand.grid(y = 1:20, x = 1:20)
categ_table <- round(table(var) * ((20*20)/(length(var))))
# note: sum from categ_table such that there are only 5 categories
# manually enter the following numbers.
categ_table <- c(23, 135, 130, 78, 25, 9)
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

### FIGURE 5 - host revenue percentiles graph

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

### FIGURE 6 - multilistings graph
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

### FIGURE 7 - housing loss
GH_total <- 
  GH %>% 
  st_drop_geometry() %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>% 
  pull(GH_units) %>% 
  rollmean(365, align = "right")

housing_loss <- 
  FREH %>% 
  group_by(date) %>% 
  summarize(`Entire home/apt` = n()) %>% 
  mutate(`Private room` = as.integer(GH_total)) %>% 
  gather(`Entire home/apt`, `Private room`, key = `Listing type`,
         value = `Housing units`)

housing_graph <- 
  ggplot(housing_loss) +
  geom_col(aes(date, `Housing units`, fill = `Listing type`),
           lwd = 0) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 6000)) +
  scale_x_date(name = NULL, limits = c(as.Date("2015-09-30"), NA)) +
  scale_fill_manual(values = c("#4295A8", "#B4656F")) +
  theme(legend.position = "bottom")
      #  text = element_text(family = "Futura", face = "plain"),
       # legend.title = element_text(family = "Futura", face = "bold", 
       #                             size = 10),
       # legend.text = element_text(family = "Futura", size = 10))

ggsave("output/figure_7.pdf", plot = housing_graph, width = 8, height = 7, 
       units = "in", useDingbats = FALSE)

### FIGURE 8 - housing loss as percentage of dwellings
airbnb_neighbourhoods %>% 
  ggplot() +
  geom_sf(aes(fill = housing_loss_pct, geometry = geometry)) +
  scale_fill_gradientn(colors = c("steelblue4", "darkseagreen4", "darkseagreen1"),
                       values = rescale(c(0, 0.005, 0.015)),
                       limits = c(0, 0.015)) + 
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


########## NOT INCLUDED #####################
#### Canadian active listings and graph

Canada_population <- 
  get_census("CA16", regions = list(C = "01"), level = "CSD") %>% 
  filter(str_starts(`Region Name`, "Toronto|Montréal|Vancouver|Calgary|Ottawa"),
         Population > 300000)


property %>% 
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>% 
  nrow() %>% 
  {. / Canada_population[3,]$`Population` * 1000}

# Montreal

property_Montreal <- property_MTL

daily_Montreal <- 
  strr_expand_daily(daily_compressed_MTL, cores = 4)

daily_Montreal <- 
  property_Montreal %>% 
  select(property_ID, host_ID, listing_type, created, scraped, housing) %>% 
  left_join(daily_Montreal, .) %>% 
  filter(date >= created, date - 30 <= scraped, status != "U") %>% 
  mutate(city = "Montreal")

daily_Montreal <- 
  strr_expand_daily(daily_compressed_Montreal, cores = 4)

daily_Montreal <- 
  property_Montreal %>% 
  select(property_ID, host_ID, listing_type, created, scraped, housing) %>% 
  left_join(daily_Montreal, .) %>% 
  filter(date >= created, date - 30 <= scraped, status != "U") %>% 
  mutate(city = "Montreal")

Montreal_info <- list()
Montreal_info[[1]] <- 
  property_Montreal %>% 
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>% 
  nrow()
Montreal_info[[2]] <- 
  Montreal_info[[1]] / Canada_population[1,]$`Population` * 1000
Montreal_info[[3]] <- 
  daily_Montreal %>% 
  filter(housing == TRUE, status == "R", date >= "2018-05-01") %>% 
  pull(price) %>% 
  sum()
Montreal_info[[4]] <- 
  Montreal_info[[3]] / Montreal_info[[1]]

# Vancouver

property_Vancouver <- property_VAN

daily_Vancouver <- 
  strr_expand_daily(daily_compressed_VAN, cores = 4)

daily_Vancouver <- 
  property_Vancouver %>% 
  select(property_ID, host_ID, listing_type, created, scraped, housing) %>% 
  left_join(daily_Vancouver, .) %>% 
  filter(date >= created, date - 30 <= scraped, status != "U") %>% 
  mutate(city = "Vancouver")

daily_Vancouver <- 
  strr_expand_daily(daily_compressed_Vancouver, cores = 4)

daily_Vancouver <- 
  property_Vancouver %>% 
  select(property_ID, host_ID, listing_type, created, scraped, housing) %>% 
  left_join(daily_Vancouver, .) %>% 
  filter(date >= created, date - 30 <= scraped, status != "U") %>% 
  mutate(city = "Vancouver")

Vancouver_info <- list()
Vancouver_info[[1]] <- 
  property_Vancouver %>% 
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>% 
  nrow()
Vancouver_info[[2]] <- 
  Vancouver_info[[1]] / Canada_population[5,]$`Population` * 1000
Vancouver_info[[3]] <- 
  daily_Vancouver %>% 
  filter(housing == TRUE, status == "R", date >= "2018-05-01") %>% 
  pull(price) %>% 
  sum()
Vancouver_info[[4]] <- 
  Vancouver_info[[3]] / Vancouver_info[[1]]

# Ottawa

daily_Ottawa <- 
  strr_expand_daily(daily_compressed_Ottawa, cores = 4)

daily_Ottawa <- 
  property_Ottawa %>% 
  select(property_ID, host_ID, listing_type, created, scraped, housing) %>% 
  left_join(daily_Ottawa, .) %>% 
  filter(date >= created, date - 30 <= scraped, status != "U") %>% 
  mutate(city = "Ottawa")

daily_Ottawa <- 
  strr_expand_daily(daily_compressed_Ottawa, cores = 4)

daily_Ottawa <- 
  property_Ottawa %>% 
  select(property_ID, host_ID, listing_type, created, scraped, housing) %>% 
  left_join(daily_Ottawa, .) %>% 
  filter(date >= created, date - 30 <= scraped, status != "U") %>% 
  mutate(city = "Ottawa")

Ottawa_info <- list()
Ottawa_info[[1]] <- 
  property_Ottawa %>% 
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>% 
  nrow()
Ottawa_info[[2]] <- 
  Ottawa_info[[1]] / Canada_population[2,]$`Population` * 1000
Ottawa_info[[3]] <- 
  daily_Ottawa %>% 
  filter(housing == TRUE, status == "R", date >= "2018-05-01") %>% 
  pull(price) %>% 
  sum()
Ottawa_info[[4]] <- 
  Ottawa_info[[3]] / Ottawa_info[[1]]

# Calgary

daily_Calgary <- 
  strr_expand_daily(daily_compressed_Calgary, cores = 4)

daily_Calgary <- 
  property_Calgary %>% 
  select(property_ID, host_ID, listing_type, created, scraped, housing) %>% 
  left_join(daily_Calgary, .) %>% 
  filter(date >= created, date - 30 <= scraped, status != "U") %>% 
  mutate(city = "Calgary")

daily_Calgary <- 
  strr_expand_daily(daily_compressed_Calgary, cores = 4)

daily_Calgary <- 
  property_Calgary %>% 
  select(property_ID, host_ID, listing_type, created, scraped, housing) %>% 
  left_join(daily_Calgary, .) %>% 
  filter(date >= created, date - 30 <= scraped, status != "U") %>% 
  mutate(city = "Calgary")

Calgary_info <- list()
Calgary_info[[1]] <- 
  property_Calgary %>% 
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>% 
  nrow()
Calgary_info[[2]] <- 
  Calgary_info[[1]] / Canada_population[4,]$`Population` * 1000
Calgary_info[[3]] <- 
  daily_Calgary %>% 
  filter(housing == TRUE, status == "R", date >= "2018-05-01") %>% 
  pull(price) %>% 
  sum()
Calgary_info[[4]] <- 
  Calgary_info[[3]] / Calgary_info[[1]]

Canada_daily <- 
  daily %>% 
  select(-ML) %>% 
  mutate(city = "Toronto") %>% 
  bind_rows(daily_Montreal, daily_Vancouver, daily_Ottawa, daily_Calgary) %>% 
  filter(housing == TRUE)

Canada_graph <- 
  Canada_daily %>% 
  count(date, city) %>% 
  ggplot() +
  geom_line(aes(date, n, colour = city), size = 1.5) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-01-01"), NA)) +
  scale_colour_manual(name = "City", 
                      values = c("#4295A8", "#B4656F", "#C7F2A7", "#96897B",
                                 "#DFD5A5")) +
  theme(text = element_text(family = "Futura"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        legend.position = "bottom")

ggsave("output/figure_2.pdf", plot = Canada_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)


