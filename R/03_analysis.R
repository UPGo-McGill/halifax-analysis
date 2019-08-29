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
load("data/neighbourhoods.Rdata")
load("data/CTs_canada.Rdata")



### Active daily listings ######################################################

## Create objects

active_listings <- 
  daily %>% 
  filter(housing == TRUE) %>% 
  count(date)

active_listings_filtered <- 
  daily %>% 
  filter(housing == TRUE, date <= scraped) %>% 
  count(date)

active_listings_filtered %>% 
  arrange(desc(date))

## Active listings from property file

# All listings
nrow(filter(property, created <= "2019-04-30", scraped >= "2019-04-30"))

# Housing listings
nrow(LTM_property)

# Listing type breakdown
nrow(filter(LTM_property, listing_type == "Shared room"))/
  nrow(LTM_property)

# Number of hosts on April 30, 2019
length(unique(LTM_property$host_ID))

# Hosts by listing type
LTM_property %>% 
  filter(listing_type == "Shared room") %>% 
  select(host_ID) %>% 
  st_drop_geometry() %>% 
  unique() %>% 
  nrow()/
  length(unique(LTM_property$host_ID))

# LTM revenue
sum(LTM_property$revenue, na.rm = TRUE)

# LTM revenue by property type
filter(LTM_property, listing_type == "Shared room") %>% 
  select(revenue) %>% 
  st_drop_geometry() %>% 
  sum(na.rm = TRUE) /
  sum(LTM_property$revenue, na.rm = TRUE)

# YOY growth rate
nrow(LTM_property) / 
  nrow(filter(property, created <= "2018-04-30", scraped >= "2018-04-30",
              housing == TRUE))

# Active listings graph
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


### Spatial distribution of listings ###########################################
exchange_rate <- mean(1.2873,	1.3129, 1.3130, 1.3041, 1.3037, 1.3010, 1.3200,
                      1.3432, 1.3301, 1.3206, 1.3368, 1.3378)
## Map

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
  ssummarize(revenue = sum(price) * exchange_rate) %>% 
  left_join(filter(property_in_HRM, housing == TRUE), .) %>% 
  mutate(Year = "2018") %>% 
  filter(revenue > 0)

property_2019 <- 
  daily %>% 
  filter(status == "R", date >= "2018-05-01", date <= "2019-04-30") %>% 
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


### Which STR platforms are used in Halifax? ###################################

nrow(filter(LTM_property, !is.na(ab_property), is.na(ha_property)))
nrow(filter(LTM_property, !is.na(ha_property), is.na(ab_property)))
nrow(filter(LTM_property, !is.na(ha_property), !is.na(ab_property)))

nrow(LTM_property)



### Listing type prevalence ####################################################

property %>% 
  filter(housing == TRUE) %>% 
  rename(`Listing type` = listing_type) %>% 
  st_drop_geometry() %>% 
  filter(created <= "2019-04-30", scraped >= "2019-04-30") %>% 
  group_by(`Listing type`) %>% 
  summarize(`Number of listings` = n(),
            `Annual revenue` = sum(revenue, na.rm = TRUE),
            `Rev. per listing` = `Annual revenue` / n()) %>% 
  mutate(
    `% of all listings` = round(`Number of listings` /
                                  sum(`Number of listings`), 3),
    `% of all listings` = paste0(100 * `% of all listings`, "%"),
    `% of annual revenue` = `Annual revenue` / sum(`Annual revenue`)) %>% 
  mutate(
    `Annual revenue` = round(`Annual revenue`),
    `Annual revenue` = paste0("$", str_sub(`Annual revenue`, 1, -7), ".",
                              str_sub(`Annual revenue`, -6, -6), " million"),
    `% of annual revenue` = round(`% of annual revenue`, 3),
    `% of annual revenue` = paste0(100 * `% of annual revenue`, "%"),
    `Rev. per listing` = round(`Rev. per listing`),
    `Rev. per listing` = paste0("$", str_sub(`Rev. per listing`, 1, -4),
                                ",", str_sub(`Rev. per listing`, -3, -1))
  ) %>% view()

### Bedroom breakdown ##########################################################

property %>% 
  filter(created <= "2019-04-30", scraped >= "2019-04-30", housing == TRUE,
         listing_type == "Entire home/apt") %>% 
  count(bedrooms) %>% 
  mutate(percentage = n / sum(n))



var <- filter(property, created <= "2019-04-30", scraped >= "2019-04-30", 
              housing == TRUE, listing_type == "Entire home/apt")$bedrooms
nrows <- 20
df <- expand.grid(y = 1:20, x = 1:20)
categ_table <- round(table(var) * ((20*20)/(length(var))))
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



### Revenue distribution and commercial operators ##############################

## Host revenue percentiles

daily %>%
  filter(housing == TRUE, date >= "2018-05-01", status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)*exchange_rate) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev)))


## Host revenue percentiles graph

revenue_graph <-
  daily %>%
  filter(housing == TRUE, date >= "2018-05-01", status == "R") %>%
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

## Median host income

LTM_property %>% 
  filter(revenue > 0) %>% 
  pull(revenue) %>% 
  quantile() %>% 
  as.list() %>% 
  as_tibble() %>% 
  select(-`0%`) %>% 
  set_names(c("25th percentile", "Median", "75th percentile", 
              "100th percentile")) %>% 
  mutate_all(round, -2)


## Multilistings

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



ML_table <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(ML),
            Revenue = sum(price * (status == "R") * ML * exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE)) %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value)

ML_table %>% 
  ggplot() +
  #geom_line(aes(Date, Value, colour = `Multilisting percentage`)) +
  geom_smooth(aes(date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.2) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = percent)

ML_table %>% 
  filter(date == "2019-04-30")


### Housing loss ###############################################################

FREH %>% 
  filter(date == "2019-04-30") %>% 
  count()

FREH %>% 
  count(date) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = comma) +
  ggtitle("FREH listings in Halifax Regional Municipality")

GH %>% 
  st_drop_geometry() %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>% 
  ggplot() +
  geom_line(aes(date, GH_units), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = comma) +
  ggtitle("Units converted to ghost hostels in the City of Toronto")

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
  theme(legend.position = "bottom",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10))

ggsave("output/figure_7.pdf", plot = housing_graph, width = 8, height = 7, 
       units = "in", useDingbats = FALSE)

# Current housing loss figure
sum(filter(housing_loss, date == "2019-04-30")$`Housing units`)

# YOY increase
sum(filter(housing_loss, date == "2019-04-30")$`Housing units`) /
  sum(filter(housing_loss, date == "2018-04-30")$`Housing units`)


## Relate housing loss to rental vacancy rate

housing <- 
  get_census("CA16", regions = list(CSD = "1209034"), level = "CSD", 
             vectors = c("v_CA16_4897", "v_CA16_405"))

housing %>% 
  select(`v_CA16_405: Private dwellings occupied by usual residents`,
         `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`) %>% 
  set_names(c("Dwellings", "Tenants")) %>% 
  pull(Tenants) %>% 
  {. * 1.016 * 0.016}

### Number of long reservations

long_reservations <- 
  daily_compressed %>% 
  filter(status == "R") %>% 
  left_join({property %>% st_drop_geometry() %>% ungroup() %>% 
      select(property_ID, created, scraped, housing)}) %>% 
  filter(start_date >= created, end_date <= scraped + 30) %>% 
  group_by(res_id)

res_length <- 
  long_reservations %>% 
  group_by(res_id) %>% 
  summarize(nights = max(end_date) - min(start_date) + 1)

mean(res_length$nights >= 28)


## Neighbourhood analysis
airbnb_neighbourhoods <- tibble(neighbourhood = character(0), active_listings = numeric(0), 
                 active_listings_LTM = numeric (0), EH_pct = numeric (0), revenue_LTM = numeric (0), 
                 GH = numeric (0), FREH = numeric (0),  housing_loss = numeric (0), 
                 revenue_10pct_LTM = numeric (0), active_listings_yoy = numeric(0), housing_loss_yoy = numeric (0))

for (n in c(1:nrow(neighbourhoods))) {
  
  neighbourhood_property <- property %>% 
    filter(housing == TRUE) %>% 
    st_join(neighbourhoods[n, "geometry"],
            join = st_within, left = FALSE)
  
  neighbourhood_daily <- daily %>% 
    filter(property_ID %in% neighbourhood_property$property_ID)
  
  airbnb_neighbourhoods[n,1] <- neighbourhoods$neighbourhood[n]
  
  airbnb_neighbourhoods[n,2] <- neighbourhood_property %>% 
    filter(created <= "2019-04-30",
           scraped >= "2019-04-30") %>% 
    nrow()
  
  airbnb_neighbourhoods[n,3] <- neighbourhood_daily %>% 
    filter(date <= "2019-04-30" & date >= "2018-05-01") %>% 
    group_by(date) %>% 
    summarize(listings = n()) %>%
    summarise(mean_listings = mean(listings))
  
  airbnb_neighbourhoods[n,4] <-   nrow(neighbourhood_daily %>% 
                           filter(date == "2019-04-30") %>% 
                           filter(listing_type == "Entire home/apt"))/
    nrow(neighbourhood_daily %>% 
           filter(date == "2019-04-30"))
  
  airbnb_neighbourhoods[n,5] <-   neighbourhood_daily %>% 
    filter(date <= "2019-04-30" & date >= "2018-05-01" &
             status == "R" ) %>%
    summarise(sum_revenue = sum(price, na.rm = TRUE)*exchange_rate)
  
  airbnb_neighbourhoods[n,6] <-      nrow(neighbourhood_daily %>% 
                                            filter(date == "2019-04-30") %>% 
                                            inner_join(GH, .))
  
  airbnb_neighbourhoods[n,7] <-     nrow(neighbourhood_daily %>% 
                                           filter(date == "2019-04-30") %>% 
                                           inner_join(FREH, .))
  
  temp <- strr_ghost(neighbourhood_property, property_ID, host_ID, created, scraped, "2018-05-01",
                     "2019-04-30", listing_type) %>% 
    filter(date == "2019-04-30") %>% 
    group_by(ghost_ID) %>% 
    summarize(n = sum(housing_units)) %>% 
    ungroup()
  
  airbnb_neighbourhoods[n,8] <- ifelse(nrow(temp) == 0, 0, temp %>% 
                           summarize(GH_housing_loss = sum(n))) %>% 
    as.numeric() +
    nrow(neighbourhood_daily %>% 
           filter(date == "2019-04-30") %>% 
           inner_join(FREH, .))
  
  airbnb_neighbourhoods[n,9] <- neighbourhood_daily %>%
    filter(date >= "2018-05-01", date <= "2019-04-30", status == "R") %>%
    group_by(host_ID) %>%
    summarize(rev = sum(price) * exchange_rate) %>%
    filter(rev > 0) %>%
    summarize(
      `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)))
  
  
  airbnb_neighbourhoods[n,10] <- neighbourhood_property %>% 
    filter(created <= "2019-04-30",
           scraped >= "2019-04-30") %>% 
    nrow() / 
    neighbourhood_property %>% 
    filter(created <= "2018-04-30",
           scraped >= "2018-04-30") %>% 
    nrow()
  
  temp2 <- strr_ghost(neighbourhood_property, property_ID, host_ID, created, scraped, "2017-05-01",
                     "2018-04-30", listing_type) %>% 
    filter(date == "2018-04-30") %>% 
    group_by(ghost_ID) %>% 
    summarize(n = sum(housing_units)) %>% 
    ungroup()
  
  airbnb_neighbourhoods[n,11] <- (ifelse(nrow(temp) == 0, 0, temp %>% 
                                         summarize(GH_housing_loss = sum(n))) %>% 
    as.numeric() +
    nrow(neighbourhood_daily %>% 
           filter(date == "2019-04-30") %>% 
           inner_join(FREH, .)))/
    (ifelse(nrow(temp2) == 0, 0, temp2 %>% 
              summarize(GH_housing_loss = sum(n))) %>% 
       as.numeric() +
       nrow(neighbourhood_daily %>% 
              filter(date == "2018-04-30") %>% 
              inner_join(FREH, .)))
  
  rm(neighbourhood_property, neighbourhood_daily, temp, temp2)
}

# add neighbourhood geometries

airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  left_join(neighbourhoods) %>% 
  select(-count)

# add census variables


# add housing loss as percentage of dwellings



################ NOT INCLUDED AS OF RIGHT NOW ###############################
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



### Listings likely in violation of principal residence requirement ############

## LFRML calculations

# Add ML field to property file
property <- 
  daily %>% 
  filter(date == "2019-04-30") %>% 
  select(property_ID, ML) %>% 
  left_join(property, .) %>% 
  mutate(ML = if_else(is.na(ML), FALSE, ML))

# Add n_reserved and n_available fields
property <- 
  daily %>% 
  filter(status == "R") %>% 
  group_by(property_ID) %>% 
  summarize(n_reserved = n()) %>% 
  left_join(property, .)

property <- 
  daily %>% 
  filter(status == "R" | status == "A") %>% 
  group_by(property_ID) %>% 
  summarize(n_available = n()) %>% 
  left_join(property, .)

# Add LFRML field
property <- 
  property %>%
  group_by(host_ID, listing_type) %>% 
  mutate(LFRML = case_when(
    listing_type != "Entire home/apt" ~ FALSE,
    ML == FALSE                       ~ FALSE,
    n_available == min(n_available)   ~ TRUE,
    TRUE                              ~ FALSE)) %>% 
  ungroup()


# Resolve ties
property <- 
  property %>% 
  group_by(host_ID, listing_type) %>% 
  mutate(prob = sample(0:10000, n(), replace = TRUE),
         LFRML = if_else(
           sum(LFRML) > 1 & prob != max(prob), FALSE, LFRML)) %>% 
  select(-prob)


# Add GH status
GH_list <-
  GH %>% 
  filter(date == "2019-04-30") %>% 
  pull(property_IDs) %>%
  unlist() %>%
  unique()

property <-
  property %>% 
  mutate(GH = if_else(property_ID %in% GH_list, TRUE, FALSE))

# Add FREH status
property <- 
  FREH %>% 
  filter(date == "2019-04-30") %>% 
  mutate(FREH = TRUE) %>% 
  left_join(property, .) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH))

# Add Legal field
legal <- 
  property %>%
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>% 
  mutate(legal = case_when(
    GH == TRUE                     ~ FALSE,
    listing_type == "Shared room"  ~ TRUE,
    listing_type == "Private room" ~ TRUE,
    FREH == TRUE                   ~ FALSE,
    LFRML == TRUE                  ~ TRUE,
    ML == TRUE                     ~ FALSE,
    TRUE                           ~ TRUE))

mean(legal$FREH, na.rm = TRUE)
mean(legal$GH, na.rm = TRUE)
mean(legal$LFRML, na.rm = TRUE)
mean(legal$ML, na.rm = TRUE)
mean(legal$legal, na.rm = TRUE)

## Alternate approach

property %>%
  st_drop_geometry() %>% 
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>% 
  nrow()

property %>%
  st_drop_geometry() %>% 
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>%
  filter(listing_type == "Entire home/apt") %>% 
  filter(ML == TRUE) %>% 
  nrow()

property %>%
  st_drop_geometry() %>% 
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>%
  filter(listing_type == "Entire home/apt") %>% 
  filter(ML == TRUE & LFRML == FALSE) %>% 
  nrow()

property %>%
  st_drop_geometry() %>% 
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>%
  filter(listing_type == "Entire home/apt") %>% 
  filter((ML == TRUE & LFRML == FALSE) | (FREH == TRUE)) %>% 
  nrow()

property %>%
  st_drop_geometry() %>% 
  filter(housing == TRUE, created <= "2019-04-30", scraped >= "2019-04-30") %>% 
  filter(GH == TRUE) %>% 
  nrow()

