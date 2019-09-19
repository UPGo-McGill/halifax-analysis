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
load("data/CTs_halifax.Rdata")

# Set up dates
start_date <- "2018-05-01"
end_date <- "2019-04-30"
date_yoy <- "2018-04-30"

# Exchange rate (average over last twelve months)
exchange_rate <- mean(1.2873,	1.3129, 1.3130, 1.3041, 1.3037, 1.3010, 1.3200,
                      1.3432, 1.3301, 1.3206, 1.3368, 1.3378)

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
nrow(filter(property, created <= end_date, scraped >= end_date))

# Housing listings over the last twelve months
nrow(LTM_property)

# Listing type breakdown
nrow(filter(LTM_property, listing_type == "Shared room"))/
  nrow(LTM_property)

# Number of hosts over last twelve months
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
  nrow(filter(property, created <= date_yoy, scraped >= date_yoy,
              housing == TRUE))


### Which STR platforms are used in Halifax? ###################################

# Airbnb and not Homeaway
nrow(filter(LTM_property, !is.na(ab_property), is.na(ha_property)))

# Homeaway and not Airbnb
nrow(filter(LTM_property, !is.na(ha_property), is.na(ab_property)))

# Both Airbnb and Homeaway
nrow(filter(LTM_property, !is.na(ha_property), !is.na(ab_property)))

nrow(LTM_property)


### Listing type prevalence ####################################################

property %>% 
  filter(housing == TRUE) %>% 
  rename(`Listing type` = listing_type) %>% 
  st_drop_geometry() %>% 
  filter(created <= end_date, scraped >= end_date) %>% 
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
  filter(created <= end_date, scraped >= end_date, housing == TRUE,
         listing_type == "Entire home/apt") %>% 
  count(bedrooms) %>% 
  mutate(percentage = n / sum(n))

### Revenue distribution and commercial operators ##############################

## Host revenue percentiles

daily %>%
  filter(housing == TRUE, date >= start_date, status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)*exchange_rate) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev)))


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


## Top earning host(s)
LTM_property %>% 
  group_by(host_ID) %>% 
  summarise(host_rev = sum(revenue)) %>% 
  filter(host_rev>0)


## Multilistings

ML_table <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(ML),
            Revenue = sum(price * (status == "R") * ML * exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE)) %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value)

ML_table %>% 
  filter(date == end_date)

### Housing loss ###############################################################

FREH %>% 
  filter(date == end_date) %>% 
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
  ggtitle("Units converted to ghost hostels in Halifax Regional Municipality")

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

# Current housing loss figure
sum(filter(housing_loss, date == end_date)$`Housing units`)

# YOY increase
sum(filter(housing_loss, date == end_date)$`Housing units`) /
  sum(filter(housing_loss, date == date_yoy)$`Housing units`)


## Relate housing loss to rental vacancy rate

vacancy_rate <- 1.016

housing <- 
  get_census("CA16", regions = list(CSD = "1209034"), level = "CSD", 
             vectors = c("v_CA16_4897", "v_CA16_405"))

housing %>% 
  select(`v_CA16_405: Private dwellings occupied by usual residents`,
         `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`) %>% 
  set_names(c("Dwellings", "Tenants")) %>% 
  pull(Tenants) %>% 
  {. * vacancy_rate * (vacancy_rate - 1)}

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
    filter(created <= end_date,
           scraped >= end_date) %>% 
    nrow()
  
  airbnb_neighbourhoods[n,3] <- neighbourhood_daily %>% 
    filter(date <= end_date & date >= start_date) %>% 
    group_by(date) %>% 
    summarize(listings = n()) %>%
    summarise(mean_listings = mean(listings))
  
  airbnb_neighbourhoods[n,4] <-   nrow(neighbourhood_daily %>% 
                           filter(date == end_date) %>% 
                           filter(listing_type == "Entire home/apt"))/
    nrow(neighbourhood_daily %>% 
           filter(date == end_date))
  
  airbnb_neighbourhoods[n,5] <-   neighbourhood_daily %>% 
    filter(date <= end_date & date >= start_date &
             status == "R" ) %>%
    summarise(sum_revenue = sum(price, na.rm = TRUE)*exchange_rate)
  
  airbnb_neighbourhoods[n,6] <-      nrow(neighbourhood_daily %>% 
                                            filter(date == end_date) %>% 
                                            inner_join(GH, .))
  
  airbnb_neighbourhoods[n,7] <-     nrow(neighbourhood_daily %>% 
                                           filter(date == end_date) %>% 
                                           inner_join(FREH, .))
  
  temp <- strr_ghost(neighbourhood_property, property_ID, host_ID, created, scraped, start_date,
                     end_date, listing_type) %>% 
    filter(date == end_date) %>% 
    group_by(ghost_ID) %>% 
    summarize(n = sum(housing_units)) %>% 
    ungroup()
  
  airbnb_neighbourhoods[n,8] <- ifelse(nrow(temp) == 0, 0, temp %>% 
                           summarize(GH_housing_loss = sum(n))) %>% 
    as.numeric() +
    nrow(neighbourhood_daily %>% 
           filter(date == end_date) %>% 
           inner_join(FREH, .))
  
  airbnb_neighbourhoods[n,9] <- neighbourhood_daily %>%
    filter(date >= start_date, date <= end_date, status == "R") %>%
    group_by(host_ID) %>%
    summarize(rev = sum(price) * exchange_rate) %>%
    filter(rev > 0) %>%
    summarize(
      `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)))
  
  
  airbnb_neighbourhoods[n,10] <- neighbourhood_property %>% 
    filter(created <= end_date,
           scraped >= end_date) %>% 
    nrow() / 
    neighbourhood_property %>% 
    filter(created <= date_yoy,
           scraped >= date_yoy) %>% 
    nrow()
  
  temp2 <- strr_ghost(neighbourhood_property, property_ID, host_ID, created, scraped, "2017-05-01",
                     end_date = date_yoy, listing_type) %>% 
    filter(date == date_yoy) %>% 
    group_by(ghost_ID) %>% 
    summarize(n = sum(housing_units)) %>% 
    ungroup()
  
  airbnb_neighbourhoods[n,11] <- (ifelse(nrow(temp) == 0, 0, temp %>% 
                                         summarize(GH_housing_loss = sum(n))) %>% 
    as.numeric() +
    nrow(neighbourhood_daily %>% 
           filter(date == end_date) %>% 
           inner_join(FREH, .)))/
    (ifelse(nrow(temp2) == 0, 0, temp2 %>% 
              summarize(GH_housing_loss = sum(n))) %>% 
       as.numeric() +
       nrow(neighbourhood_daily %>% 
              filter(date == date_yoy) %>% 
              inner_join(FREH, .)))
  
  rm(neighbourhood_property, neighbourhood_daily, temp, temp2)
}

# Add census variables and geometries

airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  left_join(neighbourhoods)

# Add housing loss as percentage of dwellings

airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(housing_loss_pct = housing_loss/households)

save(airbnb_neighbourhoods, file = "data/airbnb_neighbourhoods.Rdata")

################ NOT INCLUDED AS OF RIGHT NOW ###############################
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

