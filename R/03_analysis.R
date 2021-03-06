#### HALIFAX ANALYSIS ##########################################################

source("R/01_helper_functions.R")

load("data/HRM_property.Rdata")
load("data/HRM_LTM_property.Rdata")
load("data/HRM_FREH.Rdata")
load("data/HRM_GH.Rdata")
load("data/HRM_daily.Rdata")
load("data/HRM_daily_compressed.Rdata")
load("data/HRM_neighbourhoods.Rdata")
load("data/CTs_halifax.Rdata")

# Set up dates
start_date <- "2018-09-01"
end_date <- "2019-08-31"
date_yoy <- "2018-08-31"

# Exchange rate (average over last twelve months)
exchange_rate <- mean(1.3037, 1.3010, 1.3200,
                      1.3432, 1.3301, 1.3206, 
                      1.3368, 1.3378, 1.3438,
                      1.3188, 1.3046, 1.3316)


### Region comparison ##########################################################

revenue <-  property_AC %>% 
    filter(created <= end_date, scraped >= start_date, housing == TRUE) %>%
    group_by(region) %>% 
    summarise(region_revenue = sum(revenue, na.rm = TRUE))
         
cities <-
  property_AC %>% 
  filter(created <= end_date, scraped >= end_date, housing == TRUE) %>% 
  count(city) 

region <- property_AC %>% 
  filter(created <= end_date, scraped >= end_date, housing == TRUE) %>% 
  count(region) 

canada_population <- 
  get_census("CA16", regions = list(PR = c("10", "11", "12", "13")), 
             level = "CSD")


### Active daily listings ######################################################

## Create objects
active_listings <- 
  daily %>% 
  filter(housing == TRUE) %>% 
  count(date)

active_listings_filtered <- 
  daily %>% 
  filter(housing == TRUE, date <= scraped, date <= end_date, 
         date >= "2016-05-01") %>% 
  count(date)

active_listings_filtered %>% 
  arrange(desc(date))

## Active listings from property file
# All listings
nrow(filter(property, created <= end_date, scraped >= end_date, 
            housing == TRUE))

# Housing listings over the last twelve months
nrow(LTM_property)

# Listing type breakdown
nrow(filter(property, created <= end_date, scraped >= end_date, 
            listing_type == "Entire home/apt")) /
  nrow(filter(property, created <= end_date, scraped >= end_date))

nrow(filter(LTM_property, listing_type == "Shared room"))/
  nrow(LTM_property)

# Number of hosts over last twelve months
length(unique(LTM_property$host_ID))

# Hosts by listing type
LTM_property %>% 
  filter(listing_type == "Entire home/apt") %>% 
  select(host_ID) %>% 
  st_drop_geometry() %>% 
  unique() %>% 
  nrow()/
  length(unique(LTM_property$host_ID))

# LTM revenue
sum(LTM_property$revenue, na.rm = TRUE)

# LTM revenue by property type
filter(LTM_property, listing_type == "Entire home/apt") %>% 
  select(revenue) %>% 
  st_drop_geometry() %>% 
  sum(na.rm = TRUE) /
  sum(LTM_property$revenue, na.rm = TRUE)

# YOY growth rate
nrow(filter(property, created <= end_date, scraped >= end_date,
            housing == TRUE)) / 
  nrow(filter(property, created <= date_yoy, scraped >= date_yoy,
              housing == TRUE))


### Nova Scotia mapping prep ###################################################

## Raffle of NS listings by DA

upgo_connect()

property_NS <- 
  property_all %>% 
  filter(country == "Canada", region == "Nova Scotia") %>% 
  collect()

upgo_disconnect()

DA_NS <- 
  cancensus::get_census("CA16", list(PR = "12"), "DA", geo_format = "sf") %>% 
  st_transform(32617)

property_NS <- 
  property_NS %>% 
  filter(housing == TRUE, created <= "2019-08-31", scraped >= "2019-08-31") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32617)

property_NS <- 
  property_NS %>% 
  strr_raffle(DA_NS, GeoUID, Dwellings, cores = 4)

DA_NS <- 
  property_NS %>% 
  st_drop_geometry() %>% 
  count(winner) %>% 
  rename(GeoUID = winner) %>% 
  left_join(DA_NS, .)


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

# Entire home multilistings

daily %>% 
 filter(listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(Listings = sum(ML)) %>% 
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
  rollmean(30, align = "right") 

GH_total <- GH_total[(length(GH_total) + 1 - n_groups(FREH %>% group_by(date))):length(GH_total)]

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
  select(
    `v_CA16_405: Private dwellings occupied by usual residents`,
`v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`
         ) %>% 
  set_names(c("Dwellings", "Tenants")) %>% 
  pull(Tenants) %>% 
  {. * vacancy_rate * (vacancy_rate - 1)}

### Number of long reservations

long_reservations <- 
  daily_compressed %>% 
  filter(status == "R") %>% 
  filter(start_date >= created, end_date <= scraped + 30) %>% 
  group_by(res_ID)

res_length <- 
  long_reservations %>% 
  group_by(res_ID) %>% 
  summarize(nights = max(end_date) - min(start_date) + 1)

mean(res_length$nights >= 28)

### Listings likely in violation of principal residence requirement ############

## LFRML calculations

# Add ML field to property file
property <- 
  daily %>% 
  filter(date == end_date) %>% 
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
  filter(date == end_date) %>% 
  pull(property_IDs) %>%
  unlist() %>%
  unique()

property <-
  property %>% 
  mutate(GH = if_else(property_ID %in% GH_list, TRUE, FALSE))

# Add FREH status
property <- 
  FREH %>% 
  filter(date == end_date) %>% 
  mutate(FREH = TRUE) %>% 
  left_join(property, .) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH))

# Add Legal field
legal <- 
  property %>%
  filter(housing == TRUE, created <= end_date, scraped >= end_date) %>% 
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

# Legal by neighbourhood
legal %>% 
  group_by(neighbourhood) %>% 
  summarize(non_PR = length(legal[legal == FALSE])) %>% 
  st_drop_geometry() %>% 
  left_join(neighbourhoods) %>% 
  select(name, non_PR) %>% 
  view()

## Neighbourhood analysis ####################################

airbnb_neighbourhoods <- tibble(neighbourhood = character(0), 
                                active_listings = numeric(0), 
                                active_listings_LTM = numeric(0), 
                                EH_pct = numeric(0), 
                                revenue_LTM = numeric(0),
                                GH = numeric(0), 
                                FREH = numeric(0),
                                housing_loss = numeric(0), 
                                revenue_10pct_LTM = numeric(0), 
                                active_listings_yoy = numeric(0), 
                                housing_loss_yoy = numeric(0))

for (n in c(1:nrow(neighbourhoods))) {
  
  neighbourhood_property <- 
    property %>% 
    filter(housing == TRUE) %>% 
    filter(neighbourhood == neighbourhoods$neighbourhood[n])
  
  neighbourhood_daily <- 
    daily %>% 
    filter(property_ID %in% neighbourhood_property$property_ID)
  
  airbnb_neighbourhoods[n,1] <- 
    neighbourhoods$neighbourhood[n]
  
  airbnb_neighbourhoods[n,2] <- 
    neighbourhood_property %>% 
    filter(created <= end_date,
           scraped >= end_date) %>% 
    nrow()
  
  airbnb_neighbourhoods[n,3] <- 
    neighbourhood_daily %>% 
    filter(date <= end_date & date >= start_date) %>% 
    group_by(date) %>% 
    summarize(listings = n()) %>%
    summarise(mean_listings = mean(listings))
  
  airbnb_neighbourhoods[n,4] <-   
    nrow(neighbourhood_daily %>% 
           filter(date == end_date) %>% 
           filter(listing_type == "Entire home/apt")) / 
    nrow(neighbourhood_daily %>% 
           filter(date == end_date))
  
  airbnb_neighbourhoods[n,5] <-   
    neighbourhood_daily %>% 
    filter(date <= end_date & date >= start_date & status == "R") %>%
    summarize(sum_revenue = sum(price, na.rm = TRUE) * exchange_rate)
  
  airbnb_neighbourhoods[n,6] <- 
    ifelse(neighbourhood_daily %>% 
             filter(date == end_date) %>% 
             inner_join(GH, .) %>% nrow() == 0,
           0,
           neighbourhood_daily %>% 
             filter(date == end_date) %>% 
             inner_join(GH, .) %>% 
             select(ghost_ID, housing_units) %>% 
             st_drop_geometry() %>% 
             distinct() %>% 
             select(housing_units) %>% 
             sum())
  
  airbnb_neighbourhoods[n,7] <- 
    nrow(neighbourhood_daily %>% 
           filter(date == end_date) %>% 
           inner_join(FREH, .))

  airbnb_neighbourhoods[n,8] <-
    airbnb_neighbourhoods[n,6] + airbnb_neighbourhoods[n,7]
  
  airbnb_neighbourhoods[n,9] <- 
    neighbourhood_daily %>%
    filter(date >= start_date, date <= end_date, status == "R") %>%
    group_by(host_ID) %>%
    summarize(rev = sum(price) * exchange_rate) %>%
    filter(rev > 0) %>%
    summarize(`Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)))
  
  airbnb_neighbourhoods[n,10] <- 
    neighbourhood_property %>% 
    filter(created <= end_date, scraped >= end_date) %>% 
    nrow() / 
    neighbourhood_property %>% 
    filter(created <= date_yoy, scraped >= date_yoy) %>% 
    nrow()
  
  airbnb_neighbourhoods[n,11] <- 
    airbnb_neighbourhoods[n, 8] / 
    (ifelse(neighbourhood_daily %>% 
              filter(date == date_yoy) %>% 
              inner_join(GH, .) %>% nrow() == 0,
            0,
            neighbourhood_daily %>% 
              filter(date == date_yoy) %>% 
              inner_join(GH, .) %>% 
              select(ghost_ID, housing_units) %>% 
              st_drop_geometry() %>% 
              distinct() %>% 
              select(housing_units) %>% 
              sum()) +
      nrow(neighbourhood_daily %>% 
             filter(date == end_date) %>% 
             inner_join(FREH, .)))
    
  rm(neighbourhood_property, neighbourhood_daily)
}

# Add census variables, names, and geometries

airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  left_join(neighbourhoods) %>% 
  st_as_sf()

# Add housing loss as percentage of dwellings

airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(housing_loss_pct = housing_loss/households,
         active_listings_pct = active_listings/households)

## Neighbourhood primary residences
legal %>% 
  filter(legal == FALSE) %>% 
  group_by(neighbourhood) %>% 
  summarize(housing_returned = n()) %>% 
  st_drop_geometry() %>% 
  left_join(neighbourhoods) %>% 
  select(name, housing_returned, households) %>% 
  mutate(housing_returned_pct = housing_returned/households) %>% 
  view()

# Top ten active listings
airbnb_neighbourhoods %>% 
  arrange(desc(active_listings)) %>% 
  slice(1:10) %>% 
  select(name, active_listings)

# Active listings/dwelling
airbnb_neighbourhoods %>% 
  arrange(desc(active_listings_pct)) %>% 
  slice(1:10) %>% 
  select(name, active_listings_pct)

# Active listings YOY growth rate
airbnb_neighbourhoods %>% 
  filter(active_listings >= 50) %>% 
  arrange(desc(active_listings_yoy)) %>% 
  slice(1:10) %>% 
  select(name, active_listings_yoy)

# Revenue_LTM
airbnb_neighbourhoods %>% 
  arrange(desc(revenue_LTM)) %>% 
  slice(1:10) %>% 
  select(name, revenue_LTM)

# Housing loss
airbnb_neighbourhoods %>% 
  arrange(desc(housing_loss)) %>% 
  slice(1:10) %>% 
  select(name, housing_loss)

# Housing loss/dwelling
airbnb_neighbourhoods %>% 
  arrange(desc(housing_loss_pct)) %>% 
  slice(1:10) %>% 
  select(name, housing_loss_pct)

# Housing loss YOY growth rate
airbnb_neighbourhoods %>% 
  filter(active_listings >=50) %>% 
  arrange(desc(housing_loss_yoy)) %>% 
  slice(1:10) %>% 
  select(name, housing_loss_yoy)

# Non - principal residences / active listings
airbnb_neighbourhoods <- legal %>% 
group_by(neighbourhood) %>% 
  summarize(non_PR = length(legal[legal == FALSE])) %>% 
  st_drop_geometry() %>% 
  left_join(airbnb_neighbourhoods) 

airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(non_PR_pct_listings = non_PR/active_listings) 

airbnb_neighbourhoods <- legal %>% 
  filter(legal == FALSE) %>% 
  group_by(neighbourhood) %>% 
  summarize(housing_returned = n()) %>% 
  st_drop_geometry() %>% 
  left_join(airbnb_neighbourhoods) %>% 
  mutate(housing_returned_pct = housing_returned/households) 

# Neighbourhood table with all
airbnb_neighbourhoods %>% 
  arrange(desc(active_listings)) %>% 
  slice(1:10) %>% 
  select(name, active_listings, active_listings_pct, active_listings_yoy,
         revenue_LTM, housing_loss, housing_loss_pct, housing_loss_yoy, non_PR, 
         non_PR_pct_listings, housing_returned, housing_returned_pct) %>% 
  view()

## Urban/rural comparison ########################
areas <- c("halifax", "dartmouth", "other_urban", "rural")

halifax <- c("H_1","H_2", "H_3", "H_4", "H_11", "H_6", "H_5",
             "H_8", "H_9", "H_10", "H_12", "H_7")
dartmouth <- c("D_5", "D_4", "D_3", "D_6", "D_1","D_7",
               "D_2")
other_urban <- c("B_1","C_15", "C_18", "C_21", "C_16",
                 "C_22", "C_20", "C_17", "C_14")

property <- property %>% 
  mutate(halifax = neighbourhood%in%halifax,
         dartmouth = neighbourhood%in%dartmouth,
         other_urban = neighbourhood%in%other_urban)

property <- property %>% 
  mutate(urban_rural = ifelse(halifax == TRUE, "halifax",
                              ifelse(dartmouth == TRUE, "dartmouth",
                                     ifelse(other_urban == TRUE, "other_urban", 
                                            "rural"))))
neighbourhoods <- neighbourhoods %>% 
  mutate(halifax = neighbourhood%in%halifax,
         dartmouth = neighbourhood%in%dartmouth,
         other_urban = neighbourhood%in%other_urban)

neighbourhoods <- neighbourhoods %>% 
  mutate(urban_rural = ifelse(halifax == TRUE, "halifax",
                              ifelse(dartmouth == TRUE, "dartmouth",
                                     ifelse(other_urban == TRUE, "other_urban", 
                                            "rural"))))


urban_rural <- tibble(area = character(0), active_listings = numeric(0), 
                                active_listings_LTM = numeric (0), EH_pct = numeric (0), 
                                revenue_LTM = numeric (0), 
                                GH = numeric (0), FREH = numeric (0),  housing_loss = numeric (0), 
                                revenue_10pct_LTM = numeric (0), active_listings_yoy = numeric(0),
                                housing_loss_yoy = numeric (0), households = numeric (0))

for (n in c(1:length(areas))) {
  
  urban_rural_property <- property %>% 
    filter(housing == TRUE) %>% 
    filter(urban_rural == areas[n])
  
  urban_rural_daily <- daily %>% 
    filter(property_ID %in% urban_rural_property$property_ID)
  
  urban_rural[n,1] <- areas[n]
  
  urban_rural[n,2] <- urban_rural_property %>% 
    filter(created <= end_date,
           scraped >= end_date) %>% 
    nrow()
  
 urban_rural[n,3] <- urban_rural_daily %>% 
    filter(date <= end_date & date >= start_date) %>% 
    group_by(date) %>% 
    summarize(listings = n()) %>%
    summarise(mean_listings = mean(listings))
  
  urban_rural[n,4] <-   nrow(urban_rural_daily %>% 
                                         filter(date == end_date) %>% 
                                         filter(listing_type == "Entire home/apt"))/
    nrow(urban_rural_daily %>% 
           filter(date == end_date))
  
  urban_rural[n,5] <-   urban_rural_daily %>% 
    filter(date <= end_date & date >= start_date &
             status == "R" ) %>%
    summarise(sum_revenue = sum(price, na.rm = TRUE)*exchange_rate)
  
  urban_rural[n,6] <-       ifelse(urban_rural_daily %>% 
                                               filter(date == end_date) %>% 
                                               inner_join(GH, .) %>% nrow() == 0,
                                             0,
                                             urban_rural_daily %>% 
                                               filter(date == end_date) %>% 
                                               inner_join(GH, .) %>% 
                                               select(ghost_ID, housing_units) %>% 
                                               st_drop_geometry() %>% 
                                               distinct() %>% 
                                               select(housing_units) %>% 
                                               sum())
  
  urban_rural[n,7] <-     nrow(urban_rural_daily %>% 
                                           filter(date == end_date) %>% 
                                           inner_join(FREH, .))
  
  urban_rural[n,8] <-  urban_rural[n,6] +  urban_rural[n,7]
  
  urban_rural[n,9] <- urban_rural_daily %>%
    filter(date >= start_date, date <= end_date, status == "R") %>%
    group_by(host_ID) %>%
    summarize(rev = sum(price) * exchange_rate) %>%
    filter(rev > 0) %>%
    summarize(
      `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)))
  
  
 urban_rural[n,10] <- urban_rural_property %>% 
    filter(created <= end_date,
           scraped >= end_date) %>% 
    nrow() / 
    urban_rural_property %>% 
    filter(created <= date_yoy,
           scraped >= date_yoy) %>% 
    nrow()
  
  urban_rural[n,11] <- urban_rural[n, 8] /
    (ifelse(urban_rural_daily %>% 
              filter(date == date_yoy) %>% 
              inner_join(GH, .) %>% nrow() == 0,
            0,
           urban_rural_daily %>% 
              filter(date == date_yoy) %>% 
              inner_join(GH, .) %>% 
              select(ghost_ID, housing_units) %>% 
              st_drop_geometry() %>% 
              distinct() %>% 
              select(housing_units) %>% 
              sum()) +
       nrow(urban_rural_daily %>% 
              filter(date == date_yoy) %>% 
              inner_join(FREH, .)))
  
  urban_rural[n, 12] <- neighbourhoods %>%
    filter(urban_rural == areas[n]) %>% 
    select(households) %>% 
    st_drop_geometry() %>% 
    sum()
}

rm(urban_rural_daily, urban_rural_property)

urban_rural <- urban_rural %>% 
  mutate(active_listings_pct = active_listings/households,
         housing_loss_pct = housing_loss/households) %>% 
  select(-c(active_listings_LTM, EH_pct, GH, FREH, revenue_10pct_LTM, households))


## Save files #####################################
save(active_listings_filtered, file = "data/active_listings_filtered.Rdata")
save(property, file = "data/HRM_property.Rdata")
save(housing_loss, file = "data/housing_loss.Rdata")
save(airbnb_neighbourhoods, file = "data/airbnb_neighbourhoods.Rdata")
save(legal, file = "data/legal.Rdata")
save(urban_rural, file = "data/urban_rural.Rdata")
save(neighbourhoods, file = "data/neighbourhoods.Rdata")






### Reimplementation of FREH/GH ################################################

GH_nbhd <- 
  GH %>% 
  group_by(ghost_ID) %>% 
  slice(1) %>%
  ungroup() %>% 
  st_centroid() %>% 
  st_intersection(neighbourhoods) %>% 
  st_drop_geometry() %>% 
  select(ghost_ID, neighbourhood = name) %>% 
  arrange(ghost_ID)

GH_CT <- 
  GH %>% 
  group_by(ghost_ID) %>% 
  slice(1) %>%
  ungroup() %>% 
  st_centroid() %>% 
  st_intersection(CTs_halifax) %>% 
  st_drop_geometry() %>% 
  select(ghost_ID, CT = Geo_UID) %>% 
  arrange(ghost_ID)

GH <- 
  GH %>% left_join(GH_nbhd) %>% left_join(GH_CT)

## Add current housing loss figures

airbnb_neighbourhoods <-
  GH %>% 
  st_drop_geometry() %>% 
  filter(date >= "2019-08-01") %>% 
  select(-host_ID, -property_IDs, -data) %>% 
  group_by(date, neighbourhood) %>% 
  summarize(GH = sum(housing_units)) %>% 
  group_by(neighbourhood) %>% 
  summarize(GH = round(mean(GH, na.rm = TRUE))) %>% 
  left_join(airbnb_neighbourhoods, ., by = c("name" = "neighbourhood")) %>% 
  mutate(GH.x = if_else(is.na(GH.y), 0, GH.y)) %>% 
  rename(GH = GH.x) %>% 
  select(-GH.y)

airbnb_neighbourhoods <- 
  FREH %>% 
  filter(date == "2019-08-31") %>% 
  left_join(select(st_drop_geometry(property), property_ID, neighbourhood)) %>% 
  count(neighbourhood) %>% 
  left_join(airbnb_neighbourhoods, .) %>% 
  mutate(FREH = n,
         housing_loss = GH + FREH,
         housing_loss_pct = housing_loss / households) %>% 
  select(-n)

## Add year-ago housing loss figures

airbnb_neighbourhoods <-
  GH %>% 
  st_drop_geometry() %>% 
  filter(date >= "2018-08-01", date <= "2018-08-31") %>% 
  select(-host_ID, -property_IDs, -data) %>% 
  group_by(date, neighbourhood) %>% 
  summarize(GH_2018 = sum(housing_units)) %>% 
  group_by(neighbourhood) %>% 
  summarize(GH_2018 = round(mean(GH_2018, na.rm = TRUE))) %>% 
  left_join(airbnb_neighbourhoods, ., by = c("name" = "neighbourhood")) %>% 
  mutate(GH_2018 = replace_na(GH_2018, 0))

airbnb_neighbourhoods <-
  FREH %>% 
  filter(date == "2018-08-31") %>% 
  left_join(select(st_drop_geometry(property), property_ID, neighbourhood)) %>% 
  count(neighbourhood) %>% 
  left_join(airbnb_neighbourhoods, .) %>% 
  mutate(FREH_2018 = replace_na(n, 0),
         housing_loss_2018 = GH_2018 + FREH_2018,
         housing_loss_yoy = housing_loss / housing_loss_2018) %>% 
  select(-n)


### Do vacancy rate analyses for CMHC neighbourhoods ###########################

property <- 
  st_intersection(property, select(CMHC, zone))

GH_CMHC <- 
  GH %>% 
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
  filter(date >= "2019-08-01") %>% 
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
  filter(date == "2019-08-31") %>% 
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


## Percentage of housing-loss listings which are condos

FREH %>% 
  filter(date == "2019-08-31",
         property_ID %in% (property %>% 
                             filter(housing == TRUE,
                                    str_detect(property_type, "condo|Condo") |
                                      str_detect(listing_title, "condo|Condo"))
                           )$property_ID)

GH %>% filter(date == "2019-08-31") %>% 
  pull(property_IDs) %>% 
  unlist() %>% 
  unique() %>% 
  {. %in% (property %>% 
             filter(housing == TRUE,
                    str_detect(property_type, "condo|Condo") |
                      str_detect(listing_title, "condo|Condo"))
  )$property_ID} %>% sum()


CMHC %>% 
  st_drop_geometry %>% 
  summarize(total_vacancy = sum(rental_units * rental_vacancy))



### Neighbourhood summary table ################################################

airbnb_neighbourhoods %>% 
  st_drop_geometry %>% 
  arrange(name) %>% 
  mutate(listings_per_1000 = 1000 * active_listings / households,
         housing_loss_yoy = housing_loss / housing_loss_2018) %>% 
  select(name, active_listings, active_listings_yoy, listings_per_1000,
         housing_loss, housing_loss_yoy) %>% 
  write_csv("nbhd.csv")
