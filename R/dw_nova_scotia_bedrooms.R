#### NOVA SCOTIA BEDROOM COUNT #################################################

library(tidyverse)
library(sf)
library(upgo)
library(strr)
library(data.table)


upgo_connect()

property <- 
  property_all %>% 
  filter(country == "Canada", region == "Nova Scotia") %>% 
  collect()

daily <- 
  daily_all %>% 
  filter(property_ID %in% !! property$property_ID) %>% 
  collect()

ML_property <- 
  property_all %>% 
  filter(host_ID %in% !! property$host_ID) %>% 
  collect()

ML_daily <- 
  daily_all %>% 
  filter(property_ID %in% !! ML_property$property_ID) %>% 
  collect()

upgo_disconnect()


### Expand daily files #########################################################

daily <- strr_expand(daily, cores = 6)
ML_daily <- strr_expand(ML_daily, cores = 6)

save(daily, ML_daily, property, ML_property, file = "province.Rdata")


### Process property and daily files ###########################################

property <-
  property %>% 
  filter(created <= "2019-08-31", scraped >= "2018-09-01") %>% 
  select(property_ID, host_ID, listing_type:longitude, city, bedrooms)

ML_property <-
  ML_property %>% 
  filter(created <= "2019-08-31", scraped >= "2018-09-01") %>% 
  select(property_ID, host_ID, listing_type:longitude, city, bedrooms)

daily <- 
  daily %>% 
  filter(date >= created, date - 30 <= scraped, status != "U",
         date >= "2018-09-01")

ML_daily <- 
  ML_daily %>% 
  filter(date >= created, date - 30 <= scraped, status != "U",
         date >= "2018-09-01")


### Process multilistings ######################################################

## Do ML calculations

EH_ML <- 
  ML_daily %>% 
  filter(listing_type == "Entire home/apt") %>% 
  group_by(listing_type, host_ID, date) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n >= 2) %>% 
  mutate(ML = TRUE)

PR_ML <- 
  ML_daily %>% 
  filter(listing_type == "Private room") %>% 
  group_by(listing_type, host_ID, date) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n >= 3) %>% 
  mutate(PR_ML = TRUE)

daily <- 
  EH_ML %>% 
  select(-n) %>% 
  left_join(daily, .)

daily <- 
  PR_ML %>% 
  select(-n) %>% 
  left_join(daily, .) %>% 
  mutate(ML = if_else(is.na(ML), PR_ML, ML)) %>% 
  mutate(ML = if_else(is.na(ML), FALSE, ML)) %>% 
  select(-PR_ML)


### Calculate FREH and GH listings #############################################

FREH <- 
  daily %>% 
  strr_FREH("2018-09-01", "2019-08-31", cores = 4) %>% as_tibble() %>%   
  filter(FREH == TRUE) %>% 
  select(-FREH)

daily <- daily %>% as_tibble()

GH <- 
  property %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32617) %>% 
  strr_ghost(start_date = "2018-09-01", end_date = "2019-08-31")


### Calculate primary residence ################################################

## LFRML calculations

# Add ML field to property file
property <- 
  daily %>% 
  filter(date == "2019-08-31") %>% 
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
  filter(date >= "2019-08-01") %>% 
  pull(property_IDs) %>%
  unlist() %>%
  unique()

property <-
  property %>% 
  mutate(GH = if_else(property_ID %in% GH_list, TRUE, FALSE))

# Add FREH status
property <- 
  FREH %>% 
  filter(date == "2019-08-31") %>% 
  mutate(FREH = TRUE) %>% 
  left_join(property, .) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH))

property <- property %>% ungroup()

# Add legal field
legal <- 
  property %>%
  filter(created <= "2019-08-31", scraped >= "2019-08-31") %>% 
  mutate(legal = case_when(
    housing == FALSE               ~ FALSE,
    GH == TRUE                     ~ FALSE,
    listing_type == "Shared room"  ~ TRUE,
    listing_type == "Private room" ~ TRUE,
    FREH == TRUE                   ~ FALSE,
    LFRML == TRUE                  ~ TRUE,
    ML == TRUE                     ~ FALSE,
    TRUE                           ~ TRUE))


### Aggregate information by hosts #############################################

## Filter to just commercial hosts

commercial_hosts <-
  legal %>% 
  filter(!is.na(host_ID)) %>%
  filter(legal == FALSE) %>% 
  count(host_ID)

commercial_property <- 
  legal %>% 
  filter(host_ID %in% commercial_hosts$host_ID)

commercial_bedrooms <-
  commercial_property %>% 
  group_by(host_ID) %>% 
  tally(bedrooms)

sum(commercial_bedrooms$n >= 11)
sum(commercial_bedrooms$n <= 10)

commercial_property %>% 
  ungroup() %>% 
  filter(host_ID %in% filter(commercial_bedrooms, n >= 11)$host_ID) %>% 
  tally(bedrooms)

commercial_property %>% 
  ungroup() %>% 
  filter(host_ID %in% filter(commercial_bedrooms, n <= 10)$host_ID) %>% 
  tally(bedrooms)



### Presentation of information ################################################

property %>% 
  filter(created <= "2019-08-31", scraped >= "2019-09-01") %>%
  nrow()

property %>% 
  filter(created <= "2019-08-31", scraped >= "2019-09-01") %>%
  filter(housing == TRUE) %>% 
  nrow()

#' On August 31, 2019, we count 6895 listings active on Airbnb, HomeAway and 
#' VRBO in the province of Nova Scotia. (This includes 749 listings which are
#' not operated out of conventional housing units, which we excluded from our
#' Halifax report but which presumably would be required to be registered.)


property %>% 
  filter(is.na(host_ID)) %>% 
  nrow()

#' 1016 of these listings are HomeAway or VRBO and have no easily
#' identifiable host information. (This is a problem we are currently working on
#' solving, and would be in a position to do if we were taking this on in a more
#' formal way, but we don't have it ready just yet.) Because we can't make any
#' assumptions about these listings, we will assume that they have the same
#' profile as the 5879 listings for which we do have authoritative host
#' information. So remaining numbers will be rounded to reflect this 
#' uncertainty.

property %>% 
  filter(created <= "2019-08-31", scraped >= "2019-09-01", !is.na(host_ID)) %>%
  count(host_ID, sort = TRUE) %>% 
  nrow() %>% 
  {. * 6895/5879}

property %>% 
  filter(created <= "2019-08-31", scraped >= "2019-09-01", !is.na(host_ID)) %>%
  filter(housing == TRUE) %>% 
  count(host_ID) %>% 
  nrow() %>% 
  {. * 6895/5879}

legal %>% 
  # filter(housing == TRUE) %>% 
  filter(legal == FALSE) %>% 
  count(host_ID) %>% 
  nrow() %>% 
  {. * 6895/5879}

#' These listings are operated by 4480 hosts (4300 if non-housing listings are
#' excluded.) Using a combination of data points about the activity of STR
#' listings and their structural characteristics, we estimate that 2740 hosts
#' are only offering listings in their principal residence, while 1745 hosts are
#' offering listings which are not located in their principal residences.

sum(commercial_bedrooms$n >= 11) * 1745/1427 
sum(commercial_bedrooms$n <= 10) * 1745/1427


#' Narrowing in on the 1745 hosts offering non-principal-residence STRs, 90 of 
#' them have listings with a total of 11 or more bedrooms, while the remaining
#' 1655 control listings with 10 or fewer bedrooms. In other words, 
#' approximately 5% of commercial hosts would fall into the 11+ bedrooms
#' category.


commercial_property %>% 
  ungroup() %>% 
  filter(host_ID %in% filter(commercial_bedrooms, n >= 11)$host_ID) %>% 
  tally(bedrooms) %>% 
  {. * 6895/5879}

commercial_property %>% 
  ungroup() %>% 
  filter(host_ID %in% filter(commercial_bedrooms, n <= 10)$host_ID) %>% 
  tally(bedrooms) %>% 
  {. * 6895/5879}

#' However, because these hosts operate a large number of listings, they account
#' for much more than 5% of the bedrooms being offered in 
#' non-principal-residence listings. The 11+ bedroom hosts control 2390
#' bedrooms, while the remaining commercial hosts control 4910 bedrooms. In
#' other words, the 5% of commercial hosts with 11 or more bedrooms in fact
#' host 33% of the bedrooms offered by non-principal-residence operators.



save.image("working_image.Rdata")
