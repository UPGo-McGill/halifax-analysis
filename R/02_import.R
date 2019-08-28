#### HALIFAX IMPORT ############################################################

source("R/01_helper_functions.R")

### Build geometries ###########################################################

HRM <-
  get_census(dataset = "CA16", regions = list(CSD = "1209034"), level = "CSD",
             geo_format = "sf") %>% 
  st_transform(32617)

streets <- 
  (getbb("Halifax") * c(1.01, 0.99, 0.99, 1.01)) %>% 
  opq(timeout = 200) %>% 
  add_osm_feature(key = "highway", value = c("secondary", "tertiary")) %>% 
  osmdata_sf()

HRM_streets <- 
  rbind(streets$osm_polygons %>% st_cast("LINESTRING"),streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32617) %>%
  st_intersection(HRM) %>% 
  select(osm_id, name, geometry)


### Import data from server ####################################################

con <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  user = "charlottebelot",
  password = "iR7AXqmyKmChXCtFMMjyzjbH",
  host = "025wpgs.campus.mcgill.ca",
  dbname = "airdna")

property_all <- tbl(con, "property")
daily_all <- tbl(con, "daily_old")

property <- 
  property_all %>% 
  filter(city == "Haifax") %>% 
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

rm(con, daily_all, property_all)



### Process files ##############################################################

## Set up property and daily files

property <- 
  property %>% 
  filter(!is.na(listing_type)) %>% 
  select(property_ID:longitude, ab_property:ha_host) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32617)

daily <- 
  strr_expand_daily(daily, cores = 4)

daily <- 
  property %>% 
  st_drop_geometry() %>% 
  select(property_ID, host_ID, listing_type, created, scraped, housing) %>% 
  left_join(daily, .)

daily <- 
  daily %>% 
  filter(date >= created, date - 30 <= scraped, status != "U")


## Add LTM revenue

exchange_rate <- mean(1.2873,	1.3129, 1.3130, 1.3041, 1.3037, 1.3010, 1.3200,
                      1.3432, 1.3301, 1.3206, 1.3368, 1.3378)

property <- 
  daily %>% 
  filter(date >= "2018-05-01", status == "R") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  select(property_ID, revenue) %>% 
  left_join(property, .)


## Create LTM_property

LTM_property <- property %>% 
  filter(created <= "2019-04-30", scraped >= "2019-04-30", housing == TRUE)



### Process multilistings ######################################################

## Prepare ML_daily

ML_daily <- 
  strr_expand_daily(ML_daily, cores = 4)

ML_daily <- 
  ML_property %>% 
  select(property_ID, host_ID, listing_type, created, scraped, housing) %>% 
  inner_join(ML_daily, .)

ML_daily <- 
  ML_daily %>% 
  filter(date >= created, date <= scraped + 30, status != "U")


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
  strr_FREH("2015-09-30", "2019-04-30", cores = 4) %>% as_tibble() %>% 
  filter(FREH == TRUE) %>% 
  select(-FREH)

GH <- 
  property %>% 
  filter(housing == TRUE) %>% 
  strr_ghost(property_ID, host_ID, created, scraped, "2014-10-01", "2019-04-30",
             listing_type, cores = 4)



### Save files #################################################################

save(HRM, file = "data/HRM.Rdata")
save(HRM_streets, file = "data/HRM_streets.Rdata")
save(property, file = "data/HRM_property.Rdata")
save(LTM_property, file = "data/HRM_LTM_property.Rdata")
save(GH, file = "data/HRM_GH.Rdata")
save(FREH, file = "data/HRM_FREH.Rdata")
save(daily, file = "data/HRM_daily.Rdata")
