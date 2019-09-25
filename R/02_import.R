#### HALIFAX IMPORT ############################################################

source("R/01_helper_functions.R")

### Build geometries ###########################################################

HRM <-
  get_census(dataset = "CA16", regions = list(CSD = "1209034"), level = "CSD",
             geo_format = "sf") %>% 
  st_transform(32617)

streets <- 
  (getbb("Halifax Nova Scotia") * c(1.01, 0.99, 0.99, 1.01)) %>% 
  opq(timeout = 200) %>% 
  add_osm_feature(key = "highway", value = c("secondary", "tertiary")) %>% 
  osmdata_sf()

HRM_streets <- 
  rbind(streets$osm_polygons %>% st_cast("LINESTRING"), streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32617) %>%
  st_intersection(HRM) %>% 
  select(osm_id, name, geometry)

neighbourhoods <-
  read_sf(dsn = "data", layer = "halifax")%>%
  st_transform(32617) %>% 
  select(id = OBJECTID, neighbourhood = OLD_DIST, geometry) %>% 
  group_by(neighbourhood) %>% 
  st_buffer(0) %>% 
  summarize(count = n())

### Census import #############################################################
CTs_halifax <-
  get_census(
    dataset = "CA16", regions = list(PR = "12"), level = "CT",
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6695",
                "v_CA16_4837", "v_CA16_4838", "v_CA16_512", 
                "v_CA16_3393", "v_CA16_3996"),
    geo_format = "sf") %>% 
  st_transform(32617) %>% 
  filter(Type == "CT") %>% 
  select(GeoUID, PR_UID, CMA_UID, Population, Households, contains("v_CA"))

names(CTs_halifax) <- 
  c("Geo_UID", "PR_UID", "CMA_UID", "population", "households", "med_income",
    "university_education", "housing_need", "non_mover", "owner_occupier", 
    "rental", "official_language", "citizen", "white", "geometry")

CTs_halifax <- CTs_halifax %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupier", "rental"),
    .funs = list(`pct_household` = ~{. / households}))

### Add census variables to neighbourhoods ##################################

neighbourhoods <- st_intersect_summarize(
  CTs_halifax,
  neighbourhoods,
  group_vars = vars(neighbourhood),
  population = population,
  sum_vars = vars(households, university_education, housing_need, non_mover, owner_occupier,
                  rental, official_language, citizen, white),
  mean_vars = vars(med_income)) %>% 
  ungroup() %>% 
  drop_units() %>% 
  mutate(households = households * population)

### Import data from server ####################################################

con <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  user = "amybolt",
  password = "",
  host = "025wpgs.campus.mcgill.ca",
  dbname = "airdna")

property_all <- tbl(con, "property")

daily_all <- tbl(con, "daily")

property <- 
  property_all %>% 
  filter(country == "Canada", city == "Halifax Regional Municipality") %>% 
  collect()

property <-  property %>% 
  filter(!is.na(listing_type)) %>% 
  select(property_ID:longitude, ab_property:ha_host, bedrooms) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32617)

daily_compressed <- 
  daily_all %>% 
  filter(property_ID %in% !! property$property_ID) %>% 
  collect()

# Set up multilisting file at this point as some hosts may have properties in other cities
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

start_date <- "2018-05-01"
end_date <- "2019-04-30"

daily <- 
  strr_expand_daily(daily_compressed, cores = 4)

daily <- 
  daily %>% 
  filter(date >= created, date - 30 <= scraped, status != "U")


## Run the raffle to assign a neighbourhood and a census tract #######################

property <- 
  property %>% 
strr_raffle(neighbourhoods, neighbourhood, households) %>% 
  mutate(neighbourhood = winner) %>% 
  select(-winner) %>% 
strr_raffle(CTs_halifax, Geo_UID, households) %>% 
  mutate(CT_GeoUID = winner) %>% 
  select(-winner)


## Add last twelve months revenue

exchange_rate <- mean(1.2873,	1.3129, 1.3130, 1.3041, 1.3037, 1.3010, 1.3200,
                      1.3432, 1.3301, 1.3206, 1.3368, 1.3378)

property <- 
  daily %>% 
  filter(date >= start_date, status == "R") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  select(property_ID, revenue) %>% 
  left_join(property, .)


## Create last twelve months property file

LTM_property <- property %>% 
  filter(created <= end_date, scraped >= start_date, housing == TRUE)


### Process multilistings ######################################################

## Prepare ML_daily

ML_daily <- 
  strr_expand_daily(ML_daily, cores = 4)

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
  strr_FREH("2015-09-30", end_date, cores = 4) %>% as_tibble() %>%   
  filter(FREH == TRUE) %>% 
  select(-FREH)

GH <- 
  property %>% 
  filter(housing == TRUE) %>% 
  strr_ghost(start_date = "2014-10-01", end_date = end_date)


### Atlantic Canada #############################################################
daily_AC <- 
  daily_all %>% 
  filter(country == "Canada", region %in% c("Nova Scotia", "New Brunswick", 
                                            "Prince Edward Island", "Newfoundland and Labrador")) %>% 
  collect()

### Save files #################################################################

save(HRM, file = "data/HRM.Rdata")
save(HRM_streets, file = "data/HRM_streets.Rdata")
save(property, file = "data/HRM_property.Rdata")
save(LTM_property, file = "data/HRM_LTM_property.Rdata")
save(GH, file = "data/HRM_GH.Rdata")
save(FREH, file = "data/HRM_FREH.Rdata")
save(daily, file = "data/HRM_daily.Rdata")
save(daily_compressed, file = "data/HRM_daily_compressed.Rdata")
save(CTs_halifax, file = "data/CTs_halifax.Rdata")
save(neighbourhoods, file = "data/HRM_neighbourhoods.Rdata")
save(daily_AC, file = "data/daily_AC.Rdata")
