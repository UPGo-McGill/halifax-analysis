#### HALIFAX LIBRARIES AND HELPER FUNCTIONS ####################################

### Libraries ##################################################################

library(tidyverse)
library(sf)
library(RPostgres)
library(extrafont)
library(zoo)
library(cancensus)
library(ggspatial)
library(scales)
library(parallel)
library(pbapply)
library(osmdata)
library(data.table)
library(ggplot2)
library(units)
library(devtools)
install_github("dwachsmuth/strr", force = TRUE)
library(strr)
library(spatstat)
library(polyCub)

### Cancensus api #############################################################
options(cancensus.api_key = "")

### Helper functions ###########################################################

## st_intersect_summarize helper function
st_intersect_summarize <- function(data, poly, group_vars, population, sum_vars,
                                   mean_vars) {
  
  pop <- enquo(population)
  
  data <- data %>% 
    mutate(CT_area = st_area(.))
  
  intersects <- suppressWarnings(st_intersection(data, poly)) %>%
    mutate(int_area_pct = st_area(.data$geometry) / .data$CT_area,
           population_int = !! pop * int_area_pct) %>%
    group_by(!!! group_vars)
  
  population <- intersects %>% 
    summarize(!! pop := sum(population_int, na.rm = TRUE))
  
  sums <- intersects %>%
    summarize_at(sum_vars, ~{sum(. * int_area_pct, na.rm = TRUE) /
        sum(population_int, na.rm = TRUE)})
  
  means <- intersects %>% 
    summarize_at(mean_vars, ~{
      sum(. * population_int, na.rm = TRUE) / sum(population_int, na.rm = TRUE)
    })
  
  suppressMessages(reduce(list(population,
                               st_drop_geometry(sums),
                               st_drop_geometry(means)),
                          full_join))
  
}

# function to identify frequently rented entire homes
strr_FREH <- function(daily, start_date, end_date, property_ID = property_ID,
                      date = date, status = status, status_types = c("R", "A"),
                      listing_type = listing_type,
                      entire_home = "Entire home/apt", n_days = 365, R_cut = 90,
                      AR_cut = 183, cores = 1) {
  
  .datatable.aware = TRUE
  
  setDT(daily)
  
  # Wrangle dates
  start_date <- as.Date(start_date, origin = "1970-01-01")
  end_date <- as.Date(end_date, origin = "1970-01-01")
  
  # Filter daily file
  daily <-
    daily[housing == TRUE & status %in% c("A", "R") & date >= start_date - 364 &
            date <= end_date & listing_type == "Entire home/apt"]
  
  if (cores > 1) {
    cl <- parallel::makeForkCluster(cores)
    pbapply::pblapply(start_date:end_date, function(date_check) {
      daily <- daily[date >= date_check - 364 & date <= date_check]
      daily[, AR := .N, by = property_ID]
      daily[, R := sum(status == "R"), by = property_ID]
      daily[, list(date = as.Date(date_check, origin = "1970-01-01"),
                   FREH = as.logical((mean(AR) >= AR_cut) * (mean(R) >= R_cut))),
            by = property_ID]
    }) %>% rbindlist()
  } else {
    lapply(start_date:end_date, function(date_check) {
      daily <- daily[date >= date_check - 364 & date <= date_check]
      daily[, AR := .N, by = property_ID]
      daily[, R := sum(status == "R"), by = property_ID]
      daily[, list(date = as.Date(date_check, origin = "1970-01-01"),
                   FREH = as.logical((mean(AR) >= AR_cut) * (mean(R) >= R_cut))),
            by = property_ID]
    }) %>% rbindlist()
  }
}

## ggplot bounding box function
gg_bbox <- function(geom, x1 = 0, x2 = 1, y1 = 0, y2 = 1) {
  
  bbox <- st_bbox(geom)
  
  matrix_x <- matrix(bbox[c(1,3)], nrow = 1) %*% matrix(
    c(1 - x1, x1, 1 - x2, x2), nrow = 2)
  
  matrix_y <- matrix(bbox[c(2,4)], nrow = 1) %*% matrix(
    c(1 - y1, y1, 1- y2, y2), nrow = 2)
  
  coord_sf(xlim = as.vector(matrix_x), ylim = as.vector(matrix_y))
}

# number of groups
n_groups <- function(tbl) {
  g <- groups(tbl)
  
  if (is.null(g)) {
    tbl_nm <- deparse(substitute(tbl))
    warning(tbl_nm, " is not grouped", immediate. = TRUE)
    0
  } else {
    g <- unlist(lapply(g, as.character))
    nrow(unique(tbl[, g, drop = FALSE]))
  }
}
