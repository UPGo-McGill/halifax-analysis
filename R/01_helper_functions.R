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
library(units)
library(cowplot)
library(devtools)
install_github("dwachsmuth/strr")
install_github("UPGo-McGill/upgo")
library(strr)
library(upgo)


### Helper functions ###########################################################

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
                   FREH = as.logical((mean(AR) >= AR_cut) * 
                                       (mean(R) >= R_cut))),
            by = property_ID]
    }) %>% rbindlist()
  } else {
    lapply(start_date:end_date, function(date_check) {
      daily <- daily[date >= date_check - 364 & date <= date_check]
      daily[, AR := .N, by = property_ID]
      daily[, R := sum(status == "R"), by = property_ID]
      daily[, list(date = as.Date(date_check, origin = "1970-01-01"),
                   FREH = as.logical((mean(AR) >= AR_cut) * 
                                       (mean(R) >= R_cut))),
            by = property_ID]
    }) %>% rbindlist()
  }
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
