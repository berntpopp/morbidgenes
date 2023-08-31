############################################
## load libraries
library(tidyverse)  ## needed for general table operations
library("R.utils")  ## gzip files
library(config)     ## needed for config loading
############################################


############################################
## define relative script path
project_topic <- "morbidgenes"
project_name <- "morbidgenes"
script_path <- "/db/R/"

## read config
config_vars_proj <- config::get(file = Sys.getenv("CONFIG_FILE"),
    config = project_topic)

## set working directory
setwd(paste0(config_vars_proj$projectsdir, script_path))
############################################


############################################
## set global options
options(scipen = 999)
############################################


############################################
## create user table
table_date <- strftime(as.POSIXlt(Sys.time(),
    "UTC", "%Y-%m-%dT%H:%M:%S"),
  "%Y-%m-%d")

user <- tibble(
    user_id = numeric(),
    user_name = character(),
    password = character(),
    email = character(),
    orcid = character(),
    first_name = character(),
    family_name = character(),
    user_role = character(),
    terms_agreed = integer(),
    approved = integer()
  ) %>%
  add_row(user_id = 1,
    user_name = "Admin",
    password = config_vars_proj$initial_user_pass[1],
    email = "admin@morbidgenes.org",
    user_role = "Administrator",
    orcid = "",
    first_name = "Admin",
    family_name = "Admin",
    terms_agreed = 1,
    approved = 1) %>%
  add_row(user_id = 2,
    user_name = "Bernt",
    password = config_vars_proj$initial_user_pass[2],
    email = "bernt.popp@gmail.com",
    user_role = "Curator",
    orcid = "0000-0002-3679-1081",
    first_name = "Bernt",
    family_name = "Popp",
    terms_agreed = 1,
    approved = 1) %>%
  arrange(user_id) %>%
  mutate(created_at = table_date) %>%
  mutate(password_reset_date = NA)
############################################


############################################
## export as csv with date of creation
creation_date <- strftime(as.POSIXlt(Sys.time(),
    "UTC", "%Y-%m-%dT%H:%M:%S"),
  "%Y-%m-%d")

write_csv(user,
  file = paste0("results/user.", creation_date, ".csv"), na = "")

gzip(paste0("results/user.", creation_date, ".csv"),
  overwrite = TRUE)
############################################
