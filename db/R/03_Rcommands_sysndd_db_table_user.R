############################################
## load libraries
library(tidyverse)  ##needed for general table operations
library("R.utils")  ## gzip files
############################################



############################################
## set working directory
# TODO: needs to be adapted to your specific working directory
setwd("C:/development/morbidgenes/db/R")
## set global options
options(scipen = 999)
############################################



############################################
##
table_date <- strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")

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
    password = "testpassword1234",
    email = "admin@sysndd.org",
    user_role = "Administrator",
    orcid = "",
    first_name = "Bernt",
    family_name = "Popp",
    terms_agreed = 1,
    approved = 1) %>%
  add_row(user_id = 2,
    user_name = "Bernt",
    password = "testpassword1234",
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
creation_date <- strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")
write_csv(user, file = paste0("results/user.",creation_date, ".csv"), na = "")
gzip(paste0("results/user.",creation_date,".csv"), overwrite = TRUE)
############################################
