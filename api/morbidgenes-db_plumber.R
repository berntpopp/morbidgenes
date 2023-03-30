# morbidgenes-db_plumber.R
## to do: adapt "serializer json list(na="null")"

##-------------------------------------------------------------------##
# load libraries -----
library(plumber)
library(tidyverse)
library(stringr)
library(cowplot)
library(DBI)
library(RMariaDB)
library(jsonlite)
library(config)
library(jose)
library(plotly)
library(RCurl)
library(stringdist)
library(xlsx)
library(easyPubMed)
library(rvest)
library(lubridate)
library(pool)
library(memoise)
library(coop)
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
dw <- config::get("morbidgenes_db_local")
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = dw$dbname,
  host = dw$host,
  user = dw$user,
  password = dw$password,
  server = dw$server,
  port = dw$port
)
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
## global variables

##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
##-------------------------------------------------------------------##
# Define global functions


##-------------------------------------------------------------------##
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
##-------------------------------------------------------------------##
#* @apiTitle morbidgenes-db API

#* @apiDescription This is the API powering the morbidgenes-db website and allowing programmatic access to the database contents.
#* @apiVersion 0.1.0
#* @apiTOS http://www.morbidgenes.org/terms/
#* @apiContact list(name = "API Support", url = "http://www.morbidgenes.org/support", email = "support@morbidgenes.org")
#* @apiLicense list(name = "CC BY 4.0", url = "https://creativecommons.org/licenses/by/4.0/")

#* @apiTag panel Endpoint to get the current or a specific panel version
#* @apiTag authentication Endpoint for user authentication
#* @apiTag user Endpoint for user tables
##-------------------------------------------------------------------##
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
# filters -----

##### cross origin requests =====

#* @filter cors
## enable cross origin requests
## based on https://github.com/rstudio/plumber/issues/66
function(req, res) {
  
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200 
    return(list())
  } else {
    plumber::forward()
  }
}


##### check sign_in =====
#* @filter check_signin
#* checks signin from header token and set user variable to request
function(req, res) {
  # load secret and convert to raw
  key <- charToRaw(dw$secret)
  
  if (req$REQUEST_METHOD == "GET" && is.null(req$HTTP_AUTHORIZATION)) {
    plumber::forward()
  } else if (req$REQUEST_METHOD == "GET" && !is.null(req$HTTP_AUTHORIZATION)) {
    # load jwt from header
    jwt <- str_remove(req$HTTP_AUTHORIZATION, "Bearer ")
    # decode jwt
    user <- jwt_decode_hmac(str_remove(req$HTTP_AUTHORIZATION, "Bearer "),
                            secret = key)
    # add user_id and user_role as value to request
    req$user_id <- as.integer(user$user_id)
    req$user_role <- user$user_role
    # and forward request
    plumber::forward()
  } else if (req$REQUEST_METHOD == "POST" &&
             (req$PATH_INFO == "/api/panel/")) {
    # and forward request
    plumber::forward()
  } else {
    if (is.null(req$HTTP_AUTHORIZATION)) {
      res$status <- 401 # Unauthorized
      return(list(error = "Authorization http header missing."))
    } else if (jwt_decode_hmac(str_remove(req$HTTP_AUTHORIZATION, "Bearer "),
                               secret = key)$exp < as.numeric(Sys.time())) {
      res$status <- 401 # Unauthorized
      return(list(error = "Token expired."))
    } else {
      # load jwt from header
      jwt <- str_remove(req$HTTP_AUTHORIZATION, "Bearer ")
      # decode jwt
      user <- jwt_decode_hmac(str_remove(req$HTTP_AUTHORIZATION, "Bearer "),
                              secret = key)
      # add user_id and user_role as value to request
      req$user_id <- as.integer(user$user_id)
      req$user_role <- user$user_role
      # and forward request
      plumber::forward()
    }
  }
}
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
# Panel endpoints ------

##### get panel ======

#* @tag panel
## get specific panel version (default: current)
#* @serializer json list(na="string")
#' @get /api/panel/
function(res, sort = "symbol", `page[after]` = 0, `page[size]` = "all", version = "current") {
  
  panel = if(version == "current"){
    pool %>% 
      tbl("view_panel_current")
  } else{
    pool %>% 
      tbl("view_panel_all") %>% 
      filter(panel_version == version)
  }
  
  # get number of rows in view_panel_current
  morbidgenes_db_panel_current_rows <- (panel %>% 
                                          summarise(n = n()) %>%
                                          collect()
  )$n
  
  # split the sort input by comma and check if hgnc_ids in the resulting list, if not append to the list for unique sorting
  sort_list <- str_split(str_squish(sort), ",")[[1]]
  
  if ( !("hgnc_id" %in% sort) ){
    sort_list <- append(sort, "hgnc_id")
  }
  
  # check if `page[size]` is either "all" or a valid integer and convert or assign values accordingly
  if ( `page[size]` == "all" ){
    page_after <- 0
    page_size <- morbidgenes_db_panel_current_rows
    page_count <- ceiling(morbidgenes_db_panel_current_rows/page_size)
  } else if ( is.numeric(as.integer(`page[size]`)) )
  {
    page_after <- as.integer(`page[after]`)
    page_size <- as.integer(`page[size]`)
    page_count <- ceiling(morbidgenes_db_panel_current_rows/page_size)
  } else
  {
    res$status <- 400 #Bad Request
    return(list(error="Invalid Parameter Value Error."))
  }
  
  # get data from database
  morbidgenes_db_panel_current_table <- panel %>% 
    arrange(!!!syms(sort_list)) %>%
    collect()
  
  # find the current row of the requested page_after entry
  page_after_row <- (morbidgenes_db_panel_current_table %>%
                       mutate(row = row_number()) %>%
                       filter(hgnc_id == page_after)
  )$row
  
  if ( length(page_after_row) == 0 ){
    page_after_row <- 0
    page_after_row_next <- ( morbidgenes_db_panel_current_table %>%
                               filter(row_number() == page_after_row + page_size + 1) )$hgnc_id
  } else {
    page_after_row_next <- ( morbidgenes_db_panel_current_table %>%
                               filter(row_number() == page_after_row + page_size) )$hgnc_id
  }
  
  # find next and prev item row
  page_after_row_prev <- ( morbidgenes_db_panel_current_table %>%
                             filter(row_number() == page_after_row - page_size) )$hgnc_id
  page_after_row_last <- ( morbidgenes_db_panel_current_table %>%
                             filter(row_number() ==  page_size * (page_count - 1) ) )$hgnc_id
  
  # filter by row
  morbidgenes_db_panel_current_collected <- morbidgenes_db_panel_current_table %>%
    filter(row_number() > page_after_row & row_number() <= page_after_row + page_size)
  
  # generate links for self, next and prev pages
  self <- paste0("http://", dw$host, ":", dw$port_self, "/api/reports/?sort=", sort, "&page[after]=", `page[after]`, "&page[size]=", `page[size]`)
  if ( length(page_after_row_prev) == 0 ){
    prev <- "null"
  } else
  {
    prev <- paste0("http://", dw$host, ":", dw$port_self, "/api/reports?sort=", sort, "&page[after]=", page_after_row_prev, "&page[size]=", `page[size]`)
  }
  
  if ( length(page_after_row_next) == 0 ){
    `next` <- "null"
  } else
  {
    `next` <- paste0("http://", dw$host, ":", dw$port_self, "/api/reports?sort=", sort, "&page[after]=", page_after_row_next, "&page[size]=", `page[size]`)
  }
  
  if ( length(page_after_row_last) == 0 ){
    last <- "null"
  } else
  {
    last <- paste0("http://", dw$host, ":", dw$port_self, "/api/reports?sort=", sort, "&page[after]=", page_after_row_last, "&page[size]=", `page[size]`)
  }
  
  links <- as_tibble(list("prev" = prev, "self" = self, "next" = `next`, "last" = last))
  
  # 
  list(links = links, data = morbidgenes_db_panel_current_collected)
}


##### get excel ======

#* @tag panel
## get current panel for download as Excel file
#* @serializer contentType list(type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
#' @get /api/panel/excel/
function(res) {
  
  # get data from database
  morbidgenes_current <- pool %>% 
    tbl("view_panel_current") %>%
    collect()
  
  # generate request statistic for output
  creation_date <- strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%dT %H:%M:%S")
  
  request_stats <- tibble(
    creation_date = creation_date
  ) %>%
    pivot_longer(everything(), names_to = "request", values_to = "value")
  
  # generate excel file output
  filename <- file.path(tempdir(), "morbidgenes_current.xlsx")
  write.xlsx(morbidgenes_current, filename, sheetName="morbidgenes_current", append=FALSE)
  write.xlsx(request_stats, filename, sheetName="request", append=TRUE)
  attachmentString = paste0("attachment; filename=morbidgenes_current.", creation_date, ".xlsx")
  
  res$setHeader("Content-Disposition", attachmentString)
  
  # Read in the raw contents of the binary file
  bin <- readBin(filename, "raw", n=file.info(filename)$size)
  
  #Check file existence and delete
  if (file.exists(filename)) {
    file.remove(filename)
  }
  
  #Return the binary contents
  bin
}


##### get by HGNC ID =====

#* @tag panel
## get infos for a single gene in the current panel by hgnc_id
#* @serializer json list(na="string")
#' @get /api/panel/<hgnc_id>
function(hgnc_id) {
  
  hgnc <- paste0("HGNC:", URLdecode(gsub("[^0-9.-]", "", hgnc_id)))
  
  # get data from database and filter
  hgnc_collected <- pool %>% 
    tbl("view_panel_current") %>%
    filter(hgnc_id == hgnc) %>%
    arrange(hgnc_id) %>%
    collect()
}

## Panel endpoints
##-------------------------------------------------------------------##


##-------------------------------------------------------------------##
# Authentication endpoints ------

##### authenticate =====
#* @tag authentication
#* does user login
## based on "https://github.com/
## jandix/sealr/blob/master/examples/jwt_simple_example.R"
#* @serializer json list(na="string")
#' @get /api/auth/authenticate
function(req, res, user_name, password) {
  
  check_user <- user_name
  check_pass <- URLdecode(password)
  
  # load secret and convert to raw
  key <- charToRaw(dw$secret)
  
  # check if user provided credentials
  if (is.null(check_user) ||
      nchar(check_user) < 5 ||
      nchar(check_user) > 20 ||
      is.null(check_pass) ||
      nchar(check_pass) < 5 ||
      nchar(check_pass) > 50) {
    res$status <- 404
    res$body <- "Please provide valid username and password."
    res
  }
  
  # connect to database, find user in database and password is correct
  user_filtered <- pool %>%
    tbl("user") %>%
    filter(user_name == check_user & password == check_pass & approved == 1) %>%
    select(-password) %>%
    collect() %>%
    mutate(iat = as.numeric(Sys.time())) %>%
    mutate(exp = as.numeric(Sys.time()) + dw$refresh)
  
  # return answer depending on user credentials status
  if (nrow(user_filtered) != 1) {
    res$status <- 401
    res$body <- "User or password wrong."
    res
  }
  
  if (nrow(user_filtered) == 1) {
    claim <- jwt_claim(user_id = user_filtered$user_id,
                       user_name = user_filtered$user_name,
                       email = user_filtered$email,
                       user_role = user_filtered$user_role,
                       user_created = user_filtered$created_at,
                       iat = user_filtered$iat,
                       exp = user_filtered$exp)
    
    jwt <- jwt_encode_hmac(claim, secret = key)
    jwt
  }
}



##### sign in ======

#* @tag authentication
#* does user authentication
#* @serializer json list(na="string")
#' @get /api/auth/signin
function(req, res) {
  # load secret and convert to raw
  key <- charToRaw(dw$secret)
  
  # load jwt from header
  jwt <- str_remove(req$HTTP_AUTHORIZATION, "Bearer ")
  
  user <- jwt_decode_hmac(jwt, secret = key)
  user$token_expired <- (user$exp < as.numeric(Sys.time()))
  
  if (is.null(jwt) || user$token_expired) {
    res$status <- 401 # Unauthorized
    return(list(error = "Authentication not successful."))
  } else {
    return(list(user_id = user$user_id,
                user_name = user$user_name,
                email = user$email,
                user_role = user$user_role,
                user_created = user$user_created,
                exp = user$exp))
  }
}


##### refresh =====

#* @tag authentication
#* does authentication refresh
#* @serializer json list(na="string")
#' @get /api/auth/refresh
function(req, res) {
  # load secret and convert to raw
  key <- charToRaw(dw$secret)
  
  # load jwt from header
  jwt <- str_remove(req$HTTP_AUTHORIZATION, "Bearer ")
  
  user <- jwt_decode_hmac(jwt, secret = key)
  user$token_expired <- (user$exp < as.numeric(Sys.time()))
  
  if (is.null(jwt) || user$token_expired) {
    res$status <- 401 # Unauthorized
    return(list(error = "Authentication not successful."))
  } else {
    claim <- jwt_claim(user_id = user$user_id,
                       user_name = user$user_name,
                       email = user$email,
                       user_role = user$user_role,
                       user_created = user$user_created,
                       iat = as.numeric(Sys.time()),
                       exp = as.numeric(Sys.time()) + dw$refresh)
    
    jwt <- jwt_encode_hmac(claim, secret = key)
    jwt
  }
}

##Authentication section
##-------------------------------------------------------------------##


##-------------------------------------------------------------------##
# User endpoint section -----

## get user table =====

#* @tag user
#* gets a summary table of users
#* @serializer json list(na="string")
#' @get /api/user/table
function(req, res) {
  
  user <- req$user_id
  
  # first check rights
  if (length(user) == 0) {
    
    res$status <- 401 # Unauthorized
    return(list(error = "Please authenticate."))
    
  } else if (req$user_role %in% c("Administrator")) {
    
    user_table <- pool %>%
      tbl("user") %>%
      select(user_id,
             user_name,
             email,
             created_at,
             user_role,
             approved) %>%
      collect()
    
    # return tibble
    user_table
    
  } else if (req$user_role %in% c("Curator")) {
    
    user_table <- pool %>%
      tbl("user") %>%
      select(user_id,
             user_name,
             email,
             created_at,
             user_role,
             approved) %>%
      filter(approved == 0) %>%
      collect()
    
    # return tibble
    user_table
    
  } else {
    res$status <- 403 # Forbidden
    return(list(error = "Read access forbidden."))
  }
}

##User section
##-------------------------------------------------------------------##
