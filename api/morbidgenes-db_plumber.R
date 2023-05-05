# morbidgenes-db_plumber.R
## TODO: adapt "serializer json list(na="null")"

##-------------------------------------------------------------------##
# load libraries
library(plumber)
library(tidyverse)
library(cowplot)
library(DBI)
library(RMariaDB)
library(jsonlite)
library(config)
library(jose)
library(RCurl)
library(stringdist)
library(xlsx)
library(easyPubMed)
library(rvest)
library(lubridate)
library(pool)
library(memoise)
library(coop)
library(keyring)
library(rlang)
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
dw <- config::get(Sys.getenv("API_CONFIG"))
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
## global variables
# define serializers
serializers <- list(
  "json" = serializer_json(),
  "xlsx" = serializer_content_type(type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
)

# time as GMT
Sys.setenv(TZ = "GMT")
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
# generate a pool of connections to the database
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
##-------------------------------------------------------------------##
# Global API functions
options("plumber.apiURL" = dw$api_base_url)

# load source files
source("functions/helper-functions.R", local = TRUE)

# convert to memoise functions
# Expire items in cache after 60 minutes
# and set cache 100 MB limit
cm <- cachem::cache_mem(max_age = 60 * 60,
  max_size = 100 * 1024 ^ 2)

generate_tibble_fspec_mem <- memoise(generate_tibble_fspec,
  cache = cm)

##-------------------------------------------------------------------##
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
##-------------------------------------------------------------------##
#* @apiTitle morbidgenes-db API

#* @apiDescription This is the API powering the morbidgenes-db website
#* and allowing programmatic access to the database contents.
#* @apiVersion 0.1.0
#* @apiTOS http://www.morbidgenes.org/terms/
#* @apiContact list(name = "API Support",
#* url = "http://www.morbidgenes.org/support",
#* email = "support@morbidgenes.org")
#* @apiLicense list(name = "CC BY 4.0",
#* url = "https://creativecommons.org/licenses/by/4.0/")

#* @apiTag panel Endpoint to get the current MorbidGenes Panel
##-------------------------------------------------------------------##
##-------------------------------------------------------------------##


##-------------------------------------------------------------------##
## hooks

#* @plumber
function(pr) {

  pr %>%
    plumber::pr_hook("exit", function() {
      pool::poolClose(pool)
      message("Disconnected")
    })
}
##-------------------------------------------------------------------##


##-------------------------------------------------------------------##
## filters

#* @filter cors
## enable cross origin requests
## based on https://github.com/rstudio/plumber/issues/66
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "*")
    res$setHeader("Access-Control-Allow-Headers",
      req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

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
      (req$PATH_INFO == "/api/upload/csv")) {
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
## Panel endpoints

#* @tag panel
#* allows filtering and field selection from all panels
#* @serializer json list(na="string")
#* @param sort:str  Output column to arrange output on.
#* @param filter:str Comma separated list of filters to apply.
#* @param fields:str Comma separated list of output columns.
#* @param page_after:str Cursor after which entries are shown.
#* @param page_size:str Page size in cursor pagination.
#* @param fspec:str Fields to generate fied specification for.
#* @response 200 A cursor pagination object with links, meta information and gene objects in the data field.
#* @response 500 Internal server error.
#' @get /api/panel/
function(req,
  res,
  sort = "symbol",
  filter = "",
  fields = "",
  `page_after` = "",
  `page_size` = "all",
  fspec = "panel_version,hgnc_id,symbol,bed_hg19,bed_hg38,PanelApp,AustraliaPanelApp,HGMD_pathogenic,Phenotype_MIM,ClinVarPathogenic,UKPanelApp,SysNDD,Manually,mg_score",
  format = "json") {
  # set serializers
  res$serializer <- serializers[[format]]

  # TODO: most the following code needs to go into a function script
  start_time <- Sys.time()

  # generate sort expression based on sort input
  sort_exprs <- generate_sort_expressions(sort, unique_id = "hgnc_id")

  # generate filter expression based on filter input
  filter_exprs <- generate_filter_expressions(filter)

  # get data from database
  mg_panel_current_view <- pool %>%
    tbl("view_panel_current") %>%
    collect()

  mg_panel_current_table <- mg_panel_current_view %>%
    arrange(!!!rlang::parse_exprs(sort_exprs)) %>%
    filter(!!!rlang::parse_exprs(filter_exprs))

  # select fields from table based on input
  # using the helper function "select_tibble_fields"
  mg_panel_current_table <- select_tibble_fields(mg_panel_current_table,
    fields,
    "hgnc_id")

  # use the helper generate_cursor_pag_inf to
  # generate cursor pagination information from a tibble
  mg_panel_pag_info <- generate_cursor_pag_inf(
    mg_panel_current_table,
    `page_size`,
    `page_after`,
    "hgnc_id")

  # use the helper generate_tibble_fspec to
  # generate fields specs from a tibble
  # first for the unfiltered and unsubset table
  mg_panel_current_fspec <- generate_tibble_fspec_mem(mg_panel_current_view,
    fspec)
  # then for the filtered/ subset one
  mg_panel_current_table_fspec <- generate_tibble_fspec_mem(
    mg_panel_current_table,
    fspec)
  # assign the second to the first as filtered
  mg_panel_current_fspec$fspec$count_filtered <-
    mg_panel_current_table_fspec$fspec$count

  # compute execution time
  end_time <- Sys.time()
  execution_time <- as.character(paste0(round(end_time - start_time, 2),
    " secs"))

  # add columns to the meta information from
  # generate_cursor_pag_inf function return
  meta <- mg_panel_pag_info$meta %>%
    add_column(tibble::as_tibble(list("sort" = sort,
    "filter" = filter,
    "fields" = fields,
    "fspec" = mg_panel_current_fspec,
    "executionTime" = execution_time)))

  # add host, port and other information to links from
  # the link information from generate_cursor_pag_inf function return
  links <- mg_panel_pag_info$links %>%
      pivot_longer(everything(), names_to = "type", values_to = "link") %>%
    mutate(link = case_when(
      link != "null" ~ paste0(
        dw$api_base_url,
        "/api/panel?sort=",
        sort,
        ifelse(filter != "", paste0("&filter=", filter), ""),
        ifelse(fields != "", paste0("&fields=", fields), ""),
        link),
      link == "null" ~ "null"
    )) %>%
      pivot_wider(everything(), names_from = "type", values_from = "link")

  # generate object to return
  return_list <- list(links = links,
    meta = meta,
    data = mg_panel_pag_info$data)

  # if xlsx requested compute this and return
  if (format == "xlsx") {
    # generate creation date statistic for output
    creation_date <- strftime(as.POSIXlt(Sys.time(),
      "UTC",
      "%Y-%m-%dT%H:%M:%S"),
      "%Y-%m-%d_T%H-%M-%S")

    # generate base filename from api name
    base_filename <- str_replace_all(req$PATH_INFO, "\\/", "_") %>%
        str_replace_all("_api_", "")

    filename <- file.path(paste0(base_filename,
      "_",
      creation_date,
      ".xlsx"))

    # generate xlsx bin using helper function
    bin <- generate_xlsx_bin(return_list, base_filename)

    # Return the binary contents
    as_attachment(bin, filename)
  } else {
    return_list
  }
}

# TODO: deprecate after implementing in UI
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
  creation_date <- strftime(as.POSIXlt(Sys.time(),
      "UTC",
      "%Y-%m-%dT%H:%M:%S"),
    "%Y-%m-%dT %H:%M:%S")

  request_stats <- tibble(
    creation_date = creation_date
  ) %>%
  pivot_longer(everything(), names_to = "request", values_to = "value")

  # generate excel file output
  filename <- file.path(tempdir(), "morbidgenes_current.xlsx")
  write.xlsx(morbidgenes_current, filename,
    sheetName = "morbidgenes_current",
    append = FALSE)
  write.xlsx(request_stats, filename, sheetName = "request", append = TRUE)
  attachment_string <- paste0("attachment; filename=morbidgenes_current.",
    creation_date,
    ".xlsx")

  res$setHeader("Content-Disposition", attachment_string)

  # Read in the raw contents of the binary file
  bin <- readBin(filename, "raw", n = file.info(filename)$size)

  #Check file existence and delete
  if (file.exists(filename)) {
    file.remove(filename)
  }

  #Return the binary contents
  bin
}


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
## Authentication section

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
    orcid = user_filtered$orcid,
    iat = user_filtered$iat,
    exp = user_filtered$exp)

    jwt <- jwt_encode_hmac(claim, secret = key)
    jwt
  }
}


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
      orcid = user$orcid,
      exp = user$exp))
  }
}


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
      orcid = user$orcid,
      iat = as.numeric(Sys.time()),
      exp = as.numeric(Sys.time()) + dw$refresh)

    jwt <- jwt_encode_hmac(claim, secret = key)
    jwt
  }
}

##Authentication section
##-------------------------------------------------------------------##