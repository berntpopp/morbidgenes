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
library(tools)      ## needed for md5sum
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
dw <- config::get(Sys.getenv("API_CONFIG"))
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
## global variables
# define serializers
serializers <- list(
  "json" = serializer_json(),
  "xlsx" = serializer_content_type(type =
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
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
source("functions/file-functions.R", local = TRUE)
source("functions/hgnc-functions.R", local = TRUE)
source("functions/database-functions.R", local = TRUE)

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

#* @apiTag panel Endpoints to get information from the current MorbidGenes Panel
#* @apiTag upload Endpoints to submit new data to the database
#* @apiTag authentication Endpoints related to authentication
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

#* Filter and Select Fields from All Panels
#*
#* This endpoint allows users to filter and select specific fields
#* from all panels. It provides functionality for sorting, filtering,
#* selecting fields, and more.
#*
#* # `Details`
#* This is a Plumber endpoint function that retrieves panel
#* information based on the provided parameters. Users can sort the
#* output, apply filters, select specific fields, specify pagination
#* parameters, and more. The endpoint fetches relevant panel data.
#*
#* # `Return`
#* The function returns a cursor pagination object with links, meta
#* information, and gene objects. For format 'xlsx', data is in xlsx.
#*
#* @tag panel
#* @serializer json list(na="string")
#*
#* @param sort:str  Output column to arrange output on.
#* @param filter:str Filters to apply, separated by commas.
#* @param fields:str Output columns, separated by commas.
#* @param page_after:str Cursor after which entries are shown.
#* @param page_size:str Page size in cursor pagination.
#* @param fspec:str Fields for field specification.
#* @param format:str Output format: 'json' or 'xlsx'.
#*
#* @response 200 Cursor pagination object with links, meta, and data.
#* @response 500 Internal server error.
#*
#* @get /api/panel
function(req,
  res,
  sort = "symbol",
  filter = "equals(is_current,1)",
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
    tbl("view_panel") %>%
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
      pivot_wider(id_cols = everything(), names_from = "type", values_from = "link")

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


#* Retrieve Panel Information by HGNC ID
#*
#* This endpoint retrieves information for a gene in the current
#* panel based on the provided HGNC ID.
#*
#* # `Details`
#* This is a Plumber endpoint that fetches panel information for a
#* gene based on its HGNC ID. The HGNC ID is decoded and used to
#* query the database.
#*
#* # `Return`
#* The function returns panel information for the specified gene.
#* If not found, it may return an empty response or an error.
#*
#* @tag panel
#* @serializer json list(na="string")
#*
#* @param hgnc_id The HGNC ID of the gene for panel information.
#*
#* @get /api/panel/<hgnc_id>
function(hgnc_id) {

  hgnc <- paste0("HGNC:", URLdecode(gsub("[^0-9.-]", "", hgnc_id)))

  # get data from database and filter
  hgnc_collected <- pool %>%
    tbl("view_panel") %>%
    filter(is_current == 1) %>%
    filter(hgnc_id == hgnc) %>%
    arrange(hgnc_id) %>%
    collect()
}
## Panel endpoints
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
## Upload endpoints

#* Upload Gene List File
#*
#* This endpoint allows users to upload a gene list file to the server.
#* It takes in a panel file and a config file, performs various checks,
#* and updates the database accordingly.
#*
#* # `Details`
#* This is a Plumber endpoint function that processes the uploaded gene
#* list file. It checks if the file exists, writes the binary content
#* to a temporary file, reads the file content, computes md5sum, and
#* updates the database tables. Various error checks are performed
#* to ensure data integrity.
#*
#* # `Return`
#* The function returns appropriate status and messages based on the
#* outcome of the upload and processing.
#*
#* @tag upload
#* @serializer json list(na="string")
#*
#* @param panel_file:file Gzipped file containing panel content.
#* @param config_file:file Config file with column configuration.
#*
#* @response 200 Successful file upload and processing.
#* @response 400 File with the same version already exists or processing error.
#* @response 401 Unauthorized if user is not authenticated.
#*
#* @post /api/upload/panel
function(req, res, panel_file, config_file) {

  # define user variables
  upload_user_id <- req$user_id

  # get file name
  panel_file_name <- basename(names(panel_file))
  print(check_filename_match(panel_file, config_file))

  # generate tmp file path
  tmp_file <- file.path("data", panel_file_name)

  # check if file exists already and delete if so
  if (file.exists(tmp_file)) {
    file.remove(tmp_file)
  }

  # write binary content to file
  writeBin(panel_file[[1]], tmp_file)

  # Read file content
  table <- read_delim(tmp_file,
    delim = ";", escape_double = FALSE, trim_ws = TRUE)

  # compute md5sum of panel_file
  file_md5sum <- md5sum(tmp_file)
  print(file_md5sum)

  # get mg_panel_version data from database
  # this is used to check if the uploaded file is already in the database and newer
  # we deselect the panel_id as we need to get this later from the first insert into mg_panel_version and
  # it is managed by the database
  mg_panel_version_table <- pool %>%
    tbl("mg_panel_version") %>%
    collect() %>%
    select(-panel_id, -upload_user)

  # get mg_source data from database
  # this is used to check for novel sources (columns) and assign them a source_id
  mg_source_table <- pool %>%
    tbl("mg_source") %>%
    collect()

  # logic
  # 1) update mg_panel_version table
  # 2) update mg_source table
  # 3) update mg_panel_genes_join table
  # 4) get the panel_hgnc_id from the mg_panel_genes_join table
  # 5) generate mg_panel_genes_source_join table
  # TODO: use transactions: https://dbi.r-dbi.org/reference/transactions

  # compute panel_version, panel_date from file name and is_current from database
  file_version <- tmp_file %>%
    as_tibble() %>%
    separate(value, c("path", "file"), sep = "\\/") %>%
    mutate(file_path = paste0(path, "/", file)) %>%
    mutate(file_basename = str_remove_all(file, "\\.csv\\.gz")) %>%
    separate(file_basename,
      c("analysis", "version"),
      sep = "_") %>%
    separate(version,
      c("panel_version", "panel_date"),
      sep = "-") %>%
    mutate(panel_version = paste0(panel_version, "-", panel_date)) %>%
    mutate(panel_date = as.Date(panel_date, format = "%Y%m%d")) %>%
    mutate(md5sum_import = md5sum(file_path)) %>%
    dplyr::select(panel_version,
    panel_date,
    file_path,
    md5sum_import) %>%
    filter(str_detect(file_path, "MorbidGenesPanel"))

    # check if ile is already in mg_panel_version table
    if (file_version$panel_version %in% mg_panel_version_table$panel_version) {
      res$status <- 400
      res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = 400,
      message = paste0("File with panel version ",
        file_version$panel_version,
        " already in database.")
      ))
      return(res)
    } else {
      # check if the file is newer than the ones in the database
      # TODO: maybe add a flag to force setting a file as current
      file_version_current <- bind_rows(mg_panel_version_table, file_version) %>%
        mutate(is_current = (max(panel_date) == panel_date)) %>%
        filter(panel_version == file_version$panel_version)
    }

  # filter table and replace NA with FALSE
  # panel_id is not present as we need to get this from the first insert into mg_panel_version
  # we also get the hgnc_id from the file to check if this matches the one in the database
  # if it does not match we compute the hgnc_id from the symbol
  table_filtered <- table %>%
    filter(morbidscore != 0) %>%
    select(symbol,
      hgnc_id = id_hgnc,
      hgmd_pathogenic_cutoff,
      clinvar_pathogenic_cutoff,
      manually_added,
      panelapp,
      panelapp_UK,
      panelapp_australia,
      sysndd,
      omim_phenotype) %>%
    replace(is.na(.), FALSE)

  # compute hgnc_id from symbol
  # this function is defined in the helper-functions.R script
  # it currently uses the hgnc API to get the hgnc_id
  # TODO: maybe speed this up by using our internal hgnc table
  morbidgenes_panel_hngc <- table_filtered %>%
    mutate(hgnc_id = paste0("HGNC:", hgnc_id_from_symbol_grouped(symbol))) %>%
    select(hgnc_id,
      hgmd_pathogenic_cutoff,
      clinvar_pathogenic_cutoff,
      manually_added,
      panelapp,
      panelapp_UK,
      panelapp_australia,
      sysndd,
      omim_phenotype)

  # add upload_user_id to file_version_current
  file_version_current_user <- file_version_current %>%
    mutate(upload_user = upload_user_id)

}

## Upload endpoints
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
## Authentication section

#* User Login
#*
#* This endpoint authenticates users based on username and password.
#* It's based on the example from 'https://github.com/jandix/sealr/'.
#*
#* # `Details`
#* This is a Plumber endpoint function that checks if the provided username and
#* password are valid. If the credentials are valid, a JWT token is returned.
#* If not, appropriate error messages are returned.
#*
#* # `Return`
#* If the credentials are valid, a JWT token is returned. Otherwise, an error
#* message is returned.
#*
#* @tag authentication
#* @serializer json list(na="string")
#*
#* @param user_name The username provided for authentication.
#* @param password The password provided for authentication.
#*
#* @response 200 JWT token if the authentication is successful.
#* @response 401 Unauthorized if credentials are incorrect.
#* @response 404 Invalid username or password format.
#*
#* @get /api/auth/authenticate
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


#* User Authentication
#*
#* This endpoint authenticates users based on a provided JWT token.
#*
#* # `Details`
#* This is a Plumber endpoint function that decodes the JWT token provided in
#* the header and returns the user details if the token is valid.
#*
#* # `Return`
#* If the JWT token is valid, user details are returned. Otherwise, an error
#* message is returned.
#*
#* @tag authentication
#* @serializer json list(na="string")
#*
#* @response 200 User details if the JWT token is valid.
#* @response 401 Unauthorized if the JWT token is invalid.
#*
#* @get /api/auth/signin
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


#* Refresh Authentication
#*
#* This endpoint refreshes the user's authentication based on a provided JWT
#* token.
#*
#* # `Details`
#* This is a Plumber endpoint function that decodes the JWT token provided in
#* the header. If the token is valid, a new refreshed JWT token is returned.
#*
#* # `Return`
#* If the JWT token is valid, a new JWT token is returned. Otherwise, an error
#* message is returned.
#*
#* @tag authentication
#* @serializer json list(na="string")
#*
#* @response 200 New JWT token if the original token is valid.
#* @response 401 Unauthorized if the JWT token is invalid.
#*
#* @get /api/auth/refresh
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