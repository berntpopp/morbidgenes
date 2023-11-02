# morbidgenes-db_plumber.R
## TODO: adapt "serializer json list(na="null")"

##-------------------------------------------------------------------##
# load libraries
library(plumber)    ## needed for plumber
library(tidyverse)  ## needed for tibble
library(cowplot)    ## needed for plot
library(DBI)        ## needed for database
library(RMariaDB)   ## needed for database
library(jsonlite)   ## needed for json
library(config)     ## needed for config
library(jose)       ## needed for jwt
library(RCurl)      ## needed for curl
library(stringdist) ## needed for stringdist
library(xlsx)       ## needed for xlsx output
library(easyPubMed) ## needed for get_pubmed_data
library(rvest)      ## needed for html_table
library(lubridate)  ## needed for as_datetime
library(pool)       ## needed for dbPool
library(memoise)    ## needed for memoise
library(coop)       ## needed for as_attachment
library(keyring)    ## needed for keyring
library(rlang)      ## needed for parse_exprs
library(tools)      ## needed for md5sum
library(yaml)       ## needed for yaml loading
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
source("functions/validation-functions.R", local = TRUE)

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

  # Extract the file basenames
  panel_file_name <- basename(names(panel_file))
  config_file_name <- basename(names(config_file))

  # Check if the basenames match using the helper function "check_filename_match"
  # validation step in the endpoint that returns 400 if not
  if (!check_filename_match(panel_file_name, config_file_name)) {
    res$status <- 400
    res$body <- "The file names do not match."
    res
  }

  # write binary content of the gene list and config file to directory
  # and get the directory of the written files
  # using the helper function "write_binary_to_file"
  panel_file_dir <- write_binary_to_file(panel_file, panel_file_name)
  config_file_dir <- write_binary_to_file(config_file, config_file_name)

  # read the content of both files using the helper function
  # "read_upload_files"
  tables_upload_files <- read_upload_files(panel_file_dir, config_file_dir)

  # correct the hgnc_id and symbol in the csv tibble
  # using the helper function "correct_hgnc_id_and_symbol"
  # from helper-functions.R
  tables_upload_files$csv_tibble <- correct_hgnc_id_and_symbol(tables_upload_files$csv_tibble, pool)

  # compute panel_version, panel_date from file name and is_current from database
  # add upload_user_id to file_version_current
  # check if the uploaded panel file is already in the database and newer
  # we the panel_id is deselect as we need to get this later from the first
  # insert into mg_panel_version and it is managed by the database
  # use the validation function "compute_panel_metadata"
  # TODO: the optional parameter override_is_current is not yet implemented in the endpoint
  file_version_tibble <- compute_panel_metadata(panel_file_dir,
    upload_user_id,
    pool,
    override_is_current = FALSE)

  # compute the mg_source table and actions
  # use the validation function "check_config_update_source"
  # TODO: the optional parameter overwrite_config is not yet implemented in the endpoint
  updated_config <- check_config_update_source(tables_upload_files$yaml_tibble,
    pool,
    overwrite_config = TRUE)

  ## define logic for database update
  # 1) update mg_panel_version table
  # 2) update mg_source table
  # 3) update mg_panel_genes_join table
  # 4) get the panel_hgnc_id from the mg_panel_genes_join table
  # 5) generate mg_panel_genes_source_join table
  # TODO: implement usage of transactions: https://dbi.r-dbi.org/reference/transactions

  # 1)
  # update mg_panel_version table:
  # here we update the mg_panel_version table using
  # the file_version_tibble tibble computed before
  # and the function update_mg_panel_version from database-functions.R
  # the return value of this function after database update
  # includes the updated mg_source table with all current source_id
  # TODO: add if else statement to check if update was successful and return error if not

  updated_file_version_response <- update_mg_panel_version(file_version_tibble, pool)

  # 2)
  # update mg_source table:
  # here we update the mg_source table using
  # the updated_config tibble computed before
  # and the function update_mg_source from database-functions.R
  # the return value of this function after database update
  # includes the updated mg_source table with all current source_id
  # TODO: add if else statement to check if update was successful and return error if not

  updated_config_response <- update_mg_source(updated_config, pool)

  # 3)
  # update the mg_panel_genes_join table
  # using the tables_upload_files$csv_tibble and the
  # tables_upload_files$yaml_tibble with the helper function
  # "convert_panel_to_long_format" we first compute a long version
  # we then use the function update_mg_panel_genes_join from database-functions.R
  # in which we filter unique rows from the long tibble
  # from which we select the columns hgnc_id
  # and add the panel_id from step 1)
  # TODO: return entries written to database in response

  csv_tibble_long <- convert_panel_to_long_format(tables_upload_files$csv_tibble,
    tables_upload_files$yaml_tibble)

  # filter the panel_id of the panel to be uploaded using panel_file_dir
  update_panel_id <- updated_file_version_response$updated_table %>%
    filter(file_path == panel_file_dir) %>%
    select(panel_id)

  # update the mg_panel_genes_join table
  # TODO: update update_mg_panel_genes_join to first delete old data if panel_id already in table
  mg_panel_genes_join_update_response <- update_mg_panel_genes_join(update_panel_id$panel_id, csv_tibble_long, pool)

  # 4) & 5)
  # get the panel_hgnc_id from the mg_panel_genes_join table & generate mg_panel_genes_source_join table:
  # here we get the posted_data with the panel_hgnc_id from step 3)
  # using the tables_upload_files$csv_tibble
  # we first compute a long version of the tibble
  # where we replace the true/false values in the wide format
  # with the panel_hgnc_id from step 3)
  # and replace the previous column names in the wide format
  # with the source_id from step 2)
  # we then update the mg_panel_genes_source_join table
  # using the long tibble from step 4)
  # the return value after database update
  # should be the final endpoint response
  # e.g. 200 if everything went well
  # or respective error messages if not
  # TODO: for panel updates delete old data from mg_panel_genes_source_join using the panel_hgnc_source_id filtered using the panel_hgnc_id from step 3)

  mg_panel_genes_source_join_response <- update_mg_panel_genes_source_join(mg_panel_genes_join_update_response$posted_data,  tables_upload_files$csv_tibble, updated_config_response$updated_table, pool)

  # TODO: check submission logic to keep old IDs in mg_panel_genes_source_join and mg_panel_genes_join if in new panel

  return(list(status_code = mg_panel_genes_source_join_response$status_code,
    message = mg_panel_genes_source_join_response$message))
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