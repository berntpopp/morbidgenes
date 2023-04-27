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
library(keyring)
library(rlang)
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
dw <- config::get(Sys.getenv("API_CONFIG"))
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
## global variables

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

##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
## Panel endpoints

#* @tag panel
## get current panel
#* @serializer json list(na="string")
#' @get /api/panel/
function(res,
  sort = "symbol",
  filter = "",
  fields = "",
  `page_after` = "",
  `page_size` = "all",
  fspec = "panel_version,hgnc_id,symbol,bed_hg19,bed_hg38,PanelApp,AustraliaPanelApp,HGMD_pathogenic,Phenotype_MIM,ClinVarPathogenic,UKPanelApp,SysNDD,Manually,mg_score") {

  start_time <- Sys.time()

  # generate sort expression based on sort input
  sort_exprs <- generate_sort_expressions(sort, unique_id = "hgnc_id")

  # generate filter expression based on filter input
  filter_exprs <- generate_filter_expressions(filter)

  # get data from database
  mb_panel_current_view <- pool %>%
    tbl("view_panel_current") %>%
    collect()

  mb_panel_current_table <- mb_panel_current_view %>%
    arrange(!!!rlang::parse_exprs(sort_exprs)) %>%
    filter(!!!rlang::parse_exprs(filter_exprs))

  # select fields from table based on input
  # using the helper function "select_tibble_fields"
  mb_panel_current_table <- select_tibble_fields(mb_panel_current_table,
    fields,
    "hgnc_id")

  # use the helper generate_cursor_pag_inf to
  # generate cursor pagination information from a tibble
  mb_panel_pag_info <- generate_cursor_pag_inf(
    mb_panel_current_table,
    `page_size`,
    `page_after`,
    "hgnc_id")

  # use the helper generate_tibble_fspec to
  # generate fields specs from a tibble
  # first for the unfiltered and unsubset table
  mb_panel_current_fspec <- generate_tibble_fspec_mem(mb_panel_current_view,
    fspec)
  # then for the filtered/ subset one
  mb_panel_current_table_fspec <- generate_tibble_fspec_mem(
    mb_panel_current_table,
    fspec)
  # assign the second to the first as filtered
  mb_panel_current_fspec$fspec$count_filtered <-
    mb_panel_current_table_fspec$fspec$count

  # compute execution time
  end_time <- Sys.time()
  execution_time <- as.character(paste0(round(end_time - start_time, 2),
    " secs"))

  # add columns to the meta information from
  # generate_cursor_pag_inf function return
  meta <- mb_panel_pag_info$meta %>%
    add_column(tibble::as_tibble(list("sort" = sort,
    "filter" = filter,
    "fields" = fields,
    "fspec" = mb_panel_current_fspec,
    "executionTime" = execution_time)))

  # add host, port and other information to links from
  # the link information from generate_cursor_pag_inf function return
  links <- mb_panel_pag_info$links %>%
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
  list(links = links,
    meta = meta,
    data = mb_panel_pag_info$data)
}


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