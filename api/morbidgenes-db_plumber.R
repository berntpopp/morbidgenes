# morbidgenes-db_plumber.R
## to do: adapt "serializer json list(na="null")"

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
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
dw <- config::get("morbidgenes_db")
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

#* @apiTag panel Endpoint to get the current MorbidGenes Panel
##-------------------------------------------------------------------##
##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
## filters

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

##-------------------------------------------------------------------##



##-------------------------------------------------------------------##
## Panel endpoints

#* @tag panel
## get current panel
#* @serializer json list(na="string")
#' @get /api/panel/
function(res, sort = "symbol", `page[after]` = 0, `page[size]` = "all") {

	# get number of rows in view_panel_current
	morbidgenes_db_panel_current_rows <- (pool %>% 
		tbl("view_panel_current") %>%
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
	morbidgenes_db_panel_current_table <- pool %>% 
		tbl("view_panel_current") %>%
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