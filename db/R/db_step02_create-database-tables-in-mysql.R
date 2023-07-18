# load libraries
library(tidyverse)	##needed for general table operations
library(DBI)		##needed for MySQL data export
library(RMariaDB)	##needed for MySQL data export
library(sqlr)		##needed for MySQL data export
library(tools)		##needed for md5sum calculation

# set working directory (needs to be adapted to your specific working directory)
setwd("./")
# set global options
options(scipen = 999)

# connect to the database
morbidgenes_db <- dbConnect(
								RMariaDB::MariaDB(), 
								dbname = "morbidgenes_db", 
								user = "root", 
								password = "morbidgenes-db", 
								host = "127.0.0.1",
								port = "9918"
							)

  
import_date <- strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")

results_csv_table <- 	list.files(path = "results/", pattern = ".csv$") %>%
						as_tibble() %>%
						separate(value, c("table_name", "table_date", "extension"), sep = "\\.") %>%
						mutate(file_name = paste0(table_name, ".", table_date, ".", extension)) %>%
						mutate(import_date = import_date) %>%
						mutate(results_file_id = row_number()) %>%
						mutate(md5sum_file = md5sum(paste0("results/", file_name))) %>%
						dplyr::select	(
											results_file_id, 
											file_name, 
											table_name, 
											table_date, 
											extension, 
											import_date, 
											md5sum_file
										) %>%
						mutate(table_date = as.Date(table_date)) %>% 
						group_by(table_name) %>% 
						filter(table_date == max(table_date)) %>% 
						ungroup()

results_csv_table_as_data_frame <- as.data.frame(results_csv_table)

## drop all tables if they exist
drop_db_tbl("results_csv_table", force = TRUE)
drop_db_tbl(results_csv_table_as_data_frame$table_name, force = TRUE)

##
get_keys <- pk_spec(names(results_csv_table_as_data_frame)[1])
write_db_tbl	(
					name = "results_csv_table", 
					data = results_csv_table_as_data_frame, 
					keys = get_keys, 
					char_set = "utf8"
				)

for (row in 1:nrow(results_csv_table_as_data_frame)) {
	file_name <- slice(results_csv_table_as_data_frame, row)$file_name
	
	table_to_import <- as.data.frame(
										read_delim	(
														paste0("results/", file_name),
														",",
														col_names = TRUE
													)
									)
	
	get_table_name <- slice(results_csv_table_as_data_frame, row)$table_name
	get_keys <- pk_spec(names(table_to_import)[1])
	write_db_tbl(	
					name = get_table_name, 
					data = table_to_import, 
					keys = get_keys, 
					char_set = "utf8"
				)
}

## close database connection
rm_con()
