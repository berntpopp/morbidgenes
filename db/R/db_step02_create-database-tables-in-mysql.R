# load libraries
library(tidyverse)	##needed for general table operations
library(sqlr)		##needed for MySQL data export
library(tools)		##needed for md5sum calculation

source('utils.R', chdir = TRUE)
config <- get_config()

# set working directory (needs to be adapted to your specific working directory)
setwd(config$working_directory)
# set global options
options(scipen = config$scipen)

results_csv_table <- 	list.files(path = config$results_directory, pattern = ".csv$") %>%
						dplyr::as_tibble() %>%
						tidyr::separate(value, c("table_name", "table_date", "extension"), sep = "\\.") %>%
						dplyr::mutate(file_name = paste0(table_name, ".", table_date, ".", extension)) %>%
						dplyr::mutate(import_date = get_current_date()) %>%
						dplyr::mutate(results_file_id = row_number()) %>%
						dplyr::mutate(md5sum_file = tools::md5sum(paste0(config$results_directory, file_name))) %>%
						dplyr::select	(
											results_file_id, 
											file_name, 
											table_name, 
											table_date, 
											extension, 
											import_date, 
											md5sum_file
										) %>%
						dplyr::mutate(table_date = as.Date(table_date)) %>% 
						dplyr::group_by(table_name) %>% 
						dplyr::filter(table_date == max(table_date)) %>% 
						dplyr::ungroup()

results_csv_table_as_data_frame <- as.data.frame(results_csv_table)

## drop all tables if they exist
sqlr::drop_db_tbl("results_csv_table", force = TRUE)
sqlr::drop_db_tbl(results_csv_table_as_data_frame$table_name, force = TRUE)

##
get_keys <- sqlr::pk_spec(names(results_csv_table_as_data_frame)[1])
sqlr::write_db_tbl	(
					name = "results_csv_table", 
					data = results_csv_table_as_data_frame, 
					keys = get_keys, 
					char_set = "utf8"
				)

for (row in 1:nrow(results_csv_table_as_data_frame)) {
	file_name <- dplyr::slice(results_csv_table_as_data_frame, row)$file_name
	
	table_to_import <- as.data.frame(
										read_delim	(
														paste0(config$results_directory, file_name),
														",",
														col_names = TRUE
													)
									)
	
	get_table_name <- dplyr::slice(results_csv_table_as_data_frame, row)$table_name
	get_keys <- sqlr::pk_spec(names(table_to_import)[1])
	sqlr::write_db_tbl	(	
							name = get_table_name, 
							data = table_to_import, 
							keys = get_keys, 
							char_set = "utf8"
						)
}

## close database connection
sqlr::rm_con()
