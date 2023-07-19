# load libraries
library(tidyverse)		##needed for general table operations
library(jsonlite)		##needed for HGNC requests

source('utils.R', chdir = TRUE)
config <- get_config()

# set working directory (needs to be adapted to your specific working directory)
setwd(config$working_directory)
# set global options
options(scipen = config$scipen)

############################################
# define functions

hgnc_id_from_prevsymbol <- function(symbol_input)  {
	symbol_request <- jsonlite::fromJSON(paste0("http://rest.genenames.org/search/prev_symbol/", symbol_input))

	hgnc_id_from_symbol <- dplyr::as_tibble(symbol_request$response$docs)
	
	hgnc_id_from_symbol <-	hgnc_id_from_symbol %>%
							dplyr::mutate(
								hgnc_id = 
									if (exists('hgnc_id', where = hgnc_id_from_symbol)) 
										hgnc_id 
									else 
										NA
							) %>%
							dplyr::mutate(
								symbol = 
									if (exists('symbol', where = hgnc_id_from_symbol)) 
										symbol 
									else 
										""
							) %>%
							dplyr::mutate(
								score = 
									if (exists('score', where = hgnc_id_from_symbol)) 
										score 
									else 
										0
							) %>%
							dplyr::arrange(desc(score)) %>%
							dplyr::mutate(
								hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2])
							)

	result <- as.integer(hgnc_id_from_symbol$hgnc_id[1])

	return(result)
}

hgnc_id_from_aliassymbol <- function(symbol_input)  {
	symbol_request <- jsonlite::fromJSON(paste0("http://rest.genenames.org/search/alias_symbol/", symbol_input))

	hgnc_id_from_symbol <- dplyr::as_tibble(symbol_request$response$docs)
	
	hgnc_id_from_symbol <- 	hgnc_id_from_symbol %>%
							dplyr::mutate(
								hgnc_id = 
									if (exists('hgnc_id', where = hgnc_id_from_symbol)) 
										hgnc_id 
									else 
										NA
							) %>%
							dplyr::mutate(
								symbol = 
									if (exists('symbol', where = hgnc_id_from_symbol)) 
										symbol 
									else 
										""
							) %>%
							dplyr::mutate(
								score = 
									if (exists('score', where = hgnc_id_from_symbol)) 
										score 
									else 
										0
							) %>%
							dplyr::arrange(desc(score)) %>%
							dplyr::mutate(
								hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2])
							)

	result <- as.integer(hgnc_id_from_symbol$hgnc_id[1])

	return(result)
}

hgnc_id_from_symbol <- function(symbol_tibble) {
	symbol_list_tibble <- 	dplyr::as_tibble(symbol_tibble) %>% 
							dplyr::select(symbol = value) %>% 
							dplyr::mutate(symbol = toupper(symbol))
	
	symbol_request <- jsonlite::fromJSON	(
									paste0	(
												"http://rest.genenames.org/search/symbol/", 
												str_c(symbol_list_tibble$symbol, collapse = "+OR+")
											)
								)

	hgnc_id_from_symbol <- dplyr::as_tibble(symbol_request$response$docs)
	
	hgnc_id_from_symbol <- 	hgnc_id_from_symbol %>%
							dplyr::mutate(
								hgnc_id = 
									if (exists('hgnc_id', where = hgnc_id_from_symbol)) 
										hgnc_id 
									else 
										NA
							) %>%
							dplyr::mutate(
								symbol = 
									if (exists('symbol', where = hgnc_id_from_symbol)) 
										toupper(symbol) 
									else ""
							) %>%
							dplyr::mutate(
								hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2])
							)
		
	return_tibble <- 	symbol_list_tibble %>% 
						dplyr::left_join(hgnc_id_from_symbol, by = "symbol") %>%
						dplyr::select(hgnc_id)

	return(return_tibble)
}


symbol_from_hgnc_id <- function(hgnc_id_tibble) {
	hgnc_id_list_tibble <- 	dplyr::as_tibble(hgnc_id_tibble) %>% 
							dplyr::select(hgnc_id = value) %>%
							dplyr::mutate(hgnc_id = as.integer(hgnc_id))
	
	hgnc_id_request <- jsonlite::fromJSON	(
									paste0	(
												"http://rest.genenames.org/search/hgnc_id/", 
												str_c(hgnc_id_list_tibble$hgnc_id, collapse = "+OR+")
											)
								)

	hgnc_id_from_hgnc_id <- dplyr::as_tibble(hgnc_id_request$response$docs)
	
	hgnc_id_from_hgnc_id <- 	hgnc_id_from_hgnc_id %>%
								dplyr::mutate(
									hgnc_id = 
										if (exists('hgnc_id', where = hgnc_id_from_hgnc_id)) 
											hgnc_id 
										else 
											NA
								) %>%
								dplyr::mutate(
									hgnc_id = 
										if (exists('hgnc_id', where = hgnc_id_from_hgnc_id)) 
											toupper(hgnc_id) 
										else 
											""
								) %>%
								dplyr::mutate(
									hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2])
								)
		
	return_tibble <- 	hgnc_id_list_tibble %>% 
						dplyr::left_join(hgnc_id_from_hgnc_id, by = "hgnc_id") %>%
						dplyr::select(symbol)

	return(return_tibble)
}


hgnc_id_from_symbol_grouped <- function(input_tibble, request_max = 150) {
	input_tibble <- dplyr::as_tibble(input_tibble)
	
	row_number <- nrow(input_tibble)
	groups_number <- ceiling(row_number/request_max)
	
	input_tibble_request <- 	input_tibble %>%
								dplyr::mutate(group = sample(1:groups_number, row_number, replace=T)) %>%
								dplyr::group_by(group) %>%
								dplyr::mutate(response = hgnc_id_from_symbol(value)$hgnc_id) %>%
								dplyr::ungroup()
	
	input_tibble_request_repair <- 	input_tibble_request %>%
									filter(is.na(response)) %>%
									dplyr::select(value) %>%
									unique() %>%
									dplyr::rowwise() %>%
									dplyr::mutate(
										response = hgnc_id_from_prevsymbol(value)
									) %>%
									dplyr::mutate(
										response = 
											dplyr::case_when(
												!is.na(response) ~ response, 
												is.na(response) ~ hgnc_id_from_aliassymbol(value)
											)
									)
	
	input_tibble_request <- 	input_tibble_request %>%
								dplyr::left_join(input_tibble_request_repair, by = "value") %>%
								dplyr::mutate(
									response = 
										dplyr::case_when(
											!is.na(response.x) ~ response.x, 
											is.na(response.x) ~ response.y
										)
								)
	
	result <- input_tibble_request$response

	return(result)
}

############################################



############################################
# load csv file(s) and reformat
MorbidGenes_Panel <- 	read_delim	(
										paste0(config$data_directory, "MorbidGenes-Panel-v2023-01.1.csv"), 
										delim = ";", 
										escape_double = FALSE, 
										trim_ws = TRUE
						) %>%
						filter(morbidscore != 0) %>%
						dplyr::select	(
											symbol, 
											hgmd_pathogenic_cutoff, 
											clinvar_pathogenic_cutoff, 
											manually_added, 
											panelapp, 
											panelapp_UK, 
											panelapp_australia, 
											sysndd, 
											omim_phenotype, 
											gencc
						) %>% 
						dplyr::mutate(version = "v2023_01_1") %>% 
						replace(is.na(.), FALSE)
	
# compute HGNC ID
MorbidGenes_Panel_hngc <- 	MorbidGenes_Panel %>%
							dplyr::mutate(hgnc_id = paste0("HGNC:", hgnc_id_from_symbol_grouped(symbol))) %>%
							dplyr::select	(
												hgnc_id, 
												hgmd_pathogenic_cutoff, 
												clinvar_pathogenic_cutoff, 
												manually_added, 
												panelapp, 
												panelapp_UK, 
												panelapp_australia, 
												sysndd, 
												omim_phenotype, 
												gencc
											)
############################################

# Create User table
user <- dplyr::tibble	(
		user_id = numeric(),
		user_name = character(),
		password = character(),
		email = character(),
		user_role = character(),
		approved = integer()
	) %>%
  	dplyr::add_row(
		user_id = 1, 
		user_name = "Robin", 
		password = "password1salt1", 
		email="Robin-Tobias.Jauss@medizin.uni-leipzig.de", 
		user_role="Administrator", 
		approved = 1
	) %>%
  	dplyr::add_row(
		user_id = 2, 
		user_name = "Bernt", 
		password = "password2salt2", 
		email="bernt.popp@charite.de", 
		user_role="Administrator", 
		approved = 1
	) %>%
  	dplyr::arrange(user_id) %>%
  	dplyr::mutate(created_at = get_current_date())



############################################
## USE THIS SECTION IF THIS IS THE FIRST PANEL VERSION OR FIRST ID
## create mb_panel_version table
mb_panel_version <- dplyr::tibble(
	  panel_id = numeric(),
	  panel_version = character(),
	  panel_date = character(),
	  is_current = logical(), 
	  upload_user = integer()
	) %>%
  	dplyr::add_row(
			panel_id = 1, 
			panel_version = "v2023_01_1", 
			panel_date = get_current_date(), 
			is_current=TRUE, 
			upload_user = 1
	)

############################################
## USE THIS IF THIS IS NOT THE FIRST VERSION
#mb_panel_version = read_csv((list.files("results/", pattern = "mb_panel_version", full.names = T) %>% sort() %>% tail(n = 1))) %>% 
#  mutate(is_current = F)
#new_panel_id = (mb_panel_version %>% slice(n()))$panel_id + 1
#mb_panel_version = mb_panel_version %>%
#  add_row(panel_id = new_panel_id, panel_version = "v2023_02_1", panel_date = as.Date(strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")), is_current=TRUE)


############################################
## USE THIS SECTION IF THIS IS THE FIRST PANEL VERSION OR FIRST ID
## create mb_panel_genes_join table
mb_panel_genes_join <- 	MorbidGenes_Panel_hngc %>%
						dplyr::select(hgnc_id) %>%
						dplyr::mutate(panel_id = 1) %>% 
						dplyr::mutate(panel_hgnc_id = row_number()) %>%
						dplyr::select(panel_hgnc_id, panel_id, hgnc_id)

############################################
## USE THIS IF THIS IS NOT THE FIRST VERSION
#mb_panel_genes_join_old = read_csv((list.files("results/", pattern = "mb_panel_genes_join", full.names = T) %>% sort() %>% tail(n = 1)))
#latestrow = max(mb_panel_genes_join_old$panel_hgnc_id)
#mb_panel_genes_join_new <- MorbidGenes_Panel_hngc %>%
#  select(hgnc_id) %>%
#  mutate(panel_id = new_panel_id) %>% 
#  mutate(panel_hgnc_id = row_number() + latestrow) %>%
#  select(panel_hgnc_id, panel_id, hgnc_id)
#mb_panel_genes_join = rbind(mb_panel_genes_join_old, mb_panel_genes_join_new)

############################################
## USE THIS SECTION IF THIS IS THE FIRST PANEL VERSION OR FIRST ID
## create mb_panel_genes_source_join table
mb_panel_genes_source <- 	MorbidGenes_Panel_hngc %>%
							dplyr::mutate(panel_id = 1) %>% 
							dplyr::mutate(panel_hgnc_id = row_number()) %>% 
							dplyr::select(
								panel_hgnc_id, 
								hgmd_pathogenic_cutoff, 
								clinvar_pathogenic_cutoff, 
								manually_added, 
								panelapp, 
								panelapp_UK, 
								panelapp_australia, 
								sysndd, 
								omim_phenotype, 
								gencc
							) %>% 
							tidyr::pivot_longer(
								!panel_hgnc_id, 
								names_to = "source_name", 
								values_to = "in_source"
							) %>%
							filter(in_source) #%>% 
	#mutate(source_name = str_replace(source_name, "Gene", "")) %>% 
	#mutate(source_name = str_replace(source_name, "is", "")) %>%
	#mutate(source_name = str_replace(source_name, "has_", "")) %>%
	#mutate(source_name = str_replace(source_name, "_variant_count_cutoff", "")) %>%
	#mutate(source_name = str_replace(source_name, "_Number", "")) %>%
	#mutate(source_name = str_replace(source_name, "Count_cutoff", "")) %>%
	#mutate(source_name = str_replace(source_name, "added", ""))

############################################
## USE THIS IF THIS IS NOT THE FIRST VERSION
## create mb_panel_genes_source_join table
#mb_panel_genes_source <- MorbidGenes_Panel_hngc %>%
#  mutate(panel_id = new_panel_id) %>% 
#  mutate(panel_hgnc_id = row_number() + latestrow) %>% 
#  select(panel_hgnc_id, hgmd_pathogenic_cutoff, clinvar_pathogenic_cutoff, manually_added, panelapp, panelapp_UK, panelapp_australia, sysndd, omim_phenotype, gencc) %>% 
#  pivot_longer(!panel_hgnc_id, names_to = "source_name", values_to = "in_source") %>%
#  filter(in_source) #%>% 
  #mutate(source_name = str_replace(source_name, "Gene", "")) %>% 
  #mutate(source_name = str_replace(source_name, "is", "")) %>%
  #mutate(source_name = str_replace(source_name, "has_", "")) %>%
  #mutate(source_name = str_replace(source_name, "_variant_count_cutoff", "")) %>%
  #mutate(source_name = str_replace(source_name, "_Number", "")) %>%
  #mutate(source_name = str_replace(source_name, "Count_cutoff", "")) %>%
  #mutate(source_name = str_replace(source_name, "added", ""))

mb_source <- 	mb_panel_genes_source %>%
				dplyr::select(source_name) %>%
				unique() %>% 
				dplyr::mutate(source_id = row_number()) %>% 
				dplyr::mutate(source_logic = "dummy") %>% 
				dplyr::select(source_id, source_name, source_logic)


############################################
## USE THIS SECTION IF THIS IS THE FIRST PANEL VERSION OR FIRST ID
## create mb_panel_genes_source_join table
mb_panel_genes_source_join <- 	mb_panel_genes_source %>%
								dplyr::left_join(mb_source, by = "source_name") %>%
								dplyr::select(-source_name, -source_logic) %>% 
								dplyr::mutate(panel_hgnc_source_id = row_number()) %>%
								dplyr::select(panel_hgnc_source_id, panel_hgnc_id, source_id)

############################################
## USE THIS IF THIS IS NOT THE FIRST VERSION
## create mb_panel_genes_source_join table
#mb_panel_genes_source_join_old = read_csv((list.files("results/", pattern = "mb_panel_genes_source_join", full.names = T) %>% sort() %>% tail(n = 1)))
#latestrow2 = max(mb_panel_genes_source_join_old$panel_hgnc_source_id)
#mb_panel_genes_source_join_new <- mb_panel_genes_source %>%
#  left_join(mb_source, by = "source_name") %>%
#  select(-source_name, -source_logic) %>% 
#  mutate(panel_hgnc_source_id = row_number() + latestrow2) %>%
#  select(panel_hgnc_source_id, panel_hgnc_id, source_id)
#mb_panel_genes_source_join = rbind(mb_panel_genes_source_join_old, mb_panel_genes_source_join_new)


############################################
## export table as csv with date of creation
user_file_name <- paste0(config$results_directory, "user.", get_current_date(), ".csv")
write_csv(
	user, 
	file = user_file_name,
	na = ""
)

mb_panel_version_file_name <- paste0(config$results_directory, "mb_panel_version.",get_current_date(), ".csv")
write_csv(
	mb_panel_version, 
	file = mb_panel_version_file_name
)
#gzip(mb_panel_version_file_name)

mb_panel_genes_join_file_name <- paste0(config$results_directory, "mb_panel_genes_join.", get_current_date(), ".csv")
write_csv(
	mb_panel_genes_join, 
	file = mb_panel_genes_join_file_name
)
#gzip(mb_panel_genes_join_file_name)

mb_source_file_name <- paste0(config$results_directory, "mb_source.",get_current_date(),".csv")
write_csv(
	mb_source, 
	file = mb_source_file_name
)
#gzip(mb_source_file_name)

mb_panel_genes_source_join_file_name <- paste0(config$results_directory, "mb_panel_genes_source_join.",get_current_date(),".csv")
write_csv(
	mb_panel_genes_source_join, 
	file = mb_panel_genes_source_join_file_name
)
#gzip(mb_panel_genes_source_join_file_name)
