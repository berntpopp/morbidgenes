# load libraries
library(tidyverse)		##needed for general table operations
library(jsonlite)		##needed for HGNC requests

# set working directory (needs to be adapted to your specific working directory)
setwd("./")
# set global options
options(scipen = 999)

############################################
# define functions

hgnc_id_from_prevsymbol <- function(symbol_input)  {
	symbol_request <- jsonlite::fromJSON(paste0("http://rest.genenames.org/search/prev_symbol/", symbol_input))

	hgnc_id_from_symbol <- dplry::as_tibble(symbol_request$response$docs)
	
	hgnc_id_from_symbol <-	hgnc_id_from_symbol %>%
							dplry::mutate(
								hgnc_id = 
									if (exists('hgnc_id', where = hgnc_id_from_symbol)) 
										hgnc_id 
									else 
										NA
							) %>%
							dplry::mutate(
								symbol = 
									if (exists('symbol', where = hgnc_id_from_symbol)) 
										symbol 
									else 
										""
							) %>%
							dplry::mutate(
								score = 
									if (exists('score', where = hgnc_id_from_symbol)) 
										score 
									else 
										0
							) %>%
							dplry::arrange(desc(score)) %>%
							dplry::mutate(
								hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2])
							)

	result <- as.integer(hgnc_id_from_symbol$hgnc_id[1])

	return(result)
}

hgnc_id_from_aliassymbol <- function(symbol_input)  {
	symbol_request <- jsonlite::fromJSON(paste0("http://rest.genenames.org/search/alias_symbol/", symbol_input))

	hgnc_id_from_symbol <- dplry::as_tibble(symbol_request$response$docs)
	
	hgnc_id_from_symbol <- 	hgnc_id_from_symbol %>%
							dplry::mutate(
								hgnc_id = 
									if (exists('hgnc_id', where = hgnc_id_from_symbol)) 
										hgnc_id 
									else 
										NA
							) %>%
							dplry::mutate(
								symbol = 
									if (exists('symbol', where = hgnc_id_from_symbol)) 
										symbol 
									else 
										""
							) %>%
							dplry::mutate(
								score = 
									if (exists('score', where = hgnc_id_from_symbol)) 
										score 
									else 
										0
							) %>%
							dplry::arrange(desc(score)) %>%
							dplry::mutate(
								hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2])
							)

	result <- as.integer(hgnc_id_from_symbol$hgnc_id[1])

	return(result)
}

hgnc_id_from_symbol <- function(symbol_tibble) {
	symbol_list_tibble <- 	dplry::as_tibble(symbol_tibble) %>% 
							dplry::select(symbol = value) %>% 
							dplry::mutate(symbol = toupper(symbol))
	
	symbol_request <- jsonlite::fromJSON	(
									paste0	(
												"http://rest.genenames.org/search/symbol/", 
												str_c(symbol_list_tibble$symbol, collapse = "+OR+")
											)
								)

	hgnc_id_from_symbol <- dplry::as_tibble(symbol_request$response$docs)
	
	hgnc_id_from_symbol <- 	hgnc_id_from_symbol %>%
							dplry::mutate(
								hgnc_id = 
									if (exists('hgnc_id', where = hgnc_id_from_symbol)) 
										hgnc_id 
									else 
										NA
							) %>%
							dplry::mutate(
								symbol = 
									if (exists('symbol', where = hgnc_id_from_symbol)) 
										toupper(symbol) 
									else ""
							) %>%
							dplry::mutate(
								hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2])
							)
		
	return_tibble <- 	symbol_list_tibble %>% 
						dplry::left_join(hgnc_id_from_symbol, by = "symbol") %>%
						dplry::select(hgnc_id)

	return(return_tibble)
}


symbol_from_hgnc_id <- function(hgnc_id_tibble) {
	hgnc_id_list_tibble <- 	dplry::as_tibble(hgnc_id_tibble) %>% 
							dplry::select(hgnc_id = value) %>%
							dplry::mutate(hgnc_id = as.integer(hgnc_id))
	
	hgnc_id_request <- jsonlite::fromJSON	(
									paste0	(
												"http://rest.genenames.org/search/hgnc_id/", 
												str_c(hgnc_id_list_tibble$hgnc_id, collapse = "+OR+")
											)
								)

	hgnc_id_from_hgnc_id <- dplry::as_tibble(hgnc_id_request$response$docs)
	
	hgnc_id_from_hgnc_id <- 	hgnc_id_from_hgnc_id %>%
								dplry::mutate(
									hgnc_id = 
										if (exists('hgnc_id', where = hgnc_id_from_hgnc_id)) 
											hgnc_id 
										else 
											NA
								) %>%
								dplry::mutate(
									hgnc_id = 
										if (exists('hgnc_id', where = hgnc_id_from_hgnc_id)) 
											toupper(hgnc_id) 
										else 
											""
								) %>%
								dplry::mutate(
									hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2])
								)
		
	return_tibble <- 	hgnc_id_list_tibble %>% 
						dplry::left_join(hgnc_id_from_hgnc_id, by = "hgnc_id") %>%
						dplry::select(symbol)

	return(return_tibble)
}


hgnc_id_from_symbol_grouped <- function(input_tibble, request_max = 150) {
	input_tibble <- dplry::as_tibble(input_tibble)
	
	row_number <- nrow(input_tibble)
	groups_number <- ceiling(row_number/request_max)
	
	input_tibble_request <- 	input_tibble %>%
								dplry::mutate(group = sample(1:groups_number, row_number, replace=T)) %>%
								dplry::group_by(group) %>%
								dplry::mutate(response = hgnc_id_from_symbol(value)$hgnc_id) %>%
								dplry::ungroup()
	
	input_tibble_request_repair <- 	input_tibble_request %>%
									filter(is.na(response)) %>%
									dplry::select(value) %>%
									unique() %>%
									dplry::rowwise() %>%
									dplry::mutate(
										response = hgnc_id_from_prevsymbol(value)
									) %>%
									dplry::mutate(
										response = 
											dplry::case_when(
												!is.na(response) ~ response, 
												is.na(response) ~ hgnc_id_from_aliassymbol(value)
											)
									)
	
	input_tibble_request <- 	input_tibble_request %>%
								dplry::left_join(input_tibble_request_repair, by = "value") %>%
								dplry::mutate(
									response = 
										dplry::case_when(
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
										"data/MorbidGenes-Panel-v2023-01.1.csv", 
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
						dplry::mutate(version = "v2023_01_1") %>% 
						replace(is.na(.), FALSE)
	
# compute HGNC ID
MorbidGenes_Panel_hngc <- 	MorbidGenes_Panel %>%
							dplry::mutate(hgnc_id = paste0("HGNC:", hgnc_id_from_symbol_grouped(symbol))) %>%
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
table_date <- 	strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")

user <- dplry::tibble	(
		user_id = numeric(),
		user_name = character(),
		password = character(),
		email = character(),
		user_role = character(),
		approved = integer()
	) %>%
  	dplry::add_row(
		user_id = 1, 
		user_name = "Robin", 
		password = "password1salt1", 
		email="Robin-Tobias.Jauss@medizin.uni-leipzig.de", 
		user_role="Administrator", 
		approved = 1
	) %>%
  	dplry::add_row(
		user_id = 2, 
		user_name = "Bernt", 
		password = "password2salt2", 
		email="bernt.popp@charite.de", 
		user_role="Administrator", 
		approved = 1
	) %>%
  	dplry::arrange(user_id) %>%
  	dplry::mutate(created_at = table_date)



############################################
## USE THIS SECTION IF THIS IS THE FIRST PANEL VERSION OR FIRST ID
## create mb_panel_version table
mb_panel_version <- dplry::tibble(
	  panel_id = numeric(),
	  panel_version = character(),
	  panel_date = character(),
	  is_current = logical(), 
	  upload_user = integer()
	) %>%
  	dplry::add_row(
			panel_id = 1, 
			panel_version = "v2023_01_1", 
			panel_date = strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), 
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
						dplry::select(hgnc_id) %>%
						dplry::mutate(panel_id = 1) %>% 
						dplry::mutate(panel_hgnc_id = row_number()) %>%
						dplry::select(panel_hgnc_id, panel_id, hgnc_id)

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
							dplry::mutate(panel_id = 1) %>% 
							dplry::mutate(panel_hgnc_id = row_number()) %>% 
							dplry::select(
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
				dplry::select(source_name) %>%
				unique() %>% 
				dplry::mutate(source_id = row_number()) %>% 
				dplry::mutate(source_logic = "dummy") %>% 
				dplry::select(source_id, source_name, source_logic)


############################################
## USE THIS SECTION IF THIS IS THE FIRST PANEL VERSION OR FIRST ID
## create mb_panel_genes_source_join table
mb_panel_genes_source_join <- 	mb_panel_genes_source %>%
								dplry::left_join(mb_source, by = "source_name") %>%
								dplry::select(-source_name, -source_logic) %>% 
								dplry::mutate(panel_hgnc_source_id = row_number()) %>%
								dplry::select(panel_hgnc_source_id, panel_hgnc_id, source_id)

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
creation_date <- strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")

write_csv(
	user, 
	file = paste0("results/user.",creation_date,".csv"),
	na = ""
)

write_csv(
	mb_panel_version, 
	file = paste0("results/mb_panel_version.",creation_date,".csv")
)
#gzip(paste0("results/mb_panel_version.",creation_date,".csv"))

write_csv(
	mb_panel_genes_join, 
	file = paste0("results/mb_panel_genes_join.",creation_date,".csv")
)
#gzip(paste0("results/mb_panel_genes_join.",creation_date,".csv"))

write_csv(
	mb_source, 
	file = paste0("results/mb_source.",creation_date,".csv")
)
#gzip(paste0("results/mb_source.",creation_date,".csv"))

write_csv(
	mb_panel_genes_source_join, 
	file = paste0("results/mb_panel_genes_source_join.",creation_date,".csv")
)
#gzip(paste0("results/mb_panel_genes_source_join.",creation_date,".csv"))
