############################################
## load libraries
library(tidyverse)		##needed for general table operations
library(jsonlite)		##needed for HGNC requests
############################################


############################################
## set working directory (needs to be adapted to your specific working directory)
setwd("/home/MorbidGenes/00_DEV/morbidgenes/db/R")
## set global options
options(scipen = 999)
############################################



############################################
## define functions

hgnc_id_from_prevsymbol <- function(symbol_input)  {
	symbol_request <- fromJSON(paste0("http://rest.genenames.org/search/prev_symbol/", symbol_input))

	hgnc_id_from_symbol <- as_tibble(symbol_request$response$docs)
	
	hgnc_id_from_symbol <- hgnc_id_from_symbol %>%
	mutate(hgnc_id = if (exists('hgnc_id', where = hgnc_id_from_symbol)) hgnc_id else NA) %>%
	mutate(symbol = if (exists('symbol', where = hgnc_id_from_symbol)) symbol else "") %>%
	mutate(score = if (exists('score', where = hgnc_id_from_symbol)) score else 0) %>%
	arrange(desc(score)) %>%
	mutate(hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2]))

	return(as.integer(hgnc_id_from_symbol$hgnc_id[1]))
}

hgnc_id_from_aliassymbol <- function(symbol_input)  {
	symbol_request <- fromJSON(paste0("http://rest.genenames.org/search/alias_symbol/", symbol_input))

	hgnc_id_from_symbol <- as_tibble(symbol_request$response$docs)
	
	hgnc_id_from_symbol <- hgnc_id_from_symbol %>%
	mutate(hgnc_id = if (exists('hgnc_id', where = hgnc_id_from_symbol)) hgnc_id else NA) %>%
	mutate(symbol = if (exists('symbol', where = hgnc_id_from_symbol)) symbol else "") %>%
	mutate(score = if (exists('score', where = hgnc_id_from_symbol)) score else 0) %>%
	arrange(desc(score)) %>%
	mutate(hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2]))

	return(as.integer(hgnc_id_from_symbol$hgnc_id[1]))
}

hgnc_id_from_symbol <- function(symbol_tibble) {
	symbol_list_tibble <- as_tibble(symbol_tibble) %>% select(symbol = value) %>% mutate(symbol = toupper(symbol))
	
	symbol_request <- fromJSON(paste0("http://rest.genenames.org/search/symbol/", str_c(symbol_list_tibble$symbol, collapse = "+OR+")))

	hgnc_id_from_symbol <- as_tibble(symbol_request$response$docs)
	
	hgnc_id_from_symbol <- hgnc_id_from_symbol %>%
	mutate(hgnc_id = if (exists('hgnc_id', where = hgnc_id_from_symbol)) hgnc_id else NA) %>%
	mutate(symbol = if (exists('symbol', where = hgnc_id_from_symbol)) toupper(symbol) else "") %>%
	mutate(hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2]))
		
	return_tibble <- symbol_list_tibble %>% 
	left_join(hgnc_id_from_symbol, by = "symbol") %>%
	select(hgnc_id)

	return(return_tibble)
}


symbol_from_hgnc_id <- function(hgnc_id_tibble) {
	hgnc_id_list_tibble <- as_tibble(hgnc_id_tibble) %>% 
		select(hgnc_id = value) %>%
		mutate(hgnc_id = as.integer(hgnc_id))
	
	hgnc_id_request <- fromJSON(paste0("http://rest.genenames.org/search/hgnc_id/", str_c(hgnc_id_list_tibble$hgnc_id, collapse = "+OR+")))

	hgnc_id_from_hgnc_id <- as_tibble(hgnc_id_request$response$docs)
	
	hgnc_id_from_hgnc_id <- hgnc_id_from_hgnc_id %>%
	mutate(hgnc_id = if (exists('hgnc_id', where = hgnc_id_from_hgnc_id)) hgnc_id else NA) %>%
	mutate(hgnc_id = if (exists('hgnc_id', where = hgnc_id_from_hgnc_id)) toupper(hgnc_id) else "") %>%
	mutate(hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2]))
		
	return_tibble <- hgnc_id_list_tibble %>% 
	left_join(hgnc_id_from_hgnc_id, by = "hgnc_id") %>%
	select(symbol)

	return(return_tibble)
}


hgnc_id_from_symbol_grouped <- function(input_tibble, request_max = 150) {
	input_tibble <- as_tibble(input_tibble)
	
	row_number <- nrow(input_tibble)
	groups_number <- ceiling(row_number/request_max)
	
	input_tibble_request <- input_tibble %>%
		mutate(group = sample(1:groups_number, row_number, replace=T)) %>%
		group_by(group) %>%
		mutate(response = hgnc_id_from_symbol(value)$hgnc_id) %>%
		ungroup()
	
	input_tibble_request_repair <- input_tibble_request %>%
		filter(is.na(response)) %>%
		select(value) %>%
		unique() %>%
		rowwise() %>%
		mutate(response = hgnc_id_from_prevsymbol(value)) %>%
		mutate(response = case_when(!is.na(response) ~ response, is.na(response) ~ hgnc_id_from_aliassymbol(value)))
	
	input_tibble_request <- input_tibble_request %>%
		left_join(input_tibble_request_repair, by = "value") %>%
		mutate(response = case_when(!is.na(response.x) ~ response.x, is.na(response.x) ~ response.y))
	
	return(input_tibble_request$response)
}

############################################



############################################
## load csv file(s) and reformat
MorbidGenes_Panel <- read_delim("data/MorbidGenes-Panel-v2022-02.1.csv.gz", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
	filter(MorbidScore != 0) %>%
	select(symbol = SYMBOL, HGMD_pathogenic_variant_count_cutoff, ClinVarPathogenicCount_cutoff, addedManually, isPanelAppGene, isUKPanelAppGene, isAustraliaPanelAppGene, isSysNDDGene, has_Phenotype_MIM_Number) %>% 
	mutate(version = "MorbidGenes_Panel_v2022_02_1") %>% 
	replace(is.na(.), FALSE)
	
# compute HGNC ID
MorbidGenes_Panel_hngc <- MorbidGenes_Panel %>%
	mutate(hgnc_id = paste0("HGNC:", hgnc_id_from_symbol_grouped(symbol))) %>%
	select(hgnc_id, HGMD_pathogenic_variant_count_cutoff, ClinVarPathogenicCount_cutoff, addedManually, isPanelAppGene, isUKPanelAppGene, isAustraliaPanelAppGene, isSysNDDGene, has_Phenotype_MIM_Number)
############################################



############################################
## create mb_panel_version table
mb_panel_version <- tibble(
	  panel_id = numeric(),
	  panel_version = character(),
	  panel_date = character(),
	  is_current = logical()
	) %>%
	add_row(panel_id = 1, panel_version = "MorbidGenes_Panel_v2022_02_1", panel_date = strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), is_current=TRUE)
############################################



############################################
## create mb_panel_genes_join table
mb_panel_genes_join <- MorbidGenes_Panel_hngc %>%
	select(hgnc_id) %>%
	mutate(panel_id = 1) %>% 
	mutate(panel_hgnc_id = row_number()) %>%
	select(panel_hgnc_id, panel_id, hgnc_id)
############################################



############################################
## create mb_panel_genes_source_join table
mb_panel_genes_source <- MorbidGenes_Panel_hngc %>%
	mutate(panel_id = 1) %>% 
	mutate(panel_hgnc_id = row_number()) %>% 
	select(panel_hgnc_id, HGMD_pathogenic_variant_count_cutoff, ClinVarPathogenicCount_cutoff, addedManually, isPanelAppGene, isUKPanelAppGene, isAustraliaPanelAppGene, isSysNDDGene, has_Phenotype_MIM_Number) %>% 
	pivot_longer(!panel_hgnc_id, names_to = "source_name", values_to = "in_source") %>%
	filter(in_source) %>% 
	mutate(source_name = str_replace(source_name, "Gene", "")) %>% 
	mutate(source_name = str_replace(source_name, "is", "")) %>%
	mutate(source_name = str_replace(source_name, "has_", "")) %>%
	mutate(source_name = str_replace(source_name, "_variant_count_cutoff", "")) %>%
	mutate(source_name = str_replace(source_name, "_Number", "")) %>%
	mutate(source_name = str_replace(source_name, "Count_cutoff", "")) %>%
	mutate(source_name = str_replace(source_name, "added", ""))

mb_source <- mb_panel_genes_source %>%
	select(source_name) %>%
	unique() %>% 
	mutate(source_id = row_number()) %>% 
	mutate(source_logic = "dummy") %>% 
	select(source_id, panel_hgnc_id, source_logic)

mb_panel_genes_source_join <- mb_panel_genes_source %>%
	left_join(mb_source, by = "source_name") %>%
	select(-source_name, -source_logic) %>% 
	mutate(panel_hgnc_source_id = row_number()) %>%
	select(panel_hgnc_source_id, panel_hgnc_id, source_id)

############################################



############################################
## export table as csv with date of creation
creation_date <- strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")

write_csv(mb_panel_version, file = paste0("results/mb_panel_version.",creation_date,".csv"))
gzip(paste0("results/mb_panel_version.",creation_date,".csv"))

write_csv(mb_panel_genes_join, file = paste0("results/mb_panel_genes_join.",creation_date,".csv"))
gzip(paste0("results/mb_panel_genes_join.",creation_date,".csv"))

write_csv(mb_source, file = paste0("results/mb_source.",creation_date,".csv"))
gzip(paste0("results/mb_source.",creation_date,".csv"))

write_csv(mb_panel_genes_source_join, file = paste0("results/mb_panel_genes_source_join.",creation_date,".csv"))
gzip(paste0("results/mb_panel_genes_source_join.",creation_date,".csv"))
############################################
