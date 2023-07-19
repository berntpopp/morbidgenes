# load libraries
library(tidyverse)	## needed for general table operations
library(biomaRt)	## needed to get gene coordinates
#library("R.utils")	## gzip downloaded files

source('utils.R', chdir = TRUE)
config <- get_config()

# set working directory (needs to be adapted to your specific working directory)
setwd(config$working_directory)
# set global options
options(scipen = config$scipen)

## functions to get gene coordinates from symbol name using biomart

# define mart
mart_hg19 <- useMart("ensembl", host="grch37.ensembl.org")
mart_hg19 <- useDataset("hsapiens_gene_ensembl", mart_hg19)

mart_hg38 <- useMart("ensembl", host="ensembl.org")
mart_hg38 <- useDataset("hsapiens_gene_ensembl", mart_hg38)

# function to retrive bed format style gene coordinates
gene_coordinates_from_symbol <- function(gene_symbols, reference = "hg19") {
	gene_symbol_list <- 	dplyr::as_tibble(gene_symbols) %>%
							dplyr::select(hgnc_symbol = value)

	if (reference == "hg19") {
		mart <- mart_hg19
	} else {
		mart <- mart_hg38
	}

	attributes <- c("hgnc_symbol", "chromosome_name", "start_position", "end_position")
	filters <- c("hgnc_symbol")

	values <- list(hgnc_symbol = gene_symbol_list$hgnc_symbol)

	gene_coordinates_hg19 <- 	getBM(
										attributes=attributes, 
										filters=filters, 
										values=values, 
										mart=mart
								) %>%
								dplyr::group_by(hgnc_symbol) %>%
								dplyr::summarise(
									hgnc_symbol = max(hgnc_symbol), 
									chromosome_name = max(chromosome_name), 
									start_position = max(start_position), 
									end_position = max(end_position)
								) %>%
								dplyr::mutate(
									bed_format = paste0("chr", chromosome_name, ":", start_position, "-", end_position)
								) %>%
								dplyr::select(
									hgnc_symbol, 
									bed_format
								)
	
	gene_symbol_list_return <- 	gene_symbol_list %>%
								dplyr::left_join(
									gene_coordinates_hg19, 
									by = ("hgnc_symbol")
								)
	
	return(gene_symbol_list_return)
}

# 
gene_coordinates_from_ensembl <- function(ensembl_id, reference = "hg19") {
	ensembl_id_list <- 	dplyr::as_tibble(ensembl_id) %>%
						dplyr::select(ensembl_gene_id = value)

	if (reference == "hg19") {
		mart <- mart_hg19
	} else {
		mart <- mart_hg38
	}
	
	attributes <- c("ensembl_gene_id", "chromosome_name", "start_position", "end_position")
	filters <- c("ensembl_gene_id")

	values <- list(ensembl_gene_id = ensembl_id_list$ensembl_gene_id)

	gene_coordinates_hg19 <- 	getBM(
									attributes=attributes, 
									filters=filters, 
									values=values, 
									mart=mart
								) %>%
								dplyr::group_by(ensembl_gene_id) %>%
								dplyr::summarise(
									ensembl_gene_id = max(ensembl_gene_id), 
									chromosome_name = max(chromosome_name), 
									start_position = max(start_position), 
									end_position = max(end_position)
								) %>%
								dplyr::mutate(
									bed_format = paste0("chr", chromosome_name, ":", start_position, "-", end_position)
								) %>%
								dplyr::select(
									ensembl_gene_id, 
									bed_format
								)
	
	ensembl_id_list_return <- 	ensembl_id_list %>%
								left_join(
									gene_coordinates_hg19, 
									by = ("ensembl_gene_id")
								)
	
	return(ensembl_id_list_return)
}

##-------------------------------------------------------------------------------##

# download HGNC file
hgnc_link <- "https://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/tsv/non_alt_loci_set.txt"
hgnc_file <- paste0(config$data_directory, "non_alt_loci_set.txt")
download.file(hgnc_link, hgnc_file, mode = "wb")
#gzip(hgnc_file)

# load the downloaded HGNC file
non_alt_loci_set <- 	read_delim(hgnc_file, "\t", col_names = TRUE) %>%
						dplyr::mutate(update_date = get_current_date()) 

non_alt_loci_set_coordinates <- 	non_alt_loci_set %>%
									dplyr::mutate(hg19_coordinates_from_ensembl = gene_coordinates_from_ensembl(ensembl_gene_id)) %>%
									dplyr::mutate(hg19_coordinates_from_symbol = gene_coordinates_from_symbol(symbol)) %>%
									dplyr::mutate(hg38_coordinates_from_ensembl = gene_coordinates_from_ensembl(ensembl_gene_id, reference = "hg38")) %>%
									dplyr::mutate(hg38_coordinates_from_symbol = gene_coordinates_from_symbol(symbol, reference = "hg38")) %>% 
									dplyr::mutate(bed_hg19 =
										dplyr::case_when(
											!is.na(hg19_coordinates_from_ensembl$bed_format) ~ hg19_coordinates_from_ensembl$bed_format,
											is.na(hg19_coordinates_from_ensembl$bed_format) ~ hg19_coordinates_from_symbol$bed_format,
										)
									) %>% 
									dplyr::mutate(bed_hg38 =
										dplyr::case_when(
											!is.na(hg38_coordinates_from_ensembl$bed_format) ~ hg38_coordinates_from_ensembl$bed_format,
											is.na(hg38_coordinates_from_ensembl$bed_format) ~ hg38_coordinates_from_symbol$bed_format,
										)
									) %>% 
									dplyr::select(
										-hg19_coordinates_from_ensembl, 
										-hg19_coordinates_from_symbol, 
										-hg38_coordinates_from_ensembl, 
										-hg38_coordinates_from_symbol
									)

mb_genes_hgnc_connect <- 	non_alt_loci_set_coordinates %>%
							dplyr::select(hgnc_id) %>%
							dplyr::mutate(is_active = TRUE)

# export table as csv with date of creation

non_alt_loci_set_coordinates_file_name <- paste0(config$results_directory, "non_alt_loci_set_coordinates.", get_current_date(), ".csv")
write_csv(
	non_alt_loci_set_coordinates, 
	file = non_alt_loci_set_coordinates_file_name
)
#gzip(non_alt_loci_set_coordinates_file_name)

mb_genes_hgnc_connect_file_name <- paste0(config$results_directory, "mb_genes_hgnc_connect.",get_current_date(),".csv")
write_csv(
	mb_genes_hgnc_connect, 
	file = mb_genes_hgnc_connect_file_name
)
#gzip(mb_genes_hgnc_connect_file_name)
