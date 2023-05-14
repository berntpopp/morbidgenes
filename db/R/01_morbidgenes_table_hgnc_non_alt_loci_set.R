############################################
## load libraries
library(tidyverse)  ## needed for general table operations
library(biomaRt)  ## needed to get gene coordinates
library("R.utils")  ## gzip downloaded files
library(config)    ## needed for config loading
############################################


############################################
## define relative script path
subfolder_path <- "/db/R/"
## read config
config_vars <- config::get(file = Sys.getenv("CONFIG_FILE"))
## set working directory
setwd(paste0(config_vars$projectsdir, subfolder_path))
############################################


############################################
## set global options
options(scipen = 999)
############################################


############################################
# load global functions
# hgnc functions
source("../functions/ensembl-functions.R", local = TRUE)
############################################


############################################
## download HGNC file
file_date <- strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")
hgnc_link <- "http://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/tsv/non_alt_loci_set.txt"
hgnc_file <- "data/non_alt_loci_set.txt"
download.file(hgnc_link, hgnc_file, mode = "wb")
gzip(hgnc_file, overwrite = TRUE)
############################################



############################################
## load the downloaded HGNC file
non_alt_loci_set <- read_delim(paste0(hgnc_file,".gz"), "\t", col_names = TRUE) %>%
  mutate(update_date = file_date)

non_alt_loci_set_coordinates <- non_alt_loci_set %>%
  mutate(hg19_coordinates_from_ensembl = gene_coordinates_from_ensembl(ensembl_gene_id)) %>%
  mutate(hg19_coordinates_from_symbol = gene_coordinates_from_symbol(symbol)) %>%
  mutate(hg38_coordinates_from_ensembl = gene_coordinates_from_ensembl(ensembl_gene_id, reference = "hg38")) %>%
  mutate(hg38_coordinates_from_symbol = gene_coordinates_from_symbol(symbol, reference = "hg38")) %>% 
  mutate(bed_hg19 =
    case_when(
      !is.na(hg19_coordinates_from_ensembl$bed_format) ~ hg19_coordinates_from_ensembl$bed_format,
      is.na(hg19_coordinates_from_ensembl$bed_format) ~ hg19_coordinates_from_symbol$bed_format,
    )
  ) %>% 
  mutate(bed_hg38 =
    case_when(
      !is.na(hg38_coordinates_from_ensembl$bed_format) ~ hg38_coordinates_from_ensembl$bed_format,
      is.na(hg38_coordinates_from_ensembl$bed_format) ~ hg38_coordinates_from_symbol$bed_format,
    )
  ) %>% 
  dplyr::select(-hg19_coordinates_from_ensembl, -hg19_coordinates_from_symbol, -hg38_coordinates_from_ensembl, -hg38_coordinates_from_symbol)

mg_genes_hgnc_connect <- non_alt_loci_set_coordinates %>%
  dplyr::select(hgnc_id) %>%
  mutate(is_active = TRUE)

############################################



############################################
## export table as csv with date of creation
creation_date <- strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")

write_csv(non_alt_loci_set_coordinates, file = paste0("results/non_alt_loci_set_coordinates.",creation_date,".csv"))
gzip(paste0("results/non_alt_loci_set_coordinates.",creation_date,".csv"), overwrite = TRUE)

write_csv(mg_genes_hgnc_connect, file = paste0("results/mg_genes_hgnc_connect.",creation_date,".csv"))
gzip(paste0("results/mg_genes_hgnc_connect.",creation_date,".csv"), overwrite = TRUE)
############################################