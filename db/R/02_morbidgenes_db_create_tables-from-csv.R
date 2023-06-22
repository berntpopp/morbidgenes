############################################
## load libraries
library(tidyverse)  ## needed for general table operations
library(jsonlite)   ## needed for HGNC requests
library("R.utils")  ## gzip files
library(config)     ## needed for config loading
library(tools)      ## needed for md5sum
############################################


############################################
## define relative script path
subfolder_path <- "/db/R/"
## read config
config_vars_default <- config::get(file = Sys.getenv("CONFIG_FILE"),
    config = "default")
config_vars_db <- config::get(file = Sys.getenv("CONFIG_FILE"),
    config = "db_setup")
## set working directory
setwd(paste0(config_vars$projectsdir, subfolder_path))
############################################


############################################
# load global functions
# hgnc functions
source("functions/hgnc-functions.R", local = TRUE)
source("functions/hpo-functions.R", local = TRUE)
############################################


############################################
# find all CSV files in results folders and filter

# define analyses paths
analyses_paths <- c("data/")

# select only MorbidGenes files
# select newest file
results_csv_table <- list.files(path = analyses_paths,
    pattern = ".csv.gz",
    full.names = TRUE) %>%
  as_tibble() %>%
  separate(value, c("path", "file"), sep = "\\/") %>%
  mutate(file_path = paste0(path, "/", file)) %>%
  mutate(file_basename = str_remove_all(file, "\\.csv\\.gz")) %>%
  separate(file_basename,
    c("mg", "panel", "version_year", "version_month"),
    sep = "-") %>%
  mutate(analysis = paste0(mg, "_", panel)) %>%
  mutate(version_month = str_replace_all(version_month, "\\.", "_")) %>%
  mutate(panel_version = paste0(version_year, "_", version_month)) %>%
  mutate(panel_date = str_replace_all(panel_version, "_", "-")) %>%
  mutate(panel_date = str_replace_all(panel_date, "v", "")) %>%
  mutate(panel_id = row_number()) %>%
  mutate(md5sum_import = md5sum(file_path)) %>%
  dplyr::select(panel_id,
    analysis,
    file_path,
    panel_version,
    panel_date,
    md5sum_import) %>%
  filter(str_detect(file_path, "MorbidGenes-Panel")) %>%
  group_by(analysis) %>%
  mutate(is_current = (max(panel_date) == panel_date)) %>%
  ungroup() %>%
  arrange(panel_id)
############################################


############################################
## load csv file(s) and reformat
# TODO: specify column specifications to suppress warnings
# load the csv files
results_panels <- results_csv_table %>%
  rowwise() %>%
  mutate(panel_list = list(read_csv2(file_path))) %>%
  ungroup() %>%
  select(analysis, panel_id, panel_list) %>%
  unnest(panel_list) %>%
  filter(morbidscore != 0) %>%
  select(symbol,
    hgmd_pathogenic_cutoff,
    clinvar_pathogenic_cutoff,
    manually_added,
    panelapp,
    panelapp_UK,
    panelapp_australia,
    sysndd,
    omim_phenotype,
    panel_id) %>%
  replace(is.na(.), FALSE)

# compute HGNC ID
morbidgenes_panel_hngc <- results_panels %>%
  mutate(hgnc_id = paste0("HGNC:", hgnc_id_from_symbol_grouped(symbol))) %>%
  select(hgnc_id,
    hgmd_pathogenic_cutoff,
    clinvar_pathogenic_cutoff,
    manually_added,
    panelapp,
    panelapp_UK,
    panelapp_australia,
    sysndd,
    omim_phenotype,
    panel_id)
############################################


############################################
## create mg_panel_version table
# TODO: add user from config or later in EP from token
mg_panel_version <- results_csv_table %>%
  select(panel_id, panel_version, panel_date, is_current) %>%
  distinct() %>%
  mutate(upload_user = 1)
############################################


############################################
## create mg_panel_genes_join table
mg_panel_genes_join <- morbidgenes_panel_hngc %>%
  select(hgnc_id, panel_id) %>%
  mutate(panel_hgnc_id = row_number()) %>%
  select(panel_hgnc_id, panel_id, hgnc_id)
############################################


############################################
## create mg_panel_genes_source_join table
mg_panel_genes_source <- morbidgenes_panel_hngc %>%
  mutate(panel_hgnc_id = row_number()) %>%
  select(panel_hgnc_id,
    hgmd_pathogenic_cutoff,
    clinvar_pathogenic_cutoff,
    manually_added,
    panelapp,
    panelapp_UK,
    panelapp_australia,
    sysndd,
    omim_phenotype) %>%
  pivot_longer(!panel_hgnc_id,
    names_to = "source_name",
    values_to = "in_source") %>%
  filter(in_source)

# TODO: add source_logic based on an input file
mg_source <- mg_panel_genes_source %>%
  select(source_name) %>%
  unique() %>%
  mutate(source_id = row_number()) %>%
  mutate(source_logic = "dummy") %>%
  select(source_id, source_name, source_logic)

mg_panel_genes_source_join <- mg_panel_genes_source %>%
  left_join(mg_source, by = "source_name") %>%
  select(-source_name, -source_logic) %>%
  mutate(panel_hgnc_source_id = row_number()) %>%
  select(panel_hgnc_source_id, panel_hgnc_id, source_id)
############################################


############################################
## export table as csv with date of creation
creation_date <- strftime(as.POSIXlt(Sys.time(),
    "UTC", "%Y-%m-%dT%H:%M:%S"),
  "%Y-%m-%d")

write_csv(mg_panel_version,
  file = paste0("results/mg_panel_version.", creation_date, ".csv"))

gzip(paste0("results/mg_panel_version.", creation_date, ".csv"),
  overwrite = TRUE)

write_csv(mg_panel_genes_join,
  file = paste0("results/mg_panel_genes_join.", creation_date, ".csv"))

gzip(paste0("results/mg_panel_genes_join.", creation_date, ".csv"),
  overwrite = TRUE)

write_csv(mg_source,
  file = paste0("results/mg_source.", creation_date, ".csv"))

gzip(paste0("results/mg_source.", creation_date, ".csv"),
  overwrite = TRUE)

write_csv(mg_panel_genes_source_join,
  file = paste0("results/mg_panel_genes_source_join.", creation_date, ".csv"))

gzip(paste0("results/mg_panel_genes_source_join.", creation_date, ".csv"),
  overwrite = TRUE)
############################################