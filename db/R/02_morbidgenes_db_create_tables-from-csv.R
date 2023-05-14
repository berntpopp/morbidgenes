# TODO: adapt this script tzo read all the csv files and generate complete tables

############################################
## load libraries
library(tidyverse)    ##needed for general table operations
library(jsonlite)    ##needed for HGNC requests
library("R.utils")  ## gzip files
############################################


############################################
## set working directory
# TODO: needs to be adapted to your specific working directory
setwd("C:/development/morbidgenes/db/R")
## set global options
options(scipen = 999)
############################################


############################################
# load global functions
# hgnc functions
source("../functions/hgnc-functions.R", local = TRUE)
source("../functions/hpo-functions.R", local = TRUE)
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
## create mg_panel_version table
mg_panel_version <- tibble(
    panel_id = numeric(),
    panel_version = character(),
    panel_date = character(),
    is_current = logical(),
    upload_user = numeric()
  ) %>%
  add_row(panel_id = 1,
    panel_version = "MorbidGenes_Panel_v2022_02_1",
    panel_date = strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"),
    is_current = TRUE,
    upload_user = 1)
############################################



############################################
## create mg_panel_genes_join table
mg_panel_genes_join <- MorbidGenes_Panel_hngc %>%
  select(hgnc_id) %>%
  mutate(panel_id = 1) %>% 
  mutate(panel_hgnc_id = row_number()) %>%
  select(panel_hgnc_id, panel_id, hgnc_id)
############################################



############################################
## create mg_panel_genes_source_join table
mg_panel_genes_source <- MorbidGenes_Panel_hngc %>%
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
creation_date <- strftime(as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d")

write_csv(mg_panel_version, file = paste0("results/mg_panel_version.",creation_date,".csv"))
gzip(paste0("results/mg_panel_version.",creation_date,".csv"), overwrite = TRUE)

write_csv(mg_panel_genes_join, file = paste0("results/mg_panel_genes_join.",creation_date,".csv"))
gzip(paste0("results/mg_panel_genes_join.",creation_date,".csv"), overwrite = TRUE)

write_csv(mg_source, file = paste0("results/mg_source.",creation_date,".csv"))
gzip(paste0("results/mg_source.",creation_date,".csv"), overwrite = TRUE)

write_csv(mg_panel_genes_source_join, file = paste0("results/mg_panel_genes_source_join.",creation_date,".csv"))
gzip(paste0("results/mg_panel_genes_source_join.",creation_date,".csv"), overwrite = TRUE)
############################################
