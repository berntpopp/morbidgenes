# TODO: define column types for all tables
# TODO: set constrains (is_current, is_active, ...)
############################################
## load libraries
library(tidyverse)  ## needed for general table operations
library(DBI)        ## needed for MySQL data export
library(RMariaDB)   ## needed for MySQL data export
library(sqlr)       ## needed for MySQL data export
library(config)     ## needed for config loading
############################################


############################################
## define relative script path
project_topic <- "morbidgenes"
project_name <- "morbidgenes"
script_path <- "/db/R/"

## read config
config_vars_proj <- config::get(file = Sys.getenv("CONFIG_FILE"),
    config = project_topic)

## set working directory
setwd(paste0(config_vars_proj$projectsdir, script_path))

## load db config file
config_vars_db <- config::get(file = "./config.yml",
    config = "db_setup")
############################################


############################################
## set global options
options(scipen = 999)
############################################


############################################
## connect to the database
morbidgenes_db <- dbConnect(RMariaDB::MariaDB(),
    dbname = config_vars_db$dbname,
    user = config_vars_db$username,
    password = config_vars_db$password,
    server = config_vars_db$server,
    port = config_vars_db$port)
############################################


############################################
## make the primary keys auto increment
# panel_id in mg_panel_version
## set default value to 0 for primary keys auto incrementing
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY panel_id int NOT NULL DEFAULT 0;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY panel_id int AUTO_INCREMENT;")
dbClearResult(rs)

# panel_hgnc_id in mg_panel_genes_join
## set default value to 0 for primary keys auto incrementing
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_join MODIFY panel_hgnc_id int NOT NULL DEFAULT 0;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_join MODIFY COLUMN panel_hgnc_id int AUTO_INCREMENT;")
dbClearResult(rs)

# panel_hgnc_source_id in mg_panel_genes_source_join
## set default value to 0 for primary keys auto incrementing
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_source_join MODIFY panel_hgnc_source_id int NOT NULL DEFAULT 0;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_source_join MODIFY panel_hgnc_source_id int AUTO_INCREMENT;")
dbClearResult(rs)

# source_id in mg_source
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_source MODIFY source_id int NOT NULL DEFAULT 0;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_source MODIFY source_id int AUTO_INCREMENT;")
dbClearResult(rs)

# user_id in user
## set default value to 0 for primary keys auto incrementing
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY user_id int NOT NULL DEFAULT 0;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY user_id int AUTO_INCREMENT;")
dbClearResult(rs)

############################################
## make panel_id in all tables compatible as int
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_join MODIFY panel_id int NOT NULL;")
dbClearResult(rs)

############################################
## make panel_hgnc_id in all tables compatible as int
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_source_join MODIFY panel_hgnc_id int NOT NULL;")
dbClearResult(rs)

############################################
## make source_id in all tables compatible as int
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_source MODIFY source_id int NOT NULL;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_source_join MODIFY source_id int NOT NULL;")
dbClearResult(rs)

############################################
## make user_ids in all tables compatible as int
## and make the entry user required in all tables
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY user_id int;")
dbClearResult(rs)

rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY upload_user int NOT NULL;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY upload_user int;")
dbClearResult(rs)

############################################
## add foreign key constrains
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_join ADD FOREIGN KEY (panel_id) REFERENCES morbidgenes_db.mg_panel_version(panel_id);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_join ADD FOREIGN KEY (hgnc_id) REFERENCES morbidgenes_db.mg_genes_hgnc_connect(hgnc_id);")
dbClearResult(rs)

rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_genes_hgnc_connect ADD FOREIGN KEY (hgnc_id) REFERENCES morbidgenes_db.non_alt_loci_set_coordinates(hgnc_id);")
dbClearResult(rs)

rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_source_join ADD FOREIGN KEY (panel_hgnc_id) REFERENCES morbidgenes_db.mg_panel_genes_join(panel_hgnc_id);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_source_join ADD FOREIGN KEY (source_id) REFERENCES morbidgenes_db.mg_source(source_id);")
dbClearResult(rs)

rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version ADD FOREIGN KEY (upload_user) REFERENCES morbidgenes_db.user(user_id);")
dbClearResult(rs)

############################################
## create views
# view_genes_hgnc
rs <- dbSendQuery(morbidgenes_db, "CREATE OR REPLACE VIEW `morbidgenes_db`.`view_genes_hgnc` AS
    SELECT 
        `morbidgenes_db`.`non_alt_loci_set_coordinates`.`hgnc_id` AS `hgnc_id`,
        `morbidgenes_db`.`non_alt_loci_set_coordinates`.`symbol` AS `symbol`,
        `morbidgenes_db`.`non_alt_loci_set_coordinates`.`bed_hg19` AS `bed_hg19`,
        `morbidgenes_db`.`non_alt_loci_set_coordinates`.`bed_hg38` AS `bed_hg38`
    FROM
        (`morbidgenes_db`.`mg_genes_hgnc_connect`
        JOIN `morbidgenes_db`.`non_alt_loci_set_coordinates` ON ((`morbidgenes_db`.`mg_genes_hgnc_connect`.`hgnc_id` = `morbidgenes_db`.`non_alt_loci_set_coordinates`.`hgnc_id`)))
    WHERE
        (`morbidgenes_db`.`mg_genes_hgnc_connect`.`is_active` = 1)")
dbClearResult(rs)


# view_panel_genes
rs <- dbSendQuery(morbidgenes_db, "CREATE OR REPLACE VIEW `morbidgenes_db`.`view_panel_genes` AS
    SELECT 
        `morbidgenes_db`.`mg_panel_version`.`panel_id` AS `panel_id`,
        `morbidgenes_db`.`mg_panel_version`.`panel_version` AS `panel_version`,
        `morbidgenes_db`.`mg_panel_genes_join`.`panel_hgnc_id` AS `panel_hgnc_id`,
        `morbidgenes_db`.`mg_panel_genes_join`.`hgnc_id` AS `hgnc_id`,
        `morbidgenes_db`.`mg_panel_version`.`is_current` AS `is_current`
    FROM
        (`morbidgenes_db`.`mg_panel_version`
        JOIN `morbidgenes_db`.`mg_panel_genes_join` ON ((`morbidgenes_db`.`mg_panel_version`.`panel_id` = `morbidgenes_db`.`mg_panel_genes_join`.`panel_id`)))")
dbClearResult(rs)


# view_panel_genes_source
rs <- dbSendQuery(morbidgenes_db, "CREATE OR REPLACE VIEW `morbidgenes_db`.`view_panel_genes_source` AS
    SELECT 
        `morbidgenes_db`.`mg_panel_genes_source_join`.`panel_hgnc_id` AS `panel_hgnc_id`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mg_source`.`source_name` = 'PanelApp') THEN 1
            ELSE 0
        END)) AS `PanelApp`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mg_source`.`source_name` = 'AustraliaPanelApp') THEN 1
            ELSE 0
        END)) AS `AustraliaPanelApp`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mg_source`.`source_name` = 'HGMD_pathogenic') THEN 1
            ELSE 0
        END)) AS `HGMD_pathogenic`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mg_source`.`source_name` = 'Phenotype_MIM') THEN 1
            ELSE 0
        END)) AS `Phenotype_MIM`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mg_source`.`source_name` = 'ClinVarPathogenic') THEN 1
            ELSE 0
        END)) AS `ClinVarPathogenic`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mg_source`.`source_name` = 'UKPanelApp') THEN 1
            ELSE 0
        END)) AS `UKPanelApp`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mg_source`.`source_name` = 'SysNDD') THEN 1
            ELSE 0
        END)) AS `SysNDD`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mg_source`.`source_name` = 'Manually') THEN 1
            ELSE 0
        END)) AS `Manually`,
        COUNT(0) AS `mg_score`
    FROM
        (`morbidgenes_db`.`mg_panel_genes_source_join`
        JOIN `morbidgenes_db`.`mg_source` ON ((`morbidgenes_db`.`mg_panel_genes_source_join`.`source_id` = `morbidgenes_db`.`mg_source`.`source_id`)))
    GROUP BY `morbidgenes_db`.`mg_panel_genes_source_join`.`panel_hgnc_id`")
dbClearResult(rs)


# view_panel
rs <- dbSendQuery(morbidgenes_db, "CREATE OR REPLACE VIEW `morbidgenes_db`.`view_panel` AS
    SELECT 
        `morbidgenes_db`.`view_panel_genes`.`panel_version` AS `panel_version`,
        `morbidgenes_db`.`view_genes_hgnc`.`hgnc_id` AS `hgnc_id`,
        `morbidgenes_db`.`view_genes_hgnc`.`symbol` AS `symbol`,
        `morbidgenes_db`.`view_genes_hgnc`.`bed_hg19` AS `bed_hg19`,
        `morbidgenes_db`.`view_genes_hgnc`.`bed_hg38` AS `bed_hg38`,
        `morbidgenes_db`.`view_panel_genes_source`.`PanelApp` AS `PanelApp`,
        `morbidgenes_db`.`view_panel_genes_source`.`AustraliaPanelApp` AS `AustraliaPanelApp`,
        `morbidgenes_db`.`view_panel_genes_source`.`HGMD_pathogenic` AS `HGMD_pathogenic`,
        `morbidgenes_db`.`view_panel_genes_source`.`Phenotype_MIM` AS `Phenotype_MIM`,
        `morbidgenes_db`.`view_panel_genes_source`.`ClinVarPathogenic` AS `ClinVarPathogenic`,
        `morbidgenes_db`.`view_panel_genes_source`.`UKPanelApp` AS `UKPanelApp`,
        `morbidgenes_db`.`view_panel_genes_source`.`SysNDD` AS `SysNDD`,
        `morbidgenes_db`.`view_panel_genes_source`.`Manually` AS `Manually`,
        `morbidgenes_db`.`view_panel_genes_source`.`mg_score` AS `mg_score`,
        `morbidgenes_db`.`view_panel_genes`.`is_current` AS `is_current`
    FROM
        ((`morbidgenes_db`.`view_panel_genes`
        JOIN `morbidgenes_db`.`view_genes_hgnc` ON ((`morbidgenes_db`.`view_panel_genes`.`hgnc_id` = `morbidgenes_db`.`view_genes_hgnc`.`hgnc_id`)))
        JOIN `morbidgenes_db`.`view_panel_genes_source` ON ((`morbidgenes_db`.`view_panel_genes`.`panel_hgnc_id` = `morbidgenes_db`.`view_panel_genes_source`.`panel_hgnc_id`)))")
dbClearResult(rs)

############################################
## close database connection
rm_con()
############################################