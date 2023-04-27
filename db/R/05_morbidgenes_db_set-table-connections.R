############################################
## load libraries
library(tidyverse)  ##needed for general table operations
library(DBI)    ##needed for MySQL data export
library(RMariaDB)  ##needed for MySQL data export
library(sqlr)    ##needed for MySQL data export
############################################



############################################
## set working directory
# TODO: needs to be adapted to your specific working directory
setwd("C:/development/morbidgenes/db/R")
## set global options
options(scipen = 999)
############################################



############################################
## connect to the database
morbidgenes_db <- dbConnect(RMariaDB::MariaDB(), dbname = "morbidgenes_db", user = "root", password = "morbidgenes-db", server = "127.0.0.1", port = "9918")
############################################


############################################
## make the primary keys auto increment
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_version MODIFY panel_id int auto_increment;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_genes_join MODIFY panel_hgnc_id int auto_increment;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_genes_source_join MODIFY panel_hgnc_source_id int auto_increment;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_source MODIFY source_id int auto_increment;")
dbClearResult(rs)

############################################
## make panel_id in all tables compatible as int
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_version MODIFY panel_id int NOT NULL;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_genes_join MODIFY panel_id int NOT NULL;")
dbClearResult(rs)

############################################
## make panel_hgnc_id in all tables compatible as int
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_genes_join MODIFY panel_hgnc_id int NOT NULL;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_genes_source_join MODIFY panel_hgnc_id int NOT NULL;")
dbClearResult(rs)

############################################
## make source_id in all tables compatible as int
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_genes_source_join MODIFY source_id int NOT NULL;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_source MODIFY source_id int NOT NULL;")
dbClearResult(rs)


############################################
## add foreign key constrains
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_genes_join ADD FOREIGN KEY (panel_id) REFERENCES morbidgenes_db.mb_panel_version(panel_id);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_genes_join ADD FOREIGN KEY (hgnc_id) REFERENCES morbidgenes_db.mb_genes_hgnc_connect(hgnc_id);")
dbClearResult(rs)

rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_genes_hgnc_connect ADD FOREIGN KEY (hgnc_id) REFERENCES morbidgenes_db.non_alt_loci_set_coordinates(hgnc_id);")
dbClearResult(rs)

rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_genes_source_join ADD FOREIGN KEY (panel_hgnc_id) REFERENCES morbidgenes_db.mb_panel_genes_join(panel_hgnc_id);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mb_panel_genes_source_join ADD FOREIGN KEY (source_id) REFERENCES morbidgenes_db.mb_source(source_id);")
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
        (`morbidgenes_db`.`mb_genes_hgnc_connect`
        JOIN `morbidgenes_db`.`non_alt_loci_set_coordinates` ON ((`morbidgenes_db`.`mb_genes_hgnc_connect`.`hgnc_id` = `morbidgenes_db`.`non_alt_loci_set_coordinates`.`hgnc_id`)))
    WHERE
        (`morbidgenes_db`.`mb_genes_hgnc_connect`.`is_active` = 1)")
dbClearResult(rs)


# view_panel_genes
rs <- dbSendQuery(morbidgenes_db, "CREATE OR REPLACE VIEW `morbidgenes_db`.`view_panel_genes` AS
    SELECT 
        `morbidgenes_db`.`mb_panel_version`.`panel_id` AS `panel_id`,
        `morbidgenes_db`.`mb_panel_version`.`panel_version` AS `panel_version`,
        `morbidgenes_db`.`mb_panel_genes_join`.`panel_hgnc_id` AS `panel_hgnc_id`,
        `morbidgenes_db`.`mb_panel_genes_join`.`hgnc_id` AS `hgnc_id`
    FROM
        (`morbidgenes_db`.`mb_panel_version`
        JOIN `morbidgenes_db`.`mb_panel_genes_join` ON ((`morbidgenes_db`.`mb_panel_version`.`panel_id` = `morbidgenes_db`.`mb_panel_genes_join`.`panel_id`)))
    WHERE
        (`morbidgenes_db`.`mb_panel_version`.`is_current` = 1)")
dbClearResult(rs)


# view_panel_genes_source
rs <- dbSendQuery(morbidgenes_db, "CREATE OR REPLACE VIEW `morbidgenes_db`.`view_panel_genes_source` AS
    SELECT 
        `morbidgenes_db`.`mb_panel_genes_source_join`.`panel_hgnc_id` AS `panel_hgnc_id`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mb_source`.`source_name` = 'PanelApp') THEN 1
            ELSE 0
        END)) AS `PanelApp`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mb_source`.`source_name` = 'AustraliaPanelApp') THEN 1
            ELSE 0
        END)) AS `AustraliaPanelApp`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mb_source`.`source_name` = 'HGMD_pathogenic') THEN 1
            ELSE 0
        END)) AS `HGMD_pathogenic`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mb_source`.`source_name` = 'Phenotype_MIM') THEN 1
            ELSE 0
        END)) AS `Phenotype_MIM`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mb_source`.`source_name` = 'ClinVarPathogenic') THEN 1
            ELSE 0
        END)) AS `ClinVarPathogenic`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mb_source`.`source_name` = 'UKPanelApp') THEN 1
            ELSE 0
        END)) AS `UKPanelApp`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mb_source`.`source_name` = 'SysNDD') THEN 1
            ELSE 0
        END)) AS `SysNDD`,
        MAX((CASE
            WHEN (`morbidgenes_db`.`mb_source`.`source_name` = 'Manually') THEN 1
            ELSE 0
        END)) AS `Manually`,
        COUNT(0) AS `mg_score`
    FROM
        (`morbidgenes_db`.`mb_panel_genes_source_join`
        JOIN `morbidgenes_db`.`mb_source` ON ((`morbidgenes_db`.`mb_panel_genes_source_join`.`source_id` = `morbidgenes_db`.`mb_source`.`source_id`)))
    GROUP BY `morbidgenes_db`.`mb_panel_genes_source_join`.`panel_hgnc_id`")
dbClearResult(rs)


# view_panel_current
rs <- dbSendQuery(morbidgenes_db, "CREATE OR REPLACE VIEW `morbidgenes_db`.`view_panel_current` AS
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
        `morbidgenes_db`.`view_panel_genes_source`.`mg_score` AS `mg_score`
    FROM
        ((`morbidgenes_db`.`view_panel_genes`
        JOIN `morbidgenes_db`.`view_genes_hgnc` ON ((`morbidgenes_db`.`view_panel_genes`.`hgnc_id` = `morbidgenes_db`.`view_genes_hgnc`.`hgnc_id`)))
        JOIN `morbidgenes_db`.`view_panel_genes_source` ON ((`morbidgenes_db`.`view_panel_genes`.`panel_hgnc_id` = `morbidgenes_db`.`view_panel_genes_source`.`panel_hgnc_id`)))")
dbClearResult(rs)

############################################
## close database connection
rm_con()
############################################