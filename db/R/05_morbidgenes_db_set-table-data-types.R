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
## set column types in user table
# user_id	int
# user_name	varchar(50)
# password	varchar(50)
# email	varchar(150)
# orcid	varchar(50)
# first_name	varchar(100)
# family_name	varchar(100)
# user_role	varchar(50)
# terms_agreed	tinyint
# approved	tinyint
# created_at	TIMESTAMP
# password_reset_date	TIMESTAMP
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY user_id int;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY user_name varchar(50);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY password varchar(50);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY email varchar(150);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY orcid varchar(50);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY first_name varchar(100);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY family_name varchar(100);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY user_role varchar(50);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY terms_agreed tinyint;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY approved tinyint;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY created_at TIMESTAMP;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.user MODIFY password_reset_date TIMESTAMP;")
dbClearResult(rs)
############################################


############################################
## set column types in mg_panel_version table
# panel_id	int
# panel_version	varchar(50)
# panel_date	date
# is_current	tinyint
# upload_user	int
# file_path	varchar(100)
# md5sum_import	varchar(100)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY panel_id int;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY panel_version varchar(50);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY panel_date date;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY is_current tinyint;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY upload_user int;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY file_path varchar(100);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_version MODIFY md5sum_import varchar(100);")
dbClearResult(rs)
############################################


############################################
## set column types in mg_panel_genes_join table
# panel_hgnc_id	int
# panel_id	integer(10)
# hgnc_id	varchar(10)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_join MODIFY panel_hgnc_id int;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_join MODIFY panel_id integer(10);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_join MODIFY hgnc_id varchar(10);")
dbClearResult(rs)
############################################


############################################
## set column types in mg_panel_genes_source_join table
# panel_hgnc_source_id	int
# panel_hgnc_id	int
# source_id	int
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_source_join MODIFY panel_hgnc_source_id int;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_source_join MODIFY panel_hgnc_id int;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_panel_genes_source_join MODIFY source_id int;")
dbClearResult(rs)
############################################


############################################
## set column types in mg_source table
# source_id	int
# source_name	varchar(255)
# source_logic	varchar(255)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_source MODIFY source_id int;")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_source MODIFY source_name varchar(255);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_source MODIFY source_logic varchar(255);")
dbClearResult(rs)
############################################


############################################
## set column types in mg_genes_hgnc_connect table
# hgnc_id	varchar(10)	 
# is_active	tinyint
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_genes_hgnc_connect MODIFY hgnc_id varchar(10);")
dbClearResult(rs)
rs <- dbSendQuery(morbidgenes_db, "ALTER TABLE morbidgenes_db.mg_genes_hgnc_connect MODIFY is_active tinyint;")
dbClearResult(rs)
############################################


############################################
## close database connection
rm_con()
############################################