# R "Is creating the database used in Morbidgenes panel from csv"

## Folders an files
- _data_: contains input files, vreated by Robin-Tobias Jauss
- _results_: output folder
- _db_step01_create_tables-from-csv.R_: input in data folder csv-file is used to create tables in data folder
- _db_step02_create-database-tables-in-mysql.R_: creates tables from rtesult folder and writes them in database
- _db_step03_set-table-connections.R_: sets connections between tables in db
- _setup.R_: script to install required R-Packages
- _table_hgnc_non_alt_loci_set.R_: gets gene positions and creates table in result folder


## Requirements
- running MySQL Database
- installed R with packages
    - tidyverse
    - jsonlite
    - tools
    - DBI
    - RMariaDB
    - biomaRt
    - sqlr --> You need to use Bernt's forked package 
        - https://github.com/berntpopp/sqlr

### Installation
- in `/db/R`
- install R-Core
    - `sudo apt install r-base-core r-cran-xml2 libcurl4-openssl-dev libmariadb-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev`
- install R-Packages
    - `sudo Rscript setup.R`

## Create Database
- `cd morbidgenes/db/R`
- `Rscript db_step01_create_tables-from-csv.R`
- `Rscript db_step02_create-database-tables-in-mysql.R`
