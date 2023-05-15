# MorbidGenes Database R Scripts
## R scripts needed to generate the morbidgenes MySQL DB schema

This repository contains a set of R scripts that are used to create and manage the MorbidGenes database. The scripts are located in the `db/R/` directory.

## Scripts

1. **01_morbidgenes_table_hgnc_non_alt_loci_set.R**
   - This script is used to download the HGNC gene file from EBI's FTP site, process the gene data, and then export the final tables into CSV files. The tables created include gene coordinates and their respective HGNC IDs.

2. **02_morbidgenes_db_create_tables-from-csv.R**
   - This script reads the CSV file "MorbidGenes-Panel-v2022-02.1.csv.gz", processes the data and creates four different tables: mg_panel_version, mg_panel_genes_join, mg_source and mg_panel_genes_source_join.
Each table is then written to a gzipped CSV file in the results/ directory.

3. **03_Rcommands_sysndd_db_table_user.R**
   - This script generates a user table that contains information about users, such as their usernames, passwords, emails, names, roles, and other details.
   - It currently generates data for two users: an administrator and a curator.
   - The table is then written to a gzipped CSV file in the results/ directory.

4. **04_morbidgenes_db_create-database-tables-in-mysql.R**
   - This script loads all gzipped CSV files from the results/ directory, calculates their MD5 checksums, and stores them in a table called results_csv_table.
   - The script then drops any existing tables in the database that have the same names as the tables to be imported.
   - The results_csv_table is written into the MySQL database.
   - Each table is then imported into the MySQL database.
   - Finally, the database connection is closed.

5. **05_morbidgenes_db_set-table-connections.R**
   - This script connects to a MySQL database, modifies table structures, and creates several views in the database. It uses the tidyverse, DBI, RMariaDB, sqlr, and config libraries.
   - The script requires a configuration file that specifies the database name, user, password, server, and port, which is set in the environment variable CONFIG_FILE.

## Dependencies

- tidyverse
- biomaRt
- R.utils
- config

## Configuration

The scripts expects a configuration file to be specified in the `CONFIG_FILE` environment variable, which should contain a `projectsdir` value pointing to the base directory of the project. The configuration file should be in the format expected by the `config` package.