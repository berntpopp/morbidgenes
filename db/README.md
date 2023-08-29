# MorbidGenes DB Directory

## Table of Contents

- [Overview](#overview)
- [Directory Structure](#directory-structure)
- [Files Description](#files-description)
- [Database Schema](#database-schema)
  - [Tables](#tables)
  - [Relationships](#relationships)
  - [Constraints](#constraints)
  - [Indexes](#indexes)
- [Library Dependencies](#library-dependencies)

## Overview

The `db` directory contains scripts and files that manage the database schema and data of the MorbidGenes project. It includes R scripts to create tables, set relationships, and perform various database operations. The database used is MySQL.


## Directory Structure

```
db/
├── R/
│   ├── 01_morbidgenes_table_hgnc_non_alt_loci_set.R
│   ├── 02_morbidgenes_db_create_tables-from-csv.R
│   ├── 03_morbidgenes_db_create_table_user.R
│   ├── 04_morbidgenes_db_create-database-tables-in-mysql.R
│   ├── 05_morbidgenes_db_set-table-connections.R
│   ├── README.md
│   ├── config_dummy.yml
│   ├── data/
│   ├── functions/
│   └── results/
└── README.md (You are here)
```


## Files Description

- `01_morbidgenes_table_hgnc_non_alt_loci_set.R`: Responsible for dealing with HGNC data.
- `02_morbidgenes_db_create_tables-from-csv.R`: Creates database tables from CSV files.
- `03_morbidgenes_db_create_table_user.R`: Creates the user table in the MySQL database.
- `04_morbidgenes_db_create-database-tables-in-mysql.R`: Sets up database tables in MySQL.
- `05_morbidgenes_db_set-table-connections.R`: Sets up table connections in the database.


## Database Schema

### Tables

1. **`mg_panel_genes_join`**: Joins `panel_id` and `hgnc_id` to represent genes and panels.
    - `panel_hgnc_id`: Auto-incremented ID (Primary Key)
    - `panel_id`: Panel ID (Foreign Key)
    - `hgnc_id`: HGNC ID of the gene (Foreign Key)

2. **`non_alt_loci_set_coordinates`**: Gene coordinates information.
    - `hgnc_id`: HGNC ID (Primary Key)
    - `symbol`: Gene symbol
    - `name`: Gene name
    - `locus_group`: Locus group
    - `update_date`: Timestamp
    - `is_current`: Current status flag

3. **`mg_panel_version`**: Stores versions of gene panels.
    - `panel_id`: Panel ID (Primary Key)
    - `panel_version`: Version of the panel
    - `upload_user`: User who uploaded the panel

4. **`mg_source`**: Source data information.
    - `source_id`: Source ID (Primary Key)
    - `source_name`: Name of the source
    - `source_description`: Description of the source

5. **`mg_panel_genes_source_join`**: Joins `panel_hgnc_id` and `source_id`.
    - `panel_hgnc_id`: Panel HGNC ID (Foreign Key)
    - `source_id`: Source ID (Foreign Key)

6. **`user`**: User information.
    - `user_id`: User ID (Primary Key)
    - `user_name`: User name
    - `password`: Password
    - `email`: Email
    - `user_role`: Role of the user
    - `orcid`: ORCID ID
    - `created_at`: Account creation date
    - `approved`: Approval flag

### Relationships

- `mg_panel_genes_join` is related to `mg_panel_version` through `panel_id`.
- `mg_panel_genes_join` is related to `non_alt_loci_set_coordinates` through `hgnc_id`.
- `mg_panel_genes_source_join` is related to `mg_panel_genes_join` through `panel_hgnc_id`.
- `mg_panel_genes_source_join` is related to `mg_source` through `source_id`.

### Constraints

- Tables have `PRIMARY KEY` constraints.
- `FOREIGN KEY` constraints are used for integrity.

### Indexes

- Primary keys serve as indexes for faster data retrieval.