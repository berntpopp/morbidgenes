# MorbidGenes API

## Overview

This directory contains the backend API for the MorbidGenes project, implemented using R and the Plumber package.


## Getting Started

1. Build the Docker image using the provided Dockerfile, or install the R dependencies listed below.
2. Run `start_api.R` to start the API.


## Library Dependencies

- `plumber`: For setting up the REST API.
- `tidyverse`: For data manipulation and visualization.
- `DBI` and `RMariaDB`: For database interaction.
- `jsonlite`: For JSON data handling.
- `config`: For configuration management.
- `lubridate`: For date-time manipulation.
- `cowplot`: For advanced plots.
- `jose`: For JSON Web Tokens.
- `RCurl`: For URL-based data access.
- `stringdist`: For string distance computations.
- `xlsx`: For Excel file operations.
- `easyPubMed`: For PubMed data retrieval.
- `rvest`: For web scraping.
- `pool`: For database connections.
- `memoise`: For memoization.
- `coop`: For cooperative multitasking.
- `keyring`: For secure key storage.
- `rlang`: For tidy evaluation.
- `tools`: For additional utilities like `md5sum`.


## API Endpoint Details

### Panel Endpoints

#### `GET /api/panel`

- **Purpose**: To filter and select fields from all panels.
- **Parameters**: `sort, filter, fields, page_after, page_size, fspec, format`
- **Response**: Cursor pagination object with links, meta information, and gene objects.

#### `GET /api/panel/<hgnc_id>`

- **Purpose**: To retrieve panel information by HGNC ID.
- **Parameters**: `hgnc_id`
- **Response**: Panel information for the specified gene.

### Upload Endpoints

#### `POST /api/upload/panel`

- **Purpose**: To upload a gene list file.
- **Parameters**: `panel_file, config_file`
- **Response**: Status and messages based on the upload and processing.

### Authentication Endpoints

#### `GET /api/auth/authenticate`

- **Purpose**: User login.
- **Parameters**: `user_name, password`
- **Response**: JWT token or error message.

#### `GET /api/auth/signin`

- **Purpose**: User authentication.
- **Response**: User details or error message.

#### `GET /api/auth/refresh`

- **Purpose**: Refresh authentication.
- **Response**: New JWT token or error message.


## Directory Structure

```
- api/
  - .gitignore
  - Dockerfile
  - README.md
  - data/
    - ... (data files)
  - functions/
    - ... (R functions)
  - morbidgenes-db_plumber.R
  - start_api.R
```