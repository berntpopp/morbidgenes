#### This file holds functions to validate data input
## (e.g. against the database state)


#' Compute Panel Metadata
#'
#' @description
#' Computes and returns metadata for a specified panel file. It compares the
#' new panel data with the existing data in the database, determining the
#' necessary actions ("post", "put", or "none") based on this comparison. It
#' also assigns the `is_current` status and `upload_user` as per specified
#' parameters or existing values.
#'
#' @param file_path
#' A string specifying the full path of the file.
#' @param upload_user
#' A string specifying the upload user. Used to assign the `upload_user` value
#' for new or updated records.
#' @param pool
#' A database connection pool object for fetching existing data and performing
#' SQL operations.
#' @param override_is_current
#' Optional logical to override the `is_current` field for new or updated
#' records. Default is FALSE, where the field is determined based on existing
#' data and the new panel date.
#'
#' @return
#' A tibble with metadata, including `panel_id`, `panel_version`, `panel_date`,
#' `file_path`, `md5sum_import`, `is_current`, `upload_user`, and an `actions`
#' column indicating the necessary action ("post", "put", or "none") for each
#' record.
#'
#' @examples
#' # Assuming a database connection pool exists
#' compute_panel_metadata("path/to/file.csv.gz", 1, pool,
#'                        override_is_current = TRUE)
#'
#' @export
compute_panel_metadata <- function(file_path, upload_user_input, pool, override_is_current = FALSE) {
  #TODO: check cases where we override a panel
  # Fetch existing data from the database
  mg_panel_version_table <- pool %>%
    tbl("mg_panel_version") %>%
    collect()

  # Compute metadata from the file name
  file_version <- tibble(value = file_path) %>%
    separate(value, c("path", "file"), sep = "\\/") %>%
    mutate(file_path = paste0(path, "/", file)) %>%
    mutate(file_basename = str_remove_all(file, "\\.csv\\.gz")) %>%
    separate(file_basename, c("analysis", "version"), sep = "_") %>%
    separate(version, c("panel_version", "panel_date"), sep = "-") %>%
    mutate(panel_version = paste0(panel_version, "-", panel_date)) %>%
    mutate(panel_date = as.Date(panel_date, format = "%Y%m%d")) %>%
    mutate(md5sum_import = tools::md5sum(file_path)) %>%
    select(panel_version, panel_date, file_path, md5sum_import)

  # Join new data with existing data to identify new and unchanged records
  joined_data <- left_join(file_version, mg_panel_version_table, by = "panel_version", suffix = c(".new", ".db")) %>%
    mutate(
      panel_date = coalesce(panel_date.new, panel_date.db),
      file_path = coalesce(file_path.new, file_path.db),
      md5sum_import = coalesce(md5sum_import.new, md5sum_import.db)
    ) %>%
    select(panel_id, panel_version, panel_date, file_path, md5sum_import, is_current)

  # Determine the necessary actions and set the is_current field
  # logic to determine is_current:
  # 1. If override_is_current is TRUE, use the submitted panel version
  # 2. If override_is_current is FALSE, determined based on existing data and new panel date
  final_tibble <- bind_rows(mg_panel_version_table, joined_data) %>%
    mutate(
      is_current = case_when(
        !is.na(panel_id) & !override_is_current ~ 0,
        is.na(panel_id) & !override_is_current ~ 1,
        override_is_current & panel_version == file_version$panel_version ~ 1,
      )
    ) %>%
    select(panel_id, panel_version, panel_date, file_path, md5sum_import, is_current, upload_user)

  # Get the data from the database that is unaffected by the new file data
  affected_data <- anti_join(final_tibble, mg_panel_version_table, by = c("panel_version", "is_current")) %>%
    mutate(
      actions = case_when(
        is.na(panel_id) ~ "post",
        !is.na(panel_id) ~ "put"
      ),
      upload_user = case_when(
        actions %in% c("post", "put") ~ upload_user_input
    )
  )

  return(affected_data)
}


#' Correct HGNC IDs and symbols in a Panel Tibble
#'
#' @description
#' This function corrects HGNC IDs and symbols in a given panel tibble.
#'
#' @param panel_tibble A tibble containing the panel data with 'hgnc_id' and 'symbol' columns.
#' @param pool A database connection.
#' @param stringency String specifying the level of stringency. Accepts "strict" or "lenient".
#'
#' @return A tibble with corrected 'hgnc_id' and 'symbol'.
#'
#' @examples
#' # Assuming a database connection pool exists
#' corrected_panel <- correct_hgnc_id_and_symbol(panel_tibble, pool, stringency = "strict")
#'
#' @export
correct_hgnc_id_and_symbol <- function(panel_tibble, pool, stringency = "strict") {
  # Step 1: Add "HGNC:" prefix to hgnc_id if not present
  corrected_tibble <- panel_tibble %>%
    mutate(hgnc_id = ifelse(!str_detect(hgnc_id, "^HGNC:"), paste0("HGNC:", hgnc_id), hgnc_id))

  # Step 2: Query the database for valid hgnc_ids and symbols
  view_genes_hgnc_table <- pool %>%
    tbl("view_genes_hgnc") %>%
    select(hgnc_id, symbol) %>%
    collect()

  # Check if all hgnc_ids and symbols in the panel are valid
  invalid_entries <- anti_join(corrected_tibble, view_genes_hgnc_table, by = c("hgnc_id", "symbol"))

  if (nrow(invalid_entries) > 0) {
    message_text <- paste("Invalid hgnc_id and symbol combinations detected:", nrow(invalid_entries))
    if (stringency == "strict") {
      stop(message_text)
    } else {
      warning(message_text)
    }
  }

  # Step 3: Recompute hgnc_id and symbol for invalid entries
  corrected_tibble <- corrected_tibble %>%
    mutate(hgnc_id = ifelse(hgnc_id %in% invalid_entries$hgnc_id, paste0("HGNC:", hgnc_id_from_symbol_grouped(symbol)), hgnc_id)) %>%
    mutate(symbol = ifelse(symbol %in% invalid_entries$symbol, symbol_from_hgnc_id_grouped(hgnc_id), symbol))

  return(corrected_tibble)
}


#' Check and Update Source Information from a Config Tibble using DB Information
#'
#' @description
#' This function checks and optionally updates the 'source_name' and 
#' 'source_logic' columns in a given config tibble against the database.
#'
#' @param config_tibble A tibble containing 'source_name' and 'source_logic'.
#' @param pool A database connection.
#' @param overwrite_config Optional boolean to control behavior for 'source_logic'.
#'
#' @return 
#' A tibble with updated 'source_id', 'source_name', 'source_logic', and 
#' 'actions'.
#'
#' @examples
#' # Assuming a database connection pool exists
#' updated_config <- check_config_update_source(config_tibble, pool, overwrite_config = TRUE)
#'
#' @export
check_config_update_source <- function(config_tibble, pool, overwrite_config = NULL) {
  # Step 1: Get current mg_source from the database
  mg_source_table <- pool %>%
    tbl("mg_source") %>%
    collect()

  # Step 2 & 3: Check presence and logic of source_name in database
  joined_sources <- left_join(config_tibble, mg_source_table, 
                              by = "source_name", suffix = c(".input", ".db"))

  if (nrow(joined_sources) > 0 && is.null(overwrite_config)) {
    mismatched_logic <- filter(joined_sources, source_logic.input != source_logic.db)
    if (nrow(mismatched_logic) > 0) {
      stop("Mismatched source_logic found and 'overwrite_config' parameter is not set.")
    }
  }

  # Step 4 & 5: Add source_id and control logic overwriting
  final_tibble <- joined_sources %>%
    mutate(
      source_logic = case_when(
        source_logic.input == source_logic.db ~ source_logic.input,
        source_logic.input != source_logic.db & !is.null(overwrite_config) & overwrite_config ~ 
          source_logic.input,
        source_logic.input != source_logic.db & !is.null(overwrite_config) & !overwrite_config ~ 
          source_logic.db,
        TRUE ~ source_logic.input
      ),
      actions = case_when(
        source_logic.input == source_logic.db ~ "none",
        source_logic.input != source_logic.db & !is.null(overwrite_config) & overwrite_config ~ "put",
        is.na(source_id) ~ "post",
        TRUE ~ "none"
      )
    ) %>%
    select(source_name, source_logic, source_id, actions)

  return(final_tibble)
}
