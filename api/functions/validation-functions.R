#### This file holds functions to validate data input
## (e.g. against the database state)


#' Compute Panel Metadata
#'
#' @description
#' This function computes and returns metadata for a given panel file.
#'
#' @param file_path 
#' Character string specifying the full path of the file.
#' @param upload_user 
#' Character string specifying the upload user.
#' @param pool
#' A database connection.
#' @param override_is_current
#' Optional logical value to override the is_current field. Default is NULL.
#'
#' @return
#' A tibble containing metadata including panel_version, panel_date, file_path,
#' md5sum_import, is_current, and upload_user.
#'
#' @examples
#' # Assuming a database connection pool exists
#' compute_panel_metadata("path/to/file.csv.gz", "user1", pool, override_is_current = TRUE)
#'
#' @export
compute_panel_metadata <- function(file_path, upload_user, pool, override_is_current = NULL) {

  # Fetch existing data from the database
  mg_panel_version_table <- pool %>%
    tbl("mg_panel_version") %>%
    collect() %>%
    select(-panel_id, -upload_user)

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
    select(panel_version, panel_date, file_path, md5sum_import) %>%
    filter(str_detect(file_path, "MorbidGenesPanel"))

  # Check for existing versions in the database
  if (file_version$panel_version %in% mg_panel_version_table$panel_version) {
    stop(paste0("File with panel version ", file_version$panel_version, " already in database."))
  } else {
    # Determine if the file is current
    file_version_current <- bind_rows(mg_panel_version_table, file_version) %>%
      mutate(is_current = ifelse(!is.null(override_is_current), override_is_current, (max(panel_date) == panel_date))) %>%
      filter(panel_version == file_version$panel_version)
  }

  # Add upload_user to the tibble
  file_version_current_user <- file_version_current %>%
    mutate(upload_user = upload_user)

  return(file_version_current_user)
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