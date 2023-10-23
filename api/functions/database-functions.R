#' Deactivate All Current Panels in the Database
#'
#' @description
#' Connects to a MariaDB database and sets the "is_current" status for all
#' panels in the "mg_panel_version" table to 0 (deactivated).
#'
#' @param pool The database connection pool.
#'
#' @return A list containing the HTTP status and a message confirming the
#' deactivation of all current panels.
#'
#' @examples
#' # Assuming pool is a valid database connection pool
#' result <- put_db_panel_deactivation(pool)
#' print(result)
#'
#' @export
put_db_panel_deactivation <- function(pool) {
  ##-------------------------------------------------------------------##

  # perform the panel update
  dbExecute(pool, "UPDATE mg_panel_version SET is_current = 0 WHERE is_current = 1;")

  # return OK
  return(list(status = 200, message = "OK. Status is_current reset for all panels."))
  ##-------------------------------------------------------------------##
}


#' Activate a Specific Panel in the Database
#'
#' @description
#' Connects to a MariaDB database and sets "is_current" for a specific
#' panel in "mg_panel_version" to 1 (activated).
#'
#' @param panel_id ID of the panel to set as active.
#' @param pool The database connection pool.
#'
#' @return A list with HTTP status and message confirming panel activation.
#'
#' @examples
#' # Assuming pool is a valid database connection pool
#' result <- put_db_panel_activation(123, pool)
#' print(result)
#'
#' @export
put_db_panel_activation <- function(panel_id, pool) {
  ##-------------------------------------------------------------------##

  # Deactivate all panels using the existing function
  deactivation_result <- put_db_panel_deactivation(pool)

  # Check if deactivation was successful
  if (deactivation_result$status != 200) {
    return(
      list(
        status = 500,
        message = "Failed to deactivate current panels."
      )
    )
  }

  # Activate specific panel
  dbExecute(
    pool,
    paste0(
      "UPDATE mg_panel_version SET is_current = 1 WHERE panel_id = ",
      panel_id,
      ";"
    )
  )

  # Return OK
  return(
    list(
      status = 200,
      message = paste0("OK. Panel with ID ", panel_id, " is now active.")
    )
  )
  ##-------------------------------------------------------------------##
}


#' Update the `mg_panel_version` Table with New Configurations
#'
#' @description
#' This function updates the `mg_panel_version` table based on the actions specified
#' in the file version tibble. It can add new entries (post), update existing
#' entries (put), and do nothing for "none" actions.
#'
#' @param file_version_tibble A tibble containing file version configurations.
#' @param pool A database connection.
#'
#' @return
#' A list containing two elements:
#' - `status_messages`: A list of status messages and codes for each operation
#'   (post, put, fetch, and overall status).
#' - `updated_table`: A tibble of the updated `mg_panel_version` table.
#'
update_mg_panel_version <- function(file_version_tibble, pool) {

  # Split the tibble based on actions
  post_entries <- filter(file_version_tibble, actions == "post") %>% 
    select(-actions)

  put_entries <- filter(file_version_tibble, actions == "put")

  # Initialize a list to store messages and status codes
  status_messages <- list()

  # Flag to track overall success
  overall_success <- TRUE

  # Update entries with action "post" (New entries)
  if (nrow(post_entries) > 0) {
    tryCatch(
      {
        dbWriteTable(pool, "mg_panel_version", post_entries, append = TRUE)
        status_messages[["post_status"]] <- list(code = 200, message = "New entries added successfully.")
      },
      error = function(e) {
        status_messages[["post_status"]] <- list(code = 500, message = paste("Error adding new entries:", e$message))
        overall_success <- FALSE
      }
    )
  }

  # Update entries with action "put" (Existing entries to be updated)
  if (nrow(put_entries) > 0) {
    tryCatch(
      {
        # Assuming `panel_id` is the primary key for updates
        dbExecute(pool, 
                  "UPDATE mg_panel_version SET panel_version = ?, panel_date = ?, file_path = ?, md5sum_import = ?, is_current = ?, upload_user = ? WHERE panel_id = ?", 
                  list(put_entries$panel_version, put_entries$panel_date, put_entries$file_path, put_entries$md5sum_import, put_entries$is_current, put_entries$upload_user, put_entries$panel_id))
        status_messages[["put_status"]] <- list(code = 200, message = "Existing entries updated successfully.")
      },
      error = function(e) {
        status_messages[["put_status"]] <- list(code = 500, message = paste("Error updating existing entries:", e$message))
        overall_success <- FALSE
      }
    )
  }

  # Fetch the updated mg_panel_version table
  updated_mg_panel_version_table <- NULL
  tryCatch(
    {
      updated_mg_panel_version_table <- pool %>%
        tbl("mg_panel_version") %>%
        collect()
    },
    error = function(e) {
      status_messages[["fetch_status"]] <- list(code = 500, message = paste("Error fetching updated table:", e$message))
      overall_success <- FALSE
    }
  )

  # Set overall status
  if (overall_success) {
    status_messages[["overall_status"]] <- list(code = 200, message = "Operation completed successfully.")
  } else {
    status_messages[["overall_status"]] <- list(code = 500, message = "Operation completed with errors.")
  }

  # Return the status messages and the updated table
  return(list(status_messages = status_messages, updated_table = updated_mg_panel_version_table))
}


#' Update the `mg_source` Table with New Configurations
#'
#' @description
#' This function updates the `mg_source` table based on the actions specified
#' in the source tibble. It can add new entries (post) and update existing
#' entries (put). It returns a list containing status messages for each
#' operation and the updated `mg_source` table.
#'
#' @param source_tibble A tibble containing source configurations with columns:
#' 'source_name', 'source_logic', 'source_id', and 'actions'.
#' @param pool A database connection.
#'
#' @return
#' A list containing two elements:
#' - `status_messages`: A list of status messages and codes for each operation
#'   (post, put, fetch, and overall status).
#' - `updated_table`: A tibble of the updated `mg_source` table.
#'
#' @examples
#' # Assuming a DB connection pool exists and a properly formatted source_tibble
#' result <- update_mg_source(source_tibble, pool)
#' # View the status messages
#' print(result$status_messages)
#' # View the updated table
#' print(result$updated_table)
#'
#' @export
update_mg_source <- function(source_tibble, pool) {

  # Split the tibble based on actions
  post_entries <- filter(source_tibble, actions == "post") %>% 
    select(-source_id, -actions)

  put_entries <- filter(source_tibble, actions == "put")

  # Initialize a list to store messages and status codes
  status_messages <- list()

  # Flag to track overall success
  overall_success <- TRUE

  # Update entries with action "post" (New entries)
  if (nrow(post_entries) > 0) {
    tryCatch(
      {
        dbWriteTable(pool, "mg_source", post_entries,
                     fields = c("source_name", "source_logic"), append = TRUE)
        status_messages[["post_status"]] <- list(code = 200, message = "New entries added successfully.")
      },
      error = function(e) {
        status_messages[["post_status"]] <- list(code = 500, message = paste("Error adding new entries:", e$message))
        overall_success <- FALSE
      }
    )
  }

  # Update entries with action "put" (Existing entries to be updated)
  if (nrow(put_entries) > 0) {
    tryCatch(
      {
        dbExecute(pool, 
                  "UPDATE mg_source SET source_logic = ? WHERE source_id = ?", 
                  list(put_entries$source_logic, put_entries$source_id))
        status_messages[["put_status"]] <- list(code = 200, message = "Existing entries updated successfully.")
      },
      error = function(e) {
        status_messages[["put_status"]] <- list(code = 500, message = paste("Error updating existing entries:", e$message))
        overall_success <- FALSE
      }
    )
  }

  # Fetch the updated mg_source table
  updated_mg_source_table <- NULL
  tryCatch(
    {
      updated_mg_source_table <- pool %>%
        tbl("mg_source") %>%
        collect()
    },
    error = function(e) {
      status_messages[["fetch_status"]] <- list(code = 500, message = paste("Error fetching updated table:", e$message))
      overall_success <- FALSE
    }
  )

  # Set overall status
  if (overall_success) {
    status_messages[["overall_status"]] <- list(code = 200, message = "Operation completed successfully.")
  } else {
    status_messages[["overall_status"]] <- list(code = 500, message = "Operation completed with errors.")
  }

  # Return the status messages and the updated table
  return(list(status_messages = status_messages, updated_table = updated_mg_source_table))
}


#' Update mg_panel_genes_join Table with New Panel Data
#'
#' @description
#' This function takes a panel ID and a long format CSV tibble as inputs, selects unique HGNC ID values, adds the panel ID, and then posts this data to the "mg_panel_genes_join" table in the database.
#'
#' @param panel_id An integer representing the panel ID.
#' @param csv_tibble_long A long format tibble generated from the csv tibble with the help of `convert_panel_to_long_format` function.
#' @param pool The database connection pool.
#'
#' @return 
#' A message indicating the success or failure of the operation.
#'
#' @examples
#' # Assuming pool is a valid database connection pool and csv_tibble_long is the long format tibble
#' update_mg_panel_genes_join(panel_id = 1, csv_tibble_long, pool)
#'
#' @export
update_mg_panel_genes_join <- function(panel_id, csv_tibble_long, pool) {
  # Step 1: Select unique hgnc_id values
  unique_hgnc_ids <- csv_tibble_long %>%
    dplyr::select(hgnc_id) %>%
    dplyr::distinct()

  # Step 2: Add panel_id column
  data_to_post <- unique_hgnc_ids %>%
    dplyr::mutate(panel_id = panel_id)

  # Step 3: Post the data to the database
  tryCatch({
    dbWriteTable(pool, "mg_panel_genes_join", value = data_to_post, append = TRUE, row.names = FALSE)
    message("Data successfully posted to the database.")
    return(list(status_code = 200, message = "Data successfully posted to the database."))
  }, error = function(e) {
    message("Failed to post data to the database: ", e$message)
    return(list(status_code = 500, message = paste("Failed to post data to the database:", e$message)))
  })
}
