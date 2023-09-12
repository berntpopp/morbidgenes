#' Deactivate All Current Panels in the Database
#'
#' @description
#' Connects to a MariaDB database and sets the "is_current" status for all
#' panels in the "mg_panel_version" table to 0 (deactivated).
#'
#' @return A list containing the HTTP status and a message confirming the
#' deactivation of all current panels.
#'
#' @examples
#' result <- put_db_panel_deactivation()
#' print(result)
#'
#' @export
put_db_panel_deactivation <- function() {
  ##-------------------------------------------------------------------##
  # connect to database
  morbidgenes_db <- dbConnect(RMariaDB::MariaDB(),
                              dbname = dw$dbname,
                              user = dw$user,
                              password = dw$password,
                              server = dw$server,
                              host = dw$host,
                              port = dw$port)

  # perform the panel update
  dbExecute(morbidgenes_db, paste0("UPDATE mg_panel_version SET ",
  "is_current = 0",
  " WHERE is_current = 1;")
  )

  # disconnect from database
  dbDisconnect(morbidgenes_db)

  # return OK
  return(list(status = 200,
    message = "OK. Status is_current reset for all panels."))
  ##-------------------------------------------------------------------##
}


#' Activate a Specific Panel in the Database
#'
#' @description
#' Connects to a MariaDB database and sets "is_current" for a specific
#' panel in "mg_panel_version" to 1 (activated).
#'
#' @param panel_id ID of the panel to set as active.
#'
#' @return A list with HTTP status and message confirming panel activation.
#'
#' @examples
#' result <- put_db_panel_activation(123)
#' print(result)
#'
#' @export
put_db_panel_activation <- function(panel_id) {
  ##-------------------------------------------------------------------##
  # Deactivate all panels using the existing function
  deactivation_result <- put_db_panel_deactivation()

  # Check if deactivation was successful
  if (deactivation_result$status != 200) {
    return(
      list(
        status = 500,
        message = "Failed to deactivate current panels."
      )
    )
  }

  # Connect to database for activation
  morbidgenes_db <- dbConnect(
    RMariaDB::MariaDB(),
    dbname = dw$dbname,
    user = dw$user,
    password = dw$password,
    server = dw$server,
    host = dw$host,
    port = dw$port
  )

  # Activate specific panel
  dbExecute(
    morbidgenes_db,
    paste0(
      "UPDATE mg_panel_version SET is_current = 1 WHERE panel_id = ",
      panel_id,
      ";"
    )
  )

  # Disconnect from database
  dbDisconnect(morbidgenes_db)

  # Return OK
  return(
    list(
      status = 200,
      message = paste0("OK. Panel with ID ", panel_id, " is now active.")
    )
  )
  ##-------------------------------------------------------------------##
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