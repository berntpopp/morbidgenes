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