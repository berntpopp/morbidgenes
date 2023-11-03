#### This file holds functions to for user authentication

#' Authenticate User Credentials
#'
#' This function checks if the provided username and password correspond to
#' an approved user in the database. If the user is found and approved, their
#' record is returned without the password field.
#'
#' @param username The username provided for authentication.
#' @param password The password provided for authentication.
#' @param pool The database connection pool object.
#'
#' @return A dataframe with user details if authentication is successful; 
#' otherwise, NULL.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a database connection pool named 'db_pool'
#' authenticated_user <- authenticate_user("johndoe", "password123", db_pool)
#' if (!is.null(authenticated_user)) {
#'   print("Authentication successful!")
#'   print(authenticated_user)
#' } else {
#'   print("Authentication failed.")
#' }
#' }
#'
#' @export
#'
#' @import dplyr
#' @importFrom DBI collect
authenticate_user <- function(username, password, pool) {
  # Retrieve user from the database
  user_record <- pool %>%
    tbl("user") %>%
    filter(user_name == username, password == password, approved == 1) %>%
    select(-password) %>% # It's a good practice to not include the password in the results
    collect()

  if (nrow(user_record) == 1) {
    # Return the user record if authentication is successful
    return(user_record)
  } else {
    # Return NULL if authentication fails
    return(NULL)
  }
}
