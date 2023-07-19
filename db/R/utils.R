# load libraries
library(config)
library(DBI)        # needed for MySQL data export
library(RMariaDB)	# needed for MySQL data export

get_config <- function() {
    # read config.yml values
    return(config::get())
}

get_current_date <- function() {
    result <- strftime  (
                            as.POSIXlt  (
                                            Sys.time(), 
                                            "UTC", 
                                            "%Y-%m-%dT%H:%M:%S"
                                        ), 
                            "%Y-%m-%d"
                        )

    return(result)
}

get_db_connection <- function () {
    config <- get_config();

    db_connection <- DBI::dbConnect(
								RMariaDB::MariaDB(), 
								dbname = config$db_name, 
								user = config$db_user, 
								password = config$db_password, 
								host = config$db_host,
								port = config$db_port
							)

    return(db_connection)
}