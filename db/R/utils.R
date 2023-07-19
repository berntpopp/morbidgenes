# load libraries
library(config)

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

}