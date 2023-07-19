get_current_date <- function(){
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