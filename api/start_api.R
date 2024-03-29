##-------------------------------------------------------------------##
# load libraries
library(plumber)
library(logger)
library(tictoc)
library(fs)
##-------------------------------------------------------------------##


##-------------------------------------------------------------------##
# set work directory
setwd("/morbidgenes_api_volume")
##-------------------------------------------------------------------##


##-------------------------------------------------------------------##
# Specify how logs are written
log_dir <- "logs"
if (!fs::dir_exists(log_dir)) fs::dir_create(log_dir)
log_appender(appender_file(tempfile("plumber_", log_dir, ".log")))

# TODO: should this be a helper function?
# helper function to handle empty string
convert_empty <- function(string) {
  if (string == "") {
    "-"
  } else {
    string
  }
}
##-------------------------------------------------------------------##


##-------------------------------------------------------------------##
# start the API
root <- pr("morbidgenes-db_plumber.R") %>%
    pr_hook("preroute", function() {
      tictoc::tic()
    }) %>%
    pr_hook("postroute", function(req, res) {
            end <- tictoc::toc(quiet = TRUE)
             # Log details about the request and the response
            log_info(paste0("{convert_empty(req$REMOTE_ADDR)} ",
            "'{convert_empty(req$HTTP_USER_AGENT)}' ",
            "{convert_empty(req$HTTP_HOST)} ",
            "{convert_empty(req$REQUEST_METHOD)} ",
            "{convert_empty(req$PATH_INFO)} ",
            "{convert_empty(req$QUERY_STRING)} ",
            "{convert_empty(req$postBody)} ",
            "{convert_empty(res$status)} ",
            "{round(end$toc - end$tic, digits = getOption('digits', 5))}")
            )
        }) %>%
    pr_set_api_spec(function(spec) {
      spec$components$securitySchemes$bearerAuth$type <- "http"
      spec$components$securitySchemes$bearerAuth$scheme <- "bearer"
      spec$components$securitySchemes$bearerAuth$bearerFormat <- "JWT"
      spec$security[[1]]$bearerAuth <- ""
      ## return spec
      spec
    }) %>%
    pr_run(host = "0.0.0.0", port = 10918)