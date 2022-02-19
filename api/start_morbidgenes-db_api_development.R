library(plumber)

setwd("C:/development/morbidgenes/api")

root <- pr("morbidgenes-db_plumber.R") %>%
        pr_run(host = "0.0.0.0", port = 10918) %>%
		pr_hook("exit", function(){ poolClose(pool) })