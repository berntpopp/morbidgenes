install.packages  (       
                        c(
                              "config",
                              "devtools",
                              "BiocManager", 
                              "DBI", 
                              "jsonlite", 
                              "RCurl",
                              "RMariaDB", 
                              "tidyverse"
                        ),
                        repos="http://cran.us.r-project.org"
                  )

BiocManager::install("biomaRt")
devtools::install_github("berntpopp/sqlr")