#### This file holds file functions

#' Generate an xlsx file and return its binary info
#'
#' @description
#'
#' generate_xlsx_bin is an R function that creates a temporary
#' Excel (xlsx) file with three sheets:'data', 'meta', and 'links',
#' populated with the corresponding data from a given data object.
#' The function then reads the binary content of the generated
#' xlsx file and returns it. The temporary file is deleted
#' once the binary content is read.
#' The function performs the following steps:
#'
#' 1. Generate a temporary xlsx file path.
#' 2. Write the 'data' element of the data object to the 'data' sheet.
#' 3. Write the 'meta' element of the data object to the 'meta' sheet,
#' excluding the 'fspec' column if present.
#' 4. Write the 'links' element of the data object to the 'links' sheet.
#' 5. Read the binary content of the generated xlsx file.
#' 6. Delete the temporary xlsx file.
#' 7. Return the binary content of the file.
#'
#' @param data_object A list containing three elements: 'data', 'meta', and 'links', each containing a data frame to be written to the respective sheets in the output Excel file.
#' @param file_base_name A string representing the base name to be used for the temporary Excel file.
#'
#' @return The binary content of the generated xlsx file as a raw vector
#' @export
generate_xlsx_bin <- function(data_object, file_base_name) {

  # generate excel file output
  xlsx_file <- file.path(tempdir(),
    paste0(file_base_name, ".xlsx"))

  write.xlsx(data_object$data,
    xlsx_file,
    sheetName = "data",
    append = FALSE)

  # Transform all nested columns in meta to string
  meta_transformed <- data_object$meta %>%
      mutate(across(where(is.list), as.character))

  write.xlsx(meta_transformed,
    xlsx_file,
    sheetName = "meta",
    append = TRUE)

  write.xlsx(data_object$links,
    xlsx_file,
    sheetName = "links",
    append = TRUE)

  # Read in the raw contents of the binary file
  bin <- readBin(xlsx_file, "raw", n = file.info(xlsx_file)$size)

  # Check file existence and delete
  if (file.exists(xlsx_file)) {
    file.remove(xlsx_file)
  }

  # return the binary contents
  return(bin)
}


#' Check if the basenames of Two Files Match
#'
#' This function checks if the basenames (excluding extensions) of two provided files match.
#' The first file must have the extension ".csv.gz" and the second file must have the extension ".yml".
#' If the basenames match, the function returns TRUE. If not, it throws an error.
#'
#' @param file1 A named list where the name is the path of the gzipped CSV file.
#' @param file2 A named list where the name is the path of the YAML file.
#'
#' @return Logical TRUE if the basenames of the two files match.
#' @export
#'
#' @examples
#' # Assuming you have two files: "sample.csv.gz" and "sample.yml" in your working directory:
#' check_filename_match(list("path/to/sample.csv.gz" = NULL), list("path/to/sample.yml" = NULL))
#'
check_filename_match <- function(file1, file2) {

  # Extract the basenames
  file1_name <- basename(names(file1))
  file2_name <- basename(names(file2))

  # check if first file has the extension ".csv.gz"
  if (grepl(".csv.gz", file1_name)) {
    base1 <- gsub(".csv.gz", "", file1_name)
  } else {
    stop("First file is not a .csv.gz file!")
  }

  # check if second file has the extension ".yml"
  if (grepl(".yml", file2_name)) {
    base2 <- gsub(".yml", "", file2_name)
  } else {
    stop("Second file is not a .yml file!")
  }

  # Check if the basenames match
  if (base1 == base2) {

    # Return TRUE if the basenames match
    return(TRUE)

  } else {
    stop("The basenames of the files do not match!")
  }
}