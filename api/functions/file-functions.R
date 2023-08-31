#### This file holds file functions

#' Generate an XLSX File and Return its Binary Info
#'
#' @description
#'
#' The `generate_xlsx_bin` function creates a temporary Excel (xlsx) file with
#' three sheets: 'data', 'meta', and 'links', using the provided data object.
#' After creating the file, it reads its binary content and returns this
#' content. The temporary file is deleted after the content is read.
#' The function follows these steps:
#' 
#' 1. Generate a temporary xlsx file path.
#' 2. Write 'data' from the data object to the 'data' sheet.
#' 3. Write 'meta' from the data object to the 'meta' sheet.
#' 4. Write 'links' from the data object to the 'links' sheet.
#' 5. Read the binary content of the generated xlsx file.
#' 6. Delete the temporary xlsx file.
#' 7. Return the binary content.
#'
#' @param data_object A list with 'data', 'meta', and 'links', each a data frame.
#' @param file_base_name String for the Excel file's base name.
#'
#' @return Binary content of the generated xlsx file as a raw vector.
#'
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


#' Check if Basenames of Two Files Match
#'
#' @description
#'
#' The `check_filename_match` function verifies if the basenames (excluding
#' extensions) of two given files match. The first file must have the extension
#' ".csv.gz" and the second one ".yml". If the basenames match, the function
#' returns TRUE. If they don't match, an error is thrown.
#'
#' @param file1 Named list where the name is the path of the gzipped CSV file.
#' @param file2 Named list where the name is the path of the YAML file.
#'
#' @return TRUE if the basenames of the two files match.
#'
#' @examples
#' # With "sample.csv.gz" and "sample.yml" in the directory:
#' check_filename_match(list("path/to/sample.csv.gz" = NULL),
#'                      list("path/to/sample.yml" = NULL))
#'
#' @export
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