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

  # here we unselect the nested column fspec
  # based on https://stackoverflow.com/questions/43786883/how-do-i-select-columns-that-may-or-may-not-exist
  # TODO: instead of unselecting
  # TODO: we could transform to string for all nested
  write.xlsx(data_object$meta %>%
      select(-any_of(c("fspec"))),
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