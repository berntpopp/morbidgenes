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
check_filename_match <- function(file1_name, file2_name) {

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


#' Read and Filter corresponding Panel CSV and YAML Files
#'
#' @description
#'
#' The `read_upload_files` function reads a gzipped CSV file and a YAML file.
#' It filters the CSV based on `morbidscore` and selects columns based on
#' the YAML file. The function returns filtered and selected data as tibbles.
#'
#' @param csv_file_path
#' Character string for the path to the gzipped CSV file.
#' @param yaml_file_path
#' Character string for the path to the YAML file.
#' @param stringency
#' String specifying the level of stringency for missing columns.
#' Accepts "strict" or "lenient".
#'
#' @return 
#' A list containing two tibbles: `csv_tibble` with filtered and selected
#' data from the CSV, and `yaml_tibble` with long-format data from the YAML.
#'
#' @examples
#' # With "sample.csv.gz" and "sample.yml" in the directory:
#' result <- read_upload_files("path/to/sample.csv.gz",
#'                             "path/to/sample.yml",
#'                             stringency = "strict")
#' # Access the tibbles
#' csv_data <- result$csv_tibble
#' yaml_data <- result$yaml_tibble
#'
#' @export
read_upload_files <- function(csv_file_path, yaml_file_path, stringency = "strict") {
  # check if file names match
  if (!check_filename_match(csv_file_path, yaml_file_path)) {
    stop("Filenames do not match!")
  }

  # Read YAML file
  # Convert YAML content to tibble
  # Transform YAML tibble to long format
  yaml_tibble_long <- yaml::yaml.load_file(yaml_file_path) %>%
    as_tibble() %>%
    tidyr::pivot_longer(
      cols = everything(), 
      names_to = "source_name", 
      values_to = "source_logic"
    )

  # Read gzipped CSV file into a tibble
  # Filter csv_tibble based on morbidscore
  csv_tibble <- readr::read_delim(csv_file_path,
    delim = ";", escape_double = FALSE, trim_ws = TRUE)

  # Check if 'morbidscore' column exists in CSV
  if (!any(names(csv_tibble) == "morbidscore")) {
    stop("The column 'morbidscore' is missing in the CSV file.")
  }

  # Filter CSV based on morbidscore
  csv_tibble_filtered <- csv_tibble %>%
    filter(morbidscore != 0)

  # Dynamically select columns based on yaml_tibble_long
  additional_cols <- intersect(names(csv_tibble_filtered), yaml_tibble_long$source_name)

  # Check if all source_name columns in YAML are present in CSV
  missing_cols <- setdiff(yaml_tibble_long$source_name, names(csv_tibble_filtered))
  if (length(missing_cols) > 0) {
    missing_cols_str <- paste(missing_cols, collapse = ", ")
    if (stringency == "strict") {
      stop(paste("The following columns are missing in the CSV file:", missing_cols_str))
    } else {
      warning(paste("The following columns are missing in the CSV file and will be excluded:", missing_cols_str))
    }
  }

  # Final column selection and NA replacement
  cols_to_select <- c("symbol", "hgnc_id" = "id_hgnc", additional_cols)
  csv_tibble_selected <- dplyr::select(csv_tibble_filtered, dplyr::all_of(cols_to_select)) %>%
    replace(is.na(.), FALSE)

  # Return list of tibbles
  return(list(csv_tibble = csv_tibble_selected, yaml_tibble = yaml_tibble_long))
}


#' Write Binary Content to File
#'
#' @description
#' The function writes binary content to a file in a specified directory.
#' If the file already exists, it will be overwritten.
#'
#' @param binary_content
#' A list containing the binary content to be written to the file.
#' @param file_name
#' Character string specifying the name of the file to be written.
#' @param dir_name
#' Character string specifying the directory where the file should be written.
#' Defaults to "data".
#'
#' @return
#' Character string specifying the relative path of the written file.
#'
#' @examples
#' write_binary_to_file(binary_content = list("some binary data"),
#'                      file_name = "example_file.csv.gz")
#'
#' @export
write_binary_to_file <- function(binary_content, file_name, dir_name = "data") {

  # Generate full file path
  full_file_path <- file.path(dir_name, file_name)

  # Check if file already exists and remove it if so
  if (file.exists(full_file_path)) {
    file.remove(full_file_path)
  }

  # Write binary content to file
  writeBin(binary_content[[1]], full_file_path)

  return(full_file_path)
}
