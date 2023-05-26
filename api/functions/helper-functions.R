#### This file holds helper functions

# generate sort expressions to parse
generate_sort_expressions <- function(sort_string, unique_id = "hgnc_id") {

  # split the sort input by comma and compute
  # directions based on presence of + or - in front of the string
  sort_tibble <- as_tibble(str_split(
          str_replace_all(sort_string, fixed(" "), ""), ",")[[1]]) %>%
        select(column = value) %>%
        mutate(direction = case_when(
            str_sub(column, 1, 1) == "+" ~ "asc",
            str_sub(column, 1, 1) == "-" ~ "desc",
            TRUE ~ "asc",
        )) %>%
        mutate(column = case_when(
            str_sub(column, 1, 1) == "+" ~ str_sub(column, 2, -1),
            str_sub(column, 1, 1) == "-" ~ str_sub(column, 2, -1),
            TRUE ~ column,
        )) %>%        mutate(exprs = case_when(
            direction == "asc" ~ column,
            direction == "desc" ~ paste0("desc(", column, ")"),
        )) %>%
    unique %>%
    group_by(column) %>%
    mutate(count = n())

    sort_list <- sort_tibble$exprs

    # and check if hgnc_id is in the resulting list,
    # if not append to the list for unique sorting
    if (!(unique_id %in% sort_list ||
        paste0("desc(", unique_id, ")") %in% sort_list)) {
      sort_list <- append(sort_list, unique_id)
    }

    return(sort_list)
}


# generate filter expressions to parse
# semantics according to https://www.jsonapi.net/usage/reading/filtering.html
# currently only implemented "Equality" and "Contains text"
# TODO: need to implement error handling
# TODO: need to implement whether the respective columns exist
# TODO: need to implement allowed Operations as input argument
# TODO: need to implement column type handling
generate_filter_expressions <- function(filter_string,
    operations_allowed =
    "equals,contains,any,all,lessThan,greaterThan,lessOrEqual,greaterOrEqual") {

  # define supported operations
  operations_supported <- paste0("equals,contains,any,all,",
      "lessThan,greaterThan,lessOrEqual,greaterOrEqual,and,or,not") %>%
    str_split(pattern = ",", simplify = TRUE) %>%
    str_replace_all(" ", "") %>%
    unique()

  # define supported logic
  logic_supported <- "and,or,not" %>%
    str_split(pattern = ",", simplify = TRUE) %>%
    str_replace_all(" ", "") %>%
    unique()

  # transform submitted operations to list
  operations_allowed <- URLdecode(operations_allowed) %>%
    str_split(pattern = ",", simplify = TRUE) %>%
    str_replace_all(" ", "") %>%
    unique()

  filter_string <- URLdecode(filter_string) %>%
    str_trim()

  logical_operator <- stringr::str_extract(string = filter_string,
      pattern = ".+?\\(") %>%
    stringr::str_remove_all("\\(")

  if (logical_operator %in% logic_supported) {
    filter_string <- filter_string %>%
      stringr::str_extract(pattern = "(?<=\\().*(?=\\))")
  } else {
    logical_operator <- "and"
  }

  # check if requested operations are supported, if not throw error
  if (all(operations_allowed %in% operations_supported)) {
    if (filter_string != "") {

      # generate tibble from expressions
      filter_string_tibble <- as_tibble(str_split(str_squish(filter_string),
          "\\),")[[1]]) %>%
        separate(value, c("logic", "column_value"), sep = "\\(") %>%
        separate(column_value, c("column", "filter_value"),
          sep = "\\,",
          extra = "merge") %>%
        mutate(filter_value = str_remove_all(filter_value, "'|\\)"))

      # check if hash is in filter expression
      filter_string_hash <- filter_string_tibble %>%
        filter(str_detect(column, "hash"))

      filter_string_has_hash <- (nrow(filter_string_hash) >= 1)

      if (filter_string_has_hash) {
        # check if hash is present in database
        table_hash_filter <- pool %>%
          tbl("table_hash") %>%
          collect() %>%
          filter(hash_256 == filter_string_hash$filter_value[1])

        hash_found <- (nrow(table_hash_filter) == 1)
      }

      # compute filter expressions if hash keyword IS found
      if (filter_string_has_hash && hash_found) {
        table_hash_filter_value <- fromJSON(table_hash_filter$json_text)

        filter_list <- paste0(
          colnames(table_hash_filter_value),
          " %in% c('",
          str_c(as.list(table_hash_filter_value)[[1]], collapse = "','"),
          "')")

      } else if (filter_string_has_hash && !hash_found) {
        stop("Hash not found.")
      }  else {
      # compute filter expressions if hash keyword NOT found
      filter_tibble <- filter_string_tibble %>%
        filter(!str_detect(column, "hash"))  %>%
        mutate(exprs = case_when(
      ## logic for contains based on regex
          column == "any" & logic == "contains" ~
            paste0("if_any(everything(), ~str_detect(.x, '",
              filter_value, "'))"),
          column == "all" & logic == "contains" ~
            paste0("if_all(everything(), ~str_detect(.x, '",
              filter_value, "'))"),
          !(column %in% c("all", "any")) & logic == "contains" ~
            paste0("str_detect(", column, ", '", filter_value, "')"),
      ## logic for equals based on regex
          column == "any" & logic == "equals" ~
            paste0("if_any(everything(), ~str_detect(.x, '^",
              filter_value, "$'))"),
          column == "all" & logic == "equals" ~
            paste0("if_all(everything(), ~str_detect(.x, '^",
              filter_value, "$'))"),
          !(column %in% c("all", "any")) & logic == "equals" ~
            paste0("str_detect(", column, ", '^", filter_value, "$')"),
      ## logic for any based on regex
          column == "any" & logic == "any" ~
            paste0("if_any(everything(), ~str_detect(.x, ",
              str_replace_all(paste0("'", filter_value, "')"),
                pattern = "\\,",
                replacement = "|"), ")"),
          column == "all" & logic == "any" ~
            paste0("if_all(everything(), ~str_detect(.x, ",
              str_replace_all(paste0("'", filter_value, "')"),
                pattern = "\\,",
                replacement = "|"), ")"),
          !(column %in% c("all", "any")) & logic == "any" ~
            paste0("str_detect(", column, ", ",
              str_replace_all(paste0("'",
                filter_value, "')"),
                pattern = "\\,",
                replacement = "|")),
      ## logic for all based on regex
          column == "any" & logic == "all" ~
            paste0("if_any(everything(), ~str_detect(.x, ",
              str_replace_all(paste0("'(?=.*", filter_value, ")')"),
                pattern = "\\,",
                replacement = ")(?=.*"), ")"),
          column == "all" & logic == "all" ~
            paste0("if_all(everything(), ~str_detect(.x, ",
              str_replace_all(paste0("'(?=.*", filter_value, ")')"),
                pattern = "\\,",
                replacement = ")(?=.*"), ")"),
          !(column %in% c("all", "any")) & logic == "all" ~
            paste0("str_detect(", column, ", ",
              str_replace_all(paste0("'(?=.*",
                filter_value, ")')"),
                pattern = "\\,",
                replacement = ")(?=.*")),
      ## logic for Less than
          column == "any" & logic == "lessThan" ~
            paste0("if_any(everything(), .x < '", filter_value, "'"),
          column == "all" & logic == "lessThan" ~
            paste0("if_any(everything(), .x < '", filter_value, "'"),
          !(column %in% c("all", "any")) & logic == "lessThan" ~
            paste0(column, " < '", filter_value, "'"),
      ## logic for Greater than
          column == "any" & logic == "greaterThan" ~
            paste0("if_any(everything(), .x > '", filter_value, "'"),
          column == "all" & logic == "greaterThan" ~
            paste0("if_any(everything(), .x > '", filter_value, "'"),
          !(column %in% c("all", "any")) & logic == "greaterThan" ~
            paste0(column, " > '", filter_value, "'"),
      ## logic for Less than or equal to
          column == "any" & logic == "lessOrEqual" ~
            paste0("if_any(everything(), .x <= '", filter_value, "'"),
          column == "all" & logic == "lessOrEqual" ~
            paste0("if_any(everything(), .x <= '", filter_value, "'"),
          !(column %in% c("all", "any")) & logic == "lessOrEqual" ~
            paste0(column, " <= '", filter_value, "'"),
      ## logic for Greater than or equal to
          column == "any" & logic == "greaterOrEqual" ~
            paste0("if_any(everything(), .x >= '", filter_value, "'"),
          column == "all" & logic == "greaterOrEqual" ~
            paste0("if_any(everything(), .x >= '", filter_value, "'"),
          !(column %in% c("all", "any")) & logic == "greaterOrEqual" ~
            paste0(column, " >= '", filter_value, "'"),
        )) %>%
      ## remove non fitting values
        filter(logic %in% operations_allowed) %>%
        filter(!is.na(exprs))

      ## generate a list of filters
      filter_list <- filter_tibble$exprs
      }

      # compute filter string based on input logic
      if (logical_operator == "and") {
        filter_expression <- stringr::str_c(filter_list, collapse = " & ")
      } else if (logical_operator == "or") {
        filter_expression <- stringr::str_c(filter_list, collapse = " | ")
      } else if (logical_operator == "not") {
        filter_expression <- paste0("!( ",
          stringr::str_c(filter_list, collapse = " | "),
          " )")
      }

      return(filter_expression)
    } else {
      return(filter_string)
    }
  } else {
    stop("Some requested operations are not supported.")
  }
}


# select requested fields from tibble
select_tibble_fields <- function(selection_tibble,
  fields_requested,
  unique_id = "hgnc_id") {

  # get column names from selection_tibble
  tibble_colnames <- colnames(selection_tibble)

  # check if fields_requested is empty string,
  # if so assign tibble_colnames to it, else
  # split the fields_requested input by comma
  if (fields_requested != "") {
    fields_requested <- str_split(str_replace_all(
      fields_requested, fixed(" "), ""), ",")[[1]]
  } else {
    fields_requested <- tibble_colnames
  }

  # check if unique_id variable is in the column names,
  # if not prepend to the list for unique sorting
  if (!(unique_id %in% fields_requested)) {
    fields_requested <- purrr::prepend(fields_requested, unique_id)
    fields_requested <- Filter(function(x) !identical("", x), fields_requested)
  }

  # check if requested column names exist in tibble, if error
  if (all(fields_requested %in% tibble_colnames)) {
    selection_tibble <- selection_tibble %>%
    select(all_of(fields_requested))
  } else {
    stop("Some requested fields are not in the column names.")
  }
    return(selection_tibble)
}


# generate cursor pagination information from a tibble
generate_cursor_pag_inf <- function(pagination_tibble,
  page_size = "all",
  page_after = 0,
  pagination_identifier = "hgnc_id") {

  # get number of rows in filtered ndd_entity_view
  pagination_tibble_rows <- (pagination_tibble %>%
    summarize(n = n()))$n

  # check if page_size is either "all" or
  # a valid integer and convert or assign values accordingly
  if (page_size == "all") {
    page_after <- 0
    page_size <- pagination_tibble_rows
    page_count <- ceiling(pagination_tibble_rows / page_size)
  } else if (is.numeric(as.integer(page_size))) {
    page_size <- as.integer(page_size)
    page_count <- ceiling(pagination_tibble_rows / page_size)
  } else {
    stop("Page size provided is not numeric or all.")
  }

  # find the current row of the requested page_after entry
  page_after_row <- (pagination_tibble %>%
    mutate(row = row_number()) %>%
    filter(!!sym(pagination_identifier) == page_after)
    )$row

  if (length(page_after_row) == 0) {
    page_after_row <- 0
    page_after_row_next <- (pagination_tibble %>%
      filter(row_number() == page_after_row + page_size + 1) %>%
      select(!!sym(pagination_identifier)))[[1]]
  } else {
    page_after_row_next <- (pagination_tibble %>%
      filter(row_number() == page_after_row + page_size) %>%
      select(!!sym(pagination_identifier)))[[1]]
  }

  # find next and prev item row
  page_after_row_prev <- (pagination_tibble %>%
    filter(row_number() == page_after_row - page_size) %>%
      select(!!sym(pagination_identifier)))[[1]]
  page_after_row_last <- (pagination_tibble %>%
    filter(row_number() == page_size * (page_count - 1)) %>%
      select(!!sym(pagination_identifier)))[[1]]

  # filter by row
  pagination_tibble <- pagination_tibble %>%
    filter((row_number() > page_after_row) &
      (row_number() <= page_after_row + page_size))

  # generate links for self, next and prev pages
  self <- paste0("&page_after=", page_after, "&page_size=", page_size)
  if (length(page_after_row_prev) == 0) {
    prev <- "null"
  } else {
    prev <- paste0("&page_after=",
      page_after_row_prev,
      "&page_size=",
      page_size)
  }

  if (length(page_after_row_next) == 0) {
    `next` <- "null"
  } else {
    `next` <- paste0("&page_after=",
      page_after_row_next,
      "&page_size=",
      page_size)
  }

  if (length(page_after_row_last) == 0) {
    last <- "null"
  } else {
    last <- paste0("&page_after=",
      page_after_row_last,
      "&page_size=",
      page_size)
  }

  # generate links object
  links <- as_tibble(list("prev" = prev,
    "self" = self,
    "next" = `next`,
    "last" = last))

  # generate meta object
  meta <- as_tibble(list("perPage" = page_size,
    "currentPage" = ceiling((page_after_row + 1) / page_size),
    "totalPages" = page_count,
    "prevItemID" = (if (length(page_after_row_prev) == 0) {
      "null"
      } else {
        page_after_row_prev
        }),
    "currentItemID" = page_after,
    "nextItemID" = (if (length(page_after_row_next) == 0) {
      "null"
      } else {
        page_after_row_next
        }),
    "lastItemID" = (if (length(page_after_row_last) == 0) {
      "null"
      } else {
        page_after_row_last
        }),
    "totalItems" = pagination_tibble_rows)
  )

  # generate return list
  return_data <- list(links = links, meta = meta, data = pagination_tibble)

  return(return_data)
}


# generate field specs from a tibble
generate_tibble_fspec <- function(field_tibble, fspecInput) {

    # get column names from field_tibble
    tibble_colnames <- colnames(field_tibble)

    # check if fspecInput is empty string,
    # if so assign tibble_colnames to it, else
    # split the fields_requested input by comma
    if (fspecInput != "") {
      fspecInput <- str_split(str_replace_all(
        fspecInput, fixed(" "), ""), ",")[[1]]
    } else {
      fspecInput <- tibble_colnames
    }

    # generate fields object
    fields_values <- field_tibble %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(everything(),
        names_to = "key",
        values_to = "values",
        values_ptypes = list(values = character())) %>%
      arrange(key, values) %>%
      unique() %>%
      group_by(key) %>%
      summarize(selectOptions = list(values)) %>%
      mutate(count = lengths(selectOptions)) %>%
      mutate(filterable = case_when(
        count > 10 ~ TRUE,
        count <= 10 ~ FALSE,
      )) %>%
      mutate(multi_selectable = case_when(
        count <= 10 & count > 2 ~ TRUE,
        TRUE ~ FALSE,
      )) %>%
      mutate(selectable = case_when(
        count <= 2 ~ TRUE,
        TRUE ~ FALSE,
      )) %>%
      mutate(selectOptions = case_when(
        count > 10 ~ list("null"),
        count <= 10 ~ selectOptions,
      )) %>%
      mutate(sortDirection = "asc") %>%
      mutate(sortable = TRUE) %>%
      mutate(class = "text-left") %>%
      mutate(label = str_to_sentence(str_replace_all(key, "_", " "))) %>%
      filter(key %in% fspecInput) %>%
      arrange(factor(key, levels = fspecInput)) %>%
      {if ("details" %in% fspecInput)
        add_row(., key = "details",
          selectOptions = NULL,
          filterable = FALSE,
          selectable = FALSE,
          multi_selectable = FALSE,
          sortable = FALSE,
          sortDirection = "asc",
          class = "text-center",
          label = "Details")
      else .
      }

  # generate return list
  return_data <- list(fspec = fields_values)

  return(return_data)
}


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