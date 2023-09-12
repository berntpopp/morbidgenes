#### This file holds analyses functions for the hgnc standardization

#' Retrieve HGNC ID from Previous Symbol
#'
#' @description
#' Retrieves the HGNC ID for a provided previous symbol.
#'
#' @param symbol_input The prior symbol to retrieve the HGNC ID for.
#'
#' @return Integer HGNC ID for the input previous symbol.
#'
#' @examples
#' hgnc_id_from_prevsymbol("lysine (K)-specific methyltransferase 2B")
#'
#' @export
hgnc_id_from_prevsymbol <- function(symbol_input) {
  symbol_request <- fromJSON(paste0("http://rest.genenames.org/search/prev_symbol/", symbol_input))

  hgnc_id_from_symbol <- as_tibble(symbol_request$response$docs)

  hgnc_id_from_symbol <- hgnc_id_from_symbol %>%
  mutate(hgnc_id = if (exists('hgnc_id', where = hgnc_id_from_symbol)) hgnc_id else NA) %>%
  mutate(symbol = if (exists('symbol', where = hgnc_id_from_symbol)) symbol else "") %>%
  mutate(score = if (exists('score', where = hgnc_id_from_symbol)) score else 0) %>%
  arrange(desc(score)) %>%
  mutate(hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2]))

  return(as.integer(hgnc_id_from_symbol$hgnc_id[1]))
}


#' Retrieve HGNC ID from Alias Symbol
#'
#' @description
#' Retrieves the HGNC ID for a provided alias symbol.
#'
#' @param symbol_input Alias symbol to retrieve the HGNC ID for.
#'
#' @return Integer HGNC ID for the input alias symbol.
#'
#' @examples
#' hgnc_id_from_aliassymbol("MLL2")
#'
#' @export
hgnc_id_from_aliassymbol <- function(symbol_input) {
  symbol_request <- fromJSON(paste0("http://rest.genenames.org/search/alias_symbol/", symbol_input))

  hgnc_id_from_symbol <- as_tibble(symbol_request$response$docs)

  hgnc_id_from_symbol <- hgnc_id_from_symbol %>%
  mutate(hgnc_id = if (exists('hgnc_id', where = hgnc_id_from_symbol)) hgnc_id else NA) %>%
  mutate(symbol = if (exists('symbol', where = hgnc_id_from_symbol)) symbol else "") %>%
  mutate(score = if (exists('score', where = hgnc_id_from_symbol)) score else 0) %>%
  arrange(desc(score)) %>%
  mutate(hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2]))

  return(as.integer(hgnc_id_from_symbol$hgnc_id[1]))
}


#' Retrieve HGNC ID from Symbol
#'
#' @description
#' Retrieves the HGNC ID for a given symbol or multiple symbols.
#'
#' @param symbol_tibble A tibble of symbols to get HGNC IDs for.
#'
#' @return A tibble of HGNC IDs for the input symbols.
#'
#' @examples
#' symbol_tibble <- tibble(value = c("symbol1", "symbol2", "symbol3"))
#' hgnc_id_from_symbol(symbol_tibble)
#'
#' @export
hgnc_id_from_symbol <- function(symbol_tibble) {
  symbol_list_tibble <- as_tibble(symbol_tibble) %>% dplyr::select(symbol = value) %>% mutate(symbol = toupper(symbol))

  symbol_request <- fromJSON(paste0("http://rest.genenames.org/search/symbol/", str_c(symbol_list_tibble$symbol, collapse = "+OR+")))

  hgnc_id_from_symbol <- as_tibble(symbol_request$response$docs)

  hgnc_id_from_symbol <- hgnc_id_from_symbol %>%
  mutate(hgnc_id = if (exists('hgnc_id', where = hgnc_id_from_symbol)) hgnc_id else NA) %>%
  mutate(symbol = if (exists('symbol', where = hgnc_id_from_symbol)) toupper(symbol) else "") %>%
  mutate(hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2]))

  return_tibble <- symbol_list_tibble %>% 
  left_join(hgnc_id_from_symbol, by = "symbol") %>%
  dplyr::select(hgnc_id)

  return(return_tibble)
}


#' Parallelized Function to Retrieve HGNC ID from Symbol
#'
#' @description
#' Retrieves the HGNC ID for symbols in parallel using grouped requests.
#'
#' @param input_tibble A tibble of symbols to get HGNC IDs for.
#' @param request_max Max symbols per grouped request (default: 150).
#'
#' @return Vector of HGNC IDs for the input symbols.
#'
#' @examples
#' input_tibble <- tibble(value = c("ARID1B", "GRIN2B", "NAA10"))
#' hgnc_id_from_symbol_grouped(input_tibble)
#'
#' @export
hgnc_id_from_symbol_grouped <- function(input_tibble, request_max = 150) {
  input_tibble <- as_tibble(input_tibble)

  row_number <- nrow(input_tibble)
  groups_number <- ceiling(row_number/request_max)

  input_tibble_request <- input_tibble %>%
  mutate(group = sample(1:groups_number, row_number, replace=T)) %>%
  group_by(group) %>%
  mutate(response = hgnc_id_from_symbol(value)$hgnc_id) %>%
  ungroup()

  input_tibble_request_repair <- input_tibble_request %>%
  filter(is.na(response)) %>%
  dplyr::select(value) %>%
  unique() %>%
  rowwise() %>%
  mutate(response = hgnc_id_from_prevsymbol(value)) %>%
  mutate(response = case_when(!is.na(response) ~ response, is.na(response) ~ hgnc_id_from_aliassymbol(value)))

  input_tibble_request <- input_tibble_request %>%
  left_join(input_tibble_request_repair, by = "value") %>%
  mutate(response = case_when(!is.na(response.x) ~ response.x, is.na(response.x) ~ response.y))

  return(input_tibble_request$response)
}


#' Retrieve Symbol from HGNC ID
#'
#' @description
#' Retrieves the symbol for a provided HGNC ID or multiple HGNC IDs.
#' Supports both integer HGNC IDs (e.g., 23336) and prefixed HGNC IDs (e.g., "HGNC:23336").
#'
#' @param hgnc_id_tibble A tibble of HGNC IDs to get symbols for.
#'
#' @return A tibble of symbols for the input HGNC IDs.
#'
#' @examples
#' hgnc_id_tibble <- tibble(value = c(123, 456, 789))
#' symbol_from_hgnc_id(hgnc_id_tibble)
#' hgnc_id_tibble_with_prefix <- tibble(value = c("HGNC:123", "HGNC:456", "HGNC:789"))
#' symbol_from_hgnc_id(hgnc_id_tibble_with_prefix)
#'
#' @export
symbol_from_hgnc_id <- function(hgnc_id_tibble) {
  hgnc_id_list_tibble <- as_tibble(hgnc_id_tibble) %>%
    dplyr::select(hgnc_id = value) %>%
    mutate(hgnc_id = str_replace(hgnc_id, "HGNC:", "")) %>%
    mutate(hgnc_id = as.integer(hgnc_id))

  hgnc_id_request <- jsonlite::fromJSON(paste0("http://rest.genenames.org/search/hgnc_id/", str_c(hgnc_id_list_tibble$hgnc_id, collapse = "+OR+")))

  hgnc_id_from_hgnc_id <- as_tibble(hgnc_id_request$response$docs)

  hgnc_id_from_hgnc_id <- hgnc_id_from_hgnc_id %>%
    mutate(hgnc_id = ifelse(exists('hgnc_id', where = hgnc_id_from_hgnc_id), toupper(hgnc_id), NA)) %>%
    mutate(hgnc_id = as.integer(str_split_fixed(hgnc_id, ":", 2)[, 2]))

  return_tibble <- hgnc_id_list_tibble %>% 
    left_join(hgnc_id_from_hgnc_id, by = "hgnc_id") %>%
    dplyr::select(symbol)

  return(return_tibble)
}



#' Parallelized Function to Retrieve Symbol from HGNC ID
#'
#' @description
#' Retrieves symbols for HGNC IDs in parallel using grouped requests.
#'
#' @param input_tibble A tibble of HGNC IDs to get symbols for.
#' @param request_max Max HGNC IDs per grouped request (default: 150).
#'
#' @return Vector of symbols for the input HGNC IDs.
#'
#' @examples
#' input_tibble <- tibble(value = c(123, 456, 789))
#' symbol_from_hgnc_id_grouped(input_tibble)
#'
#' @export
symbol_from_hgnc_id_grouped <- function(input_tibble, request_max = 150) {
  input_tibble <- as_tibble(input_tibble)

  row_number <- nrow(input_tibble)
  groups_number <- ceiling(row_number/request_max)

  input_tibble_request <- input_tibble %>%
    mutate(group = sample(1:groups_number, row_number, replace=T)) %>%
    group_by(group) %>%
    mutate(response = symbol_from_hgnc_id(value)$symbol) %>%
    ungroup()

  return(input_tibble_request$response)
}