#' Read a CSV with nursery_names and nursery_ref columns.
#'
#' @author Daniel Braby
#'
#' @param path Path to the CSV file.
#'
#' @importFrom readr read_csv
#'
#' @return Tibble with correct column types.
#' @export


read_nursery_names <- function(path) {
  path %>% readr::read_csv()
}
