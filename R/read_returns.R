#' Read a single nursery survey return from a spreadsheet.
#'
#' @return Tibble with disaggregated country and genetically improved stock variables.
#'
#' @importFrom purrr map_dfr
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate select recode
#'
#' @export
#'
read_return <- function(file_path) {
  invisible(readxl::read_xlsx(path = file_path, sheet = "return")) |>
    with_pivot("country_sold_to", "volume_thousand",
               function (x) {
                 x |> dplyr::mutate("E&W" = GB - Scotland) |>
                   dplyr::select(!GB)
               }) |>
    with_pivot("stock_type", "volume_thousand",
               function (x) {
                 x |> dplyr::mutate("Non-GI" = Total - GI) |>
                   dplyr::select(!Total)
               }) |>
    dplyr::mutate(
      gi = recode(stock_type, "GI" = TRUE, "Non-GI" = FALSE),
      year = as.integer(year),
      nursery_ref = as.integer(nursery_ref),
      volume = volume_thousand * 1000,
      .keep = "unused"
    )
}

#' Read all descendant spreadsheets in a folder and bind the reports together.
#'
#' @author Dan Braby
#'
#' @description
#' Reads in all Nursery Survey returns in a specified spreadsheet and compiles into a single dataframe
#'
#' @param dir_path Path to search for spreadsheets.
#'
#' @importFrom purrr map_dfr
#'
#' @return Combined tibble of reports.
#' @export
#'
read_returns <- function(dir_path) {
  dir_path |>
    files_matching("\\.xlsx$") |>
    purrr::map_dfr(read_return)
}


