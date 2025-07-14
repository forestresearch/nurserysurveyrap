#' Read Standardized Nursery Names
#'
#' Reads a CSV file containing standardised nursery names and reference numbers.
#' This reference data ensures consistent nursery naming across survey years and
#' corrects for variations in how nurseries report their names.
#'
#' @param path Character. File path to the CSV file containing nursery reference data.
#'   Expected to have columns: nursery_ref, nursery_name.
#'
#' @details
#' The nursery names file serves as a master reference for:
#' \itemize{
#'   \item **Consistency**: Standardises nursery names across multiple survey years
#'   \item **Correction**: Handles variations in self-reported nursery names
#'   \item **Reference**: Links nursery_ref numbers to official names
#'   \item **Quality**: Ensures data integrity in time series analysis
#' }
#'
#' Expected CSV structure:
#' \itemize{
#'   \item \code{nursery_ref} - Integer, unique nursery reference number
#'   \item \code{nursery_name} - Character, standardized nursery name
#'   \item No header requirements beyond these core columns
#' }
#'
#' @return A tibble with standardized nursery reference data containing:
#' \itemize{
#'   \item \code{nursery_ref} - Integer, nursery reference number
#'   \item \code{nursery_name} - Character, standardized nursery name
#'   \item Additional columns if present in the CSV file
#' }
#'
#' @importFrom readr read_csv
#'
#' @examples
#' \dontrun{
#' # Read nursery names reference data
#' nursery_names <- read_nursery_names("data/nursery_names.csv")
#'
#' # View the structure
#' head(nursery_names)
#' table(nursery_names$nursery_ref)
#' }
#'
#' @seealso
#' \code{\link{fix_returns}} for applying these standardized names to survey data
#'
#' @export


read_nursery_names <- function(path) {
  path %>% readr::read_csv()
}
