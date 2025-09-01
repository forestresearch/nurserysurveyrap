#' Read Individual Nursery Survey Return
#'
#' Reads and processes a single nursery survey return from an Excel spreadsheet.
#' The function handles the specific data structure of nursery survey returns,
#' disaggregating totals into component parts and standardizing variable names.
#'
#' @param file_path Character. File path to the Excel workbook containing the
#'   nursery survey return. Must contain a "return" sheet with the expected format.
#'
#' @details
#' The function performs several data transformations:
#' \itemize{
#'   \item **Country disaggregation**: Calculates "England & Wales" from Great Britain minus Scotland
#'   \item **Stock type disaggregation**: Calculates "Non-GI" from Total minus Genetically Improved (GI)
#'   \item **Variable standardization**: Creates boolean GI variable and converts units
#'   \item **Data type conversion**: Ensures proper numeric and integer types
#' }
#'
#' Expected spreadsheet structure:
#' \itemize{
#'   \item Sheet name: "return"
#'   \item Columns include: year, nursery_ref, tree_sp, prod_method
#'   \item Country columns: GB (Great Britain), Scotland
#'   \item Stock type columns: Total, GI (Genetically Improved)
#'   \item Volume data in thousands of plants
#' }
#'
#' @return A tibble with disaggregated nursery survey data containing:
#' \itemize{
#'   \item \code{year} - Integer, survey year
#'   \item \code{nursery_ref} - Integer, nursery reference number
#'   \item \code{tree_sp} - Character, tree species ("Sitka spruce" or "Scots pine")
#'   \item \code{prod_method} - Character, production method ("Seedlings", "Transplants", "Cuttings")
#'   \item \code{country_sold_to} - Character, destination country ("Scotland" or "E&W")
#'   \item \code{gi} - Logical, TRUE for genetically improved stock, FALSE otherwise
#'   \item \code{volume} - Numeric, number of plants (converted from thousands)
#'   \item \code{stock_type} - Character, "GI" or "Non-GI"
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate select recode
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' # Read a single nursery return
#' single_return <- read_return("path/to/nursery_return_2024.xlsx")
#' head(single_return)
#' }
#'
#' @seealso
#' \code{\link{read_returns}} for reading multiple returns
#' \code{\link{with_pivot}} for the pivoting functionality used internally
#'
#' @export
#'
read_return <- function(file_path) {
  invisible(readxl::read_xlsx(path = file_path, sheet = "return")) %>%
    with_pivot("country_sold_to", "volume_thousand",
               function (x) {
                 x %>% dplyr::mutate("E&W" = GB - Scotland) %>%
                   dplyr::select(!GB)
               }) %>%
    with_pivot("volume_type", "volume_thousand",
               function (x) {
                 x %>% dplyr::mutate("Non-GI" = Total - GI) %>%
                   dplyr::select(!Total)
               }) %>%
    dplyr::mutate(
      gi = dplyr::recode(volume_type, "GI" = TRUE, "Non-GI" = FALSE),
      year = as.integer(year),
      nursery_ref = as.integer(nursery_ref),
      volume = volume_thousand * 1000,
      .keep = "unused"
    )
}

#' Read All Nursery Survey Returns from Directory
#'
#' Reads and combines all nursery survey returns from Excel spreadsheets in a
#' specified directory. This function processes all .xlsx files found recursively
#' and compiles them into a single dataset for analysis.
#'
#' @param dir_path Character. Path to directory containing nursery survey return
#'   spreadsheets. All .xlsx files in this directory and subdirectories will be processed.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Searches recursively for all .xlsx files in the specified directory
#'   \item Processes each file using \code{\link{read_return}}
#'   \item Combines all returns into a single tibble
#'   \item Maintains data structure consistency across all files
#' }
#'
#' Expected directory structure:
#' \itemize{
#'   \item Contains Excel workbooks (.xlsx) with nursery survey returns
#'   \item Each workbook must have a "return" sheet with standardized format
#'   \item Files can be in subdirectories (recursive search)
#' }
#'
#' @return A tibble combining all nursery survey returns with columns:
#' \itemize{
#'   \item \code{year} - Integer, survey year
#'   \item \code{nursery_ref} - Integer, nursery reference number
#'   \item \code{nursery_name} - Character, nursery name from spreadsheet
#'   \item \code{tree_sp} - Character, tree species
#'   \item \code{prod_method} - Character, production method
#'   \item \code{country_sold_to} - Character, destination country
#'   \item \code{gi} - Logical, genetically improved flag
#'   \item \code{volume} - Numeric, number of plants
#'   \item \code{stock_type} - Character, stock type classification
#' }
#'
#' @importFrom purrr map_dfr
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' # Read all nursery returns from a directory
#' all_returns <- read_returns("data/nursery_returns_2024/")
#'
#' # View summary
#' summary(all_returns)
#' table(all_returns$tree_sp, all_returns$prod_method)
#' }
#'
#' @seealso
#' \code{\link{read_return}} for reading individual returns
#' \code{\link{files_matching}} for file discovery functionality
#' \code{\link{fix_returns}} for subsequent data cleaning
#'
#' @export
#'
read_returns <- function(dir_path) {
  dir_path %>%
    files_matching("\\.xlsx$") %>%
    purrr::map_dfr(read_return)
}


