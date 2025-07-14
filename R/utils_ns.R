#' Find Files Matching Pattern in Directory
#'
#' Recursively searches a directory and returns paths to files matching a specified
#' regular expression pattern. Useful for finding all files of a particular type
#' (e.g., .xlsx files) within a directory structure.
#'
#' @param dir_path Character. Path to the directory to search. Search includes
#'   all subdirectories recursively.
#' @param regex Character. Regular expression pattern to match against file names.
#'   Use standard regex syntax (e.g., "\\.xlsx$" for Excel files).
#'
#' @details
#' Common regex patterns:
#' \itemize{
#'   \item \code{"\\.xlsx$"} - Excel workbooks
#'   \item \code{"\\.csv$"} - CSV files  
#'   \item \code{"\\.rds$"} - R data files
#'   \item \code{"^nursery.*\\.xlsx$"} - Excel files starting with "nursery"
#' }
#'
#' @return Character vector of file paths matching the specified pattern.
#'   Returns empty character vector if no matches found.
#'
#' @importFrom fs dir_info
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' # Find all Excel files in directory
#' excel_files <- files_matching("data/surveys/", "\\.xlsx$")
#' 
#' # Find all CSV files
#' csv_files <- files_matching("data/", "\\.csv$")
#' 
#' # Find files with specific naming pattern
#' nursery_files <- files_matching("data/", "^nursery_return.*\\.xlsx$")
#' }
#'
#' @seealso
#' \code{\link{read_returns}} for reading the found Excel files
#'
#' @export
#'
files_matching <- function(dir_path, regex) {
  paths <- fs::dir_info(dir_path, recurse = TRUE)$path
  paths[grepl(regex, paths)]
}



#' Recursively match file paths under directory.
#'
#' @author Daniel Braby
#'
#' @description Return paths to files matching specified criteria.
#'
#' @param dir_path Directory path.
#' @param regex Matching criteria.
#'
#' @importFrom fs dir_info
#'
#' @return File paths.
#' @export
#'

names_diff <- function(new, old) {
  setdiff(names(new), names(old))
}


#' Perform pivots
#'
#' @author Daniel Braby
#'
#' @description Bespoke pivot longer for Nursery Survey returns
#'
#' @param dir_path Directory path.
#' @param regex Matching criteria.
#'
#' @importFrom fs dir_info
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom magrittr "%>%"
#'
#' @return File paths.
#' @export


with_pivot <- function(x, names_from, values_from, action) {
  x %>%
    tidyr::pivot_wider(names_from = all_of(names_from), values_from = all_of(values_from)) %>%
    action() %>%
    tidyr::pivot_longer(names_diff(., x), names_to = names_from, values_to = values_from,
                 names_sep = switch(length(names_from) > 1, "_", NA),
                 names_transform = function(x) type.convert(x, as.is = TRUE))
}


#' Check for Disaggregation Error Corrections
#'
#' Validates nursery survey data to determine if disaggregation error corrections
#' have been applied. Returns a message indicating whether any negative volumes
#' are present, which would indicate that corrections were made during processing.
#'
#' @param returns Tibble. Processed nursery survey data, typically the output from
#'   \code{\link{fix_returns}}. Must contain a volume column.
#'
#' @details
#' Disaggregation errors in nursery surveys typically occur when:
#' \itemize{
#'   \item Nurseries report Scotland totals that exceed Great Britain totals
#'   \item This results in negative "England & Wales" figures
#'   \item The \code{\link{fix_returns}} function corrects these automatically
#'   \item Corrections may result in some negative volume values
#' }
#'
#' This function provides quality assurance by:
#' \itemize{
#'   \item Scanning all volume values for negatives
#'   \item Alerting users when corrections have been applied
#'   \item Suggesting manual review of affected cases
#' }
#'
#' @return Character string message:
#' \itemize{
#'   \item If negative volumes found: Instructions to review affected cases
#'   \item If no negative volumes: Confirmation that all data is valid
#' }
#'
#' @importFrom dplyr if_else
#'
#' @examples
#' \dontrun{
#' # Check data after processing
#' raw_data <- read_returns("data/nursery_returns/")
#' nursery_names <- read_nursery_names("data/nursery_names.csv")
#' corrected_data <- fix_returns(raw_data, nursery_names)
#' 
#' # Validate the results
#' message <- check_returns(corrected_data)
#' print(message)
#' 
#' # If corrections were made, review the data
#' if (any(corrected_data$volume < 0)) {
#'   View(corrected_data[order(corrected_data$volume), ])
#' }
#' }
#'
#' @seealso
#' \code{\link{fix_returns}} for the function that applies corrections
#'
#' @export


check_returns <- function(returns) {
    dplyr::if_else(any(returns$volume < 0), "A disaggregation error has been corrected. Run View(returns) and sort by volume to identify which cases were affected.", "All is well.")
    }

