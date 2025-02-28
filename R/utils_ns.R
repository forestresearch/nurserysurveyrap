#' Recursively match file paths under directory.
#'
#' @author Daniel Braby
#'
#' @description Return paths to files matching specified criteria. RD: I don't believe you need this line in any of these sections
#'
#' @param dir_path Directory path.
#' @param regex Matching criteria.
#'
#' @importFrom fs dir_info
#' @importFrom magrittr "%>%"
#'
#' @return File paths.
#' @export
#'
files_matching <- function(dir_path, regex) {
  paths <- fs::dir_info(dir_path, recurse = TRUE)$path
  paths[grepl(regex, paths)]
}


RD: thisis repeated from above. Needs to be updated
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
#'RS: update inputs
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


#' Check for disaggregation error corrections
#'
#' @author Daniel Braby
#'
#' @description runs check of compiled time series data to identify where disaggregation error updates have occured
#'
#' @param returns dataset.
#'
#' @importFrom dplyr if_else
#'
#' @return Message
#' @export


check_returns <- function(returns) {
    dplyr::if_else(any(returns$volume < 0), "A disaggregation error has been corrected. Run View(returns) and sort by volume to identify which cases were affected.", "All is well.")
    }

