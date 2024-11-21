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
#' @importFrom magrittr "%>%"
#' @importFrom fs dir_info
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @return File paths.
#' @export


with_pivot <- function(x, names_from, values_from, action) {
  x %>%
    pivot_wider(names_from = all_of(names_from), values_from = all_of(values_from)) %>%
    action() %>%
    pivot_longer(names_diff(., x), names_to = names_from, values_to = values_from,
                 names_sep = switch(length(names_from) > 1, "_", NA),
                 names_transform = function(x) type.convert(x, as.is = TRUE))
}


#' Rounding function with correction to rounding with 0.5
#'
#' @md
#'
#' @description
#' https://stackoverflow.com/questions/12688717/round-up-from-5
#'
#' "Note that for rounding off a 5, the IEC 60559 standard is expected to be used, ‘go to the even digit’. Therefore round(0.5) is 0 and round(-1.5) is -2. However, this is dependent on OS services and on representation error (since e.g. 0.15 is not represented exactly, the rounding rule applies to the represented number and not to the printed number, and so round(0.15, 1) could be either 0.1 or 0.2)."
#'
#' @param x Number or vector of numbers.
#' @param digits Number specifying how many digits to round to.
#'
#' @return Rounded number or vector of numbers
#' @export
#'
round_safe <- function(x,
                       digits) {
  posneg <- sign(x)
  z <- abs(x)*10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z/10^digits
  z*posneg
}

#' Rounding and formatting function for R markdown
#'
#' @description
#' Rounding and formatting function for R markdown that displays the correct number of decimal places in a rendered R markdown document.
#'
#' For example, '3.0' would display as '3.0', rather than '3'.
#'
#' @param x Number or vector of numbers.
#' @param digits Number specifying how many digits to round to.
#'
#' @return Rounded number or vector of numbers
#' @export
#'
round_fmt_rmd <- function(x,
                          digits) {
  fmtd_num <- format(round_safe(x, digits = digits), nsmall = digits)
  return(fmtd_num)
}
