#' Date to planting year interval.
#'
#' @author Daniel Braby
#'
#' @description Planting years run from 1st October to 30th September the following year.
#'
#' @param date Date to get the planting year from.
#'
#' @importFrom lubridate year interval make_date
#'
#' @return Planting year interval.
#' @export
#'


in_planting_year <- function(date) {
  ending_year <- year(date) + ifelse(month(date) >= 10, 1, 0)
  interval(make_date(ending_year - 1, 10, 1),
           make_date(ending_year, 9, 30))
}

#' Get the planting year interval that is represented by a given year.
#'
#' @description The interval \strong{starts} in the year that is specified.
#'
#' @param year Year to convert to planting year interval.
#'
#' @return Planting year interval.
#' @export
#'

to_planting_year <- function(year) {
  in_planting_year(make_date(year, 10, 1))
}

#' Get the planting year as text.
#'
#' @description The interval \strong{starts} in the year that is specified.
#'
#' @param year Year to convert to planting year interval.
#'
#' @return Planting year text.
#' @export
#'

planting_year <- function(year) {
  interval <- to_planting_year(year)
  interval_f <- function(f) as.character(year(f(interval)))
  start <- interval_f(int_start)
  end <- interval_f(int_end)
  paste(start, "/", stringr::str_sub(end, -2), sep = "")
}
