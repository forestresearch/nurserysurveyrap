#' Convert Date to Planting Year Interval
#'
#' Converts a specific date to its corresponding planting year interval. Planting
#' years in UK forestry run from 1st October to 30th September the following year,
#' aligning with the natural planting season cycle.
#'
#' @param date Date object or character string (YYYY-MM-DD) representing the date
#'   to convert to a planting year interval.
#'
#' @details
#' The UK forestry planting year convention:
#' \itemize{
#'   \item **Start**: 1st October
#'   \item **End**: 30th September (following year)
#'   \item **Example**: 2023/24 planting year runs from 1 Oct 2023 to 30 Sep 2024
#' }
#'
#' Date assignment logic:
#' \itemize{
#'   \item Dates from January-September belong to the planting year ending that year
#'   \item Dates from October-December belong to the planting year ending the following year
#' }
#'
#' @return A lubridate interval object representing the planting year period.
#'
#' @importFrom lubridate year interval make_date month
#'
#' @examples
#' \dontrun{
#' # Date in winter (planting season)
#' winter_date <- as.Date("2024-01-15")
#' planting_interval <- in_planting_year(winter_date)
#' # Returns interval from 1 Oct 2023 to 30 Sep 2024
#' 
#' # Date in autumn (start of new planting year)
#' autumn_date <- as.Date("2024-11-01")
#' planting_interval <- in_planting_year(autumn_date)
#' # Returns interval from 1 Oct 2024 to 30 Sep 2025
#' }
#'
#' @seealso
#' \code{\link{to_planting_year}} for converting years to planting year intervals
#' \code{\link{planting_year}} for getting text representation
#'
#' @export
#'


in_planting_year <- function(date) {
  ending_year <- lubridate::year(date) + ifelse(lubridate::month(date) >= 10, 1, 0)
  lubridate::interval(lubridate::make_date(ending_year - 1, 10, 1),
           lubridate::make_date(ending_year, 9, 30))
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
  in_planting_year(lubridate::make_date(year, 10, 1))
}

#' Convert Year to Planting Year Text Format
#'
#' Converts a calendar year to the standard planting year text format used in
#' UK forestry statistics. The planting year format shows both the starting and
#' ending years of the planting season (e.g., "2023/24").
#'
#' @param year Numeric. The starting year of the planting season. The planting
#'   year will run from October of this year to September of the following year.
#'
#' @details
#' The function creates the standard format used in forestry publications:
#' \itemize{
#'   \item **Format**: "YYYY/YY" (full start year, last two digits of end year)
#'   \item **Example**: Input 2023 produces "2023/24"
#'   \item **Coverage**: October 2023 to September 2024
#' }
#'
#' This format is consistent with:
#' \itemize{
#'   \item Official forestry statistics publications
#'   \item Nursery survey reporting periods
#'   \item UK government statistical standards
#' }
#'
#' @return Character string in planting year format "YYYY/YY".
#'
#' @importFrom lubridate int_start int_end year
#' @importFrom stringr str_sub
#'
#' @examples
#' \dontrun{
#' # Convert years to planting year format
#' planting_year(2023)  # Returns "2023/24"
#' planting_year(2024)  # Returns "2024/25"
#' 
#' # Use in vectorized operations
#' years <- c(2020, 2021, 2022, 2023)
#' sapply(years, planting_year)
#' # Returns c("2020/21", "2021/22", "2022/23", "2023/24")
#' }
#'
#' @seealso
#' \code{\link{to_planting_year}} for interval representation
#' \code{\link{in_planting_year}} for date-based conversion
#'
#' @export
#'

planting_year <- function(year) {
  interval <- to_planting_year(year)
  interval_f <- function(f) as.character(lubridate::year(f(interval)))
  start <- interval_f(lubridate::int_start)
  end <- interval_f(lubridate::int_end)
  paste(start, "/", stringr::str_sub(end, -2), sep = "")
}
