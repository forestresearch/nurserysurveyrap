#' Fix Nursery Survey Disaggregation Errors
#'
#' Corrects disaggregation errors in nursery survey returns and applies standardised
#' nursery names. Disaggregation errors occur when nurseries report Scotland figures
#' that don't properly aggregate with their Great Britain totals, typically resulting
#' in negative "England & Wales" values.
#'
#' @param raw_returns Tibble. Raw nursery survey data as output from \code{\link{read_returns}}.
#'   Must contain columns for volume, country_sold_to, gi, nursery_ref, tree_sp,
#'   prod_method, year, and nursery_name.
#' @param nursery_names Tibble. Standardized nursery names data as output from
#'   \code{\link{read_nursery_names}}. Must contain nursery_ref and corrected nursery_name columns.
#'
#' @details
#' The disaggregation error correction algorithm:
#' \itemize{
#'   \item **Identifies errors**: Finds negative volumes in "England & Wales" non-GI stock
#'   \item **Calculates correction**: Uses the magnitude of the negative value
#'   \item **Applies correction**: Redistributes volume between GI and non-GI categories
#'   \item **Maintains totals**: Ensures overall volumes remain consistent
#' }
#'
#' The correction logic:
#' \enumerate{
#'   \item Find the disaggregation amount (negative "E&W" non-GI volume)
#'   \item Add this amount to "E&W" non-GI stock (making it zero or positive)
#'   \item Subtract this amount from "E&W" GI stock
#'   \item Apply corresponding corrections to Scotland figures
#' }
#'
#' Name standardization:
#' \itemize{
#'   \item Removes original nursery_name from survey returns
#'   \item Joins with standardized names using nursery_ref
#'   \item Ensures consistent naming across years and surveys
#' }
#'
#' @return A tibble with corrected nursery survey data containing:
#' \itemize{
#'   \item All original columns except nursery_name is replaced with standardized version
#'   \item \code{volume} values corrected for disaggregation errors
#'   \item May contain some negative volumes if errors were detected and corrected
#' }
#'
#' @importFrom dplyr filter mutate select reframe pick everything inner_join pull
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' # Fix disaggregation errors in raw returns
#' raw_data <- read_returns("data/nursery_returns/")
#' nursery_names <- read_nursery_names("data/nursery_names.csv")
#'
#' corrected_data <- fix_returns(raw_data, nursery_names)
#'
#' # Check for any corrections made
#' check_returns(corrected_data)
#' }
#'
#' @seealso
#' \code{\link{read_returns}} for reading the raw data
#' \code{\link{read_nursery_names}} for loading standardized names
#' \code{\link{check_returns}} for validating the corrected data
#'
#' @export




fix_returns <- function(raw_returns, nursery_names) {
  fix_disaggregation_error <- function(df) {
    disaggregation <- df %>% dplyr::filter(country_sold_to == "E&W", !gi) %>% dplyr::pull(volume)
    df %>% dplyr::mutate(volume = volume + disaggregation *
                    ifelse(gi, -1, 1) *
                    ifelse(country_sold_to == "E&W", -1, 1) *
                    ifelse(disaggregation < 0, 1, 0))
  }

  raw_returns %>%
    dplyr::reframe(fix_disaggregation_error(dplyr::pick(everything())),
            .by = c(nursery_ref, tree_sp, prod_method, year)) %>%
    dplyr::select(!nursery_name) %>%
    dplyr::inner_join(nursery_names, by = "nursery_ref")
}

