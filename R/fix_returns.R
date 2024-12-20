#' Fix the returns that do not pass the validation test.
#'
#' @author Daniel Braby
#'
#'
#' @description Fixes the disaggregation errors.
#'
#' @param returns The raw returns.
#'
#' @importFrom dplyr filter mutate select reframe pick everything inner_join
#'
#'
#' @export



fix_returns <- function(raw_returns, nursery_names) {
  fix_disaggregation_error <- function(df) {
    disaggregation <- df |> dplyr::filter(country_sold_to == "E&W", !gi) |> dplyr::pull(volume)
    df |> dplyr::mutate(volume = volume + disaggregation *
                    ifelse(gi, -1, 1) *
                    ifelse(country_sold_to == "E&W", -1, 1) *
                    ifelse(disaggregation < 0, 1, 0))
  }

  raw_returns |>
    dplyr::reframe(fix_disaggregation_error(dplyr::pick(everything())),
            .by = c(nursery_ref, tree_sp, prod_method, year)) |>
    dplyr::select(!nursery_name) |>
    dplyr::inner_join(nursery_names, by = "nursery_ref")
}

