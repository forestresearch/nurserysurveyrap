#' Fix the returns that do not pass the validation test (disaggregation errors)
#'
#' @author Daniel Braby
#'
#'
#' RD: dont need this second description
#'
#' @param returns The raw returns.
#RD: include info on nursery_names input
#'
#' @importFrom dplyr filter mutate select reframe pick everything inner_join
#' @importFrom magrittr "%>%"
#'
#'
#' @export
#RD: what does this script output?


fix_returns <- function(raw_returns, nursery_names) {
  fix_disaggregation_error <- function(df) {  #RD: this is a function defined within a function. Pull it out and define separately. Also rename df to something more informative
    disaggregation <- df %>% dplyr::filter(country_sold_to == "E&W", !gi) %>% dplyr::pull(volume)
    df %>% dplyr::mutate(volume = volume + disaggregation * #RD:I don't think this is doing what it's supposed to. At this line we're assuming the values in disaggregation are matching with the correct corresponding rows df (raw_returns) but from what I can see it's not matching up correctly.
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

