#' Output Nursery Survey
#'
#' @param dir_path Path to folder containing all chapter input data files.
#' @param hist_data Name of previous historical time series of input data.
#' @param nursery_names List of corrected nursery names
#' @param latest_year String, year input data goes up to.
#' @param out_path Path to output folder.
#' @param pub_date Publication date, string. Date format example: 6 October 2030.
#' @param next_update Next update date, string. Date format example: 6 October 2030.
#'

#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter mutate group_by summarise bind_rows pivot_wider
#' @importFrom readr read_rds write_rds
#'
#' @return tbd
#' @export
#'


output_nursery <- function(dir_path, hist_data, nursery_names,
                           pub_year, out_path, pub_date, next_update) {
  returns <- read_returns(dir_path)
  nurserys <- read_nursery_names(nursery_names)
  backseries <- read_rds(hist_data)

  returns <- bind_rows(returns, backseries) %>%
    tibble::rownames_to_column("id")

  returns <- fix_returns(returns, nurserys)

  write_rds(returns, paste0(out_path, "/", "nursery_survey-", Sys.Date(), ".rds"))

  s1_subs <- returns %>%
    filter(country_sold_to == "Scotland") %>%
    mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    group_by(year, label) %>%
    summarise(volume = round_safe(sum(volume/1000000), 5))

  s1_sitka_tots <- s1_subs %>%
    filter(label != "Scots pine: Seedlings") %>%
    group_by(year) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Sitka spruce: Total")

  s1_tots <- s1_subs  %>%
    group_by(year) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Total")


  s1 <- bind_rows(s1_subs, s1_sitka_tots, s1_tots) %>%
    pivot_wider(values_from = 'volume', names_from = 'label')



  s2_subs <- returns %>%
    filter(country_sold_to == "Scotland", gi == TRUE) %>%
    mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    group_by(year, label) %>%
    summarise(volume = round_safe(sum(volume/1000000), 5))

  s2_sitka_tots <- s2_subs %>%
    filter(label != "Scots pine: Seedlings") %>%
    group_by(year) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Sitka spruce: Total")

  s2_tots <- s2_subs  %>%
    group_by(year) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Total")


  s2 <- bind_rows(s2_subs, s2_sitka_tots, s2_tots) %>%
    pivot_wider(values_from = 'volume', names_from = 'label')



  s3 = s2/s1
  s3$year <- s2$year


  s4_subs <- returns  %>%
    mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    group_by(year, label) %>%
    summarise(volume = round_safe(sum(volume/1000000), 5))

  s4_sitka_tots <- s4_subs %>%
    filter(label != "Scots pine: Seedlings") %>%
    group_by(year) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Sitka spruce: Total")

  s4_tots <- s4_subs  %>%
    group_by(year) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Total")


  s4 <- bind_rows(s4_subs, s4_sitka_tots, s4_tots) %>%
    pivot_wider(values_from = 'volume', names_from = 'label')



  s5_subs <- returns %>%
    filter(gi == TRUE) %>%
    mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    group_by(year, label) %>%
    summarise(volume = round_safe(sum(volume/1000000), 5))

  s5_sitka_tots <- s5_subs %>%
    filter(label != "Scots pine: Seedlings") %>%
    group_by(year) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Sitka spruce: Total")

  s5_tots <- s5_subs  %>%
    group_by(year) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Total")


  s5 <- bind_rows(s5_subs, s5_sitka_tots, s5_tots) %>%
    pivot_wider(values_from = 'volume', names_from = 'label')



  s6 = s5/s4
  s6$year <- s5$year



  pub_tables <- list(
    tables1 = s1,
    tables2 = s2,
    tables3 = s3,
    tables4 = s4,
    tables5 = s5,
    tables6 = s6
  )

  ns_a11y_obj <- pub_a11y_prep(pub_date = pub_date,
                              latest_year = latest_year,
                              next_update = next_update)

  options("openxlsx.dateFormat" = "dd-mmm-yy")

  output_pub_tab(in_data = pub_tables,
                 latest_year = latest_year,
                 pub_date = pub_date,
                 next_update = next_update,
                 out_path = out_path,
                 out_name_wb = paste0(out_name_wb, "_", time_date),
                 replace_wb = TRUE,
                 rnd_no_dec = c(rep(FALSE, 11)),
                 a11y_obj = ns_a11y_obj)



}
