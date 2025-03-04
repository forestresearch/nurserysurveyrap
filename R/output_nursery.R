#' Output Nursery Survey
#'
#' @param dir_path Path to folder containing all chapter input data files.
#' @param hist_data Name of previous historical time series of input data. RD: Create a separate folder to hold the backseries outside of the yer folders. save the backseries as part of the output function and append a date stamp in the file name then select the most recent one in each run
#' @param nursery_names List of corrected nursery names RD:what do you mean by corrected?
#' @param ref_year tbd RD: needs to be updated
#' @param out_path Path to output folder.
#' @param out_name_doc doc file name
#' @param pub_date Publication date, string. Date format example: 6 October 2030.
#' @param next_update Next update date, string. Date format example: 6 October 2030.
#' @param stat_name Name of statistician # RD: to be updated

#'
#' @importFrom dplyr filter mutate group_by summarise bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom purrr map
#' @importFrom magrittr "%>%"
#'
#' @return tbd RD: needs to be updated
#' @export
#' RD: put the example here


output_nursery <- function(dir_path, hist_data, nursery_names,  #RD: nursery_names can be removed as an input parameter as this is internal to the package now
                           ref_year, out_path,
                           out_name_doc, pub_date, next_update,stat_name) {
  returns <- read_returns(dir_path)
  nurserys <- read_nursery_names(nursery_names) #RD: read this in from the package now
  backseries <- readr::read_rds(hist_data)

  returns <- dplyr::bind_rows(returns, backseries)

  returns <- fix_returns(returns, nurserys)

  check_returns(returns) #RD: Rename this function to something more informative like print_message_on_dissagregation_check

  readr::write_rds(returns, paste0(out_path, "/", "nursery_survey-", Sys.Date(), ".rds"))
#RD: There is a lot of repeated code for producing table s1, s2, s5 and s6. Put this in a function
  s1_subs <- returns %>%
    dplyr::filter(country_sold_to == "Scotland") %>%
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::group_by(year, label) %>%
    dplyr::summarise(volume = frpubutils::round_safe(sum(volume/1000000), 5)) #RD: 1,000,000 is used quite a few times in the code. Recommend setting million <- 1000000 at the top and using this in calculations to avoid a typo

  s1_sitka_tots <- s1_subs %>%
    dplyr::filter(label != "Scots pine: Seedlings") %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Sitka spruce: Total")

  s1_tots <- s1_subs  %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Total")


  s1 <- dplyr::bind_rows(s1_subs, s1_sitka_tots, s1_tots) %>%
    tidyr::pivot_wider(values_from = 'volume', names_from = 'label')



  s2_subs <- returns %>%
    dplyr::filter(country_sold_to == "Scotland", gi == TRUE) %>%
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::group_by(year, label) %>%
    dplyr::summarise(volume = frpubutils::round_safe(sum(volume/1000000), 5))

  s2_sitka_tots <- s2_subs %>%
    dplyr::filter(label != "Scots pine: Seedlings") %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Sitka spruce: Total")

  s2_tots <- s2_subs  %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Total")


  s2 <- dplyr::bind_rows(s2_subs, s2_sitka_tots, s2_tots) %>%
    tidyr::pivot_wider(values_from = 'volume', names_from = 'label')



  s3 = s2/s1
  s3$year <- s2$year


  s4_subs <- returns  %>%
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::group_by(year, label) %>%
    dplyr::summarise(volume = frpubutils::round_safe(sum(volume/1000000), 5))

  s4_sitka_tots <- s4_subs %>%
    dplyr::filter(label != "Scots pine: Seedlings") %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Sitka spruce: Total")

  s4_tots <- s4_subs  %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Total")


  s4 <- dplyr::bind_rows(s4_subs, s4_sitka_tots, s4_tots) %>%
    tidyr::pivot_wider(values_from = 'volume', names_from = 'label')



  s5_subs <- returns %>%
    dplyr::filter(gi == TRUE) %>%
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::group_by(year, label) %>%
    dplyr::summarise(volume = frpubutils::round_safe(sum(volume/1000000), 5))

  s5_sitka_tots <- s5_subs %>%
    dplyr::filter(label != "Scots pine: Seedlings") %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Sitka spruce: Total")

  s5_tots <- s5_subs  %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Total")


  s5 <- dplyr::bind_rows(s5_subs, s5_sitka_tots, s5_tots) %>%
    tidyr::pivot_wider(values_from = 'volume', names_from = 'label')



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

  pub_tables_rnd <- purrr::map(pub_tables, .f = ~ .x %>%
                                 dplyr::mutate_if(is.numeric,
                                                  frpubutils::round_safe, #RD:list frpubutils in the roxygen info at the top
                                                  digits = 1))
#RD:The planting_year function is for producing the date as text for the report but from this list only latest_year is used for this. 
#  It would be useful to use these parameters so create them as objects to use throughout the functions then at the end of the script, just before the report, overwrite latest_year with the formatted version to use in the report
  latest_year = planting_year(ref_year - 2)
  previous_year = planting_year(ref_year - 3) 
  first_year = planting_year(min(returns$year))
  ten_ago = planting_year(ref_year - 11) 
  
#RD: lots of repeated code for producing table 1-4. Put this in a function/s
  table1 = returns %>%
    dplyr::filter(country_sold_to == "Scotland",
           year %in% c(ref_year - 3, ref_year - 2)) %>% #RD: is this filtering for latest_year and previous_year? If so, replace those parameters here.
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::group_by(year, label, gi) %>%
    dplyr::summarise(volume = sum(volume/1000000))


  table1_sitka_tot = table1 %>%
    dplyr::filter(label != "Scots pine: Seedlings") %>%
    dplyr::group_by(year, gi) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Sitka spruce: Total")

  table1_tot = table1 %>%
    dplyr::group_by(year, gi) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Total")

  table1 <- dplyr::bind_rows(table1, table1_sitka_tot, table1_tot) %>%
    dplyr::group_by(year, label) %>%
    dplyr::mutate(pct_improved = (volume/sum(volume)) * 100,
           volume_total = sum(volume)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(label, gi) %>%
    dplyr::mutate(volume_lag = lag(volume)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == ref_year - 2,
           gi == TRUE) %>%
    select(label, volume_total, volume, pct_improved) %>%
    dplyr::mutate(volume_total = frpubutils::round_safe(volume_total, 1),
           volume = frpubutils::round_safe(volume, 1),
           pct_improved = frpubutils::round_safe(pct_improved, 1))

  t2 = returns %>%
    dplyr::filter(country_sold_to == "Scotland",
           year >= (max(year) - 9),
           gi == TRUE) %>%
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::summarise(volume = sum(volume/1000000, na.rm = TRUE), .by = c(year, label))

  t2_tots = t2 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume, na.rm = TRUE)) %>%
    dplyr::mutate(label = "Total")

  t2_sitka_tots = t2 %>%
    dplyr::filter(label != "Scots pine: Seedlings") %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume, na.rm = TRUE)) %>%
    dplyr::mutate(label = "Sitka spruce: Total")

  table2 = dplyr::bind_rows(t2, t2_sitka_tots, t2_tots) %>%
    tidyr::pivot_wider(names_from = "label",
                values_from = "volume") %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ frpubutils::round_safe(.x, 1)),
           year = planting_year(year))

  table3 = returns %>%
    dplyr::filter(year %in% c(ref_year - 3, ref_year - 2)) %>%
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::group_by(year, label, gi) %>%
    dplyr::summarise(volume = sum(volume/1000000))


  table3_sitka_tot = table3 %>%
    dplyr::filter(label != "Scots pine: Seedlings") %>%
    dplyr::group_by(year, gi) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Sitka spruce: Total")

  table3_tot = table3 %>%
    dplyr::group_by(year, gi) %>%
    dplyr::summarise(volume = sum(volume)) %>%
    dplyr::mutate(label = "Total")

  table3 <- dplyr::bind_rows(table3, table3_sitka_tot, table3_tot) %>%
    dplyr::group_by(year, label) %>%
    dplyr::mutate(pct_improved = (volume/sum(volume)) * 100,
           volume_total = sum(volume)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(label, gi) %>%
    dplyr::mutate(volume_lag = lag(volume)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == ref_year - 2,
           gi == TRUE) %>%
    select(label, volume_total, volume, pct_improved) %>%
    dplyr::mutate(volume_total = frpubutils::round_safe(volume_total, 1),
           volume = frpubutils::round_safe(volume, 1),
           pct_improved = frpubutils::round_safe(pct_improved, 1))

  t4 = returns %>%
    dplyr::filter(year >= (max(year) - 9),
           gi == TRUE) %>%
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::summarise(volume = sum(volume/1000000, na.rm = TRUE), .by = c(year, label))

  t4_tots = t4 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume, na.rm = TRUE)) %>%
    dplyr::mutate(label = "Total")

  t4_sitka_tots = t4 %>%
    dplyr::filter(label != "Scots pine: Seedlings") %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(volume = sum(volume, na.rm = TRUE)) %>%
    dplyr::mutate(label = "Sitka spruce: Total")

  table4 = dplyr::bind_rows(t4, t4_sitka_tots, t4_tots) %>%
    tidyr::pivot_wider(names_from = "label",
                values_from = "volume") %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ frpubutils::round_safe(.x, 1)),
           year = planting_year(year))

  ns_a11y_obj <- pub_a11y_prep(pub_date = pub_date,
                              next_update = next_update)

  options("openxlsx.dateFormat" = "dd-mmm-yy")

  frpubutils::output_pub_tab(
                 in_data = pub_tables,
                 ref_year = latest_year,
                 pub_date = pub_date,
                 pkg_nm = "nurserysurveyrap",
                 next_update = next_update,
                 out_path = out_path,
                 out_name_wb = paste0("nursery-survey_", pub_date),
                 replace_wb = TRUE,
                 rnd_no_dec = c(rep(FALSE, 6)),
                 a11y_obj = ns_a11y_obj)

  ns_figs <- ns_figures(returns, latest_year)




  # Word document output RD: not just for QA as you can hopefully use this to update the report after QA and produce the final output

  rmd_ns_file <- system.file("rmd", "report.Rmd", package = "nurserysurveyrap")

  rmarkdown::render(rmd_ns_file, output_file = paste0(out_path, "/", out_name_doc, "_", pub_date, ".docx"), quiet = TRUE) 

  print("Progress: ---------- Nursery Survey COMPLETE ----------")
}



