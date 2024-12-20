#' Output Nursery Survey
#'
#' @param dir_path Path to folder containing all chapter input data files.
#' @param hist_data Name of previous historical time series of input data.
#' @param nursery_names List of corrected nursery names
#' @param out_path Path to output folder.
#' @param out_name_doc doc file name
#' @param pub_date Publication date, string. Date format example: 6 October 2030.
#' @param next_update Next update date, string. Date format example: 6 October 2030.
#' @param stat_name Name of statistician
#' @param ref_year tbd
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter mutate group_by summarise bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom readr read_rds write_rds
#' @importFrom purrr map
#'
#' @return tbd
#' @export
#'


output_nursery <- function(dir_path, hist_data, nursery_names,
                           ref_year, out_path,
                           out_name_doc, pub_date, next_update,stat_name) {
  returns <- read_returns(dir_path)
  nurserys <- read_nursery_names(nursery_names)
  backseries <- read_rds(hist_data)

  returns <- bind_rows(returns, backseries)

  returns <- fix_returns(returns, nurserys)

  write_rds(returns, paste0(out_path, "/", "nursery_survey-", Sys.Date(), ".rds"))

  s1_subs <- returns %>%
    filter(country_sold_to == "Scotland") %>%
    mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    group_by(year, label) %>%
    summarise(volume = frpubutils::round_safe(sum(volume/1000000), 5))

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
    summarise(volume = frpubutils::round_safe(sum(volume/1000000), 5))

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
    summarise(volume = frpubutils::round_safe(sum(volume/1000000), 5))

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
    summarise(volume = frpubutils::round_safe(sum(volume/1000000), 5))

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

  pub_tables_rnd <- map(pub_tables, .f = ~ .x %>%
                                 dplyr::mutate_if(is.numeric,
                                                  frpubutils::round_safe,
                                                  digits = 1))

  latest_year = planting_year(ref_year - 2)
  previous_year = planting_year(ref_year - 3)
  first_year = planting_year(min(returns$year))
  ten_ago = planting_year(ref_year - 11)

  table1 = returns %>%
    filter(country_sold_to == "Scotland",
           year %in% c(ref_year - 3, ref_year - 2)) %>%
    mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    group_by(year, label, gi) %>%
    summarise(volume = sum(volume/1000000))


  table1_sitka_tot = table1 %>%
    filter(label != "Scots pine: Seedlings") %>%
    group_by(year, gi) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Sitka spruce: Total")

  table1_tot = table1 %>%
    group_by(year, gi) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Total")

  table1 <- rbind(table1, table1_sitka_tot, table1_tot) %>%
    group_by(year, label) %>%
    mutate(pct_improved = (volume/sum(volume)) * 100,
           volume_total = sum(volume)) %>%
    ungroup() %>%
    group_by(label, gi) %>%
    mutate(volume_lag = lag(volume)) %>%
    ungroup() %>%
    filter(year == ref_year - 2,
           gi == TRUE) %>%
    select(label, volume_total, volume, pct_improved) %>%
    mutate(volume_total = frpubutils::round_safe(volume_total, 1),
           volume = frpubutils::round_safe(volume, 1),
           pct_improved = frpubutils::round_safe(pct_improved, 1))

  t2 = returns %>%
    filter(country_sold_to == "Scotland",
           year >= (max(year) - 9),
           gi == TRUE) %>%
    mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    summarise(volume = sum(volume/1000000, na.rm = TRUE), .by = c(year, label))

  t2_tots = t2 %>%
    group_by(year) %>%
    summarise(volume = sum(volume, na.rm = TRUE)) %>%
    mutate(label = "Total")

  t2_sitka_tots = t2 %>%
    filter(label != "Scots pine: Seedlings") %>%
    group_by(year) %>%
    summarise(volume = sum(volume, na.rm = TRUE)) %>%
    mutate(label = "Sitka spruce: Total")

  table2 = rbind(t2, t2_sitka_tots, t2_tots) %>%
    pivot_wider(names_from = "label",
                values_from = "volume") %>%
    arrange(year) %>%
    mutate(across(where(is.numeric), ~ frpubutils::round_safe(.x, 1)),
           year = planting_year(year))

  table3 = returns %>%
    filter(year %in% c(ref_year - 3, ref_year - 2)) %>%
    mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    group_by(year, label, gi) %>%
    summarise(volume = sum(volume/1000000))


  table3_sitka_tot = table3 %>%
    filter(label != "Scots pine: Seedlings") %>%
    group_by(year, gi) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Sitka spruce: Total")

  table3_tot = table3 %>%
    group_by(year, gi) %>%
    summarise(volume = sum(volume)) %>%
    mutate(label = "Total")

  table3 <- rbind(table3, table3_sitka_tot, table3_tot) %>%
    group_by(year, label) %>%
    mutate(pct_improved = (volume/sum(volume)) * 100,
           volume_total = sum(volume)) %>%
    ungroup() %>%
    group_by(label, gi) %>%
    mutate(volume_lag = lag(volume)) %>%
    ungroup() %>%
    filter(year == ref_year - 2,
           gi == TRUE) %>%
    select(label, volume_total, volume, pct_improved) %>%
    mutate(volume_total = frpubutils::round_safe(volume_total, 1),
           volume = frpubutils::round_safe(volume, 1),
           pct_improved = frpubutils::round_safe(pct_improved, 1))

  t4 = returns %>%
    filter(year >= (max(year) - 9),
           gi == TRUE) %>%
    mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    summarise(volume = sum(volume/1000000, na.rm = TRUE), .by = c(year, label))

  t4_tots = t4 %>%
    group_by(year) %>%
    summarise(volume = sum(volume, na.rm = TRUE)) %>%
    mutate(label = "Total")

  t4_sitka_tots = t4 %>%
    filter(label != "Scots pine: Seedlings") %>%
    group_by(year) %>%
    summarise(volume = sum(volume, na.rm = TRUE)) %>%
    mutate(label = "Sitka spruce: Total")

  table4 = rbind(t4, t4_sitka_tots, t4_tots) %>%
    pivot_wider(names_from = "label",
                values_from = "volume") %>%
    arrange(year) %>%
    mutate(across(where(is.numeric), ~ frpubutils::round_safe(.x, 1)),
           year = planting_year(year))

  ns_a11y_obj <- pub_a11y_prep(pub_date = pub_date,
                              next_update = next_update)

  options("openxlsx.dateFormat" = "dd-mmm-yy")

  output_pub_tab(in_data = pub_tables,
                 latest_year = latest_year,
                 pub_date = pub_date,
                 next_update = next_update,
                 out_path = out_path,
                 out_name_wb = paste0("nursery-survey_", pub_date),
                 replace_wb = TRUE,
                 rnd_no_dec = c(rep(FALSE, 6)),
                 a11y_obj = ns_a11y_obj)

  ns_figs <- ns_figures(returns, latest_year)




  # Word document for QA
  rmarkdown::render("./inst/Rmd/report.Rmd", output_file = paste0(out_path, "/", out_name_doc, "_", pub_date, ".docx"), quiet = TRUE)

  print("Progress: ---------- Nursery Survey COMPLETE ----------")
}



