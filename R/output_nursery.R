#' Generate Nursery Survey Publication
#'
#' Main wrapper function for the Nursery Survey Reproducible Analytical Pipeline (RAP).
#' Processes nursery survey returns, generates official statistics on improved nursery
#' stock sales, and produces publication-ready outputs including Word documents and
#' Excel spreadsheets for Quality Assurance.
#'
#' @param dir_path Character. Path to directory containing nursery survey return
#'   spreadsheets (.xlsx files). All Excel files in this directory will be processed.
#' @param hist_data Character. File path to the historical time series data (RDS format)
#'   containing previous years' nursery survey data for building the backseries.
#' @param nursery_names Character. File path to CSV file containing standardized nursery
#'   names and reference numbers for data cleaning and consistency.
#' @param ref_year Numeric. The reference year for publication (e.g., 2024). Data will
#'   be processed for the planting year ending two years prior (ref_year - 2).
#' @param out_path Character. Directory path where all outputs will be saved including
#'   Excel workbooks, Word documents, and updated RDS files.
#' @param out_name_doc Character. Base name for the output Word document (without extension).
#'   Final filename will include publication date: "out_name_doc_pub_date.docx".
#' @param pub_date Character. Publication date in "YYYY-MM-DD" format (e.g., "2024-10-17").
#'   Used for file naming and document metadata.
#' @param next_update Character. Next scheduled update date in "YYYY-MM-DD" format
#'   (e.g., "2025-11-16"). Used in publication metadata.
#'
#' @details
#' This function orchestrates the complete nursery survey pipeline:
#' \itemize{
#'   \item **Data Reading**: Processes all Excel returns in the specified directory
#'   \item **Data Cleaning**: Applies standardized nursery names and fixes disaggregation errors
#'   \item **Quality Assurance**: Validates data and identifies any corrections made
#'   \item **Time Series**: Combines new data with historical backseries
#'   \item **Analysis**: Generates statistics on sales volumes and improvement percentages
#'   \item **Publication**: Creates accessible Excel workbook and Word document
#' }
#'
#' The survey covers:
#' \itemize{
#'   \item **Species**: Sitka spruce and Scots pine
#'   \item **Regions**: Scotland and Great Britain totals
#'   \item **Stock Types**: Genetically improved (GI) and non-improved stock
#'   \item **Production Methods**: Seedlings, transplants, and cuttings
#' }
#'
#' Disaggregation error correction: The function automatically detects and corrects
#' cases where nurseries report Scotland totals that don't match their Great Britain
#' totals, typically occurring when "England & Wales" figures are negative.
#'
#' @return Invisible NULL. The function produces the following outputs:
#' \itemize{
#'   \item **Excel workbook**: Accessible publication spreadsheet with 6 supplementary tables
#'   \item **Word document**: QA report with tables, figures, and narrative text
#'   \item **RDS file**: Updated time series data for future publication cycles
#'   \item **Console message**: Confirmation of successful completion
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise bind_rows select ungroup arrange
#' @importFrom dplyr case_when recode where lag across
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom purrr map map_dfr
#' @importFrom readr read_rds write_rds
#' @importFrom magrittr "%>%"
#' @importFrom frpubutils round_safe output_pub_tab
#' @importFrom rmarkdown render
#'
#' @examples
#' \dontrun{
#' # Generate 2024 nursery survey publication
#' output_nursery(
#'   dir_path = "Z:/IFOS/Statistics/Data/Nursery Survey/2022-23/4_Surveys returned",
#'   hist_data = "Z:/IFOS/Statistics/Data/Nursery Survey/nursery_survey-2023-10-15.rds",
#'   nursery_names = "Z:/IFOS/Statistics/Data/Nursery Survey/names.csv",
#'   ref_year = 2024,
#'   out_path = "Z:/IFOS/Statistics/Data/Nursery Survey",
#'   out_name_doc = "nursery-survey",
#'   pub_date = "2024-10-17",
#'   next_update = "2025-11-16"
#' )
#' }
#'
#' @seealso
#' \code{\link{read_returns}} for reading survey data
#' \code{\link{fix_returns}} for data cleaning and error correction
#' \code{\link{check_returns}} for quality assurance validation
#' \code{\link{ns_figures}} for generating publication charts
#'
#' @export
#'


output_nursery <- function(dir_path,
                           hist_data,
                           nursery_names,
                           ref_year,
                           out_path,
                           out_name_doc,
                           pub_date,
                           next_update) {

  returns <- read_returns(dir_path)
  nurserys <- read_nursery_names(nursery_names)
  backseries <- readr::read_rds(hist_data)

  returns <- dplyr::bind_rows(returns, backseries)

  returns <- fix_returns(returns, nurserys)

  check_returns(returns)

  readr::write_rds(returns, paste0(out_path, "/", "nursery_survey-", Sys.Date(), ".rds"))

  s1_subs <- returns %>%
    dplyr::filter(country_sold_to == "Scotland") %>%
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::group_by(year, label) %>%
    dplyr::summarise(volume = frpubutils::round_safe(sum(volume/1000000), 5))

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
                                                  frpubutils::round_safe,
                                                  digits = 1))

  latest_year = planting_year(ref_year - 2)
  previous_year = planting_year(ref_year - 3)
  first_year = planting_year(min(returns$year))
  ten_ago = planting_year(ref_year - 11)

  table1 = returns %>%
    dplyr::filter(country_sold_to == "Scotland",
                  year %in% c(ref_year - 3, ref_year - 2)) %>%
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
    dplyr::mutate(
      pct_improved = (volume / sum(volume)) * 100,
      volume_total = sum(volume)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(label, gi) %>%
    dplyr::mutate(volume_lag = dplyr::lag(volume)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == ref_year - 2, gi == TRUE) %>%
    dplyr::select(label, volume_total, volume, pct_improved) %>%
    dplyr::mutate(
      volume_total = frpubutils::round_safe(volume_total, 1),
      volume = frpubutils::round_safe(volume, 1),
      pct_improved = frpubutils::round_safe(pct_improved, 1)
    ) %>%
    dplyr::slice(match(
      c("Sitka spruce: Seedlings",
        "Sitka spruce: VP",
        "Sitka spruce: Total",
        "Scots pine: Seedlings",
        "Total"),
      label
    ))


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
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ frpubutils::round_safe(.x, 1)),
      year = planting_year(year)
    ) %>%
    dplyr::select(
      year,
      `Sitka spruce: Seedlings`,
      `Sitka spruce: VP`,
      `Sitka spruce: Total`,
      `Scots pine: Seedlings`,
      Total
    )


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
    dplyr::mutate(
      pct_improved = (volume / sum(volume)) * 100,
      volume_total = sum(volume)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(label, gi) %>%
    dplyr::mutate(volume_lag = lag(volume)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == ref_year - 2, gi == TRUE) %>%
    dplyr::select(label, volume_total, volume, pct_improved) %>%
    dplyr::mutate(
      volume_total = frpubutils::round_safe(volume_total, 1),
      volume = frpubutils::round_safe(volume, 1),
      pct_improved = frpubutils::round_safe(pct_improved, 1)
    ) %>%
    dplyr::slice(match(
      c("Sitka spruce: Seedlings",
        "Sitka spruce: VP",
        "Sitka spruce: Total",
        "Scots pine: Seedlings",
        "Total"),
      label
    ))


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
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ frpubutils::round_safe(.x, 1)),
      year = planting_year(year)
    ) %>%
    dplyr::select(
      year,
      `Sitka spruce: Seedlings`,
      `Sitka spruce: VP`,
      `Sitka spruce: Total`,
      `Scots pine: Seedlings`,
      Total
    )


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
    af_obj = ns_a11y_obj)

  ns_figs <- ns_figures(returns, latest_year)




  # Word document for QA

  rmd_ns_file <- system.file("rmd", "report.Rmd", package = "nurserysurveyrap")

  rmarkdown::render(rmd_ns_file, output_file = paste0(out_path, "/", out_name_doc, "_", pub_date, ".docx"), quiet = TRUE)

  print("Progress: ---------- Nursery Survey COMPLETE ----------")
}



