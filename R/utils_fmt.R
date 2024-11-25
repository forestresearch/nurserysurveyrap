#' Prepare content and formatting lists for use with a11ytables for a publication Excel spreadsheet
#'
#' @param pub_date Publication date, string. Date format example: 6 October 2030.
#' @param next_update Next update date, string. Date format example: 6 October 2030.
#'
#' @importFrom lubridate year
#'
#' @return List of cover, contents and note sheets for input to a11ytables and other formatting info.
#' @export


pub_a11y_prep <- function(pub_date,
                          next_update) {

  reporting_year <- planting_year(lubridate::year(pub_date) - 2)

  tab_titles <- c("Cover", "Contents", "Notes", "Table_S1",
                  "Table_S2", "Table_S3", "Table_S4",
                  "Table_S5", "Table_S6")

  sheet_types <- c("cover", "contents", "notes", "tables",
                   "tables", "tables", "tables",
                   "tables", "tables")

  sheet_titles <- c(
    paste0("Nursery Survey, Great Britain, 2005/05 to ",
           reporting_year),
    "Contents",
    "Notes",
    paste0("Table S1: Sales of nursery stock, Scotland, 2005/06 to ",
           reporting_year),
    paste0("Table S2: Sales of improved nursery stock, Scotland, 2005/06 to ", reporting_year),
    paste0("Table S3: Percentage of nursery stock sold that is genetically improved, Scotland, 2005/05 to", reporting_year),
    paste0("Table S4: Sales of nursery stock, Great Britain, 2005/06 to ",
           reporting_year),
    paste0("Table S5: Sales of improved nursery stock, Great Britain, 2005/06 to ", reporting_year),
    paste0("Table S6: Percentage of nursery stock sold that is genetically improved, Great Britain, 2005/05 to", reporting_year)
  )

  custom_rows <- list(
    NA_character_,
    NA_character_,
    NA_character_,
    "Information: Units are millions of plants",
    "Information: Units are millions of plants",
    "Information: Units are millions of plants",
    "Information: Units are millions of plants",
    "Information: Units are millions of plants",
    "Information: Units are millions of plants"
  )

  sources <- c(
    rep(NA_character_, 3),
    "Forest Research",
    "Forest Research",
    "Forest Research",
    "Forest Research",
    "Forest Research",
    "Forest Research"
  )

  cover_list <- list(
    "Publication information" = c(
      paste0("Publication date: ", pub_date),
      paste0("Data period: Final data for ", reporting_year),
      paste0("Next update: ", next_update)),
    "Further information" = c(
      "[Forestry Statistics publication (opens in a new window)](https://www.forestresearch.gov.uk/tools-and-resources/statistics/statistics-by-topic/other-topics/nursery-survey/)",
      "[Forest Research revisions policy for Official Statistics (opens in a new window)](https://cdn.forestresearch.gov.uk/2022/02/revisions_policy_k9zouvn-5.pdf)"),
    "Contact information" = c(
      "Forestry Information and Statistics team",
      "0300 067 5238",
      "[statistics@forestresearch.gov.uk](mailto:statistics@forestresearch.gov.uk)",
      "",
      "Defra Newsdesk (media enquiries)",
      "[newsdesk@defra.gov.uk](mailto:newsdesk@defra.gov.uk)"
      )
    )

  contents_df <- data.frame(
    "Sheet name" = c("Notes", "Table_S1", "Table_S2", "Table_S3",
                     "Table_S4", "Table_S5", "Table_S6"),
    "Sheet title" = c(
      "Notes used in this workbook",
      paste0("Table S1: Sales of nursery stock, Scotland, 2005/06 to ",
             reporting_year),
      paste0("Table S2: Sales of improved nursery stock, Scotland, 2005/06 to ", reporting_year),
      paste0("Table S3: Percentage of nursery stock sold that is genetically improved, Scotland, 2005/05 to", reporting_year),
      paste0("Table S4: Sales of nursery stock, Great Britain, 2005/06 to ",
             reporting_year),
      paste0("Table S5: Sales of improved nursery stock, Great Britain, 2005/06 to ", reporting_year),
      paste0("Table S6: Percentage of nursery stock sold that is genetically improved, Great Britain, 2005/05 to", reporting_year
    ),
    check.names = FALSE
  ))

  notes_df <- data.frame(
    "Footnote number" = "[note 1]",
    "Footnote text" = "Results for respondents only. This is estimated to account for around 90 to 100% of all sales of Sitka spruce and Scots pine in each of the years shown in the table."
  ,
  check.names = FALSE
  )

  table_colnames <- list(
    tables1 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "sitka_seed",
                "Sitka spruce: VP" = "sitka_vp",
                "Sitka spruce: Total" = "sitka_tot",
                "Scots pine: Seedlings" = "scots_seed",
                "Total" = "tot"),
    tables2 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "sitka_seed",
                "Sitka spruce: VP" = "sitka_vp",
                "Sitka spruce: Total" = "sitka_tot",
                "Scots pine: Seedlings" = "scots_seed",
                "Total" = "tot"),
    tables3 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "sitka_seed",
                "Sitka spruce: VP" = "sitka_vp",
                "Sitka spruce: Total" = "sitka_tot",
                "Scots pine: Seedlings" = "scots_seed",
                "Total" = "tot"),
    tables4 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "sitka_seed",
                "Sitka spruce: VP" = "sitka_vp",
                "Sitka spruce: Total" = "sitka_tot",
                "Scots pine: Seedlings" = "scots_seed",
                "Total" = "tot"),
    tables5 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "sitka_seed",
                "Sitka spruce: VP" = "sitka_vp",
                "Sitka spruce: Total" = "sitka_tot",
                "Scots pine: Seedlings" = "scots_seed",
                "Total" = "tot"),
    tables6 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "sitka_seed",
                "Sitka spruce: VP" = "sitka_vp",
                "Sitka spruce: Total" = "sitka_tot",
                "Scots pine: Seedlings" = "scots_seed",
                "Total" = "tot")
  )


}

#' Format table with custom flextable theme for publication Word document
#'
#' @param in_table Table (tibble or data frame) to be formatted
#' @param round_it Boolean, default is TRUE. TRUE will round numbers in table to 0 decimal places, FALSE will not round.
#' @param char_cols Vector containing index of column(s) that are character, can be length zero.
#' @param num_cols Vector containing index of column(s) that are numeric, can be length zero.
#'
#' @importFrom flextable colformat_double align valign font fontsize nrow_part bg bold autofit vline border_outer set_table_properties height hrule width dim_pretty
#' @importFrom officer fp_border
#' @importFrom purrr map_lgl
#'
#' @return Flextable
#' @export
#'
ft_theme_pub <- function(in_table,
                         round_it = TRUE,
                         char_cols,
                         num_cols) {

  if (round_it == TRUE) {
    in_table <- flextable::colformat_double(in_table, big.mark = ",", digits = 0)
  } else {
    in_table <- flextable::colformat_double(in_table, big.mark = ",")
  }

  in_table <- flextable::colformat_double(in_table, j = 1, big.mark = "", digits = 0)
  if (length(num_cols) > 0) {
    in_table <- flextable::align(in_table, align = "right", j = num_cols)
  }
  if (length(char_cols) > 0) {
    in_table <- flextable::align(in_table, align = "left", j = char_cols)
  }
  in_table <- flextable::align(in_table, align = "center", part = "header")
  in_table <- flextable::valign(in_table, valign = "center", part = "all")
  in_table <- flextable::font(in_table, fontname = "Verdana", part = "all")
  in_table <- flextable::fontsize(in_table, size = 12, part = "all")

  # borders
  pub_border <- officer::fp_border(color = "black", style = "single")
  in_table <- flextable::border_outer(in_table, border = pub_border, part = "all")
  in_table <- flextable::vline(in_table, j = 1)

  # copied from flextable::theme_zebra and adapted colours
  odd_header <- "#D9D9D9"
  even_body <- "#F2F2F2"
  even_header <- "transparent"
  odd_body <- "transparent"
  h_nrow <- flextable::nrow_part(in_table, "header")
  f_nrow <- flextable::nrow_part(in_table, "footer")
  b_nrow <- flextable::nrow_part(in_table, "body")

  if (h_nrow > 0) {
    even <- seq_len(h_nrow)%%2 == 0
    odd <- !even
    in_table <- flextable::bg(x = in_table, i = odd, bg = odd_header, part = "header")
    in_table <- flextable::bg(x = in_table, i = even, bg = even_header, part = "header")
    in_table <- flextable::bold(x = in_table, bold = TRUE, part = "header")
  }
  if (f_nrow > 0) {
    even <- seq_len(f_nrow)%%2 == 0
    odd <- !even
    in_table <- flextable::bg(x = in_table, i = odd, bg = odd_header, part = "footer")
    in_table <- flextable::bg(x = in_table, i = even, bg = even_header, part = "footer")
    in_table <- flextable::bold(x = in_table, bold = TRUE, part = "footer")
  }
  if (b_nrow > 0) {
    even <- seq_len(b_nrow)%%2 == 0
    odd <- !even
    in_table <- flextable::bg(x = in_table, i = odd, bg = odd_body, part = "body")
    in_table <- flextable::bg(x = in_table, i = even, bg = even_body, part = "body")
  }

  in_table <- flextable::set_table_properties(in_table, width = 1, layout = "autofit")
  in_table_pretty_dim <- flextable::dim_pretty(in_table, part = "body")
  in_table <- flextable::height(in_table, height = in_table_pretty_dim$height, part = "body")
  in_table <- flextable::width(in_table, width = in_table_pretty_dim$width)
  in_table <- flextable::hrule(in_table, rule = "atleast", part = "body")

  return(in_table)
}

#' Label flextable headings with option to include superscript text at end of label
#'
#' @description
#' Label flextable headings with option to contain superscript text at end of label. Arguments need to be provided for all flextable object ('in_table') columns.
#' Can be used to label a flextable objects headings with no superscripts if 'sup_vals' argument is not used.
#'
#'
#' @param in_table Flextable
#' @param var_names Vector containing names of columns in flextable
#' @param label_names Vector containing new labels for column headings
#' @param sup_vals Vector containing superscript text to be added to end of column label, can be blank for columns that don't need superscript (i.e. "").
#'
#' @importFrom magrittr "%>%"
#' @importFrom flextable compose ncol_keys as_paragraph as_sup
#'
#' @return Flextable
#' @export
#'
ft_header_label <- function(in_table, var_names, label_names, sup_vals = NULL) {

  if (is.null(sup_vals) == TRUE) {
    sup_vals <- rep("", flextable::ncol_keys(in_table))
  }

  if (min(length(var_names), length(label_names), length(sup_vals)) != flextable::ncol_keys(in_table)) {
    stop("ensure the vector arguments contain the same number of elements as the number of columns in 'in_table'.")
  }

  for (i in 1:length(var_names)) {
    in_table <- in_table %>% flextable::compose(i = 1, j = var_names[i], part = "header",
                                                value = flextable::as_paragraph(label_names[i], flextable::as_sup(sup_vals[i])))
  }

  return(in_table)

}
