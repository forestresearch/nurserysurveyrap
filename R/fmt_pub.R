#' Prepare content and formatting lists for use with a11ytables for a publication Excel spreadsheet
#'
#' @description 
#' This function prepares all the necessary components for creating an accessible Excel spreadsheet
#' publication using a11ytables. It generates cover sheet information, table of contents, notes,
#' and formatting specifications for nursery survey data tables.
#'
#' @param pub_date Publication date, string. Date format example: 6 October 2030.
#' @param next_update Next update date, string. Date format example: 6 October 2030.
#'
#' @importFrom lubridate year
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{cover_list}{List containing publication information, further information links, and contact details}
#'   \item{contents_df}{Data frame with sheet names and titles for the table of contents}
#'   \item{notes_df}{Data frame containing footnotes used in the tables}
#'   \item{tab_titles}{Character vector of sheet tab names}
#'   \item{sheet_types}{Character vector specifying the type of each sheet}
#'   \item{sheet_titles}{Character vector of full sheet titles}
#'   \item{custom_rows}{List of custom row information for each sheet}
#'   \item{sources}{Character vector of data sources for each sheet}
#'   \item{table_colnames}{List of column name mappings for each table}
#'   \item{blank_cells}{Character vector for handling blank cells}
#' }
#' @export


pub_a11y_prep <- function(pub_date,
                          next_update) {

  # Apply date formatting
  pub_date <- frpubutils::long_fmt_date(pub_date)
  next_update <- frpubutils::long_fmt_date(next_update)

  reporting_year <- planting_year(lubridate::year(pub_date) - 2)

  tab_titles <- c("Cover", "Contents", "Notes", "Table_S1",
                  "Table_S2", "Table_S3", "Table_S4",
                  "Table_S5", "Table_S6")

  sheet_types <- c("cover", "contents", "notes", "tables",
                   "tables", "tables", "tables",
                   "tables", "tables")

  sheet_titles <- c(
    paste0("Nursery Survey, Great Britain, 2005/06 to ",
           reporting_year),
    "Contents",
    "Notes",
    paste0("Table S1: Sales of nursery stock, Scotland, 2005/06 to ", reporting_year, " [note 1]"),
    paste0("Table S2: Sales of improved nursery stock, Scotland, 2005/06 to ", reporting_year, " [note 1]"),
    paste0("Table S3: Percentage of nursery stock sold that is genetically improved, Scotland, 2005/06 to ", reporting_year, " [note 1]"),
    paste0("Table S4: Sales of nursery stock, Great Britain, 2005/06 to ", reporting_year, " [note 1]"),
    paste0("Table S5: Sales of improved nursery stock, Great Britain, 2005/06 to ", reporting_year, " [note 1]"),
    paste0("Table S6: Percentage of nursery stock sold that is genetically improved, Great Britain, 2005/06 to ", reporting_year, " [note 1]")
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
    rep("Forest Research", 6)
  )

  cover_list <- list(
    "Publication information" = c(
      paste0("Publication date: ", pub_date),
      paste0("Data period: Final data for ", reporting_year),
      paste0("Next update: ", next_update)),
    "Further information" = c(
      "[Nursery Survey publication (opens in a new window)](https://www.forestresearch.gov.uk/tools-and-resources/statistics/statistics-by-topic/other-topics/nursery-survey/)",
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
      paste0("Table S3: Percentage of nursery stock sold that is genetically improved, Scotland, 2005/06 to ", reporting_year),
      paste0("Table S4: Sales of nursery stock, Great Britain, 2005/06 to ",
             reporting_year),
      paste0("Table S5: Sales of improved nursery stock, Great Britain, 2005/06 to ", reporting_year),
      paste0("Table S6: Percentage of nursery stock sold that is genetically improved, Great Britain, 2005/06 to ", reporting_year
    )),
    check.names = FALSE
  )

  notes_df <- data.frame(
    "Footnote number" = c("[note 1]"),
    "Footnote text" = c("Results for respondents only. This is estimated to account for around 90 to 100% of all sales of Sitka spruce and Scots pine in each of the years shown in the table.")
  ,
  check.names = FALSE
  )

  table_colnames <- list(
    tables1 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "Sitka spruce: Seedlings",
                "Sitka spruce: VP" = "Sitka spruce: VP",
                "Sitka spruce: Total" = "Sitka spruce: Total",
                "Scots pine: Seedlings" = "Scots pine: Seedlings",
                "Total" = "Total"),
    tables2 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "Sitka spruce: Seedlings",
                "Sitka spruce: VP" = "Sitka spruce: VP",
                "Sitka spruce: Total" = "Sitka spruce: Total",
                "Scots pine: Seedlings" = "Scots pine: Seedlings",
                "Total" = "Total"),
    tables3 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "Sitka spruce: Seedlings",
                "Sitka spruce: VP" = "Sitka spruce: VP",
                "Sitka spruce: Total" = "Sitka spruce: Total",
                "Scots pine: Seedlings" = "Scots pine: Seedlings",
                "Total" = "Total"),
    tables4 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "Sitka spruce: Seedlings",
                "Sitka spruce: VP" = "Sitka spruce: VP",
                "Sitka spruce: Total" = "Sitka spruce: Total",
                "Scots pine: Seedlings" = "Scots pine: Seedlings",
                "Total" = "Total"),
    tables5 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "Sitka spruce: Seedlings",
                "Sitka spruce: VP" = "Sitka spruce: VP",
                "Sitka spruce: Total" = "Sitka spruce: Total",
                "Scots pine: Seedlings" = "Scots pine: Seedlings",
                "Total" = "Total"),
    tables6 = c("Planting year" = "year",
                "Sitka spruce: Seedlings" = "Sitka spruce: Seedlings",
                "Sitka spruce: VP" = "Sitka spruce: VP",
                "Sitka spruce: Total" = "Sitka spruce: Total",
                "Scots pine: Seedlings" = "Scots pine: Seedlings",
                "Total" = "Total")
  )

  blank_cells <- c(rep(NA_character_, 9))

  out_list <- list(cover_list = cover_list,
                   contents_df = contents_df,
                   notes_df = notes_df,
                   tab_titles = tab_titles,
                   sheet_types = sheet_types,
                   sheet_titles = sheet_titles,
                   custom_rows = custom_rows,
                   sources = sources,
                   table_colnames = table_colnames,
                   blank_cells = blank_cells)

  return(out_list)


}

#' Format table with custom flextable theme for publication Word document
#'
#' @description
#' Applies Forest Research's publication formatting standards to a flextable object.
#' This includes font settings (Verdana 12pt), alternating row colors, borders,
#' column alignment, and number formatting.
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
#' @return A formatted flextable object with publication-ready styling
#' 
#' @examples
#' \dontrun{
#' # Create a simple data frame
#' df <- data.frame(
#'   Year = 2020:2023,
#'   Sales = c(1234567, 2345678, 3456789, 4567890),
#'   Category = c("A", "B", "C", "D")
#' )
#' 
#' # Create flextable and apply theme
#' ft <- flextable::flextable(df)
#' ft_formatted <- ft_theme_pub(ft, round_it = TRUE, char_cols = 3, num_cols = 2)
#' }
#' 
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
#' When sup_vals is NULL (default), the function will simply relabel the headers without adding any superscripts.
#'
#' @param in_table Flextable
#' @param var_names Vector containing names of columns in flextable
#' @param label_names Vector containing new labels for column headings
#' @param sup_vals Vector containing superscript text to be added to end of column label, can be blank for columns that don't need superscript (i.e. ""). Default is NULL.
#'
#' @importFrom flextable compose ncol_keys as_paragraph as_sup
#' @importFrom magrittr "%>%"
#'
#' @return A flextable object with updated column headers
#' 
#' @examples
#' \dontrun{
#' # Create a simple flextable
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#' ft <- flextable::flextable(df)
#' 
#' # Add labels without superscripts
#' ft_labeled <- ft_header_label(ft, 
#'                               var_names = c("a", "b", "c"),
#'                               label_names = c("Column A", "Column B", "Column C"))
#' 
#' # Add labels with superscripts
#' ft_labeled_sup <- ft_header_label(ft, 
#'                                   var_names = c("a", "b", "c"),
#'                                   label_names = c("Column A", "Column B", "Column C"),
#'                                   sup_vals = c("1", "2", ""))
#' }
#' 
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

#' Create list of NS publication ready figures 
#'
#' @description
#' Generates publication-ready figures for the Nursery Survey publication showing
#' sales of genetically improved nursery stock over time. Creates line graphs for
#' both Scotland and Great Britain data using Forest Research's standard chart formatting.
#'
#' @param returns A data frame containing compiled nursery survey returns data with columns:
#'   \describe{
#'     \item{country_sold_to}{Country where stock was sold}
#'     \item{gi}{Logical indicating if stock is genetically improved}
#'     \item{tree_sp}{Tree species (e.g., "Sitka spruce", "Scots pine")}
#'     \item{prod_method}{Production method (e.g., "Seedlings", "VP")}
#'     \item{year}{Year of data}
#'     \item{volume}{Volume of sales}
#'   }
#' @param latest_year Integer. The most recent year of data in the dataset.
#'
#' @importFrom dplyr select arrange mutate case_when filter summarise group_by ungroup slice_max pull
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#' @importFrom scales date_format
#' @importFrom magrittr "%>%"
#'
#' @return A list containing two ggplot2 figure objects:
#' \describe{
#'   \item{fig1}{Line graph showing sales of genetically improved nursery stock in Scotland}
#'   \item{fig2}{Line graph showing sales of genetically improved nursery stock in Great Britain}
#' }
#' 
#' @note This function requires the afcharts package for Forest Research styling.
#' The afcharts::use_afcharts() function is called within to apply theme settings.
#' 
#' @examples
#' \dontrun{
#' # Assuming 'nursery_data' is your compiled returns data
#' figures <- ns_figures(returns = nursery_data, latest_year = 2023)
#' 
#' # Access individual figures
#' scotland_fig <- figures$fig1
#' gb_fig <- figures$fig2
#' }
#' 
#' @export
#'
ns_figures <- function(returns,
                        latest_year) {

  # Apply Forest Research chart formatting
  # Note: afcharts package is required for styling
  afcharts::use_afcharts() 

  next_ten <- function(x) { 10*ceiling(x/10) }

  gb_next_10 <- returns %>% dplyr::filter(gi == TRUE) %>%
    dplyr::mutate(label = paste(tree_sp, prod_method)) %>%
    dplyr::group_by(label, year) %>%
    dplyr::summarise(volume = sum(volume/1000000)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(volume) %>%
    dplyr::pull(volume) %>%
    next_ten()

  sc_next_10 <- returns %>% dplyr::filter(country_sold_to == "Scotland",
                                   gi == TRUE) %>%
    dplyr::mutate(label = paste(tree_sp, prod_method)) %>%
    dplyr::group_by(label, year) %>%
    dplyr::summarise(volume = sum(volume/1000000)) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(volume) %>%
    dplyr::pull(volume) %>%
    next_ten()

  fig1 = returns %>%
    dplyr::filter(country_sold_to == "Scotland",
           gi == TRUE) %>%
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::summarise(volume = sum(volume, na.rm = TRUE), .by = c(year, label)) %>%
    ggplot2::ggplot(aes(year, volume/1000000, colour = label, group = label)) +
    ggplot2::geom_line(linewidth = 1) +
    afcharts::theme_af(legend = "bottom") +
    afcharts::scale_colour_discrete_af(labels = c(
      "Scots pine: Seedlings",
      "Sitka spruce: Seedlings",
      "Sitka spruce: VP"
    )) +
    ggplot2::scale_x_continuous(labels = planting_year) +
    ggplot2::scale_y_continuous(breaks = seq(0, sc_next_10, 10),
                       expand = ggplot2::expansion(mult = c(0, 0)),
                       limits = c(0, sc_next_10)) +
    ggplot2::labs(x = NULL,
         y = NULL,
         colour = NULL)

  fig2 = returns %>%
    dplyr::filter(country_sold_to == "Scotland",
           gi == TRUE) %>%
    dplyr::mutate(label = paste(tree_sp, prod_method, sep = ": ")) %>%
    dplyr::summarise(volume = sum(volume, na.rm = TRUE), .by = c(year, label)) %>%
    ggplot2::ggplot(aes(year, volume/1000000, colour = label, group = label)) +
    ggplot2::geom_line(linewidth = 1) +
    afcharts::theme_af(legend = "bottom") +
    afcharts::scale_colour_discrete_af(labels = c(
      "Scots pine: Seedlings",
      "Sitka spruce: Seedlings",
      "Sitka spruce: VP"
    )) +
    ggplot2::scale_x_continuous(labels = planting_year) +
    ggplot2::scale_y_continuous(breaks = seq(0, gb_next_10, 10),
                       expand = ggplot2::expansion(mult = c(0, 0)),
                       limits = c(0, gb_next_10)) +
    ggplot2::labs(x = NULL,
         y = NULL,
         colour = NULL)


  return(fig_list = list(
    fig1 = fig1,
    fig2 = fig2
  ))
}
