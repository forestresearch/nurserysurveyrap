#' Output publication tables to publication ready Excel spreadsheet
#'
#' @author Kirsten Piller
#'
#' @description
#' Output publication tables to publication ready Excel spreadsheet.
#'
#' 'in_data' tables should be rounded to the required decimal places already.
#'
#' @param in_data List of input tibbles or data frames for the a11ytable tables.
#' @param latest_year Latest year of data.
#' @param pub_date Publication date, string. Date format example: 6 October 2030.
#' @param pub_name Name of publication, which will determine the formatting. One of either "ukwpt" (UK Wood production and Trade) or "fs" (Forestry Statistics).
#' @param next_update Next update date, string. Date format example: 6 October 2030.
#' @param out_path Path to output folder.
#' @param out_name_wb Name for output .xlsx workbook.
#' @param replace_wb Boolean. If workbook already exists, TRUE replaces the workbook, FALSE will load the existing workbook. Default is FALSE.
#' @param which_chap Number of the chapter to run.
#' @param rnd_no_dec Vector containing TRUE or FALSE to indicate whether the values in that table of 'in_data' should be rounded to 0 decimal places, which the majority of tables usually are. Default is TRUE for all. Vector length needs to be the same as the list of input tables, 'in_data'.
#' @param cont_hyperlink Boolean. If TRUE, replaces sheet names in Contents sheet with hyperlinks to those sheets. Default is FALSE.
#' @param a11y_obj Optional. Supply list of objects required to run a11ytables if not using Forestry Statistics or UKWPT specific function pub_a11y_prep. Default is NULL.
#'
#' @importFrom magrittr "%>%"
#' @importFrom openxlsx saveWorkbook insertImage addStyle addStyle
#' @importFrom purrr map2 map map_lgl
#' @importFrom dplyr rename all_of if_else mutate pull
#' @importFrom tibble tibble
#' @import a11ytables
#'
#' @export
#'
output_pub_tab <- function(in_data,
                           latest_year,
                           pub_date,
                           pub_name = "Nursery Survey",
                           next_update,
                           out_path,
                           out_name_wb,
                           replace_wb = FALSE,
                           rnd_no_dec = NULL,
                           cont_hyperlink = FALSE,
                           a11y_obj = NULL) {

  # check arguments
  if (is.null(rnd_no_dec) == TRUE) {
    rnd_no_dec <- rep(TRUE, length(in_data))
  }

  if (length(in_data) != length(rnd_no_dec)) {
    stop("Length of 'rnd_no_dec' is not equal to the number of tables submitted for 'in_data.")
  }

  if (all(rnd_no_dec %in% c(TRUE, FALSE)) == FALSE) {
    stop("'rnd_no_dec' should only contain TRUE or FALSE.")
  }

  # check if file already exists
  output_data_xlsx_check(out_type = "pub",
                         out_path = out_path,
                         out_name = out_name_wb,
                         replace_wb = replace_wb)

  # prepare a11ytables contents and other formatting info
  if (is.null(a11y_obj) == TRUE) {
    pub_a11y_other <- pub_a11y_prep(pub_name = pub_name,
                                    pub_date = pub_date)
  } else {
    pub_a11y_other <- a11y_obj
  }

  # rename columns of data tables
  if (length(pub_a11y_other$table_colnames) != length(in_data)) {
    stop(paste0("ensure that the number of tables for 'in_data' provided to 'output_pub_tab' is the same as the number of column name vectors created in 'pub_xlsx_colnames_prep' (", length(pub_a11y_other$table_colnames), ")."))
  }

  if (all(names(pub_a11y_other$table_colnames) != names(in_data))) {
    stop(paste0("ensure that the 'in_data' list element names provided to 'output_pub_tab' are the same as the names of the list elements created in 'pub_xlsx_colnames_prep' (", paste0(names(pub_a11y_other$table_colnames), collapse=", "), ")."))
  }

  in_data <- purrr::map2(in_data, pub_a11y_other$table_colnames,
                         ~ .x %>% dplyr::rename(dplyr::all_of(.y)))

  # Combine components into a11ytable object
  a11ytable_prepd <-
    a11ytables::create_a11ytable(
      tab_titles = pub_a11y_other$tab_titles,
      sheet_types = pub_a11y_other$sheet_types,
      sheet_titles = pub_a11y_other$sheet_titles,
      blank_cells = pub_a11y_other$blank_cells,
      custom_rows = pub_a11y_other$custom_rows,
      sources = pub_a11y_other$sources,
      tables = c(list(pub_a11y_other$cover_list, pub_a11y_other$contents_df, pub_a11y_other$notes_df),
                 in_data)
    )

  # generate the workbook
  out_wb <- a11ytables::generate_workbook(a11ytable_prepd)

  # create list of vectors of variables per table to add style to (comma for thousands), then start rows for each table
  thou_cols <- purrr::map(in_data,
                          ~  which((purrr::map_lgl(.x, is.numeric)) & (names(.x) != "Year")))

  num_start_row <- tibble::tibble(extra_row = purrr::map_lgl(in_data,
                                                             ~ any(grepl( "note", names(.x)))) + !is.na(a11ytable_prepd$blank_cells[4:(length(in_data) + 3)])) %>% # check which tables have note in column name or note on blank cells (both take up one row between them)
    dplyr::mutate(start_row = dplyr::if_else(extra_row == 1, 4, 3)) %>% # 4 and 3 assume only one row for sources and one row for custom notes
    dplyr::pull(start_row)

  # add commas to thousands columns, except year variable
  style_thou <- openxlsx::createStyle(numFmt = "#,##0")

  for (i in 1:length(a11ytable_prepd$tab_title[4:(length(in_data) + 3)])) {
    if (rnd_no_dec[i] == TRUE) {
      sheet_i <- i + 3
      openxlsx::addStyle(wb = out_wb, sheet = sheet_i, style = style_thou,
                         rows = num_start_row[i]:100, cols = thou_cols[[i]], # assumes no more than 100 rows
                         gridExpand = T, stack = T)
    }
  }

  # add branding/official statistics images
  openxlsx::insertImage(wb = out_wb, sheet = "Cover",
                        file = "../forestry_statistics/inst/img/FRlogo_linear_colour_transparent.png",
                        startRow = 1, startCol = 4, height = 161, width = 791, units = "px")

  openxlsx::insertImage(wb = out_wb, sheet = "Cover",
                        file = "../forestry_statistics/inst/img/accredited_official_statistics_logo_english_hq.png",
                        startRow = 1, startCol = 7, height = 400, width = 400, units = "px")

  # add hyperlinks for contents sheet
  if (cont_hyperlink == TRUE) {
    row_in <- 4
    for (i in 1:length(pub_a11y_other$contents_df$`Sheet name`)) {
      openxlsx::writeFormula(
        out_wb, "Contents",
        startRow = row_in, startCol = 2,
        x = openxlsx::makeHyperlinkString(
          sheet = pub_a11y_other$contents_df$`Sheet name`[i],
          text = pub_a11y_other$contents_df$`Sheet title`[i]
        )
      )
      row_in <- row_in + 1
    }
  }

  # save workbook
  openxlsx::saveWorkbook(out_wb, paste0(out_path, "/", out_name_wb, ".xlsx"), overwrite = TRUE)

}

#' Checking file status prior to adding data to new or existing Excel spreadsheet for diagnostics checks or publication
#'
#' @author Kirsten Piller
#'
#' @description
#' Checking file status prior to adding data to new or existing Excel spreadsheet for diagnostics checks or publication. Will check for existence of file first.
#'
#' @param out_path Path to output Excel spreadsheet to.
#' @param out_name Name of output Excel spreadsheet.
#' @param replace_wb Boolean. If workbook already exists, TRUE replaces the workbook, FALSE will load the existing workbook. Default is FALSE.
#' @param out_type One of "pub" or "diag". "pub" uses a11ytables so different checks.
#' @param replace_sheet Boolean. If workbook already exists, TRUE replaces the sheet, FALSE will stop the function. Default is NULL.
#'
#' @importFrom openxlsx createWorkbook addWorksheet loadWorkbook getSheetNames removeWorksheet saveWorkbook protectWorkbook
#'
#' @export
#'
output_data_xlsx_check <- function(
    out_type,
    out_path,
    out_name,
    replace_wb = FALSE,
    replace_sheet = NULL) {

  ##### check for existing file and sheet #####

  out_file_name <- paste0(out_path, "/", out_name, ".xlsx")

  if (out_type == "pub") {
    if ((file.exists(out_file_name) == TRUE) & (replace_wb == FALSE)) {
      stop("workbook already exists and replace_workbook argument is currently set to FALSE so workbook will not be overwritten.")
    }
  }

  if (out_type == "diag") {

    if (is.null(replace_sheet) == TRUE) {
      stop("provide TRUE or FALSE for 'replace_sheet' argument of 'output_data_xlsx_setup' function when 'out_type' is 'diag'")
    }

    if (file.exists(out_file_name) == FALSE) {
      # create workbook
      out_wb <- openxlsx::createWorkbook()
      # Create sheet in workbook to contain main data
      for (i in 1:length(sheet_name)) {
        openxlsx::addWorksheet(out_wb, sheetName = sheet_name[i])
      }
    } else {
      if (replace_wb == TRUE) {
        # create workbook
        out_wb <- openxlsx::createWorkbook()
        # Create sheet in workbook to contain main data
        for (i in 1:length(sheet_name)) {
          openxlsx::addWorksheet(out_wb, sheetName = sheet_name[i])
        }
      } else {
        out_wb <- openxlsx::loadWorkbook(out_file_name)
        for (i in 1:length(sheet_name)) {
          if ((sheet_name[i] %in% openxlsx::getSheetNames(out_file_name) == FALSE)) {
            openxlsx::addWorksheet(out_wb, sheetName = sheet_name[i])
          } else {
            if (replace_sheet == TRUE) { # if sheet exists in existing workbook and replace
              openxlsx::removeWorksheet(out_wb, sheet = sheet_name[i])
              openxlsx::addWorksheet(out_wb, sheetName = sheet_name[i])
            } else {
              stop("sheet already exists and replace_sheet argument is currently set to FALSE so sheet will not be overwritten.")
            }
          }
        }
      }
    }
  }
}


#' Add data to new or existing Excel spreadsheet for diagnostics checks
#'
#' @author Kirsten Piller
#'
#' @description
#' Add data to a new or existing Excel spreadsheet:
#' * a data frame or tibble containing a year based time series to new or existing Excel spreadsheet for diagnostics checks.
#'
#' Diagnostics will have a simpler format designed for interaction and options to add summary statistics. Publication will have a publication ready format.
#'
#' @param data_in A data frame or tibble, or list of them for "pub_data". The first column needs to contain the year, and be called "year" in any case. If out_type = "pub", to work with a11ytables the list must contain first three data frames or tibbles; the first element for the cover sheet, second element for the contents sheet and third for the notes sheet. Then followed by any tables of data.
#' @param year_var Name of variable containing year in data frame or tibble.
#' @param data_out_names Vector of names for columns output to spreadsheet tables, or list of them for "pub_data" if data_in is a list. Length the same as the number of columns in 'data_in' and column order the same as for 'data_in'.
#' @param sheet_name Text name of sheet or, if out_type = "pub_cover", a vector containing two names for the Cover and Notes sheets respectively.
#' @param out_path Path to output Excel spreadsheet to.
#' @param out_name Name of output Excel spreadsheet.
#' @param main_table_text Vector containing text to display in the rows below the main table. Each element of the vector is added on a new row.
#' @param diag_inc_perc_chg If out_type = "diag". Boolean, TRUE includes table of percentage change over last five years of 'data_in' below main table. Default is FALSE.
#' @param diag_inc_perc_tot If out_type = "diag". Boolean, TRUE includes table of percentage of total over last five years of 'data_in' below main table. Default is FALSE.
#' @param replace_wb Boolean. If workbook already exists, TRUE replaces the workbook, FALSE will load the existing workbook. Default is FALSE.
#' @param replace_sheet Boolean. If workbook already exists, TRUE replaces the sheet, FALSE will stop the function. Default is FALSE.
#' @param sheet_title Text title of sheet, to appear in first row and column.
#' @param main_title Text title of main data table, to appear above the table.
#' @param latest_year_stage String to add to sheet that indicates the stage of the latest year's processing. One of: eg (expert group), prov (provisional), qa (quality assured), rev (revised) or final.
#' @param latest_year Latest year of data.
#' @param pub_date Publication date, string. Date format example: 6 October 2030. Default is NULL.
#' @param next_update Next update date, string. Date format example: 6 October 2030. Default is NULL.
#'
#' @importFrom openxlsx createWorkbook addWorksheet loadWorkbook getSheetNames removeWorksheet saveWorkbook protectWorkbook
#'
#' @export
#'
output_data_diag_xlsx <- function(data_in,
                                  sheet_name = NULL,
                                  out_path,
                                  out_name,
                                  latest_year,
                                  latest_year_stage = NULL,
                                  pub_date = NULL,
                                  next_update = NULL,
                                  data_out_names = NULL,
                                  year_var = NULL,
                                  main_table_text = NULL,
                                  diag_inc_perc_chg = FALSE,
                                  diag_inc_perc_tot = FALSE,
                                  replace_wb = FALSE,
                                  replace_sheet = FALSE,
                                  sheet_title = NULL,
                                  main_title = NULL) {

  ##### check argument inputs #####

  if (is.null(data_out_names)) {
    if (ncol(data_in) != length(data_out_names)) {
      stop("'data_out_names' does not contain enough elements to name the columns of 'data_in'.")
    }
  }

  if ((is.null(latest_year_stage) == FALSE) & (!(latest_year_stage %in% c("eg", "prov", "qa", "rev", "final")) == TRUE) == TRUE) {
    stop("please indicate the stage of processing for argument 'latest_year_stage' for diagnostics.")
  }

  ##### check for existing file and sheet #####

  out_file_name <- paste0(out_path, "/", out_name, ".xlsx")

  if (file.exists(out_file_name) == FALSE) {
    # create workbook
    out_wb <- openxlsx::createWorkbook()
    # Create sheet in workbook to contain main data
    for (i in 1:length(sheet_name)) {
      openxlsx::addWorksheet(out_wb, sheetName = sheet_name[i])
    }
  } else {
    if (replace_wb == TRUE) {
      # create workbook
      out_wb <- openxlsx::createWorkbook()
      # Create sheet in workbook to contain main data
      for (i in 1:length(sheet_name)) {
        openxlsx::addWorksheet(out_wb, sheetName = sheet_name[i])
      }
    } else {
      out_wb <- openxlsx::loadWorkbook(out_file_name)
      for (i in 1:length(sheet_name)) {
        if ((sheet_name[i] %in% openxlsx::getSheetNames(out_file_name) == FALSE)) {
          openxlsx::addWorksheet(out_wb, sheetName = sheet_name[i])
        } else {
          if (replace_sheet == TRUE) { # if sheet exists in existing workbook and replace
            openxlsx::removeWorksheet(out_wb, sheet = sheet_name[i])
            openxlsx::addWorksheet(out_wb, sheetName = sheet_name[i])
          } else {
            stop("sheet already exists and replace_sheet argument is currently set to FALSE so sheet will not be overwritten.")
          }
        }
      }
    }
  }

  # formatting
  out_wb <- format_ts_diag(data_in = data_in,
                           wb_in = out_wb,
                           year_var = year_var,
                           data_out_names = data_out_names,
                           sheet_name = sheet_name,
                           latest_year_stage = latest_year_stage,
                           main_table_text = main_table_text,
                           inc_perc_chg = diag_inc_perc_chg,
                           inc_perc_tot = diag_inc_perc_tot,
                           sheet_title = sheet_title,
                           main_title = main_title)

  # Save the workbook to a file
  openxlsx::saveWorkbook(out_wb, file = out_file_name, overwrite = TRUE)
}

#' Format table with year based time series to new or existing Excel spreadsheet for diagnostics checks
#'
#' @author Kirsten Piller
#'
#' @description
#' Format table with year based time series to new or existing Excel spreadsheet for diagnostics checks. There are some
#' custom formatting options available. See https://ycphs.github.io/openxlsx/reference/index.html for
#' further spreadsheet customisation options.
#'
#' @param data_in Data table as data frames or tibbles. The first column needs to contain the year, and be called "year" in any case.
#' @param wb_in Workbook object to write to.
#' @param year_var Name of variable containing year in data frame or tibble.
#' @param data_out_names Vector of names for columns output to spreadsheet tables, length the same as the number of columns in 'data_in' and column order the same as for 'data_in'.
#' @param sheet_name Text name of sheet.
#' @param main_table_text Vector containing text to display in the rows below the main table. Each element of the vector is added on a new row.
#' @param inc_perc_chg Boolean, TRUE includes table of percentage change over last five years of 'data_in' below main table. Default is FALSE.
#' @param inc_perc_tot Boolean, TRUE includes table of percentage of total over last five years of 'data_in' below main table. Default is FALSE.
#' @param sheet_title Text title of sheet, to appear in first row and column.
#' @param main_title Text title of main data table, to appear above the table.
#' @param latest_year_stage String to add to sheet that indicates the stage of the latest year's processing. One of: eg (expert group), prov (provisional), qa (quality assured), rev (revised) or final.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr rename_with
#' @importFrom openxlsx modifyBaseFont createStyle writeData addStyle writeDataTable setColWidths
#'
#' @return out_wb - Workbook object
#'
#' @export
#'
format_ts_diag <- function(data_in,
                           wb_in,
                           sheet_name,
                           latest_year_stage = NULL,
                           data_out_names = NULL,
                           year_var,
                           main_table_text = NULL,
                           inc_perc_chg = FALSE,
                           inc_perc_tot = FALSE,
                           sheet_title = "",
                           main_title = "") {

  ##### set up styles #####

  # default font style
  openxlsx::modifyBaseFont(wb_in, fontSize = 12, fontColour = "black", fontName = "Verdana")

  # main header style
  header1Style <- openxlsx::createStyle(
    fontSize = 14, fontColour = "black", halign = "left", textDecoration = c("BOLD")
  )

  # sub header style
  header2Style <- openxlsx::createStyle(
    fontSize = 12, fontColour = "black", halign = "left", textDecoration = c("BOLD")
  )

  # Date last updated style
  date_style <- openxlsx::createStyle(
    fontSize = 12, fontColour = "black", halign = "center", numFmt = c("DD-MM-YYYY"),
    border = "TopBottomLeftRight", borderColour = "black", borderStyle = "dashed"
  )

  # processing stage style
  stage_style <- openxlsx::createStyle(
    fontSize = 12, fontColour = "black", halign = "center",
    border = "TopBottomLeftRight", borderColour = "black", borderStyle = "dashed"
  )

  ##### add titles and date #####

  # add title for sheet
  openxlsx::writeData(wb_in, sheet = sheet_name, startCol = 1, startRow = 1, sheet_title)
  openxlsx::addStyle(wb_in, sheet = sheet_name, style = header1Style, rows = 1, cols = 1)

  # add date last updated (current date) to sheet
  openxlsx::writeData(wb_in, sheet = sheet_name, startCol = 1, startRow = 2, "Date last updated:")
  openxlsx::addStyle(wb_in, sheet = sheet_name, style = header2Style, rows = 2, cols = 1)
  openxlsx::writeData(wb_in, sheet = sheet_name, startCol = 3, startRow = 2,  Sys.Date())
  openxlsx::addStyle(wb_in, sheet = sheet_name, style = date_style, rows = 2, cols = 3)

  # add processing stage for latest year of
  if (is.null(latest_year_stage) == FALSE) {
    if (latest_year_stage == "eg") {
      stage_name <- "Expert group"
    } else if (latest_year_stage == "prov") {
      stage_name <- "Provisional"
    } else if (latest_year_stage == "qa") {
      stage_name <- "Quality assured"
    } else if (latest_year_stage == "rev") {
      stage_name <- "Revised"
    } else if (latest_year_stage == "final") {
      stage_name <- "Final"
    }

    openxlsx::writeData(wb_in, sheet = sheet_name, startCol = 5, startRow = 2, "Processing stage:")
    openxlsx::addStyle(wb_in, sheet = sheet_name, style = header2Style, rows = 2, cols = 5)
    openxlsx::writeData(wb_in, sheet = sheet_name, startCol = 7, startRow = 2, stage_name)
    openxlsx::addStyle(wb_in, sheet = sheet_name, style = stage_style, rows = 2, cols = 7)
  }

  # add title for main table
  openxlsx::writeData(wb_in, sheet = sheet_name, startCol = 1, startRow = 4, main_title)
  openxlsx::addStyle(wb_in, sheet = sheet_name, style = header2Style, rows = 4, cols = 1)

  #### CHECK add table value unit

  ##### add data and text notes #####

  # add total column to input table and set year variable as first column
  data_in_tot <- data_in %>% dplyr::arrange(!!year_var)
  data_in_tot$Total <- rowSums(data_in_tot[, c(2:ncol(data_in_tot))], na.rm = TRUE)

  if (is.null(data_out_names) == FALSE) {
    data_in_tot_nm <- data_in_tot %>% dplyr::rename_with(~ data_out_names, dplyr::all_of(colnames(data_in)))
  } else {
    data_in_tot_nm <- data_in_tot
  }

  # Add main table
  openxlsx::writeDataTable(wb_in, sheet = sheet_name, x = data_in_tot_nm,
                           colNames = TRUE,
                           tableStyle = "TableStyleLight9",
                           startRow = 5, startCol = 1)
  openxlsx::addStyle(wb_in, sheet_name, style = createStyle(numFmt = "#,##0"), cols = 2:ncol(data_in_tot),
                     rows = 5:(5 + nrow(data_in)), gridExpand = TRUE)

  # text for below main table
  if(is.null(main_table_text) == FALSE) {

    main_text_length <- length(main_table_text)
    main_text <- data.frame(main_table_text)

    # Add main table text
    openxlsx::writeData(wb_in, sheet = sheet_name, x = main_text,
                        colNames = FALSE,
                        startRow = nrow(data_in) + 6, startCol = 1)
  } else {
    main_text_length <- 0
  }

  if((is.null(inc_perc_chg) == FALSE) | (is.null(inc_perc_tot) == FALSE)) {
    # Create a percent style
    pct_style <- openxlsx::createStyle(numFmt = "0%")
  }

  if(is.null(inc_perc_chg) == FALSE) {
    #  Add percentage of total for each year over last 5 years
    data_in_tot5 <- perc_oftotal_nyr(data_in, year_var = year_var, nyear = 5)

    if (is.null(data_out_names) == FALSE) {
      data_in_tot5_nm <- data_in_tot5 %>% dplyr::rename_with(~ data_out_names, dplyr::all_of(colnames(data_in)))
    } else {
      data_in_tot5_nm <- data_in_tot5
    }

    openxlsx::writeDataTable(wb_in, sheet = sheet_name, x = data_in_tot5_nm,
                             colNames = TRUE,
                             tableStyle = "TableStyleLight9",
                             startRow = nrow(data_in) + 8 + main_text_length, startCol = 1)

    # Add the percent style to cells
    openxlsx::addStyle(wb_in, sheet_name, style = pct_style, cols = 2:ncol(data_in_tot5),
                       rows = (nrow(data_in) + 8 + main_text_length):(nrow(data_in) + 8 + nrow(data_in_tot5) + main_text_length),
                       gridExpand = TRUE)

    # add title
    openxlsx::writeData(wb_in, sheet = sheet_name, startCol = 1, startRow = (nrow(data_in) + 7 + main_text_length), "Percentage of total over last five years")
    openxlsx::addStyle(wb_in, sheet = sheet_name, style = header2Style, rows = (nrow(data_in) + 7 + main_text_length), cols = 1)
  }

  if(is.null(inc_perc_tot) == FALSE) {
    # Add percentage changes over last 5 years
    if(is.null(inc_perc_chg) == FALSE) {
      perc_tot_row <- 16
    } else {
      perc_tot_row <- 8
    }
    data_in_perc5 <- perc_change_5yr(data_in_tot, year_var = year_var)

    if (is.null(data_out_names) == FALSE) {
      data_in_perc5_nm <- data_in_perc5 %>% dplyr::rename_with(~ data_out_names, dplyr::all_of(colnames(data_in)))
    } else {
      data_in_perc5_nm <- data_in_perc5
    }

    openxlsx::writeDataTable(wb_in, sheet = sheet_name, x = data_in_perc5_nm,
                             colNames = TRUE,
                             tableStyle = "TableStyleLight9",
                             startRow = nrow(data_in) + perc_tot_row + main_text_length, startCol = 1)

    # Add the percent style to cells
    openxlsx::addStyle(wb_in, sheet_name, style = pct_style, cols = 2:ncol(data_in_perc5),
                       rows = (nrow(data_in) + perc_tot_row + main_text_length):(nrow(data_in) + perc_tot_row + nrow(data_in_perc5) + main_text_length),
                       gridExpand = TRUE)

    # add title
    openxlsx::writeData(wb_in, sheet = sheet_name, startCol = 1, startRow = (nrow(data_in) + perc_tot_row + main_text_length - 1), "Percentage change over last five years")
    openxlsx::addStyle(wb_in, sheet = sheet_name, style = header2Style, rows = (nrow(data_in) + perc_tot_row + main_text_length - 1), cols = 1)
  }

  # Change column width
  openxlsx::setColWidths(wb_in, sheet = sheet_name, cols = c(1:ncol(data_in)), widths = 11)

  return(out_wb = wb_in)
}
