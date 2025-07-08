############################# EXPORT TXT ####################################


#' Save grid in a text file (txt).
#'
#' `saveAsTxt` will save the grid as a `.txt` file
#' in format used by \pkg{OpenRepGrid}. This file format can also
#' easily be edited by hand (see [importTxt()] for a
#' description).
#'
#' @param x     `repgrid` object.
#' @param file  Filename to save the grid to. The name should have
#'              the suffix `.txt`.
#' @return  Invisibly returns the name of the file.
#'
#' @note
#' Structure of a txt file that can be read by [importTxt()].
#'
#' `---------------- .txt file -----------------`
#'
#' `anything not contained within the tags will be discarded`
#'
#' \tabular{l}{
#' `ELEMENTS`         \cr
#' `element 1`        \cr
#' `element 2`        \cr
#' `element 3`        \cr
#' `END ELEMENTS`     \cr
#' \cr
#' `CONSTRUCTS`                 \cr
#' `left pole 1 : right pole 1` \cr
#' `left pole 2 : right pole 2` \cr
#' `left pole 3 : right pole 3` \cr
#' `left pole 4 : right pole 4` \cr
#' `END CONSTRUCTS`             \cr
#' \cr
#' `RATINGS`        \cr
#' `1 3 2`          \cr
#' `4 1 1`          \cr
#' `1 4 4`          \cr
#' `3 1 1`          \cr
#' `END RATINGS`    \cr
#' \cr
#' `RANGE`          \cr
#' `1 4`            \cr
#' `END RANGE`      \cr
#' }
#' `---------------- end of file ----------------`
#'
#' @export
#' @seealso  [importTxt()]
#' @examples \dontrun{
#'
#' x <- randomGrid()
#' saveAsTxt(x, "random.txt")
#' }
#'
saveAsTxt <- function(x, file = NA) {
  fileName <- file
  enames <- elements(x)
  cnames <- constructs(x)
  scores <- getRatingLayer(x)
  # write txt file
  con <- file(fileName, "w") # open an output file connection

  cat("=========================\n", file = con)
  cat("Data File for OpenRepGrid\n", file = con)
  cat("=========================\n", file = con)

  # write element names
  cat("\nELEMENTS\n", file = con)
  for (ename in enames) {
    cat(ename, "\n", sep = "", file = con)
  }
  cat("END ELEMENTS\n", file = con)

  # write construct names
  cat("\nCONSTRUCTS\n", file = con)
  cnames.string <- paste(cnames[, 1], ":", cnames[, 2])
  for (cname in cnames.string) {
    cat(cname, "\n", sep = "", file = con)
  }
  cat("END CONSTRUCTS\n", file = con)

  # write ratings to file
  cat("\nRATINGS\n", file = con)
  write.table(scores,
    file = con, sep = " ",
    row.names = FALSE, col.names = FALSE
  )
  cat("END RATINGS\n", file = con)

  # write scale range to file
  cat("\nRANGE\n", file = con)
  cat(x@scale$min, x@scale$max, "\n", file = con)
  cat("END RANGE\n", file = con)

  close(con)
  cat("grid succesfully written to file: ", unlist(fileName))

  invisible(fileName)
}


### .TXT FILE FORMAT ###

# everything not contained within the tags will be discarded
#
# ELEMENTS
# element 1
# element 2
# element 3
# element 4
# END ELEMENTS
#
# CONSTRUCTS
# left pole 1 : right pole 1
# left pole 2 : right pole 2
# left pole 3 : right pole 3
# left pole 4 : right pole 4
# END CONSTRUCTS
#
# RATINGS
# 1 3 NA 2
# 4 1 3 NA
# 1 4 1 3
# 3 1 1 6
# END RATINGS
#
# RANGE
# 1 6
# END RANGE


# exportTxt <- function(x, file=NULL)
# {
#   file <- "test.txt"
#   # redirect output to connection
#   sink(file)
#
#   # elements
#   cat("\n")
#   cat("ELEMENTS\n")
#   for (name in getElementNames(g))
#     cat(name, "\n")
#   cat("END ELEMENTS\n\n")
#
#   # constructs
#   cat("CONSTRUCTS\n")
#   for (name in getConstructNames2(g, sep=" : ", trim=NA))
#     cat(name, "\n")
#   cat("END CONSTRUCTS\n\n")
#
#   # ratings
#   cat("RATINGS\n")
#   r <- getRatingLayer(g)
#   for (i in 1:nrow(r))
#     cat(r[i, ], "\n")
#   cat("END RATINGS\n\n")
#
#   # range
#   cat("RANGE\n")
#   cat(getScale(g), "\n")
#   cat("END RANGE\n\n")
#
#   # reset output stream
#   sink()
# }


############################# EXPORT EXCEL ####################################


#' Save grids as Microsoft Excel file (.xlsx)
#'
#' `saveAsExcel` will save one or more grids in an Excel file (`.xlsx`).
#'
#' @param x A `repgrid` object or a list of grids.
#' @param file File path. Suffix must be `.xlsx`.
#' @param format Two output formats are supported: `wide` (default) where each column represents one element, each row
#'   represent one constructs (a common grid representation), and `long` where each row contains an element-construct
#'   combination and the corresponding rating value. See [importExcel()] for details and examples.
#' @param sheet Vector of sheet names with same length as `x`. If `NULL` (default), `default_sheet` is used. If `x`
#'   is a list if grids, a sequential index is appended. For named list entries (if `x` is a list of grids), the name
#'   overwrites the default sheet name.
#' @param default_sheet Default sheet name to use if not supplied in `sheet` or via list names of `x`.
#' @return  Invisibly returns file path.
#' @export
#' @seealso  [importExcel()], [saveAsWorksheet()]
#' @example inst/examples/example-save-as-excel.R
#'
saveAsExcel <- function(x, file, format = "wide", sheet = NULL, default_sheet = "grid") {
  ext <- tools::file_ext(file)
  if (ext != "xlsx") {
    stop("The file extension must be '.xlsx'. Found '", ext, "' instead.", call. = FALSE)
  }
  wb <- openxlsx::createWorkbook()
  wb <- saveAsWorksheet(x, wb = wb, format = format, sheet = sheet, default_sheet = default_sheet)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}


#' Add grids as sheets to an openxlsx Workbook
#'
#' `saveAsWorksheet` will add one or more grids to an a [openxlsx](https://CRAN.R-project.org/package=openxlsx)
#' `Workbook` object.
#'
#' @param wb A [openxlsx](https://CRAN.R-project.org/package=openxlsx) `Workbook` object.
#' @inheritParams saveAsExcel
#' @return  Invisibly returns Workbook object.
#' @export
#' @seealso  [saveAsExcel()]
#' @example inst/examples/example-save-as-worksheet.R
#'
saveAsWorksheet <- function(x, wb, format = "wide", sheet = NULL, default_sheet = "grid") {
  stop_if_not_inherits(wb, "Workbook")
  if (!is_list_of_repgrids(x)) {
    sheet <- sheet %||% default_sheet
    wb <- add_one_sheet_with_grid(x, wb = wb, format = format, sheet = sheet)
  } else {
    ii <- seq_along(x)
    list_names <- get_names_na(x)
    # sheet name precedence: sheet, list, default
    if (length(sheet) == length(x)) {
      sheets <- sheet
    } else { # if (is.null(sheet)
      default_names <- paste(default_sheet, ii)
      sheets <- ifelse(is.na(list_names), default_names, list_names)
    }
    for (i in ii) {
      wb <- add_one_sheet_with_grid(x[[i]], wb = wb, format = format, sheet = sheets[i])
    }
  }
  invisible(wb)
}



# saveAsExcelv2 <- function(x, file, format = "wide", sheet = "grid") {
#   ext <- tools::file_ext(file)
#   if (ext != "xlsx") {
#     stop("The file extension must be '.xlsx'. Found '", ext, "' instead.", call. = FALSE)
#   }
#   if (is.null(wb)) {
#     wb <- openxlsx::createWorkbook()
#   }
#   if (!is_list_of_repgrids(x)) {
#     wb <- add_one_sheet_with_grid(x, wb = wb, format = format, sheet = sheet)
#   } else {
#     ii <- seq_along(x)
#     list_names <- get_names_na(x)
#     sheets_numbered <- paste(sheet %||% "grid", ii)
#     sheets <- ifelse(is.na(list_names), sheets_numbered)
#     for (i in ii) {
#       wb <- add_one_sheet_with_grid(x[[i]], wb = wb, format = format, sheet = sheets[i])
#     }
#   }
#   openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
#   invisible(file)
# }
#
# saveAsExcel_v1 <- function(x, file, format = "wide", sheet = NULL) {
#   if (is_list_of_repgrids(x)) {
#
#   }
#
#   stop_if_not_is_repgrid(x)
#   ext <- tools::file_ext(file)
#   if (ext != "xlsx") {
#     stop("The file extension must be '.xlsx'. Found '", ext, "' instead.", call. = FALSE)
#   }
#   format <- match.arg(tolower(format), c("wide", "long"))
#
#   if (format == "wide") {
#     df <- grid_to_df_wide(x)
#     sheet <- sheet %||% "grid (wide format)"
#     jj_rows <- seq_len(nrow(df) + 1)
#     jj_elements <- seq_len(ncol(x)) + 1
#     j_left_pole <- 1
#     j_right_pole <- ncol(x) + 2
#     j_preferred <- ncol(df)
#
#     style_header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "grey90", border = "bottom")
#     style_center <- openxlsx::createStyle(halign = "center")
#     style_pole_left <- openxlsx::createStyle(textDecoration = "bold", halign = "right", border = "right")
#     style_pole_right <- openxlsx::createStyle(textDecoration = "bold", halign = "left", border = "left")
#     style_text_grey <- openxlsx::createStyle(fontColour = "grey50")
#
#     wb <- openxlsx::createWorkbook()
#
#     openxlsx::addWorksheet(wb, sheet)
#     openxlsx::writeData(wb, sheet, x = df, headerStyle = style_header, rowNames = FALSE, colNames = TRUE)
#     openxlsx::addStyle(wb, sheet, style = style_center, rows = jj_rows, cols = jj_elements, stack = TRUE, gridExpand = TRUE)
#     openxlsx::addStyle(wb, sheet, style = style_pole_left, rows = jj_rows, cols = j_left_pole, stack = TRUE, gridExpand = TRUE)
#     openxlsx::addStyle(wb, sheet, style = style_pole_right, rows = jj_rows, cols = j_right_pole, stack = TRUE, gridExpand = TRUE)
#     openxlsx::addStyle(wb, sheet, style = style_text_grey, rows = jj_rows, cols = j_preferred, stack = TRUE, gridExpand = TRUE)
#     openxlsx::setColWidths(wb, sheet, cols = c(j_left_pole, j_right_pole, j_preferred), widths = c(20, 20, 13))
#     openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
#   } else {
#     df <- grid_to_df_long(x)
#     sheet <- sheet %||% "grid (long format)"
#     jj_rows <- seq_len(nrow(df) + 1)
#     jj_cols <- seq_len(ncol(df))
#     jj_cols_optional <- 5:7
#     j_preferred <- 5
#     ii_end_construct <- rle(df$left_pole)$lengths %>% cumsum() %>% head(-1) + 1
#
#     style_header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "grey90", border = "bottom")
#     style_border_left <- openxlsx::createStyle(border = "left")
#     style_border_bottom <- openxlsx::createStyle(border = "bottom", borderStyle = "dashed")
#     style_text_grey <- openxlsx::createStyle(fontColour = "grey50")
#
#     wb <- openxlsx::createWorkbook()
#     openxlsx::addWorksheet(wb, sheet)
#     openxlsx::writeData(wb, sheet, x = df, headerStyle = style_header, rowNames = FALSE, colNames = TRUE)
#     openxlsx::addStyle(wb, sheet, style = style_border_left, rows = jj_rows, cols = j_preferred, stack = TRUE, gridExpand = TRUE)
#     openxlsx::addStyle(wb, sheet, style = style_text_grey, rows = jj_rows, cols = jj_cols_optional, stack = TRUE, gridExpand = TRUE)
#     openxlsx::addStyle(wb, sheet, style = style_border_bottom, rows = ii_end_construct, cols = jj_cols, stack = TRUE, gridExpand = TRUE)
#     openxlsx::setColWidths(wb, sheet, cols = jj_cols, widths = c(20, 20, 20, 10, 13, 10, 10))
#     openxlsx::freezePane(wb, sheet, firstActiveRow = 2)
#     openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
#   }
#   invisible(file)
# }

add_one_sheet_with_grid <- function(x, wb, format = "wide", sheet = "grid") {
  stop_if_not_is_repgrid(x)
  stop_if_not_inherits(wb, "Workbook")

  format <- match.arg(tolower(format), c("wide", "long"))

  sheet <- sheet %||% "grid"
  if (sheet %in% names(wb)) {
    stop("Worksheet ", shQuote(sheet), " already exists", call. = FALSE)
  }

  if (format == "wide") {
    df <- grid_to_df_wide(x)
    jj_rows <- seq_len(nrow(df) + 1)
    jj_elements <- seq_len(ncol(x)) + 1
    j_left_pole <- 1
    j_right_pole <- ncol(x) + 2
    j_preferred <- ncol(df)

    style_header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "grey90", border = "bottom")
    style_center <- openxlsx::createStyle(halign = "center")
    style_pole_left <- openxlsx::createStyle(textDecoration = "bold", halign = "right", border = "right")
    style_pole_right <- openxlsx::createStyle(textDecoration = "bold", halign = "left", border = "left")
    style_text_grey <- openxlsx::createStyle(fontColour = "grey50")

    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, x = df, headerStyle = style_header, rowNames = FALSE, colNames = TRUE)
    openxlsx::addStyle(wb, sheet, style = style_center, rows = jj_rows, cols = jj_elements, stack = TRUE, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet, style = style_pole_left, rows = jj_rows, cols = j_left_pole, stack = TRUE, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet, style = style_pole_right, rows = jj_rows, cols = j_right_pole, stack = TRUE, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet, style = style_text_grey, rows = jj_rows, cols = j_preferred, stack = TRUE, gridExpand = TRUE)
    openxlsx::setColWidths(wb, sheet, cols = c(j_left_pole, j_right_pole, j_preferred), widths = c(20, 20, 13))
  } else {
    df <- grid_to_df_long(x)
    jj_rows <- seq_len(nrow(df) + 1)
    jj_cols <- seq_len(ncol(df))
    jj_cols_optional <- 5:7
    j_preferred <- 5
    ii_end_construct <- rle(df$left_pole)$lengths %>%
      cumsum() %>%
      head(-1) + 1

    style_header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "grey90", border = "bottom")
    style_border_left <- openxlsx::createStyle(border = "left")
    style_border_bottom <- openxlsx::createStyle(border = "bottom", borderStyle = "dashed")
    style_text_grey <- openxlsx::createStyle(fontColour = "grey50")

    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, x = df, headerStyle = style_header, rowNames = FALSE, colNames = TRUE)
    openxlsx::addStyle(wb, sheet, style = style_border_left, rows = jj_rows, cols = j_preferred, stack = TRUE, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet, style = style_text_grey, rows = jj_rows, cols = jj_cols_optional, stack = TRUE, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet, style = style_border_bottom, rows = ii_end_construct, cols = jj_cols, stack = TRUE, gridExpand = TRUE)
    openxlsx::setColWidths(wb, sheet, cols = jj_cols, widths = c(20, 20, 20, 10, 13, 10, 10))
    openxlsx::freezePane(wb, sheet, firstActiveRow = 2)
  }
  wb
}



#' Export a grid to dataframe with wide format
#' @param x A `repgrid` object.
#' @export
#' @keywords internal
grid_to_df_wide <- function(x) {
  stop_if_not_is_repgrid(x)
  enames <- elements(x)
  cnames <- constructs(x)
  ratings <- ratings(x, names = FALSE)
  min_max <- getScale(x)
  preferred_pole <- preferredPoles(x)

  df <- cbind(cnames$leftpole, as.data.frame(ratings), cnames$rightpole, preferred_pole)
  names(df) <- c(min_max[1], enames, min_max[2], "preferred_pole")
  df
}


#' Export a grid to dataframe with long format
#' @param x A `repgrid` object.
#' @export
#' @keywords internal
grid_to_df_long <- function(x) {
  stop_if_not_is_repgrid(x)
  element <- left_pole <- right_pole <- preferred_pole <- NULL # register for R CMD CHECK
  df <- grid_to_df_wide(x)
  names(df)[c(1L, ncol(x) + 2)] <- c("left_pole", "right_pole")
  df_long <- df %>%
    tidyr::pivot_longer(-c(left_pole, right_pole, preferred_pole), names_to = "element", values_to = "rating")
  rmin <- getScale(x)["min"]
  rmax <- getScale(x)["max"]
  df_long <- df_long %>%
    dplyr::select(element, left_pole, right_pole, rating, preferred_pole) %>%
    dplyr::mutate(rmin = rmin, rmax = rmax)
  df_long
}
