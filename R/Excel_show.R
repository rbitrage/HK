#' Show data in spreadsheet
#' `r lifecycle::badge("experimental")`
#'
#' Quickly inspect your data in a spreadsheet with auto-filters and all columns formatted as numbers. Currently, this is a very tight definition of a helper function. More flexibility is planned in the future.
#'
#' @param data a `data.frame`, a `tibble` or any other object as defined in `?openxlsx::writeData()`.
#'
#' @author Harald Kaba (2021)
#' @references A wrapper around the brilliant R package `openxlsx`:
#' \url{https://ycphs.github.io/openxlsx/index.html}
#' @return This function is called for its side-effect of opening a spreadsheet.
#' @export
#'
#' @examples
#' \dontrun{
#' Excel_show_numeric(airquality) # works fine with a data.frame
#' Excel_show_numeric(WorldPhones) # but ignores row names (e.g. from arrays)}
Excel_show_numeric <- function(data) {
  workbook <-
    openxlsx::createWorkbook(
      creator = paste0("HK_", utils::packageVersion("HK")))
  openxlsx::addWorksheet(wb = workbook,
                         sheetName = deparse(substitute(data)))
  openxlsx::writeData(wb = workbook,
                      sheet = 1, # required (with no default)
                      x = data,
                      withFilter = TRUE)
  openxlsx::addStyle(wb = workbook,
                     sheet = 1, # required (with no default)
                     style =
                       openxlsx::createStyle(numFmt = "#,##0"),
                     rows = 1:(nrow(data) + 1),
                     cols = 1:ncol(data),
                     gridExpand = TRUE)
  openxlsx::setColWidths(wb = workbook,
                         sheet = 1, # required (with no default)
                         cols = 1:ncol(data),
                         widths = 15)
  openxlsx::freezePane(wb = workbook,
                       sheet = 1,
                       firstRow = TRUE,
                       firstCol = TRUE)
  openxlsx::openXL(file = workbook)

}
