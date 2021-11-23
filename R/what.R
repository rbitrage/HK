#' What is this Data Frame?
#'
#' Print the output of some exploratory helper functions to console.
#'
#' @param df a data frame
#'
#' @return
#' A skim_df object, see package `skimr`.
#'
#' @references
#'
#' A wrapper around the brilliant R functions `pillar::glimpse()`, `dplyr::slice_sample()`, and `skimr::skim()`.
#'
#' @export
#'
#' @examples
#' HK::what(mtcars)
what <- function(df) {
  # rstudioapi::executeCommand("layoutZoomConsole")
  message("pillar::glimpse()")
  df |>
    pillar::glimpse()
  message("dplyr::slice_sample(n = 5)")
  df |>
    dplyr::slice_sample(n = 5) |>
    print()
  message("skimr::skim()")
  df |>
    skimr::skim()
}
