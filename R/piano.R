#' Piano Plot
#'
#' The piano plot, introduced in this function on Feb 15, 2022, is based on the violin plot and, likewise, supposed to improve information density of the plot.
#'
#' @param data dataframe
#' @param plot_x x (defaults to second column in dataframe)
#'
#' @return
#' A piano plot of the data.
#' @export
#'
#' @examples
#' piano(data.frame(a = 1, x = runif(100))) # a is an arbitrary first column
piano <- function(data, plot_x = NULL) {

  NULL -> ..density..

  if (is.null(plot_x)) {
    x <- data[[2]]
  } else {
    x <- data[[plot_x]]
  }
  ggplot2::ggplot(data, ggplot2::aes(x)) +
    ggplot2::geom_histogram(ggplot2::aes(x = x,
                                         y = -..density..),
                            bins = 52,
                            color = "black",
                            fill = "white") +
    ggplot2::geom_density(fill = "black") +
    ggplot2::coord_flip()
}
