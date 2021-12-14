#' Histogram and statistics
#'
#' @param data dataframe
#' @param plot_x x (defaults to second column in dataframe)
#' @param plot_y y
#' @param alpha_1 lower/left probability
#' @param alpha_2 higher/right probability
#'
#' @return
#' Histogram
#' @export
#'
#' @examples
#' hist(data = mtcars, plot_x = "wt")
hist <- function(data,
                 plot_x = NULL,
                 plot_y = NULL,
                 alpha_1 = 0.05,
                 alpha_2 = 0.95) {

  if (is.null(plot_x)) {
    x <- data[[2]]
  } else {
    x <- data[[plot_x]]
  }
  n <- sum(!is.na(x))
  mean <- mean(x)
  q_1 <- stats::quantile(x, probs = alpha_1)
  q_2 <- stats::quantile(x, probs = alpha_2)
  ggplot2::ggplot(data) +
    ggplot2::geom_histogram(ggplot2::aes(x, ggplot2::after_stat(density)),
                            bins = 50,
                            # binwidth = 1,
                            color = "grey") +
    ggplot2::geom_density(ggplot2::aes(x), color = "red") +
    # # #
    ggplot2::stat_function(mapping = ggplot2::aes(x),
                           fun = dnorm, args = list(mean = mean(x),
                                                    sd = sd(x))) +
    # # #
    ggplot2::geom_vline(xintercept = mean, color = "blue") +
    ggplot2::geom_vline(xintercept = q_1) +
    ggplot2::geom_vline(xintercept = q_2) +
    ggplot2::geom_vline(xintercept = 0, color = "green") +
    ggplot2::labs(title = paste0(deparse(substitute(data)),
                                 " (n = ", n, " in [",
                                 min(x), ", ", max(x), "], ",
                                 "mean = ", mean, ")"),
                  x = plot_x,
                  y = "density") +
    ggplot2::annotate(
      x = c(q_1,
            q_2),
      y = +Inf,
      label = c(
        paste0("q(", alpha_1, ")\n", q_1),
        paste0("q(", alpha_2, ")\n", q_2)),
      vjust = 2,
      geom = "label"
    )
}
