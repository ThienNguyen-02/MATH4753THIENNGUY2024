#' @title Bootstrap Confidence Interval
#'
#' @param x A vector of sample data.
#' @param iter Number of bootstrap iterations.
#' @param fun The statistic function to apply.
#' @param alpha Significance level for the CI.
#' @param cx A parameter for size of text in plot.
#' @param ... Additional parameters for the histogram.
#'
#' @return A histogram of sample means
#' @export
#'
#' @examples
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n = length(x)
  x = NULL;
  y = sample(x, n * iter, replace = TRUE)
  rs.mat = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, fun)
  ci = quantile(xstat, c(alpha / 2, 1 - alpha / 2))
  list(ci = ci, fun = fun, x = x)
}
