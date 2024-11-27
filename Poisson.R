#' @title CLT for Poisson Distribution
#'
#' @param n The sample size.
#' @param iter Number of iteration.
#' @param lambda The lambda parameter.
#' @param ... Additional parameters.
#'
#' @return A histogram of sample means
#' @export
#'
#' @examples
#' mycltp(n = 10, iter = 10000, lambda = 4)
mycltp = function(n, iter, lambda = 10, ...){
  x = NULL;
  y = rpois(n * iter, lambda = lambda)
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  w = apply(data, 2, mean)
  param = hist(w, plot = FALSE)

  ymax = max(param$density)
  ymax = 1.1 * ymax
  layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE))

  hist(w, freq = FALSE, ylim = c(0, ymax), col = rainbow(max(w)), main = paste("Histogram of sample mean", "\n", "sample size= ", n, " iter=", iter, " lambda=", lambda, sep = ""), xlab = "Sample mean", ...)
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda / n)), add = TRUE, col = "Red", lty = 2, lwd = 3)
}
