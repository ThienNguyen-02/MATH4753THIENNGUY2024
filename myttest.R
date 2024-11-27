#' @title T-Test Calculation
#'
#' @param t0 The observed t-statistic.
#' @param xmax The range of the x-axis for the t-distribution curve.
#' @param n The sample size used in the t-test.
#' @param alpha The significance level for the hypothesis test.
#'
#' @return A plot showing the rejection regions and p-value of the t-distribution.
#' @export
#'
#' @importFrom stats dt pt
#'
#' @examples
#' mypvalue(t0 = 2, xmax = 4, n = 30, alpha = 0.05)
mypvalue <- function(t0, xmax = 4, n = 20, alpha = 0.05) {
  x = NULL;
  va <- round(pt(-t0, df = n - 1), 4)
  pv <- 2 * va

  curve(dt(x, df = n-1), xlim = c(-xmax, xmax), ylab = "T Density", xlab = expression(t),
        main = substitute(paste("P-value=", pv, " alpha=", alpha)))

  xcurve <- seq(t0, xmax, length = 1000)
  ycurve <- dt(xcurve, df = n - 1)
  polygon(c(t0, xcurve, xmax), c(0, ycurve, 0), col = "Blue")

  xlcurve <- seq(-t0, -xmax, length = 1000)
  ylcurve <- dt(xlcurve, df = n - 1)
  polygon(c(-t0, xlcurve, -xmax), c(0, ylcurve, 0), col = "Blue")
}
