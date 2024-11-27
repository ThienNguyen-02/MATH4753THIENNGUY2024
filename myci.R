#' @title Confidence Interval Function
#'
#' @param x A numeric vector of data values.
#'
#' @return A numeric vector containing the lower and upper bounds of the confidence interval.
#' @export
#'
#' @importFrom stats qt sd
#'
#' @examples
#' set.seed(23)
#' x <- rnorm(30, mean = 10, sd = 12)
#' myci(x)
myci <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  error <- qt(0.975, df = n - 1) * sd_x / sqrt(n)
  ci <- c(mean_x - error, mean_x + error)
  return(ci)
}
