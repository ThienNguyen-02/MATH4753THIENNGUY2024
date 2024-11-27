#' @title myncurve function
#'
#' @param mu the mean value
#' @param sigma standard deviation
#' @param a shaded area
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm
#'
#' @return values of the area
#' @export
#'
#' @examples
#' myncurve(5, 10, 6)
myncurve = function(mu, sigma, a){

  x <- NULL
  # Plot the normal distribution curve
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu-3*sigma, mu+3*sigma),
        main = paste("Normal Distribution with mu =", mu, "and sigma =", sigma),
        ylab = "Density", xlab = "X")

  # Shading the area from -infinity to a
  x_values <- seq(mu-3*sigma, a, length = 1000)
  y_values <- dnorm(x_values, mean = mu, sd = sigma)
  polygon(c(x_values, a, mu-3*sigma), c(y_values, 0, 0), col = "skyblue")

  # Calculate the area under the curve (P(X <= a))
  area <- pnorm(a, mean = mu, sd = sigma)

  # Return the result in a list
  return(list(mu = mu, sigma = sigma, a = a, area = area))
}
