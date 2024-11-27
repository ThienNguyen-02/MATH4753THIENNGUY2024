#' @title mybin
#'
#' @description Simulates binomial distribution and produces a barplot.
#'
#' @param iter Number of iterations (default is 100).
#' @param n Sample size for each iteration (default is 10).
#' @param p Probability of success (default is 0.5).
#'
#' @return A barplot of binomial simulation results.
#' @export
#'
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#'
#' @examples
#' mybin(iter = 100, n = 10, p = 0.5)
mybin <- function(iter = 100, n = 10, p = 0.5) {
  # Create a matrix to hold the samples, initially filled with NA's
  sam.mat <- matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  # Make a vector to hold the number of successes in each trial
  succ <- numeric(iter)

  for (i in 1:iter) {
    # Fill each column with a new sample
    sam.mat[, i] <- sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))

    # Calculate the number of successes for each trial
    succ[i] <- sum(sam.mat[, i])
  }

  # Create a table of successes
  succ.tab <- table(factor(succ, levels = 0:n))

  # Create a barplot of the proportions
  graphics::barplot(succ.tab / iter, col = grDevices::rainbow(n + 1),
                    main = "Binomial simulation", xlab = "Number of successes")

  # Return proportions
  return(succ.tab / iter)
}
