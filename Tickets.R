#' @title N-tickets
#'
#' @param N Number of seats on the flight
#' @param gamma The probability that overbooking occurs
#' @param p Probability that a ticket holder shows up
#'
#' @importFrom stats pbinom pnorm optimize
#' @importFrom graphics abline
#'
#' @return A named list with discrete and continuous calculations (`nd` and `nc`) along with the optimized `nc`.
#' @export
#'
#' @example
#' ntickets(N = 400, gamma = 0.2, p = 0.95)
ntickets <- function(N, gamma, p) {

  n <- seq(N, floor(N + (0.1 * N)), by = 1)

  discrete <- 1 - gamma - pbinom(N, n, p)

  nd <- n[which.min(abs(discrete))]

  mean <- N * p
  sd <- sqrt(N * p * (1 - p))
  n_values <- function(x) {
    1 - gamma - pnorm(N, mean = x * p, sd = sqrt(x * p * (1 - p)))
  }

  nc <- optimize(f = function(x) abs(n_values
                                         (x)), interval = c(N, floor(N + 0.1 * N)))$minimum

  output <- list(
    nd = nd,
    nc = nc,
    N = N,
    p = p,
    gamma = gamma
  )

  plot(n, discrete, type = "b", pch = 20, col = "blue", lwd = 2,
       main = paste("Objective Vs n to find optimal tickets sold\n(", round(nd, 0), ") gamma=", gamma, " N=", N, " discrete", sep = ""),
       xlab = "n", ylab = "Objective", ylim = c(min(discrete), max(discrete)))
  abline(h = 0, col = "red", lwd = 2)
  abline(v = nd, col = "red", lwd = 2)

  continuous_vals <- sapply(n, function(x) n_values(x))
  plot(n, continuous_vals, type = "l", col = "black", lwd = 2,
       main = paste("Objective Vs n to find optimal tickets sold\n(", round(nc, 2), ") gamma=", gamma, " N=", N, " continuous", sep = ""),
       xlab = "n", ylab = "Objective", ylim = c(min(continuous_vals), max(continuous_vals)))
  abline(h = 0, col = "blue", lwd = 2)
  abline(v = nc, col = "blue", lwd = 2)

  return(output)
}
