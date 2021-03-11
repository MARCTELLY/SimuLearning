#' simuSIR
#'
#' Simulate a Susceptible, Infected and Removed persons in
#' in such periode and return a matrix in which each row
#' represente a given time between the beginning of spread and the final
#' time of spread
#' @param alpha infection probability
#' @param beta removal probability
#' @param initSusc initial susceptible
#' @param timeOfSpread time of spread
#'
#' @return matrix of SIR in each time (row)
#' @export
#'
#' @examples
#' > simuSIR(0.5, 0.03, 100, 10)
#' [,1] [,2] [,3]
#' [1,]  100    1    0
#' [2,]   49   52    0
#' [3,]    0  100    1
#' [4,]    0   96    5
#' [5,]    0   92    9
#' [6,]    0   87   14
#' [7,]    0   83   18
#' [8,]    0   82   19
#' [9,]    0   82   19
#' [10,]    0   81   20
#' [11,]    0   78   23
#'
simuSIR <- function(alpha, beta, initSusc, timeOfSpread) {

  # Initiation of container
  susceptible <- rep(0, timeOfSpread+1)
  infected <- rep(0, timeOfSpread+1)
  removed <- rep(0, timeOfSpread+1)

  # initiation at the beginning
  susceptible[1] <- initSusc
  infected[1] <- 1
  removed[1] <- 0

  for (i in 1:timeOfSpread) {
    susceptible[i+1] <- rbinom(1, susceptible[i], (1 - alpha)^infected[i])
    removed[i+1] <- removed[i] + rbinom(1, infected[i], beta)
    infected[i+1] <- initSusc + 1 - removed[i+1] - susceptible[i+1]
  }
  return(matrix(c(susceptible, infected, removed), ncol = 3))
}
