#' computeSIR
#' Compute susceptible, Infected and Removote at a given time
#' @param alpha infection rate
#' @param beta removal rate
#' @param initSusc initial susceptible
#' @param timeOfSpread time of spread
#'
#' @return vector
#' @export
#'
#' @examples
#' > computeSIR(0.5, 0.02, 10, 3)
#' [1]  0 11  0
#'
computeSIR <- function(alpha, beta, initSusc, timeOfSpread) {

  susceptible <- initSusc
  infected <- 1
  removed <- 0
  for (i in 1:timeOfSpread) {
    susceptible <- rbinom(1, susceptible, (1 - alpha)^infected)
    removed <- removed + rbinom(1, infected, beta)
    infected <- initSusc + 1 - susceptible - removed
  }
  return(c(susceptible, infected, removed))
}
