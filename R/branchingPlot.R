#' branchingPlot
#'
#' @param gen generations
#' @param rv.sim simulation of random variables ex. runif(n)
#' @param ...
#' @param reps number of repetition
#' @param logplot if log?
#'
#' @return graphic
#' @export
#'
#' @examples
branchingPlot <- function(gen, rv.sim, ..., reps = 1, logplot = TRUE) {
  # simulates and plots the population of a branching process
  # from generation 0 to gen; rv.sim(n, ...) simulates n rv's
  # from the offspring distribution
  # the plot is repeated reps times
  # if logplot = TRUE then the population is plotted on a log scale
  # Z[i,j] is population at generation j-1 in the i-th repeat
  popGen <- matrix(0, nrow = reps, ncol = gen+1)
  for (i in 1:reps) {
    popGen[i,] <- branching(gen, rv.sim, ...)
  }
  if (logplot) {
    popGen <- log(popGen)
  }
  plot(c(0, gen), c(0, max(popGen)), type = "n", xlab = "generation",
       ylab = if (logplot) "log population" else "population")
  for (i in 1:reps) {
    lines(0:gen, popGen[i,])
  }
  return(invisible(popGen))
}
