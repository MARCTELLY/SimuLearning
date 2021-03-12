#' branching
#'
#' @param gen generation
#' @param rv.sim simulation
#' @param ... other parameters
#'
#' @return vector
#' @export
#'
branching <- function(gen, rv.sim, ...) {
  # population of a branching process from generation 0 to gen
  # rv.sim(n, ...) simulates n rv's from the offspring distribution
  # Z[i] is population at generation i-1; Z[1] = 1
  popGen <- rep(0, gen+1)
  popGen[1] <- 1

  for (i in 1:gen) {
    if (popGen[i] > 0) {
      popGen[i+1] <- sum(rv.sim(popGen[i], ...))
    }
  }
  return(popGen)
}
