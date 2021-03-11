#' simuBranching
#'
#' @param gen number of generation
#' @param rv.sim random variable simulation ex. runif(n)
#' @param ... autheur paramaters
#'
#' @return a numeric
#' @export
#'
#' @examples
simuBranching <- function(gen, rv.sim, ...) {

  popGen <- 1
  for (i in 1:gen) {
    if (popGen > 0) {
      popGen <- sum(rv.sim(popGen, ...))
    }
  }
  return(popGen)
}
