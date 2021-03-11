#' forestFire
#'
#' @param X data
#' @param apha probability to infect on each step
#' @param beta probability to be removed
#' @param pausing
#'
#' @return
#' @export
#'
#' @examples
forestFire <- function(X, apha, beta, pausing = FALSE) {
  # simulate forest fire epidemic model
  # X[i, j] = 2 for susceptible; 1 for infected; 0 for removed
  # set up plot
  plot(c(1,nrow(X)), c(1,ncol(X)), type = "n", xlab = "", ylab = "")
  forestFirePlot(X)
  # main loop
  burning <- TRUE
  while (burning) {
    burning <- FALSE
    # check if pausing between updates
    if (pausing) {
      input <- readline("hit any key to continue")
    }
    # update
    B <- X
    for (i in 1:nrow(X)) {
      for (j in 1:ncol(X)) {
        if (X[i, j] == 2) {
          if (runif(1) > (1 - apha)^neighbours(X, i, j)) {
            B[i, j] <- 1
          }
        } else if (X[i, j] == 1) {
          burning <- TRUE
          if (runif(1) < beta) {
            B[i, j] <- 0
          }
        }
      }
    }
    X <- B
    # plot
    forestFirePlot(X)
  }
  return(X)
}
