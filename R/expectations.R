#' expectedWinBetOnRed
#'  compute the expected winnings per game
#' @return numeric
#' @export
#'
#' @examples
#' > expectedWinBetOnRed()
#' [1] -0.02714
expectedWinBetOnRed <- function() {
  total_win <- 0
  i <- 1

  while (i <= 100000) {
    total_win = total_win + gameBetOnRed()[1]
    i = i + 1
  }

  return (total_win / 100000)
}

#' expectedWinBetOnNumber
#' compute the expected winnings per game
#' @return numeric
#' @export
#'
#' @examples
#' > expectedWinBetOnNumber()
#' [1] -0.0082
#'
expectedWinBetOnNumber <- function() {
  total_win <- 0
  i <- 1

  while (i <= 100000) {
    total_win = total_win + gameBetOnNumber()[1]
    i = i + 1
  }

  return (total_win / 100000)
}




#' expectedMartingaleSys
#' compute the expected winnings per game
#' @return
#' @export
#'
#' @examples
#' > expectedMartingaleSys()
#' [1] -2.15629
#'
expectedMartingaleSys <- function() {
  total_win <- 0
  i <- 1

  while (i <= 100000) {
    total_win = total_win + gameMartingaleSys()[1]
    i = i + 1
  }

  return (total_win / 100000)
}


#' expectedLabouchereSys
#' compute the expected winnings per game
#' @return numeric
#' @export
#'
#' @examples
#' > expectedMartingaleSys()
#' [1] -1.86651
#'
expectedLabouchereSys <- function() {
  total_win <-0
  i <- 1

  while (i <= 100000) {
    total_win = total_win + gameLabouchereSys()[1]
    i = i + 1
  }

  return (total_win / 100000)
}
