#' propWinBetOnRed
#' compute the proportion of games you win
#' @return numeric
#' @export
#'
#' @examples
#' propWinBetOnRed()
#' [1] 0.48502
#'
propWinBetOnRed <- function(){
  win <- 0
  i <- 0
  while(i <100000){
    if(gameBetOnRed()[1]==1){
      win <- win +1
    }
    i <- i + 1
  }
  return(win/100000)
}

#' propWinBetOnNumber
#' compute the proportion of games you win
#' @return numeric
#' @export
#'
#' @examples
#' > propWinBetOnNumber()
#' [1] 0.02697
#'
propWinBetOnNumber <- function(){
  win <- 0
  i <- 0
  while(i < 100000){
    if(gameBetOnNumber()[1]!=-1){
      win <- win +1
    }
    i <- i + 1
  }
  return(win/100000)
}

#' propWinMartingaleSys
#' compute the proportion of games you win
#' @return numeric
#' @export
#'
#' @examples
#' > propWinMartingaleSys()
#' [1] 0
#'
propWinMartingaleSys<- function(){
  win <- 0
  i <- 0
  while(i < 100000){
    if(gameBetOnNumber()[1]==10){
      win <- win +1
    }
    i <- i + 1
  }
  return(win/100000)
}

#' propWinLabouchereSys
#' compute the proportion of games you win
#'
#' @return numeric
#' @export
#'
#' @examples
#' > propWinLabouchereSys()
#' [1] 0.95786
#'
propWinLabouchereSys <- function(){
  win <- 0
  i <- 0
  while(i < 100000){
    if(gameLabouchereSys()[1]>0){
      win <- win +1
    }
    i <- i + 1
  }
  return(win/100000)
}

