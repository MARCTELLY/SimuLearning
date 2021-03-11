#' gameBetOnRed
#'
#' run Betting on Red
#' This game involves just one bet. You bet $1 on red. If the ball lands on red
#' you win $1, otherwise you lose.
#'
#' @return vector
#' @export
#'
#' @examples
#' > gameBetOnRed()
#' [1] 1 1
gameBetOnRed <- function(){
  plate <- numeric(37)
  plate[1:36] <- rep(c('red','black'), time=18) ; plate[37]<-'green'
  if(length(which(plate=='red'))==0){
    stop("Erreur dans le choix de la couleur")
  }
  amout <- 1
  nbBet <- 1
  amoutGain <- 0
  if (sample(plate,1, replace = TRUE) == 'red') {
    amoutGain <- amout
  } else
    amoutGain <- amoutGain - amout
  return(c(amoutGain, nbBet))
}



#' gameBetOnNumber
#'
#' run Betting on a Number
#' This game involves just one bet. You bet $1 on a particular number, say
#' 17; if the ball lands on that number you win $35, otherwise you lose
#'
#' @return vector
#' @export
#'
#' @examples
#' > gameBetOnNumber()
#' [1] -1  1
gameBetOnNumber <- function(){
  amoutGain <- sample(c(35, -1),1, replace = TRUE, prob =c(1/37, 36/37))
  nbBet <- 1
  return(c(amoutGain, nbBet))
}

#' gameMartingaleSys
#'
#' Martingale System
#' In this game you start by betting $1 on red. If you lose, you double your
#' previous bet; if you win, you bet $1 again. You continue to play until you
#' have won $10, or the bet exceeds $100.
#'
#' @return vector
#' @export
#'
#' @examples
#' > gameMartingaleSys()
#' [1] 10 24
gameMartingaleSys <- function() {
  amoutGain <- 0
  nbBet <- 0
  amoutBet <- 1

  while (amoutGain < 10 && amoutBet < 100) {
    current_winnings = gameBetOnRed()[1]
    if (current_winnings == -1) {
      amoutGain = amoutGain - amoutBet
      amoutBet = amoutBet * 2
    }
    else{
      amoutGain = amoutGain + amoutBet
      amoutBet = 1
    }
    nbBet = nbBet + 1
  }

  return(c(amoutGain, nbBet))
}


#' gameLabouchereSys
#'
#' Labouchere System
#' In this game you start with the list of numbers (1, 2, 3, 4). You bet the sum
#' of the first and last numbers on red (initially $5). If you win you delete the
#' first and last numbers from the list (so if you win your first bet it becomes
#' (2,3)), otherwise you add the sum to the end of your list (so if you lose
#' list is empty, or the bet exceeds $100. If only one number is left on the list,
#' you bet that number.
#'
#' @return vector
#' @export
#'
#' @examples
#' > gameLabouchereSys()
#' [1] 10  8
gameLabouchereSys <- function(){
  listNumber = c(1, 2, 3, 4)
  amoutGain <- 0
  amoutBet = listNumber[1] + listNumber[length(listNumber)]
  nbBet <- 0

  while (length(listNumber) >= 1 && amoutBet < 100) {
    current_winnings <- sample(c(-amoutBet, amoutBet),1,replace = T,
        prob = c(19 / 37, 18 / 37))

    if (current_winnings > 0){
      amoutGain = amoutGain + current_winnings
      listNumber = listNumber[-c(1, length(listNumber))]

      if (length(listNumber) == 1){
        amoutBet <- listNumber[1]

      }

      else {
        amoutBet <- listNumber[1] + listNumber[length(listNumber)]
      }

    }

    else{
      amoutGain = amoutGain + current_winnings
      listNumber = c(listNumber, abs(current_winnings))
      amoutBet = listNumber[1] + listNumber[length(listNumber)]
    }

    nbBet = nbBet + 1

  }

  return (c(amoutGain, nbBet))

}

