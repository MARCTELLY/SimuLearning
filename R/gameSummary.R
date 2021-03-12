#' gameSummary
#' return result of 5 replication
#' @return list
#'
#' @export
#' @examples
#' gameSummary()
#' [[1]]
#' [1] -1
#'
#' [[2]]
#' [1] 1
#'
#' [[3]]
#' [1] -1
#'
#' [[4]]
#' [1] 1
#'
#' [[5]]
#' [1] 10
#'
#' [[6]]
#' [1] 10
#'
#' [[7]]
#' [1] 10
#'
#' [[8]]
#' [1] 10
#'
#' [[9]]
#' [1] 0.4836
#'
#' [[10]]
#' [1] 0.4862
#'
#' [[11]]
#' [1] 0.02624
#'
#' [[12]]
#' [1] 0.02793
#'
#' [[13]]
#' [1] 0
#'
#' [[14]]
#' [1] 0
#'
#' [[15]]
#' [1] 0.95635
#'
#' [[16]]
#' [1] 0.95759
#'
#' [[17]]
#' [1] 1
#'
#' [[18]]
#' [1] 1
#'
#' [[19]]
#' [1] 1
#'
#' [[20]]
#' [1] 1
#'
#' [[21]]
#' [1] 11
#'
#' [[22]]
#' [1] 31
#'
#' [[23]]
#' [1] 2
#'
#' [[24]]
#' [1] 23
gameSummary<-function(){
  betOnRed = replicate(5, gameBetOnRed()[1])
  betOnNum = replicate(5, gameBetOnNumber()[1])
  betMart = replicate(5, gameMartingaleSys()[1])
  betLab = replicate(5, gameLabouchereSys()[1])
  propOnRed = replicate(5, propWinBetOnRed())
  propOnNum = replicate(5, propWinBetOnNumber())
  propOnMart = replicate(5, propWinMartingaleSys())
  propOnLab = replicate(5, propWinLabouchereSys())
  playTimeOnRed <- 1
  playTimeOnNum <- 1
  playTimeOnMart = replicate(5, gameMartingaleSys()[2])
  playTimeOnLab = replicate(5, gameLabouchereSys()[2])



  return (list(min(betOnRed),max(betOnRed)
               ,min(betOnRed),max(betOnRed)
               ,min(betMart) ,max(betMart),min(betLab)
               ,max(betLab),min(propOnRed)
               ,max(propOnRed),min(propOnNum)
               ,max(propOnNum),min(propOnMart),max(propOnMart)
               ,min(propOnLab),max(propOnLab),min(playTimeOnRed)
               ,max(playTimeOnRed),min(playTimeOnNum)
               ,max(playTimeOnNum),min(playTimeOnMart)
               ,max(playTimeOnMart),min(playTimeOnLab)
               ,max(playTimeOnLab)))
}
