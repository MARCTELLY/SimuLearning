#' neighbours
#'
#' @param A grid of
#' @param i,j coordinate of grid
#'
#' @return number of neighbours
#' @export
#'
neighbours <- function(A, i, j) {
  # calculate number of neighbours of A[i,j] that are infected
  # we have to check for the edge of the grid
  nbrs <- 0

  # sum across row i - 1
  if (i > 1) {
    if (j > 1) nbrs <- nbrs + (A[i-1, j-1] == 1)
    nbrs <- nbrs + (A[i-1, j] == 1)
    if (j < ncol(A)) nbrs <- nbrs + (A[i-1, j+1] == 1)
  }
  # sum across row i
  if (j > 1) nbrs <- nbrs + (A[i, j-1] == 1)
  nbrs <- nbrs + (A[i, j] == 1)
  if (j < ncol(A)) nbrs <- nbrs + (A[i, j+1] == 1)
  # sum across row i + 1

  if (i < nrow(A)) {
    if (j > 1) nbrs <- nbrs + (A[i+1, j-1] == 1)
    nbrs <- nbrs + (A[i+1, j] == 1)
    if (j < ncol(A)) nbrs <- nbrs + (A[i+1, j+1] == 1)
  }
  return(nbrs)
}
