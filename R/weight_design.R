#' Create design matrices with different weights
#'
#' Creates all the possible matrices given a number of blocks and a number of
#' weights. Optionally it can create just a subset of those based on a numeric
#' vector.
#' @param weights A numeric value with the number of weights to be used
#' @param size A numveric value with the numer of datasets on the design.
#' @param diff0 A Numeric vector of position which should be different from 0
#' from the lower.tri
#' @return A list of matrices with the designs with different weights
#' @export
#' @author Flodel \url{https://codereview.stackexchange.com/a/203517/36067}
#' @examples
#' out <- weight_design(4, 4)
#' head(out)
weight_design <- function(weights = 4, size, diff0 = NULL){

  p <- size * (size - 1) / 2    # 6
  w <- seq(from = 0, to = 1, length.out = weights)
  X <- matrix(1:(size*size), size, size) # pattern matrix of indices
  lt <- lower.tri(X)

  if (!is.null(diff0)) {
    w <- w[w != 0] # Filter those values that are 0
    W <- as.matrix(expand.grid(rep(list(w), length(diff0))))
    keep <- diff0 %in% X[lt]

    if (!any(keep)) {
      stop("Incorrect indices in diff0, it should be the lower.tri")
    }
    lt <- diff0
  } else {
    # all possible combinations by doing:
    W <- as.matrix(expand.grid(rep(list(w), p)))
  }
  A <- matrix(0, nrow(W), size * size)
  lower.pos <- X[lt]
  upper.pos <- t(X)[lt]

  # Replace the positions by the weights
  A[, lower.pos] <- W
  A[, upper.pos] <- W

  # A 3D array with the weights
  dim(A) <- c(nrow(W), size, size)

  # Convert to a list
  lapply(seq(nrow(A)), function(i) A[i, , ])
}
