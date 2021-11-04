#' Create design matrices with different weights
#'
#' Creates all the possible matrices given a number of blocks and a number of
#' weights. Optionally it can create just a subset of those based on a numeric
#' vector.
#' @param weights A numeric value with the number of weights to be used
#' @param size A numeric value with the number of datasets on the design.
#' @param diff0 A Numeric vector of position which should be different from 0
#' from the `lower.tri()`. See examples
#' @return A list of matrices with the designs with different weights.
#' @export
#' @author Flodel: <https://codereview.stackexchange.com/a/203517/36067>
#' @seealso `subSymm()`
#' @examples
#' out <- weight_design(4, 4)
#' head(out)
#' # Using previously defined matrix
#' C <- diag(4)
#' diag(C) <- 0
#' C <- subSymm(C, 1, 2, 0.456)
#' C <- subSymm(C, 2, 3, runif(1))
#' C <- subSymm(C, 1, 3, runif(1))
#' C <- subSymm(C, 3, 4, runif(1))
#' # Explore matrices which are similar to C
#' d <- weight_design(weights = 11, size = 4, which(lower.tri(C) & C != 0))
#' head(d)
weight_design <- function(weights = 4, size, diff0 = NULL){

  p <- size * (size - 1) / 2    # 6
  w <- seq(from = 0, to = 1, length.out = weights)
  stopifnot(is.numeric(size))
  stopifnot(size > 0)
  X <- matrix(seq_len(size*size), size, size) # pattern matrix of indices
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
