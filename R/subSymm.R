#' Subsitute in a symmetric matrix
#'
#' @param m The symmetric matrix
#' @param x Row position
#' @param y Column position
#' @param val Value to insert in the given position
#' @return The symmetric matrix with the value inserted in the right positions
#' @seealso \code{\link{symm}}, \code{\link{correct}}
#' @export
subSymm <- function(m, x, y, val) {
  if (!isSymmetric(m)) {
    stop("m should be a symmetric matrix.")
  }
  m[x, y] <- val
  m[y, x] <- val
  m
}




#' Create symmetric matrix
#'
#' @param m Square numeric matrix.
#' @param data Numeric values of the upper triangular side of the matrix
#' @note After the upper case there can be other values that are assumed to be
#' in the diagonal.
#' @return A square symmetric matrix.
#' @seealso \code{\link{subSymm}}, \code{\link{correct}}
#' @export
#' @importFrom methods is
symm <- function(m, data) {

  if (!is(data, "numeric")) {
    data <- unlist(data)
  }
  weights <- (ncol(m)^2 - ncol(m))/2
  upper <- data[seq_len(weights)]
  if (length(data) > weights) {
    Diag <- rep(0, ncol(m))
    Diag[seq_len(length(data) - weights)] <- data[seq_along(data) > weights]
    diag(m) <- Diag
  }
  m[upper.tri(m)] <- upper
  as.matrix(Matrix::forceSymmetric(m, "U"))
}
