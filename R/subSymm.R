#' Substitute in a symmetric matrix
#'
#' Helper to create models, which substitute in a symmetric matrix for a value.
#' @param m The symmetric matrix
#' @param x Row position
#' @param y Column position
#' @param val Value to insert in the given position
#' @return The symmetric matrix with the value inserted in the right positions
#' @seealso `symm()`, `correct()`, `weight_design()`
#' @export
#' @examples
#' m <- matrix(0, ncol = 3, nrow = 3)
#' subSymm(m, 1, 2, 1)
subSymm <- function(m, x, y, val) {
  dm <- dimnames(m)
  check_names <- dm[[1]] == dm[[2]]
  check_size <- nrow(m) != ncol(m)
  if (!all(check_names) || check_size) {
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
#' @seealso `subSymm()`, `correct()`
#' @export
#' @importFrom methods is
#' @examples
#' m <- matrix(0, ncol = 5, nrow = 5)
#' symm(m, c(0, 1, 1, 1, 0, 0.5, 0.2, 0.3, 0.7, 0.1))
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



#' Extract model
#'
#' Once evaluated extract a model.
#' @param model A matrix with the names of the blocks.
#' @param models_eval The output of `iterate_model()` or `search_model()`.
#' @param which Either inner, outer or the row to extract the model.
#' @return A matrix with the weights on the models_eval argument.
#' @export
#' @examples
#' data("Russett", package = "RGCCA")
#' X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
#' X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
#' X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
#'                                   "dictator")])
#' A <- list(Agric = X_agric, Ind = X_ind, Polit = X_polit)
#' C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' out <- search_model(A = A, C = C, c1 =rep(1, 3), scheme = "factorial",
#'                     scale = FALSE, verbose = FALSE,  ncomp = rep(1, length(A)),
#'                     bias = TRUE, BPPARAM = BiocParallel::SerialParam())
#' extract_model(C, out, "inner")
extract_model <- function(model, models_eval, which) {
  if (is.character(which) && which %in% c("inner", "outer")) {
    w <- paste0("AVE_", which)
    row <- which.max(models_eval[[w]])
  } else if (is.numeric(which) && which != 0 && which < nrow(models_eval)) {
    row <- which
  } else {
    stop("which should be either the row index or the column of AVE to subset the models.")
  }
  columns <- grep("var", colnames(models_eval))
  # Guess the right size
  blocks <- nrow(model)
  if ((blocks^2-blocks)/2 != length(columns)) {
    stop("model do not match the evaluation data provided.")
  }
  symm(model, models_eval[row, columns])
}
