testing <- function(x, ...) {
  try({
    result.sgcca <- RGCCA::sgcca(C = x,
                                 ...)
    analyze(result.sgcca)},
    silent = TRUE)
}

#' Iterate to find the best model
#'
#' Given a design and
#' @param ... All the same arguments that would be passed to sggca, pass named arguments.
#' @seealso [sgcca]
#' @importFrom RGCCA scale2
#' @examples
#' data("Russett", package = "RGCCA")
#' X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
#' X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
#' X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
#'                                   "dictator")])
#' A <- list(X_agric, X_ind, X_polit)
#' A <- lapply(A, function(x) RGCCA::scale2(x, bias = TRUE))
#' C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' out <- iterate(A = A, C = C, c1 =rep(1, 3), scheme = "factorial",
#'                scale = FALSE, verbose = FALSE,
#'                ncomp = rep(2, length(A)),
#'                bias = TRUE)
#' head(out)
iterate <- function(...) {
  l <- list(...)
  A <- lapply(l$A, scale2, bias = l$bias)
  diff0 <- which(lower.tri(l$C) & l$C != 0)
  designs <- weight_design(weights = 11, size = length(A), diff0 = diff0)
  l <- l[!names(l)  %in% c("A", "C", "scale")]

  out <- sapply(designs, function(x){do.call(testing, c(list(A = A, x = x), l))}, USE.NAMES = FALSE)
  if (is.list(out)) {
    warning("Some errors happened on the iteration.")
    out <- simplify2array(out[lengths(out) != 1])
  }
  as.data.frame(t(out))
}
