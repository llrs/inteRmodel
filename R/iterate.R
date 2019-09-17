testing <- function(x, ...) {
  try({
    result.sgcca <- RGCCA::sgcca(C = x,
                                 ...)
    analyze(result.sgcca)},
    silent = TRUE)
}

#' Functions related to finding models
#'
#' Look for every variation of the models changing the weights by 0.1.
#' @param ... All the same arguments that would be passed to sggca, pass named arguments.
#' @param nWeights The number of weights used to check the possible designs.
#' @seealso \code{sgcca}
#' @importFrom RGCCA scale2
#' @return A matrix with the design of the model
#' @examples
#' data("Russett", package = "RGCCA")
#' X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
#' X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
#' X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
#'                                   "dictator")])
#' A <- list(Agric = X_agric, Ind = X_ind, Polit = X_polit)
#' C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' out <- search_model(A = A, C = C, c1 =rep(1, 3), scheme = "factorial",
#'                scale = FALSE, verbose = FALSE,
#'                ncomp = rep(1, length(A)),
#'                bias = TRUE)
#' head(out)
#' columns <- grep("var", colnames(out))
#' model <- symm(C, out[which.max(out$AVE_inner), columns])
#' out <- iterate_model(A = A, C = model, c1 =rep(1, 3), scheme = "factorial",
#'                scale = FALSE, verbose = FALSE,
#'                ncomp = rep(1, length(A)),
#'                bias = TRUE)
#' @rdname model
#' @export
#' @import utils
iterate_model <- function(...) {
  l <- list(...)
  A <- lapply(l$A, scale2, bias = l$bias)
  diff0 <- which(lower.tri(l$C) & l$C != 0)
  designs <- weight_design(weights = 11, size = length(A), diff0 = diff0)
  l <- l[!names(l)  %in% c("A", "C", "scale")]
  iterate(designs, l, A)
}

#' @export
#' @describeIn model Search for the right model for the blocks provided.
search_model <- function(..., nWeights = 3) {
  l <- list(...)
  A <- lapply(l$A, scale2, bias = l$bias)
  designs <- weight_design(weights = nWeights, size = length(l$A))
  k <- vapply(designs, correct, logical(1L))
  designs <- designs[k]
  l <- l[!names(l)  %in% c("A", "C", "scale")]
  iterate(designs, l, A)
}


iterate <- function(designs, l, A) {
  out <- sapply(designs, function(x){
    l$scale <- FALSE
    do.call(testing, c(list(A = A, x = x), l))}, USE.NAMES = FALSE)
  if (is.list(out)) {
    warning("Some errors happened on the iteration.")
    out <- simplify2array(out[lengths(out) != 1])
  }
  as.data.frame(t(out))
}
