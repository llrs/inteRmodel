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
#' @param BPPARAM Set up parallel backend (see BiocParallel documentation).
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
#'                bias = TRUE, BPPARAM = BiocParallel::SerialParam())
#' head(out)
#' # From all the models, we select that with the higher inner AVE:
#' columns <- grep("var", colnames(out))
#' model <- symm(C, out[which.max(out$AVE_inner), columns])
#' # We then look for a variation of the weights of this model
#' out <- iterate_model(A = A, C = model, c1 =rep(1, 3), scheme = "factorial",
#'                scale = FALSE, verbose = FALSE,
#'                ncomp = rep(1, length(A)),
#'                bias = TRUE)
#' @rdname model
#' @export
#' @import utils
iterate_model <- function(..., BPPARAM = BiocParallel::SerialParam()) {
  l <- list(...)
  A <- lapply(l$A, scale2, bias = l$bias)
  diff0 <- which(lower.tri(l$C) & l$C != 0)
  designs <- weight_design(weights = 11, size = length(A), diff0 = diff0)
  l <- l[!names(l)  %in% c("A", "C", "scale")]
  iterate(designs, l, A)
}

#' @export
#' @describeIn model Search for the right model for the blocks provided.
search_model <- function(..., nWeights = 3, BPPARAM = BiocParallel::SerialParam()) {
  l <- list(...)
  A <- lapply(l$A, scale2, bias = l$bias)
  designs <- weight_design(weights = nWeights, size = length(l$A))
  k <- BiocParallel::bplapply(designs, correct, BPPARAM = BPPARAM)
  designs <- designs[unlist(k)]
  l <- l[!names(l)  %in% c("A", "C", "scale")]
  iterate(designs, l, A, BPPARAM)
}


iterate <- function(designs, l, A, BPPARAM = BiocParallel::SerialParam()) {
  out <- BiocParallel::bplapply(designs, function(x){
    l$scale <- FALSE
    do.call(testing, c(list(A = A, x = x), l))}, BPPARAM = BPPARAM)
  out <- simplify2array(out[lengths(out) != 1])
  as.data.frame(t(out))
}
