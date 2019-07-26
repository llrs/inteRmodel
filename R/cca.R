#' Canonical correlations and covariates
#'
#' Calculates the canonical correlation and canonical covariates.
#' @param rgcca The output of SGCCA or RGCCA
#' @return A list of matrix with the correlation and covariation between CCA
#' dimensions
#' @export
#' @importFrom stats cor
#' @importFrom stats cov
#' @examples
#' data("Russett", package = "RGCCA")
#' X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
#' X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
#' X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
#'                                   "dictator")])
#' A <- list(X_agric, X_ind, X_polit)
#' A <- lapply(A, function(x) RGCCA::scale2(x, bias = TRUE))
#' C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' out <- RGCCA::rgcca(A, C, tau =rep(0, 3), scheme = "factorial",
#'                     scale = FALSE, verbose = FALSE, ncomp = rep(2, length(A)))
#' out <- improve(out, c("Agric", "Ind", "Polit"))
#' cca_rgcca(out)
cca_rgcca <- function(rgcca) {

  l <- vector("list", length(rgcca$Y))
  named <- !is.null(names(rgcca$Y))
  if (named) {
    names(l) <- names(rgcca$Y)
  }
  for (i in seq_along(rgcca$Y)) {
    l[[i]] <- vector("list", length(rgcca$Y))
    if (named) {
      names(l[[i]]) <- names(rgcca$Y)
    }
    for (j in seq_along(rgcca$Y)) {
      l[[i]][[j]] <- list(
        "cor" = cor(rgcca$Y[[i]], rgcca$Y[[j]]),
        "cov" = cov(rgcca$Y[[i]], rgcca$Y[[j]])
      )
    }
  }
  l
}
