#' Canonical correlations and covariates
#'
#' Calculates the canonical correlation and canonical covariates.
#' @param rgcca The output of SGCCA or RGCCA
#' @return A list of matrix with the correlation and covariation between CCA
#' dimensions
#' @export
#' @importFrom stats cor
#' @importFrom stats cov
cca_rgcca <- function(rgcca) {
  l <- list()
  for (i in seq_along(rgcca$Y)) {
    l[[names(rgcca$Y)[i]]] <- list()
    for (j in seq_along(rgcca$Y)) {
      l[[names(rgcca$Y)[i]]][[names(rgcca$Y)[j]]] <- list(
        "cor" = cor(rgcca$Y[[i]], rgcca$Y[[j]]),
        "cov" = cov(rgcca$Y[[i]], rgcca$Y[[j]])
      )
    }
  }
  l
}
