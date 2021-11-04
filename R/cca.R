#' Canonical correlations and covariates
#'
#' Calculates the canonical correlation and canonical covariates.
#' @param rgcca The output of SGCCA or RGCCA
#' @return A data.frame with the correlation and covariation between CCA
#' components
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
#' A <- lapply(A, function(x) scale2(x, bias = TRUE))
#' C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' out <- RGCCA::rgcca(A, C, tau =rep(0, 3), scheme = "factorial",
#'                     scale = FALSE, verbose = FALSE, ncomp = rep(2, length(A)))
#' out <- improve(out, c("Agric", "Ind", "Polit"))
#' ccas <- cca_rgcca(out)
cca_rgcca <- function(rgcca) {
  vars <- names(rgcca$Y)
  if (new_rgcca_version()) {
    comp <- seq_along(rgcca$AVE$AVE_inner)
  } else {
    comp <- seq_along(rgcca$AVE$AVE_inner_model)
  }
  df <- expand.grid(Var1 = vars, Var2 = vars,
                    Comp1 = comp, Comp2 = comp,
                    stringsAsFactors = FALSE)
  df <- cbind(df, cor = NA, cov = NA)
  for (i in seq_len(nrow(df))) {
    df[i, "cor"] <- cor(rgcca$Y[[df[i, 1]]][, df[i, 3]],
                        rgcca$Y[[df[i, 2]]][, df[i, 4]])
    df[i, "cov"] <- cov(rgcca$Y[[df[i, 1]]][, df[i, 3]],
                        rgcca$Y[[df[i, 2]]][, df[i, 4]])
  }
  df
}

