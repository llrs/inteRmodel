#' Analyze a sgcca object
#'
#' Performs calculations to the output of sgcca to make it easier to retrieve
#' the information about the result.
#'
#' Calculates the correlations between the canonical dimensions,
#' calculates the canonical correlations,
#' returns also the weight of each link of the model used,
#' all of this in a tidy way.
#' @param sgcca SGCCA object from the RGCCA package
#' @return A vector with the correlation between components, AVE (both inner
#' and outer), the canonical correlation, the weight in the design matrix, and
#' the number of interactions that exists.
#' @export
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
#'                     scale = FALSE, verbose = TRUE)
#' analyze(out)
analyze <- function(sgcca) {

  C <- connections(sgcca)
  ind <- apply(which(upper.tri(C), arr.ind = TRUE), 1,
        paste0, collapse = "")

  cY <- dimensions_correlation(sgcca)
  cc <- helper_cc(sgcca, cY)

  # Values of the correlation between the dimensions
  var <- cY[upper.tri(cY)]
  names(var) <- paste0("vs", ind)

  # Values of the design matrix
  vars <- C[upper.tri(C)]
  names(vars) <- paste0("var", ind)

  # weights used
  weight <- sum(vars != 0)
  names(weight) <- "weights"

  # Output
  c(var, unlist(sgcca$AVE[c("AVE_inner", "AVE_outer")]), cc1 = cc,
    vars, weight)
}

dimensions_correlation <- function(sgcca) {
  # Correlation between Y
  Y <- simplify2array(sgcca$Y, higher = FALSE)
  cor(Y)
}

helper_cc <- function(sgcca, cY) {
  d <- cY * connections(sgcca)
  switch(scheme(sgcca),
         centroid = sum(abs(d[upper.tri(d)])),
         horst = sum(d[upper.tri(d)]),
         factorial = sum(d[upper.tri(d)]^2))
}

#' Method to simplify AVE
#'
#' This simplifies the AVE_X to make it easier to understand.
#'
#' It converts to a matrix where each row is a dimension and each column is a block.
#' @param x rgcca or sgcca object
#' @return The same object with AVE_X simplified
#' @export
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
#' out$AVE
#' out <- aves(out)
#' out$AVE
aves <- function(x){
  x$AVE$AVE_X <- simplify2array(x$AVE$AVE_X)
  x
}


#' Improve the information on sgcca classes
#'
#' Add names to data returned by `rgcca` or `sgcca`, simplify AVE output
#' @param sgcca An object of class `sgcca` or `rgcca`.
#' @param namesA The names of the original data
#' @return An object of class `sgcca` or `rgcca`
#' @export
#' @examples
#' data("Russett", package = "RGCCA")
#' X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
#' X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
#' X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
#'                                   "dictator")])
#' A <- list(X_agric, X_ind, X_polit)
#' A <- lapply(A, function(x) scale2(x, bias = TRUE))
#' C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' out <- RGCCA::rgcca(A, C, tau = rep(0, 3), scheme = "factorial",
#'                     scale = FALSE, verbose = FALSE, ncomp = rep(2, length(A)))
#' out$AVE
#' out <- improve(out, c("Agric", "Ind", "Polit"))
#' out$AVE
improve <- function(sgcca, namesA) {
  if (is.null(namesA)) {
    stop("namesA shouldn't be NULL\n",
         "Consider adding names to A.")
  }

  names(sgcca$Y) <- namesA
  names(sgcca$a) <- namesA
  names(sgcca$astar) <- namesA
  names(sgcca$AVE$AVE_X) <- namesA
  if (!new_rgcca_version()) {
    colnames(sgcca$C) <- namesA
    rownames(sgcca$C) <- namesA
  }
  aves(sgcca)
}
