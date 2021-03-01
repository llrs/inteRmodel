#' Find variables contributions
#'
#' Return the specific contribution of each variable for each sample for the
#' first dimension of each block.
#' @param A The original data.
#' @param res The results of [sgcca()] or [rgcca()].
#' @param scale Logical value whether to scale or not the data.
#' @param bias Logical value whether to use bias or not when scaling the data.
#' @return A list of matrices with the variables of each block and the value
#' for each sample that helps place the sample on the reduced-dimensional space.
#' @export
#' @examples
#' \dontrun{
#' # Download the dataset's package at http://biodev.cea.fr/sgcca/.
#' # --> gliomaData_0.4.tar.gz
#'
#' require(gliomaData)
#' data(ge_cgh_locIGR)
#'
#' A <- ge_cgh_locIGR$multiblocks
#' Loc <- factor(ge_cgh_locIGR$ylabel)
#' levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
#' C <-  matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' tau = c(1, 1, 0)
#'
#' # rgcca algorithm using the dual formulation for X1 and X2
#' # and the dual formulation for X3
#' A[[3]] = A[[3]][, -3]
#' # sgcca algorithm
#' result.sgcca = sgcca(A, C, c1 = c(.071,.2, 1), ncomp = c(1, 1, 1),
#'                      scheme = "centroid", verbose = FALSE)
#' weights <- variables_contribution(A, result.sgcca)
#' }
variables_contribution <- function(A, res, scale = TRUE, bias = TRUE){
  A2 <- vector("list", length(A))
  names(A2) <- names(A)
  for (i in seq_along(A)) {
    if (scale) {
      y <- scale2(A[[i]], bias = bias) / sqrt(NCOL(A[[i]]))
    } else {
      y <- A[[i]]
    }
    A2[[i]] <- apply(y, 1, function(x){x * res$astar[[i]]}[, 1])
    rownames(A2[[i]]) <- colnames(A[[i]])
  }
  A2
}


#' Find the variables that are related
#'
#' Performs the correlation between the variables of the block.
#' @param rel Relationship model
#' @param comp A numeric vector with the blocks you want to compare.
#' @export
#' @examples
#' \dontrun{
#' # Download the dataset's package at http://biodev.cea.fr/sgcca/.
#' # --> gliomaData_0.4.tar.gz
#'
#' require(gliomaData)
#' data(ge_cgh_locIGR)
#'
#' A <- ge_cgh_locIGR$multiblocks
#' Loc <- factor(ge_cgh_locIGR$ylabel)
#' levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
#' C <-  matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' tau = c(1, 1, 0)
#'
#' # rgcca algorithm using the dual formulation for X1 and X2
#' # and the dual formulation for X3
#' A[[3]] = A[[3]][, -3]
#' # sgcca algorithm
#' result.sgcca = sgcca(A, C, c1 = c(.071,.2, 1), ncomp = c(1, 1, 1),
#'                      scheme = "centroid", verbose = FALSE)
#' weights <- variables_contribution(A, result.sgcca)
#' rel <- variables_relations(weights)
#' rel[1:15, 1:5]
#' }
variables_relations <- function(rel, comp = c(1, 2)) {
  stopifnot(length(comp) == 2, is.numeric(comp), all(comp <= length(rel)))
  x <- t(rel[[comp[1]]])
  y <- t(rel[[comp[2]]])

  cor(x[, apply(x, 2, function(x){any(x != 0)}), drop = FALSE],
      y[, apply(y, 2, function(x){any(x != 0)}), drop = FALSE])
}
