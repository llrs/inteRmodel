testing <- function(x, ...) {
  try({
    result.sgcca <- RGCCA::sgcca(C = x,
                                 verbose = FALSE,
                                 scale = FALSE,
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
iterate <- function(...) {
  l <- list(...)
  A <- lapply(l$A, scale2, bias = l$bias)
  diff0 <- which(lower.tri(l$C) & l$C != 0)
  out <- sapply(designs1.2, testing, A = l$A, USE.NAMES = FALSE)
  as.data.frame(t(out))
}
