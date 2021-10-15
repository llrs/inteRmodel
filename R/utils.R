new_rgcca_version <- function() {
  # Assumes the user will have the CRAN branch!
  packageVersion("RGCCA") > "2.1.2"
}


repl_vec <- function(x, old, new){
  x[[new]] <- x[[old]]
  x[[old]] <- NULL
  x
}

#' @importFrom RGCCA rgcca
#' @rawNamespace if (!packageVersion("RGCCA") >= "3") {
#'   importFrom(RGCCA, scale2)
#' }
if (!new_rgcca_version()) {
  scale2 <- RGCCA::scale2
} else {
  sgcca <- function(...) {
    l <- as.list(...)
    l$method <- "sgcca"

    l <- repl_vec(l, "c1", "sparsity")
    l <- repl_vec(l, "A", "blocks")
    l <- repl_vec(l, "C", "connection")
    do.call(rgcca, l)
  }

  cov2 <- function(x, y = NULL, bias = TRUE) {

    if (is.null(y)) {
      x <- as.matrix(x)
      y <- x
    }

    suppressWarnings({C <- cov(x, y, use = "pairwise.complete.obs")})
    if (bias) {
      n <- NROW(x)
      C <- ((n - 1) / n) * C
    }
    return(C)
  }


  scale2 <- function(A, center = TRUE, scale = TRUE, bias = TRUE) {
    if (center == TRUE & scale == TRUE) {
      A <- scale(A, center = TRUE, scale = FALSE)
      std <- sqrt(apply(A, 2, cov2, bias = bias))
      if (any(std == 0)) {
        sprintf("there were %d constant variables", sum(std == 0))
        std[std == 0] <- 1
      }
      A <- A / matrix(rep(std, NROW(A)), NROW(A), NCOL(A), byrow = TRUE)
      attr(A, "scaled:scale") <- std
      return(A)
    }
    if (center == TRUE & scale == FALSE) {
      A <- scale(A, center = TRUE, scale = FALSE)
      return(A)
    }
    if (center == FALSE & scale == TRUE) {
      std <- apply(A, 2, cov2, bias = bias)
      A <- A / matrix(rep(std, NROW(A)), NROW(A), NCOL(A), byrow = TRUE)
      attr(A, "scaled:scale") <- std
      return(A)
    }
  }
}
