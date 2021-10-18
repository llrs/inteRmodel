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
#' } else {
#'   importFrom(RGCCA, sgcca)
#' }
NULL

# For reverse dependency
#' Scale2
#'
#' If possible reexport scale2 of RGCCA, if not make it available to users.
#' @param ... Named arguments for scale2, A (Matrix), center, scale, bias (logical).
#' @export
scale2 <- function(...) {
  if (!new_rgcca_version()) {
    scale2 <- RGCCA::scale2
  } else {
    # code copied from RGCCA package
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

    # code copied from RGCCA package
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
  scale2(...)
}

# Adapt sgcca to the new version
sgcca <- function(...) {
  if (new_rgcca_version()) {
    l2 <- list(...)
    l2$method <- "sgcca"

    l2 <- repl_vec(l2, "c1", "sparsity")
    l2 <- repl_vec(l2, "A", "blocks")
    l2 <- repl_vec(l2, "C", "connection")
    do.call(RGCCA::rgcca, l2)
  } else {
    RGCCA::sgcca(...)
  }
}

# Find the connections used
connections <- function(x) {
  if (new_rgcca_version()) {
    con <- x$call$connection
  } else {
    con <- x$C
  }
  con
}

# Find the scheme used
scheme <- function(x) {
  if (new_rgcca_version()) {
    x$call$scheme
  } else {
    x$scheme
  }
}
