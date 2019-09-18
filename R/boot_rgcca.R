#' Bootstrap sgcca
#'
#' Function to perform bootstrap on the samples. \code{boot_samples_sgcca}
#' bootstrap given original data, while \code{boot_index_sgcca} given some
#' index of samples it iterates over it.
#'
#' \code{boot_index_sgcca} Iterate over the index,
#' which is a list of vectors with the position of samples to use.
#' \code{boot_samples_sgcca} Iterate over random samples.
#' @param ... Named arguments passed to sgcca.
#' @param nb_boot Number of bootstraps to perform.
#' @return A list with two elements: the coefficient of each variable of the
#' input blocks; and the AVE values, both inner, and outer
#' @export
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom RGCCA sgcca
#' @examples
#' data("Russett", package = "RGCCA")
#' X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
#' X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
#' X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
#'                                   "dictator")])
#' A <- list(X_agric, X_ind, X_polit)
#' C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
#' out <- boot_samples_sgcca(A = A, C = C, c1 = rep(1, 3),  nb_boot = 10)
#' head(out$AVE)
#' @rdname boot
boot_samples_sgcca <- function(..., nb_boot = 1000) {

  l <- list(...)
  A <- l$A
  shrinkage <- l$c1
  if (is.null(shrinkage)) {
    shrinkage <- rep(1, length(A))
  }
  STAB <- vector("list", length = length(l$A))
  AVE <- matrix(NA, ncol = 2, nrow = nb_boot)
  colnames(AVE) <- c("inner", "outer")

  for (j in seq_along(l$A)) {
    STAB[[j]] <- matrix(NA, nb_boot, ncol(l$A[[j]]))
    colnames(STAB[[j]]) <- colnames(l$A[[j]])
  }
  names(STAB) <- names(l$A)
  pb <-  txtProgressBar(min = 0, max = nb_boot, initial = 0, style = 3)
  # Bootstrap the data
  for (i in seq_len(nb_boot)) {
    setTxtProgressBar(pb, i)
    ind <- sample(nrow(A[[1]]), replace = TRUE)

    Bscr <- subsetData(A, ind)
    min_shrinkage <- vapply(Bscr, function(x) {
      1 / sqrt(ncol(x))
    }, numeric(1L))
    # Recalculate just in case
    shrinkage2 <- ifelse(shrinkage < min_shrinkage, min_shrinkage, shrinkage)
    l$scale <- TRUE
    l$c1 <- shrinkage2
    l$A <- Bscr

    try( # Prevents the error from LAPACK subroutine
      {
        res <- do.call(sgcca, l)

        AVE[i, "inner"] <- res$AVE$AVE_inner
        AVE[i, "outer"] <- res$AVE$AVE_outer

        for (j in seq_along(l$A)) {
          STAB[[j]][i, rownames(res$a[[j]])] <- res$a[[j]]
        }
      },
      silent = FALSE
    )
  }
  return(list("STAB" = STAB, "AVE" = AVE))
}

#' @rdname boot
#' @param index A list of numeric values for selecting values
#' @export
#' @examples
#' boots <- 10
#' index <- vector("list", length = boots)
#' for (i in seq_len(boots)) {
#'   index[[i]] <- sample(nrow(A[[1]]), replace = TRUE)
#' }
#' boot_i <- boot_index_sgcca(index, A = A, C = C)
boot_index_sgcca <- function(index, ...) {
  l <- lapply(index, base_boot, ... = ...)
  AVE <- sapply(l, function(x){x$AVE})
  STAB <- sapply(seq_along(list(...)$A), function(y) {
    do.call(rbind, lapply(l, function(x){x$STAB[[y]]}))
  })
  list(AVE = t(AVE), STAB = STAB)
}


base_boot <- function(index, ...) {
  l <- list(...)
  STAB <- vector("list", length = length(l$A))
  AVE <- vector("numeric", length = 2)
  names(AVE) <- c("inner", "outer")
  names(STAB) <- names(l$A)

  l$A <- subsetData(l$A, index)
  l$scale <- TRUE # force to scale

  try({
    res <- do.call(sgcca, l)
    AVE["inner"] <- res$AVE$AVE_inner
    AVE["outer"] <- res$AVE$AVE_outer
    for (j in seq_along(l$A)) {
      STAB[[j]] <- res$a[[j]][, 1]
    }

  }, silent = TRUE)
  list(AVE = AVE, STAB = STAB)
}