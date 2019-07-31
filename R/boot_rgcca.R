#' Bootstrap sgcca
#'
#' Function to perform bootstrap on the code
#' @param ... Arguments passed to sgcca
#' @param nb_boot Number of bootstraps to perform
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
#' out <- boot_sgcca(A = A, C = C, c1 = rep(1, 3),  nb_boot = 10)
#' head(out$AVE)
#' @name boot
boot_sgcca <- function(..., nb_boot = 1000) {
  l <- list(...)
  shrinkage <- l$c1
  A <- l$A
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
