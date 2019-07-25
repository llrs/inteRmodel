#' Bootstrap sgcca
#'
#' Performs the centroid bootstrap
#'
#' @param A The list with the original data
#' @param C The symmetric matrix with the relationships between datsets.
#' @param shrinkage Shrinkage estimated (use the estimated for the original datastet)
#' @param nb_boot Number of bootstraps to perform
#' @return A list with two elements: the coefficient of each variable of the
#' input blocks; and the AVE values, both inner, and outer
#' @export
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom RGCCA sgcca
boot_sgcca <- function(A, C, shrinkage, nb_boot = 1000) {
  STAB <- vector("list", length = length(A))
  AVE <- matrix(NA, ncol = 2, nrow = nb_boot)
  colnames(AVE) <- c("inner", "outer")

  for (j in seq_along(A)) {
    STAB[[j]] <- matrix(NA, nb_boot, ncol(A[[j]]))
    colnames(STAB[[j]]) <- colnames(A[[j]])
  }
  names(STAB) <- names(A)
  pb <-  txtProgressBar(min = 0, max = nb_boot, initial = 0, style = 3)
  # Bootstrap the data
  for (i in seq_len(nb_boot)) {
    setTxtProgressBar(pb, i)
    ind <- sample(nrow(A[[1]]), replace = TRUE)

    Bscr <- subsetData(A, ind)
    min_shrinkage <- vapply(A, function(x) {
      1 / sqrt(ncol(x))
    }, numeric(1L))
    # Recalculate just in case
    shrinkage2 <- ifelse(shrinkage < min_shrinkage, min_shrinkage, shrinkage)
    try( # Prevents the error from LAPACK subroutine
      {
        res <- sgcca(
          Bscr, C,
          c1 = shrinkage2,
          ncomp = c(rep(1, length(A))),
          scheme = "centroid",
          scale = TRUE
        )

        AVE[i, "inner"] <- res$AVE$AVE_inner
        AVE[i, "outer"] <- res$AVE$AVE_outer

        for (j in seq_along(A)) {
          STAB[[j]][i, rownames(res$a[[j]])] <- res$a[[j]]
        }
      },
      silent = FALSE
    )
  }
  return(list("STAB" = STAB, "AVE" = AVE))
}
