test_that("cca works", {
  data("Russett", package = "RGCCA")
  X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
  X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
  X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
                                    "dictator")])
  A <- list(X_agric, X_ind, X_polit)
  A <- lapply(A, function(x) scale2(x, bias = TRUE))
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  if (new_rgcca_version()) {
    out <- RGCCA::rgcca(A, C, tau = rep(0, 3), scheme = "factorial", method = "rgcca",
                        scale = FALSE, verbose = FALSE, ncomp = rep(2, length(A)), scale_block = TRUE)
  } else {
    out <- RGCCA::rgcca(A, C, tau = rep(0, 3), scheme = "factorial",
                        scale = FALSE, verbose = FALSE, ncomp = rep(2, length(A)))
  }

  out <- improve(out, c("Agric", "Ind", "Polit"))
  out <- cca_rgcca(out)
  expect_equal(nrow(out), 36L)
  expect_equal(ncol(out), 6L)
  expect_equal(out[1, 5], 1L)
})
