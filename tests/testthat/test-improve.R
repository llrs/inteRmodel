test_that("improve works", {
  data("Russett", package = "RGCCA")
  X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
  X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
  X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
                                    "dictator")])
  A <- list(X_agric, X_ind, X_polit)
  A <- lapply(A, function(x) RGCCA::scale2(x, bias = TRUE))
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
  out <- RGCCA::rgcca(A, C, tau =rep(0, 3), scheme = "factorial",
                      scale = FALSE, verbose = FALSE, ncomp = rep(2, length(A)))

  out <- improve(out, c("Agric", "Ind", "Polit"))
  expect_equal(dim(out$AVE$AVE_X), c(2, 3))
  expect_equal(colnames(out$AVE$AVE_X), c("Agric", "Ind", "Polit"))
  expect_named(out$Y, c("Agric", "Ind", "Polit"))
  expect_named(out$a, c("Agric", "Ind", "Polit"))
  expect_named(out$astar, c("Agric", "Ind", "Polit"))
  expect_equal(colnames(out$C), c("Agric", "Ind", "Polit"))
  expect_equal(rownames(out$C), c("Agric", "Ind", "Polit"))

  expect_error(improve(out, NULL), "shouldn't be NULL")
})
