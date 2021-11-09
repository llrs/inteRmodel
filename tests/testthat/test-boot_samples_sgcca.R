test_that("boot_samples_sgcca works", {
  data("Russett", package = "RGCCA")
  X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
  X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
  X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
                                    "dictator")])
  A <- list(X_agric, X_ind, X_polit)
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)

  out <- boot_samples_sgcca(A = A, C = C, c1 = rep(1, 3),  nb_boot = 10,
                            verbose = FALSE)
  expect_equal(dim(out$AVE), c(10L, 2L))
  expect_length(out$STAB, 3L)
  expect_equal(ncol(out$STAB[[1]]), 3L)
  expect_equal(ncol(out$STAB[[2]]), 2L)
  expect_equal(ncol(out$STAB[[3]]), 5L)
})


test_that("boot_index_sgcca works", {
  data("Russett", package = "RGCCA")
  X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
  X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
  X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
                                    "dictator")])
  A <- list(X_agric, X_ind, X_polit)
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)

  index <- boot_index(nrow(A[[1]]), 10)
  if (new_rgcca_version()) {
    boot_i <- boot_index_sgcca(index, blocks = A, connection = C)
  } else {
    boot_i <- boot_index_sgcca(index, A = A, C = C)
  }
  expect_true(any(boot_i$AVE[, 1] != 0))
  expect_true(any(boot_i$AVE[, 2] != 0))
  expect_equal(dim(boot_i$AVE), c(10L, 2L))
  expect_length(boot_i$STAB, 3L)
  expect_true(!is.null(boot_i$STAB[[1]]))
  expect_equal(ncol(boot_i$STAB[[1]]), 3L)
  expect_equal(ncol(boot_i$STAB[[2]]), 2L)
  expect_equal(ncol(boot_i$STAB[[3]]), 5L)

})
