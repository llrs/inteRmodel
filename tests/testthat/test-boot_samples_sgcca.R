data("Russett", package = "RGCCA")
X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
                                  "dictator")])
A <- list(X_agric, X_ind, X_polit)
C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)

test_that("boot_samples_sgcca works", {
  out <- boot_samples_sgcca(A = A, C = C, c1 = rep(1, 3),  nb_boot = 10,
                            verbose = FALSE)
  expect_equal(dim(out$AVE), c(10L, 2L))
  expect_length(out$STAB, 3L)
  expect_equal(ncol(out$STAB[[1]]), 3L)
  expect_equal(ncol(out$STAB[[2]]), 2L)
  expect_equal(ncol(out$STAB[[3]]), 5L)
})


test_that("boot_index_sgcca works", {
  boots <- 10
  index <- vector("list", length = boots)
  for (i in seq_len(boots)) {
    index[[i]] <- sample(nrow(A[[1]]), replace = TRUE)
  }
  boot_i <- boot_index_sgcca(index, A = A, C = C)

  expect_equal(dim(boot_i$AVE), c(10L, 2L))
  expect_length(boot_i$STAB, 3L)
  expect_equal(ncol(boot_i$STAB[[1]]), 3L)
  expect_equal(ncol(boot_i$STAB[[2]]), 2L)
  expect_equal(ncol(boot_i$STAB[[3]]), 5L)

})
