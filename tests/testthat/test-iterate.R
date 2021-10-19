test_that("search_model works", {

  data("Russett", package = "RGCCA")
  X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
  X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
  X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
                                    "dictator")])
  A <- list(Agric = X_agric, Ind = X_ind, Polit = X_polit)
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)

  if (new_rgcca_version()) {
    out <- search_model(A = A, C = C, tau = rep(1, 3), scheme = "factorial", method = "rgcca", scale_block = FALSE,
                        scale = FALSE, verbose = FALSE,
                        ncomp = rep(1, length(A)),
                        bias = TRUE)
  } else {
    out <- search_model(A = A, C = C, c1 = rep(1, 3), scheme = "factorial",
                        scale = FALSE, verbose = FALSE,
                        ncomp = rep(1, length(A)),
                        bias = TRUE)
  }
  expect_s3_class(out, "data.frame")
  expect_equal(dim(out), c(20L, 10L))
})

test_that("iterate_model works", {

  data("Russett", package = "RGCCA")
  X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
  X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
  X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
                                    "dictator")])
  A <- list(Agric = X_agric, Ind = X_ind, Polit = X_polit)
  C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)

  if (new_rgcca_version()) {
    out <- search_model(A = A, C = C, tau =rep(1, 3), scheme = "factorial", method = "rgcca", scale_block = FALSE,
                        scale = FALSE, verbose = FALSE,
                        ncomp = rep(1, length(A)),
                        bias = TRUE)
  } else {
    out <- search_model(A = A, C = C, c1 = rep(1, 3), scheme = "factorial",
                        scale = FALSE, verbose = FALSE,
                        ncomp = rep(1, length(A)),
                        bias = TRUE)
  }
  columns <- grep("var", colnames(out))
  model <- symm(C, out[which.max(out$AVE_inner), columns])
  # We then look for a variation of the weights of this model
  if (new_rgcca_version()) {
    out <- iterate_model(A = A, C = model, tau =rep(1, 3), scheme = "factorial", method = "rgcca", scale_block = FALSE,
                        scale = FALSE, verbose = FALSE,
                        ncomp = rep(1, length(A)),
                        bias = TRUE)
  } else {
    out <- iterate_model(A = A, C = model, c1 = rep(1, 3), scheme = "factorial",
                        scale = FALSE, verbose = FALSE,
                        ncomp = rep(1, length(A)),
                        bias = TRUE)
  }
  expect_s3_class(out, "data.frame")
  expect_true(all(out$weights == 2L))
  expect_equal(dim(out), c(100L, 10L))
})
