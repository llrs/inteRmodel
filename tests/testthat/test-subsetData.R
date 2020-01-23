test_that("subsetData works", {
  data(ge_cgh_locIGR)
  A <- subsetData(ge_cgh_locIGR$multiblocks, sample(53, replace = TRUE))
  expect_equal(nrow(A[[1]]), 53)
  expect_equal(names(A), names(ge_cgh_locIGR$multiblocks))

  A[[2]] <- matrix(c(rep(0, 53), runif(53)), ncol = 2, nrow = 53)
  B <- subsetData(A, sample(53, replace = TRUE))
  expect_equal(ncol(B[[2]]), 1L)

})
