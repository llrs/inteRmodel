test_that("model_RGCCA works", {
  out <- model_RGCCA(iris, "Species")
  expect_equal(nrow(out), nrow(iris))
  expect_equal(ncol(out), length(levels(iris$Species)) - 1)
  expect_length(colnames(out), 2L) # Test names and fixed amount of columns
})
