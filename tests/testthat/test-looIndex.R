test_that("looIndex works", {
  out <- looIndex(15)
  expect_length(out, 15L)
  expect_true(all(lengths(out) == 14L))
})
