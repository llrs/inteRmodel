test_that("index works", {
  out <- boot_index(10, 43)
  expect_length(out, 43)
  expect_true(all(lengths(out) == 10))
})

