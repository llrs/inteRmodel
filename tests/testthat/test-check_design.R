context("test-check_design")

test_that("weight_design works", {
  designs <- weight_design(4, 4)
  keep <- sapply(designs, correct)
  expect_false(keep[145])
  expect_false(keep[140])
})

test_that("weight_design works with diff0", {
  d <- weight_design(4, 4)[[60]]
  pos <- which(lower.tri(d) & d != 0)
  w <- weight_design(4, 4, pos)
  keep <- sapply(w, function(x){
    all(x[1, 2:4] != 0)
  })
  expect_true(all(keep))
})
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
