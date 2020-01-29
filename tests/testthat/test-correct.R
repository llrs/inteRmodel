context("test-correct")

test_that("correct works", {
  C <- matrix(0, ncol = 5, nrow = 5)
  Ci <- C
  Ci[1, 3] <- 1
  expect_false(correct(Ci))
  model0 <- subSymm(C, 1, 3, 1)
  expect_false(correct(model0))
  model1 <- subSymm(C, 1, 2, 1)
  model1 <- subSymm(model1, 2, 3, 1)
  model1 <- subSymm(model1, 3, 4, 1)
  model1 <- subSymm(model1, 4, 5, 1)
  expect_true(correct(model1))
  model2 <- subSymm(C, 1, 4, 1)
  model2 <- subSymm(model2, 2, 3, 1)
  model2 <- subSymm(model2, 2, 5, 1)
  model2 <- subSymm(model2, 3, 5, 1)
  expect_false(correct(model2))
})

test_that("disconnected networks", {
  m <- structure(c(0, 0, 0, 0.333333333333333, 0, 0, 0.666666666666667,
              0, 0, 0.666666666666667, 0, 0, 0.333333333333333, 0, 0, 0),
              .Dim = c(4L, 4L))
  expect_false(correct(m))
})
