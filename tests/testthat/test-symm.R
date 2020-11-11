context("test-symm")

test_that("symm works", {
  row <- structure(list(var12 = 0, var13 = 0, var23 = 1, var14 = 1, var24 = 0.5,
                        var34 = 0, var15 = 0, var25 = 0, var35 = 1, var45 = 0),
                   row.names = 1L, class = "data.frame")
  C <- matrix(0, ncol = 5, nrow = 5)
  out <- symm(C, row)
  expect_true(isSymmetric(out))

  row$var11 <- 1
  out2 <- symm(C, row)
  expect_equal(out2[1, 1], 1L)

  row$var22 <- 1
  row$var33 <- 1
  row$var44 <- 1
  row$var55 <- 1
  out3 <- symm(C, row)
  expect_equal(diag(out3), rep(1, 5))

  row$var66 <- 1
  expect_error(symm(C, row), "diagonal has wrong length")

  m <- matrix(0, ncol = 3, nrow = 3)
  colnames(m) <- letters[1:3]
  rownames(m) <- LETTERS[1:3]
  expect_error(subSymm(m, 1, 2, 1), "symmetric matrix.")

  C_out <- subSymm(C, 2, 3, 1)
  C_out <- subSymm(C_out, 1, 4, 1)
  C_out <- subSymm(C_out, 2, 4, 0.5)
  C_out <- subSymm(C_out, 3, 5, 1)
  expect_equivalent(out, C_out)
})
