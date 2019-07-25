context("test-symm")

test_that("symm works", {
  row <- structure(list(var12 = 0, var13 = 0, var23 = 1, var14 = 1, var24 = 0.5,
                        var34 = 0, var15 = 0, var25 = 0, var35 = 1, var45 = 0),
                   row.names = 1L, class = "data.frame")
  C <- matrix(0, ncol = 5, nrow = 5)
  out <- symm(C, row)
  C_out <- subSymm(C, 2, 3, 1)
  C_out <- subSymm(C_out, 1, 4, 1)
  C_out <- subSymm(C_out, 2, 4, 0.5)
  C_out <- subSymm(C_out, 3, 5, 1)
  expect_equivalent(out, C_out)
})
