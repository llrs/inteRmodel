data(ge_cgh_locIGR)

A <- ge_cgh_locIGR$multiblocks
Loc <- factor(ge_cgh_locIGR$ylabel)
levels(Loc) <- colnames(ge_cgh_locIGR$multiblocks$y)
C <-  matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
tau = c(1, 1, 0)

# rgcca algorithm using the dual formulation for X1 and X2
# and the dual formulation for X3
A[[3]] = A[[3]][, -3]
# sgcca algorithm
result.sgcca <- sgcca(A, C, c1 = c(.071,.2, 1), ncomp = c(1, 1, 1),
                     scheme = "centroid", verbose = FALSE)

test_that("variables_contribution works", {
  w <- variables_contribution(A, result.sgcca)
  expect_is(w, "list")
  expect_length(w, 3L)
})
