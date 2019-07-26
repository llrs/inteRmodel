

# Check that each data block is to all
# See this answer:
# https://math.stackexchange.com/a/551947
# There should work
#' Check that the design is fully connected
#'
#' Given the design matrix, checks that all the blocks are connected between them.
#' Without leaving any block not connected or some blocks isolated from the others.
#' @param x Design matrix, a symmetric matrix with
#' @return A logical value if it is fully connected or not.
#' @references \url{https://math.stackexchange.com/a/551947}
#' @export
#' @examples
#' C <- matrix(0, nrow = 4, ncol = 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
#' (m1 <- subSymm(C, 1, 2, 1))
#' correct(m1) # Several blocks wouldn't be connected
#' (m2 <- subSymm(m1, 3, 4, 1))
#' correct(m2) # All blocks are connected, but there are two networks.
#' (m3 <- subSymm(m2, 1, 4, 1))
#' correct(m3) # All blocks are connected and there is a single network
correct <- function(x) {
  if (!isSymmetric(x)) {
    return(FALSE)
  }
  A <- x != 0 # Adjacency
  # Repeat the adjaceny as much as it is needed.
  l <- lapply(seq_len(ncol(A) - 1), function(y){A})
  # Calculate the power
  # (there are more efficient ways but for small matrices it should work)
  red <- Reduce(`%*%`, l, init = A, accumulate = TRUE)
  # Add them up (S)
  final <- Reduce(`+`, red)
  all(final != 0)
}
