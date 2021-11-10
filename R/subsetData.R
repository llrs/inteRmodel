# Given an index keep those of the list of matrices or data.frames
#' Subset a list
#'
#' Given a list and an index subset each element of the list and
#' remove the variables that are constant.
#' @note It also removes constant variables of the data.
#' @param A A list of an array with samples in rows and variables in the columns.
#' @param index The samples to keep.
#' @return A list with the samples.
#' @export
#' @importFrom stats var
#' @examples
#' data(ge_cgh_locIGR)
#' A <- subsetData(ge_cgh_locIGR$multiblocks, sample(53, replace = TRUE))
#' str(A)
subsetData <- function(A, index) {
  if (max(index) > nrow(A[[1]])) {
    stop("Wrong index for the data given.",
         "\nExpected to select a sample that does not exists")
  }
  l <- lapply(A, function(x, inde) {
    y <- x[inde, , drop = FALSE] # subset
    v <- apply(y, 2, var)
    y <- y[,  !is.na(v) & v != 0, drop = FALSE] # Remove variables that are constant.
    rownames(y) <- seq_len(nrow(y)) # To have the same row names without duplicates
    y
  }, inde = index)
  names(l) <- names(A)
  l
}
