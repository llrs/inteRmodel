# Given an index keep those of the list of matrices or data.frames
#' Subset a list
#'
#' Given a list and an index subset each element of the list.
#' @note It also removes constant variables of the data.
#' @param A A list of an array with samples in rows and variables in the columns.
#' @param index The samples to keep.
#' @return A list with the samples.
#' @export
#' @importFrom stats sd
subsetData <- function(A, index) {
  lapply(A, function(x, inde){
    y <- x[inde, , drop = FALSE] # subset
    y[, apply(y, 2, sd) != 0] # Remove variables that are constant.
  }, inde = index)
}
