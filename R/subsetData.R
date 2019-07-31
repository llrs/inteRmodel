# Given an index keep those of the list of matrices or data.frames
#' Subset a list
#'
#' Given a list and an index subset each element of the list.
#' @note It also removes constant variables of the data.
#' @param A A list of an array with samples in rows and variables in the columns.
#' @param index The samples to keep.
#' @return A list with the samples.
#' @export
#' @importFrom stats var
#' @examples
#' data(ge_cgh_locIGR)
#' A <- subsetData(ge_cgh_locIGR$multiblocks, sample(53))
subsetData <- function(A, index) {
  lapply(A, function(x, inde){
    y <- x[inde, , drop = FALSE] # subset
    v <- apply(y, 2, var)
    y[,  !is.na(v) | v != 0] # Remove variables that are constant.
  }, inde = index)
}
