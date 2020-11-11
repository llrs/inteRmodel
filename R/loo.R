
#' Indexes without one sample
#'
#' Performs the leave-out-one indexing
#' @param size The number of samples
#' @return A list of indices where one sample is excluded
#' @export
#' @examples
#' looIndex(15)
looIndex <- function(size){

  l <- seq_len(size)
  vl <- vector("list", length = size)
  for (i in l) {
    vl[[i]] <- l[-i]
  }
  vl
}
