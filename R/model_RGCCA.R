
#' Prepare metadata data for RGCCA.
#'
#' Prepares factors into their vectors. Each level of a factor is converted
#' to a column, numeric columns are left as is.
#' @param data A data.frame with the information about the samples
#' @param columns The name of the columns to be used to build the matrix
#' @param intercept A logical value if you want one column with all 1 or not.
#' @return A matrix with each factor is decomposed in as much columns as
#' factors has minus 1 and with the numeric values as they were.
#' @export
#' @seealso [fastDummies::dummy_cols()]
#' @examples
#' block <- model_RGCCA(iris, c("Petal.Width", "Species"))
model_RGCCA <- function(data, columns, intercept = FALSE){

  m <- data[, columns, drop = FALSE]
  num <- vapply(m, is.numeric, logical(1L))
  if (any(!num)) { # For categorical data
    if (sum(!num) > 1) { # When multiple columns are present
      o <- sapply(m[, !num, drop = FALSE], function(x){
        levels <- unique(x)
        levels <- levels[!is.na(levels)]
        o <- vapply(levels, function(level) {
          as.numeric(x %in% level)
        }, numeric(nrow(data)))
        colnames(o) <- levels
        o[, -1, drop = FALSE]
      })
      o <- do.call(cbind, o)
    } else { # Just one categorical column (we must not drop the dimensions)
      levels <- unique(m[, !num])
      levels <- levels[!is.na(levels)]
      o <- vapply(levels, function(level) {
        as.numeric(m[, !num] %in% level)
      }, numeric(nrow(data)))
      colnames(o) <- levels
      o <- o[, -1, drop = FALSE]
    }
  }

  if (any(!num) & any(num)) {
    out <- cbind(o, m[, num, drop = FALSE])
  } else if (any(!num)) {
    out <- o
  } else {
    out <- m[, num, drop = FALSE]
  }

  colnames(out)[colnames(out) == ""] <- seq_len(sum(colnames(out) == ""))

  if (intercept) {
    cbind(1, out)
  } else {
    out
  }
}
