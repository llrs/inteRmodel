#' Explore multiple designs
#'
#' @inheritParams boot_samples_sgcca
#' @inheritDotParams boot_samples_sgcca
#' @param designs A list of designs such as that obtained with [weight_design()].
#' @param sample A numeric value with the number of samples to test.
#' @export
#' @return A data.frame with information about the models performance.
#' @seealso [boot_index()]
boot_design <- function(designs, sample = 10000, ...,
                        BPPARAM = BiocParallel::SerialParam()) {

  # Subset the designs tested
  if (sample < length(designs)) {
    designs <- sample(designs, sample)
  }
  out <- BiocParallel::bplapply(designs, do.call, what = testing,
                                args = as.list(...), BPPARAM = BPPARAM)
  out <- simplify2array(out[lengths(out) != 1])
  as.data.frame(t(out))
}
