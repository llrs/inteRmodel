% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterate.R
\name{iterate_model}
\alias{iterate_model}
\alias{search_model}
\title{Functions related to finding models}
\usage{
iterate_model(..., BPPARAM = BiocParallel::SerialParam())

search_model(..., nWeights = 3, BPPARAM = BiocParallel::SerialParam())
}
\arguments{
\item{...}{All the same arguments that would be passed to sggca, pass named arguments.}

\item{BPPARAM}{Set up parallel backend (see BiocParallel documentation).}

\item{nWeights}{The number of weights used to check the possible designs.}
}
\value{
A matrix with the design of the model
}
\description{
Look for every variation of the models changing the weights by 0.1.
}
\section{Functions}{
\itemize{
\item \code{search_model}: Search for the right model for the blocks provided.
}}

\examples{
data("Russett", package = "RGCCA")
X_agric <- as.matrix(Russett[, c("gini", "farm", "rent")])
X_ind <- as.matrix(Russett[, c("gnpr", "labo")])
X_polit <- as.matrix(Russett[ , c("inst", "ecks",  "death", "demostab",
                                  "dictator")])
A <- list(Agric = X_agric, Ind = X_ind, Polit = X_polit)
C <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), 3, 3)
out <- search_model(A = A, C = C, c1 =rep(1, 3), scheme = "factorial",
               scale = FALSE, verbose = FALSE,
               ncomp = rep(1, length(A)),
               bias = TRUE, BPPARAM = BiocParallel::SerialParam())
head(out)
# From all the models, we select that with the higher inner AVE:
model <- extract_model(C, out, "inner")
# We then look for a variation of the weights of this model
out <- iterate_model(A = A, C = model, c1 =rep(1, 3), scheme = "factorial",
               scale = FALSE, verbose = FALSE,
               ncomp = rep(1, length(A)),
               bias = TRUE)
}
\seealso{
\code{sgcca}
}
