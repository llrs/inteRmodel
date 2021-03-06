#' @name ge_cgh_locIGR
#' @title Data from GE CGH and location
#'
#' @description This data set contains a list of 3 dataframe. On dataframe for
#' GE, CGH and localization. This dat is the 2nd version of segmentation.
#' Misclassified patients were excluded and segmentation was re-run excluding
#' those patients (done july 2012 - C Philippe)
#' @docType data
#' @usage data(ge_cgh_locIGR)
#' @format one instance, 1 row per patient
#' @source IGR, Villejuif, France
#' @author V Frouin, 2012-07-31
#' @note This data is copied from the gliomaData package at \url{http://biodev.cea.fr/sgcca/}
#' @examples
#' #read data
#' data(ge_cgh_locIGR)
#' names(ge_cgh_locIGR)
#' names(ge_cgh_locIGR$multiblocks)
#' class(ge_cgh_locIGR$multiblocks$GE)
#' class(ge_cgh_locIGR$multiblocks$CGH)
#' class(ge_cgh_locIGR$multiblocks$y)
#' head(ge_cgh_locIGR$multiblocks$y)
#' class(ge_cgh_locIGR$ylabel)
#' # See difference between multiblock_y (a dummy variable matrix)
#' # and ylabel a vector of labels (1, 2, 3)
#' head(ge_cgh_locIGR$ylabel,10)
#' head(ge_cgh_locIGR$multiblocks$y,10)
NULL
