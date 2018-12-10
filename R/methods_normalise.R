#' # library size normalise
#' .lib_size_norm <- function(x) {
#'     lib_sizes <- colSums(x)
#'     norm_factors <- lib_sizes / 1000000
#'     t(t(x) / norm_factors)
#' }
#'
#'
#' #' Library size normalise and transform to log scale
#' #'
#' #' @param x the SingleCellExperiment or count matrix
#' #' @param log whether output should be log-scale
#' #' @param offset the counts to add before taking log
#' #'
#' #' @return matrix of normalised values
#' #' @export
#' lib_size_norm_expr <- function(x, log = TRUE, offset = 1) {
#'     stopifnot(is_one_of(x, c("matrix", "SingleCellExperiment")))
#'
#'     if (is(x, "matrix")) {
#'         counts <- x
#'     } else {
#'         counts <- SingleCellExperiment::counts(x)
#'     }
#'
#'     expr <- .lib_size_norm(counts)
#'
#'     if (log) {
#'         output <- base::log(expr + offset)
#'     } else {
#'         output <- expr
#'     }
#'
#'     output
#' }
#'
#' #' Scran normalisation
#' #'
#' #' @param x the SingleCellExperiment object
#' #' @param log TRUE if normalised values should be returned on log-scale
#' #'
#' #' @return
#' #'
#' #' @importFrom scran computeSumFactors
#' #' @importFrom scater normalize
#' #' @export
#' scran_norm_expr <- function(x, log = TRUE) {
#'     stopifnot(is(x, "SingleCellExperiment"))
#'
#'     x <- scran::computeSumFactors(x)
#'     x <- scater::normalize(x, log = log)
#'
#'     SingleCellExperiment::logcounts(x)
#' }
