#' Impute using DrImpute
#'
#' @param expr the expression matrix to impute on
#' @return matrix of imputed expression values
#' @export
impute_drimpute <- function(expr) {
    expr_processed <- DrImpute::preprocessSC(expr)
    count_mat <- DrImpute::DrImpute(expr_processed)
    count_mat
}

#' Impute using knn smooth
#'
#' @param expr the expression matrix to impute on
#' @param k the number of nearest neighbours to use for smoothing
#' @return matrix of imputed expression values
#' @export
impute_knn_smooth <- function(expr, k = ceiling(ncol(data) / 20)) {
    knn_smoothing <- function() {} # dummy binding
    source("https://raw.githubusercontent.com/yanailab/knn-smoothing/master/knn_smooth.R")
    count_mat <- knn_smoothing(mat = expr, k = k) %>% lib_size_norm_expr()
    count_mat
}

#' Impute using knn BaSICS
#'
#' @param expr the expression matrix to impute on
#' @param ... additional arguments for BaSICS
#' @return matrix of imputed expression values
#' @export
impute_basics <- function(expr, ...) {
    count_mat <- run_basics(expr, ...)
    log(count_mat + 1)
}
