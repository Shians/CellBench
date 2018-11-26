#' @export
impute_drimpute <- function(data) {
    expr <- scran_norm_expr(data)
    expr_processed <- DrImpute::preprocessSC(expr)
    count_mat <- DrImpute::DrImpute(expr_processed)
    count_mat
}

#' @export
impute_knn_smooth <- function(data, k = ceiling(ncol(data) / 20)) {
    source("https://raw.githubusercontent.com/yanailab/knn-smoothing/master/knn_smooth.R")
    expr <- scran_norm_expr(data)
    count_mat <- knn_smoothing(mat = expr, k = k) %>% lib_size_norm_expr()
    count_mat
}

#' @export
impute_basics <- function(data, ...) {
    count_mat <- run_basics(data, ...)
    log(count_mat + 1)
}
