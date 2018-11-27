#' @export
impute_drimpute <- function(expr) {
    expr_processed <- DrImpute::preprocessSC(expr)
    count_mat <- DrImpute::DrImpute(expr_processed)
    count_mat
}

#' @export
impute_knn_smooth <- function(expr, k = ceiling(ncol(data) / 20)) {
    source("https://raw.githubusercontent.com/yanailab/knn-smoothing/master/knn_smooth.R")
    count_mat <- knn_smoothing(mat = expr, k = k) %>% lib_size_norm_expr()
    count_mat
}

#' @export
impute_basics <- function(expr, ...) {
    count_mat <- run_basics(expr, ...)
    log(count_mat + 1)
}
