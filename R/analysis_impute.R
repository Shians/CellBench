impute_drimpute <- function(data) {
    expr <- lib_size_norm_expr(data)
    expr_processed <- DrImpute::preprocessSC(expr)
    count_mat <- DrImpute::DrImpute(expr_processed)
    count_mat
}

impute_knn_smooth <- function(data) {
    source("https://raw.githubusercontent.com/yanailab/knn-smoothing/master/knn_smooth.R")
    expr <- counts(data)
    k <- ceiling(ncol(data) / 20)
    count_mat <- knn_smoothing(mat = expr, k = k) %>% lib_size_norm_expr()
    count_mat
}

impute_basics <- function(data) {
    count_mat <- run_basics(data)
    count_mat %>% lib_size_norm_expr()
}
