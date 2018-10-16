lib_size_norm <- function(x) {
    lib_sizes <- colSums(x)
    norm_factors <- lib_sizes / 1000000
    t(t(x) / norm_factors)
}

lib_size_norm_expr <- function(x, log = TRUE, prior = 1) {
    counts <- counts(x)
    expr <- lib_size_norm(counts)
    if (log) {
        base::log(expr + prior)
    } else {
        expr
    }
}

impute_drimpute <- function(data) {
    expr <- lib_size_norm_expr(data)
    expr_processed <- DrImpute::preprocessSC(expr)
    DrImpute::DrImpute(expr_processed)
}

impute_knn_smooth <- function(data) {
    source("https://raw.githubusercontent.com/yanailab/knn-smoothing/master/knn_smooth.R")
    expr <- lib_size_norm_expr(data)
    knn_smoothing(mat = expr, k = 32)
}

impute_basics <- function(data) {
    counts <- counts(data)
    stringr::str_detect(rownames(counts), "ERCC")
    SpikeInput = rgamma(10,1,1)
    SpikeInfo <- data.frame("SpikeID" = paste0("Spike", 1:10), "SpikeInput" = SpikeInput)

    data = newBASiCS_Data(Counts, Tech, SpikeInfo)

}