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
    counts <- counts(data) %>% filter_zero_genes()
    tech_features <- stringr::str_detect(rownames(Counts), "ERCC")
    tech_counts <- counts[tech_features, ]
    spike_info <- data.frame("SpikeID" = rownames(Counts)[tech_features], "SpikeInput" = row_apply(tech_counts, max))

    data <- BaSICS::newBASiCS_Data(counts, tech_features, spike_info)
    chain <- BaSICS::BASiCS_MCMC(data, N = 200, Thin = 2, Burn = 100, Regression = TRUE, PrintProgress = FALSE)
    BaSICS::BASiCS_DenoisedCounts(Data = data, Chain = chain)
}
