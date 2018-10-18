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

    data <- BASiCS::newBASiCS_Data(counts, tech_features, spike_info)
    chain <- BASiCS::BASiCS_MCMC(data, N = 50, Thin = 2, Burn = 10, Regression = TRUE, PrintProgress = FALSE)
    BASiCS::BASiCS_DenoisedCounts(Data = data, Chain = chain)
}
