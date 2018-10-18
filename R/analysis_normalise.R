# library size normalise
lib_size_norm <- function(x) {
    lib_sizes <- colSums(x)
    norm_factors <- lib_sizes / 1000000
    t(t(x) / norm_factors)
}

# library size normalise and transform to log scale
lib_size_norm_expr <- function(x, log = TRUE, offset = 1) {
    stopifnot(is_one_of(x, c("matrix", "SingleCellExperiment")))
    if (is(x, "matrix")) {
        counts <- x
    } else {
        counts <- SingleCellExperiment::counts(x)
    }
    expr <- lib_size_norm(counts)
    if (log) {
        base::log(expr + offset)
    } else {
        expr
    }
}
