# sample n rows from data
sample_rows <- function(x, n) {
    if (nrow(x) > n) {
        x[sample(1:nrow(x), n), ]
    } else {
        x
    }
}

# sample n cols from data
sample_cols <- function(x, n) {
    if (ncol(x) > n) {
        x[, sample(1:ncol(x), n)]
    } else {
        x
    }
}

# sample n cells from SingleCellExperiment
sample_cells <- function(x, n) {
    stopifnot(is(x, "SingleCellExperiment"))
    sample_cols(x, n)
}

# sample n genes from SingleCellExperiment
sample_genes <- function(x, n) {
    stopifnot(is(x, "SingleCellExperiment"))
    sample_rows(x, n)
}
