# sample n rows from data
sample_rows <- function(x, n) {
    if (nrow(x) > n) {
        x[sample(seq_nrow(x), n), ]
    } else {
        x
    }
}

# sample n cols from data
sample_cols <- function(x, n) {
    if (ncol(x) > n) {
        x[, sample(seq_ncol(x), n)]
    } else {
        x
    }
}

#' @export
# sample n cells from SingleCellExperiment
sample_cells <- function(x, n) {
    stopifnot(is(x, "SingleCellExperiment"))
    sample_cols(x, n)
}

#' @export
# sample n genes from SingleCellExperiment
sample_genes <- function(x, n) {
    stopifnot(is(x, "SingleCellExperiment"))
    sample_rows(x, n)
}
