filter_zero_genes <- function(x) {
    stopifnot(is_one_of(x, c("SingleCellExperiment", "matrix")))

    if (is(x, "SingleCellExperiment")) {
        zero_genes <- rowSums(SingleCellExperiment::counts(x)) == 0
    } else {
        zero_genes <- rowSums(x) == 0
    }

    x[!zero_genes, ]
}
