# filter out genes that have zero expression in all samples
filter_zero_genes <- function(x) {
    stopifnot(is_one_of(x, c("SingleCellExperiment", "matrix")))

    if (is(x, "SingleCellExperiment")) {
        zero_genes <- rowSums(SingleCellExperiment::counts(x)) == 0
    } else {
        zero_genes <- rowSums(x) == 0
    }

    x[!zero_genes, ]
}

# filter down to highest expressed genes
keep_high_count_genes <- function(x, n) {
    stopifnot(is_one_of(x, c("SingleCellExperiment", "matrix")))

    if (is(x, "SingleCellExperiment")) {
        counts <- SingleCellExperiment::counts(x)
    } else {
        counts <- x
    }

    highest <- rowSums(counts) %>%
        order(decreasing = TRUE) %>%
        subset_inds(seq_len(n)) %>%
        sort()

    x[highest, ]
}

# filter down to largest samples
keep_high_count_cells <- function(x, n) {
    stopifnot(is_one_of(x, c("SingleCellExperiment", "matrix")))

    if (is(x, "SingleCellExperiment")) {
        counts <- SingleCellExperiment::counts(x)
    } else {
        counts <- x
    }

    highest <- colSums(counts) %>%
        order(decreasing = TRUE) %>%
        subset_inds(seq_len(n))  %>%
        sort()

    x[, highest]
}

#' @importFrom stats var
# filter down to highest expressed genes
keep_high_var_genes <- function(x, n) {
    stopifnot(is_one_of(x, c("SingleCellExperiment", "matrix")))

    if (is(x, "SingleCellExperiment")) {
        counts <- SingleCellExperiment::counts(x)
    } else {
        counts <- x
    }

    scaled_var <- row_apply(counts, stats::var) / rowSums(counts)
    highest <- scaled_var %>%
        order(decreasing = TRUE) %>%
        `[`(seq_len(n))

    x[highest, ]
}
