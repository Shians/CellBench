# alias for subsetting operator for code clarity elsewhere
subset_inds <- `[`

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

# filter down to highest expressed genes
keep_high_count_genes <- function(x, n) {
    stopifnot(is(x, "SingleCellExperiment"))
    highest <- rowSums(SingleCellExperiment::counts(x)) %>%
        order(decreasing = TRUE) %>%
        subset_inds(seq_len(n))

    x[highest, ]
}

# filter down to largest samples
keep_high_count_cells <- function(x, n) {
    stopifnot(is(x, "SingleCellExperiment"))
    highest <- colSums(SingleCellExperiment::counts(x)) %>%
        order(decreasing = TRUE) %>%
        subset_inds(seq_len(n))

    x[, highest]
}

# filter down to highest expressed genes
keep_high_var_genes <- function(x, n) {
    stopifnot(is(x, "SingleCellExperiment"))
    counts <- SingleCellExperiment::counts(x)
    scaled_var <- row_apply(counts, var) / rowSums(counts)
    highest <- scaled_var %>%
        order(decreasing = TRUE) %>%
        `[`(seq_len(n))

    x[highest, ]
}