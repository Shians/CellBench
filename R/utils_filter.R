#' Filter out zero count genes
#'
#' Remove all genes (rows) where the total count is 0
#'
#' @param x the SingleCellExperiment or matrix to filter
#'
#' @return object of same type as input with all zero count genes removed
#' @export
#'
#' @examples
#' x <- matrix(rep(0:5, times = 5), nrow = 6, ncol = 5)
#' filter_zero_genes(x)
filter_zero_genes <- function(x) {
    stopifnot(is_one_of(x, c("SingleCellExperiment", "matrix")))

    if (is(x, "SingleCellExperiment")) {
        zero_genes <- rowSums(SingleCellExperiment::counts(x)) == 0
    } else {
        zero_genes <- rowSums(x) == 0
    }

    x[!zero_genes, ]
}

#' Filter down to the highest count genes
#'
#' Filter a SingleCellExperiment or matrix down to the genes (rows) with the
#' highest counts
#'
#' @param x the SingleCellExperiment or matrix
#' @param n the number of highest count genes to keep
#'
#' @return object of same type as input containing the highest count genes
#'
#' @importFrom magrittr extract
#' @export
#'
#' @examples
#' data(sample_sce_data)
#' keep_high_count_genes(sample_sce_data, 300)
keep_high_count_genes <- function(x, n) {
    stopifnot(is_one_of(x, c("SingleCellExperiment", "matrix")))

    if (is(x, "SingleCellExperiment")) {
        counts <- SingleCellExperiment::counts(x)
    } else {
        counts <- x
    }

    highest <- rowSums(counts) %>%
        order(decreasing = TRUE) %>%
        magrittr::extract(seq_len(n)) %>%
        sort()

    x[highest, ]
}

#' Filter down to the highest count cells
#'
#' Filter a SingleCellExperiment or matrix down to the cells (columns) with the
#' highest counts
#'
#' @param x the SingleCellExperiment or matrix
#' @param n the number of highest count cells to keep
#'
#' @return object of same type as input containing the highest count cells
#'
#' @importFrom magrittr extract
#' @export
#'
#' @examples
#' data(sample_sce_data)
#' keep_high_count_cells(sample_sce_data, 10)
keep_high_count_cells <- function(x, n) {
    stopifnot(is_one_of(x, c("SingleCellExperiment", "matrix")))

    if (is(x, "SingleCellExperiment")) {
        counts <- SingleCellExperiment::counts(x)
    } else {
        counts <- x
    }

    highest <- colSums(counts) %>%
        order(decreasing = TRUE) %>%
        magrittr::extract(seq_len(n))  %>%
        sort()

    x[, highest]
}

#' Filter down to the most variable genes
#'
#' Filter a SingleCellExperiment or matrix down to the most variable genes
#' (rows), variability is determined by var() scaled by the total counts for the
#' gene.
#'
#' @param x the SingleCellExperiment or matrix
#' @param n the number of most variable genes to keep
#'
#' @return object of same type as input containing the most variable genes
#'
#' @importFrom stats var
#' @importFrom magrittr extract
#' @export
#'
#' @examples
#' data(sample_sce_data)
#' keep_high_var_genes(sample_sce_data, 50)
keep_high_var_genes <- function(x, n) {
    stopifnot(is_one_of(x, c("SingleCellExperiment", "matrix")))
    stopifnot(n < nrow(x))

    if (is(x, "SingleCellExperiment")) {
        counts <- SingleCellExperiment::counts(x)
    } else {
        counts <- x
    }

    scaled_var <- row_apply(counts, stats::var) / rowSums(counts)
    highest <- scaled_var %>%
        order(decreasing = TRUE) %>%
        magrittr::extract(seq_len(n))

    x[highest, ]
}
