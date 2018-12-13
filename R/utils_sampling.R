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

#' Sample cells from a SingleCellExperiment
#'
#' Sample n cells from a SingleCellExperiment object with no replacement.
#'
#' @param x the SingleCellExperiment object
#' @param n the number of cells to sample
#'
#' @return SingleCellExperiment object
#' @export
#'
#' @examples
#' sample_sce_data <- readRDS(cellbench_file("celseq_sce_sample.rds"))
#' dim(sample_sce_data)
#' x <- sample_cells(sample_sce_data, 10)
#' dim(x)
sample_cells <- function(x, n) {
    stopifnot(is(x, "SingleCellExperiment"))
    sample_cols(x, n)
}

#' Sample genes from a SingleCellExperiment
#'
#' Sample n genes from a SingleCellExperiment object with no replacement
#'
#' @param x the SingleCellExperiment object
#' @param n the number of genes to sample
#'
#' @return SingleCellExperiment object
#' @export
#'
#' @examples
#' sample_sce_data <- readRDS(cellbench_file("10x_sce_sample.rds"))
#' dim(sample_sce_data)
#' x <- sample_genes(sample_sce_data, 50)
#' dim(x)
sample_genes <- function(x, n) {
    stopifnot(is(x, "SingleCellExperiment"))
    sample_rows(x, n)
}
