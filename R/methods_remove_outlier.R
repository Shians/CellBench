remove_outlier_scpipe <- function(x) {
    stopifnot(is(x, "SingleCellExperiment"))

    x <- scPipe::detect_outlier(x, comp = 2)
    scPipe::remove_outliers(x)
}