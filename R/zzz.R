.onLoad <- function(libname, pkgname) {
    op <- options()
    op.CellBench <- list(
        CellBench.threads = 1,
        CellBench.bpparam = BiocParallel::SerialParam(stop.on.error = FALSE)
    )

    # set any options not already set
    toset <- !(names(op.CellBench) %in% names(op))
    if (any(toset)) options(op.CellBench[toset])

    invisible()
}
