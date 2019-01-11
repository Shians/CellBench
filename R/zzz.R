.onLoad <- function(libname, pkgname) {
    op <- options()
    op.CellBench <- list()

    # set any options not already set
    toset <- !(names(op.CellBench) %in% names(op))
    if (any(toset)) options(op.CellBench[toset])

    invisible()
}
