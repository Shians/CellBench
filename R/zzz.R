.onLoad <- function(libname, pkgname) {
    op <- options()
    op.CellBench <- list(
        "CellBench.threads" = 1
    )

    toset <- !(names(op.CellBench) %in% names(op))
    if (any(toset)) options(op.CellBench[toset])

    invisible()
}
