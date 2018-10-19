# take the "head" of matrix/tables as a n by n block
mhead <- function(x, n = 6) {
    stopifnot(!is.null(dim(x)))
    stopifnot(n < ncol(x))
    stopifnot(n < nrow(x))

    x[1:n, 1:n]
}

# left to right function composition
chain <- function(...) {
    fs <- lapply(list(...), match.fun)

    first <- fs[[1]]
    rest <- fs[-1]

    function(...) {
        out <- first(...)
        for (f in rest) {
            out <- f(out)
        }
        out
    }
}

