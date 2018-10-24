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

# check if object is a list of functions
is_fn_list <- function(x) {
    is(x, "list") && all(purrr::map_lgl(x, is.function))
}

# create outer product by function composition
fn_outer_prod <- function(fn_list1, fn_list2) {
    stopifnot(is_fn_list(fn_list1))
    stopifnot(is_fn_list(fn_list2))

    fnames1 <- names(fn_list1)
    fnames2 <- names(fn_list2)

    output <- list()
    for (fname1 in fnames1) {
        for (fname2 in fnames2) {
            fname <- paste(fname1, fname2, sep = "..")
            output[[fname]] <- chain(fname1, fname2)
        }
    }

    class(output) <- c("fn_list", class(output))
    output
}