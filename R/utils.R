# take the "head" of matrix/tables as a n by n block
mhead <- function(x, n = 6) {
    stopifnot(
        !is.null(dim(x)),
        n < ncol(x),
        n < nrow(x)
    )

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

# explicitly set namespaced function for scope of function
using <- function(x, allow.global = FALSE) {
    stopifnot(is.function(x))

    namespaced_fn <- deparse(substitute(x))
    fn_name <- gsub(".*::", "", namespaced_fn)

    parent_is_global <- identical(parent.frame(), globalenv())
    if (parent_is_global && !allow.global) {
        stop(glue::glue("Cannot assign namespaced function `{namespaced_fn}` to global environment."))
    }

    assign(fn_name, eval(namespaced_fn), envir = parent.frame())
}

# summarise benchmark_tbl into two columns
pipeline_summarise <- function(x, sep = arrow_sep("right"), drop.steps = TRUE) {
    stopifnot(
        is(x, "benchmark_tbl"),
        dplyr::last(colnames(x)) == "result"
    )

    results <- dplyr::pull(x, result)

    x <- dplyr::select(x, -result) %>%
        tidyr::unite("pipeline", dplyr::everything(), sep = sep, remove = drop.steps) %>%
        dplyr::select(-pipeline, pipeline) %>%
        tibble::as.tibble()

    tibble::add_column(x, result = results)
}

unicode_arrow <- function(towards = c("right", "left", "up", "down")) {
    towards <- match.arg(towards)
    switch(
        towards,
        "right" = "→",
        "left" = "←",
        "up" = "↑",
        "down" = "↓"
    )
}

arrow_sep <- function(towards = c("right", "left", "up", "down")) {
    towards <- match.arg(towards)
    arrow <- unicode_arrow(towards)
    glue::glue(" {arrow} ")
}

# create factor with levels in order they appear rather than alphabetically
# sorted
factor_no_sort <- function(x) {
    factor(x, levels = unique(x))
}

# convert to list of results with pipeline as name
as_pipeline_list <- function(x) {
    stopifnot(is(x, "benchmark_tbl"))

    if (dplyr::last(colnames(x)) != "result") {
        # if benchmark_tbl has been manipulated by user to non-standard form
        return(as.list.data.frame(x))
    }

    x <- pipeline_summarise(x, sep = "..")

    setNames(x$result, nm = x$pipeline)
}

# suppress prints, which many people use as if they were messages
suppressPrint <- function(expr) {
    capture.output(x <- expr)
    x
}

# suppresses messages and prints
suppressMsgAndPrint <- function(expr, suppress = TRUE) {
    if (suppress) {
        suppressMessages(suppressPrint(expr))
    } else {
        expr
    }
}