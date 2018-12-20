#' Get head of 2 dimensional object as a square block
#'
#' head prints all columns which may flood the console, mhead takes a square
#' block which can look nicer and still provide a good inspection of the
#' contents
#'
#' @param x the object with 2 dimensions
#' @param n the size of the n-by-n block to extract
#'
#' @return an n-by-n sized subset of x
#' @export
#'
#' @examples
#' x <- matrix(runif(100), nrow = 10, ncol = 10)
#'
#' mhead(x)
#' mhead(x, n = 3)
mhead <- function(x, n = 6) {
    stopifnot(
        !is.null(dim(x)),
        is.numeric(n),
        n > 0
    )

    n1 <- min(n, nrow(x))
    n2 <- min(n, ncol(x))
    x[seq_len(n1), seq_len(n2)]
}

# left to right function composition
chain <- function(...) {
    do.call(purrr::compose, rev(list(...)))
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

#' Collapse benchmark_tbl into a two column summary
#'
#' Collapse benchmark_tbl into two columns: "pipeline" and "result". The
#' "pipeline" column will be the concatenated values from the data and methods
#' columns while the "result" column remains unchanged from the benchmark_tbl.
#' This is useful for having a string summary of the pipeline for annotating.
#'
#' @param x the benchmark_tbl to collapse
#' @param sep the separator to use for concatenating the pipeline steps
#' @param drop.steps if the data name and methods steps should be dropped from
#'   the output. TRUE by default.
#'
#' @return benchmark_tbl with pipeline and result columns (and all other columns
#'   if drop.steps is FALSE)
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # list of data
#' datasets <- list(
#'     set1 = rnorm(500, mean = 2, sd = 1),
#'     set2 = rnorm(500, mean = 1, sd = 2)
#' )
#'
#' # list of functions
#' add_noise <- list(
#'     none = identity,
#'     add_bias = function(x) { x + 1 }
#' )
#'
#' res <- apply_methods(datasets, add_noise)
#' pipeline_collapse(res)
pipeline_collapse <- function(x, sep = arrow_sep("right"), drop.steps = TRUE) {
    stopifnot(
        is(x, "benchmark_tbl"),
        dplyr::last(colnames(x)) == "result"
    )

    results <- dplyr::pull(x, "result")

    x <- dplyr::select(x, -"result") %>%
        tidyr::unite("pipeline", dplyr::everything(), sep = sep, remove = drop.steps) %>%
        dplyr::select(-"pipeline", "pipeline") %>%
        tibble::as.tibble()

    tibble::add_column(x, result = results)
}

unicode_arrow <- function(towards = c("right", "left", "up", "down")) {
    towards <- match.arg(towards)
    switch(
        towards,
        "left" = "\u2190",
        "up" = "\u2191",
        "right" = "\u2192",
        "down" = "\u2193"
    )
}

#' Unicode arrow separators
#'
#' Utility function for generating unicode arrow separators.
#'
#' @param towards the direction the unicode arrow points towards
#'
#' @return a string containing an unicode arrow surrounded by two spaces
#' @export
#'
#' @examples
#' arrow_sep("left") # left arrrow
#' arrow_sep("up") # up arrrow
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

#' convert to list of results with pipeline as name
#'
#' @param x the benchmark_tbl object to convert
#'
#' @importFrom stats setNames
#' @export
#'
#' @return list containing the results with names set to data and pipeline steps
#'   separated by ..
#'
#' @examples
#' # list of data
#' datasets <- list(
#'     set1 = rnorm(500, mean = 2, sd = 1),
#'     set2 = rnorm(500, mean = 1, sd = 2)
#' )
#'
#' # list of functions
#' add_noise <- list(
#'     none = identity,
#'     add_bias = function(x) { x + 1 }
#' )
#'
#' res <- apply_methods(datasets, add_noise)
#' as_pipeline_list(res)
as_pipeline_list <- function(x) {
    stopifnot(is(x, "benchmark_tbl"))

    if (dplyr::last(colnames(x)) != "result") {
        # if benchmark_tbl has been manipulated by user to non-standard form
        return(as.list.data.frame(x))
    }

    x <- pipeline_collapse(x, sep = "..")

    setNames(x$result, nm = x$pipeline)
}

# suppress prints, which many people use as if they were messages
suppressPrint <- function(expr) {
    utils::capture.output(x <- expr)
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

# generate sequence along number of rows
seq_nrow <- function(x) {
    if (!is.numeric(nrow(x))) return(integer(0))
    seq_len(nrow(x))
}

# generate sequence along number of columns
seq_ncol <- function(x) {
    if (!is.numeric(ncol(x))) return(integer(0))
    seq_len(ncol(x))
}

make_combinations <- function(...) {
    out <- do.call(
        purrr::partial(expand.grid, stringsAsFactors = FALSE),
        rev(list(...))
    )
    out[, rev(colnames(out))]
}

all_same_class <- function(x) {
    classes <- purrr::map(x, class)

    intersect_classes <- sort(purrr::reduce(classes, intersect))
    first_classes <- sort(classes[[1]])

    # all elements have the same class if the intersection of all classes
    # has the same length as the first classes and same values
    (length(intersect_classes) == length(first_classes)) &&
        all.equal(intersect_classes, first_classes)
}