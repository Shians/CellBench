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
    is(x, "list") && purrr::every(x, is.function)
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
#' @param data.name if the dataset name should be included in the pipeline
#'   string. Useful if only a single dataset is used.
#'
#' @return benchmark_tbl with pipeline and result columns (and all other columns
#'   if drop.steps is FALSE)
#'
#' @importFrom rlang .data
#' @export
#'
#' @seealso \code{\link{as_pipeline_list}}
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
#' collapse_pipeline(res)
collapse_pipeline <- function(
    x,
    sep = arrow_sep("right"),
    drop.steps = TRUE,
    data.name = TRUE
) {
    stopifnot(
        is(x, "benchmark_tbl"),
        dplyr::last(colnames(x)) == "result"
    )

    results <- dplyr::pull(x, "result")

    if (!data.name) {
        data <- x$data
        x <- dplyr::select(x, -"data")
    }

    x <- dplyr::select(x, -"result") %>%
        tidyr::unite("pipeline", dplyr::everything(), sep = sep, remove = drop.steps) %>%
        dplyr::select(-"pipeline", "pipeline") %>% # put "pipeline" on last column
        tibble::as_tibble() %>%
        dplyr::mutate(pipeline = factor_no_sort(.data$pipeline))

    if (!data.name) {
        tibble::add_column(x, data = data, before = 1)
    }

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

ascii_arrow <- function(towards = c("right", "left")) {
    switch(
        towards,
        "left" = "\u00AB",
        "right" = "\u00BB"
    )
}

#' Unicode arrow separators
#'
#' Utility function for generating unicode arrow separators.
#'
#' @param towards the direction the unicode arrow points towards
#' @param unicode whether unicode arrows should be used. Does not work inside
#'   plots within knitted PDF documents.
#'
#' @return a string containing an unicode arrow surrounded by two spaces
#' @export
#'
#' @examples
#' arrow_sep("left") # left arrrow
#' arrow_sep("right") # right arrrow
arrow_sep <- function(towards = c("right", "left"), unicode = FALSE) {
    towards <- match.arg(towards)
    if (!unicode) {
        arrow <- ascii_arrow(towards)
    } else {
        arrow <- unicode_arrow(towards)
    }
    glue::glue(" {arrow} ")
}

# create factor with levels in order they appear rather than alphabetically
# sorted
factor_no_sort <- function(x) {
    factor(x, levels = unique(x))
}

#' convert benchmark_tbl to list
#'
#' convert a benchmark_tbl to a list where the name of the elements represent the pipeline steps separated by "..". This can be useful for using the apply family of functions.
#'
#' @param x the benchmark_tbl object to convert
#'
#' @importFrom stats setNames
#' @export
#'
#' @return list containing the results with names set to data and pipeline steps
#'   separated by ..
#'
#' @seealso \code{\link{collapse_pipeline}}
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
        stop("final column should contain 'result'")
    }

    x <- collapse_pipeline(x, sep = "..")

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

# expand.grid altered so that last variable varies the fastest
# @importFrom tibble as_tibble
# @importFrom magrittr set_names
make_combinations <- function(...) {
    input_names <- infer_names_from_dots(...)
    input <- list(...) %>%
        magrittr::set_names(input_names)

    # unnaming data.frame list elements required for tidyr >= 1.0.0
    names(input)[purrr::map_lgl(input, is.data.frame)] <- ""

    is.valid.input <- function(x) {
        is.character(x) || is.data.frame(x) || is.factor(x)
    }

    if (!purrr::every(input, is.valid.input)) {
        stop("all arguments must be either data.frames, character or factor vectors")
    }

    input <- purrr::map(
        input,
        function(x) {
            if (is.data.frame(x)) {
                return(x)
            } else {
                return(factor_no_sort(x))
            }
        }
    )

    tibble::as_tibble(do.call(tidyr::crossing, input))
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

# check that all elements of a list have length one
all_length_one <- function(x) {
    stopifnot(is(x, "list"))
    purrr::every(x, function(x) { length(x) == 1 })
}

# add class to the classes of an object
add_class <- function(x, class) {
    stopifnot(is.character(class))

    existing_class <- class(x)
    if (class %in% existing_class) {
        return(x)
    } else {
        class(x) <- c(class, existing_class)
        return(x)
    }
}

# drop class from classes of an object
drop_class <- function(x, class) {
    stopifnot(is.character(class))

    classes <- class(x)
    if (!class %in% classes) {
        return(x)
    } else {
        class(x) <- setdiff(classes, class)
        return(x)
    }
}

# convert to duration in seconds
#' @importFrom lubridate seconds as.duration
duration_seconds <- function(x, digits = 3) {
    round(x, digits = digits) %>%
        lubridate::seconds() %>%
        lubridate::as.duration()
}

# wrapper to return summaried time in numerics rath
simple_time <- function(...) {
    summary(system.time(...))
}

# replace null values with default value
if_null_then <- function(x, value) {
    if (is.null(x)) {
        x <- value
    }
    x
}

# replace empty values with default value
if_empty_then <- function(x, value) {
    if (length(x) == 0) {
        x <- value
    }
    x
}

# take variadic ellipses and return a vector of names,
#' @importFrom rlang exprs
infer_names_from_dots <- function(...) {
    var_names <- purrr::map_chr(as.list(substitute(list(...))[-1L]), deparse)
    given_names <- names(list(...))
    if (is.null(given_names)) {
        output <- var_names
    } else {
        output <- ifelse(given_names == "", var_names, given_names)
    }

    if (!all_unique(output)) {
        warning("not all names were unique, numbers appended to duplicates")
    }
    make.names(output, unique = TRUE)
}

# convert rows of a data frame into list
#' @importFrom magrittr set_names
df_to_tasks <- function(df, names = rownames(df)) {
    stopifnot(is(df, "data.frame"))
    split(df, seq(nrow(df))) %>%
        magrittr::set_names(names)
}

# Retain for backward compatibility
#' @rdname collapse_pipeline
#' @export
pipeline_collapse <- collapse_pipeline
