#' Time methods
#'
#' time_methods() take either lists of datasets or benchmark_timing_tbl objects
#' and applies a list of functions. The output is a benchmark_timing_tbl where
#' each method has been applied to each dataset or preceding result. Unlike
#' apply_methods(), time_methods() is always single threaded as to produce fair
#' and more consistent timings.
#'
#' @param x the list of data or benchmark timing tibble to apply methods to
#' @param fn_list the list of methods to be applied
#' @param name (optional) the name of the column for methods applied
#' @param suppress.messages TRUE if messages from running methods should be
#'   suppressed
#'
#' @return benchmark_timing_tbl object containing results from methods applied,
#'   the first column is the name of the dataset as factors, middle columns
#'   contain method names as factors and the final column is a list of lists
#'   containing the results of applying the methods and timings from calling
#'   system.time().
#'
#' @importFrom magrittr %>%
#' @importFrom BiocParallel SerialParam bplapply bptry
#'
#' @seealso \code{\link{apply_methods}}
#'
#' @export
#'
#' @examples
#' datasets <- list(
#'     set1 = 1:1e7
#' )
#'
#' transform <- list(
#'     sqrt = sqrt,
#'     log = log
#' )
#'
#' time_methods(datasets, transform) %>%
#'     unpack_timing() # extract timings out of list
#'
time_methods <- function(x, fn_list, name = NULL, suppress.messages = TRUE) {
    method_names <- names(fn_list)
    if (length(method_names) != length(fn_list)) {
        stop("every element of fn_list must be named")
    }

    UseMethod("time_methods", x)
}

#' @rdname time_methods
#' @export
time_methods.list <- function(
    x,
    fn_list,
    name = NULL,
    suppress.messages = TRUE
) {
    data_names <- names(x)
    method_names <- names(fn_list)

    if (is.null(name)) {
        name <- deparse(substitute(fn_list))
    }

    output <- make_combinations(data_names, method_names)
    colnames(output) <- c("data", name)

    tasks <- .generate_tasks(output, x, fn_list, name)

    timed_result <-
        .bp_try_apply(
            BPPARAM = BiocParallel::SerialParam(stop.on.error = FALSE),
            X = tasks,
            FUN = function(task) {
                list(
                    timing = simple_time(res <- task$method(task$data)),
                    result = res
                )
            }
        )

    output <- .make_output(output, timed_result, name, timed = TRUE)
    output <- add_class(output, "benchmark_timing_tbl")

    output
}

#' @rdname time_methods
#' @importFrom rlang .data
#' @importFrom BiocParallel SerialParam bplapply bptry
#' @export
time_methods.benchmark_timing_tbl <- function(
    x,
    fn_list,
    name = NULL,
    suppress.messages = TRUE
) {
    stopifnot(all_unique(names(fn_list)))

    method_names <- names(fn_list)

    if (missing("name")) {
        # get name from variable name
        name <- deparse(substitute(fn_list))
        name <- gsub("methods$", "method", name)
    }

    tasks <- list()
    for (data in x$timed_result) {
        for (fn in fn_list) {
            tasks <- append(
                tasks,
                list(
                    list(
                        method = fn,
                        data = data$result,
                        timing = data$timing
                    )
                )
            )
        }
    }

    results <-
        .bp_try_apply(
            BPPARAM = BiocParallel::SerialParam(stop.on.error = FALSE),
            X = tasks,
            FUN = function(task) {
                list(
                    timing = simple_time(res <- task$method(task$data)) + task$timing,
                    result = res
                )
            }
        )

    output <- x %>% dplyr::select(-"timed_result")
    output <- tidyr::crossing(output, factor_no_sort(method_names))
    names(output)[ncol(output)] <- name
    output <- output %>%
        tibble::add_column(timed_result = results)

    if (!"benchmark_timing_tbl" %in% class(output)) {
        output <- add_class(output, "benchmark_timing_tbl")
    }

    output
}
