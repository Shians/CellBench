#' Time methods
#'
#' apply_methods take either lists of datasets or benchmark_tbl objects and
#' apply a list of functions. The output is a benchmark_timing_tbl where each
#' method has been applied to each dataset or preceeding result.
#'
#' @param x the list of data or benchmark tibble to apply methods to
#' @param fn_list the list of methods to be applied
#' @param .name (optional) the name of the column for methods applied
#' @param suppress.messages TRUE if messages from running methods should be
#'   suppressed
#'
#' @return benchmark_tbl object containing results and timing from methods applied
#'
#' @importFrom magrittr %>%
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
#'     unpack_timing()
#'
time_methods <- function(x, fn_list, .name = NULL, suppress.messages = TRUE) {
    method_names <- names(fn_list)
    if (length(method_names) != length(fn_list)) {
        stop("every element of fn_list must be named")
    }

    UseMethod("time_methods", x)
}

#' @export
time_methods.list <- function(
    x,
    fn_list,
    .name = NULL,
    suppress.messages = TRUE
) {
    data_names <- names(x)
    method_names <- names(fn_list)

    if (is.null(.name)) {
        .name <- deparse(substitute(fn_list))
    }

    output <- make_combinations(data_names, method_names)
    colnames(output) <- c("data", .name)

    tasks <- purrr::map2(
        output$data,
        output[[.name]],
        function(dname, fname) {
            list(
                data = x[[dname]],
                method = fn_list[[fname]]
            )
        }
    )

    timed_result <- lapply(
        X = tasks,
        FUN = function(task) {
            list(
                timing = simple_time(res <- task$method(task$data)),
                result = res
            )
        }
    )

    output <- tibble::as_tibble(output)
    output <- tibble::add_column(output, timed_result = timed_result)

    output$data <- factor_no_sort(output$data)
    output[[.name]] <- factor_no_sort(output[[.name]])

    class(output) <- c("benchmark_timing_tbl", class(output))

    output
}

#' @rdname time_methods
#' @importFrom rlang .data
#' @export
time_methods.benchmark_timing_tbl <- function(
    x,
    fn_list,
    .name = NULL,
    suppress.messages = TRUE
) {
    stopifnot(all_unique(names(fn_list)))

    method_names <- names(fn_list)

    if (missing(".name")) {
        .name <- deparse(substitute(fn_list))
    }

    output <- make_combinations(x, method_names)

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

    results <- lapply(
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
    names(output)[ncol(output)] <- .name
    output <- output %>%
        tibble::add_column(timed_result = results)

    if (!"benchmark_timing_tbl" %in% class(output)) {
        class(output) <- c("benchmark_timing_tbl", class(output))
    }

    output
}
