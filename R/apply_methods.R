#' Apply methods
#'
#' apply_methods() and its aliases apply_metrics and begin_benchmark take either
#' lists of datasets or benchmark_tbl objects and applies a list of functions.
#' The output is a benchmark_tbl where each method has been applied to each
#' dataset or preceeding result.
#'
#' @param x the list of data or benchmark tibble to apply methods to
#' @param fn_list the list of methods to be applied
#' @param name (optional) the name of the column for methods applied
#' @param suppress.messages TRUE if messages from running methods should be
#'   suppressed
#'
#' @return benchmark_tbl object containing results from methods applied, the
#'   first column is the name of the dataset as factors, middle columns contain
#'   method names as factors and the final column is a list of results of
#'   applying the methods.
#'
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{time_methods}}
#'
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
#'
apply_methods <- function(x, fn_list, name = NULL, suppress.messages = TRUE) {
    method_names <- names(fn_list)
    if (length(method_names) != length(fn_list)) {
        stop("every element of fn_list must be named")
    }

    UseMethod("apply_methods", x)
}

#' @rdname apply_methods
#' @importFrom BiocParallel SnowParam MulticoreParam
#' @importFrom tibble tibble
#' @export
apply_methods.list <- function(
    x,
    fn_list,
    name = NULL,
    suppress.messages = TRUE
) {
    data_names <- names(x)
    if (length(data_names) != length(x)) {
        stop("every element of x must be named")
    }

    method_names <- names(fn_list)

    if (is.null(name)) {
        name <- deparse(substitute(fn_list))
    }

    multithread_param <- getOption("CellBench.bpparam")

    output <- make_combinations(data_names, method_names) %>%
        magrittr::set_colnames(c("data", name))

    tasks <- .generate_tasks(output, x, fn_list, name)

    result <- .bp_try_apply(
        BPPARAM = multithread_param,
        X = tasks,
        suppress.messages = suppress.messages,
        FUN = function(task, suppress.messages) {
            suppressMsgAndPrint(
                task$method(task$data),
                suppress = suppress.messages
            )
        }
    )

    result <- purrr::map(
        result,
        name = name,
        function(res, name) {
            if (is.error(res) && is.null(res$error_location)) {
                res$error_location <- name
                res <- add_class(res, "task_error")
            }
            res
        }
    )

    output <- .make_output(output, result, name)
    output <- add_class(output, "benchmark_tbl")

    output
}

#' @rdname apply_methods
#' @importFrom rlang .data
#' @export
apply_methods.benchmark_tbl <- function(
    x,
    fn_list,
    name = NULL,
    suppress.messages = TRUE
) {
    stopifnot(all_unique(names(fn_list)))

    method_names <- names(fn_list)

    if (missing("name")) {
        name <- deparse(substitute(fn_list))
    }

    multithread_param <- getOption("CellBench.bpparam")

    tasks <- list()
    for (data in x$result) {
        for (fn in fn_list) {
            tasks <- append(
                tasks,
                list(list(method = fn, data = data))
            )
        }
    }

    results <- .bp_try_apply(
        BPPARAM = multithread_param,
        X = tasks,
        suppress.messages = suppress.messages,
        FUN = function(task, suppress.messages) {
            if (is.error(task$data)) {
                task$data
            } else {
                suppressMsgAndPrint(
                    task$method(task$data),
                    suppress = suppress.messages
                )
            }
        }
    )

    results <- purrr::map(
        results,
        name = name,
        function(res, name) {
            if (is.error(res) && is.null(res$error_location)) {
                res$error_location <- name
                res <- add_class(res, "task_error")
            }
            res
        }
    )

    output <- x %>% dplyr::select(-"result")
    output <- tidyr::crossing(output, factor_no_sort(method_names))
    names(output)[ncol(output)] <- name
    output <- output %>%
        tibble::add_column(result = results)

    if (all_length_one(output$result)) {
        output$result <- unlist(output$result)
    }


    output <- add_class(output, "benchmark_tbl")

    output
}

#' @rdname apply_methods
#' @export
apply_methods.tbl_df <- apply_methods.benchmark_tbl

#' @rdname apply_methods
#'
#' @export
apply_metrics <- apply_methods

#' @rdname apply_methods
#'
#' @export
begin_benchmark <- apply_methods

# wrapper for task generation
.generate_tasks <- function(output_tbl, x, fn_list, name) {
    purrr::map2(
        output_tbl$data,
        output_tbl[[name]],
        function(dname, fname) {
            list(
                data = x[[dname]],
                method = fn_list[[fname]]
            )
        }
    )
}

# wrapper for bptry-bplapply pattern
.bp_try_apply <- function(...) {
    BiocParallel::bptry(
        BiocParallel::bplapply(
            ...
        )
    )
}

# assemble output
.make_output <- function(output, result, name, timed = FALSE) {
    output <- tibble::as_tibble(output)

    if (timed) {
        output <- tibble::add_column(output, timed_result = result)
    } else {
        output <- tibble::add_column(output, result = result)
        if (all_length_one(output$result)) {
            output$result <- unlist(output$result)
        }
    }

    output$data <- factor_no_sort(output$data)
    output[[name]] <- factor_no_sort(output[[name]])

    output
}
