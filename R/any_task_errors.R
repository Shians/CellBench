#' Check if any tasks produced errors
#'
#' Check the results column of a benchmark tibble for any task_error objects.
#'
#' @param x the tibble to check
#' @param verbose TRUE if the rows with errors should be reported
#'
#' @return TRUE if any entry in the result column is a task_error object
#' @export
any_task_errors <- function(x, verbose) {
    UseMethod("any_task_errors", x)
}

#' @describeIn any_task_errors
#'
#' @export
any_task_errors.benchmark_tbl <- function(x, verbose = FALSE) {
    any_errors <- FALSE

    for (i in seq_along(x$result)) {
        res <- x$result[[i]]

        if (is(res, "task_error")) {
            any_errors <- TRUE

            if (verbose) {
                message(glue::glue("task_error in row {i}"))
            }
        }
    }

    any_errors
}

# TODO: Implement for benchmark_timing_tbl
