#' Check for task errors
#'
#' This is a helper function for checking the result column of a benchmark_tbl
#' for task_error objects. This is useful for filtering out rows where the
#' result is a task error.
#'
#' @param x the object to be tested
#'
#' @return vector of logicals denoting if elements of the list are task_error objects
#' @export
is.task_error <- function(x) {
    purrr::map_lgl(x, function(xx) is(xx, "task_error"))
}
