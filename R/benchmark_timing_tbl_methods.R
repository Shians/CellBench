#' Strip timing information
#'
#' Takes the result of a time_methods() call and remove timing information from
#' the `timed_result` column, replacing it with a `result` column and converting
#' it to a benchmark_tbl.
#'
#' @param x the benchmark_timing_tbl object
#'
#' @return benchmark_tbl
#'
#' @seealso [unpack_timing()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' datasets <- list(
#'     data1 = 1:1e8,
#' )
#'
#' transforms <- list(
#'     log = log,
#'     sqrt = sqrt
#' )
#'
#' datasets %>%
#'     time_methods(transforms) %>%
#'     strip_timing()
#' }
#'
strip_timing <- function(x) {
    UseMethod("strip_timing", x)
}

#' @rdname strip_timing
#' @importFrom rlang .data
#' @export
#' @keywords internal
strip_timing.benchmark_timing_tbl <- function(x) {
    x <- x %>%
        dplyr::mutate(result = purrr::map(.data$timed_result, function(x) x$result)) %>%
        dplyr::select(-"timed_result")

    if (all_length_one(x$result)) {
        x$result <- unlist(x$result)
    }

    x <- drop_class(x, "benchmark_timing_tbl")
    x <- add_class(x, "benchmark_tbl")

    x
}


#' Unpack timing information
#'
#' Takes the result of a time_methods() call and remove the `timed_result`
#' column, replacing it with three columns of durations representing the
#' `system`, `user` and `elapsed` times from a system.time() call.
#'
#' @param x the benchmark_timing_tbl object
#'
#' @return a tibble containing pipeline steps and timing information
#'
#' @seealso [strip_timing()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' datasets <- list(
#'     data1 = c(1, 2, 3)
#' )
#'
#' transforms <- list(
#'     log = function(x) { Sys.sleep(0.1); log(x) },
#'     sqrt = function(x) { Sys.sleep(0.1); sqrt(x) }
#' )
#'
#' datasets %>%
#'     time_methods(transforms) %>%
#'     unpack_timing()
#' }
#'
unpack_timing <- function(x) {
    UseMethod("unpack_timing", x)
}

#' @rdname unpack_timing
#' @importFrom dplyr mutate select
#' @importFrom purrr map map_dbl
#' @export
#' @keywords internal
unpack_timing.benchmark_timing_tbl <- function(x) {
    x %>%
        dplyr::mutate(
            timing = purrr::map(.data$timed_result, function(x) x$timing)
        ) %>%
        dplyr::mutate(
            user = duration_seconds(
                purrr::map_dbl(.data$timing, function(x) x[["user"]])
            ),
            system = duration_seconds(
                purrr::map_dbl(.data$timing, function(x) x[["system"]])
            ),
            elapsed = duration_seconds(
                purrr::map_dbl(.data$timing, function(x) x[["elapsed"]])
            )
        ) %>%
        dplyr::select(-"timing", -"timed_result") %>%
        drop_class("benchmark_timing_tbl")
}
