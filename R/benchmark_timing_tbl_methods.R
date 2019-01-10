strip_timing <- function(x) {
    UseMethod("strip_timing", x)
}

#' importFrom rlang .data
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

unpack_timing <- function(x, ...) {
    UseMethod("unpack_timing", x)
}

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
        dplyr::select(-"timing", -"timed_result")
}
