strip_results <- function(x) {
    UseMethod("strip_results", x)
}

strip_results.benchmark_timing_tbl <- function(x) {
    x <- x %>%
        mutate(timing = map(timed_result, function(x) x[["timing"]])) %>%
        select(-"timed_result")

    class(x) <- add_class(x, "benchmark_timing_tbl")

    x
}

strip_timing <- function(x) {
    UseMethod("strip_timing", x)
}

strip_timing.benchmark_timing_tbl <- function(x) {
    x <- x %>%
        mutate(result = map(timed_result, function(x) x$result)) %>%
        select(-"timed_result")

    if (all_length_one(x$result)) {
        x$result <- unlist(x$result)
    }

    x <- drop_class(x, "benchmark_timing_tbl")
    x <- add_class(x, "benchmark_tbl")

    x
}

unpack_timed_result.benchmark_timing_tbl <- function(x) {
    x %>%
        mutate(timing = map(timed_result, function(x) x$timing)) %>%
        mutate(
            user = duration_seconds(
                map_dbl(timing, function(x) x[["user"]])
            ),
            system = duration_seconds(
                map_dbl(timing, function(x) x[["system"]])
            ),
            elapsed = duration_seconds(
                map_dbl(timing, function(x) x[["elapsed"]])
            )
        ) %>%
        select(-timing, -timed_result)
}
