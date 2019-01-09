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

keep_time_attr <- function(x, ...) {
    UseMethod("keep_time_attr", x)
}

keep_time_attr.benchmark_timing_tbl <- function(x, key) {
    x %>%
        mutate(timing = map_dbl(timing, function(x) x[[key]])) %>%
        mutate(timing = as.duration(seconds(round(timing, digits = 3))))
}

keep_elapsed <- function(x, ...) {
    UseMethod("keep_elapsed", x)
}

keep_elapsed.benchmark_timing_tbl <- function(x) {
    keep_time_attr(x, "elapsed")
}

keep_user <- function(x, ...) {
    UseMethod("keep_user", x)
}

keep_user.benchmark_timing_tbl <- function(x) {
    keep_time_attr(x, "user")
}

keep_system <- function(x, ...) {
    UseMethod("keep_system", x)
}

keep_system.benchmark_timing_tbl <- function(x) {
    keep_time_attr(x, "system")
}
