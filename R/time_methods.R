library(tibble)

simple_time <- function(...) {
    summary(system.time(...))
}

time_methods <- function(x, fn_list, .name = NULL, suppress.messages = TRUE) {
    UseMethod("time_methods", x)
}

time_methods.list <- function(
    x,
    fn_list,
    .name = NULL,
    suppress.messages = TRUE
) {
    data_names <- names(x)
    method_names <- names(fn_list)

    if (length(method_names) != length(fn_list)) {
        stop("every element of fn_list must be named")
    }

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
