apply_methods <- function(x, fn_list) {
    UseMethod("apply_methods", x)
}

# create data x methods cross product
# returns list of (data_name, method_name, result) values
apply_methods.list <- function(data_list, fn_list) {
    d_names <- names(data_list)
    m_names <- names(fn_list)

    output <- list()
    for (d_name in d_names) {
        for (m_name in m_names) {
            result <- list(
                data_list = d_name,
                .temp = m_name,
                result = fn_list[[m_name]](data_list[[d_name]])
            )
            output <- append(output, list(result))
        }
    }

    output <- tibble::tibble(
        data = purrr::map_chr(output, function(x) x$data),
        .temp = purrr::map_chr(output, function(x) x$.temp),
        result = purrr::map(output, function(x) x$result)
    )
    names(output)[2] <- deparse(substitute(fn_list))

    if (all_length_one(output$result)) {
        output$result <- unlist(output$result)
    }
    class(output) <- c("benchmark_tbl", class(output))

    output
}

all_length_one <- function(x) {
    stopifnot(is(x, "list"))
    all(sapply(x, function(x) { length(x) == 1 }))
}

apply_methods.tbl_df <- function(tbl_df, fn_list) {
    m_names <- names(fn_list)

    results <- list()
    for (data in tbl_df$result) {
        for (fn in fn_list[order(names(fn_list))]) {
            results <- append(results, list(fn(data)))
        }
    }

    output <- tbl_df %>% select(-result)
    output <- tidyr::crossing(tbl_df, names(fn_list))
    names(output)[ncol(output)] <- deparse(substitute(fn_list))
    output <- output %>%
        dplyr::mutate(result = results) %>%
        dplyr::select(-result, result) # move result column to end

    if (all_length_one(output$result)) {
        output$result <- unlist(output$result)
    }
    class(output) <- c("benchmark_tbl", class(output))

    output
}

apply_metrics <- apply_methods
begin_benchmark <- apply_methods