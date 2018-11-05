#' Apply methods
#'
#' @param x the list of data or benchmark tibble to apply methods to
#' @param fn_list the list of methods to be applied
#' @param .name (optional) the name of the column for methods applied
#'
#' @return benchmark_tbl object containing results from methods applied
#' @export
#'
#' @examples
apply_methods <- function(x, fn_list, .name = NULL) {
    UseMethod("apply_methods", x)
}

# create data x methods cross product
# returns list of (data_name, method_name, result) values
apply_methods.list <- function(data_list, fn_list, .name = NULL) {
    d_names <- names(data_list)
    m_names <- names(fn_list)
    if (length(m_names) != length(fn_list)) {
        stop("every element of fn_list must be named")
    }

    if (is.null(.name)) {
        .name <- deparse(substitute(fn_list))
    }

    n_threads <- min(getOption("CellBench.threads"), length(data_list))

    if (n_threads > 1) {
        output <- BiocParallel::bplapply(
            BPPARAM = BiocParallel::MulticoreParam(n_threads),
            d_names,
            function(d_name) {
                purrr::map(
                    m_names,
                    function(m_name) {
                        result <- list(
                            data_list = d_name,
                            .temp = m_name,
                            result = fn_list[[m_name]](data_list[[d_name]])
                        )
                        list(result)
                    }
                ) %>% purrr::reduce(append)
            }
        ) %>% purrr::reduce(append)
    } else {
        output <- purrr::map(
            d_names,
            function(d_name) {
                purrr::map(
                    m_names,
                    function(m_name) {
                        result <- list(
                            data_list = d_name,
                            .temp = m_name,
                            result = fn_list[[m_name]](data_list[[d_name]])
                        )
                        list(result)
                    }
                ) %>% purrr::reduce(append)
            }
        ) %>% purrr::reduce(append)
    }

    output <- tibble::tibble(
        data = purrr::map_chr(output, function(x) x$data),
        .temp = purrr::map_chr(output, function(x) x$.temp),
        result = purrr::map(output, function(x) x$result)
    )
    names(output)[2] <- .name

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

apply_methods.tbl_df <- function(tbl_df, fn_list, .name = NULL) {
    m_names <- names(fn_list)

    if (missing(".name")) {
        .name <- deparse(substitute(fn_list))
    }

    results <- list()
    for (data in tbl_df$result) {
        for (fn in fn_list[order(names(fn_list))]) {
            results <- append(results, list(fn(data)))
        }
    }

    output <- tbl_df %>% select(-result)
    output <- tidyr::crossing(tbl_df, names(fn_list))
    names(output)[ncol(output)] <- .name
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