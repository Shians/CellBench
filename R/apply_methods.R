#' Apply methods
#'
#' @param x the list of data or benchmark tibble to apply methods to
#' @param fn_list the list of methods to be applied
#' @param .name (optional) the name of the column for methods applied
#' @param suppress.messages TRUE if messages from running methods should be
#'   suppressed
#'
#' @return benchmark_tbl object containing results from methods applied
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
#'     add_bias = function(x) { x + 1 },
#' )
#'
#' res <- apply_methods(datasets, add_noise)
#'
apply_methods <- function(x, fn_list, .name = NULL, suppress.messages = TRUE) {
    UseMethod("apply_methods", x)
}

# create data x methods cross product
# returns list of (data_name, method_name, result) values
apply_methods.list <- function(data_list, fn_list, .name = NULL, suppress.messages = TRUE) {
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
                            result = suppressMessages(fn_list[[m_name]](data_list[[d_name]]))
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
                            result = suppressMessages(fn_list[[m_name]](data_list[[d_name]]))
                        )
                        list(result)
                    }
                ) %>% purrr::reduce(append)
            }
        ) %>% purrr::reduce(append)
    }

    output <- tibble::tibble(
        data = factor_no_sort(purrr::map_chr(output, function(x) x$data)),
        .temp = factor_no_sort(purrr::map_chr(output, function(x) x$.temp)),
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

apply_methods.tbl_df <- function(tbl_df, fn_list, .name = NULL, suppress.messages = TRUE) {
    m_names <- names(fn_list)

    if (missing(".name")) {
        .name <- deparse(substitute(fn_list))
    }

    results <- list()
    for (data in tbl_df$result) {
        for (fn in fn_list) {
            results <- append(results, list(suppressMessages(fn(data))))
        }
    }

    output <- tbl_df %>% select(-result)
    output <- tidyr::crossing(tbl_df, factor_no_sort(m_names))
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

#' @describeIn apply_methods
#'
#' @export
#'
apply_metrics <- apply_methods

#' @describeIn apply_methods
#'
#' @export
#'
begin_benchmark <- apply_methods
