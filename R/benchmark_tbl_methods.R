#' Summary of benchmark_tbl
#'
#' @param object the benchmark_tbl to be summarised
#' @param ... additional arguments affecting the summary produced.
#'
#' @return None
#'
#' @export
#' @keywords internal
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
#'     add_bias = function(x) { x + 1 }
#' )
#'
#' res <- apply_methods(datasets, add_noise)
#' summary(res)
summary.benchmark_tbl <- function(object, ...) {
    if (dplyr::last(colnames(object)) != "result") {
        # if benchmark_tbl has been manipulated by user to non-standard form
        print(summary.data.frame(object))
        return()
    }

    method_names <- names(object)
    method_names <- method_names[-1]
    method_names <- method_names[-length(method_names)]

    out <- ""

    out <- c(glue::glue("Pipeline summary:"), out)
    pipeline_str_vec <- c("data", glue::glue("{method_names}"), "result")
    out <- c(out, glue::glue_collapse(pipeline_str_vec, sep = " \u2192 "))

    names(method_names) <- method_names
    unique_method_list <- purrr::map(
        method_names,
        function(nm) unique(object[, nm]) %>% dplyr::pull(1)
        )

    for (method_name in method_names) {
        unique_methods <- unique_method_list[[method_name]]
        out <- c(out, glue::glue(""))
        out <- c(out, glue::glue("{method_name} variants:"))
        out <- c(out, glue::glue_collapse(glue::glue(" * {unique_methods}"), sep = "\n"))
    }

    cat(out, sep = "\n")
}
