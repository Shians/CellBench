#' Constructor for a function list
#'
#' Constructor for a list of functions, a thin wrapper around list() which
#' checks that all the inputs are functions and have names
#'
#' @param ... objects, must all be named
#'
#' @return a list of named functions
#' @export
#'
#' @examples
#' flist <- fn_list(
#'     mean = mean,
#'     median = median
#' )
fn_list <- function(...) {
    out <- list(...)

    if (!purrr::every(out, is.function)) {
        stop("all fn_list members must be functions")
    }

    if (is.null(names(out))) {
        fn_name <- deparse(substitute(...))
        stop(glue::glue("all fn_list members must have names, e.g. fn_list(fn1 = {fn_name})"))
    }

    if (any(names(out) == "")) {
        missing_names <- which(names(out) == "")
        missing_names <- glue::glue(collapse_with_comma("{missing_names}"))
        stop(glue::glue("all fn_list members must have names, indices of members without name: {missing_names}"))
    }

    out
}
