#' Constructor for a data list
#'
#' Constructor for a list of data, a thin wrapper around list() which
#' checks that all the inputs are of the same type and have names
#'
#' @param ... objects, must all be named
#'
#' @return a list of named data
#' @export
#'
#' @examples
#' data(iris)
#' flist <- data_list(
#'     data1 = iris[1:20, ],
#'     data2 = iris[21:40, ]
#' )
data_list <- function(...) {
    out <- list(...)

    if (!all_same_class(out)) {
        stop("all data_list members must be have the same class")
    }

    if (is.null(names(out))) {
        data_name <- deparse(substitute(...))
        stop(glue::glue("all data_list members must have names, e.g. data_list(fn1 = {data_name})"))
    }

    if (any(names(out) == "")) {
        missing_names <- which(names(out) == "")
        missing_names <- glue::glue(collapse_with_comma("{missing_names}"))
        stop(glue::glue("all data_list members must have names, indices of members without name: {missing_names}"))
    }

    class(out) <- c("data_list", class(out))
    out
}
