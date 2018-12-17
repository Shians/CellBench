# constructor for a function list
fn_list <- function(...) {
    out <- list(...)

    if (!all(purrr::map_lgl(out, is.function))) {
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

    class(out) <- c("fn_list", class(out))
    out
}
