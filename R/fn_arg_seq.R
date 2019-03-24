#' Create a list of functions with arguments varying over a sequence
#'
#' Generate a list of functions where specific arguments have been pre-applied
#' from a sequences of arguments, i.e. a function f(x, n) may have the 'n'
#' argument pre-applied with specific values to obtain functions f1(x, n = 1)
#' and f2(x, n = 2) stored in a list.
#'
#' If multiple argument vectors are provided
#' then the combinations of arguments in the sequences will be generated.
#'
#' @param func function to generate list from
#' @param ... vectors of values to use as arguments
#' @param .strict TRUE if argument names are checked, giving an error if
#'   specified argument does not appear in function signature. Note that
#'   functions with multiple methods generally have only f(x, ...) as their
#'   signature, so the check would fail even if the arguments are passed on.
#'
#' @return list of functions with the specified arguments pre-applied. Names of
#'     the list indicate the values that have been pre-applied.
#'
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' f <- function(x) {
#'     cat("x:", x)
#' }
#'
#' f_list <- fn_arg_seq(f, x = c(1, 2))
#' f_list
#' f_list[[1]]() # x: 1
#' f_list[[2]]() # x: 2
#'
#' g <- function(x, y) {
#'     cat("x:", x, "y:", y)
#' }
#'
#' g_list <- fn_arg_seq(g, x = c(1, 2), y = c(3, 4))
#' g_list
#' g_list[[1]]() # x: 1 y: 3
#' g_list[[2]]() # x: 1 y: 4
#' g_list[[3]]() # x: 2 y: 3
#' g_list[[4]]() # x: 2 y: 4
fn_arg_seq <- function(func, ..., .strict = FALSE) {
    stopifnot(is.function(func))

    # get actual fucntion name
    func_name <- deparse(substitute(func))
    args <- list(...)

    # find arguments inputted but not used by func
    names_args <- names(args)
    names_f_args <- names(formals(func))
    invalid_args <- setdiff(names_args, names_f_args)

    # stop if there are invalid args
    if (.strict && length(invalid_args) != 0) {
        invalid_args <- glue::glue("'{invalid_args}'") %>% collapse_with_comma()
        stop(glue::glue("args not used in {func_name}: {invalid_args}"))
    }

    arg_combs <- do.call(
        purrr::partial(expand.grid, stringsAsFactors = FALSE),
        args
    )
    cnames <- colnames(arg_combs)

    rows_as_list <- function(df) {
        row_apply(df, function(x) { as.list(x) %>% setNames(nm = cnames) }) %>%
            purrr::map(function(x) { append(list(".f" = func), x) })
    }

    out <- purrr::map(
        rows_as_list(arg_combs),
        function(x) { do.call(purrr::partial, x) }
    )

    arg_sigs <- row_apply(
        arg_combs,
        function(x) {
            paste(glue::glue("{cnames} = {x}"), collapse = ", ")
        }
    )

    call_sigs <- glue::glue("{func_name}({arg_sigs})")

    names(out) <- call_sigs

    out
}
