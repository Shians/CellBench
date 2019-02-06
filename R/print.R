#' Print method for task_error object
#'
#' task_error are objects that result from failed methods
#'
#' @param x a task_error object
#' @param ... not used
#'
#' @export
#' @keywords internal
print.task_error <- function(x, ...) {
    cat(
        glue::glue(
            "error at step: '{x$error_location}'",
            "message: 'error: {x$message}'",
            "traceback() is available for this object",
            .sep = "\n"
        )
    )

    invisible() # guard against implicit return
}

#' Print method for fn_arg_seq output
#'
#' @param x fn_arg_seq object
#' @param ... addition arguments for print
#'
#' @export
#' @keywords internal
#'
#' @examples
#' fn_seq <- fn_arg_seq(kmeans, centers = 1:3)
#' fn_seq
print.fn_arg_seq <- function(x, ...) {
    print(utils::str(x), ...)
}
