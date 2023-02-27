#' Print method for task_error object
#'
#' task_error are objects that result from failed methods
#'
#' @param x a task_error object
#' @param ... not used
#'
#' @return None
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
#' @return None
#'
#' @export
#' @keywords internal
#'
#' @examples
#' fn_seq <- fn_arg_seq(kmeans, centers = 1:3)
#' fn_seq
print.fn_arg_seq <- function(x, ...) {
    print(glue::glue("List of {length(x)} partial functions"))
    print(glue::glue(" $ {names(x)}"))
}

#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.task_error <- function(x, ...) {
  "task_error"
}

#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.remote_error <- function(x, ...) {
  "remote_error"
}

#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.SingleCellExperiment <- function(x, ...) {
  "SingleCellExperiment"
}
