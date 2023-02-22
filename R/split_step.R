#' Split combined pipeline step
#'
#' Some methods perform multiple steps of a pipeline. This function assists with
#' splitting the combined pipeline step into multiple steps with duplicated
#' method names.
#'
#' @param x a results data.frame from `apply_methods()`.
#' @param step the name of the column to split.
#' @param into the name of the columns to split into.
#'
#' @return a results data.frame where the `step` column has been split into
#' the `into` columns with duplicated values.
#'
#' @importFrom rlang :=
#' @export
#'
#' @examples
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
#'
#' res %>%
#'     split_step("add_noise", c("split1", "split2"))
split_step <- function(x, step, into) {
    assert_that(is(x, "data.frame"), msg = "`x` must be a data.frame.")
    assert_that(
        is(step, "character"),
        length(step) == 1,
        msg = "`step` must be character of length 1."
    )
    assert_that(
        is(into, "character"),
        length(into) > 1,
        msg = "`into` must be character of length >1."
    )

    x <- x %>%
        dplyr::rename(!!into[1] := !!step)

    for (i in length(into):2) {
        x <- x %>%
            dplyr::mutate(!!into[i] := .data[[!!into[1]]], .after = !!into[1])
    }

    x
}
