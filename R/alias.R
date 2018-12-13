###
# File for storing simple aliases for existing functions
###

# alias for subsetting operator for code clarity elsewhere
subset_inds <- `[`

# apply function across rows
row_apply <- purrr::partial(apply, MARGIN = 1)

# apply function down columns
col_apply <- purrr::partial(apply, MARGIN = 2)

# glue collapse with default sep and last
collapse_with_comma <- purrr::partial(
    glue::glue_collapse,
    sep = ", ",
    last = " and "
)


#' Get path to CellBench packaged data
#'
#' Search CellBench package for packaged data, leaving argument empty will list
#' the available data.
#'
#' @param filename the name of the file to look for
#'
#' @return string containing the path to the packaged data
#' @export
#'
#' @examples
cellbench_file <- function(filename = NULL) {
    if (is.null(filename)) {
        dir(system.file("extdata", package = "CellBench"))
    } else {
        system.file("extdata", filename, package = "CellBench")
    }
}
