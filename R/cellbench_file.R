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
#' cellbench_file() # shows available files
#' cellbench_file("10x_sce_sample.rds") # returns path to 10x sample data
cellbench_file <- function(filename = NULL) {
    if (is.null(filename)) {
        dir(system.file("extdata", package = "CellBench"))
    } else {
        output <- system.file("extdata", filename, package = "CellBench")
        if (output == "") {
            stop("file not found, run cellbench_file() to see available files")
        }
        output
    }
}
