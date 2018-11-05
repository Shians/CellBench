#' Set number of threads used by CellBench
#'
#' Sets global parameter for CellBench to use multiple threads for applying
#' methods. If any methods applied are multi-threaded then it's recommended to
#' set CellBench threads to 1. It only recommended to use CellBench with
#' multiple threads if methods applied can be set to run on single threads.
#'
#' @param nthreads the number of threads used by CellBench
#'
#' @export
#'
#' @examples
#' set_cellbench_threads(1) # CellBench runs on a single thread
#'
set_cellbench_threads <- function(nthreads = 1) {
    stopifnot(is.numeric(nthreads))

    if (nthreads < 1) {
        nthreads = 1
    }

    options("CellBench.threads" = nthreads)

    invisible()
}
