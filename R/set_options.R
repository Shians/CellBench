#' Set number of threads used by CellBench
#'
#' Sets global parameter for CellBench to use multiple threads for applying
#' methods. If any methods applied are multi-threaded then it's recommended to
#' set CellBench threads to 1. It only recommended to use CellBench with
#' multiple threads if methods applied can be set to run on single threads.
#'
#' @param nthreads the number of threads used by CellBench
#'
#' @return None
#'
#' @export
#'
#' @examples
#' set_cellbench_threads(1) # CellBench runs on a single thread
#'
set_cellbench_threads <- function(nthreads = 1) {
    stopifnot(is.numeric(nthreads))

    if (nthreads < 1) {
        nthreads <- 1
    }

    options("CellBench.threads" = nthreads)

    invisible()
}

#' Set CellBench cache path
#'
#' @param path the path to where method caches should be stored
#'
#' @return None
#'
#' @export
#'
#' @seealso [cache_method()]
#'
#' @examples
#' \dontrun{
#' set_cellbench_cache(".CellBenchCache")) # hidden folder in local path
#' }
#' set_cellbench_cache(file.path(tempdir(), ".CellBenchCache")) # store in temp directory valid for this session
#'
set_cellbench_cache_path <- function(path = "./.CellBenchCache") {
    stopifnot(is.character(path))

    options("CellBench.cache" = memoise::cache_filesystem(path = path))
    options("CellBench.cache_path" = path)

    invisible()
}

