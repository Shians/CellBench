#' Create a cached function for CellBench
#'
#' Take a function and return a cached version. The arguments and results of a
#' cached method is saved to disk and if the cached function is called again
#' with the same arguments then the results will be retrieved from the cache
#' rather than be recomputed.
#'
#' \bold{(CAUTION)} Because cached functions called
#' with the same argument will always return the same output, pseudo-random
#' methods will not return varying results over repeated runs as one might
#' expect.
#'
#' This function is a thin wrapper around \code{\link[memoise]{memoise}}
#'
#' @param f the function to be cached
#' @param cache the cache information (from memoise package)
#'
#' @return function whose results are cached and is called identically to f
#' @export
#'
#' @seealso \code{\link{set_cellbench_cache_path}}
#'
#' @examples
#' # sets cache path to a temporary directory
#' set_cellbench_cache_path(file.path(tempdir(), ".CellBenchCache"))
#' f <- function(x) { x + 1 }
#' cached_f <- cache_method(f)
#'
cache_method <- function(f, cache = getOption("CellBench.cache")) {
    stopifnot(is.function(f))

    if (is.null(cache)) {
        stop("CellBench cache path has not been set, please run `set_cellbench_cache_path()`")
    }

    memoise::memoise(f, cache = cache)
}

#' Clear CellBench Cache
#'
#' Clears the method cache for CellBench
#'
#' @return None
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clear_cellbench_cache()
#' }
#'
clear_cellbench_cache <- function() {
    cache_path <- getOption("CellBench.cache_path")
    if (is.null(cache_path)) {
        return()
    }

    files <- file.path(cache_path, dir(cache_path))

    if (length(files) > 0) {
        rem <- file.remove(files)
    }

    message(glue::glue("{sum(rem)} files removed from cache"))

    invisible()
}