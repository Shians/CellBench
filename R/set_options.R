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
#' @seealso \code{\link{set_cellbench_bpparam}} for more advanced interface
#'
#' @export
#'
#' @examples
#' set_cellbench_threads(1) # CellBench runs on a single thread
#'
set_cellbench_threads <- function(nthreads = 1) {
    stopifnot(
        is.numeric(nthreads),
        nthreads >= 1
    )

    options("CellBench.threads" = nthreads)

    if (nthreads == 1) {
        options(
            "CellBench.bpparam" = BiocParallel::SerialParam(
                stop.on.error = FALSE
            )
        )
    } else {
        if (.Platform$OS.type == "windows") {
            options(
                "CellBench.bpparam" = BiocParallel::SnowParam(
                    workers = nthreads,
                    stop.on.error = FALSE
                )
            )
        } else {
            options(
                "CellBench.bpparam" = BiocParallel::MulticoreParam(
                    workers = nthreads,
                    stop.on.error = FALSE
                )
            )
        }
    }

    invisible() # guard against implicit returns
}

#' Set BiocParallel parameter used CellBench
#'
#' This is a more advanced interface for changing CellBench's parallelism
#' settings. Internally CellBench uses BiocParallel for parallelism, consult
#' the documentation of BiocParallel to see what settings are available.
#'
#' @param param the BiocParallel parameter object
#'
#' @seealso \code{\link{set_cellbench_threads}} for more basic interface
#'
#' @return None
#'
#' @importFrom BiocParallel bpnworkers
#' @export
#'
#' @examples
#' set_cellbench_threads(1) # CellBench runs on a single thread
#'
set_cellbench_bpparam <- function(param) {
    stopifnot(is(param, "BiocParallelParam"))

    options("CellBench.threads" = BiocParallel::bpnworkers(param))
    options("CellBench.bpparam" = param)

    invisible() # guard against implicit returns
}

#' Set CellBench cache path
#'
#' @return None
#'
#' @param path the path to where method caches should be stored
#'
#' @export
#'
#' @seealso \code{\link{cache_method}} for constructing cached methods.
#'
#' @examples
#' \dontrun{
#' # hidden folder in local path
#' set_cellbench_cache_path(".CellBenchCache"))
#' }
#' # store in temp directory valid for this session
#' set_cellbench_cache_path(file.path(tempdir(), ".CellBenchCache"))
#'
set_cellbench_cache_path <- function(path = "./.CellBenchCache") {
    stopifnot(is.character(path))

    options("CellBench.cache" = memoise::cache_filesystem(path = path))
    options("CellBench.cache_path" = path)

    invisible() # guard against implicit returns
}

