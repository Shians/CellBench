#' @importFrom rappdirs user_cache_dir
#' @importFrom BiocFileCache BiocFileCache
.get_cache <- function() {
    cache <- rappdirs::user_cache_dir(appname = "CellBench")
    BiocFileCache::BiocFileCache(cache)
}

#' @importFrom BiocFileCache bfcquery bfcadd bfcrpath
#' @importFrom glue glue
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
get_data <- function(url, filename) {
    bfc <- .get_cache()
    rid <- BiocFileCache::bfcquery(bfc, filename, "rname")$rid

    if (length(rid) == 0) {
        message(glue::glue("Downloading data file from {url}"))
        rid <- names(BiocFileCache::bfcadd(bfc, filename, url))
    }

    BiocFileCache::bfcrpath(bfc, rids = rid)
}

#' @describeIn load_all_data Load single cell data
#' @export
load_sc_data <- function() {
    data_path <- get_data(
        "https://github.com/Shians/scBenchData/raw/master/single_cell_data.RData",
        "sc_data"
    )

    # dummy bindings
    sc_10x <- NULL
    sc_celseq <- NULL
    sc_dropseq <- NULL

    load(data_path)

    out <- list(
        "sc_10x" = sc_10x,
        "sc_celseq" = sc_celseq,
        "sc_dropseq" = sc_dropseq
    )

    invisible(out)
}

#' @describeIn load_all_data Load cell mixture data
#' @export
load_cell_mix_data <- function() {
    data_path <- get_data(
        "https://github.com/Shians/scBenchData/raw/master/mix_9cell_data.RData",
        "mrna_mix_data"
    )

    # dummy bindings
    mix_9cell_07clean_1cell_mat <- NULL
    mix_9cell_08clean_1cell_mat <- NULL
    mix_9cell_09clean_1cell_mat <- NULL
    mix_9cell_07clean_3cell_mat <- NULL
    mix_9cell_07clean_90cell_mat <- NULL

    load(data_path)

    out <- list(
        "cell_mix1" = mix_9cell_07clean_1cell_mat,
        "cell_mix2" = mix_9cell_08clean_1cell_mat,
        "cell_mix3" = mix_9cell_09clean_1cell_mat,
        "cell_mix4" = mix_9cell_07clean_3cell_mat,
        "cell_mix5" = mix_9cell_07clean_90cell_mat
    )

    invisible(out)
}

#' @describeIn load_all_data Load mrna mixture data
#' @export
load_mrna_mix_data <- function() {
    data_path <- get_data(
        "https://github.com/Shians/scBenchData/raw/master/mrna_mix_data.RData",
        "cell_mix_data"
    )

    # dummy bindings
    mrna_mix_celseq <- NULL
    mrna_mix_sortseq <- NULL

    load(data_path)

    out <- list(
        "mrna_mix_celseq" = mrna_mix_celseq,
        "mrna_mix_sortseq" = mrna_mix_sortseq
    )

    invisible(out)
}


#' Load CellBench Data
#'
#' Load in all CellBench data described \href{https://github.com/LuyiTian/CellBench_data/blob/25f73cfcc87a84282cf55b4ac963032cc4da4988/README.md#summary-of-all-datasets}{here}.
#'
#' @return list of SingleCellExperiment
#' @export
#'
#' @examples
#' \dontrun{
#' cellbench_data <- load_all_data()
#' }
load_all_data <- function() {
    data_path1 <- get_data(
        "https://github.com/Shians/scBenchData/raw/master/single_cell_data.RData",
        "sc_data"
    )
    data_path2 <- get_data(
        "https://github.com/Shians/scBenchData/raw/master/mrna_mix_data.RData",
        "mrna_mix_data"
    )
    data_path3 <- get_data(
        "https://github.com/Shians/scBenchData/raw/master/mix_9cell_data.RData",
        "cell_mix_data"
    )

    # dummy bindings
    sc_10x <- NULL
    sc_celseq <- NULL
    sc_dropseq <- NULL
    mix_9cell_07clean_1cell_mat <- NULL
    mix_9cell_08clean_1cell_mat <- NULL
    mix_9cell_09clean_1cell_mat <- NULL
    mix_9cell_07clean_3cell_mat <- NULL
    mix_9cell_07clean_90cell_mat <- NULL
    mrna_mix_celseq <- NULL
    mrna_mix_sortseq <- NULL

    load(data_path1)
    load(data_path2)
    load(data_path3)

    out <- list(
        "sc_10x" = sc_10x,
        "sc_celseq" = sc_celseq,
        "sc_dropseq" = sc_dropseq,
        "cell_mix1" = mix_9cell_07clean_1cell_mat,
        "cell_mix2" = mix_9cell_08clean_1cell_mat,
        "cell_mix3" = mix_9cell_09clean_1cell_mat,
        "cell_mix4" = mix_9cell_07clean_3cell_mat,
        "cell_mix5" = mix_9cell_07clean_90cell_mat,
        "mrna_mix_celseq" = mrna_mix_celseq,
        "mrna_mix_sortseq" = mrna_mix_sortseq
    )

    invisible(out)
}


#' Clear cached datasets
#'
#' Delete the datasets cached by the load_*_data set of functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clear_cached_datasets()
#' }
clear_cached_datasets <- function() {
    bfc <- .get_cache()

    get_query_rid <- function(query) {
        BiocFileCache::bfcquery(bfc, query)$rid
    }

    BiocFileCache::bfcremove(bfc, get_query_rid("sc_data"))
    BiocFileCache::bfcremove(bfc, get_query_rid("mrna_mix_data"))
    BiocFileCache::bfcremove(bfc, get_query_rid("cell_mix_data"))
}
