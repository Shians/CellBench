#' A framework for benchmarking combinations of methods in multi-stage pipelines
#'
#' This package contains a framework for benchmarking combinations of methods in
#' a multi-stage pipeline. It is mainly based around the \code{apply_methods}
#' function, which takes lists of functions to be applied in stages of a
#' pipeline.
#'
#' @docType package
#' @name CellBench-package
#' @aliases CellBench
#'
#' @author Shian Su <\url{https://www.github.com/shians}>
#' @seealso The core function in this package is \code{\link{apply_methods}},
#'   see \code{vignette("Introduction", package = "CellBench")} for basic usage.
#'   Run \code{cellbench_case_study()} to see a case study using CellBench. The
#'   data loading functions from \code{\link{load_all_data}} may also be of
#'   interest.
#' @importFrom methods is
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
NULL
