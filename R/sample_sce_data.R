#' This is data for testing functions in CellBench.
#'
#' A dataset containing 200 genes and 50 cells randomly sampled from the CelSeq
#' mRNA mixture dataset, each sample is a mixture of mRNA material from 3
#' different human adenocarcinoma cell lines. Useful for quick prototyping of
#' method wrappers.
#'
#' @usage \code{data(sample_sce_data)}
#'
#' @format A SingleCellExperiment object with sample annotations in
#'   \code{colData(sample_sce_data)}. The annotation contains various QC metrics
#'   as well as the cell line mixture proportions
#' \describe{
#'   \item{H2228_prop}{proportion of mRNA from H2228 cell line}
#'   \item{H1975_prop}{proportion of mRNA from H1975 cell line}
#'   \item{HCC827_prop}{proportion of mRNA from HCC827 cell line}
#' }
#'
#' @seealso \code{\link{load_mrna_mix_data}}
"sample_sce_data"
