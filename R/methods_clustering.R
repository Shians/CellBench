#' Clustering using RaceID
#' @param sce the SingleCellExperiment object to perform clustering on
#' @export
clustering_raceid <- function(sce){
    sc <- RaceID::SCseq(as.data.frame(as.matrix(counts(sce))))
    sc <- RaceID::filterdata(sc, mintotal = 1)
    sc <- RaceID::compdist(sc)
    sc <- RaceID::clustexp(sc)
    sc <- RaceID::findoutliers(sc)

    res <- sc@cpart

    return(factor(res))
}

# pre-cleaning required for sc3 clustering
#' @importMethodsFrom SingleCellExperiment counts
pre_clean <- function(sce) {
    ave.counts <- rowMeans(counts(sce))
    keep <- ave.counts >= 1
    sce1 <- sce[keep,]
    sce1 <- scater::calculateQCMetrics(sce1)
    sce1 <- scater::normalize(sce1)

    return(sce1)
}

#' Clustering using SC3
#' @inheritParams clustering_raceid
#' @param col.sym the column name containing gene symbols
#' @importMethodsFrom SingleCellExperiment rowData colData
#' @importMethodsFrom SummarizedExperiment  rowData<- colData<-
#' @export
clustering_sc3 <- function(sce, col.sym = "Symbol") {
    sce_cleaned <- pre_clean(sce)

    rowData(sce_cleaned)$feature_symbol <- rownames(sce_cleaned)
    SingleCellExperiment::isSpike(sce_cleaned, "ERCC") <- FALSE

    sce_cleaned <- SC3::sc3_estimate_k(sce_cleaned)
    k_est <- sce_cleaned@metadata$sc3$k_estimation

    sce_cleaned <- SC3::sc3(
        sce_cleaned,
        ks = k_est,
        biology = FALSE,
        k_estimator = FALSE,
        rand_seed = 0,
        n_cores = 1
    )

    res <- colData(sce_cleaned)[[glue::glue("sc3_{k_est}_clusters")]]

    return(factor(res))
}

# #' Clustering using Seurat
# #' @inheritParams clustering_raceid
# #' @importMethodsFrom SingleCellExperiment counts
# #' @export
# clustering_seurat <- function(sce) {
#     num_dim <- floor(nrow(sce)/5000)
#     if (num_dim > 5) {
#         num_dim <- 5
#     }

#     # rough guess of how many PCA components to use
#     num_dim <- c(10, 10, 20, 30, 40, 50)[num_dim + 1]

#     pbmc <- Seurat::CreateSeuratObject(
#         raw.data = as.matrix(counts(sce)),
#         min.cells = 3,
#         min.genes = 0,
#         project = "",
#         display.progress = FALSE
#     )

#     pbmc <- Seurat::NormalizeData(object = pbmc, display.progress = FALSE)

#     pbmc <- Seurat::FindVariableGenes(
#         object = pbmc,
#         x.low.cutoff = 0.0125,
#         x.high.cutoff = 3,
#         y.cutoff = 0.5,
#         display.progress = FALSE
#     )

#     pbmc <- Seurat::ScaleData(
#         object = pbmc,
#         vars.to.regress = c("nUMI"),
#         display.progress = FALSE,
#         do.cpp = TRUE
#     )

#     pbmc <- Seurat::RunPCA(
#         object = pbmc,
#         pcs.compute = num_dim,
#         pc.genes = pbmc@var.genes,
#         do.print = FALSE
#     )

#     pbmc <- Seurat::FindClusters(
#         object = pbmc,
#         dims.use = 1:num_dim,
#         print.output = FALSE
#     )

#     res <- pbmc@meta.data$res.0.8

#     return(factor(res))
# }
