clustering_raceid <- function(sce){
  require("RaceID")

  sc <- SCseq(as.data.frame(as.matrix(counts(sce))))

  sc <- filterdata(sc, mintotal=1, minexpr = 5, minnumber = 5,
                   LBatch = NULL, knn = 10, CGenes = NULL, FGenes = NULL, ccor = 0.4,
                   bmode = "RaceID")
  sc <- compdist(sc, metric="pearson", FSelect = TRUE, knn = NULL)
  sc <- clustexp(sc, sat = TRUE, samp = NULL, cln = NULL, clustnr = 30,
                 bootnr = 50, rseed = 17000, FUNcluster = "kmedoids")
  sc <- findoutliers(sc, probthr = 0.001, outminc = 5, outlg = 2,
                     outdistquant = 0.95)

  res <- sc@cpart

  return(res)

}

pre_clean <- function(sce) {

  ave.counts <- rowMeans(counts(sce))
  keep <- ave.counts >= 1
  sce1 <- sce[keep,]
  # del_gns <- c(which(rowData(sce1)$is_feature_control_rbp), which(rowData(sce1)$is_feature_control_mt))
  # sce1 <- sce1[-del_gns,]
  sce1 <- calculateQCMetrics(sce1)
  sce1 <- normalize(sce1)

  return(sce1)
}


clustering_sc3 <- function(sce, col.sym = "Symbol"){

  require("scater")
  require("SC3")

  sce_cleaned <- pre_clean(sce)

  rowData(sce_cleaned)$feature_symbol <- rownames(sce_cleaned)
  isSpike(sce_cleaned, "ERCC") <- FALSE

  sce_cleaned <- sc3_estimate_k(sce_cleaned)
  k_est <- sce_cleaned@metadata$sc3$k_estimation
  sce_cleaned <- sc3(sce_cleaned, ks = k_est, biology = FALSE, k_estimator = FALSE, rand_seed=0)

  res <- colData(sce_cleaned)[[glue::glue("sc3_{k_est}_clusters")]]

  return(res)
}

clustering_seurat <- function(sce) {

  require("Seurat")

  num_dim <- floor(dim(sce)[1]/5000)
  if(num_dim>5) num_dim=5
  num_dim <- c(10, 10, 20, 30, 40, 50)[num_dim+1]

  pbmc <-  CreateSeuratObject(raw.data = as.matrix(counts(sce)),
                              min.cells = 3, min.genes = 0,  project = "",
                              is.expr = 0, normalization.method = NULL,
                              scale.factor = 10000, do.scale = FALSE, do.center = FALSE,
                              names.field = 1, names.delim = "_", meta.data = NULL,
                              display.progress = FALSE)

  pbmc <- NormalizeData(object = pbmc, normalization.method = "LogNormalize", scale.factor = 10000,
                        assay.type = "RNA", display.progress = FALSE)

  pbmc <- FindVariableGenes(object = pbmc, mean.function = ExpMean, dispersion.function = LogVMR,
                            x.low.cutoff = 0.0125, x.high.cutoff = 3, y.cutoff = 0.5,
                            y.high.cutoff = Inf, num.bin = 20, binning.method = "equal_width",
                            selection.method = "mean.var.plot", top.genes = 1000, do.recalc = TRUE,
                            sort.results = TRUE, do.cpp = TRUE, display.progress = FALSE)

  pbmc <- ScaleData(object = pbmc, vars.to.regress = c("nUMI"),
                    model.use = "linear", use.umi = FALSE, do.scale = TRUE,
                    do.center = TRUE, scale.max = 10, block.size = 1000,
                    min.cells.to.block = 3000, display.progress = FALSE, assay.type = "RNA",
                    do.cpp = TRUE, check.for.norm = TRUE, do.par = FALSE, num.cores = 1)

  pbmc <- RunPCA(object = pbmc, pc.genes = pbmc@var.genes, do.print = FALSE, reduction.name = "pca",
                 reduction.key = "PC", assay.type = "RNA", seed.use = 42)

  pbmc <- JackStraw(pbmc, num.pc = 20, num.replicate = 100, prop.freq = 0.01,
                    display.progress = FALSE, do.par = FALSE, num.cores = 1, maxit = 1000)

  pbmc <- FindClusters(object = pbmc, reduction.type = "pca",
                       dims.use = 1:num_dim,  print.output = FALSE, save.SNN = FALSE,
                       genes.use = NULL,
                       k.param = 30, plot.SNN = FALSE, prune.SNN = 1/15,
                       distance.matrix = NULL,
                       reuse.SNN = FALSE, force.recalc = FALSE, nn.eps = 0,
                       modularity.fxn = 1, resolution = 0.8, algorithm = 1, n.start = 100,
                       n.iter = 10, random.seed = 0)

  res <- pbmc@meta.data$res.0.8

  return(res)
}


