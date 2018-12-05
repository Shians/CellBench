plot_intron_prop <- function(datasets) {
    plot_map_metrics(datasets, metrics = "mapped_to_intron")
}

plot_exon_prop <- function(datasets) {
    plot_map_metrics(datasets, metrics = "mapped_to_exon")
}

plot_map_metrics <- function(
    datasets,
    metrics = c("mapped_to_exon", "mapped_to_intron", "mapped_to_MT")
) {
    exp_names <- names(datasets)

    prop <- datasets %>%
        lapply(function(x) {
            map_stat_columns <- c(
                "unaligned",
                "aligned_unmapped",
                "mapped_to_exon",
                "mapped_to_intron",
                "ambiguous_mapping",
                "mapped_to_ERCC",
                "mapped_to_MT"
            )

            total_count <- scPipe::QC_metrics(x)[, map_stat_columns] %>%
                as.matrix() %>%
                rowSums()
            count <- scPipe::QC_metrics(x)[, metrics, drop=FALSE]

            count %>%
                data.frame() %>%
                dplyr::mutate_if(
                    is.numeric,
                    function(x) { x / total_count }
                )
        })

    prop_table <- data.frame(
        dataset = rep(exp_names, times = purrr::map_int(prop, nrow)),
        do.call(rbind, prop)
    )

    prop_table %>%
        tidyr::gather_(metrics, key = "metric", value = "proportion") %>%
        ggplot2::ggplot(ggplot2::aes(x = dataset, y = proportion, fill = metric)) +
        ggplot2::geom_boxplot() +
        ggplot2::ggtitle("Mapping proportions")
}

#' Compute PCA coordinates
#'
#' @param gene_expr the gene expression matrix with samples in columns and expression in rows
#' @param ndims the number of dimensions to return
#'
#' @return data.frame with PCA dimensions labelled Dim(n) where (n) is the dimension number
#'
#' @importFrom stats prcomp
#' @importFrom stats setNames
#'
#' @export
compute_pca <- function(gene_expr, ndims = 2) {
    pca <- prcomp(t(gene_expr))

    if (ndims > ncol(pca$x)) {
        ndims <- ncol(pca$x)
    }

    setNames(data.frame(pca$x[, 1:ndims]), nm = glue::glue("Dim{1:ndims}"))
}

#' Compute the PCA coordinates using the most variable genes
#'
#' @describeIn compute_pca
#'
#' @inheritParams compute_pca
#' @param ngenes the number of most variable genes to use
#'
#' @export
compute_pca_most_var <- function(gene_expr, ndims = 2, ngenes = 500) {
    stopifnot(
        is.matrix(gene_expr),
        ngenes > 0
    )

    gene_expr <- filter_zero_genes(gene_expr)
    if (ngenes > nrow(gene_expr)) {
        ngenes <- nrow(gene_expr)
    }

    coef_of_var <- row_apply(gene_expr, var) / row_apply(gene_expr, mean)

    sel <- order(coef_of_var)[1:ngenes]

    compute_pca(gene_expr[sel, ], ndims = ndims)
}

plot_pca <- function(gene_expr, col_group = NULL) {
    plot_data <- compute_pca(gene_expr)

    if (!is.null(col_group)) {
        plot_data$group <- col_group
        p <- plot_data %>%
            ggplot2::ggplot(ggplot2::aes(x = Dim1, y = Dim2, col = group))
    } else {
        p <- plot_data %>%
            ggplot2::ggplot(ggplot2::aes(x = Dim1, y = Dim2))
    }

    p +
        ggplot2::geom_point() +
        ggplot2::ggtitle("PCA plot")
}
