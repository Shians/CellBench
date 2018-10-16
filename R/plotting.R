plot_intron_prop <- function(datasets) {
    plot_map_metrics(datasets, metrics = "mapped_to_intron")
}

plot_exon_prop <- function(datasets) {
    plot_map_metrics(datasets, metrics = "mapped_to_exon")
}

plot_map_metrics <- function(datasets, metrics = c("mapped_to_exon", "mapped_to_intron", "mapped_to_MT")) {
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

            total_count <- rowSums(as.matrix(scPipe::QC_metrics(x)[, map_stat_columns]))
            count <- scPipe::QC_metrics(x)[, metrics, drop=FALSE]

            count %>%
                data.frame() %>%
                dplyr::mutate_if(
                    is.numeric,
                    function(x) { x / total_count }
                )
        })

    prop_table <- data.frame(
        dataset = rep(exp_names, times = sapply(prop, nrow)),
        do.call(rbind, prop)
    )

    prop_table %>%
        tidyr::gather_(metrics, key = "metric", value = "proportion") %>%
        ggplot2::ggplot(ggplot2::aes(x = dataset, y = proportion, fill = metric)) +
        ggplot2::geom_boxplot() +
        ggplot2::ggtitle("Mapping proportions")
}

compute_pca <- function(gene_expr, ndims = 2) {
    pca <- prcomp(t(gene_expr))

    if (ndims < ncol(pca$x)) {
        data.frame(pca$x[, 1:ndims])
    } else {
        data.frame(pca$x)
    }
}

plot_pca <- function(gene_expr, col_group = NULL) {
    plot_data <- compute_pca(gene_expr)

    if (!is.null(col_group)) {
        plot_data$group <- col_group
        p <- plot_data %>%
            ggplot2::ggplot(ggplot2::aes(x = dim1, y = dim2, col = group))
    } else {
        p <- plot_data %>%
            ggplot2::ggplot(ggplot2::aes(x = dim1, y = dim2))
    }

    p +
        ggplot2::geom_point() +
        ggplot2::ggtitle("PCA plot")
}
