# get pca coords
post_compute_pca <- function(gene_expr, ndims = 2) {
    pca <- prcomp(t(gene_expr))

    if (ndims > ncol(pca$x)) {
        ndims <- ncol(pca$x)
    }

    setNames(data.frame(pca$x[, 1:ndims]), nm = glue::glue("Dim{1:ndims}"))
}
