# impute_drimpute_and_plot_pca <- function(datasets) {
#     impute_and_plot_pca(datasets, impute_drimpute)
# }

# impute_knn_smooth_and_plot_pca <- function(datasets) {
#     impute_and_plot_pca(datasets, impute_knn_smooth)
# }

# impute_basics_and_plot_pca <- function(datasets) {
#     impute_and_plot_pca(datasets, impute_basics)
# }

# #' @importFrom rlang .data
# impute_and_plot_pca <- function(datasets, impute_fn, ...) {
#     stopifnot(is(datasets, "list"))

#     exp_names <- names(datasets)

#     reg_exprs <- lapply(datasets, lib_size_norm_expr)
#     imputed_exprs <- parallel::mclapply(datasets, impute_fn, mc.cores = 4)

#     reg_pca_vals <- parallel::mclapply(reg_exprs, compute_pca, mc.cores = 4)
#     imp_pca_vals <- parallel::mclapply(imputed_exprs, compute_pca, mc.cores = 4)

#     col_groups <- lapply(datasets, function(x) colData(x)$cell_line)

#     reg_pca_df <- do.call(rbind, reg_pca_vals)
#     names(reg_pca_df) <- paste0("dim", 1:2)
#     reg_pca_df$group <- do.call(c, col_groups)
#     reg_pca_df$dataset <- rep(exp_names, times = purrr::map_int(reg_pca_vals, nrow))
#     reg_pca_df$type <- "raw"

#     imp_pca_df <- do.call(rbind, imp_pca_vals)
#     names(imp_pca_df) <- paste0("dim", 1:2)
#     imp_pca_df$group <- do.call(c, col_groups)
#     imp_pca_df$dataset <- rep(exp_names, times = purrr::map_int(imp_pca_vals, nrow))
#     imp_pca_df$type <- "imputed"

#     plot_df <- rbind(reg_pca_df, imp_pca_df)
#     plot_df$type <- factor(plot_df$type, levels = c("raw", "imputed"))

#     plot_df %>%
#         ggplot2::ggplot(ggplot2::aes_string(x = "Dim1", y = "Dim2", col = "group")) +
#         ggplot2::geom_point() +
#         ggplot2::facet_grid(type~dataset, scales = "free", switch = "y")
# }
