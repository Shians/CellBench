context("benchmark_tbl methods")

test_that(
    "benchmark_tbl summary works properly", {
    sample_sce_data <- readRDS(cellbench_file("10x_sce_sample.rds"))
    data_list <- list(
        data = sample_sce_data
    )

    method_list <- list(
        sample_cells = purrr::partial(sample_cells, n = 10),
        sample_genes = purrr::partial(sample_genes, n = 10)
    )

    res <- apply_methods(data_list, method_list)

    expect_output(summary(res))
})
