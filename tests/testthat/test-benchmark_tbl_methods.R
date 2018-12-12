context("benchmark_tbl methods")

test_that(
    "benchmark_tbl summary works properly", {
    data(sample_sce_data)
    data_list <- list(
        data = sample_sce_data
    )

    method_list <- list(
        sample_cells = partial(sample_cells, n = 10),
        sample_genes = partial(sample_genes, n = 10)
    )

    res <- apply_methods(data_list, method_list)

    expect_output(summary(res))
})