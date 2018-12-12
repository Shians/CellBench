context("Random sampling")

test_that(
    "Correct number of rows sampled", {
    data("sample_sce_data")

    x <- sample_cells(sample_sce_data, n = 5)
    expect_equal(ncol(x), 5)

    x <- sample_cells(sample_sce_data, n = 51)
    expect_equal(ncol(x), 50)

    y <- sample_genes(sample_sce_data, n = 5)
    expect_equal(nrow(y), 5)

    y <- sample_genes(sample_sce_data, n = 201)
    expect_equal(nrow(y), 200)
})