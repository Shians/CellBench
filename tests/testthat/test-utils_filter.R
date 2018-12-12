context("Filtering utilities")

test_that(
    "Filtering zero genes works", {
    x <- matrix(runif(100), nrow = 10, ncol = 10)

    x[1, ] <- 0

    expect_identical(
        filter_zero_genes(x),
        x[-1, ]
    )

    data(sample_sce_data)

    x <- sample_sce_data
    counts(x)[1, ] <- 0
    expect_equal(
        filter_zero_genes(x),
        x[-1, ]
    )

    y <- "foo"
    expect_error(
        filter_zero_genes(y),
        "is_one_of(x, c(\"SingleCellExperiment\", \"matrix\")) is not TRUE",
        fixed = TRUE
    )
})

test_that(
    "Keeping high count genes works", {
    x <- matrix(rep(1:5, times = 10), nrow = 5)
    expect_identical(
        keep_high_count_genes(x, n = 2),
        x[4:5, ]
    )
})

test_that(
    "Keeping high count cells works", {
    x <- matrix(rep(1:5, times = 10), ncol = 5, byrow = TRUE)
    expect_identical(
        keep_high_count_cells(x, n = 2),
        x[, 4:5]
    )
})

test_that(
    "Keeping high variance genes works", {
    x <- matrix(rep(1:5, times = 10), ncol = 5, byrow = TRUE)
    x[1:8, ] <- 1
    expect_identical(
        keep_high_var_genes(x, n = 2),
        x[9:10, ]
    )
})