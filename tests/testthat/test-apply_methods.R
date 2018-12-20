context("Apply methods")

test_that(
    "Apply methods works for most basic case", {
    x <- list(
        data = c(1, 2, 3)
    )

    f_list <- list(
        mean = mean
    )

    expected <- structure(
        list(
            data = structure(1L, .Label = "data", class = "factor"),
            f_list = structure(1L, .Label = "mean", class = "factor"),
            result = 2
        ),
        row.names = c(NA, -1L),
        class = c("benchmark_tbl", "tbl_df", "tbl", "data.frame")
    )

    expect_identical(apply_methods(x, f_list), expected)
})

test_that(
    "Apply methods works for expanding functions", {
    x <- list(
        data = c(1, 2, 3)
    )

    f_list <- list(
        mean = mean,
        median = median
    )

    expected <- structure(
        list(
            data = structure(c(1L, 1L), .Label = "data", class = "factor"),
            f_list = structure(1:2, .Label = c("mean", "median"), class = "factor"),
            result = c(2, 2)
        ),
        row.names = c(NA, -2L),
        class = c("benchmark_tbl", "tbl_df", "tbl", "data.frame")
    )

    expect_identical(apply_methods(x, f_list), expected)
})

test_that(
    "Apply methods works for expanding data", {
    x <- list(
        data1 = c(1, 2, 3),
        data2 = c(1, 2, 3)
    )

    f_list <- list(
        mean = mean
    )

    expected <- structure(
        list(
            data = structure(1:2, .Label = c("data1", "data2"), class = "factor"),
            f_list = structure(c(1L, 1L), .Label = "mean", class = "factor"),
            result = c(2, 2)
        ),
        row.names = c(NA, -2L),
        class = c("benchmark_tbl", "tbl_df", "tbl", "data.frame")
    )

    expect_identical(apply_methods(x, f_list), expected)
})

test_that(
    "Apply methods works for expanding data and functions", {
    x <- list(
        data1 = c(1, 2, 3),
        data2 = c(1, 2, 3)
    )

    f_list <- list(
        mean = mean,
        median = median
    )

    expected <- structure(
        list(
            data = structure(c(1L, 1L, 2L, 2L), .Label = c("data1", "data2"), class = "factor"),
            f_list = structure(c(1L, 2L, 1L, 2L), .Label = c("mean", "median"), class = "factor"),
            result = c(2, 2, 2, 2)
        ),
        row.names = c(NA, -4L),
        class = c("benchmark_tbl", "tbl_df", "tbl", "data.frame")
    )

    expect_identical(apply_methods(x, f_list), expected)
})

test_that(
    "Apply methods works for chain expanding", {
    x <- list(
        data1 = c(1, 2, 3)
    )

    f_list1 <- list(
        mean = mean,
        median = median
    )

    f_list2 <- list(
        double = function(x) x * 2,
        add_one = function(x) x + 1
    )

    expected <- structure(
        list(
            data = structure(c(1L, 1L, 1L, 1L), .Label = "data1", class = "factor"),
            f_list1 = structure(c(1L, 1L, 2L, 2L), .Label = c("mean", "median"), class = "factor"),
            f_list2 = structure(c(1L, 2L, 1L, 2L), .Label = c("double", "add_one"), class = "factor"),
            result = c(4, 3, 4, 3)
        ),
        row.names = c(NA, -4L),
        class = c("benchmark_tbl", "tbl_df", "tbl", "data.frame")
    )

    res <- x %>%
        apply_methods(f_list1) %>%
        apply_methods(f_list2)

    expect_identical(res, expected)
})

test_that(
    "Errors are properly reported", {
    data_list <- list(
        x = 1:5
    )

    method_list <- list(
        mean
    )

    expect_error(
        apply_methods(data_list, method_list),
        "every element of fn_list must be named"
    )

    expect_error
})

test_that(
    "Apply methods works for expanding functions", {
    x <- list(
        data = c(1, 2, 3)
    )

    f_list <- list(
        mean = mean,
        median = median
    )

    expected <- structure(
        list(
            data = structure(c(1L, 1L), .Label = "data", class = "factor"),
            f_list = structure(1:2, .Label = c("mean", "median"), class = "factor"),
            result = c(2, 2)
        ),
        row.names = c(NA, -2L),
        class = c("benchmark_tbl", "tbl_df", "tbl", "data.frame")
    )

    expect_identical(apply_methods(x, f_list), expected)
})

test_that(
    "Multithreading works", {

    x <- list(
        data1 = c(1, 2, 3),
        data2 = c(1, 2, 3)
    )

    f_list <- list(
        mean = mean,
        median = median
    )

    set_cellbench_threads(4)
    res <- apply_methods(x, f_list)

    set_cellbench_threads(1)
    expected <- apply_methods(x, f_list)

    expect_identical(res, expected)

    f_list2 <- fn_list(
        f1 = function(x) x + 1
    )

    set_cellbench_threads(4)
    res <- x %>%
        apply_methods(f_list) %>%
        apply_methods(f_list2)

    set_cellbench_threads(1)
    expected <- x %>%
        apply_methods(f_list) %>%
        apply_methods(f_list2)

    expect_identical(res, expected)
})
