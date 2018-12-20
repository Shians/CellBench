context("Utilities")

test_that(
    "Matrix head works", {
    x <- matrix(runif(100), nrow = 10, ncol = 10)

    expect_identical(mhead(x), x[1:6, 1:6])
    expect_identical(mhead(x, n = 10), x)
    expect_identical(mhead(x, n = 11), x)

    y <- c(1, 2, 3)
    expect_error(mhead(y), "!is.null(dim(x)) is not TRUE", fixed = TRUE)
    expect_error(mhead(x, n = 0), "n > 0 is not TRUE", fixed = TRUE)
    expect_error(mhead(x, n = "a"), "is.numeric(n) is not TRUE", fixed = TRUE)
})

test_that(
    "Function outer product works", {
    fn_list1 <- list(
        log = log,
        exp = exp
    )

    fn_list2 <- list(
        log = log,
        exp = exp
    )

    expect_equal(
        purrr::map_dbl(fn_outer_prod(fn_list1, fn_list2), function(f) f(1)),
        c(log..log = -Inf, log..exp = 1, exp..log = 1, exp..exp = 15.15426),
        tolerance = 1e-6
    )
})

test_that(
    "Pipeline summarisation works", {
    methods1 <- list(
        mean = mean,
        median = median
    )

    methods2 <- list(
        add1 = function(x) { x+1 },
        times2 = function(x) { x*2 }
    )

    data <- list(
        data1 = c(1, 2, 3)
    )

    expect_identical(
        structure(
            list(
                pipeline = c(
                    "data1 → mean → add1", "data1 → mean → times2",
                    "data1 → median → add1", "data1 → median → times2"
                ),
                result = c(3, 4, 3, 4)
            ),
            row.names = c(NA, -4L),
            class = c("tbl_df", "tbl", "data.frame")
        ),
        data %>%
            apply_methods(methods1) %>%
            apply_methods(methods2) %>%
            pipeline_collapse()
    )
})

test_that(
    "all_same_class works properly", {
    x <- list(
        1, 2, 3
    )
    expect_true(all_same_class(x))

    x <- list(
        1, 2, "a"
    )
    expect_false(all_same_class(x))
})