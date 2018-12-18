context("Function list constructor")

test_that(
    "fn_list constructor works properly", {
    flist <- fn_list(
        mean = mean,
        median = median
    )

    expected <- list(
        mean = mean,
        median = median
    )
    class(expected) <- c("fn_list", "list")

    expect_identical(flist, expected)

    expect_error(
        fn_list(1),
        "all fn_list members must be functions"
    )

    expect_error(
        fn_list(log),
        "all fn_list members must have names, e.g. fn_list(fn1 = log)",
        fixed = TRUE
    )
})
