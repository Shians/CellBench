context("Bechmark timing table methods")

test_that("benchmark_timing_tbl methods work", {
    datasets <- list(
        data1 = 1:10,
        data2 = 11:20
    )

    transform <- list(
        identity = identity
    )

    res <- datasets %>%
        time_methods(transform)

    res_timing_stripped <- res %>% strip_timing()
    res_timing_unpacked <- res %>% unpack_timing()

    expect_equal(
        class(res_timing_stripped),
        c("benchmark_tbl", "tbl_df", "tbl", "data.frame")
    )

    expect_equal(
        datasets,
        res_timing_stripped$result,
        check.names = FALSE
    )

    expect_equal(
        colnames(res_timing_unpacked),
        c("data", "transform", "user", "system", "elapsed")
    )
})

