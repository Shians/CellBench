context("Time methods")

# System.time has rather large overhead and it's difficult to write extensive
# unit tests without enormous cost.

test_that("Time methods works", {
    datasets <- list(
        data1 = 1:10,
        data2 = 11:20
    )

    transform <- list(
        exp = exp
    )

    res <- datasets %>%
        time_methods(transform)

    expect_identical(res$data, factor(c("data1", "data2")))
    expect_identical(res$transform, factor(c("exp", "exp")))
    expect_equal(
        unlist(lapply(res$timed_result, function(x) x$timing["elapsed"])),
        c(0, 0),
        check.names = FALSE,
        tolerance = 0.2
    )

    transform2 <- list(
        log = log
    )

    res2 <- res %>%
        time_methods(transform2)

    expect_equal(
        res2 %>% strip_timing() %>% extract2("result"),
        datasets,
        check.names = FALSE
    )
})