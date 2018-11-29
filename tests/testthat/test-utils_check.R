context("Check checking functions")

test_that(
"Test class testing functions", {
    expect_true(is_one_of(1, c("numeric", "character")))
    expect_true(is_one_of("A", c("numeric", "character")))
    expect_false(is_one_of(TRUE, c("numeric", "character")))
    expect_false(is_one_of("A", c("numeric", "logical")))
    expect_error(is_one_of(1, 1))

    x <- 0
    class(x) <- c("foo", "numeric")
    expect_true(is_all_of(x, c("foo", "numeric")))
    expect_true(is_all_of(x, c("numeric", "foo")))
    expect_false(is_all_of(x, c("foo", "numeric", "bar")))
    expect_error(is_all_of(1, 1))
})
