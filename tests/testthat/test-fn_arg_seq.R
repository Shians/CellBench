context("Generating functions with sequence of arguments")

test_that(
    "Function sequence generated correctly", {
    fn <- function(x, y) {
        x + y
    }

    fn_list <- fn_arg_seq(fn, y = 1:3)

    expect_identical(
        names(fn_list),
        c("fn(y = 1)", "fn(y = 2)", "fn(y = 3)")
    )

    expect_identical(
        purrr::map_dbl(fn_list, function(f) {f(1)}),
        c(`fn(y = 1)` = 2, `fn(y = 2)` = 3, `fn(y = 3)` = 4)
    )
})

test_that(
    "Argument vector ordering is respected", {
    fn <- function(x, y) {
        x + y
    }
    fn_list <- fn_arg_seq(fn, x = 3:1)

    expect_identical(
        names(fn_list),
        c("fn(x = 3)", "fn(x = 2)", "fn(x = 1)")
    )
})

test_that(
    "Errors are properly reported", {
    fn <- function(x, y) {
        x + y
    }

    expect_error(fn_arg_seq(fn, z = 1:3, .strict = TRUE), "args not used in fn: 'z'", fixed = TRUE)
})
