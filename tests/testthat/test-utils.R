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
                pipeline = factor(c(
                    "data1 » mean » add1", "data1 » mean » times2",
                    "data1 » median » add1", "data1 » median » times2"
                )),
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

    expect_identical(
        structure(
            list(
                pipeline = factor(c(
                    "mean » add1", "mean » times2",
                    "median » add1", "median » times2"
                )),
                result = c(3, 4, 3, 4)
            ),
            row.names = c(NA, -4L),
            class = c("tbl_df", "tbl", "data.frame")
        ),
        data %>%
            apply_methods(methods1) %>%
            apply_methods(methods2) %>%
            pipeline_collapse(data.name = FALSE)
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

test_that(
    "class manipulators work properly", {
    expect_identical(class(add_class(1, "a")), c("a", "numeric"))
    expect_identical(
        1, 1 %>% add_class("a") %>% drop_class("a")
    )

    expect_identical(
        1, 1 %>% drop_class("numeric") %>% drop_class("numeric")
    )

    expect_identical(
        1, 1 %>% add_class("numeric")
    )

    expect_identical(
        1, 1 %>% drop_class("a")
    )
})

test_that(
    "if_null_then works properly", {
    expect_identical(if_null_then(NULL, 10), 10)
    expect_identical(if_null_then(1, 10), 1)
})

test_that(
    "if_empty_then works properly", {
    expect_identical(if_empty_then(NULL, 10), 10)
    expect_identical(if_empty_then(1, 10), 1)
})

test_that(
    "make_combinations works properly", {
    x <- c("b", "a")
    y <- c("y", "z")
    z <- c("j", "i")

    expect_equal(
        make_combinations(data.frame(x, y), z),
        tibble::tibble(
            x = factor(c("b", "b", "a", "a")),
            y = factor(c("y", "y", "z", "z")),
            z = factor_no_sort(c("j", "i", "j", "i"))
        )
    )

    expect_equal(
        make_combinations(horse = data.frame(x, y), z),
        tibble::tibble(
            x = factor(c("b", "b", "a", "a")),
            y = factor(c("y", "y", "z", "z")),
            z = factor_no_sort(c("j", "i", "j", "i"))
        )
    )

    expect_equal(
        make_combinations(data.frame(x, y), shoe = z),
        tibble::tibble(
            x = factor(c("b", "b", "a", "a")),
            y = factor(c("y", "y", "z", "z")),
            shoe = factor_no_sort(c("j", "i", "j", "i"))
        )
    )

    expect_equal(
        make_combinations(horse = data.frame(x, y), shoe = z),
        tibble::tibble(
            x = factor(c("b", "b", "a", "a")),
            y = factor(c("y", "y", "z", "z")),
            shoe = factor_no_sort(c("j", "i", "j", "i"))
        )
    )
})

test_that(
    "infer_names_from_dots works properly", {
    x <- 1
    y <- "a"
    df <- data.frame(
        foo = "foo",
        bar = "bar"
    )

    expect_identical(infer_names_from_dots(x, y), c("x", "y"))
    expect_identical(infer_names_from_dots(X = x, y), c("X", "y"))
    expect_identical(infer_names_from_dots(X = x, y, df), c("X", "y", "df"))
    expect_identical(infer_names_from_dots(X = x, y, DF = df), c("X", "y", "DF"))

    expect_warning(
        infer_names_from_dots(y = x, y, DF = df),
        "not all names were unique, numbers appended to duplicates"
    )
})

test_that(
    "seq utils work", {
    x <- matrix(1, ncol = 10, nrow = 8)

    expect_identical(seq_nrow(x), 1:8)
    expect_identical(seq_ncol(x), 1:10)

    expect_length(seq_nrow(c(1,2,3)), 0)
    expect_length(seq_ncol(c(1,2,3)), 0)
})
